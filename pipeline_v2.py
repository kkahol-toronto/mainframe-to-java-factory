#!/usr/bin/env python3
"""
COBOL to Spring Boot Migration Pipeline v2
==========================================

Enhanced pipeline that:
1. Parses JCL to extract job structure and step dependencies
2. Maps EXEC PGM= to COBOL programs
3. Uses LLM to translate COBOL paragraphs to Java methods
4. Generates complete Tasklets with full business logic
5. Creates Spring Batch Job configurations

Architecture:
  JCL → Job/Step Structure → Spring Batch Jobs
  COBOL → Paragraph Analysis → Java Tasklets (with LLM-translated logic)
"""

from __future__ import annotations

import json
import os
import re
from dataclasses import dataclass, field
from enum import Enum, auto
from pathlib import Path
from typing import Any, Dict, List, Optional, Tuple

from dotenv import load_dotenv
from openai import AzureOpenAI

load_dotenv()

# =============================================================================
# CONFIGURATION
# =============================================================================

WORKSPACE = Path(__file__).parent
MAINFRAME_DIR = WORKSPACE / "work" / "mainframe_clean"
JCL_DIR = MAINFRAME_DIR / "jcl"
COBOL_DIR = MAINFRAME_DIR / "cobol"
COPYBOOK_DIR = MAINFRAME_DIR / "copybooks"
OUTPUT_DIR = WORKSPACE / "misc-1099" / "src" / "main" / "java" / "com" / "fordcredit" / "misc1099"

# =============================================================================
# DATA STRUCTURES
# =============================================================================

class StepType(Enum):
    COBOL_PROGRAM = auto()
    SORT = auto()
    IEBGENER = auto()
    FOCUS = auto()
    OTHER = auto()


@dataclass
class JclStep:
    """Represents a single JCL step"""
    name: str
    step_type: StepType
    program: Optional[str] = None  # COBOL program name if applicable
    condition: Optional[str] = None  # COND= clause
    dd_statements: Dict[str, str] = field(default_factory=dict)  # DD name → dataset


@dataclass
class JclJob:
    """Represents a JCL job"""
    name: str
    description: str
    steps: List[JclStep] = field(default_factory=list)
    cobol_programs: List[str] = field(default_factory=list)  # Unique COBOL programs used


@dataclass
class CobolParagraph:
    """Represents a COBOL paragraph"""
    name: str
    start_line: int
    end_line: int
    content: str
    calls: List[str] = field(default_factory=list)  # PERFORMed paragraphs


@dataclass
class CobolProgram:
    """Represents a parsed COBOL program"""
    program_id: str
    source_file: Path
    file_selects: List[Dict[str, str]]
    copybooks: List[str]
    paragraphs: List[CobolParagraph]
    working_storage: str  # For variable analysis


# =============================================================================
# JCL PARSER
# =============================================================================

class JclParser:
    """Parses JCL files to extract job structure"""
    
    def parse(self, jcl_path: Path) -> JclJob:
        text = jcl_path.read_text(encoding='utf-8', errors='replace')
        
        # Extract job name
        job_match = re.search(r'^//(\w+)\s+JOB', text, re.MULTILINE)
        job_name = job_match.group(1) if job_match else jcl_path.stem
        
        # Extract description from comments
        desc_match = re.search(r'/\*\s+\w+\s+(.+?)(?:\n|$)', text)
        description = desc_match.group(1).strip() if desc_match else ""
        
        # Parse steps
        steps = self._parse_steps(text)
        
        # Extract unique COBOL programs
        cobol_programs = list(set(
            s.program for s in steps 
            if s.step_type == StepType.COBOL_PROGRAM and s.program
        ))
        
        return JclJob(
            name=job_name,
            description=description,
            steps=steps,
            cobol_programs=sorted(cobol_programs)
        )
    
    def _parse_steps(self, text: str) -> List[JclStep]:
        steps = []
        
        # Pattern for EXEC statements
        exec_pattern = re.compile(
            r'^//(\w+)\s+EXEC\s+(?:PGM=)?(\w+)(?:,(.*))?',
            re.MULTILINE
        )
        
        for match in exec_pattern.finditer(text):
            step_name = match.group(1)
            program = match.group(2)
            params = match.group(3) or ""
            
            # Determine step type
            step_type, cobol_prog = self._classify_step(program)
            
            # Extract COND clause
            cond_match = re.search(r'COND=([^,\s]+)', params)
            condition = cond_match.group(1) if cond_match else None
            
            steps.append(JclStep(
                name=step_name,
                step_type=step_type,
                program=cobol_prog,
                condition=condition
            ))
        
        return steps
    
    def _classify_step(self, program: str) -> Tuple[StepType, Optional[str]]:
        program_upper = program.upper()
        
        if program_upper in ('SORT', 'SSORT'):
            return StepType.SORT, None
        elif program_upper == 'IEBGENER':
            return StepType.IEBGENER, None
        elif program_upper == 'FOCUS':
            return StepType.FOCUS, None
        elif program_upper.startswith('CCAC6'):
            return StepType.COBOL_PROGRAM, program_upper
        else:
            return StepType.OTHER, None


# =============================================================================
# COBOL PARSER
# =============================================================================

class CobolParser:
    """Parses COBOL source to extract program structure"""
    
    def parse(self, cobol_path: Path) -> CobolProgram:
        text = cobol_path.read_text(encoding='utf-8', errors='replace')
        
        # Extract program ID
        prog_match = re.search(r'PROGRAM-ID\.\s*(\w+)', text, re.IGNORECASE)
        program_id = prog_match.group(1).upper() if prog_match else cobol_path.stem
        
        # Extract file selects
        file_selects = self._parse_file_selects(text)
        
        # Extract copybooks
        copybooks = re.findall(r'^\s*COPY\s+(\w+)', text, re.MULTILINE | re.IGNORECASE)
        
        # Extract paragraphs
        paragraphs = self._parse_paragraphs(text)
        
        # Extract working storage
        ws_match = re.search(
            r'WORKING-STORAGE\s+SECTION\.(.*?)(?:PROCEDURE\s+DIVISION|$)',
            text, re.DOTALL | re.IGNORECASE
        )
        working_storage = ws_match.group(1) if ws_match else ""
        
        return CobolProgram(
            program_id=program_id,
            source_file=cobol_path,
            file_selects=file_selects,
            copybooks=copybooks,
            paragraphs=paragraphs,
            working_storage=working_storage
        )
    
    def _parse_file_selects(self, text: str) -> List[Dict[str, str]]:
        selects = []
        pattern = re.compile(
            r'SELECT\s+([\w-]+)\s+ASSIGN\s+TO\s+([\w.-]+)',
            re.IGNORECASE
        )
        for match in pattern.finditer(text):
            selects.append({
                'name': match.group(1),
                'assign': match.group(2)
            })
        return selects
    
    def _parse_paragraphs(self, text: str) -> List[CobolParagraph]:
        paragraphs = []
        
        # Find PROCEDURE DIVISION
        proc_match = re.search(r'PROCEDURE\s+DIVISION\.', text, re.IGNORECASE)
        if not proc_match:
            return paragraphs
        
        proc_text = text[proc_match.end():]
        lines = proc_text.split('\n')
        
        # Pattern for paragraph names (name followed by period at start of line)
        para_pattern = re.compile(r'^(\d{4}-[\w-]+|\w+-[\w-]+)\s*\.\s*$')
        
        current_para = None
        current_lines = []
        start_line = 0
        
        for i, line in enumerate(lines):
            stripped = line.strip()
            para_match = para_pattern.match(stripped)
            
            if para_match:
                # Save previous paragraph
                if current_para:
                    content = '\n'.join(current_lines)
                    calls = self._extract_performs(content)
                    paragraphs.append(CobolParagraph(
                        name=current_para,
                        start_line=start_line,
                        end_line=i - 1,
                        content=content,
                        calls=calls
                    ))
                
                # Start new paragraph
                current_para = para_match.group(1)
                current_lines = []
                start_line = i
            elif current_para:
                current_lines.append(line)
        
        # Save last paragraph
        if current_para:
            content = '\n'.join(current_lines)
            calls = self._extract_performs(content)
            paragraphs.append(CobolParagraph(
                name=current_para,
                start_line=start_line,
                end_line=len(lines) - 1,
                content=content,
                calls=calls
            ))
        
        return paragraphs
    
    def _extract_performs(self, text: str) -> List[str]:
        """Extract PERFORM targets"""
        performs = re.findall(r'PERFORM\s+([\w-]+)', text, re.IGNORECASE)
        return list(set(performs))


# =============================================================================
# LLM CLIENT
# =============================================================================

class LLMClient:
    """Azure OpenAI client for code translation"""
    
    def __init__(self):
        self.client = AzureOpenAI(
            api_key=os.getenv("AZURE_OPENAI_API_KEY"),
            api_version="2024-02-15-preview",
            azure_endpoint=os.getenv("AZURE_OPENAI_ENDPOINT")
        )
        self.deployment = os.getenv("AZURE_OPENAI_DEPLOYMENT", "gpt-4o")
    
    def translate_paragraph(
        self, 
        paragraph: CobolParagraph, 
        program: CobolProgram,
        context: str = ""
    ) -> str:
        """Translate a COBOL paragraph to a Java method"""
        
        # Build list of available state variables
        available_vars = self._get_available_state_vars(program)
        
        prompt = f"""Convert this COBOL paragraph to a Java method.

COBOL Program: {program.program_id}
Paragraph: {paragraph.name}

AVAILABLE STATE VARIABLES (use these ONLY):
{available_vars}

COBOL Code to translate:
```cobol
{paragraph.content}
```

CRITICAL REQUIREMENTS:
1. Method signature: private void {self._to_java_method_name(paragraph.name)}(ProgramState state)
2. ONLY use variables from the AVAILABLE STATE VARIABLES list above
3. If a variable is not listed, create it as a local variable with String type
4. Use try/catch for file operations
5. Convert COBOL names to camelCase (WS-DATE-MONTH → wsDateMonth)
6. PERFORM xyz → call method xyz(state);
7. READ file → use state.xxxReader.readLine()
8. WRITE file → use state.xxxWriter.write(line)
9. Return ONLY the Java method body, no markdown fences

COBOL to Java mappings:
- MOVE a TO b → state.b = state.a;
- ADD a TO b → state.b += state.a;
- IF a = b → if (Objects.equals(state.a, state.b))
- END-IF → closing brace
- PERFORM x UNTIL y → while (!y) {{ x(state); }}
- HIGH-VALUES → "\\u00FF"
- LOW-VALUES → "\\u0000"
"""
        
        response = self.client.chat.completions.create(
            model=self.deployment,
            messages=[
                {"role": "system", "content": "You are a COBOL to Java translator. Output only valid Java code. Never use markdown fences."},
                {"role": "user", "content": prompt}
            ],
            temperature=0.1,
            max_tokens=2000
        )
        
        return self._clean_java_output(response.choices[0].message.content)
    
    def _get_available_state_vars(self, program: CobolProgram) -> str:
        """Generate list of available state variables"""
        vars = []
        
        # File readers/writers
        for fs in program.file_selects:
            name = fs['name']
            java_name = self._to_camel_case(name)
            if 'W' in fs['assign'].upper() or 'OUT' in name.upper():
                vars.append(f"state.{java_name}Writer (BufferedWriter)")
            else:
                vars.append(f"state.{java_name}Reader (BufferedReader)")
            vars.append(f"state.{java_name}Eof (boolean)")
            vars.append(f"state.{java_name}RawLine (String)")
        
        # Common variables
        vars.extend([
            "state.endOfRejects (boolean)",
            "state.endOfMaster (boolean)",
            "state.endOfVendor (boolean)",
            "state.endOfInput (boolean)",
            "state.totalRecordsRead (int)",
            "state.totalRecordsWritten (int)",
            "state.returnCode (int)",
            "state.wsDateMonth (String)",
            "state.wsDateDay (String)",
            "state.wsDateYear (String)",
            "state.currentLine (String)",
            "state.ccE01wDisplayRcd (String)",
        ])
        
        # Extract WS variables
        ws_pattern = re.compile(r'^\s*(?:01|05|10|15|77)\s+([\w-]+)', re.MULTILINE)
        for match in ws_pattern.finditer(program.working_storage[:3000]):
            name = match.group(1)
            if name.upper() not in ('FILLER', 'EJECT'):
                java_name = self._to_camel_case(name)
                vars.append(f"state.{java_name} (String)")
        
        return '\n'.join(vars[:60])  # Limit to avoid token overload
    
    def _to_camel_case(self, name: str) -> str:
        """Convert COBOL name to camelCase"""
        parts = name.lower().replace('_', '-').split('-')
        return parts[0] + ''.join(p.capitalize() for p in parts[1:])
    
    def _to_java_method_name(self, cobol_name: str) -> str:
        """Convert COBOL paragraph name to Java method name"""
        # Remove leading numbers and convert to camelCase
        name = re.sub(r'^\d+-', '', cobol_name)
        parts = name.lower().split('-')
        return parts[0] + ''.join(p.capitalize() for p in parts[1:])
    
    def _clean_java_output(self, text: str) -> str:
        """Clean LLM output to get just the Java code"""
        # Remove markdown fences
        text = re.sub(r'```java\s*', '', text)
        text = re.sub(r'```\s*', '', text)
        return text.strip()


# =============================================================================
# TASKLET GENERATOR
# =============================================================================

class TaskletGenerator:
    """Generates complete Java Tasklets from COBOL programs"""
    
    def __init__(self, llm: LLMClient):
        self.llm = llm
    
    def generate(self, program: CobolProgram) -> str:
        """Generate a complete Java Tasklet for a COBOL program"""
        
        # Translate all paragraphs
        translated_methods = []
        for para in program.paragraphs:
            print(f"  Translating paragraph: {para.name}")
            java_method = self.llm.translate_paragraph(para, program)
            # Clean up the method
            java_method = self._harden_method(java_method)
            translated_methods.append(java_method)
        
        # Generate the complete Tasklet
        code = self._assemble_tasklet(program, translated_methods)
        
        # Post-process to fix common issues
        code = self._harden_tasklet(code)
        
        return code
    
    def _harden_method(self, method: str) -> str:
        """Fix common LLM output issues in a method"""
        # Remove duplicate method definitions
        lines = method.split('\n')
        cleaned = []
        for line in lines:
            # Skip markdown artifacts
            if line.strip().startswith('```'):
                continue
            cleaned.append(line)
        return '\n'.join(cleaned)
    
    def _harden_tasklet(self, code: str) -> str:
        """Fix common issues in the complete Tasklet"""
        lines = code.split('\n')
        result = []
        seen_methods = set()
        in_duplicate = False
        brace_depth = 0
        current_method = None
        
        for line in lines:
            # Track method definitions
            method_match = re.match(r'\s*(private|public)\s+\w+\s+(\w+)\s*\(', line)
            if method_match:
                method_name = method_match.group(2)
                if method_name in seen_methods:
                    # Skip duplicate method
                    in_duplicate = True
                    brace_depth = 0
                else:
                    seen_methods.add(method_name)
                    in_duplicate = False
            
            if in_duplicate:
                brace_depth += line.count('{') - line.count('}')
                if brace_depth <= 0 and '}' in line:
                    in_duplicate = False
                continue
            
            result.append(line)
        
        return '\n'.join(result)
    
    def _assemble_tasklet(
        self, 
        program: CobolProgram, 
        methods: List[str]
    ) -> str:
        """Assemble the complete Tasklet class"""
        
        class_name = f"{program.program_id}Tasklet"
        
        # Generate state class fields
        state_fields = self._generate_state_fields(program)
        
        # Generate file I/O methods
        io_methods = self._generate_io_methods(program)
        
        # Join translated methods
        business_methods = "\n\n".join(methods)
        
        return f'''package com.fordcredit.misc1099.batch.program;

import org.springframework.batch.core.StepContribution;
import org.springframework.batch.core.scope.context.ChunkContext;
import org.springframework.batch.core.step.tasklet.Tasklet;
import org.springframework.batch.repeat.RepeatStatus;

/**
 * Generated Tasklet for COBOL program {program.program_id}.
 * Translated using LLM-assisted paragraph conversion.
 */
public class {class_name} implements Tasklet {{

    /**
     * Program state holder.
     */
    static class ProgramState {{
{state_fields}
    }}

    @Override
    public RepeatStatus execute(
            StepContribution contribution,
            ChunkContext chunkContext) {{

        ProgramState state = new ProgramState();
        mainline(state);
        return RepeatStatus.FINISHED;
    }}

    private void mainline(ProgramState state) {{
        openFiles(state);
        generalHousekeeping(state);
        mainProcess(state);
        closingRoutine(state);
        closeFiles(state);
    }}

    // ======================================================
    // FILE I/O
    // ======================================================

{io_methods}

    // ======================================================
    // TRANSLATED BUSINESS LOGIC
    // ======================================================

{business_methods}
}}
'''
    
    def _generate_state_fields(self, program: CobolProgram) -> str:
        """Generate ProgramState fields based on file selects and WS"""
        lines = []
        
        # File readers/writers
        for fs in program.file_selects:
            var_name = self._to_camel_case(fs['name'])
            if 'W' in fs['assign'].upper() or 'OUT' in fs['name'].upper():
                lines.append(f"        java.io.BufferedWriter {var_name}Writer;")
            else:
                lines.append(f"        java.io.BufferedReader {var_name}Reader;")
            lines.append(f"        boolean {var_name}Eof = false;")
            lines.append(f"        String {var_name}RawLine;")
        
        # Extract WS variables from COBOL
        ws_vars = self._extract_ws_variables(program.working_storage)
        
        lines.extend([
            "",
            "        // EOF flags for main processing",
            "        boolean endOfRejects = false;",
            "        boolean endOfMaster = false;",
            "        boolean endOfVendor = false;",
            "        boolean endOfInput = false;",
            "",
            "        // Counters and accumulators",
            "        int totalRecordsRead = 0;",
            "        int totalRecordsWritten = 0;",
            "        int wsTotRejRecdsRead = 0;",
            "        int wsTotVendRecdsRead = 0;",
            "        int wsTotMstrRecdsRead = 0;",
            "        int wsTotRejWrittenCnt = 0;",
            "        java.math.BigDecimal totalAmount = java.math.BigDecimal.ZERO;",
            "",
            "        // Return code",
            "        int returnCode = 0;",
            "        int ws99Lit = 99;",
            "        int ws1Lit = 1;",
            "",
            "        // Date fields",
            "        String wsDateMonth = \"\";",
            "        String wsDateDay = \"\";",
            "        String wsDateYear = \"\";",
            "        String swaCurrMo = \"01\";",
            "        String swaCurrDa = \"01\";",
            "        String swaCurrYr = \"24\";",
            "",
            "        // Error message literals",
            "        String wsEmptyRejLit = \"REJECT FILE CONTAINS 0 RECORDS     ***  \";",
            "        String wsEmptyMstrLit = \"MASTER FILE CONTAINS 0 RECORDS     ***  \";",
            "        String wsEmptyVendLit = \"VENDOR FILE CONTAINS 0 RECORDS     ***  \";",
            "",
            "        // Current record fields",
            "        String currentLine;",
            "        String ccE01wDisplayRcd = \"\";",
            "",
            "        // Record layouts - simplified as strings",
        ])
        
        for var in ws_vars:
            lines.append(f"        String {var} = \"\";")
        
        return "\n".join(lines)
    
    def _extract_ws_variables(self, ws_text: str) -> List[str]:
        """Extract variable names from WORKING-STORAGE"""
        vars = []
        # Pattern for COBOL 01/05/10 level items
        pattern = re.compile(r'^\s*(?:01|05|10|15|77)\s+([\w-]+)', re.MULTILINE)
        for match in pattern.finditer(ws_text):
            name = match.group(1)
            if name.upper() not in ('FILLER', 'EJECT'):
                java_name = self._to_camel_case(name)
                vars.append(java_name)
        return vars[:50]  # Limit to avoid huge state classes
    
    def _generate_io_methods(self, program: CobolProgram) -> str:
        """Generate file I/O methods"""
        lines = []
        
        # openFiles
        lines.append("    private void openFiles(ProgramState state) {")
        lines.append("        try {")
        lines.append(f'            String programName = "{program.program_id}";')
        lines.append('            java.nio.file.Path testDir = java.nio.file.Paths.get(')
        lines.append('                    "../work/mainframe_clean/testcases", programName);')
        
        for fs in program.file_selects:
            var_name = self._to_camel_case(fs['name'])
            file_name = self._guess_file_name(fs['name'], fs['assign'])
            
            if 'W' in fs['assign'].upper() or 'OUT' in fs['name'].upper():
                # Output file
                lines.append(f'            java.nio.file.Path {var_name}Path = testDir.resolve("output/{file_name}");')
                lines.append(f'            java.nio.file.Files.createDirectories({var_name}Path.getParent());')
                lines.append(f'            state.{var_name}Writer = java.nio.file.Files.newBufferedWriter({var_name}Path);')
            else:
                # Input file
                lines.append(f'            java.nio.file.Path {var_name}Path = testDir.resolve("input/{file_name}");')
                lines.append(f'            if (java.nio.file.Files.exists({var_name}Path)) {{')
                lines.append(f'                state.{var_name}Reader = java.nio.file.Files.newBufferedReader({var_name}Path);')
                lines.append(f'            }}')
        
        lines.append("        } catch (Exception e) {")
        lines.append('            throw new RuntimeException("Failed to open files", e);')
        lines.append("        }")
        lines.append("    }")
        lines.append("")
        
        # closeFiles
        lines.append("    private void closeFiles(ProgramState state) {")
        lines.append("        try {")
        for fs in program.file_selects:
            var_name = self._to_camel_case(fs['name'])
            if 'W' in fs['assign'].upper() or 'OUT' in fs['name'].upper():
                lines.append(f'            if (state.{var_name}Writer != null) state.{var_name}Writer.close();')
            else:
                lines.append(f'            if (state.{var_name}Reader != null) state.{var_name}Reader.close();')
        lines.append("        } catch (Exception e) {")
        lines.append('            throw new RuntimeException("Failed to close files", e);')
        lines.append("        }")
        lines.append("    }")
        
        return "\n".join(lines)
    
    def _to_camel_case(self, name: str) -> str:
        """Convert COBOL name to camelCase"""
        parts = name.lower().replace('_', '-').split('-')
        return parts[0] + ''.join(p.capitalize() for p in parts[1:])
    
    def _guess_file_name(self, select_name: str, assign: str) -> str:
        """Guess a reasonable file name for test data"""
        name_lower = select_name.lower()
        if 'master' in name_lower:
            return 'master.txt'
        elif 'reject' in name_lower:
            return 'reject.txt'
        elif 'vendor' in name_lower:
            return 'vendor.txt'
        elif 'control' in name_lower:
            return 'control.txt'
        elif 'summary' in name_lower:
            return 'summary.txt'
        elif 'corporate' in name_lower:
            return 'corporate.txt'
        else:
            return f"{self._to_camel_case(select_name)}.txt"


# =============================================================================
# SPRING BATCH JOB GENERATOR
# =============================================================================

class JobConfigGenerator:
    """Generates Spring Batch Job configurations from JCL"""
    
    def generate(self, job: JclJob) -> str:
        """Generate a Spring Batch Job configuration"""
        
        class_name = f"{job.name.replace('@', '')}JobConfig"
        
        steps = []
        for i, step in enumerate(job.steps):
            step_code = self._generate_step(step, i)
            if step_code:
                steps.append(step_code)
        
        steps_chain = " -> ".join([f"step{i}()" for i in range(len(steps))])
        
        return f'''package com.fordcredit.misc1099.batch.config;

import org.springframework.batch.core.Job;
import org.springframework.batch.core.Step;
import org.springframework.batch.core.job.builder.JobBuilder;
import org.springframework.batch.core.repository.JobRepository;
import org.springframework.batch.core.step.builder.StepBuilder;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.transaction.PlatformTransactionManager;

/**
 * Spring Batch Job configuration for JCL: {job.name}
 * {job.description}
 */
@Configuration
public class {class_name} {{

    private final JobRepository jobRepository;
    private final PlatformTransactionManager transactionManager;

    public {class_name}(JobRepository jobRepository, 
                       PlatformTransactionManager transactionManager) {{
        this.jobRepository = jobRepository;
        this.transactionManager = transactionManager;
    }}

    @Bean
    public Job {job.name.replace('@', '').lower()}Job() {{
        return new JobBuilder("{job.name}", jobRepository)
                .start({steps_chain.split(' -> ')[0] if steps else 'dummyStep()'})
                .build();
    }}

{chr(10).join(steps)}
}}
'''
    
    def _generate_step(self, step: JclStep, index: int) -> str:
        """Generate a Step bean for a JCL step"""
        
        if step.step_type == StepType.COBOL_PROGRAM:
            tasklet_class = f"{step.program}Tasklet"
            return f'''
    @Bean
    public Step step{index}() {{
        return new StepBuilder("{step.name}", jobRepository)
                .tasklet(new com.fordcredit.misc1099.batch.program.{tasklet_class}(), transactionManager)
                .build();
    }}
'''
        elif step.step_type == StepType.SORT:
            return f'''
    @Bean
    public Step step{index}() {{
        return new StepBuilder("{step.name}", jobRepository)
                .tasklet(new com.fordcredit.misc1099.batch.tasklet.SortTasklet("{step.name}"), transactionManager)
                .build();
    }}
'''
        elif step.step_type == StepType.IEBGENER:
            return f'''
    @Bean
    public Step step{index}() {{
        return new StepBuilder("{step.name}", jobRepository)
                .tasklet(new com.fordcredit.misc1099.batch.tasklet.IebgenerTasklet("{step.name}"), transactionManager)
                .build();
    }}
'''
        else:
            return ""


# =============================================================================
# MAIN PIPELINE
# =============================================================================

class MigrationPipeline:
    """Main orchestration pipeline"""
    
    def __init__(self):
        self.jcl_parser = JclParser()
        self.cobol_parser = CobolParser()
        self.llm = LLMClient()
        self.tasklet_gen = TaskletGenerator(self.llm)
        self.job_gen = JobConfigGenerator()
    
    def run(self, programs: List[str] = None):
        """Run the full migration pipeline"""
        
        print("=" * 60)
        print("COBOL to Spring Boot Migration Pipeline v2")
        print("=" * 60)
        
        # Step 1: Parse all JCL files
        print("\n[1] Parsing JCL files...")
        jcl_jobs = self._parse_all_jcl()
        
        for job in jcl_jobs:
            print(f"  - {job.name}: {len(job.steps)} steps, COBOL: {job.cobol_programs}")
        
        # Step 2: Identify unique COBOL programs
        all_cobol = set()
        for job in jcl_jobs:
            all_cobol.update(job.cobol_programs)
        
        if programs:
            all_cobol = all_cobol.intersection(set(programs))
        
        print(f"\n[2] COBOL programs to migrate: {sorted(all_cobol)}")
        
        # Step 3: Parse and translate each COBOL program
        print("\n[3] Translating COBOL programs...")
        for prog_name in sorted(all_cobol):
            self._migrate_program(prog_name)
        
        # Step 4: Generate Spring Batch Job configs
        print("\n[4] Generating Spring Batch Job configurations...")
        for job in jcl_jobs:
            self._generate_job_config(job)
        
        print("\n" + "=" * 60)
        print("Migration complete!")
        print("=" * 60)
    
    def _parse_all_jcl(self) -> List[JclJob]:
        """Parse all JCL files"""
        jobs = []
        for jcl_file in sorted(JCL_DIR.glob("*.txt")):
            job = self.jcl_parser.parse(jcl_file)
            jobs.append(job)
        return jobs
    
    def _migrate_program(self, prog_name: str):
        """Migrate a single COBOL program"""
        print(f"\n  Processing {prog_name}...")
        
        # Find COBOL source
        cobol_file = COBOL_DIR / f"{prog_name}.txt"
        if not cobol_file.exists():
            print(f"    WARNING: COBOL source not found: {cobol_file}")
            return
        
        # Parse COBOL
        program = self.cobol_parser.parse(cobol_file)
        print(f"    Found {len(program.paragraphs)} paragraphs, {len(program.file_selects)} files")
        
        # Generate Tasklet (with LLM translation)
        print(f"    Translating {len(program.paragraphs)} paragraphs with LLM...")
        tasklet_code = self.tasklet_gen.generate(program)
        
        # Write output
        output_path = OUTPUT_DIR / "batch" / "program" / f"{prog_name}Tasklet.java"
        output_path.parent.mkdir(parents=True, exist_ok=True)
        output_path.write_text(tasklet_code, encoding='utf-8')
        print(f"    Wrote: {output_path}")
    
    def _generate_job_config(self, job: JclJob):
        """Generate Spring Batch Job config"""
        config_code = self.job_gen.generate(job)
        
        class_name = f"{job.name.replace('@', '')}JobConfig"
        output_path = OUTPUT_DIR / "batch" / "config" / f"{class_name}.java"
        output_path.parent.mkdir(parents=True, exist_ok=True)
        output_path.write_text(config_code, encoding='utf-8')
        print(f"  Generated: {output_path}")


# =============================================================================
# CLI
# =============================================================================

def main():
    import argparse
    
    parser = argparse.ArgumentParser(description='COBOL to Spring Boot Migration Pipeline v2')
    parser.add_argument('--program', '-p', nargs='+', help='Specific programs to migrate')
    parser.add_argument('--list', '-l', action='store_true', help='List all programs')
    parser.add_argument('--jcl-only', action='store_true', help='Only parse JCL, no translation')
    parser.add_argument('--dry-run', action='store_true', help='Parse only, no LLM calls')
    
    args = parser.parse_args()
    
    if args.list:
        print("JCL Files:")
        for f in sorted(JCL_DIR.glob("*.txt")):
            print(f"  - {f.stem}")
        print("\nCOBOL Programs:")
        for f in sorted(COBOL_DIR.glob("*.txt")):
            print(f"  - {f.stem}")
        return
    
    pipeline = MigrationPipeline()
    
    if args.jcl_only:
        jobs = pipeline._parse_all_jcl()
        for job in jobs:
            print(f"\n{job.name}:")
            print(f"  Description: {job.description}")
            print(f"  Steps: {len(job.steps)}")
            print(f"  COBOL Programs: {job.cobol_programs}")
            for step in job.steps:
                print(f"    - {step.name}: {step.step_type.name} {step.program or ''}")
    else:
        pipeline.run(programs=args.program)


if __name__ == "__main__":
    main()

