#!/usr/bin/env python3
"""
COBOL to Java Hybrid Pipeline (Option C+E)
==========================================

This pipeline combines:
- Option E: Complete Variable Extraction from ALL COBOL data definitions
- Option C: Template-generated structure with LLM for data logic only

Key Features:
1. Extracts ALL variables from WORKING-STORAGE, FILE SECTION, 88-levels
2. Template generates: method signatures, PERFORM chains, file I/O
3. LLM only translates: MOVE, IF/EVALUATE, COMPUTE, STRING operations
4. Preserves original COBOL as comments
"""

import os
import re
import json
from pathlib import Path
from dataclasses import dataclass, field
from typing import List, Dict, Tuple, Optional
from dotenv import load_dotenv
from openai import AzureOpenAI

load_dotenv()

# =============================================================================
# PATHS
# =============================================================================

BASE_DIR = Path(__file__).parent
COBOL_DIR = BASE_DIR / "work" / "mainframe_clean" / "cobol"
OUTPUT_DIR = BASE_DIR / "misc-1099" / "src" / "main" / "java" / "com" / "fordcredit" / "misc1099" / "batch"

# =============================================================================
# DATA CLASSES
# =============================================================================

@dataclass
class CobolVariable:
    """Represents a COBOL data item"""
    level: int
    name: str
    pic: str = ""
    value: str = ""
    occurs: int = 0
    redefines: str = ""
    is_group: bool = False
    children: List['CobolVariable'] = field(default_factory=list)
    conditions: Dict[str, str] = field(default_factory=dict)  # 88-level conditions
    
    @property
    def java_name(self) -> str:
        """Convert COBOL name to Java camelCase"""
        parts = self.name.lower().replace('_', '-').split('-')
        return parts[0] + ''.join(p.capitalize() for p in parts[1:])
    
    @property
    def java_type(self) -> str:
        """Determine Java type from PIC clause"""
        if self.is_group:
            return "String"
        if not self.pic:
            return "String"
        
        pic = self.pic.upper()
        # Numeric
        if re.match(r'^S?9', pic) or 'V' in pic:
            if 'V' in pic or pic.count('9') > 9:
                return "java.math.BigDecimal"
            return "int"
        # Alphanumeric
        return "String"
    
    @property  
    def java_default(self) -> str:
        """Get Java default value"""
        if self.value:
            val = self.value.strip().strip('"\'')
            if self.java_type == "int":
                return val if val.isdigit() else "0"
            if self.java_type == "java.math.BigDecimal":
                return f'new java.math.BigDecimal("{val}")' if val else "java.math.BigDecimal.ZERO"
            return f'"{val}"'
        
        if self.java_type == "int":
            return "0"
        if self.java_type == "java.math.BigDecimal":
            return "java.math.BigDecimal.ZERO"
        return '""'


@dataclass
class CobolStatement:
    """Represents a single COBOL statement"""
    type: str  # MOVE, IF, COMPUTE, PERFORM, READ, WRITE, etc.
    raw: str   # Original COBOL text
    parsed: Dict = field(default_factory=dict)  # Parsed components


@dataclass
class CobolParagraph:
    """Represents a COBOL paragraph"""
    name: str
    content: str
    statements: List[CobolStatement] = field(default_factory=list)
    performs: List[str] = field(default_factory=list)
    
    @property
    def java_name(self) -> str:
        name = re.sub(r'^\d+-', '', self.name)
        parts = name.lower().split('-')
        return parts[0] + ''.join(p.capitalize() for p in parts[1:])


@dataclass
class CobolFile:
    """Represents a COBOL file definition"""
    select_name: str
    fd_name: str
    record_name: str
    ddname: str = ""
    
    @property
    def java_var(self) -> str:
        parts = self.select_name.lower().replace('_', '-').split('-')
        name = parts[0] + ''.join(p.capitalize() for p in parts[1:])
        # Prefix numbers with 'ten'
        if name[0].isdigit():
            name = 'ten' + name
        return name


@dataclass
class CobolProgram:
    """Complete COBOL program representation"""
    program_id: str
    variables: List[CobolVariable] = field(default_factory=list)
    files: List[CobolFile] = field(default_factory=list)
    paragraphs: List[CobolParagraph] = field(default_factory=list)
    
    def get_variable(self, name: str) -> Optional[CobolVariable]:
        """Find variable by name"""
        for v in self.variables:
            if v.name.upper() == name.upper():
                return v
        return None


# =============================================================================
# COBOL PARSER - Enhanced for Complete Variable Extraction
# =============================================================================

class EnhancedCobolParser:
    """Parses COBOL with complete variable extraction"""
    
    def parse(self, cobol_path: Path) -> CobolProgram:
        content = cobol_path.read_text(encoding='utf-8', errors='ignore')
        
        program = CobolProgram(program_id=cobol_path.stem)
        
        # Parse all sections
        program.variables = self._parse_all_variables(content)
        program.files = self._parse_files(content)
        program.paragraphs = self._parse_paragraphs(content)
        
        return program
    
    def _parse_all_variables(self, content: str) -> List[CobolVariable]:
        """Extract ALL data items from WORKING-STORAGE and FILE SECTION"""
        variables = []
        seen_names = set()
        
        # Find WORKING-STORAGE SECTION
        ws_match = re.search(
            r'WORKING-STORAGE\s+SECTION\.(.*?)(?=PROCEDURE\s+DIVISION|LINKAGE\s+SECTION|$)',
            content, re.DOTALL | re.IGNORECASE
        )
        if ws_match:
            for var in self._parse_data_items(ws_match.group(1)):
                if var.name not in seen_names:
                    variables.append(var)
                    seen_names.add(var.name)
        
        # Find FILE SECTION
        file_match = re.search(
            r'FILE\s+SECTION\.(.*?)(?=WORKING-STORAGE|LINKAGE|PROCEDURE|$)',
            content, re.DOTALL | re.IGNORECASE
        )
        if file_match:
            for var in self._parse_data_items(file_match.group(1)):
                if var.name not in seen_names:
                    variables.append(var)
                    seen_names.add(var.name)
        
        return variables
    
    def _parse_data_items(self, section: str) -> List[CobolVariable]:
        """Parse data items from a section"""
        variables = []
        current_88_parent = None
        
        # Pattern for data items
        item_pattern = re.compile(
            r'^\s*(\d{2})\s+([A-Z0-9-]+)\s*(.*?)\.?\s*$',
            re.MULTILINE | re.IGNORECASE
        )
        
        for match in item_pattern.finditer(section):
            level = int(match.group(1))
            name = match.group(2).upper()
            rest = match.group(3).strip()
            
            # Skip FILLER
            if 'FILLER' in name:
                continue
            
            var = CobolVariable(level=level, name=name)
            
            # Parse PIC clause
            pic_match = re.search(r'PIC(?:TURE)?\s+IS\s+([^\s.]+)|PIC(?:TURE)?\s+([^\s.]+)', rest, re.IGNORECASE)
            if pic_match:
                var.pic = pic_match.group(1) or pic_match.group(2)
            else:
                var.is_group = level < 77 and level != 88
            
            # Parse VALUE clause
            val_match = re.search(r'VALUE\s+(?:IS\s+)?["\']?([^"\'.\s]+)["\']?', rest, re.IGNORECASE)
            if val_match:
                var.value = val_match.group(1)
            
            # Parse OCCURS clause
            occurs_match = re.search(r'OCCURS\s+(\d+)', rest, re.IGNORECASE)
            if occurs_match:
                var.occurs = int(occurs_match.group(1))
            
            # Handle 88-level conditions
            if level == 88 and current_88_parent:
                current_88_parent.conditions[name] = var.value
            else:
                variables.append(var)
                current_88_parent = var
        
        return variables
    
    def _parse_files(self, content: str) -> List[CobolFile]:
        """Parse file definitions"""
        files = []
        
        # Find SELECT statements
        select_pattern = re.compile(
            r'SELECT\s+([A-Z0-9-]+)\s+ASSIGN\s+TO\s+([A-Z0-9-]+)',
            re.IGNORECASE
        )
        
        for match in select_pattern.finditer(content):
            select_name = match.group(1)
            ddname = match.group(2)
            
            # Find corresponding FD
            fd_pattern = re.compile(
                rf'FD\s+{re.escape(select_name)}.*?01\s+([A-Z0-9-]+)',
                re.IGNORECASE | re.DOTALL
            )
            fd_match = fd_pattern.search(content)
            
            files.append(CobolFile(
                select_name=select_name,
                fd_name=select_name,
                record_name=fd_match.group(1) if fd_match else select_name + "-REC",
                ddname=ddname
            ))
        
        return files
    
    def _parse_paragraphs(self, content: str) -> List[CobolParagraph]:
        """Parse procedure division paragraphs with statement extraction"""
        paragraphs = []
        
        # Find PROCEDURE DIVISION
        proc_match = re.search(r'PROCEDURE\s+DIVISION.*?\.(.*)', content, re.DOTALL | re.IGNORECASE)
        if not proc_match:
            return paragraphs
        
        proc_content = proc_match.group(1)
        
        # Split into paragraphs
        para_pattern = re.compile(r'^([A-Z0-9-]+)\.\s*$', re.MULTILINE)
        matches = list(para_pattern.finditer(proc_content))
        
        for i, match in enumerate(matches):
            name = match.group(1)
            start = match.end()
            end = matches[i + 1].start() if i + 1 < len(matches) else len(proc_content)
            
            body = proc_content[start:end].strip()
            
            para = CobolParagraph(name=name, content=body)
            para.statements = self._parse_statements(body)
            para.performs = self._extract_performs(body)
            
            paragraphs.append(para)
        
        return paragraphs
    
    def _parse_statements(self, body: str) -> List[CobolStatement]:
        """Parse individual COBOL statements"""
        statements = []
        
        # Normalize the body
        body = re.sub(r'\n\s+', ' ', body)
        
        # Statement patterns
        patterns = [
            (r'MOVE\s+(.+?)\s+TO\s+([A-Z0-9-]+(?:\s*,\s*[A-Z0-9-]+)*)', 'MOVE'),
            (r'IF\s+(.+?)\s+(?:THEN\s+)?(.+?)(?:ELSE\s+(.+?))?END-IF', 'IF'),
            (r'EVALUATE\s+(.+?)END-EVALUATE', 'EVALUATE'),
            (r'COMPUTE\s+([A-Z0-9-]+)\s*=\s*(.+)', 'COMPUTE'),
            (r'ADD\s+(.+?)\s+TO\s+([A-Z0-9-]+)', 'ADD'),
            (r'SUBTRACT\s+(.+?)\s+FROM\s+([A-Z0-9-]+)', 'SUBTRACT'),
            (r'MULTIPLY\s+(.+?)\s+BY\s+([A-Z0-9-]+)', 'MULTIPLY'),
            (r'DIVIDE\s+(.+?)\s+INTO\s+([A-Z0-9-]+)', 'DIVIDE'),
            (r'PERFORM\s+([A-Z0-9-]+)(?:\s+UNTIL\s+(.+))?', 'PERFORM'),
            (r'READ\s+([A-Z0-9-]+)', 'READ'),
            (r'WRITE\s+([A-Z0-9-]+)', 'WRITE'),
            (r'STRING\s+(.+?)\s+INTO\s+([A-Z0-9-]+)', 'STRING'),
            (r'INSPECT\s+([A-Z0-9-]+)\s+(.+)', 'INSPECT'),
            (r'SET\s+([A-Z0-9-]+)\s+TO\s+(.+)', 'SET'),
        ]
        
        for pattern, stmt_type in patterns:
            for match in re.finditer(pattern, body, re.IGNORECASE | re.DOTALL):
                statements.append(CobolStatement(
                    type=stmt_type,
                    raw=match.group(0),
                    parsed={'groups': match.groups()}
                ))
        
        return statements
    
    def _extract_performs(self, body: str) -> List[str]:
        """Extract PERFORM targets"""
        performs = []
        for match in re.finditer(r'PERFORM\s+([A-Z0-9-]+)', body, re.IGNORECASE):
            target = match.group(1).upper()
            if target not in ('UNTIL', 'VARYING', 'TIMES', 'THRU', 'THROUGH'):
                performs.append(target)
        return performs


# =============================================================================
# LLM CLIENT - Focused on Statement Translation
# =============================================================================

class FocusedLLMClient:
    """LLM client for translating individual COBOL statements"""
    
    def __init__(self):
        self.client = AzureOpenAI(
            api_key=os.getenv("AZURE_OPENAI_API_KEY"),
            api_version="2024-02-15-preview",
            azure_endpoint=os.getenv("AZURE_OPENAI_ENDPOINT")
        )
        self.deployment = os.getenv("AZURE_OPENAI_DEPLOYMENT", "gpt-4o")
    
    def translate_paragraph_logic(
        self, 
        paragraph: CobolParagraph,
        variable_map: Dict[str, str],  # COBOL name -> Java type
        method_list: List[str]  # Available methods
    ) -> str:
        """Translate only the data manipulation logic in a paragraph"""
        
        # Build context
        var_context = "\n".join(f"  state.{self._to_java_name(k)} : {v}" for k, v in list(variable_map.items())[:100])
        method_context = ", ".join(method_list[:30])
        
        prompt = f"""Translate the COBOL paragraph logic to Java. 
Output ONLY the method body statements (no method signature, no braces).

AVAILABLE VARIABLES (access via state.xxx):
{var_context}

AVAILABLE METHODS: {method_context}

COBOL PARAGRAPH: {paragraph.name}
```cobol
{paragraph.content}
```

TRANSLATION RULES:
1. Variables: state.variableName (camelCase)
2. MOVE A TO B → state.b = state.a;
3. ADD A TO B → state.b = state.b + state.a; (use int or String operations as appropriate)
4. IF condition → if (condition) {{ }}
5. PERFORM PARA-NAME → paraName(state);
6. PERFORM PARA UNTIL cond → while (!cond) {{ para(state); }}
7. READ FILE-NAME → readFileName(state);
8. WRITE REC → writeRecName(state);
9. Comparisons: "VALUE".equals(state.field) for String, state.field == value for int
10. 88-level conditions: check the parent variable's value
11. HIGH-VALUES → "\\uFFFF", LOW-VALUES → "\\u0000", SPACES → " "

OUTPUT ONLY JAVA STATEMENTS - no explanation, no method signature."""

        try:
            response = self.client.chat.completions.create(
                model=self.deployment,
                messages=[
                    {"role": "system", "content": "You are a COBOL to Java translator. Output only valid Java statements. No markdown, no explanation, no method signature."},
                    {"role": "user", "content": prompt}
                ],
                temperature=0.1,
                max_tokens=2000
            )
            
            result = response.choices[0].message.content
            return self._clean_output(result)
        except Exception as e:
            print(f"    LLM error: {e}")
            return "// LLM translation failed - manual implementation required"
    
    def _to_java_name(self, name: str) -> str:
        parts = name.lower().replace('_', '-').split('-')
        return parts[0] + ''.join(p.capitalize() for p in parts[1:])
    
    def _clean_output(self, text: str) -> str:
        # Remove markdown fences
        text = re.sub(r'```java\s*', '', text)
        text = re.sub(r'```\s*', '', text)
        # Remove method signature if present
        text = re.sub(r'^\s*(?:private|public)\s+void\s+\w+\s*\([^)]*\)\s*\{?\s*', '', text)
        # Remove trailing brace
        text = re.sub(r'\}\s*$', '', text)
        return text.strip()


# =============================================================================
# HYBRID TASKLET GENERATOR
# =============================================================================

class HybridTaskletGenerator:
    """Generates Tasklets using template + focused LLM approach"""
    
    def __init__(self, llm: FocusedLLMClient):
        self.llm = llm
    
    def generate(self, program: CobolProgram) -> str:
        """Generate complete Tasklet with real business logic"""
        
        # Build variable map for LLM context
        print(f"    Building variable map ({len(program.variables)} variables)...")
        var_map = {v.name: v.java_type for v in program.variables}
        
        # Get list of available methods
        method_list = [p.java_name for p in program.paragraphs]
        method_list.extend([f"read{f.java_var.capitalize()}" for f in program.files])
        method_list.extend([f"write{f.java_var.capitalize()}" for f in program.files])
        
        # Generate translated methods
        print(f"    Translating {len(program.paragraphs)} paragraphs...")
        translated_methods = []
        for i, para in enumerate(program.paragraphs):
            print(f"      [{i+1}/{len(program.paragraphs)}] {para.name}")
            method = self._generate_method(para, var_map, method_list, program)
            translated_methods.append(method)
        
        # Assemble the complete Tasklet
        print(f"    Assembling Tasklet...")
        code = self._assemble(program, translated_methods)
        
        # Harden the code
        print(f"    Hardening code...")
        code = self._harden(code, program)
        
        return code
    
    def _generate_method(
        self, 
        para: CobolParagraph, 
        var_map: Dict[str, str],
        method_list: List[str],
        program: CobolProgram
    ) -> str:
        """Generate a single method with template structure + LLM logic"""
        
        # Get translated logic from LLM
        logic = self.llm.translate_paragraph_logic(para, var_map, method_list)
        
        # Build the complete method with COBOL as comments
        cobol_lines = para.content.split('\n')
        cobol_comments = '\n'.join(f"        // {line.strip()}" for line in cobol_lines if line.strip())
        
        # Clean and balance the logic
        logic = self._balance_braces(logic)
        
        # Indent the translated logic
        logic_lines = logic.split('\n')
        indented_logic = '\n'.join(f"            {line}" for line in logic_lines if line.strip())
        
        return f"""
    private void {para.java_name}(ProgramState state) {{
        // ===== Original COBOL: {para.name} =====
{cobol_comments}
        // ===== End COBOL =====
        
        try {{
{indented_logic}
        }} catch (Exception e) {{
            throw new RuntimeException("Error in {para.java_name}: " + e.getMessage(), e);
        }}
    }}
"""
    
    def _balance_braces(self, code: str) -> str:
        """Ensure braces are balanced in the code"""
        # Count open and close braces
        open_count = code.count('{')
        close_count = code.count('}')
        
        # Add missing closing braces
        if open_count > close_count:
            code = code.rstrip() + '\n' + '}\n' * (open_count - close_count)
        
        # Remove excess closing braces
        if close_count > open_count:
            excess = close_count - open_count
            lines = code.split('\n')
            result = []
            removed = 0
            for line in reversed(lines):
                if removed < excess and line.strip() == '}':
                    removed += 1
                    continue
                result.insert(0, line)
            code = '\n'.join(result)
        
        return code
    
    def _assemble(self, program: CobolProgram, methods: List[str]) -> str:
        """Assemble the complete Tasklet class"""
        
        lines = [
            "package com.fordcredit.misc1099.batch.program;",
            "",
            "import org.springframework.batch.core.StepContribution;",
            "import org.springframework.batch.core.scope.context.ChunkContext;",
            "import org.springframework.batch.core.step.tasklet.Tasklet;",
            "import org.springframework.batch.repeat.RepeatStatus;",
            "import java.io.*;",
            "import java.nio.file.*;",
            "import java.math.BigDecimal;",
            "",
            f"public class {program.program_id}Tasklet implements Tasklet {{",
            "",
            "    private final String basePath;",
            "",
            f"    public {program.program_id}Tasklet(String basePath) {{",
            "        this.basePath = basePath;",
            "    }",
            "",
        ]
        
        # Generate ProgramState class with ALL variables
        lines.append("    // =========== PROGRAM STATE ===========")
        lines.append("    static class ProgramState {")
        lines.append("        // File I/O")
        
        for f in program.files:
            lines.append(f"        BufferedReader {f.java_var}Reader;")
            lines.append(f"        String {f.java_var}Line;")
            lines.append(f"        boolean {f.java_var}Eof = false;")
            lines.append(f"        BufferedWriter {f.java_var}Writer;")
        
        lines.append("")
        lines.append("        // WORKING-STORAGE variables")
        
        for var in program.variables:
            if var.occurs > 0:
                lines.append(f"        {var.java_type}[] {var.java_name} = new {var.java_type}[{var.occurs}];")
            else:
                lines.append(f"        {var.java_type} {var.java_name} = {var.java_default};")
            
            # Add 88-level condition comments
            for cond_name, cond_val in var.conditions.items():
                java_cond = self._to_java_name(cond_name)
                lines.append(f"        // 88 {java_cond}: {var.java_name}.equals(\"{cond_val}\")")
        
        lines.append("    }")
        lines.append("")
        
        # Execute method
        lines.append("    @Override")
        lines.append("    public RepeatStatus execute(StepContribution contribution, ChunkContext chunkContext) throws Exception {")
        lines.append("        ProgramState state = new ProgramState();")
        lines.append("        mainline(state);")
        lines.append("        return RepeatStatus.FINISHED;")
        lines.append("    }")
        lines.append("")
        
        # File I/O methods
        lines.append("    // =========== FILE I/O ===========")
        for f in program.files:
            # Read method
            lines.append(f"    private void read{f.java_var.capitalize()}(ProgramState state) {{")
            lines.append(f"        try {{")
            lines.append(f"            if (state.{f.java_var}Reader == null) {{ state.{f.java_var}Eof = true; return; }}")
            lines.append(f"            state.{f.java_var}Line = state.{f.java_var}Reader.readLine();")
            lines.append(f"            if (state.{f.java_var}Line == null) state.{f.java_var}Eof = true;")
            lines.append(f"        }} catch (Exception e) {{ throw new RuntimeException(\"Read error\", e); }}")
            lines.append(f"    }}")
            lines.append("")
            
            # Write method
            lines.append(f"    private void write{f.java_var.capitalize()}(ProgramState state, String line) {{")
            lines.append(f"        try {{")
            lines.append(f"            if (state.{f.java_var}Writer != null && line != null) {{")
            lines.append(f"                state.{f.java_var}Writer.write(line);")
            lines.append(f"                state.{f.java_var}Writer.newLine();")
            lines.append(f"            }}")
            lines.append(f"        }} catch (Exception e) {{ throw new RuntimeException(\"Write error\", e); }}")
            lines.append(f"    }}")
            lines.append("")
        
        # Add translated methods
        lines.append("    // =========== BUSINESS LOGIC ===========")
        for method in methods:
            lines.append(method)
        
        # Close class
        lines.append("}")
        
        return "\n".join(lines)
    
    def _to_java_name(self, name: str) -> str:
        parts = name.lower().replace('_', '-').split('-')
        return parts[0] + ''.join(p.capitalize() for p in parts[1:])
    
    def _harden(self, code: str, program: CobolProgram) -> str:
        """Fix common issues in generated code"""
        
        # Fix method names starting with numbers
        code = re.sub(r'(private void )(\d+)', r'\1num\2', code)
        code = re.sub(r'\b(\d+)([a-zA-Z]\w*)\(state\)', r'num\1\2(state)', code)
        
        # Fix leading zeros in numeric literals (Java treats as octal)
        code = re.sub(r'= 0(\d+);', r'= \1;', code)
        
        # Fix unbalanced braces in try-catch blocks
        code = self._fix_try_catch_balance(code)
        
        # Find all state.xxx references and ensure they exist
        state_refs = set(re.findall(r'state\.(\w+)', code))
        
        # Find existing state fields (match types like String, int, java.math.BigDecimal, etc.)
        state_match = re.search(r'static class ProgramState \{(.*?)\n    \}', code, re.DOTALL)
        if state_match:
            # Match field declarations with various type patterns
            existing_fields = set(re.findall(r'\s+(?:\w+\.)*\w+(?:\[\])?\s+(\w+)\s*[=;]', state_match.group(1)))
            missing = state_refs - existing_fields
            
            # Filter out method calls and known non-fields
            skip = {'length', 'charAt', 'equals', 'toString', 'trim', 'split', 'substring', 
                    'compareTo', 'isEmpty', 'startsWith', 'endsWith', 'indexOf', 'valueOf'}
            missing = {m for m in missing if m not in skip and not m.endswith('Reader') and not m.endswith('Writer')}
            
            if missing:
                missing_decls = [f'        String {field} = "";' for field in sorted(missing)]
                insert_pos = state_match.end() - 5
                code = code[:insert_pos] + '\n        // Auto-added fields\n' + '\n'.join(missing_decls) + '\n' + code[insert_pos:]
        
        return code
    
    def _fix_try_catch_balance(self, code: str) -> str:
        """Fix unbalanced braces in try-catch blocks within methods"""
        # Find all methods and fix each one
        method_pattern = re.compile(
            r'(private void \w+\(ProgramState state\) \{.*?try \{)(.*?)(} catch \(Exception e\) \{.*?throw new RuntimeException.*?\}.*?\})',
            re.DOTALL
        )
        
        def fix_method(match):
            prefix = match.group(1)
            body = match.group(2)
            suffix = match.group(3)
            
            # Count braces in body
            open_count = body.count('{')
            close_count = body.count('}')
            
            # Add missing closing braces before catch
            if open_count > close_count:
                missing = open_count - close_count
                body = body.rstrip() + '\n' + '        }\n' * missing
            
            return prefix + body + suffix
        
        code = method_pattern.sub(fix_method, code)
        return code


# =============================================================================
# RUNNER GENERATOR
# =============================================================================

class RunnerGenerator:
    """Generates standalone runner classes for testing"""
    
    def generate(self, program: CobolProgram) -> str:
        return f'''package com.fordcredit.misc1099.batch.runner;

import com.fordcredit.misc1099.batch.program.{program.program_id}Tasklet;

public class {program.program_id}Runner {{
    public static void main(String[] args) {{
        String basePath = args.length > 0 ? args[0] : "work/mainframe_clean/testcases/{program.program_id}";
        {program.program_id}Tasklet tasklet = new {program.program_id}Tasklet(basePath);
        try {{
            tasklet.execute(null, null);
            System.out.println("{program.program_id} completed successfully");
        }} catch (Exception e) {{
            System.err.println("{program.program_id} failed: " + e.getMessage());
            e.printStackTrace();
            System.exit(1);
        }}
    }}
}}
'''


# =============================================================================
# MAIN PIPELINE
# =============================================================================

class HybridPipeline:
    """Main pipeline orchestrator"""
    
    def __init__(self):
        self.parser = EnhancedCobolParser()
        self.llm = FocusedLLMClient()
        self.tasklet_gen = HybridTaskletGenerator(self.llm)
        self.runner_gen = RunnerGenerator()
    
    def run(self, programs: List[str] = None):
        print("=" * 60)
        print("Hybrid COBOL to Java Pipeline (C+E)")
        print("Template Structure + Focused LLM Translation")
        print("=" * 60)
        
        all_programs = [f.stem for f in sorted(COBOL_DIR.glob("*.txt"))]
        if programs:
            all_programs = [p for p in all_programs if p in programs]
        
        print(f"\nPrograms: {all_programs}\n")
        
        for prog in all_programs:
            self._migrate(prog)
        
        print("\n" + "=" * 60)
        print("Complete!")
        print("=" * 60)
    
    def _migrate(self, program_name: str):
        print(f"\n[{program_name}]")
        
        cobol_path = COBOL_DIR / f"{program_name}.txt"
        if not cobol_path.exists():
            print(f"  SKIP: {cobol_path} not found")
            return
        
        # Parse COBOL
        print(f"  Parsing COBOL...")
        program = self.parser.parse(cobol_path)
        print(f"    Found {len(program.variables)} variables, {len(program.files)} files, {len(program.paragraphs)} paragraphs")
        
        # Generate Tasklet
        print(f"  Generating Tasklet...")
        tasklet_code = self.tasklet_gen.generate(program)
        
        # Write Tasklet
        tasklet_path = OUTPUT_DIR / "program" / f"{program_name}Tasklet.java"
        tasklet_path.parent.mkdir(parents=True, exist_ok=True)
        tasklet_path.write_text(tasklet_code, encoding='utf-8')
        print(f"  Wrote: {tasklet_path.name}")
        
        # Generate Runner
        runner_code = self.runner_gen.generate(program)
        runner_path = OUTPUT_DIR / "runner" / f"{program_name}Runner.java"
        runner_path.parent.mkdir(parents=True, exist_ok=True)
        runner_path.write_text(runner_code, encoding='utf-8')
        print(f"  Wrote: {runner_path.name}")


if __name__ == "__main__":
    import argparse
    parser = argparse.ArgumentParser(description="Hybrid COBOL to Java Pipeline")
    parser.add_argument('--program', '-p', nargs='+', help='Specific programs to process')
    parser.add_argument('--all', '-a', action='store_true', help='Process all programs')
    args = parser.parse_args()
    
    pipeline = HybridPipeline()
    
    if args.all:
        pipeline.run()
    elif args.program:
        pipeline.run(args.program)
    else:
        # Default: run just one program for testing
        pipeline.run(["CCAC6340"])

