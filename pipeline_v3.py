#!/usr/bin/env python3
"""
COBOL to Spring Boot Migration Pipeline v3
==========================================

A DETERMINISTIC approach that:
1. Generates compilable Java code (no LLM for method bodies)
2. Creates detailed TODO stubs with COBOL logic embedded as comments
3. Focuses on getting a working skeleton that can be incrementally enhanced

Philosophy: Get it compiling first, then enhance business logic incrementally.
"""

from __future__ import annotations

import json
import os
import re
from dataclasses import dataclass, field
from enum import Enum, auto
from pathlib import Path
from typing import Dict, List, Optional, Tuple

from dotenv import load_dotenv

load_dotenv()

# =============================================================================
# CONFIGURATION
# =============================================================================

WORKSPACE = Path(__file__).parent
MAINFRAME_DIR = WORKSPACE / "work" / "mainframe_clean"
JCL_DIR = MAINFRAME_DIR / "jcl"
COBOL_DIR = MAINFRAME_DIR / "cobol"
OUTPUT_DIR = WORKSPACE / "misc-1099" / "src" / "main" / "java" / "com" / "fordcredit" / "misc1099"
TESTCASE_DIR = MAINFRAME_DIR / "testcases"


# =============================================================================
# DATA STRUCTURES
# =============================================================================

class FileRole(Enum):
    MASTER_INPUT = auto()
    TRANSACTION_INPUT = auto()
    CONTROL_INPUT = auto()
    LOOKUP_INPUT = auto()
    MASTER_OUTPUT = auto()
    REPORT_OUTPUT = auto()
    REJECT_OUTPUT = auto()
    SYSOUT = auto()


@dataclass
class FileInfo:
    select_name: str
    assign_name: str
    role: FileRole
    java_var: str
    test_file: str


@dataclass
class CobolParagraph:
    name: str
    content: str
    performs: List[str]


@dataclass
class CobolProgram:
    program_id: str
    source_file: Path
    files: List[FileInfo]
    paragraphs: List[CobolParagraph]
    working_storage: str


# =============================================================================
# COBOL PARSER
# =============================================================================

class CobolParser:
    """Deterministic COBOL parser"""
    
    def parse(self, cobol_path: Path) -> CobolProgram:
        text = cobol_path.read_text(encoding='utf-8', errors='replace')
        
        # Extract program ID
        prog_match = re.search(r'PROGRAM-ID\.\s*(\w+)', text, re.IGNORECASE)
        program_id = prog_match.group(1).upper() if prog_match else cobol_path.stem
        
        # Parse files
        files = self._parse_files(text)
        
        # Parse paragraphs
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
            files=files,
            paragraphs=paragraphs,
            working_storage=working_storage
        )
    
    def _parse_files(self, text: str) -> List[FileInfo]:
        files = []
        pattern = re.compile(
            r'SELECT\s+([\w-]+)\s+ASSIGN\s+TO\s+([\w.-]+)',
            re.IGNORECASE
        )
        
        for match in pattern.finditer(text):
            select_name = match.group(1)
            assign_name = match.group(2)
            
            role = self._classify_file(select_name, assign_name)
            java_var = self._to_camel_case(select_name)
            test_file = self._determine_test_file(select_name, role)
            
            files.append(FileInfo(
                select_name=select_name,
                assign_name=assign_name,
                role=role,
                java_var=java_var,
                test_file=test_file
            ))
        
        return files
    
    def _classify_file(self, select_name: str, assign_name: str) -> FileRole:
        name = select_name.upper()
        assign = assign_name.upper()
        
        # Output files
        if 'W' in assign or 'OUT' in name:
            if 'DISPLAY' in name or 'E01W' in name or 'SYSOUT' in name:
                return FileRole.SYSOUT
            elif 'REJECT' in name:
                return FileRole.REJECT_OUTPUT
            elif 'REPORT' in name or 'SUMMARY' in name:
                return FileRole.REPORT_OUTPUT
            else:
                return FileRole.MASTER_OUTPUT
        
        # Input files
        if 'CONTROL' in name or 'R01R' in name:
            return FileRole.CONTROL_INPUT
        elif 'CORPORATE' in name:
            return FileRole.TRANSACTION_INPUT
        elif 'MASTER' in name:
            return FileRole.MASTER_INPUT
        elif 'TABLE' in name or 'CODE' in name:
            return FileRole.LOOKUP_INPUT
        else:
            return FileRole.TRANSACTION_INPUT
    
    def _determine_test_file(self, select_name: str, role: FileRole) -> str:
        name = select_name.lower()
        
        if 'master' in name:
            return 'master.txt' if role in (FileRole.MASTER_INPUT,) else 'master_out.txt'
        elif 'reject' in name:
            return 'reject.txt' if role != FileRole.REJECT_OUTPUT else 'reject_out.txt'
        elif 'vendor' in name:
            return 'vendor.txt' if role != FileRole.MASTER_OUTPUT else 'vendor_out.txt'
        elif 'control' in name:
            return 'control.txt'
        elif 'corporate' in name:
            return 'corporate.txt'
        elif 'summary' in name:
            return 'summary.txt'
        elif 'display' in name or 'sysout' in name.lower():
            return 'sysout.txt'
        else:
            java_var = self._to_camel_case(select_name)
            return f"{java_var}.txt"
    
    def _parse_paragraphs(self, text: str) -> List[CobolParagraph]:
        paragraphs = []
        
        proc_match = re.search(r'PROCEDURE\s+DIVISION\.', text, re.IGNORECASE)
        if not proc_match:
            return paragraphs
        
        proc_text = text[proc_match.end():]
        
        # Pattern for paragraph names
        para_pattern = re.compile(r'^(\d{4}-[\w-]+|\w+-[\w-]+)\s*\.\s*$', re.MULTILINE)
        
        matches = list(para_pattern.finditer(proc_text))
        
        for i, match in enumerate(matches):
            name = match.group(1)
            start = match.end()
            end = matches[i + 1].start() if i + 1 < len(matches) else len(proc_text)
            content = proc_text[start:end].strip()
            
            # Extract PERFORM statements
            performs = re.findall(r'PERFORM\s+([\w-]+)', content, re.IGNORECASE)
            
            paragraphs.append(CobolParagraph(
                name=name,
                content=content,
                performs=performs
            ))
        
        return paragraphs
    
    def _to_camel_case(self, name: str) -> str:
        parts = name.lower().replace('_', '-').split('-')
        return parts[0] + ''.join(p.capitalize() for p in parts[1:])


# =============================================================================
# TASKLET GENERATOR
# =============================================================================

class TaskletGenerator:
    """Generates deterministic, compilable Tasklets"""
    
    def generate(self, program: CobolProgram) -> str:
        class_name = f"{program.program_id}Tasklet"
        
        return f'''package com.fordcredit.misc1099.batch.program;

import org.springframework.batch.core.StepContribution;
import org.springframework.batch.core.scope.context.ChunkContext;
import org.springframework.batch.core.step.tasklet.Tasklet;
import org.springframework.batch.repeat.RepeatStatus;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

/**
 * Generated Tasklet for COBOL program {program.program_id}.
 * 
 * Original COBOL: {program.source_file.name}
 * Paragraphs: {len(program.paragraphs)}
 * Files: {len(program.files)}
 */
public class {class_name} implements Tasklet {{

{self._generate_state_class(program)}

    @Override
    public RepeatStatus execute(
            StepContribution contribution,
            ChunkContext chunkContext) {{
        ProgramState state = new ProgramState();
        mainline(state);
        return RepeatStatus.FINISHED;
    }}

{self._generate_mainline(program)}

{self._generate_file_io(program)}

{self._generate_paragraph_stubs(program)}
}}
'''
    
    def _generate_state_class(self, program: CobolProgram) -> str:
        lines = ["    static class ProgramState {"]
        
        # File readers/writers
        for f in program.files:
            if f.role in (FileRole.MASTER_INPUT, FileRole.TRANSACTION_INPUT, 
                          FileRole.CONTROL_INPUT, FileRole.LOOKUP_INPUT):
                lines.append(f"        BufferedReader {f.java_var}Reader;")
            else:
                lines.append(f"        BufferedWriter {f.java_var}Writer;")
            lines.append(f"        boolean {f.java_var}Eof = false;")
            lines.append(f"        String {f.java_var}Line;")
        
        # Common state
        lines.extend([
            "",
            "        // Processing state",
            "        boolean endOfInput = false;",
            "        int returnCode = 0;",
            "        int recordsRead = 0;",
            "        int recordsWritten = 0;",
            "",
            "        // Current record",
            "        String currentLine;",
            "    }",
        ])
        
        return "\n".join(lines)
    
    def _generate_mainline(self, program: CobolProgram) -> str:
        # Find the mainline paragraph (usually 0000-MAINLINE or similar)
        mainline_para = None
        for para in program.paragraphs:
            if 'MAINLINE' in para.name.upper() or para.name.startswith('0000'):
                mainline_para = para
                break
        
        if mainline_para:
            # Generate calls based on PERFORMs in mainline
            calls = []
            for p in mainline_para.performs:
                method_name = self._to_method_name(p)
                calls.append(f"        {method_name}(state);")
            
            call_block = "\n".join(calls) if calls else "        // TODO: Add main processing logic"
        else:
            call_block = """        openFiles(state);
        processFiles(state);
        closeFiles(state);"""
        
        return f"""    private void mainline(ProgramState state) {{
        try {{
{call_block}
        }} catch (Exception e) {{
            throw new RuntimeException("Error in mainline", e);
        }}
    }}
"""
    
    def _generate_file_io(self, program: CobolProgram) -> str:
        lines = []
        
        # openFiles
        lines.append("    private void openFiles(ProgramState state) {")
        lines.append("        try {")
        lines.append(f'            Path testDir = Paths.get("../work/mainframe_clean/testcases", "{program.program_id}");')
        lines.append("")
        
        for f in program.files:
            if f.role in (FileRole.MASTER_INPUT, FileRole.TRANSACTION_INPUT, 
                          FileRole.CONTROL_INPUT, FileRole.LOOKUP_INPUT):
                lines.append(f'            Path {f.java_var}Path = testDir.resolve("input/{f.test_file}");')
                lines.append(f'            if (Files.exists({f.java_var}Path)) {{')
                lines.append(f'                state.{f.java_var}Reader = Files.newBufferedReader({f.java_var}Path);')
                lines.append(f'            }}')
            else:
                lines.append(f'            Path {f.java_var}Path = testDir.resolve("output/{f.test_file}");')
                lines.append(f'            Files.createDirectories({f.java_var}Path.getParent());')
                lines.append(f'            state.{f.java_var}Writer = Files.newBufferedWriter({f.java_var}Path);')
        
        lines.append("        } catch (Exception e) {")
        lines.append('            throw new RuntimeException("Failed to open files", e);')
        lines.append("        }")
        lines.append("    }")
        lines.append("")
        
        # closeFiles
        lines.append("    private void closeFiles(ProgramState state) {")
        lines.append("        try {")
        for f in program.files:
            if f.role in (FileRole.MASTER_INPUT, FileRole.TRANSACTION_INPUT, 
                          FileRole.CONTROL_INPUT, FileRole.LOOKUP_INPUT):
                lines.append(f'            if (state.{f.java_var}Reader != null) state.{f.java_var}Reader.close();')
            else:
                lines.append(f'            if (state.{f.java_var}Writer != null) state.{f.java_var}Writer.close();')
        lines.append("        } catch (Exception e) {")
        lines.append('            throw new RuntimeException("Failed to close files", e);')
        lines.append("        }")
        lines.append("    }")
        lines.append("")
        
        # Read methods for input files
        for f in program.files:
            if f.role in (FileRole.MASTER_INPUT, FileRole.TRANSACTION_INPUT, 
                          FileRole.CONTROL_INPUT, FileRole.LOOKUP_INPUT):
                method_name = f"read{f.java_var[0].upper()}{f.java_var[1:]}"
                lines.append(f"    private void {method_name}(ProgramState state) {{")
                lines.append("        try {")
                lines.append(f"            if (state.{f.java_var}Reader == null) {{")
                lines.append(f"                state.{f.java_var}Eof = true;")
                lines.append("                return;")
                lines.append("            }")
                lines.append(f"            state.{f.java_var}Line = state.{f.java_var}Reader.readLine();")
                lines.append(f"            if (state.{f.java_var}Line == null) {{")
                lines.append(f"                state.{f.java_var}Eof = true;")
                lines.append("            }")
                lines.append("        } catch (Exception e) {")
                lines.append(f'            throw new RuntimeException("Error reading {f.select_name}", e);')
                lines.append("        }")
                lines.append("    }")
                lines.append("")
        
        # Write methods for output files
        for f in program.files:
            if f.role not in (FileRole.MASTER_INPUT, FileRole.TRANSACTION_INPUT, 
                              FileRole.CONTROL_INPUT, FileRole.LOOKUP_INPUT):
                method_name = f"write{f.java_var[0].upper()}{f.java_var[1:]}"
                lines.append(f"    private void {method_name}(ProgramState state, String line) {{")
                lines.append("        try {")
                lines.append(f"            if (state.{f.java_var}Writer != null && line != null) {{")
                lines.append(f"                state.{f.java_var}Writer.write(line);")
                lines.append(f"                state.{f.java_var}Writer.newLine();")
                lines.append("                state.recordsWritten++;")
                lines.append("            }")
                lines.append("        } catch (Exception e) {")
                lines.append(f'            throw new RuntimeException("Error writing {f.select_name}", e);')
                lines.append("        }")
                lines.append("    }")
                lines.append("")
        
        return "\n".join(lines)
    
    def _generate_paragraph_stubs(self, program: CobolProgram) -> str:
        lines = ["    // ======================================================"]
        lines.append("    // COBOL PARAGRAPH STUBS")
        lines.append("    // ======================================================")
        lines.append("")
        
        for para in program.paragraphs:
            method_name = self._to_method_name(para.name)
            
            # Create comment block with COBOL code
            cobol_lines = para.content.split('\n')[:20]  # First 20 lines
            comment_block = '\n'.join(f"     *   {line.rstrip()}" for line in cobol_lines)
            
            lines.append(f"    /**")
            lines.append(f"     * COBOL Paragraph: {para.name}")
            lines.append(f"     * ")
            lines.append(f"     * Original COBOL:")
            lines.append(comment_block)
            if len(para.content.split('\n')) > 20:
                lines.append(f"     *   ... ({len(para.content.split(chr(10))) - 20} more lines)")
            lines.append(f"     */")
            lines.append(f"    private void {method_name}(ProgramState state) {{")
            
            # Add PERFORM calls
            if para.performs:
                for p in para.performs:
                    sub_method = self._to_method_name(p)
                    lines.append(f"        // PERFORM {p}")
                    lines.append(f"        {sub_method}(state);")
            else:
                lines.append(f"        // TODO: Implement {para.name}")
            
            lines.append("    }")
            lines.append("")
        
        # Add a generic processFiles method if not defined
        lines.append("    private void processFiles(ProgramState state) {")
        lines.append("        // TODO: Implement main processing loop")
        lines.append("    }")
        
        return "\n".join(lines)
    
    def _to_method_name(self, para_name: str) -> str:
        """Convert COBOL paragraph name to Java method name"""
        # Remove leading numbers
        name = re.sub(r'^\d+-', '', para_name)
        parts = name.lower().split('-')
        return parts[0] + ''.join(p.capitalize() for p in parts[1:])


# =============================================================================
# RUNNER GENERATOR
# =============================================================================

class RunnerGenerator:
    """Generates test runners"""
    
    def generate(self, program_id: str) -> str:
        return f'''package com.fordcredit.misc1099.batch.runner;

/**
 * Test runner for {program_id}
 */
public class {program_id}Runner {{
    public static void main(String[] args) {{
        System.out.println("▶ Running {program_id}Tasklet...");
        try {{
            var tasklet = new com.fordcredit.misc1099.batch.program.{program_id}Tasklet();
            tasklet.execute(null, null);
            System.out.println("✔ {program_id}Tasklet completed successfully");
        }} catch (Exception e) {{
            System.err.println("✘ {program_id}Tasklet failed: " + e.getMessage());
            e.printStackTrace();
            System.exit(1);
        }}
    }}
}}
'''


# =============================================================================
# TESTCASE GENERATOR
# =============================================================================

class TestcaseGenerator:
    """Generates test case structure"""
    
    def generate(self, program: CobolProgram):
        test_dir = TESTCASE_DIR / program.program_id
        input_dir = test_dir / "input"
        output_dir = test_dir / "output"
        expected_dir = test_dir / "expected"
        
        input_dir.mkdir(parents=True, exist_ok=True)
        output_dir.mkdir(parents=True, exist_ok=True)
        expected_dir.mkdir(parents=True, exist_ok=True)
        
        # Create placeholder files
        for f in program.files:
            if f.role in (FileRole.MASTER_INPUT, FileRole.TRANSACTION_INPUT, 
                          FileRole.CONTROL_INPUT, FileRole.LOOKUP_INPUT):
                path = input_dir / f.test_file
                if not path.exists():
                    path.write_text(f"# Placeholder for {f.select_name}\n")
            else:
                path = expected_dir / f.test_file
                if not path.exists():
                    path.write_text("")
        
        print(f"    Created testcase structure: {test_dir}")


# =============================================================================
# MAIN PIPELINE
# =============================================================================

class MigrationPipeline:
    """Main orchestration"""
    
    def __init__(self):
        self.parser = CobolParser()
        self.tasklet_gen = TaskletGenerator()
        self.runner_gen = RunnerGenerator()
        self.testcase_gen = TestcaseGenerator()
    
    def run(self, programs: List[str] = None):
        print("=" * 60)
        print("COBOL to Spring Boot Migration Pipeline v3")
        print("=" * 60)
        
        # Find all COBOL programs
        all_programs = [f.stem for f in sorted(COBOL_DIR.glob("*.txt"))]
        
        if programs:
            all_programs = [p for p in all_programs if p in programs]
        
        print(f"\nPrograms to migrate: {all_programs}")
        
        for prog_name in all_programs:
            self._migrate_program(prog_name)
        
        print("\n" + "=" * 60)
        print("Migration complete!")
        print("=" * 60)
    
    def _migrate_program(self, prog_name: str):
        print(f"\n  Processing {prog_name}...")
        
        cobol_file = COBOL_DIR / f"{prog_name}.txt"
        if not cobol_file.exists():
            print(f"    WARNING: Not found: {cobol_file}")
            return
        
        # Parse
        program = self.parser.parse(cobol_file)
        print(f"    Paragraphs: {len(program.paragraphs)}, Files: {len(program.files)}")
        
        # Generate Tasklet
        tasklet_code = self.tasklet_gen.generate(program)
        tasklet_path = OUTPUT_DIR / "batch" / "program" / f"{prog_name}Tasklet.java"
        tasklet_path.parent.mkdir(parents=True, exist_ok=True)
        tasklet_path.write_text(tasklet_code, encoding='utf-8')
        print(f"    Wrote: {tasklet_path.name}")
        
        # Generate Runner
        runner_code = self.runner_gen.generate(prog_name)
        runner_path = OUTPUT_DIR / "batch" / "runner" / f"{prog_name}Runner.java"
        runner_path.parent.mkdir(parents=True, exist_ok=True)
        runner_path.write_text(runner_code, encoding='utf-8')
        print(f"    Wrote: {runner_path.name}")
        
        # Generate Testcase structure
        self.testcase_gen.generate(program)


# =============================================================================
# CLI
# =============================================================================

def main():
    import argparse
    
    parser = argparse.ArgumentParser(description='COBOL to Spring Boot Migration Pipeline v3')
    parser.add_argument('--program', '-p', nargs='+', help='Specific programs to migrate')
    parser.add_argument('--all', '-a', action='store_true', help='Migrate all programs')
    parser.add_argument('--list', '-l', action='store_true', help='List all programs')
    
    args = parser.parse_args()
    
    if args.list:
        print("COBOL Programs:")
        for f in sorted(COBOL_DIR.glob("*.txt")):
            print(f"  - {f.stem}")
        return
    
    pipeline = MigrationPipeline()
    
    if args.all:
        pipeline.run()
    elif args.program:
        pipeline.run(programs=args.program)
    else:
        print("Use --all to migrate all programs or --program <name> for specific ones")


if __name__ == "__main__":
    main()

