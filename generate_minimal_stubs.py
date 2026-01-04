#!/usr/bin/env python3
"""
Generate Minimal Compiling Stubs
================================
Creates the simplest possible stubs that will definitely compile.
"""

import os
import re
from pathlib import Path
from dotenv import load_dotenv
from openai import AzureOpenAI

load_dotenv()

BASE_DIR = Path(__file__).parent
COBOL_DIR = BASE_DIR / "work" / "mainframe_clean" / "cobol"
JAVA_DIR = BASE_DIR / "misc-1099" / "src" / "main" / "java" / "com" / "fordcredit" / "misc1099" / "batch" / "program"

def to_java_name(cobol_name: str) -> str:
    """Convert COBOL name to Java camelCase"""
    parts = cobol_name.lower().replace('-', '_').split('_')
    result = parts[0] + ''.join(p.capitalize() for p in parts[1:])
    # Java method names can't start with a number
    if result and result[0].isdigit():
        result = 'p' + result  # prefix with 'p' for paragraph
    return result

def extract_paragraphs(cobol_file: Path) -> list:
    """Extract COBOL paragraph names"""
    content = cobol_file.read_text(encoding='utf-8', errors='ignore')
    
    # Find PROCEDURE DIVISION
    proc_match = re.search(r'PROCEDURE\s+DIVISION', content, re.IGNORECASE)
    if not proc_match:
        return []
    
    proc_content = content[proc_match.end():]
    
    # Find paragraphs (at start of line, format: 0000-NAME.)
    pattern = r'^([0-9][A-Z0-9-]+)\.'
    paragraphs = []
    
    for match in re.finditer(pattern, proc_content, re.MULTILINE):
        name = match.group(1)
        if name and not any(kw in name for kw in ['SECTION', 'COPY', 'EJECT']):
            paragraphs.append(name)
    
    return paragraphs

def extract_variables(cobol_file: Path) -> dict:
    """Extract WORKING-STORAGE variables"""
    content = cobol_file.read_text(encoding='utf-8', errors='ignore')
    
    variables = {}
    
    # Find numeric PIC patterns
    for match in re.finditer(r'^\s+\d+\s+([A-Z0-9-]+)\s+PIC\s+([9S()\d\-VZ,.]+)', content, re.MULTILINE | re.IGNORECASE):
        name = match.group(1)
        java_name = to_java_name(name)
        variables[java_name] = 'java.math.BigDecimal'
    
    # Find alphanumeric PIC patterns
    for match in re.finditer(r'^\s+\d+\s+([A-Z0-9-]+)\s+PIC\s+X', content, re.MULTILINE | re.IGNORECASE):
        name = match.group(1)
        java_name = to_java_name(name)
        if java_name not in variables:
            variables[java_name] = 'String'
    
    return variables

def generate_tasklet(program_name: str):
    """Generate minimal compiling Tasklet"""
    
    cobol_file = COBOL_DIR / f"{program_name}.txt"
    if not cobol_file.exists():
        print(f"COBOL file not found: {cobol_file}")
        return
    
    paragraphs = extract_paragraphs(cobol_file)
    variables = extract_variables(cobol_file)
    
    print(f"Found {len(paragraphs)} paragraphs, {len(variables)} variables")
    
    # Build ProgramState fields
    state_fields = []
    for name, java_type in variables.items():
        if java_type == 'java.math.BigDecimal':
            state_fields.append(f'        {java_type} {name} = java.math.BigDecimal.ZERO;')
        else:
            state_fields.append(f'        {java_type} {name} = "";')
    
    # Add common fields
    state_fields.extend([
        '        // File readers/writers',
        '        java.io.BufferedReader inputReader = null;',
        '        java.io.BufferedWriter outputWriter = null;',
        '        String inputLine = "";',
        '        String outputLine = "";',
        '        boolean endOfFile = false;',
        '        // Counters and flags',
        '        int recordCount = 0;',
        '        String returnCode = "0";',
    ])
    
    # Build methods
    methods = []
    for para in paragraphs:
        java_method = to_java_name(para)
        methods.append(f'''
    private void {java_method}(ProgramState state) {{
        // ===== Original COBOL: {para} =====
        // TODO: Implement business logic from COBOL paragraph
    }}
''')
    
    # Build main process
    main_calls = []
    if paragraphs:
        # Call first paragraph (usually initialization)
        main_calls.append(f'            {to_java_name(paragraphs[0])}(state);')
    
    # Build Java class
    java_code = f'''package com.fordcredit.misc1099.batch.program;

import org.springframework.batch.core.StepContribution;
import org.springframework.batch.core.scope.context.ChunkContext;
import org.springframework.batch.core.step.tasklet.Tasklet;
import org.springframework.batch.repeat.RepeatStatus;

public class {program_name}Tasklet implements Tasklet {{

    private final String basePath;

    public {program_name}Tasklet(String basePath) {{
        this.basePath = basePath;
    }}

    @Override
    public RepeatStatus execute(StepContribution contribution, ChunkContext chunkContext) throws Exception {{
        ProgramState state = new ProgramState();
        mainProcess(state);
        return RepeatStatus.FINISHED;
    }}

    private void mainProcess(ProgramState state) throws Exception {{
        try {{
{chr(10).join(main_calls) if main_calls else "            // Call paragraphs here"}
        }} catch (Exception e) {{
            throw new RuntimeException("Error in mainProcess: " + e.getMessage(), e);
        }}
    }}

    // =========================================================================
    // PROGRAM STATE
    // =========================================================================
    
    static class ProgramState {{
{chr(10).join(state_fields)}
    }}

    // =========================================================================
    // PARAGRAPH METHODS (57 paragraphs)
    // =========================================================================
{"".join(methods)}
}}
'''
    
    # Write file
    output_file = JAVA_DIR / f"{program_name}Tasklet.java"
    output_file.write_text(java_code, encoding='utf-8')
    print(f"Wrote: {output_file}")

if __name__ == "__main__":
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument('--program', '-p', required=True, help='Program name')
    args = parser.parse_args()
    
    generate_tasklet(args.program)

