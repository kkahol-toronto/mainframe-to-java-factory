#!/usr/bin/env python3
"""
Fix File I/O Method Calls
==========================
Replaces non-existent method calls with proper file I/O code.
"""

import re
from pathlib import Path

BASE_DIR = Path(__file__).parent
JAVA_FILE = BASE_DIR / "misc-1099" / "src" / "main" / "java" / "com" / "fordcredit" / "misc1099" / "batch" / "program" / "CCAC6320Tasklet.java"

# File name mappings (COBOL name -> actual file path)
FILE_MAPPINGS = {
    "TEN99-T01R-MISC-TRANS-FILE": "ten99T01rMiscTransFile.txt",
    "TEN99-T03R-BCCW-FILE": "ten99T03rBccwFile.txt",
    "TEN99-T04R-DEFT-FILE": "ten99T04rDeftFile.txt",
    "TEN99-T05R-REJ-CYCLE-FILE": "ten99T05rRejCycleFile.txt",
    "WS-T07R-1099ENTRY-CD-FILE": "wsT07r1099entryCdFile.txt",
    "CC-R01R-CONTROL-CARD": "control.txt",
    "TEN99-T01W-TRANS-OUTPUT-FILE": "ten99T01wTransOutputFile.txt",
    "TEN99-T02W-REJ-CYCLE-OUT-FILE": "ten99T02wRejCycleOutFile.txt",
    "TEN99-C01W-REJ-RPT-OUTPUT-FILE": "ten99C01wRejRptOutputFile.txt",
    "TEN99-T03W-FOREIGN-OUTPUT-FILE": "ten99T03wForeignOutputFile.txt",
    "TEN99-T04W-EXCLUDES-FILE": "ten99T04wExcludesFile.txt",
    "CC-E01W-DISPLAY-FILE": "sysout.txt",
}

def to_java_var_name(cobol_name: str) -> str:
    """Convert COBOL file name to Java variable name"""
    # Remove hyphens and convert to camelCase
    parts = cobol_name.replace('-', ' ').split()
    return parts[0].lower() + ''.join(p.capitalize() for p in parts[1:])

def fix_open_input_file(code: str) -> str:
    """Replace state.openInputFile() calls with proper BufferedReader initialization"""
    
    pattern = r'(\s+)state\.openInputFile\("([^"]+)"\);'
    
    def replace(match):
        indent = match.group(1)
        cobol_name = match.group(2)
        file_path = FILE_MAPPINGS.get(cobol_name, cobol_name.lower().replace('-', '') + ".txt")
        var_name = to_java_var_name(cobol_name) + "Reader"
        
        return f'''{indent}try {{
{indent}    java.nio.file.Path {var_name}Path = java.nio.file.Paths.get(basePath, "input", "{file_path}");
{indent}    state.{var_name} = java.nio.file.Files.newBufferedReader({var_name}Path);
{indent}}} catch (java.io.IOException e) {{
{indent}    throw new RuntimeException("Failed to open input file {file_path}: " + e.getMessage(), e);
{indent}}}'''
    
    return re.sub(pattern, replace, code)

def fix_open_output_file(code: str) -> str:
    """Replace state.openOutputFile() calls with proper BufferedWriter initialization"""
    
    pattern = r'(\s+)state\.openOutputFile\("([^"]+)"\);'
    
    def replace(match):
        indent = match.group(1)
        cobol_name = match.group(2)
        file_path = FILE_MAPPINGS.get(cobol_name, cobol_name.lower().replace('-', '') + ".txt")
        var_name = to_java_var_name(cobol_name) + "Writer"
        
        return f'''{indent}try {{
{indent}    java.nio.file.Path {var_name}Path = java.nio.file.Paths.get(basePath, "output", "{file_path}");
{indent}    java.nio.file.Files.createDirectories({var_name}Path.getParent());
{indent}    state.{var_name} = java.nio.file.Files.newBufferedWriter({var_name}Path);
{indent}}} catch (java.io.IOException e) {{
{indent}    throw new RuntimeException("Failed to open output file {file_path}: " + e.getMessage(), e);
{indent}}}'''
    
    return re.sub(pattern, replace, code)

def fix_initialize_calls(code: str) -> str:
    """Replace state.initializeXxx() calls - remove them for now as they're not needed"""
    
    # Pattern: state.initializeWsT01wOutputTransRcd();
    pattern = r'(\s+)state\.initialize(\w+)\(\);'
    
    def replace(match):
        indent = match.group(1)
        record_name = match.group(2)
        # Remove - record initialization is handled by default values in ProgramState
        return f'{indent}// Initialize {record_name} - using default values'
    
    return re.sub(pattern, replace, code)

def add_file_fields_to_state(code: str) -> str:
    """Add BufferedReader/BufferedWriter fields to ProgramState"""
    
    # Check if fields already exist
    if 'ten99T01rMiscTransFileReader' in code and 'java.io.BufferedReader ten99T01rMiscTransFileReader' in code:
        return code  # Already added
    
    # Find where to insert (replace the generic inputReader/outputWriter with specific ones)
    pattern = r'(// File readers/writers\n\s+java\.io\.BufferedReader inputReader = null;\n\s+java\.io\.BufferedWriter outputWriter = null;)'
    
    file_fields = '''// File readers/writers
        // File readers
        java.io.BufferedReader ten99T01rMiscTransFileReader;
        java.io.BufferedReader ten99T03rBccwFileReader;
        java.io.BufferedReader ten99T04rDeftFileReader;
        java.io.BufferedReader ten99T05rRejCycleFileReader;
        java.io.BufferedReader wsT07r1099entryCdFileReader;
        java.io.BufferedReader ccR01rControlCardReader;
        
        // File writers
        java.io.BufferedWriter ten99T01wTransOutputFileWriter;
        java.io.BufferedWriter ten99T02wRejCycleOutFileWriter;
        java.io.BufferedWriter ten99C01wRejRptOutputFileWriter;
        java.io.BufferedWriter ten99T03wForeignOutputFileWriter;
        java.io.BufferedWriter ten99T04wExcludesFileWriter;
        java.io.BufferedWriter ccE01wDisplayFileWriter;
        
        // Legacy fields (kept for compatibility)
        java.io.BufferedReader inputReader = null;
        java.io.BufferedWriter outputWriter = null;'''
    
    # Replace the generic fields
    if re.search(pattern, code):
        code = re.sub(pattern, file_fields, code)
    else:
        # Fallback: insert before "String inputLine"
        pattern2 = r'(\s+// File readers/writers\n\s+java\.io\.BufferedReader inputReader = null;\n\s+java\.io\.BufferedWriter outputWriter = null;\n\s+String inputLine)'
        code = re.sub(pattern2, file_fields + r'\n        String inputLine', code)
    
    return code

def main():
    print("=" * 60)
    print("Fixing File I/O Method Calls in CCAC6320Tasklet.java")
    print("=" * 60)
    
    # Read file
    code = JAVA_FILE.read_text(encoding='utf-8')
    original_code = code
    
    # Fix openInputFile calls
    print("Fixing openInputFile() calls...")
    code = fix_open_input_file(code)
    
    # Fix openOutputFile calls
    print("Fixing openOutputFile() calls...")
    code = fix_open_output_file(code)
    
    # Fix initialize calls
    print("Fixing initialize() calls...")
    code = fix_initialize_calls(code)
    
    # Add file fields to ProgramState
    print("Adding file reader/writer fields to ProgramState...")
    code = add_file_fields_to_state(code)
    
    # Write back
    if code != original_code:
        JAVA_FILE.write_text(code, encoding='utf-8')
        print("✅ Fixed file I/O calls!")
    else:
        print("⚠️ No changes needed")
    
    print("=" * 60)

if __name__ == "__main__":
    main()

