#!/usr/bin/env python3
"""
Fix All Compilation Errors
==========================
Comprehensive fix for all remaining compilation errors in CCAC6320Tasklet.java
"""

import re
from pathlib import Path

BASE_DIR = Path(__file__).parent
JAVA_FILE = BASE_DIR / "misc-1099" / "src" / "main" / "java" / "com" / "fordcredit" / "misc1099" / "batch" / "program" / "CCAC6320Tasklet.java"

def fix_file_writer_calls(code: str) -> str:
    """Fix incorrect file writer calls"""
    
    # Fix: state.ten99T03wForeignOutputFile.write -> state.ten99T03wForeignOutputFileWriter.write
    replacements = [
        (r'state\.ten99T03wForeignOutputFile\.write\(', 'state.ten99T03wForeignOutputFileWriter.write('),
        (r'state\.ten99T01wTransOutputFile\.write\(', 'state.ten99T01wTransOutputFileWriter.write('),
        (r'state\.ten99T02wRejCycleOutFile\.write\(', 'state.ten99T02wRejCycleOutFileWriter.write('),
        (r'state\.ten99T04wExcludesFile\.write\(', 'state.ten99T04wExcludesFileWriter.write('),
        (r'state\.ten99C01wRejRptOutputFile\.write\(', 'state.ten99C01wRejRptOutputFileWriter.write('),
    ]
    
    for pattern, replacement in replacements:
        code = re.sub(pattern, replacement, code)
    
    # Add newLine() after write() calls
    code = re.sub(
        r'(state\.ten99\w+FileWriter\.write\([^)]+\));',
        r'\1;\n            state.\1.newLine();',
        code
    )
    
    # Fix the newLine pattern properly
    code = re.sub(
        r'(state\.ten99\w+FileWriter\.write\([^)]+\));\n\s+state\.\1\.newLine\(\);',
        lambda m: m.group(1) + ';\n            ' + m.group(1).split('(')[0] + '.newLine();',
        code
    )
    
    return code

def fix_initialize_calls(code: str) -> str:
    """Fix .initialize() calls on String fields"""
    
    # Remove .initialize() calls on String fields
    code = re.sub(
        r'state\.(\w+)\.initialize\(\);',
        lambda m: f'state.{m.group(1)} = "";',
        code
    )
    
    return code

def fix_missing_fields(code: str) -> str:
    """Add missing header fields"""
    
    # Check if fields exist
    if 'wsT01wOutputTransHdr' not in code:
        # Find ProgramState closing brace
        pattern = r'(String wsT01wOutputHdrId = "";\n)'
        new_fields = '''String wsT01wOutputTransHdr = "";
        String wsT02wRejectRecyclingHdr = "";
        String wsT03wForOutputHdr = "";
        String wsT04wExcludesHdr = "";
        String wsC01wRejRptOutputHdr = "";
'''
        code = re.sub(pattern, r'\1' + new_fields, code)
    
    return code

def fix_wsT07r1099EntryCode_usage(code: str) -> str:
    """Fix wsT07r1099EntryCode array access"""
    
    # Fix: state.wsT07r1099EntryCode.equals -> state.wsT07r1099EntryCode[state.codeIndex].equals
    code = re.sub(
        r'state\.wsT07r1099EntryCode\.equals\(',
        'state.wsT07r1099EntryCode[state.codeIndex].equals(',
        code
    )
    
    code = re.sub(
        r'state\.wsT07r1099EntryCode\s*!=',
        'state.wsT07r1099EntryCode[state.codeIndex] !=',
        code
    )
    
    return code

def fix_p8010_write_implementation(code: str) -> str:
    """Fix p8010WriteTransRcd implementation"""
    
    pattern = r'(// WRITE TEN99-T01W-TRANS-OUTPUT-RCD FROM WS-T01W-OUTPUT-TRANS-RCD\n\s+// TODO: implement writeTen99T01wTransOutputRcdFromWsT01wOutputTransRcd\n\s+// INITIALIZE WS-T01W-OUTPUT-TRANS-RCD\n\s+// TODO: implement initializeWsT01wOutputTransRcd\n\s+// INITIALIZE REJECT-INDICATORS\n\s+// TODO: implement initializeRejectIndicators)'
    
    replacement = '''// WRITE TEN99-T01W-TRANS-OUTPUT-RCD FROM WS-T01W-OUTPUT-TRANS-RCD
        if (state.ten99T01wTransOutputFileWriter != null && state.wsT01wOutputTransRcd != null) {
            state.ten99T01wTransOutputFileWriter.write(state.wsT01wOutputTransRcd);
            state.ten99T01wTransOutputFileWriter.newLine();
        }
        // INITIALIZE WS-T01W-OUTPUT-TRANS-RCD
        state.wsT01wOutputTransRcd = "";
        // INITIALIZE REJECT-INDICATORS
        state.rejectIndicators = "";'''
    
    code = re.sub(pattern, replacement, code)
    
    return code

def fix_p8040_write_implementation(code: str) -> str:
    """Fix p8040WriteForTransRcd implementation"""
    
    pattern = r'(// WRITE TEN99-T03W-FOREIGN-OUTPUT-RCD FROM WS-T01W-OUTPUT-TRANS-RCD\n\s+state\.ten99T03wForeignOutputFile\.write\(state\.wsT01wOutputTransRcd\);\n\s+\n\s+// INITIALIZE WS-T01W-OUTPUT-TRANS-RCD\n\s+state\.wsT01wOutputTransRcd\.initialize\(\);\n\s+\n\s+// INITIALIZE REJECT-INDICATORS\n\s+state\.rejectIndicators\.initialize\(\);)'
    
    replacement = '''// WRITE TEN99-T03W-FOREIGN-OUTPUT-RCD FROM WS-T01W-OUTPUT-TRANS-RCD
        if (state.ten99T03wForeignOutputFileWriter != null && state.wsT01wOutputTransRcd != null) {
            state.ten99T03wForeignOutputFileWriter.write(state.wsT01wOutputTransRcd);
            state.ten99T03wForeignOutputFileWriter.newLine();
        }
        
        // INITIALIZE WS-T01W-OUTPUT-TRANS-RCD
        state.wsT01wOutputTransRcd = "";
        
        // INITIALIZE REJECT-INDICATORS
        state.rejectIndicators = "";'''
    
    code = re.sub(pattern, replacement, code)
    
    return code

def fix_p8050_write_implementation(code: str) -> str:
    """Fix p8050WriteExcludesRcd implementation"""
    
    pattern = r'(// WRITE TEN99-T04W-EXCLUDES-RCD FROM WS-T01W-OUTPUT-TRANS-RCD\n\s+// TODO: implement writeTen99T04wExcludesRcd\(state\.wsT01wOutputTransRcd\)\n\s+// INITIALIZE WS-T01W-OUTPUT-TRANS-RCD\n\s+// TODO: implement initializeWsT01wOutputTransRcd\(\)\n\s+// INITIALIZE REJECT-INDICATORS\n\s+// TODO: implement initializeRejectIndicators\(\))'
    
    replacement = '''// WRITE TEN99-T04W-EXCLUDES-RCD FROM WS-T01W-OUTPUT-TRANS-RCD
        if (state.ten99T04wExcludesFileWriter != null && state.wsT01wOutputTransRcd != null) {
            state.ten99T04wExcludesFileWriter.write(state.wsT01wOutputTransRcd);
            state.ten99T04wExcludesFileWriter.newLine();
        }
        // INITIALIZE WS-T01W-OUTPUT-TRANS-RCD
        state.wsT01wOutputTransRcd = "";
        // INITIALIZE REJECT-INDICATORS
        state.rejectIndicators = "";'''
    
    code = re.sub(pattern, replacement, code)
    
    return code

def fix_p7910_read_implementation(code: str) -> str:
    """Fix p7910Read1099entryTable implementation"""
    
    pattern = r'(// Simulate COBOL READ \.\.\. INTO \.\.\. AT END\n\s+boolean atEnd = false;\n\s+// TODO: unknown method \'readWsT07r1099EntryCdFile\' - cannot fix, comment out\n\s+// Object record = state\.readWsT07r1099EntryCdFile\(\); // Assume this method reads next record or returns null if at end\n\s+// TODO: unknown variable \'record\' - cannot fix, comment out\n\s+// if \(record != null\) \{\n\s+//     state\.wsT07r1099EntryCodes\[state\.codeIndex\] = record;\n\s+// \} else \{\n\s+//     atEnd = true;\n\s+//     state\.eof1099EntryTable = true;\n\s+// \}\n\s+\n\s+// IF WS-T07R-1099ENTRY-CODE \(CODE-INDEX\) = HIGH-VALUES\n\s+if \(state\.wsT07r1099EntryCode != null &&\n\s+state\.wsT07r1099EntryCode\.equals\(state\.highValues\)\) \{)'
    
    replacement = '''// Simulate COBOL READ ... INTO ... AT END
        if (state.wsT07r1099entryCdFileReader != null) {
            String line = state.wsT07r1099entryCdFileReader.readLine();
            if (line != null && state.codeIndex < state.wsT07r1099EntryCodes.length) {
                state.wsT07r1099EntryCodes[state.codeIndex] = line;
            } else {
                state.eof1099EntryTable = true;
            }
        } else {
            state.eof1099EntryTable = true;
        }
        
        // IF WS-T07R-1099ENTRY-CODE (CODE-INDEX) = HIGH-VALUES
        if (state.codeIndex < state.wsT07r1099EntryCodes.length &&
            state.wsT07r1099EntryCodes[state.codeIndex] != null &&
            state.wsT07r1099EntryCodes[state.codeIndex].equals(state.highValues)) {'''
    
    code = re.sub(pattern, replacement, code)
    
    return code

def main():
    print("=" * 60)
    print("Fixing All Compilation Errors")
    print("=" * 60)
    
    code = JAVA_FILE.read_text(encoding='utf-8')
    original_code = code
    
    print("Fixing file writer calls...")
    code = fix_file_writer_calls(code)
    
    print("Fixing initialize() calls...")
    code = fix_initialize_calls(code)
    
    print("Adding missing header fields...")
    code = fix_missing_fields(code)
    
    print("Fixing wsT07r1099EntryCode usage...")
    code = fix_wsT07r1099EntryCode_usage(code)
    
    print("Fixing p8010WriteTransRcd...")
    code = fix_p8010_write_implementation(code)
    
    print("Fixing p8040WriteForTransRcd...")
    code = fix_p8040_write_implementation(code)
    
    print("Fixing p8050WriteExcludesRcd...")
    code = fix_p8050_write_implementation(code)
    
    print("Fixing p7910Read1099entryTable...")
    code = fix_p7910_read_implementation(code)
    
    if code != original_code:
        JAVA_FILE.write_text(code, encoding='utf-8')
        print("✅ Fixed compilation errors!")
    else:
        print("⚠️ No changes needed")
    
    print("=" * 60)

if __name__ == "__main__":
    main()

