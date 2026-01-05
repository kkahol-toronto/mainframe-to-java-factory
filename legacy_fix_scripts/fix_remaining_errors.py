#!/usr/bin/env python3
"""
Fix Remaining Compilation Errors
=================================
Fixes syntax errors, missing fields, and method calls
"""

import re
from pathlib import Path

BASE_DIR = Path(__file__).parent
JAVA_FILE = BASE_DIR / "misc-1099" / "src" / "main" / "java" / "com" / "fordcredit" / "misc1099" / "batch" / "program" / "CCAC6320Tasklet.java"

def fix_newline_syntax(code: str) -> str:
    """Fix .write.newLine() syntax errors"""
    
    # Fix: state.xxx.write.newLine() -> state.xxx.newLine()
    code = re.sub(
        r'(\w+\.write)\.newLine\(\);',
        r'\1.newLine();',
        code
    )
    
    # Actually, the pattern is wrong - let me fix it properly
    code = re.sub(
        r'(\w+FileWriter)\.write\.newLine\(\);',
        r'\1.newLine();',
        code
    )
    
    # Remove duplicate newLine() calls
    code = re.sub(
        r'(\w+FileWriter)\.write\([^)]+\);\n\s+\1\.write\.newLine\(\);\n\s+\1\.newLine\(\);',
        lambda m: m.group(1) + '.write(' + re.search(r'write\(([^)]+)\)', m.group(0)).group(1) + ');\n            ' + m.group(1) + '.newLine();',
        code
    )
    
    return code

def fix_indentation_issues(code: str) -> str:
    """Fix indentation issues"""
    
    # Fix: missing indentation after try
    code = re.sub(
        r'(try \{\n\s+// INITIALIZE WS-T04R-DEFT-RCD\n)\s+state\.',
        r'\1        state.',
        code
    )
    
    return code

def fix_missing_braces(code: str) -> str:
    """Fix missing closing braces"""
    
    # Fix: missing closing brace in p7300DeftReadRcd
    code = re.sub(
        r'(state\.ten99T04rDeftEof = true;\n\s+)\} else \{',
        r'\1            } else {',
        code
    )
    
    return code

def fix_write_method_calls(code: str) -> str:
    """Fix write method calls that don't exist"""
    
    # Fix: writeTen99T01wTransOutputRcd(state.wsT01wOutputTransTrl) -> writer.write()
    replacements = [
        (r'writeTen99T01wTransOutputRcd\(state\.wsT01wOutputTransTrl\);',
         'if (state.ten99T01wTransOutputFileWriter != null && state.wsT01wOutputTransTrl != null) {\n            state.ten99T01wTransOutputFileWriter.write(state.wsT01wOutputTransTrl);\n            state.ten99T01wTransOutputFileWriter.newLine();\n        }'),
        (r'writeTen99T02wRejCycleOutRcd\(state\.wsT02wRejectRecyclingTrl\);',
         'if (state.ten99T02wRejCycleOutFileWriter != null && state.wsT02wRejectRecyclingTrl != null) {\n            state.ten99T02wRejCycleOutFileWriter.write(state.wsT02wRejectRecyclingTrl);\n            state.ten99T02wRejCycleOutFileWriter.newLine();\n        }'),
        (r'writeTen99T03wForeignOutputRcd\(state\.wsT03wForOutputTrl\);',
         'if (state.ten99T03wForeignOutputFileWriter != null && state.wsT03wForOutputTrl != null) {\n            state.ten99T03wForeignOutputFileWriter.write(state.wsT03wForOutputTrl);\n            state.ten99T03wForeignOutputFileWriter.newLine();\n        }'),
        (r'writeTen99T04wExcludesRcd\(state\.wsT04wExcludesTrl\);',
         'if (state.ten99T04wExcludesFileWriter != null && state.wsT04wExcludesTrl != null) {\n            state.ten99T04wExcludesFileWriter.write(state.wsT04wExcludesTrl);\n            state.ten99T04wExcludesFileWriter.newLine();\n        }'),
        (r'writeTen99C01wRejRptOutputRcd\(state\.wsC01wRejRptOutputTrl\);',
         'if (state.ten99C01wRejRptOutputFileWriter != null && state.wsC01wRejRptOutputTrl != null) {\n            state.ten99C01wRejRptOutputFileWriter.write(state.wsC01wRejRptOutputTrl);\n            state.ten99C01wRejRptOutputFileWriter.newLine();\n        }'),
    ]
    
    for pattern, replacement in replacements:
        code = re.sub(pattern, replacement, code)
    
    return code

def fix_sarParagraph_type(code: str) -> str:
    """Fix sarParagraph type mismatch"""
    
    # Fix: state.sarParagraph = state.litNoControlCard where sarParagraph is String but litNoControlCard might be BigDecimal[]
    # Actually, sarParagraph should be String, so String.valueOf() if needed
    code = re.sub(
        r'state\.sarParagraph = state\.(lit\w+);',
        lambda m: f'state.sarParagraph = String.valueOf(state.{m.group(1)});',
        code
    )
    
    return code

def add_missing_trailer_fields(code: str) -> str:
    """Add missing trailer fields"""
    
    # Check if fields exist
    if 'wsT01wOutputTransTrl' not in code:
        # Find a good place to add them (after header fields)
        pattern = r'(String wsC01wRejRptOutputHdr = "";\n)'
        new_fields = '''String wsT01wOutputTransTrl = "";
        String wsT02wRejectRecyclingTrl = "";
        String wsT03wForOutputTrl = "";
        String wsT04wExcludesTrl = "";
        String wsC01wRejRptOutputTrl = "";
'''
        code = re.sub(pattern, r'\1' + new_fields, code)
    
    return code

def fix_p8040_newline(code: str) -> str:
    """Fix p8040 newline syntax"""
    
    # Fix the specific line
    code = re.sub(
        r'state\.ten99T03wForeignOutputFileWriter\.write\(state\.wsT01wOutputTransRcd\);\n\s+state\.ten99T03wForeignOutputFileWriter\.write\.newLine\(\);',
        'state.ten99T03wForeignOutputFileWriter.write(state.wsT01wOutputTransRcd);\n            state.ten99T03wForeignOutputFileWriter.newLine();',
        code
    )
    
    return code

def main():
    print("=" * 60)
    print("Fixing Remaining Compilation Errors")
    print("=" * 60)
    
    code = JAVA_FILE.read_text(encoding='utf-8')
    original_code = code
    
    print("Fixing newline syntax...")
    code = fix_newline_syntax(code)
    
    print("Fixing indentation...")
    code = fix_indentation_issues(code)
    
    print("Fixing missing braces...")
    code = fix_missing_braces(code)
    
    print("Fixing write method calls...")
    code = fix_write_method_calls(code)
    
    print("Fixing sarParagraph type...")
    code = fix_sarParagraph_type(code)
    
    print("Adding missing trailer fields...")
    code = add_missing_trailer_fields(code)
    
    print("Fixing p8040 newline...")
    code = fix_p8040_newline(code)
    
    if code != original_code:
        JAVA_FILE.write_text(code, encoding='utf-8')
        print("✅ Fixed remaining errors!")
    else:
        print("⚠️ No changes needed")
    
    print("=" * 60)

if __name__ == "__main__":
    main()

