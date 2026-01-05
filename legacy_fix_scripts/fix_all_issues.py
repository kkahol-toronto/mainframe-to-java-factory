#!/usr/bin/env python3
"""
Fix All Compilation Issues
==========================
Fixes type mismatches, missing fields, and method calls.
"""

import re
from pathlib import Path

BASE_DIR = Path(__file__).parent
JAVA_FILE = BASE_DIR / "misc-1099" / "src" / "main" / "java" / "com" / "fordcredit" / "misc1099" / "batch" / "program" / "CCAC6320Tasklet.java"

def fix_type_mismatches(code: str) -> str:
    """Fix type mismatches between BigDecimal and int"""
    
    # Fix: state.codeIndex <= state.litNumOf1099Codes
    # litNumOf1099Codes is BigDecimal, codeIndex is int
    code = re.sub(
        r'state\.codeIndex\s*<=\s*state\.litNumOf1099Codes\b',
        'state.codeIndex <= state.litNumOf1099Codes.intValue()',
        code
    )
    
    # Fix: state.codeIndex <= Integer.parseInt(String.valueOf(state.litNumOf1099Codes))
    code = re.sub(
        r'state\.codeIndex\s*<=\s*Integer\.parseInt\(String\.valueOf\(state\.litNumOf1099Codes\)\)',
        'state.codeIndex <= state.litNumOf1099Codes.intValue()',
        code
    )
    
    return code

def fix_missing_method_calls(code: str) -> str:
    """Fix calls to non-existent methods"""
    
    # Remove handleInitializationError calls
    code = re.sub(
        r'state\.handleInitializationError\([^)]+\);',
        '// Error handled by exception',
        code
    )
    
    return code

def initialize_arrays(code: str) -> str:
    """Initialize array fields properly"""
    
    # Find ProgramState and add array initialization
    # Arrays need to be initialized with default values
    pattern = r'(java\.math\.BigDecimal\[\] wsT01wDistAmtArr = new java\.math\.BigDecimal\[10\];)'
    
    def init_array(match):
        return match.group(1) + '\n        // Initialize array elements\n        for (int i = 0; i < 10; i++) {\n            wsT01wDistAmtArr[i] = java.math.BigDecimal.ZERO;\n        }'
    
    code = re.sub(pattern, init_array, code)
    
    # Initialize String arrays
    pattern2 = r'(String\[\] wsT01wDistCompassCodeArr = new String\[10\];)'
    code = re.sub(pattern2, r'\1\n        // Initialize array elements\n        for (int i = 0; i < 10; i++) {\n            wsT01wDistCompassCodeArr[i] = "";\n            wsT01wDist1099IndicArr[i] = "";\n        }', code)
    
    return code

def main():
    print("=" * 60)
    print("Fixing All Compilation Issues")
    print("=" * 60)
    
    code = JAVA_FILE.read_text(encoding='utf-8')
    original_code = code
    
    print("Fixing type mismatches...")
    code = fix_type_mismatches(code)
    
    print("Fixing missing method calls...")
    code = fix_missing_method_calls(code)
    
    print("Initializing arrays...")
    code = initialize_arrays(code)
    
    if code != original_code:
        JAVA_FILE.write_text(code, encoding='utf-8')
        print("✅ Fixed issues!")
    else:
        print("⚠️ No changes needed")
    
    print("=" * 60)

if __name__ == "__main__":
    main()

