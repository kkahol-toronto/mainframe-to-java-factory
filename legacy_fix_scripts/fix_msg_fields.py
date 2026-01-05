#!/usr/bin/env python3
"""
Fix MSG Field Types
===================
Fixes msg* fields that are incorrectly declared as arrays
"""

import re
from pathlib import Path

BASE_DIR = Path(__file__).parent
JAVA_FILE = BASE_DIR / "misc-1099" / "src" / "main" / "java" / "com" / "fordcredit" / "misc1099" / "batch" / "program" / "CCAC6320Tasklet.java"

def fix_msg_fields(code: str) -> str:
    """Fix msg* field declarations"""
    
    # Fields that should be BigDecimal, not BigDecimal[]
    bigdecimal_fields = [
        'msgMiscFileAmount',
        'msgTrailerCount',
        'msgFileCount',
        'msgAmount',
        'msgTrailerAmount',
        'msgMiscFileAmt',
    ]
    
    # Fields that should be String, not BigDecimal[]
    string_fields = [
        'msgMiscFormSummary',
        'msgTrailerCountDisplay',
        'msgNoData',
        'msgNoDataFileName',
        'msgFileName',
        'msgLiteral',
        'msgControlTotals',
        'msgBadFileMsg',
        'msgFileId',
        'msgOutputFileName',
        'msgNumRcds',
        'litTransTlsDoNotAgree',
        'litForMiscellaneousInput',
    ]
    
    for field in bigdecimal_fields:
        # Fix BigDecimal[] to BigDecimal
        pattern = rf'java\.math\.BigDecimal\[\]\s+{field}\s*=\s*new\s+java\.math\.BigDecimal\[[^\]]+\];'
        replacement = f'java.math.BigDecimal {field} = java.math.BigDecimal.ZERO;'
        code = re.sub(pattern, replacement, code)
        
        # Fix String to BigDecimal
        pattern2 = rf'String\s+{field}\s*=\s*"[^"]*";'
        code = re.sub(pattern2, replacement, code)
    
    for field in string_fields:
        # Fix BigDecimal[] to String
        pattern = rf'java\.math\.BigDecimal\[\]\s+{field}\s*=\s*new\s+java\.math\.BigDecimal\[[^\]]+\];'
        replacement = f'String {field} = "";'
        code = re.sub(pattern, replacement, code)
    
    return code

def main():
    print("=" * 60)
    print("Fixing MSG Field Types")
    print("=" * 60)
    
    code = JAVA_FILE.read_text(encoding='utf-8')
    original_code = code
    
    code = fix_msg_fields(code)
    
    if code != original_code:
        JAVA_FILE.write_text(code, encoding='utf-8')
        print("✅ Fixed MSG field types!")
    else:
        print("⚠️ No changes needed")
    
    print("=" * 60)

if __name__ == "__main__":
    main()

