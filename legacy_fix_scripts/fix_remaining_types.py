#!/usr/bin/env python3
"""
Fix Remaining Type Mismatches
==============================
Fixes specific fields that are incorrectly declared as BigDecimal[]
"""

import re
from pathlib import Path

BASE_DIR = Path(__file__).parent
JAVA_FILE = BASE_DIR / "misc-1099" / "src" / "main" / "java" / "com" / "fordcredit" / "misc1099" / "batch" / "program" / "CCAC6320Tasklet.java"

def fix_field_declarations(code: str) -> str:
    """Fix specific field declarations"""
    
    # Fields that should be String, not BigDecimal[]
    string_fields_to_fix = {
        'ccE01wDisplayRcd': 'String',
        'wsT03wForSourceCode': 'String',
        'wsT03wForForeignCountry': 'String',
        'wsT03wForDetail': 'String',
        'wsT01wOperLoc': 'String',
        'wsT01wBranch': 'String',
        'wsT01wCheckDate': 'String',
        'wsT01wCheckNum': 'String',
        'wsT01wTaxType': 'String',
        'wsT01wTinInd': 'String',
        'wsT01wState': 'String',
        'wsT01wNbrDistRcd': 'java.math.BigDecimal',  # This should be BigDecimal, not String
        'wsT01w1099Amt': 'java.math.BigDecimal',
        'wsT01wDistCompassCode': 'String[]',  # Array
        'wsT01wDist1099Indic': 'String[]',  # Array
        'wsT01wDistAmt': 'java.math.BigDecimal[]',  # Array
        'wsT03rBccwCompassCode': 'String',
        'wsT03rBccwDisbAmt': 'java.math.BigDecimal',
        'wsT04rDeftCompassCode': 'String',
        'wsT04rDeftDisbAmt': 'java.math.BigDecimal',
        'wsT01wCheckNumKey': 'String',
        'wsT01rMiscFormCheckNumKey': 'String',
        'wsT01rMiscFormBranch': 'String',
        'wsT02wRejectSourceCode': 'String',
        'wsC01wRejRptSourceCode': 'String',
        'wsT02wRejectKeyOpLoc': 'String',
        'wsC01wRejRptKeyOpLoc': 'String',
        'wsT02wRejectKeyBrDeptCd': 'String',
        'wsC01wRejRptKeyBrDeptCd': 'String',
        'wsT02wRejectKeyCheckDt': 'String',
        'wsC01wRejRptKeyCheckDt': 'String',
        'wsT02wRejectKeyCheckNum': 'String',
        'wsC01wRejRptKeyCheckNum': 'String',
        'wsC01wRejRptTaxType': 'String',
        'wsT02wRejectTaxType': 'String',
        'wsC01wRejRptTinInd': 'String',
        'wsT02wRejectTinInd': 'String',
        'wsT02wRejectDetail': 'String',
        'wsC01wRejRptDetail': 'String',
    }
    
    for field, field_type in string_fields_to_fix.items():
        # Fix BigDecimal[] declarations
        pattern1 = rf'java\.math\.BigDecimal\[\]\s+{field}\s*=\s*new\s+java\.math\.BigDecimal\[[^\]]+\];'
        if field_type == 'String':
            replacement = f'String {field} = "";'
        elif field_type == 'String[]':
            replacement = f'String[] {field} = new String[10];'
        elif field_type == 'java.math.BigDecimal':
            replacement = f'java.math.BigDecimal {field} = java.math.BigDecimal.ZERO;'
        elif field_type == 'java.math.BigDecimal[]':
            replacement = f'java.math.BigDecimal[] {field} = new java.math.BigDecimal[10];'
        else:
            replacement = f'{field_type} {field} = "";'
        
        code = re.sub(pattern1, replacement, code)
        
        # Fix String declarations that should be BigDecimal
        if field_type == 'java.math.BigDecimal':
            pattern2 = rf'String\s+{field}\s*=\s*"[^"]*";'
            replacement = f'java.math.BigDecimal {field} = java.math.BigDecimal.ZERO;'
            code = re.sub(pattern2, replacement, code)
    
    # Fix boolean field that's being assigned to String
    code = re.sub(
        r'state\.foreignIndicatorCdn\s*=\s*"([^"]+)";',
        lambda m: f'state.foreignIndicatorCdn = {"true" if m.group(1) == "Y" else "false"};',
        code
    )
    
    code = re.sub(
        r'state\.foreignIndicatorPr\s*=\s*"([^"]+)";',
        lambda m: f'state.foreignIndicatorPr = {"true" if m.group(1) == "Y" else "false"};',
        code
    )
    
    return code

def fix_array_access(code: str) -> str:
    """Fix array access patterns"""
    
    # Fix: state.wsT01wDistAmt = value -> state.wsT01wDistAmt[index] = value
    # But we need to know which index - let's look for patterns
    
    # Fix: state.wsT01wDistCompassCode = value -> state.wsT01wDistCompassCode[index] = value
    # This is trickier - we need context
    
    return code

def main():
    print("=" * 60)
    print("Fixing Remaining Type Mismatches")
    print("=" * 60)
    
    code = JAVA_FILE.read_text(encoding='utf-8')
    original_code = code
    
    print("Fixing field declarations...")
    code = fix_field_declarations(code)
    
    print("Fixing array access...")
    code = fix_array_access(code)
    
    if code != original_code:
        JAVA_FILE.write_text(code, encoding='utf-8')
        print("✅ Fixed remaining type mismatches!")
    else:
        print("⚠️ No changes needed")
    
    print("=" * 60)

if __name__ == "__main__":
    main()

