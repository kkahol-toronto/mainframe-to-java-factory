#!/usr/bin/env python3
"""
Fix Type Mismatches
==================
Fixes remaining type mismatches in CCAC6320Tasklet.java
"""

import re
from pathlib import Path

BASE_DIR = Path(__file__).parent
JAVA_FILE = BASE_DIR / "misc-1099" / "src" / "main" / "java" / "com" / "fordcredit" / "misc1099" / "batch" / "program" / "CCAC6320Tasklet.java"

def fix_type_mismatches(code: str) -> str:
    """Fix type mismatches based on usage patterns"""
    
    # Fields that should be String, not BigDecimal[]
    string_fields = [
        'wsT03rBccwRcd',
        'wsT04rDeftRcd',
        'wsT05rRejCycleRcd',
        'wsT01wOutputTransRcd',
        'wsT02wRejectRecyclingRcd',
        'wsT03wForOutputRcd',
        'wsT04wExcludesOutputRcd',
        'wsC01wRejRptOutputRcd',
        'wsT01wOutputDetail',
        'wsT02wRejectDetail',
        'wsC01wRejRptDetail',
        'rejectIndicators',
        'wsT01wDistCompassCode',
        'wsT01wDist1099Indic',
        'wsT01wDistAmt',
        'wsT03rBccwCompassCode',
        'wsT03rBccwDisbAmt',
        'wsT04rDeftCompassCode',
        'wsT04rDeftDisbAmt',
        'wsT01wOperLoc',
        'wsT01wBranch',
        'wsT01wCheckDate',
        'wsT01wCheckNum',
        'wsT01wTaxType',
        'wsT01wTinInd',
        'wsT01wState',
        'wsT01wNbrDistRcd',
        'wsT01w1099Amt',
        'wsT01wCheckNumKey',
        'wsT01rMiscFormCheckNumKey',
        'wsT01rMiscFormBranch',
        'wsT01wCheckDate',
        'wsT01wCheckNum',
        'wsT01wOperLoc',
        'wsT01wBranch',
        'wsT01wTaxType',
        'wsT01wTinInd',
        'wsT01wState',
        'wsT01wNbrDistRcd',
        'wsT01w1099Amt',
        'wsT01wDistCompassCode',
        'wsT01wDist1099Indic',
        'wsT01wDistAmt',
        'wsT03rBccwCompassCode',
        'wsT03rBccwDisbAmt',
        'wsT04rDeftCompassCode',
        'wsT04rDeftDisbAmt',
        'wsT01wOperLoc',
        'wsT01wBranch',
        'wsT01wCheckDate',
        'wsT01wCheckNum',
        'wsT01wTaxType',
        'wsT01wTinInd',
        'wsT01wState',
        'wsT01wNbrDistRcd',
        'wsT01w1099Amt',
    ]
    
    # Fix BigDecimal[] declarations to String
    for field in string_fields:
        pattern = rf'java\.math\.BigDecimal\[\]\s+{field}\s*=\s*new\s+java\.math\.BigDecimal\[[^\]]+\];'
        replacement = f'String {field} = "";'
        code = re.sub(pattern, replacement, code)
    
    # Fix boolean fields that were declared as BigDecimal[]
    boolean_fields = [
        'ten99T01rMiscFormEof',
        'ten99T03rBccwEof',
        'ten99T04rDeftEof',
        'ten99T05rRejCycleEof',
        'eofControlCard',
        'eof1099EntryTable',
        'endOfBccwHeaders',
        'endOfBccwDetail',
        'endOfDeftHeaders',
        'endOfDeftDetail',
        'endOfDeftDetailYes',
        'foreignIndicatorCdn',
        'foreignIndicatorPr',
    ]
    
    for field in boolean_fields:
        # Fix BigDecimal[] to boolean
        pattern = rf'java\.math\.BigDecimal\[\]\s+{field}\s*=\s*new\s+java\.math\.BigDecimal\[[^\]]+\];'
        replacement = f'boolean {field} = false;'
        code = re.sub(pattern, replacement, code)
        
        # Fix String to boolean
        pattern = rf'String\s+{field}\s*=\s*"[^"]*";'
        replacement = f'boolean {field} = false;'
        code = re.sub(pattern, replacement, code)
    
    # Fix assignments: String to BigDecimal[] -> String to String
    for field in string_fields:
        # Fix: state.field = "value" when field is BigDecimal[]
        pattern = rf'state\.{field}\s*=\s*"([^"]+)";'
        # This should already work if we fixed the declaration above
        
        # Fix: state.field = String.valueOf(...) when field is BigDecimal[]
        pattern2 = rf'state\.{field}\s*=\s*String\.valueOf\([^)]+\);'
        # This should already work if we fixed the declaration above
    
    # Fix boolean assignments
    for field in boolean_fields:
        # Fix: state.field = "Y" -> state.field = true
        code = re.sub(
            rf'state\.{field}\s*=\s*"Y";',
            f'state.{field} = true;',
            code
        )
        # Fix: state.field = "N" -> state.field = false
        code = re.sub(
            rf'state\.{field}\s*=\s*"N";',
            f'state.{field} = false;',
            code
        )
        # Fix: if ("Y".equals(state.field)) -> if (state.field)
        code = re.sub(
            rf'"Y"\.equals\(state\.{field}\)',
            f'state.{field}',
            code
        )
        # Fix: if ("N".equals(state.field)) -> if (!state.field)
        code = re.sub(
            rf'"N"\.equals\(state\.{field}\)',
            f'!state.{field}',
            code
        )
        # Fix: if (state.field != null && state.field.equals("Y")) -> if (state.field)
        code = re.sub(
            rf'state\.{field}\s*!=\s*null\s*&&\s*state\.{field}\.equals\("Y"\)',
            f'state.{field}',
            code
        )
    
    # Fix BigDecimal[] field access that should be scalar
    for field in string_fields:
        # Fix: state.field[index] -> state.field (if it's a scalar)
        # But we need to be careful - some might actually be arrays
        # Let's only fix if we see direct assignment without index
        pass
    
    # Fix specific known issues
    # wsT01wDistAmt should be BigDecimal[], not scalar
    code = re.sub(
        r'java\.math\.BigDecimal\s+wsT01wDistAmt\s*=\s*java\.math\.BigDecimal\.ZERO;',
        'java.math.BigDecimal[] wsT01wDistAmt = new java.math.BigDecimal[10];',
        code
    )
    
    # wsT01wDistCompassCode should be String[], not scalar
    code = re.sub(
        r'String\s+wsT01wDistCompassCode\s*=\s*"";',
        'String[] wsT01wDistCompassCode = new String[10];',
        code
    )
    
    # wsT01wDist1099Indic should be String[], not scalar
    code = re.sub(
        r'String\s+wsT01wDist1099Indic\s*=\s*"";',
        'String[] wsT01wDist1099Indic = new String[10];',
        code
    )
    
    return code

def fix_boolean_comparisons(code: str) -> str:
    """Fix boolean comparisons"""
    
    # Fix: if (state.field != null && state.field) -> if (state.field)
    code = re.sub(
        r'if\s*\(state\.(\w+)\s*!=\s*null\s*&&\s*state\.\1\)',
        r'if (state.\1)',
        code
    )
    
    # Fix: if (state.field == null || !state.field) -> if (!state.field)
    code = re.sub(
        r'if\s*\(state\.(\w+)\s*==\s*null\s*\|\|\s*!state\.\1\)',
        r'if (!state.\1)',
        code
    )
    
    return code

def main():
    print("=" * 60)
    print("Fixing Type Mismatches")
    print("=" * 60)
    
    code = JAVA_FILE.read_text(encoding='utf-8')
    original_code = code
    
    print("Fixing field type declarations...")
    code = fix_type_mismatches(code)
    
    print("Fixing boolean comparisons...")
    code = fix_boolean_comparisons(code)
    
    if code != original_code:
        JAVA_FILE.write_text(code, encoding='utf-8')
        print("✅ Fixed type mismatches!")
    else:
        print("⚠️ No changes needed")
    
    print("=" * 60)

if __name__ == "__main__":
    main()

