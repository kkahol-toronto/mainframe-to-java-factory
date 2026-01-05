#!/usr/bin/env python3
"""
Fix Missing Fields
==================
Adds missing fields to ProgramState and fixes references.
"""

import re
from pathlib import Path

BASE_DIR = Path(__file__).parent
JAVA_FILE = BASE_DIR / "misc-1099" / "src" / "main" / "java" / "com" / "fordcredit" / "misc1099" / "batch" / "program" / "CCAC6320Tasklet.java"

def add_missing_fields(code: str) -> str:
    """Add missing fields to ProgramState"""
    
    # Fields that are referenced but missing
    missing_fields = [
        "int disbIndex = 0;",
        "int codeIndex = 0;",
        "boolean ten99T01rMiscFormEofYes = false;",
        "boolean ten99T03rBccwEofYes = false;",
        "boolean ten99T04rDeftEofYes = false;",
        "boolean ten99T05rRejCycleEofYes = false;",
        "boolean foreignIndicatorNo = true;",
        "boolean wsT03rValidTaxType = false;",
        "boolean canadianOpLoc = false;",
        "boolean wsUsInd = false;",
        "boolean wsCdnInd = false;",
        "String wsT01rMiscFormTrlValue = \"\";",
        "String wsT03rBccwHdrValue = \"\";",
        "String wsT03rBccwTrlValue = \"\";",
        "String wsT04rDeftHdrValue = \"\";",
        "String wsT04rDeftTrlValue = \"\";",
        "String wsT05rRejCycleTrlValue = \"\";",
        "String endOfBccwHeaders = \"\";",
        "String endOfBccwDetail = \"\";",
        "String endOfDeftHeaders = \"\";",
        "String endOfDeftDetail = \"\";",
        "java.math.BigDecimal[] wsT01wDistAmtArr = new java.math.BigDecimal[10];",
        "String[] wsT01wDistCompassCodeArr = new String[10];",
        "String[] wsT01wDist1099IndicArr = new String[10];",
        "String[] wsT07r1099EntryCodes = new String[100];",
    ]
    
    # Check which fields are missing
    fields_to_add = []
    for field in missing_fields:
        field_name = field.split()[1]  # Get variable name
        if field_name not in code or f" {field_name} " not in code:
            fields_to_add.append(f"        {field}")
    
    if not fields_to_add:
        return code
    
    # Insert before closing brace of ProgramState
    pattern = r'(\s+String highValues = "\\uFFFF\\uFFFF\\uFFFF\\uFFFF";\n\s+\})'
    
    fields_text = "\n".join(fields_to_add) + "\n"
    
    code = re.sub(pattern, r'\1'.replace(r'\1', fields_text + r'\1'), code)
    
    return code

def fix_high_values_references(code: str) -> str:
    """Fix HIGH_VALUES references to use state.highValues"""
    
    # Replace state.HIGH_VALUES with state.highValues (but not in declarations)
    code = re.sub(r'state\.HIGH_VALUES\b', 'state.highValues', code)
    
    # Replace standalone HIGH_VALUES with state.highValues (but not in constant declarations)
    # Don't replace if it's part of "private static final String HIGH_VALUES"
    code = re.sub(r'(?<!private static final String )\bHIGH_VALUES\b(?!\s*=)', 'state.highValues', code)
    
    return code

def main():
    print("=" * 60)
    print("Fixing Missing Fields in CCAC6320Tasklet.java")
    print("=" * 60)
    
    code = JAVA_FILE.read_text(encoding='utf-8')
    original_code = code
    
    print("Adding missing fields...")
    code = add_missing_fields(code)
    
    print("Fixing HIGH_VALUES references...")
    code = fix_high_values_references(code)
    
    if code != original_code:
        JAVA_FILE.write_text(code, encoding='utf-8')
        print("✅ Fixed missing fields!")
    else:
        print("⚠️ No changes needed")
    
    print("=" * 60)

if __name__ == "__main__":
    main()

