#!/usr/bin/env python3
"""
Add All Missing Fields
======================
Extracts all field references and adds missing ones to ProgramState.
"""

import re
from pathlib import Path

BASE_DIR = Path(__file__).parent
JAVA_FILE = BASE_DIR / "misc-1099" / "src" / "main" / "java" / "com" / "fordcredit" / "misc1099" / "batch" / "program" / "CCAC6320Tasklet.java"

def extract_field_references(code: str) -> set:
    """Extract all state.xxx field references"""
    pattern = r'state\.([a-zA-Z0-9_]+)\b'
    matches = re.findall(pattern, code)
    return set(matches)

def extract_declared_fields(code: str) -> set:
    """Extract all declared fields in ProgramState"""
    # Find ProgramState class
    match = re.search(r'static class ProgramState \{([^}]+)\}', code, re.DOTALL)
    if not match:
        return set()
    
    state_code = match.group(1)
    
    # Find field declarations (type name = value;)
    pattern = r'(?:java\.math\.BigDecimal|String|int|boolean|java\.io\.\w+)\[?\]?\s+([a-zA-Z0-9_]+)\s*[=;]'
    matches = re.findall(pattern, state_code)
    return set(matches)

def infer_field_type(field_name: str, code: str) -> str:
    """Infer field type from usage"""
    # Check how it's used
    if 'compareTo' in code or '.add(' in code or 'BigDecimal' in code:
        if 'Arr' in field_name or '[]' in code:
            return 'java.math.BigDecimal[]'
        return 'java.math.BigDecimal'
    if '.equals(' in code or '.length()' in code or 'String' in code:
        if 'Arr' in field_name or '[]' in code:
            return 'String[]'
        return 'String'
    if '++' in code or '--' in code or 'int' in code:
        return 'int'
    if 'true' in code or 'false' in code or 'boolean' in code:
        return 'boolean'
    
    # Default to String
    if 'Arr' in field_name:
        return 'String[]'
    return 'String'

def add_missing_fields(code: str) -> str:
    """Add all missing fields to ProgramState"""
    
    # Extract referenced and declared fields
    referenced = extract_field_references(code)
    declared = extract_declared_fields(code)
    
    missing = referenced - declared
    
    # Remove common non-field references
    missing = {f for f in missing if f not in ['equals', 'compareTo', 'add', 'intValue', 'toString', 'length', 'charAt', 'matches', 'trim', 'isEmpty', 'valueOf', 'parseInt', 'ZERO', 'ONE']}
    
    if not missing:
        return code
    
    print(f"Found {len(missing)} missing fields: {sorted(missing)[:10]}...")
    
    # Find insertion point (before closing brace of ProgramState)
    pattern = r'(String highValues = "\\uFFFF\\uFFFF\\uFFFF\\uFFFF";\n\s+\})'
    
    fields_to_add = []
    for field in sorted(missing):
        field_type = infer_field_type(field, code)
        if '[]' in field_type:
            if 'BigDecimal' in field_type:
                fields_to_add.append(f"        {field_type} {field} = new java.math.BigDecimal[10];")
            else:
                fields_to_add.append(f"        {field_type} {field} = new String[100];")
        elif field_type == 'java.math.BigDecimal':
            fields_to_add.append(f"        {field_type} {field} = java.math.BigDecimal.ZERO;")
        elif field_type == 'int':
            fields_to_add.append(f"        {field_type} {field} = 0;")
        elif field_type == 'boolean':
            fields_to_add.append(f"        {field_type} {field} = false;")
        else:
            fields_to_add.append(f"        {field_type} {field} = \"\";")
    
    fields_text = "\n".join(fields_to_add) + "\n"
    
    code = re.sub(pattern, fields_text + r'\1', code)
    
    return code

def main():
    print("=" * 60)
    print("Adding All Missing Fields")
    print("=" * 60)
    
    code = JAVA_FILE.read_text(encoding='utf-8')
    original_code = code
    
    code = add_missing_fields(code)
    
    if code != original_code:
        JAVA_FILE.write_text(code, encoding='utf-8')
        print("✅ Added missing fields!")
    else:
        print("⚠️ No fields to add")
    
    print("=" * 60)

if __name__ == "__main__":
    main()

