# Pipeline Post-Processing Fixes

## Overview

The migration pipeline (`cobol_to_springboot.py`) now includes automatic post-processing to fix common compilation issues. All fixes are consolidated in `post_process_java.py`.

## What Gets Fixed

### 1. File I/O Calls
- Replaces `state.openInputFile()` / `state.openOutputFile()` with proper `BufferedReader`/`BufferedWriter` initialization
- Adds file reader/writer fields to `ProgramState`
- Maps COBOL file names to actual file paths

### 2. Field Type Corrections
- **String fields**: Fixes `BigDecimal[]` → `String` for text fields
- **BigDecimal fields**: Fixes `BigDecimal[]` → `BigDecimal` for numeric scalars
- **Boolean fields**: Fixes `String`/`BigDecimal[]` → `boolean` for flags
- **Array fields**: Fixes `String` → `String[]` or `BigDecimal[]` for arrays

### 3. Boolean Comparisons
- Fixes `state.field = "Y"` → `state.field = true`
- Fixes `"Y".equals(state.field)` → `state.field`
- Removes unnecessary null checks on booleans

### 4. Structural Issues
- Fixes `.write.newLine()` syntax errors
- Removes `.initialize()` calls on String fields
- Adds missing constants (`HIGH_VALUES`, `LOW_VALUES`)
- Adds missing method stubs (`p9998Coredump`, `p8999WriteSysout`, etc.)

### 5. Method Implementations
- Implements write methods for output files
- Adds proper null checks and error handling

## Usage

The post-processor runs automatically when you use the main pipeline:

```bash
# Migrate a program (post-processing happens automatically)
python cobol_to_springboot.py --program CCAC6320

# Migrate all programs
python cobol_to_springboot.py --all
```

You can also run post-processing manually on an existing file:

```bash
python post_process_java.py misc-1099/src/main/java/com/fordcredit/misc1099/batch/program/CCAC6320Tasklet.java
```

## Legacy Fix Scripts

Individual fix scripts have been archived to `legacy_fix_scripts/`:
- `fix_file_io_calls.py` - Now part of `fix_file_io_calls()`
- `fix_missing_fields.py` - Now part of `add_file_fields_to_state()`
- `fix_type_mismatches.py` - Now part of `fix_field_types()`
- `fix_remaining_types.py` - Now part of `fix_field_types()`
- `fix_msg_fields.py` - Now part of `fix_field_types()`
- `fix_all_compilation_errors.py` - Consolidated into multiple methods
- `fix_remaining_errors.py` - Consolidated into multiple methods
- `fix_compilation.py` - Iterative LLM fixer (kept for manual use)
- `fix_paragraph_by_paragraph.py` - Paragraph-by-paragraph converter (kept for manual use)

## Extending the Post-Processor

To add new fixes, edit `post_process_java.py` and add a new method:

```python
def fix_new_issue(self, code: str) -> str:
    """Fix some new compilation issue"""
    # Your fix logic here
    return code

# Then add it to process():
def process(self, code: str) -> str:
    code = self.fix_file_io_calls(code)
    code = self.fix_new_issue(code)  # Add here
    # ... rest of fixes
    return code
```

## Current Status

✅ All 6 programs (CCAC6250, CCAC6310, CCAC6320, CCAC6330, CCAC6340, CCAC6350) compile successfully

The pipeline now generates compilable code from the start, reducing manual fixes needed.

