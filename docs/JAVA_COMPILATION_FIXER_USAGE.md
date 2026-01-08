# Java Compilation Fixer - Developer Guide

## Overview

The `java_compilation_fixer` utility automatically fixes Java compilation errors by:
1. Compiling your Java project
2. Capturing compilation errors
3. Sending errors to an LLM (Azure OpenAI) for fixes
4. Applying the suggested fixes
5. Repeating until compilation succeeds or max iterations reached

## Prerequisites

1. **Python Environment**
   ```bash
   # Activate your virtual environment
   source venv/bin/activate
   
   # Ensure dependencies are installed
   pip install -r requirements.txt
   ```

2. **Environment Variables**
   Create a `.env` file in the project root with:
   ```env
   AZURE_OPENAI_API_KEY=your_api_key_here
   AZURE_OPENAI_ENDPOINT=https://your-endpoint.openai.azure.com/
   AZURE_OPENAI_DEPLOYMENT=gpt-4o  # Optional, defaults to gpt-4o
   ```

3. **Maven Wrapper**
   Your Java project should have `mvnw` (Maven wrapper) in the project root.

## Quick Start

### Method 1: Simple Convenience Function

```python
from java_compilation_fixer import fix_compilation

# Fix all compilation errors in the project
success = fix_compilation(
    project_dir="misc-1099",
    max_iterations=5
)

if success:
    print("✅ Compilation successful!")
else:
    print("⚠️ Some errors may remain")
```

### Method 2: Using the Class Directly

```python
from java_compilation_fixer import IterativeCompilationFixer

# Create fixer instance
fixer = IterativeCompilationFixer(
    project_dir="misc-1099",
    java_source_dir="src/main/java"
)

# Fix all errors
success = fixer.fix(max_iterations=5)
```

### Method 3: Fix a Specific File

```python
from java_compilation_fixer import fix_compilation

# Fix only errors in a specific file
success = fix_compilation(
    project_dir="misc-1099",
    target_file="CCAC6310Tasklet.java",
    max_iterations=5
)
```

## Common Use Cases

### Use Case 1: Fix All Compilation Errors

```python
from java_compilation_fixer import IterativeCompilationFixer

fixer = IterativeCompilationFixer(project_dir="misc-1099")
success = fixer.fix(max_iterations=10)
```

### Use Case 2: Fix Errors in a Single File

```python
from java_compilation_fixer import fix_compilation

# Only fix errors in CCAC6320Tasklet.java
success = fix_compilation(
    project_dir="misc-1099",
    target_file="CCAC6320Tasklet.java",
    max_iterations=5
)
```

### Use Case 3: Custom Project Structure

```python
from java_compilation_fixer import IterativeCompilationFixer

# If your Java source is in a non-standard location
fixer = IterativeCompilationFixer(
    project_dir="/path/to/project",
    java_source_dir="custom/path/to/java"  # Relative to project_dir
)
success = fixer.fix()
```

### Use Case 4: Quiet Mode (No Verbose Output)

```python
from java_compilation_fixer import fix_compilation

success = fix_compilation(
    project_dir="misc-1099",
    verbose=False  # Suppress progress messages
)
```

### Use Case 5: Custom LLM Configuration

```python
from java_compilation_fixer import IterativeCompilationFixer, LLMCompilationFixer

# Create custom LLM fixer with different settings
llm_fixer = LLMCompilationFixer(
    temperature=0.2,  # Higher creativity
    max_tokens=8000   # More tokens for complex fixes
)

fixer = IterativeCompilationFixer(
    project_dir="misc-1099",
    llm_fixer=llm_fixer
)
success = fixer.fix()
```

## Advanced Configuration

### Custom Compiler Command

```python
from java_compilation_fixer import IterativeCompilationFixer, JavaCompiler

# Use custom compile command (e.g., direct javac)
compiler = JavaCompiler(
    project_dir="misc-1099",
    compile_command=["javac", "-cp", "lib/*", "src/**/*.java"]
)

fixer = IterativeCompilationFixer(
    project_dir="misc-1099",
    compiler=compiler
)
success = fixer.fix()
```

### Custom Method Extractor

```python
from java_compilation_fixer import IterativeCompilationFixer
import re

def custom_method_extractor(code: str) -> list:
    """Extract all public methods"""
    pattern = re.compile(r'public\s+\w+\s+(\w+)\s*\(')
    return [m for m in pattern.findall(code)]

fixer = IterativeCompilationFixer(
    project_dir="misc-1099",
    method_extractor=custom_method_extractor
)
success = fixer.fix()
```

### Adjust Error Processing

```python
from java_compilation_fixer import IterativeCompilationFixer

fixer = IterativeCompilationFixer(
    project_dir="misc-1099",
    max_errors_per_batch=20,  # Process more errors per iteration
    context_lines=10           # More context around errors
)
success = fixer.fix(max_iterations=10)
```

## API Reference

### `fix_compilation()` - Convenience Function

```python
fix_compilation(
    project_dir: str,
    target_file: Optional[str] = None,
    max_iterations: int = 5,
    java_source_dir: Optional[str] = None,
    verbose: bool = True
) -> bool
```

**Parameters:**
- `project_dir`: Root directory of the Java project (required)
- `target_file`: Optional specific file to fix (e.g., "MyClass.java")
- `max_iterations`: Maximum number of fix iterations (default: 5)
- `java_source_dir`: Java source directory relative to project_dir (default: "src/main/java")
- `verbose`: Print progress messages (default: True)

**Returns:** `True` if compilation succeeded, `False` otherwise

### `IterativeCompilationFixer` - Main Class

```python
fixer = IterativeCompilationFixer(
    project_dir: Path,
    java_source_dir: Optional[Path] = None,
    compiler: Optional[JavaCompiler] = None,
    llm_fixer: Optional[LLMCompilationFixer] = None,
    max_errors_per_batch: int = 10,
    context_lines: int = 5,
    method_extractor: Optional[Callable[[str], List[str]]] = None
)

success = fixer.fix(
    target_file: Optional[str] = None,
    max_iterations: int = 5,
    verbose: bool = True
) -> bool
```

**Constructor Parameters:**
- `project_dir`: Root directory of the Java project (required)
- `java_source_dir`: Java source directory (default: "src/main/java")
- `compiler`: Custom compiler instance (default: Maven compiler)
- `llm_fixer`: Custom LLM fixer instance (default: Azure OpenAI)
- `max_errors_per_batch`: Max errors to process per iteration (default: 10)
- `context_lines`: Lines of context around errors (default: 5)
- `method_extractor`: Function to extract available methods from code

**fix() Parameters:**
- `target_file`: Optional specific file to fix
- `max_iterations`: Maximum iterations (default: 5)
- `verbose`: Print progress (default: True)

## How It Works

1. **Compilation**: Runs `./mvnw compile` (or custom command)
2. **Error Parsing**: Extracts errors from Maven output
3. **Error Grouping**: Groups errors by file
4. **Context Extraction**: Gets code context around each error
5. **LLM Fixing**: Sends errors + context to LLM for fixes
6. **Fix Application**: Applies suggested fixes to source files
7. **Iteration**: Repeats until success or max iterations

## Troubleshooting

### Issue: "ModuleNotFoundError: No module named 'dotenv'"

**Solution:**
```bash
source venv/bin/activate
pip install python-dotenv openai
```

### Issue: "LLM error: API key not found"

**Solution:**
- Check your `.env` file exists
- Verify `AZURE_OPENAI_API_KEY` and `AZURE_OPENAI_ENDPOINT` are set
- Ensure `.env` is in the project root

### Issue: "File not found" errors

**Solution:**
- Verify `project_dir` path is correct
- Check `java_source_dir` matches your project structure
- Ensure Java files are in the expected location

### Issue: Fixes not being applied

**Solution:**
- Check that the LLM is returning fixes in the expected format
- Increase `max_iterations` if fixes need multiple passes
- Try increasing `context_lines` for better context

### Issue: Compilation still fails after max iterations

**Solution:**
- Increase `max_iterations` (e.g., 10-15)
- Check if errors are too complex for automatic fixing
- Review remaining errors manually
- Some errors may require domain-specific knowledge

## Best Practices

1. **Start Small**: Fix one file at a time for complex projects
   ```python
   fix_compilation("misc-1099", target_file="MyClass.java")
   ```

2. **Review Changes**: Always review the fixes before committing
   ```bash
   git diff  # Review changes
   ```

3. **Iterative Approach**: Use fewer iterations first, then increase if needed
   ```python
   # Start with 3 iterations
   fixer.fix(max_iterations=3)
   # If needed, run again with more iterations
   fixer.fix(max_iterations=10)
   ```

4. **Backup First**: Commit or backup your code before running
   ```bash
   git commit -am "Before auto-fix"
   ```

5. **Monitor Progress**: Keep `verbose=True` to see what's happening
   ```python
   fixer.fix(verbose=True)  # See detailed progress
   ```

## Examples

### Example 1: Fix All Errors in Project

```python
#!/usr/bin/env python3
from java_compilation_fixer import fix_compilation

if __name__ == "__main__":
    success = fix_compilation(
        project_dir="misc-1099",
        max_iterations=10
    )
    exit(0 if success else 1)
```

### Example 2: Fix Specific File in Script

```python
#!/usr/bin/env python3
import sys
from java_compilation_fixer import fix_compilation

if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("Usage: fix_file.py <filename.java>")
        sys.exit(1)
    
    filename = sys.argv[1]
    success = fix_compilation(
        project_dir="misc-1099",
        target_file=filename,
        max_iterations=5
    )
    sys.exit(0 if success else 1)
```

### Example 3: Batch Fix Multiple Files

```python
#!/usr/bin/env python3
from java_compilation_fixer import IterativeCompilationFixer

files_to_fix = [
    "CCAC6310Tasklet.java",
    "CCAC6320Tasklet.java",
    "CCAC6330Tasklet.java"
]

fixer = IterativeCompilationFixer(project_dir="misc-1099")

for file in files_to_fix:
    print(f"\nFixing {file}...")
    success = fixer.fix(target_file=file, max_iterations=5)
    if not success:
        print(f"⚠️ {file} still has errors")
```

## Integration with Existing Scripts

You can integrate the fixer into your existing Python scripts:

```python
from java_compilation_fixer import IterativeCompilationFixer

# In your migration pipeline
def migrate_and_fix():
    # ... your migration code ...
    
    # Auto-fix compilation errors
    fixer = IterativeCompilationFixer(project_dir="misc-1099")
    if not fixer.fix(max_iterations=5):
        print("Warning: Some compilation errors remain")
        # Continue or abort as needed
```

## Support

For issues or questions:
1. Check the test file: `test_compilation_fixer.py` for usage examples
2. Review the source code: `java_compilation_fixer.py`
3. Check existing usage in: `legacy_fix_scripts/fix_compilation.py`

