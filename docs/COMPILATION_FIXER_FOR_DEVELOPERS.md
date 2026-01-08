# Java Compilation Fixer - For Developers

## What It Does

Automatically fixes Java compilation errors using AI. It compiles your code, identifies errors, gets AI-suggested fixes, applies them, and repeats until compilation succeeds.

## Quick Start (3 Steps)

### 1. Setup Environment Variables

Create a `.env` file in the project root:
```env
AZURE_OPENAI_API_KEY=your_api_key_here
AZURE_OPENAI_ENDPOINT=https://your-endpoint.openai.azure.com/
```

### 2. Run the Fixer

**Option A: Command Line (Easiest)**
```bash
# Fix all errors
python fix_java_compilation.py

# Fix specific file
python fix_java_compilation.py misc-1099 MyClass.java
```

**Option B: Python Script**
```python
from java_compilation_fixer import fix_compilation

# Fix all errors
fix_compilation("misc-1099", max_iterations=5)

# Fix one file
fix_compilation("misc-1099", target_file="MyClass.java")
```

### 3. Review Changes

```bash
git diff  # Review what was changed
```

## Common Use Cases

| Task | Command |
|------|---------|
| Fix all errors | `python fix_java_compilation.py` |
| Fix one file | `python fix_java_compilation.py misc-1099 MyClass.java` |
| More iterations | `python fix_java_compilation.py misc-1099 MyClass.java 10` |
| In Python code | `fix_compilation("misc-1099", target_file="MyClass.java")` |

## Best Practices

1. **Commit First**: Always commit your code before running
   ```bash
   git commit -am "Before auto-fix"
   ```

2. **Start Small**: Fix one file at a time for complex projects
   ```bash
   python fix_java_compilation.py misc-1099 MyClass.java
   ```

3. **Review Changes**: Check what was fixed before committing
   ```bash
   git diff
   ```

4. **Increase Iterations**: If it doesn't finish, try more iterations
   ```bash
   python fix_java_compilation.py misc-1099 MyClass.java 10
   ```

## How It Works

1. Compiles your Java project (`./mvnw compile`)
2. Parses compilation errors
3. Sends errors + code context to AI
4. Gets suggested fixes
5. Applies fixes to source files
6. Repeats until success or max iterations

## Troubleshooting

| Problem | Solution |
|---------|----------|
| "Module not found" | `pip install python-dotenv openai` |
| "API key not found" | Check `.env` file has correct keys |
| "File not found" | Verify project directory path |
| Still has errors | Increase iterations or fix manually |

## Files

- **Utility**: `java_compilation_fixer.py` - Main utility module
- **CLI Script**: `fix_java_compilation.py` - Command-line interface
- **Tests**: `test_compilation_fixer.py` - Test suite
- **Full Docs**: `docs/JAVA_COMPILATION_FIXER_USAGE.md` - Complete guide
- **Quick Ref**: `docs/JAVA_COMPILATION_FIXER_QUICKSTART.md` - Quick reference

## Example Output

```
============================================================
Iterative Compilation Fixer
============================================================

[Iteration 1/5]
  Compiling...
  Found 3 errors
  Fixing MyClass.java (3 errors)...
    Applying 2 fixes...
    ✓ Fixed: int x = name;...
    ✓ Fixed: String msg = "Hello"...

[Iteration 2/5]
  Compiling...
  ✅ BUILD SUCCESS!
```

## Need Help?

- See full documentation: `docs/JAVA_COMPILATION_FIXER_USAGE.md`
- Check test examples: `test_compilation_fixer.py`
- Review source code: `java_compilation_fixer.py`

