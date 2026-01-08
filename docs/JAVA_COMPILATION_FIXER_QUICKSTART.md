# Java Compilation Fixer - Quick Reference

## Setup (One Time)

1. **Environment Variables** - Create `.env` file:
   ```env
   AZURE_OPENAI_API_KEY=your_key
   AZURE_OPENAI_ENDPOINT=https://your-endpoint.openai.azure.com/
   ```

2. **Activate Virtual Environment**:
   ```bash
   source venv/bin/activate
   ```

## Basic Usage

### Fix All Errors
```python
from java_compilation_fixer import fix_compilation

fix_compilation("misc-1099", max_iterations=5)
```

### Fix One File
```python
from java_compilation_fixer import fix_compilation

fix_compilation("misc-1099", target_file="MyClass.java")
```

### Using the Class
```python
from java_compilation_fixer import IterativeCompilationFixer

fixer = IterativeCompilationFixer(project_dir="misc-1099")
fixer.fix(max_iterations=5)
```

## Command Line Script Example

```python
#!/usr/bin/env python3
from java_compilation_fixer import fix_compilation
import sys

if __name__ == "__main__":
    project = sys.argv[1] if len(sys.argv) > 1 else "misc-1099"
    file = sys.argv[2] if len(sys.argv) > 2 else None
    
    success = fix_compilation(project, target_file=file, max_iterations=5)
    sys.exit(0 if success else 1)
```

## Common Parameters

| Parameter | Default | Description |
|-----------|---------|-------------|
| `project_dir` | Required | Java project root directory |
| `target_file` | None | Fix only this file (e.g., "MyClass.java") |
| `max_iterations` | 5 | Max fix attempts |
| `java_source_dir` | "src/main/java" | Java source location |
| `verbose` | True | Show progress messages |

## Tips

- ✅ Always commit before running: `git commit -am "Before auto-fix"`
- ✅ Start with one file: Use `target_file` parameter
- ✅ Review changes: `git diff` after fixing
- ✅ Increase iterations if needed: `max_iterations=10`

## Troubleshooting

| Issue | Solution |
|-------|----------|
| Module not found | `pip install python-dotenv openai` |
| API key error | Check `.env` file exists and has correct keys |
| File not found | Verify `project_dir` and `java_source_dir` paths |
| Still failing | Increase `max_iterations` or fix manually |

## Full Documentation

See `docs/JAVA_COMPILATION_FIXER_USAGE.md` for complete guide.

