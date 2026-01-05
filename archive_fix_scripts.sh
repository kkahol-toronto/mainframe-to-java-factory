#!/bin/bash
# Archive individual fix scripts into a legacy folder
# These fixes are now consolidated into post_process_java.py

mkdir -p legacy_fix_scripts

mv fix_*.py legacy_fix_scripts/ 2>/dev/null || true

echo "✅ Moved fix scripts to legacy_fix_scripts/"
echo ""
echo "All fixes are now consolidated in: post_process_java.py"
echo "The main pipeline (cobol_to_springboot.py) automatically applies these fixes."

