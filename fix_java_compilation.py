#!/usr/bin/env python3
"""
Simple command-line script to fix Java compilation errors.

Usage:
    python fix_java_compilation.py                    # Fix all errors in misc-1099
    python fix_java_compilation.py misc-1099          # Fix all errors in specified project
    python fix_java_compilation.py misc-1099 MyClass.java  # Fix errors in specific file
"""

import sys
from pathlib import Path
from java_compilation_fixer import fix_compilation

def main():
    # Parse command line arguments
    if len(sys.argv) > 1:
        project_dir = sys.argv[1]
    else:
        project_dir = "misc-1099"
    
    target_file = sys.argv[2] if len(sys.argv) > 2 else None
    max_iterations = int(sys.argv[3]) if len(sys.argv) > 3 else 5
    
    # Validate project directory exists
    if not Path(project_dir).exists():
        print(f"❌ Error: Project directory '{project_dir}' not found")
        sys.exit(1)
    
    # Run the fixer
    print(f"🔧 Fixing compilation errors in '{project_dir}'...")
    if target_file:
        print(f"   Target file: {target_file}")
    print(f"   Max iterations: {max_iterations}\n")
    
    try:
        success = fix_compilation(
            project_dir=project_dir,
            target_file=target_file,
            max_iterations=max_iterations,
            verbose=True
        )
        
        if success:
            print("\n✅ Compilation successful!")
            sys.exit(0)
        else:
            print("\n⚠️ Compilation may still have errors. Check output above.")
            sys.exit(1)
    
    except KeyboardInterrupt:
        print("\n\n⚠️ Interrupted by user")
        sys.exit(1)
    except Exception as e:
        print(f"\n❌ Error: {e}")
        import traceback
        traceback.print_exc()
        sys.exit(1)

if __name__ == "__main__":
    main()

