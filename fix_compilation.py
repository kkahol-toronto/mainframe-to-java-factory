#!/usr/bin/env python3
"""
Iterative Compilation Fix Pipeline
===================================

This script:
1. Compiles the Java code
2. Captures compilation errors
3. Sends errors to LLM for fixes
4. Applies fixes
5. Repeats until code compiles or max iterations reached
"""

import os
import re
import subprocess
from pathlib import Path
from dataclasses import dataclass
from typing import List, Tuple
from dotenv import load_dotenv
from openai import AzureOpenAI

load_dotenv()

# =============================================================================
# CONFIGURATION
# =============================================================================

BASE_DIR = Path(__file__).parent
MISC_1099_DIR = BASE_DIR / "misc-1099"
JAVA_DIR = MISC_1099_DIR / "src" / "main" / "java" / "com" / "fordcredit" / "misc1099" / "batch" / "program"

MAX_ITERATIONS = 5
MAX_ERRORS_PER_BATCH = 10  # Process this many errors at a time

# =============================================================================
# LLM CLIENT
# =============================================================================

class FixerLLM:
    """LLM client for fixing compilation errors"""
    
    def __init__(self):
        self.client = AzureOpenAI(
            api_key=os.getenv("AZURE_OPENAI_API_KEY"),
            api_version="2024-02-15-preview",
            azure_endpoint=os.getenv("AZURE_OPENAI_ENDPOINT")
        )
        self.deployment = os.getenv("AZURE_OPENAI_DEPLOYMENT", "gpt-4o")
    
    def fix_errors(self, java_file: Path, errors: List[dict], code_context: str, available_methods: List[str] = None) -> List[Tuple[str, str]]:
        """
        Given compilation errors and code context, return list of (old_code, new_code) replacements
        """
        
        error_descriptions = []
        for e in errors[:MAX_ERRORS_PER_BATCH]:
            error_descriptions.append(f"Line {e['line']}: {e['message']}")
        
        methods_context = ""
        if available_methods:
            methods_context = f"\nAVAILABLE METHODS (use ONLY these):\n{chr(10).join(available_methods[:50])}\n"
        
        prompt = f"""You are fixing Java compilation errors. Given the errors and code, provide EXACT replacements.

FILE: {java_file.name}
{methods_context}
COMPILATION ERRORS:
{chr(10).join(error_descriptions)}

RELEVANT CODE (with line numbers):
{code_context}

INSTRUCTIONS:
1. For each error, provide the EXACT old code and the fixed new code
2. Output format - each fix on its own block:

FIX:
OLD: <exact old code line or lines>
NEW: <fixed code>
END_FIX

3. Be precise - the old code must match exactly what's in the file
4. Common fixes:
   - "cannot find symbol: method xxx" → Use a method from AVAILABLE METHODS list, or comment out: // TODO: unknown method
   - "incompatible types: int to String" → Use String.valueOf(x)
   - "incompatible types: String to int" → Use Integer.parseInt(x)
   - "variable already defined" → Remove the duplicate declaration line entirely
   - "catch without try" → Add missing closing braces before catch

5. If you can't fix something, comment it out with // TODO:

OUTPUT ONLY FIX BLOCKS - no explanations."""

        try:
            response = self.client.chat.completions.create(
                model=self.deployment,
                messages=[
                    {"role": "system", "content": "You are a Java compilation error fixer. Output only FIX blocks with exact code replacements."},
                    {"role": "user", "content": prompt}
                ],
                temperature=0.1,
                max_tokens=3000
            )
            
            result = response.choices[0].message.content
            return self._parse_fixes(result)
        except Exception as e:
            print(f"    LLM error: {e}")
            return []
    
    def _parse_fixes(self, text: str) -> List[Tuple[str, str]]:
        """Parse FIX blocks from LLM response"""
        fixes = []
        
        # Find all FIX blocks
        pattern = r'FIX:\s*\nOLD:\s*(.*?)\nNEW:\s*(.*?)\nEND_FIX'
        matches = re.findall(pattern, text, re.DOTALL)
        
        for old, new in matches:
            old = old.strip()
            new = new.strip()
            
            # Remove line numbers if LLM included them (e.g., "1234: code")
            old = re.sub(r'^\d+:\s*', '', old)
            new = re.sub(r'^\d+:\s*', '', new)
            
            if old and old != new:
                fixes.append((old, new))
        
        return fixes


# =============================================================================
# COMPILER
# =============================================================================

@dataclass
class CompilationError:
    file: str
    line: int
    column: int
    message: str


class JavaCompiler:
    """Wrapper for Maven compilation"""
    
    def compile(self) -> Tuple[bool, List[CompilationError]]:
        """Compile the project and return (success, errors)"""
        
        result = subprocess.run(
            ["./mvnw", "compile", "-q"],
            cwd=MISC_1099_DIR,
            capture_output=True,
            text=True
        )
        
        if result.returncode == 0:
            return True, []
        
        errors = self._parse_errors(result.stdout + result.stderr)
        return False, errors
    
    def _parse_errors(self, output: str) -> List[CompilationError]:
        """Parse Maven compilation errors"""
        errors = []
        
        # Pattern: [ERROR] /path/to/File.java:[line,col] message
        pattern = r'\[ERROR\]\s+([^:]+\.java):\[(\d+),(\d+)\]\s+(.+)'
        
        for match in re.finditer(pattern, output):
            file_path = match.group(1)
            file_name = Path(file_path).name
            errors.append(CompilationError(
                file=file_name,
                line=int(match.group(2)),
                column=int(match.group(3)),
                message=match.group(4).strip()
            ))
        
        return errors


# =============================================================================
# FIXER
# =============================================================================

class IterativeFixer:
    """Main fixer that iterates until compilation succeeds"""
    
    def __init__(self):
        self.compiler = JavaCompiler()
        self.llm = FixerLLM()
    
    def fix(self, target_file: str = None):
        """Fix compilation errors iteratively"""
        
        print("=" * 60)
        print("Iterative Compilation Fixer")
        print("=" * 60)
        
        for iteration in range(1, MAX_ITERATIONS + 1):
            print(f"\n[Iteration {iteration}/{MAX_ITERATIONS}]")
            
            # Compile
            print("  Compiling...")
            success, errors = self.compiler.compile()
            
            if success:
                print("  ✅ BUILD SUCCESS!")
                return True
            
            # Filter to target file if specified
            if target_file:
                errors = [e for e in errors if target_file in e.file]
            
            if not errors:
                print("  No errors found for target file")
                return True
            
            print(f"  Found {len(errors)} errors")
            
            # Group errors by file
            errors_by_file = {}
            for e in errors:
                if e.file not in errors_by_file:
                    errors_by_file[e.file] = []
                errors_by_file[e.file].append(e)
            
            # Process each file
            for file_name, file_errors in errors_by_file.items():
                java_file = JAVA_DIR / file_name
                if not java_file.exists():
                    continue
                
                print(f"  Fixing {file_name} ({len(file_errors)} errors)...")
                
                # Read file
                code = java_file.read_text(encoding='utf-8')
                lines = code.split('\n')
                
                # Extract available methods from the file
                method_pattern = re.compile(r'private void (\w+)\(ProgramState state')
                available_methods = [f"{m}(state)" for m in method_pattern.findall(code)]
                
                # Also add file I/O methods
                read_pattern = re.compile(r'private void (read\w+)\(ProgramState state\)')
                write_pattern = re.compile(r'private void (write\w+)\(ProgramState state')
                available_methods.extend([f"{m}(state)" for m in read_pattern.findall(code)])
                available_methods.extend([f"{m}(state)" for m in write_pattern.findall(code)])
                available_methods = list(set(available_methods))
                
                # Get context around errors
                error_lines = set(e.line for e in file_errors[:MAX_ERRORS_PER_BATCH])
                context_lines = []
                for line_num in sorted(error_lines):
                    start = max(0, line_num - 5)
                    end = min(len(lines), line_num + 5)
                    for i in range(start, end):
                        context_lines.append(f"{i+1:4d}: {lines[i]}")
                
                context = '\n'.join(context_lines)
                
                # Get fixes from LLM
                error_dicts = [{'line': e.line, 'message': e.message} for e in file_errors]
                fixes = self.llm.fix_errors(java_file, error_dicts, context, available_methods)
                
                if not fixes:
                    print(f"    No fixes suggested")
                    continue
                
                print(f"    Applying {len(fixes)} fixes...")
                
                # Apply fixes
                for old, new in fixes:
                    if old in code:
                        code = code.replace(old, new, 1)
                        print(f"    ✓ Fixed: {old[:50]}...")
                    else:
                        print(f"    ✗ Could not find: {old[:50]}...")
                
                # Write back
                java_file.write_text(code, encoding='utf-8')
        
        print(f"\n⚠️ Max iterations ({MAX_ITERATIONS}) reached. Manual fixes may be needed.")
        return False


# =============================================================================
# MAIN
# =============================================================================

if __name__ == "__main__":
    import argparse
    
    parser = argparse.ArgumentParser(description="Iterative Compilation Fixer")
    parser.add_argument('--file', '-f', help='Target specific file (e.g., CCAC6310Tasklet.java)')
    parser.add_argument('--max-iter', '-m', type=int, default=5, help='Maximum iterations')
    args = parser.parse_args()
    
    if args.max_iter:
        MAX_ITERATIONS = args.max_iter
    
    fixer = IterativeFixer()
    success = fixer.fix(args.file)
    
    exit(0 if success else 1)

