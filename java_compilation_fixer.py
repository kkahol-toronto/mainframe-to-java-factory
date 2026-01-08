#!/usr/bin/env python3
"""
Java Compilation Fixer Utility
==============================

A reusable utility for iteratively fixing Java compilation errors using LLM.

Usage:
    from java_compilation_fixer import IterativeCompilationFixer
    
    fixer = IterativeCompilationFixer(
        project_dir="path/to/java/project",
        java_source_dir="src/main/java"
    )
    success = fixer.fix(target_file="MyClass.java", max_iterations=5)
"""

import os
import re
import subprocess
from pathlib import Path
from dataclasses import dataclass
from typing import List, Tuple, Optional, Dict, Callable
from dotenv import load_dotenv
from openai import AzureOpenAI

load_dotenv()


# =============================================================================
# DATA CLASSES
# =============================================================================

@dataclass
class CompilationError:
    """Represents a single compilation error"""
    file: str
    line: int
    column: int
    message: str


# =============================================================================
# COMPILER
# =============================================================================

class JavaCompiler:
    """Wrapper for Java compilation (Maven or direct javac)"""
    
    def __init__(
        self,
        project_dir: Path,
        compile_command: Optional[List[str]] = None,
        error_parser: Optional[Callable[[str], List[CompilationError]]] = None
    ):
        """
        Initialize compiler.
        
        Args:
            project_dir: Root directory of the Java project
            compile_command: Custom compile command (default: Maven)
            error_parser: Custom error parser function (default: Maven parser)
        """
        self.project_dir = Path(project_dir)
        
        if compile_command is None:
            # Default to Maven
            self.compile_command = ["./mvnw", "compile", "-q"]
        else:
            self.compile_command = compile_command
        
        if error_parser is None:
            self.error_parser = self._parse_maven_errors
        else:
            self.error_parser = error_parser
    
    def compile(self) -> Tuple[bool, List[CompilationError]]:
        """
        Compile the project and return (success, errors).
        
        Returns:
            Tuple of (success: bool, errors: List[CompilationError])
        """
        try:
            result = subprocess.run(
                self.compile_command,
                cwd=self.project_dir,
                capture_output=True,
                text=True,
                timeout=300  # 5 minute timeout
            )
            
            if result.returncode == 0:
                return True, []
            
            errors = self.error_parser(result.stdout + result.stderr)
            return False, errors
        except subprocess.TimeoutExpired:
            return False, [CompilationError("", 0, 0, "Compilation timed out")]
        except Exception as e:
            return False, [CompilationError("", 0, 0, f"Compilation failed: {str(e)}")]
    
    def _parse_maven_errors(self, output: str) -> List[CompilationError]:
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
# LLM FIXER
# =============================================================================

class LLMCompilationFixer:
    """LLM client for fixing compilation errors"""
    
    def __init__(
        self,
        api_key: Optional[str] = None,
        api_version: str = "2024-02-15-preview",
        endpoint: Optional[str] = None,
        deployment: Optional[str] = None,
        temperature: float = 0.1,
        max_tokens: int = 6000
    ):
        """
        Initialize LLM fixer.
        
        Args:
            api_key: Azure OpenAI API key (default: from AZURE_OPENAI_API_KEY env var)
            api_version: API version
            endpoint: Azure endpoint (default: from AZURE_OPENAI_ENDPOINT env var)
            deployment: Model deployment name (default: from AZURE_OPENAI_DEPLOYMENT env var or "gpt-4o")
            temperature: LLM temperature
            max_tokens: Max tokens in response
        """
        self.client = AzureOpenAI(
            api_key=api_key or os.getenv("AZURE_OPENAI_API_KEY"),
            api_version=api_version,
            azure_endpoint=endpoint or os.getenv("AZURE_OPENAI_ENDPOINT")
        )
        self.deployment = deployment or os.getenv("AZURE_OPENAI_DEPLOYMENT", "gpt-4o")
        self.temperature = temperature
        self.max_tokens = max_tokens
    
    def fix_errors(
        self,
        java_file: Path,
        errors: List[Dict],
        code_context: str,
        available_methods: Optional[List[str]] = None,
        custom_instructions: Optional[str] = None
    ) -> List[Tuple[str, str]]:
        """
        Get fixes from LLM for compilation errors.
        
        Args:
            java_file: Path to the Java file
            errors: List of error dicts with 'line' and 'message' keys
            code_context: Code context around errors (with line numbers)
            available_methods: Optional list of available methods to use
            custom_instructions: Optional custom instructions for the LLM
        
        Returns:
            List of (old_code, new_code) tuples for replacements
        """
        error_descriptions = [f"Line {e['line']}: {e['message']}" for e in errors]
        
        methods_context = ""
        if available_methods:
            methods_context = f"\nAVAILABLE METHODS (use ONLY these):\n{chr(10).join(available_methods[:50])}\n"
        
        default_instructions = """INSTRUCTIONS:
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
        
        instructions = custom_instructions or default_instructions
        
        prompt = f"""You are fixing Java compilation errors. Given the errors and code, provide EXACT replacements.

FILE: {java_file.name}
{methods_context}
COMPILATION ERRORS:
{chr(10).join(error_descriptions)}

RELEVANT CODE (with line numbers):
{code_context}

{instructions}"""

        try:
            response = self.client.chat.completions.create(
                model=self.deployment,
                messages=[
                    {"role": "system", "content": "You are a Java compilation error fixer. Output only FIX blocks with exact code replacements."},
                    {"role": "user", "content": prompt}
                ],
                temperature=self.temperature,
                max_tokens=self.max_tokens
            )
            
            result = response.choices[0].message.content
            fixes = self._parse_fixes(result)
            return fixes
        except Exception as e:
            print(f"    LLM error: {e}")
            return []
    
    def _parse_fixes(self, text: str) -> List[Tuple[str, str]]:
        """Parse FIX blocks from LLM response"""
        fixes = []
        
        # Try multiple patterns (LLM can be inconsistent)
        patterns = [
            r'FIX:\s*\nOLD:\s*(.*?)\nNEW:\s*(.*?)\nEND_FIX',
            r'FIX:\s*OLD:\s*(.*?)\s*NEW:\s*(.*?)\s*END_FIX',
            r'OLD:\s*```[^\n]*\n(.*?)```\s*NEW:\s*```[^\n]*\n(.*?)```',
            r'```java\s*// OLD\s*(.*?)```\s*```java\s*// NEW\s*(.*?)```',
        ]
        
        for pattern in patterns:
            matches = re.findall(pattern, text, re.DOTALL)
            if matches:
                for old, new in matches:
                    old = old.strip()
                    new = new.strip()
                    
                    # Remove line numbers if LLM included them (e.g., "1234: code")
                    old = re.sub(r'^\d+:\s*', '', old)
                    new = re.sub(r'^\d+:\s*', '', new)
                    
                    # Remove markdown code fences if present
                    old = re.sub(r'^```\w*\n?', '', old)
                    old = re.sub(r'\n?```$', '', old)
                    new = re.sub(r'^```\w*\n?', '', new)
                    new = re.sub(r'\n?```$', '', new)
                    
                    if old and old != new:
                        fixes.append((old, new))
                break
        
        # If still no fixes, try a looser heuristic
        if not fixes and 'OLD:' in text and 'NEW:' in text:
            parts = re.split(r'OLD:\s*', text)
            for part in parts[1:]:  # Skip first (before first OLD:)
                if 'NEW:' in part:
                    old_new = re.split(r'NEW:\s*', part, 1)
                    if len(old_new) == 2:
                        old = old_new[0].strip().split('END_FIX')[0].strip()
                        new = old_new[1].strip().split('END_FIX')[0].strip()
                        old = re.sub(r'^\d+:\s*', '', old)
                        new = re.sub(r'^\d+:\s*', '', new)
                        if old and old != new:
                            fixes.append((old, new))
        
        return fixes


# =============================================================================
# ITERATIVE FIXER
# =============================================================================

class IterativeCompilationFixer:
    """Main fixer that iterates until compilation succeeds"""
    
    def __init__(
        self,
        project_dir: Path,
        java_source_dir: Optional[Path] = None,
        compiler: Optional[JavaCompiler] = None,
        llm_fixer: Optional[LLMCompilationFixer] = None,
        max_errors_per_batch: int = 10,
        context_lines: int = 5,
        method_extractor: Optional[Callable[[str], List[str]]] = None
    ):
        """
        Initialize iterative fixer.
        
        Args:
            project_dir: Root directory of the Java project
            java_source_dir: Directory containing Java source files (relative to project_dir)
            compiler: Custom compiler instance (default: Maven compiler)
            llm_fixer: Custom LLM fixer instance (default: Azure OpenAI)
            max_errors_per_batch: Max errors to process per iteration
            context_lines: Number of lines of context around errors
            method_extractor: Custom function to extract available methods from code
        """
        self.project_dir = Path(project_dir)
        
        if java_source_dir is None:
            # Default: assume Maven structure
            self.java_source_dir = self.project_dir / "src" / "main" / "java"
        else:
            self.java_source_dir = Path(java_source_dir)
            if not self.java_source_dir.is_absolute():
                self.java_source_dir = self.project_dir / self.java_source_dir
        
        self.compiler = compiler or JavaCompiler(self.project_dir)
        self.llm_fixer = llm_fixer or LLMCompilationFixer()
        self.max_errors_per_batch = max_errors_per_batch
        self.context_lines = context_lines
        
        if method_extractor is None:
            self.method_extractor = self._default_method_extractor
        else:
            self.method_extractor = method_extractor
    
    def fix(
        self,
        target_file: Optional[str] = None,
        max_iterations: int = 5,
        verbose: bool = True
    ) -> bool:
        """
        Fix compilation errors iteratively.
        
        Args:
            target_file: Optional specific file to fix (e.g., "MyClass.java")
            max_iterations: Maximum number of iterations
            verbose: Print progress messages
        
        Returns:
            True if compilation succeeded, False otherwise
        """
        if verbose:
            print("=" * 60)
            print("Iterative Compilation Fixer")
            print("=" * 60)
        
        error_history = []  # Track error counts to detect cycles
        
        for iteration in range(1, max_iterations + 1):
            if verbose:
                print(f"\n[Iteration {iteration}/{max_iterations}]")
            
            # Compile
            if verbose:
                print("  Compiling...")
            success, errors = self.compiler.compile()
            
            if success:
                if verbose:
                    print("  ✅ BUILD SUCCESS!")
                return True
            
            # Filter to target file if specified
            if target_file:
                errors = [e for e in errors if target_file in e.file]
            
            if not errors:
                if verbose:
                    print("  No errors found for target file")
                return True
            
            error_count = len(errors)
            if verbose:
                print(f"  Found {error_count} errors")
            
            # Detect cycles - if same error count for 5+ iterations, increase context
            error_history.append(error_count)
            if len(error_history) >= 5:
                recent = error_history[-5:]
                if len(set(recent)) <= 2:  # Oscillating between 1-2 values
                    if verbose:
                        print(f"  ⚠️ Detected oscillation - adding full file context")
            
            # Group errors by file
            errors_by_file = {}
            for e in errors:
                if e.file not in errors_by_file:
                    errors_by_file[e.file] = []
                errors_by_file[e.file].append(e)
            
            # Process each file
            for file_name, file_errors in errors_by_file.items():
                java_file = self._find_java_file(file_name)
                if not java_file or not java_file.exists():
                    if verbose:
                        print(f"  ⚠️ File not found: {file_name}")
                    continue
                
                if verbose:
                    print(f"  Fixing {file_name} ({len(file_errors)} errors)...")
                
                # Read file
                code = java_file.read_text(encoding='utf-8')
                lines = code.split('\n')
                
                # Extract available methods
                available_methods = self.method_extractor(code)
                
                # Get context around errors
                error_lines = set(e.line for e in file_errors[:self.max_errors_per_batch])
                context_lines = []
                for line_num in sorted(error_lines):
                    start = max(0, line_num - self.context_lines)
                    end = min(len(lines), line_num + self.context_lines)
                    for i in range(start, end):
                        line_str = f"{i+1:4d}: {lines[i]}"
                        if line_str not in context_lines:
                            context_lines.append(line_str)
                
                context = '\n'.join(context_lines)
                
                # Get fixes from LLM
                error_dicts = [{'line': e.line, 'message': e.message} for e in file_errors]
                fixes = self.llm_fixer.fix_errors(java_file, error_dicts, context, available_methods)
                
                if not fixes:
                    if verbose:
                        print(f"    No fixes suggested")
                    continue
                
                if verbose:
                    print(f"    Applying {len(fixes)} fixes...")
                
                # Apply fixes
                applied = 0
                for old, new in fixes:
                    if old in code:
                        code = code.replace(old, new, 1)
                        applied += 1
                        if verbose:
                            print(f"    ✓ Fixed: {old[:50]}...")
                    else:
                        if verbose:
                            print(f"    ✗ Could not find: {old[:50]}...")
                
                if applied > 0:
                    # Write back
                    java_file.write_text(code, encoding='utf-8')
        
        if verbose:
            print(f"\n⚠️ Max iterations ({max_iterations}) reached. Manual fixes may be needed.")
        return False
    
    def _find_java_file(self, file_name: str) -> Optional[Path]:
        """Find Java file in the source directory"""
        # Try direct path first
        direct_path = self.java_source_dir / file_name
        if direct_path.exists():
            return direct_path
        
        # Search recursively
        for java_file in self.java_source_dir.rglob(file_name):
            return java_file
        
        return None
    
    def _default_method_extractor(self, code: str) -> List[str]:
        """Default method extractor - finds methods with ProgramState parameter"""
        methods = []
        
        # Pattern for methods with ProgramState
        method_pattern = re.compile(r'private void (\w+)\(ProgramState state')
        methods.extend([f"{m}(state)" for m in method_pattern.findall(code)])
        
        # File I/O methods
        read_pattern = re.compile(r'private void (read\w+)\(ProgramState state\)')
        write_pattern = re.compile(r'private void (write\w+)\(ProgramState state')
        methods.extend([f"{m}(state)" for m in read_pattern.findall(code)])
        methods.extend([f"{m}(state)" for m in write_pattern.findall(code)])
        
        return list(set(methods))


# =============================================================================
# CONVENIENCE FUNCTIONS
# =============================================================================

def fix_compilation(
    project_dir: str,
    target_file: Optional[str] = None,
    max_iterations: int = 5,
    java_source_dir: Optional[str] = None,
    verbose: bool = True
) -> bool:
    """
    Convenience function to fix compilation errors.
    
    Args:
        project_dir: Root directory of the Java project
        target_file: Optional specific file to fix
        max_iterations: Maximum number of iterations
        java_source_dir: Optional Java source directory (relative to project_dir)
        verbose: Print progress messages
    
    Returns:
        True if compilation succeeded, False otherwise
    """
    fixer = IterativeCompilationFixer(
        project_dir=project_dir,
        java_source_dir=java_source_dir
    )
    return fixer.fix(target_file=target_file, max_iterations=max_iterations, verbose=verbose)

