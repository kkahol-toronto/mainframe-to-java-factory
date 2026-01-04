#!/usr/bin/env python3
"""
Paragraph-by-Paragraph Converter
================================

For complex COBOL programs like CCAC6320 (57 paragraphs):
1. Start with a compiling stub version
2. Convert ONE paragraph at a time
3. Compile and fix after each paragraph
4. Move to next paragraph only when current compiles
"""

import os
import re
import subprocess
from pathlib import Path
from dotenv import load_dotenv
from openai import AzureOpenAI

load_dotenv()

BASE_DIR = Path(__file__).parent
MISC_1099_DIR = BASE_DIR / "misc-1099"
JAVA_DIR = MISC_1099_DIR / "src" / "main" / "java" / "com" / "fordcredit" / "misc1099" / "batch" / "program"
COBOL_DIR = BASE_DIR / "work" / "mainframe_clean" / "cobol"

class ParagraphConverter:
    def __init__(self, program_name: str):
        self.program_name = program_name
        self.tasklet_file = JAVA_DIR / f"{program_name}Tasklet.java"
        self.cobol_file = COBOL_DIR / f"{program_name}.txt"
        
        self.client = AzureOpenAI(
            api_key=os.getenv("AZURE_OPENAI_API_KEY"),
            api_version="2024-02-15-preview",
            azure_endpoint=os.getenv("AZURE_OPENAI_ENDPOINT")
        )
        self.deployment = os.getenv("AZURE_OPENAI_DEPLOYMENT", "gpt-4o")
    
    def get_paragraphs_from_java(self) -> list:
        """Extract paragraph method names from Java file"""
        code = self.tasklet_file.read_text(encoding='utf-8')
        
        # Find all paragraph methods with TODO stubs
        pattern = r'private void (\w+)\(ProgramState state\) \{\s*// ===== Original COBOL: ([^=]+) =====\s*// TODO:'
        matches = re.findall(pattern, code, re.DOTALL)
        
        paragraphs = []
        for method_name, cobol_name in matches:
            paragraphs.append({
                'method_name': method_name,
                'cobol_name': cobol_name.strip()
            })
        
        return paragraphs
    
    def get_cobol_paragraph(self, cobol_name: str) -> str:
        """Extract COBOL paragraph content"""
        cobol_code = self.cobol_file.read_text(encoding='utf-8')
        
        # Find paragraph start
        pattern = rf'{re.escape(cobol_name)}\.'
        match = re.search(pattern, cobol_code)
        if not match:
            return ""
        
        start = match.start()
        
        # Find next paragraph or end
        next_para = re.search(r'\n\s{7}[A-Z0-9-]+\.', cobol_code[match.end():])
        if next_para:
            end = match.end() + next_para.start()
        else:
            end = len(cobol_code)
        
        return cobol_code[start:end]
    
    def get_java_method(self, method_name: str) -> tuple:
        """Get current Java method content and its position"""
        code = self.tasklet_file.read_text(encoding='utf-8')
        
        # Find method
        pattern = rf'(    private void {method_name}\(ProgramState state\) \{{.*?^    \}})'
        match = re.search(pattern, code, re.MULTILINE | re.DOTALL)
        
        if match:
            return match.group(1), match.start(), match.end()
        return None, -1, -1
    
    def get_program_state_fields(self) -> str:
        """Extract ProgramState class fields"""
        code = self.tasklet_file.read_text(encoding='utf-8')
        
        # Find ProgramState class
        match = re.search(r'static class ProgramState \{([^}]+)\}', code, re.DOTALL)
        if match:
            return match.group(1)
        return ""
    
    def convert_paragraph(self, method_name: str, cobol_name: str) -> str:
        """Convert a single COBOL paragraph to Java"""
        
        cobol_code = self.get_cobol_paragraph(cobol_name)
        current_method, _, _ = self.get_java_method(method_name)
        state_fields = self.get_program_state_fields()[:2000]  # Limit size
        
        if not cobol_code:
            return current_method  # Keep as-is if COBOL not found
        
        prompt = f"""Convert this COBOL paragraph to Java. Output ONLY the complete Java method.

COBOL PARAGRAPH:
{cobol_code}

CURRENT JAVA METHOD (stub):
{current_method}

AVAILABLE STATE FIELDS (use state.xxx):
{state_fields}

RULES:
1. Replace TODO with actual logic
2. Use state.xxx for all variables
3. PERFORM xxx -> xxx(state);
4. MOVE A TO B -> state.b = state.a;
5. IF...END-IF -> if (...) {{ }}
6. ADD A TO B -> state.b = state.b + state.a (or .add() for BigDecimal)
7. String comparisons: use .equals() not ==
8. Keep try-catch wrapper
9. For unknown methods, comment: // TODO: implement xxx

OUTPUT ONLY THE COMPLETE JAVA METHOD - no markdown, no explanation."""

        try:
            response = self.client.chat.completions.create(
                model=self.deployment,
                messages=[
                    {"role": "system", "content": "You are a COBOL to Java converter. Output only valid Java code."},
                    {"role": "user", "content": prompt}
                ],
                temperature=0.1,
                max_tokens=2000
            )
            
            result = response.choices[0].message.content.strip()
            
            # Remove markdown if present
            result = re.sub(r'^```java\s*', '', result)
            result = re.sub(r'^```\s*', '', result)
            result = re.sub(r'\s*```$', '', result)
            
            return result
        except Exception as e:
            print(f"    LLM error: {e}")
            return current_method
    
    def replace_method(self, method_name: str, new_code: str) -> bool:
        """Replace a method in the Java file"""
        code = self.tasklet_file.read_text(encoding='utf-8')
        
        current_method, start, end = self.get_java_method(method_name)
        if start < 0:
            return False
        
        # Ensure proper indentation
        if not new_code.startswith('    private'):
            new_code = '    ' + new_code.lstrip()
        
        new_code = code[:start] + new_code + code[end:]
        self.tasklet_file.write_text(new_code, encoding='utf-8')
        return True
    
    def compile(self) -> tuple:
        """Compile and return (success, error_count)"""
        result = subprocess.run(
            ["./mvnw", "compile", "-q"],
            cwd=MISC_1099_DIR,
            capture_output=True,
            text=True
        )
        
        if result.returncode == 0:
            return True, 0
        
        # Count errors for this file
        output = result.stdout + result.stderr
        errors = len(re.findall(rf'{self.program_name}Tasklet\.java:\[\d+,\d+\]', output))
        return False, errors
    
    def run_fixer(self, max_iter: int = 20) -> bool:
        """Run the iterative fixer"""
        result = subprocess.run(
            ["python", "fix_compilation.py", "--file", f"{self.program_name}Tasklet.java", "--max-iter", str(max_iter)],
            cwd=BASE_DIR,
            capture_output=True,
            text=True
        )
        
        success, _ = self.compile()
        return success
    
    def convert_all(self, start_from: int = 0, max_paragraphs: int = None):
        """Convert all paragraphs one by one"""
        
        print("=" * 60)
        print(f"Paragraph-by-Paragraph Converter: {self.program_name}")
        print("=" * 60)
        
        # Get stub paragraphs
        paragraphs = self.get_paragraphs_from_java()
        
        if not paragraphs:
            print("No stub paragraphs found. Run pipeline_complete.py first.")
            return
        
        print(f"Found {len(paragraphs)} stub paragraphs to convert")
        
        # Initial compile check
        success, errors = self.compile()
        if not success:
            print(f"⚠️ Starting with {errors} errors. Running fixer first...")
            self.run_fixer(30)
            success, errors = self.compile()
            if not success:
                print(f"❌ Could not get to compiling state. {errors} errors remain.")
                return
        
        print("✅ Starting from compiling state")
        
        # Process each paragraph
        converted = 0
        failed = 0
        
        end_idx = len(paragraphs) if max_paragraphs is None else min(start_from + max_paragraphs, len(paragraphs))
        
        for i, para in enumerate(paragraphs[start_from:end_idx], start=start_from):
            method_name = para['method_name']
            cobol_name = para['cobol_name']
            
            print(f"\n[{i+1}/{len(paragraphs)}] Converting {method_name} ({cobol_name})")
            
            # Convert
            new_code = self.convert_paragraph(method_name, cobol_name)
            
            # Replace
            if self.replace_method(method_name, new_code):
                # Try to compile
                success, errors = self.compile()
                
                if success:
                    print(f"    ✅ Compiled!")
                    converted += 1
                else:
                    print(f"    ⚠️ {errors} errors - running fixer...")
                    if self.run_fixer(20):
                        print(f"    ✅ Fixed and compiled!")
                        converted += 1
                    else:
                        # Try harder with more iterations
                        print(f"    ⏳ Trying harder with 50 iterations...")
                        if self.run_fixer(50):
                            print(f"    ✅ Fixed and compiled!")
                            converted += 1
                        else:
                            # Revert to previous working version
                            print(f"    ❌ Could not fix. Reverting...")
                            subprocess.run(["git", "checkout", "--", str(self.tasklet_file)], cwd=BASE_DIR)
                            failed += 1
            else:
                print(f"    ❌ Could not find method")
                failed += 1
        
        print(f"\n" + "=" * 60)
        print(f"SUMMARY: {converted} converted, {failed} failed")
        print("=" * 60)


if __name__ == "__main__":
    import argparse
    
    parser = argparse.ArgumentParser(description="Paragraph-by-paragraph COBOL to Java converter")
    parser.add_argument('--program', '-p', required=True, help='Program name (e.g., CCAC6320)')
    parser.add_argument('--start', '-s', type=int, default=0, help='Start from paragraph index')
    parser.add_argument('--count', '-c', type=int, default=None, help='Max paragraphs to convert')
    args = parser.parse_args()
    
    converter = ParagraphConverter(args.program)
    converter.convert_all(start_from=args.start, max_paragraphs=args.count)

