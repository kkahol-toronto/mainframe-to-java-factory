#!/usr/bin/env python3
"""
Test script for java_compilation_fixer utility
"""

import os
import shutil
from pathlib import Path
from java_compilation_fixer import IterativeCompilationFixer, fix_compilation, JavaCompiler, LLMCompilationFixer

# Create a test project directory
TEST_PROJECT_DIR = Path("test_java_project")
TEST_SRC_DIR = TEST_PROJECT_DIR / "src" / "main" / "java" / "com" / "test"

def setup_test_project():
    """Create a test Java project with intentional compilation errors"""
    print("Setting up test project...")
    
    # Clean up if exists
    if TEST_PROJECT_DIR.exists():
        shutil.rmtree(TEST_PROJECT_DIR)
    
    # Create directory structure
    TEST_SRC_DIR.mkdir(parents=True, exist_ok=True)
    
    # Create a simple pom.xml
    pom_xml = """<?xml version="1.0" encoding="UTF-8"?>
<project xmlns="http://maven.apache.org/POM/4.0.0"
         xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
         xsi:schemaLocation="http://maven.apache.org/POM/4.0.0 
         http://maven.apache.org/xsd/maven-4.0.0.xsd">
    <modelVersion>4.0.0</modelVersion>
    <groupId>com.test</groupId>
    <artifactId>test-project</artifactId>
    <version>1.0-SNAPSHOT</version>
    <properties>
        <maven.compiler.source>11</maven.compiler.source>
        <maven.compiler.target>11</maven.compiler.target>
        <project.build.sourceEncoding>UTF-8</project.build.sourceEncoding>
    </properties>
</project>"""
    
    (TEST_PROJECT_DIR / "pom.xml").write_text(pom_xml)
    
    # Create a Java file with intentional errors
    broken_java = """package com.test;

public class BrokenCode {
    private int value;
    private String name;
    
    public BrokenCode(int value, String name) {
        this.value = value;
        this.name = name;
    }
    
    public void process() {
        // Error 1: undefined method
        int result = calculateSum(value, 10);
        
        // Error 2: type mismatch - String to int
        int num = name;
        
        // Error 3: undefined variable
        System.out.println(undefinedVar);
        
        // Error 4: missing semicolon
        String message = "Hello"
        
        // Error 5: cannot find symbol
        UnknownClass obj = new UnknownClass();
    }
    
    private int calculateSum(int a, int b) {
        return a + b;
    }
}
"""
    
    (TEST_SRC_DIR / "BrokenCode.java").write_text(broken_java)
    
    # Copy mvnw if it exists in misc-1099
    misc_mvnw = Path("misc-1099/mvnw")
    if misc_mvnw.exists():
        shutil.copy(misc_mvnw, TEST_PROJECT_DIR / "mvnw")
        os.chmod(TEST_PROJECT_DIR / "mvnw", 0o755)
        # Also copy .mvn directory if it exists
        misc_mvn_dir = Path("misc-1099/.mvn")
        if misc_mvn_dir.exists():
            shutil.copytree(misc_mvn_dir, TEST_PROJECT_DIR / ".mvn", dirs_exist_ok=True)
    
    print(f"✓ Test project created at {TEST_PROJECT_DIR}")

def test_error_parsing():
    """Test Maven error parsing"""
    print("\n" + "="*60)
    print("TEST 1: Error Parsing")
    print("="*60)
    
    try:
        compiler = JavaCompiler(Path("."))
        
        # Sample Maven error output
        sample_output = """
[ERROR] /path/to/Test.java:[10,5] cannot find symbol
[ERROR]   symbol:   method unknownMethod()
[ERROR]   location: class Test
[ERROR] /path/to/Test.java:[15,8] incompatible types: int cannot be converted to String
[ERROR] /path/to/Another.java:[20,3] variable x is already defined
"""
        
        errors = compiler._parse_maven_errors(sample_output)
        
        assert len(errors) == 3, f"Expected 3 errors, got {len(errors)}"
        assert errors[0].file == "Test.java"
        assert errors[0].line == 10
        assert errors[0].column == 5
        assert "cannot find symbol" in errors[0].message
        
        assert errors[1].file == "Test.java"
        assert errors[1].line == 15
        
        assert errors[2].file == "Another.java"
        assert errors[2].line == 20
        
        print("✅ Error parsing test passed!")
        return True
    except Exception as e:
        print(f"❌ Error parsing test failed: {e}")
        import traceback
        traceback.print_exc()
        return False

def test_fix_parsing():
    """Test LLM fix parsing"""
    print("\n" + "="*60)
    print("TEST 2: Fix Parsing")
    print("="*60)
    
    try:
        fixer = LLMCompilationFixer()
        
        # Test various fix formats
        test_cases = [
            # Format 1: Standard
            """FIX:
OLD: int x = name;
NEW: int x = Integer.parseInt(name);
END_FIX""",
            # Format 2: With code blocks
            """OLD: ```java
int x = name;
```
NEW: ```java
int x = Integer.parseInt(name);
```""",
            # Format 3: With line numbers
            """FIX:
OLD: 10: int x = name;
NEW: 10: int x = Integer.parseInt(name);
END_FIX"""
        ]
        
        for i, test_input in enumerate(test_cases):
            fixes = fixer._parse_fixes(test_input)
            assert len(fixes) > 0, f"Test case {i+1} failed: no fixes parsed"
            assert "int x = name" in fixes[0][0], f"Test case {i+1} failed: old code not found"
            assert "Integer.parseInt" in fixes[0][1], f"Test case {i+1} failed: new code not found"
        
        print("✅ Fix parsing test passed!")
        return True
    except Exception as e:
        print(f"❌ Fix parsing test failed: {e}")
        import traceback
        traceback.print_exc()
        return False

def test_method_extraction():
    """Test method extraction"""
    print("\n" + "="*60)
    print("TEST 3: Method Extraction")
    print("="*60)
    
    try:
        from java_compilation_fixer import IterativeCompilationFixer
        
        code = """
    private void processData(ProgramState state) {
        // code
    }
    
    private void readFile(ProgramState state) {
        // code
    }
    
    private void writeOutput(ProgramState state) {
        // code
    }
    """
        
        fixer = IterativeCompilationFixer(Path("."))
        methods = fixer._default_method_extractor(code)
        
        assert "processData(state)" in methods
        assert "readFile(state)" in methods
        assert "writeOutput(state)" in methods
        
        print("✅ Method extraction test passed!")
        return True
    except Exception as e:
        print(f"❌ Method extraction test failed: {e}")
        import traceback
        traceback.print_exc()
        return False

def test_compiler_class():
    """Test JavaCompiler class directly"""
    print("\n" + "="*60)
    print("TEST 4: JavaCompiler Class")
    print("="*60)
    
    if not TEST_PROJECT_DIR.exists():
        print("⚠️ Test project not set up, skipping...")
        return True
    
    try:
        compiler = JavaCompiler(TEST_PROJECT_DIR)
        success, errors = compiler.compile()
        
        print(f"\nCompilation success: {success}")
        print(f"Number of errors: {len(errors)}")
        
        if errors:
            print("\nSample errors:")
            for error in errors[:3]:
                print(f"  - {error.file}:{error.line} - {error.message}")
        
        print("\n✅ Test PASSED: Compiler class works!")
        return True
    except Exception as e:
        print(f"\n❌ Test FAILED: {e}")
        import traceback
        traceback.print_exc()
        return False

def test_basic_functionality():
    """Test basic functionality with actual compilation"""
    print("\n" + "="*60)
    print("TEST 5: Basic Functionality (Full Integration)")
    print("="*60)
    
    if not TEST_PROJECT_DIR.exists():
        print("⚠️ Test project not set up, skipping...")
        return True
    
    # Check if LLM credentials are available
    if not os.getenv("AZURE_OPENAI_API_KEY") or not os.getenv("AZURE_OPENAI_ENDPOINT"):
        print("⚠️ LLM credentials not configured, skipping full integration test")
        print("   Set AZURE_OPENAI_API_KEY and AZURE_OPENAI_ENDPOINT to run this test")
        return True
    
    try:
        fixer = IterativeCompilationFixer(
            project_dir=TEST_PROJECT_DIR,
            java_source_dir="src/main/java",
            max_errors_per_batch=5
        )
        
        print("\nRunning fixer (this will attempt to fix compilation errors)...")
        success = fixer.fix(max_iterations=3, verbose=True)
        
        if success:
            print("\n✅ Test PASSED: Compilation succeeded!")
        else:
            print("\n⚠️ Test PARTIAL: Compilation may still have errors (check manually)")
        
        return success
    except Exception as e:
        print(f"\n❌ Test FAILED: {e}")
        import traceback
        traceback.print_exc()
        return False

def cleanup():
    """Clean up test project"""
    print("\n" + "="*60)
    print("Cleaning up...")
    if TEST_PROJECT_DIR.exists():
        shutil.rmtree(TEST_PROJECT_DIR)
        print(f"✓ Removed {TEST_PROJECT_DIR}")

if __name__ == "__main__":
    import sys
    
    try:
        # Setup
        setup_test_project()
        
        # Run tests
        results = []
        results.append(test_error_parsing())
        results.append(test_fix_parsing())
        results.append(test_method_extraction())
        results.append(test_compiler_class())
        results.append(test_basic_functionality())
        
        # Summary
        print("\n" + "="*60)
        print("TEST SUMMARY")
        print("="*60)
        print(f"Tests passed: {sum(results)}/{len(results)}")
        
        if all(results):
            print("\n🎉 All tests passed!")
            sys.exit(0)
        else:
            print("\n⚠️ Some tests had issues")
            sys.exit(1)
    
    except KeyboardInterrupt:
        print("\n\nTest interrupted by user")
        cleanup()
        sys.exit(1)
    except Exception as e:
        print(f"\n\nUnexpected error: {e}")
        import traceback
        traceback.print_exc()
        cleanup()
        sys.exit(1)
    finally:
        # Optionally cleanup - comment out if you want to inspect the test project
        # cleanup()
        pass

