#!/usr/bin/env python3
"""
Layer G.2 (Manifest-Driven):
Execute Java program and compare outputs against expected results.

AUTHORITATIVE RULE:
- Output files are discovered ONLY via the runtime output manifest (G.3.2)
- Manifest format (authoritative):

{
  "program": "CCAC6340",
  "outputDir": "...",
  "outputs": {
    "master_out": ".../master_out.txt",
    "sysout": ".../sysout.txt"
  }
}
"""

from pathlib import Path
import json
import subprocess
import sys
import difflib

ROOT = Path(".")
TESTCASE_BASE = ROOT / "work/mainframe_clean/testcases"
MANIFEST_DIR = ROOT / "work/migration_ir/runtime"
JAVA_PROJECT = ROOT / "misc-1099"


# ------------------------------------------------------------------
# JAVA EXECUTION
# ------------------------------------------------------------------

def run_java(program: str):
    print(f"▶ Running Java execution for {program}")
    result = subprocess.run(
        [
            "./mvnw",
            "-q",
            "-DskipTests",
            "exec:java",
            f"-Dexec.mainClass=com.fordcredit.misc1099.batch.runner.{program}Runner",
        ],
        cwd=JAVA_PROJECT,
    )
    if result.returncode != 0:
        raise RuntimeError("Java execution failed")
    print("✔ Java execution completed")


# ------------------------------------------------------------------
# MANIFEST
# ------------------------------------------------------------------

def load_manifest(program: str) -> dict:
    manifest_path = MANIFEST_DIR / f"{program}.outputs.json"
    if not manifest_path.exists():
        raise RuntimeError(f"Missing output manifest: {manifest_path}")

    with open(manifest_path, "r", encoding="utf-8") as f:
        return json.load(f)


# ------------------------------------------------------------------
# FILE COMPARISON
# ------------------------------------------------------------------

def compare_files(output_path: Path, expected_path: Path) -> bool:
    out_lines = output_path.read_text(encoding="utf-8").splitlines()
    exp_lines = expected_path.read_text(encoding="utf-8").splitlines()

    if out_lines == exp_lines:
        return True

    diff = difflib.unified_diff(
        exp_lines,
        out_lines,
        fromfile=f"expected/{expected_path.name}",
        tofile=f"output/{output_path.name}",
        lineterm="",
    )
    print("\n".join(diff))
    return False


# ------------------------------------------------------------------
# DRIVER
# ------------------------------------------------------------------

def main():
    if len(sys.argv) != 2:
        print("Usage: python layer_g2_execute_and_compare.py <PROGRAM>")
        sys.exit(1)

    program = sys.argv[1].upper()

    testcase_dir = TESTCASE_BASE / program
    expected_dir = testcase_dir / "expected"

    if not expected_dir.exists():
        raise RuntimeError(f"Expected directory missing: {expected_dir}")

    print(f"\n=== Layer G.2: Execute & Compare for {program} ===\n")

    # Run Java
    run_java(program)

    # Load manifest
    manifest = load_manifest(program)
    outputs = manifest.get("outputs")

    if not outputs:
        raise RuntimeError("Output manifest 'outputs' is empty or missing")

    mismatches = 0
    print("▶ Comparing output files (manifest-driven)")

    # --------------------------------------------------------------
    # AUTHORITATIVE: outputs is a DICT  name -> path
    # --------------------------------------------------------------
    if isinstance(outputs, dict):
        for logical_name, path_str in outputs.items():
            output_path = Path(path_str)
            expected_path = expected_dir / output_path.name

            if not output_path.exists():
                print(f"❌ Missing output file: {output_path}")
                mismatches += 1
                continue

            if not expected_path.exists():
                print(f"❌ Missing expected file: {expected_path}")
                mismatches += 1
                continue

            if compare_files(output_path, expected_path):
                print(f"✔ {output_path.name} matches expected")
            else:
                print(f"❌ {output_path.name} mismatch")
                mismatches += 1

    # --------------------------------------------------------------
    # BACKWARD-COMPAT (optional)
    # --------------------------------------------------------------
    elif isinstance(outputs, list):
        for entry in outputs:
            if isinstance(entry, str):
                output_path = testcase_dir / "output" / entry
                expected_path = expected_dir / entry
            elif isinstance(entry, dict):
                output_path = Path(entry["path"])
                expected_path = expected_dir / output_path.name
            else:
                raise RuntimeError(f"Invalid manifest entry: {entry}")

            if not output_path.exists():
                print(f"❌ Missing output file: {output_path}")
                mismatches += 1
                continue

            if not expected_path.exists():
                print(f"❌ Missing expected file: {expected_path}")
                mismatches += 1
                continue

            if compare_files(output_path, expected_path):
                print(f"✔ {output_path.name} matches expected")
            else:
                print(f"❌ {output_path.name} mismatch")
                mismatches += 1
    else:
        raise RuntimeError("Invalid manifest format: 'outputs' must be dict or list")

    # --------------------------------------------------------------
    # RESULT
    # --------------------------------------------------------------
    if mismatches == 0:
        print("\n✅ All outputs match expected results")
    else:
        print(f"\n❌ {mismatches} output mismatches detected")
        sys.exit(1)


if __name__ == "__main__":
    main()
