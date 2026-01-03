"""
Layer G.2 (Manifest-Driven):
Execute Java program and compare outputs against expected results.

AUTHORITATIVE RULE:
- Output files are discovered ONLY via the runtime output manifest (G.3.2)
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

def run_java(program: str):
    print(f"▶ Running Java execution for {program}")
    result = subprocess.run(
        ["./mvnw", "-q", "-DskipTests", "exec:java",
         f"-Dexec.mainClass=com.fordcredit.misc1099.batch.runner.{program}Runner"],
        cwd=JAVA_PROJECT,
    )
    if result.returncode != 0:
        raise RuntimeError("Java execution failed")
    print("✔ Java execution completed")

def load_manifest(program: str) -> dict:
    manifest_path = MANIFEST_DIR / f"{program}.outputs.json"
    if not manifest_path.exists():
        raise RuntimeError(f"Missing output manifest: {manifest_path}")

    with open(manifest_path, "r", encoding="utf-8") as f:
        return json.load(f)

def compare_files(output_path: Path, expected_path: Path) -> bool:
    out_lines = output_path.read_text(encoding="utf-8").splitlines()
    exp_lines = expected_path.read_text(encoding="utf-8").splitlines()

    if out_lines == exp_lines:
        return True

    diff = difflib.unified_diff(
        exp_lines,
        out_lines,
        fromfile="expected",
        tofile="output",
        lineterm=""
    )
    print("\n".join(diff))
    return False

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

    run_java(program)

    manifest = load_manifest(program)
    outputs = manifest.get("outputs", [])

    if not outputs:
        raise RuntimeError("Output manifest is empty")

    mismatches = 0

    print("▶ Comparing output files (manifest-driven)")

    for entry in outputs:
        name = entry["name"]
        output_path = Path(entry["path"])
        expected_path = expected_dir / name

        if not output_path.exists():
            print(f"❌ Missing output file: {output_path}")
            mismatches += 1
            continue

        if not expected_path.exists():
            print(f"❌ Missing expected file: {expected_path}")
            mismatches += 1
            continue

        if compare_files(output_path, expected_path):
            print(f"✔ {name} matches expected")
        else:
            print(f"❌ {name} mismatch")
            mismatches += 1

    if mismatches == 0:
        print("\n✅ All outputs match expected results")
    else:
        print(f"\n❌ {mismatches} output mismatches detected")
        sys.exit(1)

if __name__ == "__main__":
    main()
