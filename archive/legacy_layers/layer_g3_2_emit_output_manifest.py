"""
Layer G.3.2: Emit Output Manifest (FACTORY-GRADE)

Purpose:
- After Java execution, declare exactly which output files were produced
- Eliminate filesystem guessing in G.2
- Create a single source of truth for runtime outputs

Contract:
- Reads: work/mainframe_clean/testcases/<PROGRAM>/output/*
- Writes: work/migration_ir/runtime/<PROGRAM>.outputs.json
"""

from pathlib import Path
import json
import sys

# ---------------------------------------------------------------------
# PATHS
# ---------------------------------------------------------------------

ROOT = Path(".")
TESTCASE_BASE = ROOT / "work" / "mainframe_clean" / "testcases"
RUNTIME_IR = ROOT / "work" / "migration_ir" / "runtime"
RUNTIME_IR.mkdir(parents=True, exist_ok=True)

# Known logical outputs (expand later if needed)
KNOWN_OUTPUTS = {
    "master_out": "master_out.txt",
    "sysout": "sysout.txt",
}

# ---------------------------------------------------------------------
# MAIN
# ---------------------------------------------------------------------

def main():
    if len(sys.argv) != 2:
        print("Usage: python layer_g3_2_emit_output_manifest.py <PROGRAM>")
        sys.exit(1)

    program = sys.argv[1].upper()
    output_dir = TESTCASE_BASE / program / "output"

    print(f"\n=== Layer G.3.2: Emit Output Manifest for {program} ===\n")

    if not output_dir.exists():
        print(f"❌ Output directory not found: {output_dir}")
        sys.exit(1)

    outputs = {}

    for logical_name, filename in KNOWN_OUTPUTS.items():
        path = output_dir / filename
        if path.exists():
            outputs[logical_name] = str(path)
            print(f"✔ Found output: {filename}")
        else:
            print(f"⚠ Missing output: {filename}")

    if not outputs:
        print("\n❌ No outputs found — Java runner may not have produced files")
        sys.exit(1)

    manifest = {
        "program": program,
        "outputDir": str(output_dir),
        "outputs": outputs,
    }

    manifest_path = RUNTIME_IR / f"{program}.outputs.json"
    manifest_path.write_text(json.dumps(manifest, indent=2), encoding="utf-8")

    print(f"\n✔ Output manifest written:")
    print(f"  {manifest_path}")
    print("\nNext step:")
    print(f"  python layer_g2_execute_and_compare.py {program}\n")


if __name__ == "__main__":
    main()
