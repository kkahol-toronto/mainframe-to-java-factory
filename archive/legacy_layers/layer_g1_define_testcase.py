"""
Layer G.1 — Define Golden Testcase Structure (Factory-Grade)

Responsibilities:
- Ensure testcase directory structure exists
- Enforce presence of input + expected output files
- Fail loudly if golden data is missing

This layer does NOT:
- Generate test data
- Execute batch jobs
- Compare outputs (that is G.2)

Directory layout enforced:

work/mainframe_clean/testcases/<PROGRAM>/
├── input/
└── expected/
"""

from pathlib import Path
import argparse
import sys

# ---------------------------------------------------------------------
# CONSTANTS
# ---------------------------------------------------------------------

TESTCASE_ROOT = Path("work/mainframe_clean/testcases")

# ---------------------------------------------------------------------
# CORE LOGIC
# ---------------------------------------------------------------------

def ensure_testcase_dirs(program: str) -> Path:
    """
    Create testcase directory structure if missing.
    This is equivalent to `mkdir -p`, but deterministic and auditable.
    """
    base = TESTCASE_ROOT / program
    (base / "input").mkdir(parents=True, exist_ok=True)
    (base / "expected").mkdir(parents=True, exist_ok=True)
    return base


def validate_testcase_contents(base: Path) -> None:
    """
    Ensure golden test data exists.
    Directories may exist, but must NOT be empty.
    """
    input_dir = base / "input"
    expected_dir = base / "expected"

    if not any(input_dir.iterdir()):
        raise RuntimeError(
            f"❌ No input files found in {input_dir}\n"
            f"Add at least one raw input file before running G.2"
        )

    if not any(expected_dir.iterdir()):
        raise RuntimeError(
            f"❌ No expected output files found in {expected_dir}\n"
            f"Add golden output files before running G.2"
        )

# ---------------------------------------------------------------------
# ENTRYPOINT
# ---------------------------------------------------------------------

def main() -> None:
    parser = argparse.ArgumentParser(
        description="Layer G.1 — Define Golden Testcase Structure"
    )
    parser.add_argument(
        "program",
        help="COBOL program name (e.g. CCAC6340)"
    )

    args = parser.parse_args()
    program = args.program.upper()

    print(f"\n=== Layer G.1: Define Testcase for {program} ===\n")

    base = ensure_testcase_dirs(program)

    print("✔ Testcase directories ensured:")
    print(f"  {base}/input")
    print(f"  {base}/expected")

    # Validate contents (fail loud if missing)
    validate_testcase_contents(base)

    print("\n✔ Golden testcase structure is valid.")
    print("You may proceed to Layer G.2 (execution + comparison).\n")


if __name__ == "__main__":
    try:
        main()
    except Exception as e:
        print(str(e))
        sys.exit(1)
