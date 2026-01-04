#!/usr/bin/env python3
"""
Layer 3 Full Rebuild Orchestrator (SAFE + CLEAN)

- Deletes ONLY the target program's generated files
- Rebuilds structure + logic deterministically
- Correctly invokes all Layer 3 scripts with proper arguments
- Safe to run repeatedly (no duplication, no drift)
"""

import subprocess
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent

PROGRAM_DIR = ROOT / "misc-1099/src/main/java/com/fordcredit/misc1099/batch/program"
RUNNER_DIR  = ROOT / "misc-1099/src/main/java/com/fordcredit/misc1099/batch/runner"


# ---------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------

def run(cmd: list[str]):
    print(f"\n▶ {' '.join(cmd)}")
    result = subprocess.run(cmd, cwd=ROOT)
    if result.returncode != 0:
        raise RuntimeError(f"Command failed: {' '.join(cmd)}")


def safe_delete(path: Path):
    if path.exists():
        print(f"🧹 Deleting {path}")
        path.unlink()
    else:
        print(f"↷ Skipping (not found): {path}")


# ---------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------

def main():
    if len(sys.argv) != 2:
        print("Usage: python layer3_rebuild_program.py <PROGRAM>")
        sys.exit(1)

    program = sys.argv[1].upper()

    print(f"\n=== Layer 3 FULL REBUILD for {program} ===")

    # -------------------------------------------------
    # 0️⃣ CLEAN — program-scoped only
    # -------------------------------------------------
    safe_delete(PROGRAM_DIR / f"{program}Tasklet.java")
    safe_delete(RUNNER_DIR  / f"{program}Runner.java")

    # -------------------------------------------------
    # 1️⃣ STRUCTURE (Layer 3A / 3B)
    # -------------------------------------------------
    run(["python", "layer3_cobol_to_java_tasklets.py"])

    # -------------------------------------------------
    # 2️⃣ LOGIC LAYERS
    # -------------------------------------------------
    run(["python", "layer3c_translate_paragraphs.py", program])
    run([
        "python",
        "layer3d_normalize_control_flow.py",
        "--program",
        program,
    ])

    # -------------------------------------------------
    # 3️⃣ IO + DOMAIN BINDING
    # -------------------------------------------------
    run(["python", "layer3e_generate_io_plumbing.py"])
    run(["python", "layer3e2_bind_output_file.py", program])
    run(["python", "layer3f1_bind_domain.py", program])
    run(["python", "layer3f2_bind_domain_runtime.py", program])
    run(["python", "layer3h_harden_tasklet_java.py", program])

    print(f"\n✔ Layer 3 rebuild complete for {program}")
    print("\nNext:")
    print("  cd misc-1099")
    print("  ./mvnw test")


if __name__ == "__main__":
    main()
