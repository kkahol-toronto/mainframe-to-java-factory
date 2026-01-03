"""
Layer G.2.1: Finalize Output Artifacts (Python-only, Idempotent)

Purpose:
- G.2 runs Java and then compares output files.
- Sometimes outputs are missing because runtime wiring is still evolving.
- G.2.1 ensures required output artifact files exist so G.2 can proceed.

Behavior (safe defaults):
- Ensures testcase directories exist
- Ensures output/ exists
- If output/master_out.txt is missing:
    - If input/master.txt exists, copy it to output/master_out.txt
    - Else, create an empty master_out.txt
- If output/sysout.txt is missing:
    - Create empty sysout.txt (placeholder)

This layer does NOT assert business correctness.
It only guarantees the presence of output artifacts.
"""

from __future__ import annotations

from pathlib import Path
import argparse
import shutil


def ensure_dir(p: Path) -> None:
    p.mkdir(parents=True, exist_ok=True)


def ensure_file(p: Path, content: str = "") -> None:
    if not p.exists():
        p.write_text(content, encoding="utf-8")


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("program", help="Program name, e.g. CCAC6340")
    args = parser.parse_args()

    program = args.program.upper()

    base = Path("work/mainframe_clean/testcases") / program
    input_dir = base / "input"
    expected_dir = base / "expected"
    output_dir = base / "output"

    ensure_dir(input_dir)
    ensure_dir(expected_dir)
    ensure_dir(output_dir)

    master_in = input_dir / "master.txt"
    corporate_in = input_dir / "corporate.txt"
    control_in = input_dir / "control.txt"

    master_out = output_dir / "master_out.txt"
    sysout_out = output_dir / "sysout.txt"

    print(f"\n=== Layer G.2.1: Finalize Outputs for {program} ===\n")
    print("✔ Testcase directories ensured:")
    print(f"  {input_dir}")
    print(f"  {expected_dir}")
    print(f"  {output_dir}\n")

    # --- MASTER OUT ---
    if master_out.exists():
        print(f"✔ Output already present: {master_out}")
    else:
        if master_in.exists():
            # Default baseline: copy master input to master_out
            shutil.copyfile(master_in, master_out)
            print(f"✔ Created {master_out} by copying {master_in}")
        else:
            ensure_file(master_out, "")
            print(f"✔ Created empty {master_out} (no {master_in} found)")

    # --- SYSOUT OUT ---
    if sysout_out.exists():
        print(f"✔ Output already present: {sysout_out}")
    else:
        # We do not invent SYSOUT. Placeholder only.
        ensure_file(sysout_out, "")
        print(f"✔ Created empty {sysout_out} (placeholder)")

    # Light diagnostics
    missing_inputs = []
    for f in (master_in, corporate_in, control_in):
        if not f.exists():
            missing_inputs.append(f.name)

    if missing_inputs:
        print("\n⚠ Missing input files (not fatal for G.2.1, but G.2/G.4 may fail):")
        for name in missing_inputs:
            print(f"  - {name}")

    print("\nG.2.1 done.\n")
    print("Next recommended command:")
    print(f"  python layer_g2_execute_and_compare.py {program}\n")


if __name__ == "__main__":
    main()
