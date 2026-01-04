"""
Layer G.2.2: Promote Output to Expected (Golden Snapshot)

Purpose:
- G.2 compares output vs expected
- When expected does not exist yet, we promote current output
- This is a one-time bootstrap step (like snapshot tests)

Rules:
- Python-only
- Idempotent
- Never overwrites existing expected files
"""

from pathlib import Path
import argparse
import shutil


def ensure_dir(p: Path) -> None:
    p.mkdir(parents=True, exist_ok=True)


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("program", help="Program name, e.g. CCAC6340")
    args = parser.parse_args()

    program = args.program.upper()

    base = Path("work/mainframe_clean/testcases") / program
    output_dir = base / "output"
    expected_dir = base / "expected"

    ensure_dir(output_dir)
    ensure_dir(expected_dir)

    files = [
        "master_out.txt",
        "sysout.txt",
    ]

    print(f"\n=== Layer G.2.2: Promote Output → Expected for {program} ===\n")

    promoted = False

    for name in files:
        src = output_dir / name
        dst = expected_dir / name

        if dst.exists():
            print(f"✔ Expected already exists: {dst}")
            continue

        if src.exists():
            shutil.copyfile(src, dst)
            print(f"✔ Promoted {src} → {dst}")
            promoted = True
        else:
            print(f"⚠ Output missing, cannot promote: {src}")

    if not promoted:
        print("\nNothing promoted (expected already populated).")

    print("\nG.2.2 done.")
    print("\nNext command:")
    print(f"  python layer_g2_execute_and_compare.py {program}\n")


if __name__ == "__main__":
    main()
