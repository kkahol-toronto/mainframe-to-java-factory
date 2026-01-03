"""
Layer 3F.2: Bind Domain Runtime (COMPILATION-SAFE, ANCHOR-SAFE)

- Ensures DOMAIN BINDING anchors exist (creates if missing)
- Injects bindMasterRecord / bindCorporateRecord using ReflectionDomainBinder
- Uses fully qualified names (no missing imports)
- Does NOT assume any FixedWidthParser constructor shape in user code
  (binder handles it via reflection + FieldSpecs)
"""

from __future__ import annotations
from pathlib import Path
import json
import re
import sys

ROOT = Path(".")
JAVA_FILE = ROOT / "misc-1099/src/main/java/com/fordcredit/misc1099/batch/program"

BINDINGS_DIR = ROOT / "work/migration_ir/bindings"

BEGIN = "// BEGIN DOMAIN BINDING (Layer 3F)"
END   = "// END DOMAIN BINDING (Layer 3F)"

def bindings_path(program: str) -> Path:
    return BINDINGS_DIR / f"{program}.bindings.json"

def ensure_anchors(java: str) -> str:
    if "BEGIN DOMAIN BINDING (Layer 3F)" in java and "END DOMAIN BINDING (Layer 3F)" in java:
        return java

    # Insert anchors before final class close brace
    idx = java.rfind("}")
    if idx == -1:
        raise RuntimeError("Invalid Java file: no closing brace")

    insert = "\n\n" + BEGIN + "\n" + END + "\n"
    return java[:idx].rstrip() + insert + "}\n"

def replace_between(java: str, begin: str, end: str, block: str) -> str:
    if begin not in java or end not in java:
        raise RuntimeError(f"Missing anchors: {begin} / {end}")

    before, rest = java.split(begin, 1)
    _, after = rest.split(end, 1)

    return before + begin + "\n\n" + block.rstrip() + "\n\n" + end + after

def build_binding_block(program: str, b: dict) -> str:
    mp = b["masterPojoClass"]
    ms = b["masterFieldSpecsClass"]
    cp = b["corporatePojoClass"]
    cs = b["corporateFieldSpecsClass"]

    return f"""\
    // Auto-bound runtime domain binding (Layer 3F.2)
    private void bindMasterRecord(MergeState state) {{
        state.masterRecord = com.fordcredit.misc1099.batch.bind.ReflectionDomainBinder.bind(
                state.masterRawLine,
                "{mp}",
                "{ms}"
        );
    }}

    private void bindCorporateRecord(MergeState state) {{
        state.corporateRecord = com.fordcredit.misc1099.batch.bind.ReflectionDomainBinder.bind(
                state.corporateRawLine,
                "{cp}",
                "{cs}"
        );
    }}
"""

def apply(program: str):
    tasklet_path = JAVA_FILE / f"{program}Tasklet.java"
    if not tasklet_path.exists():
        raise FileNotFoundError(tasklet_path)

    bp = bindings_path(program)
    if not bp.exists():
        raise FileNotFoundError(bp)

    b = json.loads(bp.read_text(encoding="utf-8"))

    java = tasklet_path.read_text(encoding="utf-8")
    java = ensure_anchors(java)

    block = build_binding_block(program, b)
    java = replace_between(java, BEGIN, END, block)

    tasklet_path.write_text(java, encoding="utf-8")
    print(f"✔ 3F.2 applied: {program}")

def main():
    if len(sys.argv) != 2:
        print("Usage: python layer3f2_bind_domain_runtime.py <PROGRAM>")
        sys.exit(1)
    apply(sys.argv[1].upper())

if __name__ == "__main__":
    main()
