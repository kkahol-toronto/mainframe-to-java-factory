#!/usr/bin/env python3
"""
Layer 3D — Normalize COBOL Control Flow into Clean Java (SAFE)

Responsibilities:
- Rename numeric COBOL paragraph methods to legal Java names
- Rewrite PERFORM-style calls into Java method calls
- Ensure execute() calls mainline(state)
- Guarantee exactly ONE mainline(MergeState)
- NEVER create or modify MergeState (owned by Layer 3E)
- NEVER rename methods to IO-owned names
"""

from pathlib import Path
import re
import argparse

BASE_DIR = Path("misc-1099/src/main/java/com/fordcredit/misc1099/batch/program")

# ⚠️ DO NOT include IO lifecycle names here
METHOD_MAP = {
    "0000": "mainline",
    "1000": "initialize",
    "2000": "mainProcess",
    "2200": "processEntireMaster",
    "9000": "endOfJob",
    "9010": "wrapUp",
}

# ------------------------------------------------------------------
# METHOD NORMALIZATION
# ------------------------------------------------------------------

def normalize_methods(code: str) -> str:
    """
    Rename illegal numeric paragraph methods into valid Java methods.
    Only touches methods that STILL start with digits.
    """

    def repl(match):
        para = match.group(1)
        suffix = match.group(2) or ""
        new_name = METHOD_MAP.get(para, f"p{para}{suffix}")

        return (
            f"// COBOL {para}-{suffix.upper()}\n"
            f"private void {new_name}(MergeState state) {{"
        )

    return re.sub(
        r"private\s+void\s+(\d{4})([A-Za-z0-9_]*)\s*\(\s*MergeState\s+state\s*\)\s*\{",
        repl,
        code,
    )

# ------------------------------------------------------------------
# PERFORM → METHOD CALLS
# ------------------------------------------------------------------

def replace_perform_calls(code: str) -> str:
    """
    Replace numeric paragraph invocations with Java calls.
    Only replaces numeric-leading invocations.
    """
    for para, name in METHOD_MAP.items():
        code = re.sub(
            rf"\b{para}[A-Za-z0-9_]*\s*\(\s*\)",
            f"{name}(state)",
            code,
        )
    return code

# ------------------------------------------------------------------
# EXECUTE() PATCH
# ------------------------------------------------------------------

def patch_execute(code: str) -> str:
    """
    Ensure execute() creates MergeState and calls mainline(state).
    """
    execute_pattern = re.compile(
        r"@Override\s+public\s+RepeatStatus\s+execute\s*\([^)]*\)\s*throws\s+Exception\s*\{.*?return\s+RepeatStatus\.FINISHED;\s*\}",
        re.S,
    )

    replacement = (
        "@Override\n"
        "public RepeatStatus execute(\n"
        "        StepContribution contribution,\n"
        "        ChunkContext chunkContext) throws Exception {\n\n"
        "    MergeState state = new MergeState();\n"
        "    mainline(state);\n"
        "    return RepeatStatus.FINISHED;\n"
        "}"
    )

    if execute_pattern.search(code):
        return execute_pattern.sub(replacement, code)

    raise RuntimeError("execute() not found — invalid Tasklet skeleton")

# ------------------------------------------------------------------
# MAINLINE GUARANTEE
# ------------------------------------------------------------------

def ensure_mainline_exists(code: str) -> str:
    """
    Ensure exactly one mainline(MergeState) exists.
    Never duplicates.
    """
    if re.search(r"private\s+void\s+mainline\s*\(\s*MergeState\s+state\s*\)", code):
        return code

    insert_after = re.search(
        r"@Override\s+public\s+RepeatStatus\s+execute\s*\([^)]*\)\s*throws\s+Exception\s*\{.*?\}",
        code,
        re.S,
    )

    if not insert_after:
        raise RuntimeError("Cannot find execute() to attach mainline()")

    insertion = (
        "\n\n"
        "/**\n"
        " * COBOL main entry point.\n"
        " * Control flow normalized by Layer 3D.\n"
        " */\n"
        "private void mainline(MergeState state) {\n"
        "    // Entry point — paragraph calls injected by Layer 3C\n"
        "}\n"
    )

    idx = insert_after.end()
    return code[:idx] + insertion + code[idx:]

# ------------------------------------------------------------------
# DRIVER
# ------------------------------------------------------------------

def main(program: str):
    path = BASE_DIR / f"{program}Tasklet.java"
    if not path.exists():
        raise FileNotFoundError(path)

    code = path.read_text(encoding="utf-8")

    code = normalize_methods(code)
    code = replace_perform_calls(code)
    code = patch_execute(code)
    code = ensure_mainline_exists(code)

    path.write_text(code, encoding="utf-8")
    print(f"✔ Layer 3D applied safely to {path}")

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--program", required=True)
    args = parser.parse_args()
    main(args.program)
