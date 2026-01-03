#!/usr/bin/env python3
"""
Layer 3E: Generate File I/O Plumbing (ANCHOR-SAFE, INDENTATION-AGNOSTIC)

Guarantees:
- Works regardless of indentation
- Creates Layer 3E anchors if missing
- Injects IO plumbing exactly once
- Ensures MergeState has required stub fields
- NEVER duplicates fields owned by other layers
- Keeps entire repo compiling
"""

from __future__ import annotations
from pathlib import Path
import re

# ---------------------------------------------------------------------
# PATHS
# ---------------------------------------------------------------------

ROOT = Path(".")
JAVA_DIR = ROOT / "misc-1099/src/main/java/com/fordcredit/misc1099/batch/program"

BEGIN_MARKER = "// BEGIN IO PLUMBING (Layer 3E)"
END_MARKER   = "// END IO PLUMBING (Layer 3E)"

# ---------------------------------------------------------------------
# IO METHODS (CLASS-LEVEL)
# ---------------------------------------------------------------------

IO_BLOCK = """\
    // ---- Layer 3E IO plumbing (stub-safe) ----

    private void openFiles(MergeState state) {
        if (state.masterReader != null && state.executionContext != null) {
            state.masterReader.open(state.executionContext);
        }
        if (state.corporateReader != null && state.executionContext != null) {
            state.corporateReader.open(state.executionContext);
        }
        if (state.masterWriter != null && state.executionContext != null) {
            state.masterWriter.open(state.executionContext);
        }
        if (state.sysoutWriter != null && state.executionContext != null) {
            state.sysoutWriter.open(state.executionContext);
        }
    }

    private void readMaster(MergeState state) {
        if (state.masterReader == null) return;
        try {
            state.masterRawLine = state.masterReader.read();
            if (state.masterRawLine == null) state.masterEof = true;
        } catch (Exception e) {
            throw new RuntimeException("Error reading master file", e);
        }
    }

    private void readCorporate(MergeState state) {
        if (state.corporateReader == null) return;
        try {
            state.corporateRawLine = state.corporateReader.read();
            if (state.corporateRawLine == null) state.corporateEof = true;
        } catch (Exception e) {
            throw new RuntimeException("Error reading corporate file", e);
        }
    }

    @SuppressWarnings("unchecked")
    private void writeMasterOut(MergeState state) {
        if (state.masterWriter == null || state.masterOutLine == null) return;
        try {
            state.masterWriter.write(
                new org.springframework.batch.item.Chunk<>(
                    java.util.List.of(state.masterOutLine)
                )
            );
        } catch (Exception e) {
            throw new RuntimeException("Error writing master out", e);
        }
    }

    @SuppressWarnings("unchecked")
    private void writeSysout(MergeState state) {
        if (state.sysoutWriter == null || state.sysoutLine == null) return;
        try {
            state.sysoutWriter.write(
                new org.springframework.batch.item.Chunk<>(
                    java.util.List.of(state.sysoutLine)
                )
            );
        } catch (Exception e) {
            throw new RuntimeException("Error writing sysout", e);
        }
    }
"""

# ---------------------------------------------------------------------
# MERGE STATE STUBS (STRICT OWNERSHIP)
# ---------------------------------------------------------------------
# IMPORTANT:
# - DO NOT declare masterRawLine / corporateRawLine here
#   (owned by Layer 3F)
# - Only IO plumbing state lives here
# ---------------------------------------------------------------------

MERGE_STATE_STUBS = """\
        // ---- Layer 3E required stubs ----
        org.springframework.batch.item.ItemStreamReader<String> masterReader;
        org.springframework.batch.item.ItemStreamReader<String> corporateReader;
        org.springframework.batch.item.ItemStreamWriter<String> masterWriter;
        org.springframework.batch.item.ItemStreamWriter<String> sysoutWriter;

        org.springframework.batch.item.ExecutionContext executionContext =
                new org.springframework.batch.item.ExecutionContext();

        boolean masterEof = false;
        boolean corporateEof = false;

        // Output buffers (written by business logic)
        String masterOutLine;
        String sysoutLine;
"""

# ---------------------------------------------------------------------
# ANCHOR MANAGEMENT
# ---------------------------------------------------------------------

def ensure_anchors(java: str) -> str:
    if BEGIN_MARKER in java and END_MARKER in java:
        return java

    insert = f"""
    // ======================================================
    {BEGIN_MARKER}
    // ======================================================

    // ======================================================
    {END_MARKER}
    // ======================================================
"""

    idx = java.rfind("}")
    if idx == -1:
        raise RuntimeError("Invalid Java file: no closing brace")

    return java[:idx].rstrip() + insert + "\n}\n"


# ---------------------------------------------------------------------
# MERGE STATE PATCHING
# ---------------------------------------------------------------------

def ensure_merge_state(java: str) -> str:
    m = re.search(r"static\\s+class\\s+MergeState\\s*\\{", java)
    if not m:
        return java

    brace_start = java.find("{", m.start())
    depth = 0

    for i in range(brace_start, len(java)):
        if java[i] == "{":
            depth += 1
        elif java[i] == "}":
            depth -= 1
            if depth == 0:
                block = java[brace_start:i]
                if "masterReader" in block:
                    return java  # already patched safely

                return (
                    java[:brace_start + 1]
                    + "\n"
                    + MERGE_STATE_STUBS
                    + java[brace_start + 1:]
                )
    return java


# ---------------------------------------------------------------------
# BLOCK REPLACEMENT
# ---------------------------------------------------------------------

def replace_between(java: str, block: str) -> str:
    pattern = re.compile(
        rf"{re.escape(BEGIN_MARKER)}.*?{re.escape(END_MARKER)}",
        re.DOTALL
    )

    if not pattern.search(java):
        raise RuntimeError("Layer 3E anchors missing even after ensure_anchors()")

    replacement = (
        f"{BEGIN_MARKER}\n\n"
        f"{block.rstrip()}\n\n"
        f"{END_MARKER}"
    )

    return pattern.sub(replacement, java, count=1)


# ---------------------------------------------------------------------
# DRIVER
# ---------------------------------------------------------------------

def main():
    for tasklet in sorted(JAVA_DIR.glob("*Tasklet.java")):
        java = tasklet.read_text(encoding="utf-8")

        java = ensure_anchors(java)
        java = ensure_merge_state(java)
        java = replace_between(java, IO_BLOCK)

        tasklet.write_text(java, encoding="utf-8")
        print(f"✔ Layer 3E applied: {tasklet.name}")


if __name__ == "__main__":
    main()
