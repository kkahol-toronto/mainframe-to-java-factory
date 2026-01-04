#!/usr/bin/env python3
"""
Layer 4 (CCAC6340 pilot): Deterministic merge semantics injection (ANCHOR-SAFE)

Goal (Phase 4.1 for CCAC6340):
- Implement mechanized flag behavior (WS-MECH-LIT => "M") deterministically.
- Keep everything Python-driven.
- Inject ONLY via anchors / safe regex surgery.
- Idempotent (can re-run).

This version uses a corporate SSN set (HashSet) loaded once.
Later Layer 4 will upgrade this to a two-cursor sorted merge engine.
"""

from __future__ import annotations
from pathlib import Path
import argparse
import re

ROOT = Path(".")
JAVA_FILE = ROOT / "misc-1099/src/main/java/com/fordcredit/misc1099/batch/program/CCAC6340Tasklet.java"

L4_BEGIN = "// BEGIN MERGE LOGIC (Layer 4)"
L4_END = "// END MERGE LOGIC (Layer 4)"

# We place the corporate-loading helper inside the Layer 3E IO plumbing anchor block
E_BEGIN = "// BEGIN IO PLUMBING (Layer 3E)"
E_END = "// END IO PLUMBING (Layer 3E)"

CORP_LOADER_METHOD = r"""
    // ---- Layer 4 helper: load corporate SSNs into memory ----
    private void ensureCorporateIndexLoaded(MergeState state, java.nio.file.Path corporatePath) {
        if (state == null) return;
        if (state.corporateSsnSetLoaded) return;
        state.corporateSsnSetLoaded = true;

        if (corporatePath == null) {
            // No corporate path => treat as empty
            return;
        }

        try {
            if (!java.nio.file.Files.exists(corporatePath)) {
                return;
            }
            java.util.List<String> lines = java.nio.file.Files.readAllLines(corporatePath);
            for (String line : lines) {
                if (line == null) continue;
                String ssn = line.trim();
                if (ssn.isEmpty()) continue;
                state.corporateSsnSet.add(ssn);
            }
        } catch (Exception e) {
            throw new RuntimeException("Failed to load corporate SSN index from: " + corporatePath, e);
        }
    }
""".strip("\n")

MERGE_STATE_FIELDS = r"""
        // ---- Layer 4 state (CCAC6340) ----
        java.util.Set<String> corporateSsnSet = new java.util.HashSet<>();
        boolean corporateSsnSetLoaded = false;
""".strip("\n")

L4_BLOCK = r"""
        // Layer 4: mechanized merge semantics (CCAC6340 pilot)
        // - Pass-through HDR/TRL
        // - If master SSN exists in corporate list -> append "|M"
        // - Otherwise write unchanged
        //
        // Inputs:
        //   masterRawLine   : already populated by your runner / Layer 3E read path
        //   corporate file  : work/mainframe_clean/testcases/CCAC6340/input/corporate.txt
        //
        // Output:
        //   master_out.txt (written via writeMasterLine)

        // Reset per-record
        state.deleteIndicator = null;
        state.masterOutLine = null;

        if (state.masterRawLine == null) {
            return;
        }

        // Pass-through headers and trailers
        if (state.masterRawLine.startsWith("HDR") || state.masterRawLine.startsWith("TRL")) {
            state.masterOutLine = state.masterRawLine;
            writeMasterLine(state.masterOutLine);
            return;
        }

        // Load corporate SSNs once
        java.nio.file.Path corporatePath = java.nio.file.Paths.get(
                "work", "mainframe_clean", "testcases", "CCAC6340", "input", "corporate.txt"
        );
        ensureCorporateIndexLoaded(state, corporatePath);

        // Extract SSN from master detail line: "SSN|NAME|AMT|..."
        String[] parts = state.masterRawLine.split("\\\\|", -1);
        String masterSsn = (parts.length > 0) ? parts[0].trim() : "";

        if (!masterSsn.isEmpty() && state.corporateSsnSet.contains(masterSsn)) {
            state.deleteIndicator = "M";
        }

        // Build output line
        state.masterOutLine = state.masterRawLine;

        // Ensure trailing delimiter exists, then add M (idempotent)
        if (!state.masterOutLine.endsWith("|")) {
            state.masterOutLine = state.masterOutLine + "|";
        }
        if ("M".equals(state.deleteIndicator) && !state.masterOutLine.endsWith("|M")) {
            state.masterOutLine = state.masterOutLine + "M";
        }

        writeMasterLine(state.masterOutLine);
""".strip("\n")


def replace_between(text: str, begin: str, end: str, new_block: str) -> str:
    pattern = re.compile(rf"{re.escape(begin)}.*?{re.escape(end)}", re.DOTALL)
    if not pattern.search(text):
        raise RuntimeError(f"Missing anchors: {begin} / {end}")
    replacement = f"{begin}\n{new_block}\n{end}"
    return pattern.sub(replacement, text, count=1)


def ensure_markers_in_main_process(java: str) -> str:
    """
    Ensure Layer 4 anchors exist inside mainProcess(MergeState state).
    If missing, insert an empty anchored block at start of method body.
    """
    m = re.search(r"\bprivate\s+void\s+mainProcess\s*\(\s*MergeState\s+state\s*\)\s*\{", java)
    if not m:
        raise RuntimeError("mainProcess(MergeState state) not found")

    brace_start = java.find("{", m.end() - 1)
    if brace_start < 0:
        raise RuntimeError("Could not locate mainProcess body brace")

    # Find end of method with brace matching
    depth = 0
    end_idx = None
    for i in range(brace_start, len(java)):
        if java[i] == "{":
            depth += 1
        elif java[i] == "}":
            depth -= 1
            if depth == 0:
                end_idx = i
                break
    if end_idx is None:
        raise RuntimeError("Could not find end of mainProcess")

    body = java[brace_start + 1 : end_idx]

    if L4_BEGIN in body and L4_END in body:
        return java

    anchored = f"""
{L4_BEGIN}
{L4_END}
""".strip("\n")

    # Insert anchored block at the top of mainProcess body
    new_body = anchored + "\n\n" + body.lstrip("\n")
    return java[: brace_start + 1] + "\n" + new_body + java[end_idx:]


def ensure_merge_state_fields(java: str) -> str:
    """
    Ensure MergeState contains corporate SSN set + loaded flag.
    """
    m = re.search(r"\bstatic\s+class\s+MergeState\s*\{", java)
    if not m:
        raise RuntimeError("MergeState class not found")

    brace_start = java.find("{", m.end() - 1)
    depth = 0
    end_idx = None
    for i in range(brace_start, len(java)):
        if java[i] == "{":
            depth += 1
        elif java[i] == "}":
            depth -= 1
            if depth == 0:
                end_idx = i
                break
    if end_idx is None:
        raise RuntimeError("Could not find end of MergeState")

    block = java[brace_start + 1 : end_idx]

    if "corporateSsnSet" in block and "corporateSsnSetLoaded" in block:
        return java

    # Insert near end of MergeState (before closing brace)
    insertion = "\n" + MERGE_STATE_FIELDS + "\n"
    return java[:end_idx] + insertion + java[end_idx:]


def ensure_io_helper(java: str) -> str:
    """
    Put ensureCorporateIndexLoaded(...) inside Layer 3E IO plumbing block.
    """
    if E_BEGIN not in java or E_END not in java:
        raise RuntimeError("Layer 3E anchors missing (run Layer 3E first)")

    segment = re.search(rf"{re.escape(E_BEGIN)}.*?{re.escape(E_END)}", java, re.DOTALL)
    if not segment:
        raise RuntimeError("Could not locate Layer 3E block")

    block = segment.group(0)
    if "ensureCorporateIndexLoaded" in block:
        return java

    # Inject helper just before E_END
    updated_block = block.replace(E_END, CORP_LOADER_METHOD + "\n\n" + E_END)
    return java.replace(block, updated_block, 1)


def apply(program: str):
    if program != "CCAC6340":
        raise RuntimeError("This pilot script currently supports only CCAC6340")

    if not JAVA_FILE.exists():
        raise FileNotFoundError(JAVA_FILE)

    java = JAVA_FILE.read_text(encoding="utf-8")

    java = ensure_merge_state_fields(java)
    java = ensure_io_helper(java)
    java = ensure_markers_in_main_process(java)

    # Now replace inside L4 anchors
    java = replace_between(java, L4_BEGIN, L4_END, L4_BLOCK)

    JAVA_FILE.write_text(java, encoding="utf-8")
    print(f"✔ Layer 4 applied: {JAVA_FILE}")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("program", help="Program id, e.g. CCAC6340")
    args = ap.parse_args()
    apply(args.program.upper())


if __name__ == "__main__":
    main()
