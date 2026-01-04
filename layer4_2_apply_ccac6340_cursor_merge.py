#!/usr/bin/env python3
"""
Layer 4.2 (CCAC6340): Two-cursor sorted merge semantics (ANCHOR-SAFE, IDEMPOTENT)

What it does:
- Injects cursor-based merge behavior into mainProcess(MergeState state)
- Keeps changes bounded inside Layer 4 anchors
- Adds only the minimum state fields required
- Re-usable: safe to re-run

Assumptions for CCAC6340 testcase harness:
- master lines are in state.masterRawLine (already loaded upstream)
- corporate lines are loaded via readCorporateFile(state) into state.corporateRawLine
- state.corporateEof becomes true when corporate file ends
- Output is written via writeMasterLine(...) into master_out.txt
"""

from __future__ import annotations
from pathlib import Path
import argparse
import re

ROOT = Path(".")
JAVA_FILE = ROOT / "misc-1099/src/main/java/com/fordcredit/misc1099/batch/program/CCAC6340Tasklet.java"

L4_BEGIN = "// BEGIN MERGE LOGIC (Layer 4)"
L4_END = "// END MERGE LOGIC (Layer 4)"

E_BEGIN = "// BEGIN IO PLUMBING (Layer 3E)"
E_END = "// END IO PLUMBING (Layer 3E)"

MERGE_STATE_FIELDS = r"""
        // ---- Layer 4.2 cursor merge state ----
        boolean corporatePrimed = false;   // have we read the first corporate record?
""".strip("\n")

# This is the *cursor merge* implementation we inject
L4_BLOCK = r"""
        // Layer 4.2: Two-cursor sorted merge semantics (CCAC6340)
        //
        // COBOL intent (from 2000-MAIN-PROCESS):
        // - Compare master SSN (TMMID-SOCIAL-SEC-NUMBER) to corporate SSN (TTCD-SOCIAL-SEC-NUMBER)
        // - If equal: MOVE WS-MECH-LIT ("M") to indicator; write; read next master
        // - If master < corporate: write; read next master
        // - If master > corporate: read next corporate (advance corporate cursor)
        // - If corporate EOF: remaining master writes unchanged
        //
        // This method processes *one master record per invocation* (your runner drives iteration).
        // It assumes:
        //   - state.masterRawLine is current master record
        //   - state.corporateRawLine is current corporate record (or null if not read yet)
        //   - readCorporateFile(state) advances corporate cursor & sets state.corporateEof
        //   - writeMasterLine(...) writes output line

        // Reset per record
        state.deleteIndicator = null;
        state.masterOutLine = null;

        if (state.masterRawLine == null) {
            return;
        }

        // Pass-through headers/trailers unchanged
        if (state.masterRawLine.startsWith("HDR") || state.masterRawLine.startsWith("TRL")) {
            state.masterOutLine = state.masterRawLine;
            writeMasterLine(state.masterOutLine);
            return;
        }

        // Prime corporate cursor once (only if not already EOF)
        if (!state.corporatePrimed && !state.corporateEof) {
            readCorporateFile(state);
            state.corporatePrimed = true;
        }

        // If corporate EOF: write master unchanged
        if (state.corporateEof || state.corporateRawLine == null) {
            state.masterOutLine = state.masterRawLine;
            // ensure trailing delimiter
            if (!state.masterOutLine.endsWith("|")) state.masterOutLine = state.masterOutLine + "|";
            writeMasterLine(state.masterOutLine);
            return;
        }

        // Extract SSNs
        String[] parts = state.masterRawLine.split("\\\\|", -1);
        String masterSsn = (parts.length > 0) ? parts[0].trim() : "";
        String corporateSsn = state.corporateRawLine.trim();

        // Defensive: if master SSN missing, just pass through
        if (masterSsn.isEmpty()) {
            state.masterOutLine = state.masterRawLine;
            if (!state.masterOutLine.endsWith("|")) state.masterOutLine = state.masterOutLine + "|";
            writeMasterLine(state.masterOutLine);
            return;
        }

        int cmp = masterSsn.compareTo(corporateSsn);

        if (cmp == 0) {
            // MATCH: set mechanized flag
            state.deleteIndicator = "M";
            state.masterOutLine = state.masterRawLine;
            if (!state.masterOutLine.endsWith("|")) state.masterOutLine = state.masterOutLine + "|";
            if (!state.masterOutLine.endsWith("|M")) state.masterOutLine = state.masterOutLine + "M";
            writeMasterLine(state.masterOutLine);

            // COBOL advances MASTER (corporate stays until master > corporate / next compare)
            return;
        }

        if (cmp < 0) {
            // master < corporate: write master unchanged; advance master (done by caller loop)
            state.masterOutLine = state.masterRawLine;
            if (!state.masterOutLine.endsWith("|")) state.masterOutLine = state.masterOutLine + "|";
            writeMasterLine(state.masterOutLine);
            return;
        }

        // master > corporate: advance corporate cursor (possibly multiple times)
        while (cmp > 0 && !state.corporateEof) {
            readCorporateFile(state);
            if (state.corporateEof || state.corporateRawLine == null) break;
            corporateSsn = state.corporateRawLine.trim();
            cmp = masterSsn.compareTo(corporateSsn);
        }

        // After advancing corporate:
        if (!state.corporateEof && state.corporateRawLine != null && masterSsn.equals(state.corporateRawLine.trim())) {
            state.deleteIndicator = "M";
            state.masterOutLine = state.masterRawLine;
            if (!state.masterOutLine.endsWith("|")) state.masterOutLine = state.masterOutLine + "|";
            if (!state.masterOutLine.endsWith("|M")) state.masterOutLine = state.masterOutLine + "M";
            writeMasterLine(state.masterOutLine);
            return;
        }

        // Otherwise write master unchanged
        state.masterOutLine = state.masterRawLine;
        if (!state.masterOutLine.endsWith("|")) state.masterOutLine = state.masterOutLine + "|";
        writeMasterLine(state.masterOutLine);
""".strip("\n")


def replace_between(text: str, begin: str, end: str, new_block: str) -> str:
    pattern = re.compile(rf"{re.escape(begin)}.*?{re.escape(end)}", re.DOTALL)
    if not pattern.search(text):
        raise RuntimeError(f"Missing anchors: {begin} / {end}")
    replacement = f"{begin}\n{new_block}\n{end}"
    return pattern.sub(replacement, text, count=1)


def ensure_l4_anchors_in_main_process(java: str) -> str:
    """
    Ensure L4 anchors exist inside mainProcess(MergeState state).
    """
    m = re.search(r"\bprivate\s+void\s+mainProcess\s*\(\s*MergeState\s+state\s*\)\s*\{", java)
    if not m:
        raise RuntimeError("mainProcess(MergeState state) not found")

    brace_start = java.find("{", m.end() - 1)
    if brace_start < 0:
        raise RuntimeError("Could not locate mainProcess body brace")

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

    anchored = f"{L4_BEGIN}\n{L4_END}\n"
    new_body = anchored + "\n" + body.lstrip("\n")
    return java[: brace_start + 1] + "\n" + new_body + java[end_idx:]


def ensure_merge_state_fields(java: str) -> str:
    """
    Ensure MergeState has corporatePrimed flag (and does NOT duplicate).
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

    if "corporatePrimed" in block:
        return java

    insertion = "\n" + MERGE_STATE_FIELDS + "\n"
    return java[:end_idx] + insertion + java[end_idx:]


def ensure_prereq_anchors_exist(java: str) -> None:
    if E_BEGIN not in java or E_END not in java:
        raise RuntimeError("Layer 3E anchors missing (run Layer 3E first)")
    if "private void readCorporateFile(MergeState state)" not in java:
        # some versions may have signature spacing variations; don't overfit
        pass


def apply(program: str):
    if program != "CCAC6340":
        raise RuntimeError("This script currently supports only CCAC6340")

    if not JAVA_FILE.exists():
        raise FileNotFoundError(JAVA_FILE)

    java = JAVA_FILE.read_text(encoding="utf-8")

    ensure_prereq_anchors_exist(java)

    java = ensure_merge_state_fields(java)
    java = ensure_l4_anchors_in_main_process(java)
    java = replace_between(java, L4_BEGIN, L4_END, L4_BLOCK)

    JAVA_FILE.write_text(java, encoding="utf-8")
    print(f"✔ Layer 4.2 applied (cursor merge): {JAVA_FILE}")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("program", help="Program id, e.g. CCAC6340")
    args = ap.parse_args()
    apply(args.program.upper())


if __name__ == "__main__":
    main()
