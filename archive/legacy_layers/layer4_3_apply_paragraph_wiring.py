#!/usr/bin/env python3
"""
Layer 4.3 — Paragraph Wiring (Cursor-Merge Loop)

Goal:
- Replace the *body* of mainProcess(MergeState) with a deterministic,
  COBOL-style 2-way merge loop (master vs corporate).
- Wire to existing paragraph methods generated in Layer 3/3E/3F:
    - readMaster(...) or readMasterFile(...)
    - readCorporate(...) or readCorporateFile(...)
    - writeMasterOut(...) OR writeMasterLine(...)
    - applyDeleteIndicatorToOutput(...) (if present)
- SAFE:
    - Never deletes any existing methods
    - Never touches braces outside the target method + helper anchor insert
    - Idempotent (re-running yields same output)
"""

from __future__ import annotations

from pathlib import Path
import argparse
import re

BASE_DIR = Path("misc-1099/src/main/java/com/fordcredit/misc1099/batch/program")

HELPERS_BEGIN = "    // BEGIN CURSOR MERGE HELPERS (Layer 4.3)"
HELPERS_END   = "    // END CURSOR MERGE HELPERS (Layer 4.3)"


# -------------------------------
# Small Java text utilities
# -------------------------------

def find_method_block(code: str, method_name: str) -> tuple[int, int] | None:
    """
    Find 'private void <method_name>(MergeState state) { ... }' block by brace matching.
    Returns (start_index, end_index_exclusive) of the full block text.
    """
    sig = re.search(
        rf"\bprivate\s+void\s+{re.escape(method_name)}\s*\(\s*MergeState\s+state\s*\)\s*\{{",
        code,
    )
    if not sig:
        return None

    start = sig.start()
    brace_start = code.find("{", sig.end() - 1)
    if brace_start == -1:
        return None

    depth = 0
    for i in range(brace_start, len(code)):
        if code[i] == "{":
            depth += 1
        elif code[i] == "}":
            depth -= 1
            if depth == 0:
                return (start, i + 1)
    return None


def has_method(code: str, method_name: str) -> bool:
    return re.search(
        rf"\bprivate\s+void\s+{re.escape(method_name)}\s*\(\s*MergeState\s+state\s*\)",
        code,
    ) is not None


def has_any_method(code: str, names: list[str]) -> bool:
    return any(has_method(code, n) for n in names)


def indent_of_line_containing(code: str, needle: str) -> str:
    """
    Best-effort: return leading whitespace of the line that contains `needle`.
    """
    for line in code.splitlines():
        if needle in line:
            return re.match(r"^\s*", line).group(0)
    return "    "


def ensure_helpers_anchor(code: str) -> str:
    """
    Ensure helper anchor exists once near end of class, before final '}'.
    """
    if HELPERS_BEGIN in code and HELPERS_END in code:
        return code

    idx = code.rfind("}")
    if idx == -1:
        raise RuntimeError("Invalid Java file: missing closing brace")

    anchor_block = (
        "\n\n"
        f"{HELPERS_BEGIN}\n"
        f"{HELPERS_END}\n"
    )

    return code[:idx].rstrip() + anchor_block + "\n}\n"


def replace_between_helpers(code: str, new_block: str) -> str:
    """
    Replace content between helper anchors.
    """
    if HELPERS_BEGIN not in code or HELPERS_END not in code:
        raise RuntimeError("Missing helper anchors for Layer 4.3")

    before, rest = code.split(HELPERS_BEGIN, 1)
    _, after = rest.split(HELPERS_END, 1)

    return before + HELPERS_BEGIN + "\n" + new_block.rstrip() + "\n" + HELPERS_END + after


# -------------------------------
# Rendering (Java)
# -------------------------------

def render_helpers(code: str) -> str:
    """
    Helper methods used by the wired mainProcess.
    Only relies on MergeState fields already present in your skeletons:
      - masterRawLine, corporateRawLine, corporateEof, masterEof
      - masterOutLine, deleteIndicator
    Uses applyDeleteIndicatorToOutput(state) if it exists; otherwise does a safe append.
    """

    has_apply = re.search(r"\bapplyDeleteIndicatorToOutput\s*\(\s*MergeState\s+state\s*\)", code) is not None

    apply_part = (
        "        applyDeleteIndicatorToOutput(state);\n"
        if has_apply
        else
        "        // Fallback (no applyDeleteIndicatorToOutput present)\n"
        "        if (\"M\".equals(state.deleteIndicator)) {\n"
        "            if (!state.masterOutLine.endsWith(\"|\")) state.masterOutLine = state.masterOutLine + \"|\";\n"
        "            if (!state.masterOutLine.endsWith(\"|M\")) state.masterOutLine = state.masterOutLine + \"M\";\n"
        "        }\n"
    )

    helpers = f"""
    private boolean isHeaderOrTrailer(MergeState state) {{
        if (state.masterRawLine == null) return false;
        return state.masterRawLine.startsWith("HDR") || state.masterRawLine.startsWith("TRL");
    }}

    private String masterKey(MergeState state) {{
        if (state.masterRawLine == null) return null;
        // For your testcase format: SSN|NAME|AMT|...
        String[] parts = state.masterRawLine.split("\\\\|", -1);
        return parts.length > 0 ? parts[0].trim() : null;
    }}

    private String corporateKey(MergeState state) {{
        if (state.corporateRawLine == null) return null;
        // For your testcase: corporate.txt is just "SSN"
        return state.corporateRawLine.trim();
    }}

    private int compareKeys(MergeState state) {{
        String mk = masterKey(state);
        String ck = corporateKey(state);
        if (mk == null && ck == null) return 0;
        if (mk == null) return -1;
        if (ck == null) return 1;
        return mk.compareTo(ck);
    }}

    private void setMechanized(MergeState state) {{
        // COBOL: MOVE WS-MECH-LIT TO TMMID-DELETE-INDICATOR
        state.deleteIndicator = "M";
    }}

    private void prepareOutputFromMaster(MergeState state) {{
        state.masterOutLine = state.masterRawLine;
{apply_part.rstrip()}
    }}
""".strip("\n")

    return helpers


def render_main_process_body(
    code: str,
    read_master: str,
    read_corp: str,
    write_out: str,
) -> str:
    """
    The cursor-merge loop implementation.
    - Uses read_master/read_corp methods already present.
    - Uses write_out which is either writeMasterOut(state) or writeMasterLine(state.masterOutLine)
    """

    # If write_out is "writeMasterOut", we assume it uses state.masterOutLine.
    # If write_out is "writeMasterLine", we call it with state.masterOutLine.
    if write_out == "writeMasterOut":
        write_stmt = "        writeMasterOut(state);\n"
    else:
        write_stmt = "        writeMasterLine(state.masterOutLine);\n"

    body = f"""
        // Layer 4.3: deterministic 2-way merge loop (master vs corporate)

        // Prime reads (if not already primed by earlier layers)
        if (!state.masterEof && state.masterRawLine == null) {{
            {read_master}(state);
        }}
        if (!state.corporateEof && state.corporateRawLine == null) {{
            {read_corp}(state);
        }}

        while (!state.masterEof) {{

            // Reset per-record flags (COBOL semantics)
            state.deleteIndicator = null;
            state.masterOutLine = null;

            // Pass headers/trailers through unchanged
            if (isHeaderOrTrailer(state)) {{
                prepareOutputFromMaster(state);
{write_stmt.rstrip()}
                {read_master}(state);
                continue;
            }}

            // If corporate exhausted, no matches possible: just copy master through
            if (state.corporateEof || state.corporateRawLine == null) {{
                prepareOutputFromMaster(state);
{write_stmt.rstrip()}
                {read_master}(state);
                continue;
            }}

            int cmp = compareKeys(state);

            if (cmp == 0) {{
                // MATCH → set mechanized flag and advance BOTH
                setMechanized(state);
                prepareOutputFromMaster(state);
{write_stmt.rstrip()}
                {read_master}(state);
                {read_corp}(state);
            }} else if (cmp < 0) {{
                // MASTER < CORP → write master, advance MASTER
                prepareOutputFromMaster(state);
{write_stmt.rstrip()}
                {read_master}(state);
            }} else {{
                // MASTER > CORP → advance CORPORATE only
                {read_corp}(state);
            }}
        }}
""".strip("\n")

    return body


def apply_layer_4_3(java_path: Path) -> None:
    code = java_path.read_text(encoding="utf-8")

    mp = find_method_block(code, "mainProcess")
    if not mp:
        raise RuntimeError(f"mainProcess(MergeState) not found in {java_path}")

    # Choose actual I/O method names present in the file
    read_master = "readMaster" if has_method(code, "readMaster") else "readMasterFile"
    read_corp   = "readCorporate" if has_method(code, "readCorporate") else "readCorporateFile"

    # Choose output write primitive present in file
    if has_method(code, "writeMasterOut"):
        write_out = "writeMasterOut"
    elif re.search(r"\bwriteMasterLine\s*\(", code):
        write_out = "writeMasterLine"
    else:
        raise RuntimeError("Neither writeMasterOut(MergeState) nor writeMasterLine(String) found; cannot wire outputs.")

    # Update mainProcess body
    start, end = mp
    mp_text = code[start:end]

    # Find signature line indentation
    sig_indent = indent_of_line_containing(mp_text, "private void mainProcess")
    body_indent = sig_indent + "    "

    # Build replacement method with same signature, new body
    sig_match = re.search(r"(private\s+void\s+mainProcess\s*\(\s*MergeState\s+state\s*\)\s*\{)", mp_text)
    if not sig_match:
        raise RuntimeError("Could not parse mainProcess signature")

    new_body = render_main_process_body(code, read_master, read_corp, write_out)

    # Re-indent body to match file style
    indented_body = "\n".join(
        (body_indent + line if line.strip() else "")
        for line in new_body.splitlines()
    )

    new_method = (
        sig_indent + sig_match.group(1) + "\n"
        + indented_body + "\n"
        + sig_indent + "}"
    )

    code = code[:start] + new_method + code[end:]

    # Ensure helper anchor + content
    code = ensure_helpers_anchor(code)
    helpers_block = render_helpers(code)
    code = replace_between_helpers(code, helpers_block)

    java_path.write_text(code, encoding="utf-8")


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("program", help="e.g. CCAC6340")
    args = parser.parse_args()

    program = args.program.upper()
    java_path = BASE_DIR / f"{program}Tasklet.java"
    if not java_path.exists():
        raise FileNotFoundError(java_path)

    apply_layer_4_3(java_path)
    print(f"✔ Layer 4.3 applied (paragraph wiring): {java_path}")


if __name__ == "__main__":
    main()
