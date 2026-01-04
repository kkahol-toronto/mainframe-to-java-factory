"""
Layer 3F: Domain Binding Anchors + Stubs (ANCHOR-SAFE, Python-driven)

This script:
- Adds/ensures two anchors:
  1) Inside MergeState: DOMAIN STATE (Layer 3F)
  2) In class body: DOMAIN BINDING (Layer 3F)
- Injects compile-safe stubs only (no business logic)
- Never touches braces beyond inserting inside known safe regions
- Idempotent: safe to run multiple times

NO LLM required.
"""

from pathlib import Path
import re

ROOT = Path(".")
PROGRAM_DIR = ROOT / "misc-1099/src/main/java/com/fordcredit/misc1099/batch/program"

# Anchors
STATE_BEGIN = "        // BEGIN DOMAIN STATE (Layer 3F)"
STATE_END   = "        // END DOMAIN STATE (Layer 3F)"

BIND_BEGIN  = "    // BEGIN DOMAIN BINDING (Layer 3F)"
BIND_END    = "    // END DOMAIN BINDING (Layer 3F)"

# Stubs (compile-safe; no external dependencies)
STATE_BLOCK = """\
        // BEGIN DOMAIN STATE (Layer 3F)

        /**
         * Raw lines / raw records (filled by readers in Layer 3E.1).
         * Keep as String for maximum portability until record formats are finalized.
         */
        String masterRawLine;
        String corporateRawLine;

        /**
         * Parsed domain objects (typed later when we lock record classes and parsers).
         * Layer 3F.1 will replace Object with specific POJO types.
         */
        Object masterRecord;
        Object corporateRecord;

        // END DOMAIN STATE (Layer 3F)
"""

BIND_BLOCK = """\
    // BEGIN DOMAIN BINDING (Layer 3F)

    /**
     * Convert the current raw master line into a domain object.
     * Layer 3F.1 will implement this using FieldSpecs + the runtime parser.
     */
    private void bindMasterRecord(MergeState state) {
        // TODO (Layer 3F.1):
        // 1) Use the correct FieldSpecs for master input record
        // 2) Parse state.masterRawLine into a POJO
        // 3) Assign into state.masterRecord (typed later)
    }

    /**
     * Convert the current raw corporate line into a domain object.
     * Layer 3F.1 will implement this using FieldSpecs + the runtime parser.
     */
    private void bindCorporateRecord(MergeState state) {
        // TODO (Layer 3F.1):
        // 1) Use the correct FieldSpecs for corporate input record
        // 2) Parse state.corporateRawLine into a POJO
        // 3) Assign into state.corporateRecord (typed later)
    }

    // END DOMAIN BINDING (Layer 3F)
"""

def ensure_state_anchor(java: str) -> str:
    """
    Ensure DOMAIN STATE anchor exists inside static class MergeState.
    Insert block near top of MergeState if missing.
    """
    if "BEGIN DOMAIN STATE (Layer 3F)" in java:
        return java

    # Find "static class MergeState {"
    m = re.search(r"(static\s+class\s+MergeState\s*\{\s*)", java)
    if not m:
        # If MergeState isn't present (should be), do not modify.
        raise RuntimeError("MergeState class not found. Regenerate tasklets via Layer 3B.")

    insert_pos = m.end()
    return java[:insert_pos] + "\n" + STATE_BLOCK + "\n" + java[insert_pos:]


def ensure_binding_anchor(java: str) -> str:
    """
    Ensure DOMAIN BINDING block exists in class body.
    Insert it just BEFORE the IO PLUMBING anchor if present, otherwise before final '}'.
    """
    if "BEGIN DOMAIN BINDING (Layer 3F)" in java:
        return java

    io_anchor = "// BEGIN IO PLUMBING (Layer 3E)"
    if io_anchor in java:
        idx = java.index(io_anchor)
        return java[:idx] + BIND_BLOCK + "\n\n" + java[idx:]

    # fallback: insert before last class closing brace
    last_brace = java.rfind("}")
    if last_brace == -1:
        raise RuntimeError("No closing brace found.")
    return java[:last_brace].rstrip() + "\n\n" + BIND_BLOCK + "\n\n}\n"


def main():
    if not PROGRAM_DIR.exists():
        raise FileNotFoundError(PROGRAM_DIR)

    tasklets = sorted(PROGRAM_DIR.glob("*Tasklet.java"))
    if not tasklets:
        print(f"No Tasklets found in {PROGRAM_DIR}. Run Layer 3B first.")
        return

    for tasklet in tasklets:
        java = tasklet.read_text(encoding="utf-8")

        updated = ensure_state_anchor(java)
        updated = ensure_binding_anchor(updated)

        if updated != java:
            tasklet.write_text(updated, encoding="utf-8")
            print(f"✔ Layer 3F injected: {tasklet.name}")
        else:
            print(f"✔ Layer 3F already present: {tasklet.name}")

    print("\nLayer 3F anchors/stubs complete (no business logic added).")

if __name__ == "__main__":
    main()
