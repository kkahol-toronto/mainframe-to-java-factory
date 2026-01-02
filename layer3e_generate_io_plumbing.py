"""
Layer 3E: File I/O Plumbing (FACTORY-SAFE)

Responsibilities:
- Seed Layer 3E anchors if missing
- Inject deterministic I/O helper methods
- NEVER touch Java braces
- NEVER modify Layer 3C or 3D output
- Idempotent and replay-safe

This layer assumes:
- Layer 3B owns Java structure
- Layer 3C owns paragraph methods
- Layer 3D owns control flow
"""

from pathlib import Path

# ---------------------------------------------------------------------
# PATHS
# ---------------------------------------------------------------------

ROOT = Path(".")
JAVA_DIR = ROOT / "misc-1099/src/main/java/com/fordcredit/misc1099/batch/program"

# ---------------------------------------------------------------------
# ANCHORS
# ---------------------------------------------------------------------

BEGIN = "// BEGIN IO PLUMBING (Layer 3E)"
END   = "// END IO PLUMBING (Layer 3E)"

# ---------------------------------------------------------------------
# IO METHODS (PLACEHOLDER / SAFE)
# ---------------------------------------------------------------------

IO_BLOCK = """\
    private void openFiles(MergeState state) {
        if (state.masterReader != null) {
            state.masterReader.open(state.executionContext);
        }
        if (state.corporateReader != null) {
            state.corporateReader.open(state.executionContext);
        }
        if (state.masterWriter != null) {
            state.masterWriter.open(state.executionContext);
        }
    }

    private void readMaster(MergeState state) {
        if (state.masterReader == null) return;
        try {
            state.master = state.masterReader.read();
            if (state.master == null) {
                state.masterEof = true;
            }
        } catch (Exception e) {
            throw new RuntimeException("Error reading master file", e);
        }
    }

    private void readCorporate(MergeState state) {
        if (state.corporateReader == null) return;
        try {
            state.corporate = state.corporateReader.read();
            if (state.corporate == null) {
                state.corporateEof = true;
            }
        } catch (Exception e) {
            throw new RuntimeException("Error reading corporate file", e);
        }
    }

    @SuppressWarnings("unchecked")
    private void writeMaster(MergeState state) {
        if (state.masterWriter == null || state.master == null) return;
        try {
            state.masterWriter.write(java.util.List.of(state.master));
        } catch (Exception e) {
            throw new RuntimeException("Error writing master record", e);
        }
    }
"""

# ---------------------------------------------------------------------
# STRUCTURE-SAFE HELPERS
# ---------------------------------------------------------------------

def ensure_anchors(java: str) -> str:
    """
    Ensure Layer 3E anchors exist.
    Inserts them just before the final class closing brace.
    """
    if BEGIN in java and END in java:
        return java

    lines = java.splitlines()
    out = []
    inserted = False

    for line in lines:
        if not inserted and line.strip() == "}":
            out.append("    // ======================================================")
            out.append(f"    {BEGIN}")
            out.append("")
            out.append(f"    {END}")
            out.append("    // ======================================================")
            inserted = True
        out.append(line)

    return "\n".join(out) + "\n"


def inject_io(java: str) -> str:
    """
    Inject IO_BLOCK strictly between Layer 3E anchors.
    """
    if BEGIN not in java or END not in java:
        raise RuntimeError("Layer 3E anchors missing")

    if IO_BLOCK.strip() in java:
        return java  # already applied

    before, rest = java.split(BEGIN)
    _, after = rest.split(END)

    return (
        before
        + BEGIN
        + "\n\n"
        + IO_BLOCK
        + "\n\n"
        + END
        + after
    )

# ---------------------------------------------------------------------
# MAIN
# ---------------------------------------------------------------------

def main():
    if not JAVA_DIR.exists():
        raise FileNotFoundError(JAVA_DIR)

    for tasklet in JAVA_DIR.glob("*Tasklet.java"):
        text = tasklet.read_text(encoding="utf-8")

        updated = ensure_anchors(text)
        updated = inject_io(updated)

        tasklet.write_text(updated, encoding="utf-8")
        print(f"✔ Layer 3E applied safely: {tasklet.name}")

if __name__ == "__main__":
    main()
