"""
Layer 3E.2: Bind Output File Sink (Factory-Grade)

Responsibilities:
- Bind Java tasklet output to a deterministic filesystem path
- NEVER modify Java structure outside Layer 3E anchors
- Idempotent and safe to re-run

Output contract:
work/runtime/<PROGRAM>/master_out.txt
"""

from pathlib import Path
import sys

ROOT = Path(".")
JAVA_DIR = ROOT / "misc-1099/src/main/java/com/fordcredit/misc1099/batch/program"

BEGIN = "// BEGIN IO PLUMBING (Layer 3E)"
END   = "// END IO PLUMBING (Layer 3E)"

RUNTIME_BASE = "work/runtime"

IO_BLOCK = f"""\
    private java.io.BufferedWriter masterOutWriter;

    private void initOutputWriter(String programName) {{
        try {{
            java.nio.file.Path outPath =
                java.nio.file.Paths.get("{RUNTIME_BASE}", programName, "master_out.txt");
            java.nio.file.Files.createDirectories(outPath.getParent());
            masterOutWriter = java.nio.file.Files.newBufferedWriter(outPath);
        }} catch (Exception e) {{
            throw new RuntimeException("Failed to initialize master output writer", e);
        }}
    }}

    private void writeMasterLine(String line) {{
        if (masterOutWriter == null || line == null) return;
        try {{
            masterOutWriter.write(line);
            masterOutWriter.newLine();
        }} catch (Exception e) {{
            throw new RuntimeException("Failed writing master output", e);
        }}
    }}

    private void closeOutputWriter() {{
        if (masterOutWriter != null) {{
            try {{
                masterOutWriter.close();
            }} catch (Exception e) {{
                throw new RuntimeException("Failed closing master output writer", e);
            }}
        }}
    }}
"""


def replace_between(text: str, begin: str, end: str, block: str) -> str:
    if begin not in text or end not in text:
        raise RuntimeError(f"Missing anchors: {begin} / {end}")

    before, rest = text.split(begin, 1)
    _, after = rest.split(end, 1)

    return (
        before
        + begin
        + "\n\n"
        + block
        + "\n\n"
        + end
        + after
    )


def apply_to_program(program: str):
    tasklet = JAVA_DIR / f"{program}Tasklet.java"
    if not tasklet.exists():
        raise FileNotFoundError(tasklet)

    java = tasklet.read_text(encoding="utf-8")

    if "masterOutWriter" in java:
        print(f"✔ 3E.2 already applied: {program}")
        return

    updated = replace_between(java, BEGIN, END, IO_BLOCK)
    tasklet.write_text(updated, encoding="utf-8")
    print(f"✔ 3E.2 applied: {program}")


def main():
    if len(sys.argv) != 2:
        print("Usage: python layer3e2_bind_output_file.py <PROGRAM>")
        sys.exit(1)

    apply_to_program(sys.argv[1].upper())


if __name__ == "__main__":
    main()
