"""
Layer G.3.1: Bind Runner Output Paths (Repo-root canonical, Python-only, Idempotent)

Problem this solves:
- Java runner may execute with CWD = misc-1099/
- Python G.2 checks paths relative to repo root
=> Output files exist but G.2 can't find them because Java wrote to misc-1099/work/... instead of work/...

This layer patches the generated runner so it ALWAYS resolves testcases from repo root:
- If CWD contains "misc-1099", repo root is parent of CWD
- Otherwise repo root is CWD

It then writes outputs to:
  <repoRoot>/work/mainframe_clean/testcases/<PROGRAM>/output/<file>

Idempotent:
- If marker is already present, does nothing.
"""

from __future__ import annotations

from pathlib import Path
import argparse
import re


RUNNER_DIR = Path("misc-1099/src/main/java/com/fordcredit/misc1099/batch/runner")
MARKER = "// G3.1_CANONICAL_PATHS"


def find_runner(program: str) -> Path:
    p = RUNNER_DIR / f"{program}Runner.java"
    if not p.exists():
        raise FileNotFoundError(f"Runner not found: {p}")
    return p


def ensure_imports(src: str) -> str:
    # Ensure we have java.nio.file.Path and Paths
    if "import java.nio.file.Path;" not in src:
        src = src.replace("package ", "package ", 1)  # no-op but keeps intent clear
        # Insert after package line
        lines = src.splitlines()
        out = []
        inserted = False
        for i, line in enumerate(lines):
            out.append(line)
            if not inserted and line.startswith("package "):
                out.append("")
                out.append("import java.nio.file.Path;")
                out.append("import java.nio.file.Paths;")
                inserted = True
        src = "\n".join(out) + ("\n" if not src.endswith("\n") else "")
    else:
        # Ensure Paths import too
        if "import java.nio.file.Paths;" not in src:
            src = src.replace("import java.nio.file.Path;", "import java.nio.file.Path;\nimport java.nio.file.Paths;")
    return src


def add_resolver_method(src: str, program: str) -> str:
    if MARKER in src:
        return src

    # Insert helper method inside class, near top.
    # Find first occurrence of "public class ... {"
    m = re.search(r"public\s+class\s+\w+\s*\{", src)
    if not m:
        raise RuntimeError("Could not find class declaration to patch.")

    insert_pos = m.end()

    helper = f"""

{MARKER}
    private static Path resolveRepoRoot() {{
        Path cwd = Paths.get(System.getProperty("user.dir")).toAbsolutePath().normalize();
        // If we are inside the Spring Boot module folder, repo root is its parent.
        if (cwd.getFileName() != null && cwd.getFileName().toString().equals("misc-1099")) {{
            return cwd.getParent() != null ? cwd.getParent() : cwd;
        }}
        return cwd;
    }}

    private static Path resolveTestcaseDir(String program) {{
        return resolveRepoRoot()
                .resolve("work")
                .resolve("mainframe_clean")
                .resolve("testcases")
                .resolve(program);
    }}
"""
    return src[:insert_pos] + helper + src[insert_pos:]


def patch_output_paths(src: str, program: str) -> str:
    """
    Make the runner use resolveTestcaseDir(PROGRAM)/output for any output paths.

    We do a conservative patch:
    - Replace any string literal containing "work/mainframe_clean/testcases/<program>" with resolveTestcaseDir("<program>").toString()
    - Replace any Paths.get("work", "mainframe_clean", "testcases", "<program>", "output", "<file>")
      with resolveTestcaseDir("<program>").resolve("output").resolve("<file>")
    """

    # Normalize program usage in Java string matching
    prog = program

    # Patch common Paths.get("work",...,"output","master_out.txt") patterns
    def repl_paths_get(match: re.Match) -> str:
        filename = match.group(1)
        return f'resolveTestcaseDir("{prog}").resolve("output").resolve("{filename}")'

    src = re.sub(
        r'Paths\.get\(\s*"work"\s*,\s*"mainframe_clean"\s*,\s*"testcases"\s*,\s*"' + re.escape(prog) + r'"\s*,\s*"output"\s*,\s*"([^"]+)"\s*\)',
        repl_paths_get,
        src,
    )

    # Patch "work/mainframe_clean/testcases/<prog>/output/<file>" string literals if present
    src = re.sub(
        r'"work/mainframe_clean/testcases/' + re.escape(prog) + r'/output/([^"]+)"',
        lambda m: f'resolveTestcaseDir("{prog}").resolve("output").resolve("{m.group(1)}").toString()',
        src,
    )

    # Patch "work/mainframe_clean/testcases/<prog>" base if present
    src = re.sub(
        r'"work/mainframe_clean/testcases/' + re.escape(prog) + r'"',
        lambda m: f'resolveTestcaseDir("{prog}").toString()',
        src,
    )

    return src


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("program", help="e.g. CCAC6340")
    args = parser.parse_args()

    program = args.program.upper()
    runner_path = find_runner(program)

    src = runner_path.read_text(encoding="utf-8")

    # If already patched, be idempotent
    if MARKER in src:
        print(f"✔ G.3.1 already applied: {runner_path}")
        return

    src2 = ensure_imports(src)
    src2 = add_resolver_method(src2, program)
    src2 = patch_output_paths(src2, program)

    runner_path.write_text(src2, encoding="utf-8")
    print(f"✔ Layer G.3.1 applied: {runner_path}")
    print("\nNext:")
    print("  cd misc-1099 && ./mvnw test && cd ..")
    print(f"  python layer_g2_execute_and_compare.py {program}")


if __name__ == "__main__":
    main()
