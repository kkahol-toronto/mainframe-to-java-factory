#!/usr/bin/env python3
"""
Layer 3H: Java Tasklet Hardener (STRUCTURE REPAIR)

Purpose:
- Fix broken generated Java caused by accidental extra braces or stray text.
- Ensure file contains ONLY:
  - package declaration
  - imports
  - exactly one public class block (brace-matched)

This eliminates:
- "unnamed classes are a preview feature"
- "unnamed class should not have package declaration"
- "class, interface, enum, or record expected"

Safe:
- Deterministic
- Idempotent
- No LLM calls
"""

from __future__ import annotations

from pathlib import Path
import re
import sys


ROOT = Path(".")
TASKLET_DIR = ROOT / "misc-1099/src/main/java/com/fordcredit/misc1099/batch/program"


PUBLIC_CLASS_RE = re.compile(r"\bpublic\s+class\s+([A-Za-z_][A-Za-z0-9_]*)\b")


def harden_java_source(text: str) -> str:
    lines = text.replace("\r\n", "\n").replace("\r", "\n").splitlines()

    # Drop BOM if present
    if lines and lines[0].startswith("\ufeff"):
        lines[0] = lines[0].lstrip("\ufeff")

    # Find package
    pkg_idx = None
    for i, l in enumerate(lines):
        if l.strip().startswith("package "):
            pkg_idx = i
            break
    if pkg_idx is None:
        raise RuntimeError("No package declaration found")

    # Collect package + imports (from pkg to before class)
    package_line = lines[pkg_idx].rstrip()
    import_lines = []
    for l in lines[pkg_idx + 1 :]:
        s = l.strip()
        if s.startswith("import "):
            import_lines.append(l.rstrip())

    # Find first public class
    class_idx = None
    for i, l in enumerate(lines):
        if PUBLIC_CLASS_RE.search(l):
            class_idx = i
            break
    if class_idx is None:
        raise RuntimeError("No public class found")

    # Find first '{' starting the class body
    # Start scanning from class_idx until we hit '{'
    brace_start_line = None
    brace_start_pos = None
    for i in range(class_idx, len(lines)):
        if "{" in lines[i]:
            brace_start_line = i
            brace_start_pos = lines[i].find("{")
            break
    if brace_start_line is None or brace_start_pos is None:
        raise RuntimeError("Public class has no opening brace")

    # Brace-match from that '{' across the whole file
    depth = 0
    end_line = None
    for i in range(brace_start_line, len(lines)):
        for ch in lines[i]:
            if ch == "{":
                depth += 1
            elif ch == "}":
                depth -= 1
                if depth == 0:
                    end_line = i
                    break
        if end_line is not None:
            break

    if end_line is None:
        raise RuntimeError("Could not find matching closing brace for public class")

    class_block = "\n".join(lines[class_idx : end_line + 1]).rstrip() + "\n"

    out = []
    out.append(package_line)
    out.append("")
    out.extend(import_lines)
    out.append("")
    out.append(class_block.rstrip())
    out.append("")  # trailing newline
    return "\n".join(out).strip() + "\n"


def harden_tasklet_file(path: Path) -> None:
    text = path.read_text(encoding="utf-8", errors="ignore")
    fixed = harden_java_source(text)
    path.write_text(fixed, encoding="utf-8")


def main():
    if len(sys.argv) != 2:
        print("Usage: python layer3h_harden_tasklet_java.py <PROGRAM>")
        sys.exit(1)

    program = sys.argv[1].upper()
    tasklet = TASKLET_DIR / f"{program}Tasklet.java"
    if not tasklet.exists():
        raise FileNotFoundError(tasklet)

    harden_tasklet_file(tasklet)
    print(f"✔ Layer 3H hardened: {tasklet}")


if __name__ == "__main__":
    main()
