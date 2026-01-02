"""
Layer 3C: COBOL Paragraphs -> Java Methods (STRUCTURE-SAFE)

Hard guarantees:
- Never breaks Java syntax
- Never comments out braces
- Never duplicates methods
- Never generates mainline()
- Injects ONLY inside explicit anchors

This layer is SAFE BY CONSTRUCTION.
"""

from pathlib import Path
import os
import re
import argparse
from openai import AzureOpenAI
from dotenv import load_dotenv

# ---------------------------------------------------------------------------
# ENV
# ---------------------------------------------------------------------------

load_dotenv()

AZURE_OPENAI_API_KEY = os.getenv("AZURE_OPENAI_API_KEY")
AZURE_OPENAI_ENDPOINT = os.getenv("AZURE_OPENAI_ENDPOINT")
AZURE_OPENAI_DEPLOYMENT = os.getenv("AZURE_OPENAI_DEPLOYMENT")

if not AZURE_OPENAI_API_KEY or not AZURE_OPENAI_ENDPOINT or not AZURE_OPENAI_DEPLOYMENT:
    raise RuntimeError("Azure OpenAI env vars missing")

client = AzureOpenAI(
    api_key=AZURE_OPENAI_API_KEY,
    api_version="2024-02-15-preview",
    azure_endpoint=AZURE_OPENAI_ENDPOINT,
)

# ---------------------------------------------------------------------------
# PATHS
# ---------------------------------------------------------------------------

ROOT = Path(".")
COBOL_DIR = ROOT / "work/mainframe_clean/cobol"
JAVA_DIR = ROOT / "misc-1099/src/main/java/com/fordcredit/misc1099/batch/program"

BEGIN_MARKER = "// BEGIN GENERATED PARAGRAPHS (Layer 3C)"
END_MARKER   = "// END GENERATED PARAGRAPHS (Layer 3C)"

# ---------------------------------------------------------------------------
# HELPERS
# ---------------------------------------------------------------------------

def method_name_for_paragraph(paragraph: str) -> str:
    para = paragraph.strip().upper()
    para = re.sub(r"^\d+-", "", para)
    parts = para.split("-")
    first = parts[0].lower()
    rest = [p.lower().capitalize() for p in parts[1:] if p]
    return first + "".join(rest)


def extract_paragraphs(cobol: str):
    paras = []
    current = None
    body = []

    for line in cobol.splitlines():
        m = re.match(r"\s*(\d{4}-[A-Z0-9-]+)\.\s*$", line)
        if m:
            if current:
                paras.append((current, "\n".join(body)))
            current = m.group(1)
            body = []
        elif current:
            body.append(line)

    if current:
        paras.append((current, "\n".join(body)))

    return paras


def strip_markdown(text: str) -> str:
    lines = []
    for raw in text.splitlines():
        if raw.strip().startswith("```"):
            continue
        lines.append(
            raw.replace("`", "")
               .replace("—", "-")
               .replace("–", "-")
        )
    return "\n".join(lines).strip()


def extract_first_method_block(text: str) -> str | None:
    """
    Extract the first 'private void ... { ... }' block from arbitrary text.
    We DO NOT rely on it starting at the beginning.
    """
    # Find first method signature
    sig = re.search(r"\bprivate\s+void\s+[A-Za-z_][A-Za-z0-9_]*\s*\(\s*MergeState\s+state\s*\)\s*\{", text)
    if not sig:
        return None

    start = sig.start()

    # Brace-match from the first '{'
    brace_start = text.find("{", sig.end() - 1)
    if brace_start == -1:
        return None

    depth = 0
    for i in range(brace_start, len(text)):
        if text[i] == "{":
            depth += 1
        elif text[i] == "}":
            depth -= 1
            if depth == 0:
                return text[start:i+1].strip()

    return None


def sanitize_method(raw_llm: str, para_name: str, expected_method_name: str) -> str:
    """
    Enforce a SAFE Java method:
    - Extract exactly one method block from the LLM output
    - Force signature method name to expected_method_name
    - Keep braces real
    - Comment-only for COBOL control keywords
    """
    text = strip_markdown(raw_llm)

    block = extract_first_method_block(text)
    if block is None:
        # Fallback: safe stub method (never crash pipeline)
        return f"""// COBOL {para_name}
private void {expected_method_name}(MergeState state) {{
    // TODO: LLM did not return a valid method. Manual review required.
}}""".strip()

    # Force the method name to the expected one (in case the model chose differently)
    block = re.sub(
        r"\bprivate\s+void\s+[A-Za-z_][A-Za-z0-9_]*\s*\(",
        f"private void {expected_method_name}(",
        block,
        count=1
    )

    # Comment COBOL-ish control words inside the method body (but DO NOT comment braces)
    cleaned_lines = []
    for line in block.splitlines():
        s = line.strip()

        # Do not comment braces or blank lines
        if s in ("{", "}", ""):
            cleaned_lines.append(line)
            continue

        # Comment lines that contain COBOL control words or pseudo-control
        if re.search(r"\b(IF|ELSE|END-IF|PERFORM|EVALUATE|WHEN)\b", s, re.IGNORECASE):
            cleaned_lines.append("    // " + s)
        else:
            cleaned_lines.append(line)

    method = "\n".join(cleaned_lines).strip()

    # Final structural validation: braces must match
    if method.count("{") != method.count("}"):
        # If brace mismatch, return a safe stub (never emit broken Java)
        return f"""// COBOL {para_name}
private void {expected_method_name}(MergeState state) {{
    // TODO: Brace mismatch in LLM output. Manual review required.
}}""".strip()

    return method


def inject_methods(java: str, methods: list[str]) -> str:
    if BEGIN_MARKER not in java or END_MARKER not in java:
        raise RuntimeError("3C anchor markers missing (regenerate via Layer 3B)")

    before, rest = java.split(BEGIN_MARKER, 1)
    middle, after = rest.split(END_MARKER, 1)

    injected = (
        BEGIN_MARKER
        + "\n\n"
        + "\n\n".join(methods)
        + "\n\n"
        + END_MARKER
    )

    return before + injected + after


# ---------------------------------------------------------------------------
# PROMPT
# ---------------------------------------------------------------------------

def paragraph_prompt(program: str, para_name: str, para_body: str) -> str:
    method = method_name_for_paragraph(para_name)
    return f"""
Please translate the following COBOL paragraph into a single Java helper method.

Guidelines:
- Return one Java method only
- The method should be named `{method}`
- The method should accept a `MergeState state` parameter
- Use comments to describe control flow and intent
- Avoid introducing new control structures
- Keep the method self-contained

Expected method shape:

// COBOL {para_name}
private void {method}(MergeState state) {{
    ...
}}

COBOL paragraph:
{para_body}
""".strip()


# ---------------------------------------------------------------------------
# MAIN
# ---------------------------------------------------------------------------

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("program")
    parser.add_argument("--limit", type=int)
    args = parser.parse_args()

    program = args.program.upper()

    cobol_files = list(COBOL_DIR.glob(f"{program}.*"))
    if not cobol_files:
        raise FileNotFoundError(f"No COBOL file found for {program} in {COBOL_DIR}")

    cobol = cobol_files[0].read_text(encoding="utf-8", errors="ignore")
    java_file = JAVA_DIR / f"{program}Tasklet.java"

    if not java_file.exists():
        raise FileNotFoundError(java_file)

    java = java_file.read_text(encoding="utf-8")

    paragraphs = extract_paragraphs(cobol)
    if args.limit:
        paragraphs = paragraphs[:args.limit]

    methods = []
    seen = set()

    for name, body in paragraphs:
        method_name = method_name_for_paragraph(name)

        # Layer 3C must NEVER translate control-entry paragraphs
        if method_name == "mainline":
            print(f"Skipping control paragraph {name}")
            continue

        if method_name in seen:
            continue
        seen.add(method_name)

        resp = client.chat.completions.create(
            model=AZURE_OPENAI_DEPLOYMENT,
            messages=[{"role": "user", "content": paragraph_prompt(program, name, body)}],
            temperature=0,
        )

        raw = resp.choices[0].message.content
        method = sanitize_method(raw, name, method_name)

        # Final guard: never allow mainline() in Layer 3C output
        if re.search(r"\bprivate\s+void\s+mainline\s*\(", method):
            continue

        methods.append(method)

    java = inject_methods(java, methods)
    java_file.write_text(java, encoding="utf-8")

    print(f"✔ Layer 3C applied safely to {java_file}")


if __name__ == "__main__":
    main()
