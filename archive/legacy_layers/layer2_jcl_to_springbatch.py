"""
Layer 2: JCL -> Spring Batch Job/Step Configuration (Option A)

Spring Batch 5 / Spring Boot 3 compatible.

Generates:
- One JobConfig class per JCL
- One StepsConfig class per JCL
- Tasklet placeholders only

Does NOT generate:
- Readers
- Writers
- Processors
- Business logic
"""

from pathlib import Path
import os
import re
from openai import AzureOpenAI
from dotenv import load_dotenv

# ---------------------------------------------------------------------------
# LOAD ENVIRONMENT
# ---------------------------------------------------------------------------

load_dotenv()

AZURE_OPENAI_API_KEY = os.getenv("AZURE_OPENAI_API_KEY")
AZURE_OPENAI_ENDPOINT = os.getenv("AZURE_OPENAI_ENDPOINT")
AZURE_OPENAI_DEPLOYMENT = os.getenv("AZURE_OPENAI_DEPLOYMENT")

if not AZURE_OPENAI_API_KEY:
    raise RuntimeError("AZURE_OPENAI_API_KEY is required")
if not AZURE_OPENAI_ENDPOINT:
    raise RuntimeError("AZURE_OPENAI_ENDPOINT is required")
if not AZURE_OPENAI_DEPLOYMENT:
    raise RuntimeError("AZURE_OPENAI_DEPLOYMENT is required")

client = AzureOpenAI(
    api_key=AZURE_OPENAI_API_KEY,
    api_version="2024-02-15-preview",
    azure_endpoint=AZURE_OPENAI_ENDPOINT,
)

# ---------------------------------------------------------------------------
# PATH CONFIGURATION
# ---------------------------------------------------------------------------

JCL_DIR = Path("work/mainframe_clean/jcl")
BASE_JAVA = Path("misc-1099/src/main/java")
BASE_PACKAGE = "com.fordcredit.misc1099"

JOB_PKG = f"{BASE_PACKAGE}.batch.job"
STEP_PKG = f"{BASE_PACKAGE}.batch.step"

JOB_DIR = BASE_JAVA / Path(*JOB_PKG.split("."))
STEP_DIR = BASE_JAVA / Path(*STEP_PKG.split("."))

JOB_DIR.mkdir(parents=True, exist_ok=True)
STEP_DIR.mkdir(parents=True, exist_ok=True)

JCL_FILES = sorted(JCL_DIR.glob("*.txt"))
if not JCL_FILES:
    raise RuntimeError("No JCL files found")

# ---------------------------------------------------------------------------
# JAVA HARDENING (BRACE-SAFE)
# ---------------------------------------------------------------------------

def strip_markdown_fences(text: str) -> str:
    lines = text.splitlines()
    if lines and lines[0].strip().startswith("```"):
        lines = lines[1:]
    if lines and lines[-1].strip().startswith("```"):
        lines = lines[:-1]
    return "\n".join(lines)


def harden_java_output(text: str) -> str:
    """
    Extract valid Java source:
    - package
    - imports
    - exactly one public class
    Class body is taken from public class declaration to LAST closing brace.
    """

    text = strip_markdown_fences(text)

    lines = []
    for raw in text.splitlines():
        line = raw.rstrip()
        if line.strip().startswith("```"):
            continue
        line = (
            line.replace("`", "")
                .replace("—", "-")
                .replace("–", "-")
                .replace("“", "\"")
                .replace("”", "\"")
        )
        lines.append(line)

    package_line = next((l for l in lines if l.startswith("package ")), None)
    if not package_line:
        return ""

    import_lines = [l for l in lines if l.startswith("import ")]

    class_idx = None
    for i, l in enumerate(lines):
        if re.search(r"public\s+class\s+\w+", l):
            class_idx = i
            break

    if class_idx is None:
        return ""

    start = class_idx
    while start - 1 >= 0 and lines[start - 1].strip().startswith("@"):
        start -= 1

    end = None
    for i in range(len(lines) - 1, start, -1):
        if "}" in lines[i]:
            end = i
            break

    if end is None:
        return ""

    class_block = lines[start:end + 1]

    result = [package_line, ""]
    result.extend(import_lines)
    result.append("")
    result.extend(class_block)

    return "\n".join(result).strip() + "\n"


def extract_public_class_name(java_code: str) -> str | None:
    m = re.search(r"public\s+class\s+([A-Za-z_][A-Za-z0-9_]*)", java_code)
    return m.group(1) if m else None

# ---------------------------------------------------------------------------
# PROMPTS (SPRING BATCH 5 CORRECT)
# ---------------------------------------------------------------------------

def job_prompt(jcl: str) -> str:
    return f"""
Generate EXACTLY ONE Java Spring Batch JobConfig class.

STRICT RULES (MANDATORY):
- Spring Batch 5 / Spring Boot 3 ONLY
- DO NOT use BatchConfiguration
- DO NOT extend any Batch base class
- DO NOT use @EnableBatchProcessing
- Use @Configuration only
- Use JobBuilder + JobRepository
- Java only
- No prose
- No markdown

PACKAGE:
{JOB_PKG}

JCL:
{jcl}
""".strip()


def steps_prompt(jcl: str) -> str:
    return f"""
Generate EXACTLY ONE Java Spring Batch StepsConfig class.

STRICT RULES (MANDATORY):
- Spring Batch 5 / Spring Boot 3 ONLY
- DO NOT use BatchConfiguration
- DO NOT extend any Batch base class
- DO NOT use @EnableBatchProcessing
- Use @Configuration only
- Use StepBuilder + JobRepository + PlatformTransactionManager
- One Step per EXEC
- Tasklet placeholders only
- Java only
- No prose
- No markdown

PACKAGE:
{STEP_PKG}

JCL:
{jcl}
""".strip()


# ---------------------------------------------------------------------------
# GENERATION
# ---------------------------------------------------------------------------

def generate(prompt: str, out_dir: Path, label: str):
    resp = client.chat.completions.create(
        model=AZURE_OPENAI_DEPLOYMENT,
        messages=[{"role": "user", "content": prompt}],
        temperature=0,
    )

    raw = resp.choices[0].message.content
    java = harden_java_output(raw)
    class_name = extract_public_class_name(java)

    if not class_name:
        error_file = out_dir / f"{label}.ERROR.txt"
        error_file.write_text(raw, encoding="utf-8")
        raise RuntimeError(
            f"LLM failed to generate a public class for {label}. "
            f"Raw output saved to {error_file}"
        )

    out = out_dir / f"{class_name}.java"
    out.write_text(java, encoding="utf-8")
    print(f"  → Written {out}")

# ---------------------------------------------------------------------------
# MAIN
# ---------------------------------------------------------------------------

def main():
    print("\n=== Layer 2: JCL -> Spring Batch Config (Option A) ===\n")

    for jcl_file in JCL_FILES:
        print(f"Processing JCL: {jcl_file.name}")
        jcl_text = jcl_file.read_text(encoding="utf-8", errors="ignore")

        generate(
            job_prompt(jcl_text),
            JOB_DIR,
            f"{jcl_file.stem}_JobConfig"
        )

        generate(
            steps_prompt(jcl_text),
            STEP_DIR,
            f"{jcl_file.stem}_StepsConfig"
        )

    print("\nLayer 2 generation COMPLETE.\n")


if __name__ == "__main__":
    main()
