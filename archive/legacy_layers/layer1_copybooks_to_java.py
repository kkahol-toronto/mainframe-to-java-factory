"""
Layer 1: COBOL Copybooks -> Java POJOs

- Reads copybooks one-by-one
- Calls Azure OpenAI deterministically
- Uses python-dotenv for configuration
- Strips markdown fences from LLM output
- Extracts public class name
- Writes Java domain classes into Spring Boot project
"""

from pathlib import Path
import os
import re
from openai import AzureOpenAI
from dotenv import load_dotenv

# ---------------------------------------------------------------------------
# LOAD ENVIRONMENT VARIABLES
# ---------------------------------------------------------------------------

load_dotenv()

api_key = os.getenv("AZURE_OPENAI_API_KEY")
api_endpoint = os.getenv("AZURE_OPENAI_ENDPOINT")
deployment_name = os.getenv("AZURE_OPENAI_DEPLOYMENT")

if not api_key:
    raise ValueError("AZURE_OPENAI_API_KEY environment variable is required.")
if not api_endpoint:
    raise ValueError("AZURE_OPENAI_ENDPOINT environment variable is required.")
if not deployment_name:
    raise ValueError("AZURE_OPENAI_DEPLOYMENT environment variable is required.")

# ---------------------------------------------------------------------------
# CONFIGURATION
# ---------------------------------------------------------------------------

COPYBOOKS = [
    "C2INX001.cpy",
    "C2INX002.cpy",
    "C2INX003.cpy",
    "C2INZ001.cpy",
    "C2INZ002.cpy",
    "C2INZ003.cpy",
    "C2INZ004.cpy",
    "C2INZ005.cpy",
    "CLCWW013.cpy",
]

COPYBOOK_DIR = Path("work/mainframe_clean/copybook")

JAVA_OUT_DIR = Path(
    "misc-1099/src/main/java/com/fordcredit/misc1099/domain/copybook"
)
JAVA_OUT_DIR.mkdir(parents=True, exist_ok=True)

# ---------------------------------------------------------------------------
# AZURE OPENAI CLIENT
# ---------------------------------------------------------------------------

client = AzureOpenAI(
    api_key=api_key,
    api_version="2024-02-15-preview",
    azure_endpoint=api_endpoint,
)

DEPLOYMENT_NAME = deployment_name

# ---------------------------------------------------------------------------
# MARKDOWN SANITIZATION
# ---------------------------------------------------------------------------

def strip_markdown_fences(text: str) -> str:
    """
    Removes ``` or ```java fences from LLM output.
    Ensures valid raw Java source.
    """
    lines = text.strip().splitlines()

    if lines and lines[0].strip().startswith("```"):
        lines = lines[1:]

    if lines and lines[-1].strip().startswith("```"):
        lines = lines[:-1]

    return "\n".join(lines).strip() + "\n"

# ---------------------------------------------------------------------------
# JAVA CLASS NAME EXTRACTION
# ---------------------------------------------------------------------------

def extract_public_class_name(java_code: str) -> str:
    """
    Extracts the public class name from Java source code.
    """
    match = re.search(
        r"public\s+class\s+([A-Za-z_][A-Za-z0-9_]*)",
        java_code
    )
    if not match:
        raise ValueError("No public class declaration found in Java output.")
    return match.group(1)

# ---------------------------------------------------------------------------
# PROMPT CONSTRUCTION (Layer 1 Contract)
# ---------------------------------------------------------------------------

def build_copybook_prompt(copybook_text: str) -> str:
    return f"""
You are a legacy systems migration engine.

You convert COBOL copybooks into Java data models with byte-level fidelity.
You do NOT modernize, simplify, or infer business meaning.
Correctness is more important than elegance.

Convert the following COBOL copybook into a Java data model.

TARGET ENVIRONMENT
- Java 21
- Spring Boot 3.5.x
- No Lombok
- No frameworks or annotations
- Pure Java domain model

STRICT RULES
1. Preserve field order exactly
2. Preserve COBOL hierarchy (01 / 05 / 10 levels → nested classes)
3. PIC X(...) → String
4. PIC 9 / S9 / V → BigDecimal with correct scale
5. OCCURS → List<T> or fixed-size array (document size in comments)
6. REDEFINES must be modeled explicitly and documented
7. No validation, defaults, or business logic
8. Do NOT rename fields except to make valid Java identifiers
9. Output ONLY Java source code
10. One public top-level class per copybook
11. Do NOT wrap output in markdown fences

PACKAGE
com.fordcredit.misc1099.domain.copybook

COBOL COPYBOOK
--------------------------------------------------
{copybook_text}
--------------------------------------------------
""".strip()

# ---------------------------------------------------------------------------
# AZURE OPENAI CALL
# ---------------------------------------------------------------------------

def convert_copybook_to_java(copybook_text: str) -> str:
    response = client.chat.completions.create(
        model=DEPLOYMENT_NAME,
        messages=[
            {
                "role": "user",
                "content": build_copybook_prompt(copybook_text),
            }
        ],
        temperature=0,
    )

    raw_output = response.choices[0].message.content
    return strip_markdown_fences(raw_output)

# ---------------------------------------------------------------------------
# MAIN EXECUTION
# ---------------------------------------------------------------------------

def main():
    print("\n=== Layer 1: Copybooks → Java POJOs ===\n")

    for copybook_name in COPYBOOKS:
        copybook_path = COPYBOOK_DIR / copybook_name

        if not copybook_path.exists():
            raise FileNotFoundError(f"Missing copybook: {copybook_path}")

        print(f"Processing {copybook_name} ...")

        cobol_text = copybook_path.read_text(
            encoding="utf-8", errors="ignore"
        )

        java_code = convert_copybook_to_java(cobol_text)
        class_name = extract_public_class_name(java_code)

        java_out_path = JAVA_OUT_DIR / f"{class_name}.java"
        java_out_path.write_text(java_code, encoding="utf-8")

        print(f"  → Written {java_out_path}")

    print("\nLayer 1 COMPLETE — all copybooks converted cleanly.\n")

if __name__ == "__main__":
    main()
