"""
Layer 1.5: COBOL Copybooks -> FieldSpec Lists (Pure Layout Metadata)

This layer generates ONLY fixed-width layout metadata.

Output per copybook:
- One Java file
- Exactly ONE public class
- public static List<FieldSpec> fields()
- NO generics
- NO record types
- NO setters
"""

from pathlib import Path
import os
import re
from openai import AzureOpenAI
from dotenv import load_dotenv

# ---------------------------------------------------------------------------
# LOAD ENV
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
# PATHS
# ---------------------------------------------------------------------------

COPYBOOK_DIR = Path("work/mainframe_clean/copybook")

JAVA_OUT_DIR = Path(
    "misc-1099/src/main/java/com/fordcredit/misc1099/parser"
)
JAVA_OUT_DIR.mkdir(parents=True, exist_ok=True)

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

# ---------------------------------------------------------------------------
# SANITIZATION
# ---------------------------------------------------------------------------

def sanitize_java(text: str) -> str:
    """
    Aggressively sanitize LLM output to valid Java ONLY.
    """
    allowed_prefixes = (
        "package ",
        "import ",
        "public class ",
        "public static ",
        "return ",
        "List.of",
        "FieldSpec.",
        "{",
        "}",
        ")",
        "(",
    )

    clean = []

    for raw in text.splitlines():
        line = raw.strip()

        # Drop markdown fences
        if line.startswith("```"):
            continue

        # Normalize punctuation
        line = (
            line.replace("`", "")
                .replace("—", "-")
                .replace("–", "-")
                .replace("“", "\"")
                .replace("”", "\"")
        )

        if not line:
            clean.append("")
            continue

        if line.startswith(allowed_prefixes):
            clean.append(line)
            continue

        if line.endswith(",") or line.endswith(");"):
            clean.append(line)
            continue

        # Everything else is dropped

    return "\n".join(clean).strip() + "\n"

# ---------------------------------------------------------------------------
# SINGLE PUBLIC CLASS EXTRACTION
# ---------------------------------------------------------------------------

def extract_single_public_class(java_code: str) -> str:
    lines = java_code.splitlines()

    start = None
    depth = 0
    end = None

    for i, line in enumerate(lines):
        if start is None and line.startswith("public class"):
            start = i
            depth = 0

        if start is not None:
            depth += line.count("{")
            depth -= line.count("}")

            if depth == 0:
                end = i
                break

    if start is None or end is None:
        raise ValueError("Could not isolate a public class")

    header = [
        l for l in lines
        if l.startswith("package ") or l.startswith("import ")
    ]

    body = lines[start:end + 1]
    return "\n".join(header + [""] + body).strip() + "\n"

# ---------------------------------------------------------------------------
# CLASS NAME EXTRACTION
# ---------------------------------------------------------------------------

def extract_class_name(java_code: str) -> str:
    m = re.search(r"public class ([A-Za-z_][A-Za-z0-9_]*)", java_code)
    if not m:
        raise ValueError("No public class found")
    return m.group(1)

# ---------------------------------------------------------------------------
# PROMPT (FINAL CONTRACT)
# ---------------------------------------------------------------------------

def build_prompt(copybook_text: str) -> str:
    return f"""
You are a legacy data layout compiler.

Generate Java fixed-width field layout metadata from the COBOL copybook below.

STRICT RULES
- Generate ONLY layout metadata
- NO generics
- NO record types
- NO setters
- NO builders
- EXACTLY one public class
- Output MUST compile as-is

TARGET SHAPE

public class <RecordName>FieldSpecs {{

    public static List<FieldSpec> fields() {{
        return List.of(
            FieldSpec.string("fieldName", start, length),
            FieldSpec.decimal("amount", start, length, scale)
        );
    }}
}}

MAPPING
- PIC X -> FieldSpec.string
- PIC 9 / S9 / V -> FieldSpec.decimal
- Preserve order and offsets
- Ignore OCCURS
- Ignore REDEFINES

PACKAGE
com.fordcredit.misc1099.parser

IMPORTS
java.util.List
com.fordcredit.misc1099.util.fixedwidth.FieldSpec

COBOL COPYBOOK
--------------------------------------------------
{copybook_text}
--------------------------------------------------
""".strip()

# ---------------------------------------------------------------------------
# MAIN
# ---------------------------------------------------------------------------

def main():
    print("\n=== Layer 1.5: Copybooks → FieldSpecs ===\n")

    for name in COPYBOOKS:
        path = COPYBOOK_DIR / name
        if not path.exists():
            raise FileNotFoundError(path)

        print(f"Processing {name}...")

        cobol = path.read_text(encoding="utf-8", errors="ignore")

        response = client.chat.completions.create(
            model=AZURE_OPENAI_DEPLOYMENT,
            messages=[{"role": "user", "content": build_prompt(cobol)}],
            temperature=0,
        )

        java = sanitize_java(response.choices[0].message.content)
        java = extract_single_public_class(java)

        class_name = extract_class_name(java)
        out = JAVA_OUT_DIR / f"{class_name}.java"
        out.write_text(java, encoding="utf-8")

        print(f"  → Written {out}")

    print("\nLayer 1.5 COMPLETE — FieldSpecs generated.\n")

if __name__ == "__main__":
    main()
