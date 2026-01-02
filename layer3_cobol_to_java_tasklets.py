"""
Layer 3 (factory-grade):
A) COBOL -> Program IR (JSON)
B) Program IR -> Java Tasklet skeleton (STRUCTURE-OWNED)

Key rule:
- Layer 3B OWNS Java structure
- LLM may ONLY influence comments / metadata
"""

from __future__ import annotations

from pathlib import Path
import json
import os
from typing import Any, Dict, List, Optional

from dotenv import load_dotenv
from openai import AzureOpenAI

# ---------------------------------------------------------------------
# ENV / CLIENT
# ---------------------------------------------------------------------

load_dotenv()

AZURE_OPENAI_API_KEY = os.getenv("AZURE_OPENAI_API_KEY")
AZURE_OPENAI_ENDPOINT = os.getenv("AZURE_OPENAI_ENDPOINT")
AZURE_OPENAI_DEPLOYMENT = os.getenv("AZURE_OPENAI_DEPLOYMENT")

if not AZURE_OPENAI_API_KEY or not AZURE_OPENAI_ENDPOINT or not AZURE_OPENAI_DEPLOYMENT:
    raise RuntimeError("Missing Azure OpenAI env vars")

client = AzureOpenAI(
    api_key=AZURE_OPENAI_API_KEY,
    api_version="2024-02-15-preview",
    azure_endpoint=AZURE_OPENAI_ENDPOINT,
)

MODEL = AZURE_OPENAI_DEPLOYMENT

# ---------------------------------------------------------------------
# PATHS
# ---------------------------------------------------------------------

COBOL_DIR = Path("work/mainframe_clean/cobol")
COBOL_FILES: Optional[List[Path]] = None

IR_OUT_DIR = Path("work/migration_ir/cobol")
IR_OUT_DIR.mkdir(parents=True, exist_ok=True)

JAVA_BASE = Path("misc-1099/src/main/java")
BASE_PACKAGE = "com.fordcredit.misc1099"
TASKLET_PKG = f"{BASE_PACKAGE}.batch.program"

TASKLET_DIR = JAVA_BASE / Path(*TASKLET_PKG.split("."))
TASKLET_DIR.mkdir(parents=True, exist_ok=True)

# ---------------------------------------------------------------------
# HELPERS
# ---------------------------------------------------------------------

def list_cobol_inputs() -> List[Path]:
    if COBOL_FILES:
        return COBOL_FILES
    if not COBOL_DIR.exists():
        raise FileNotFoundError(f"COBOL_DIR not found: {COBOL_DIR}")
    return (
        sorted(COBOL_DIR.glob("*.cbl")) +
        sorted(COBOL_DIR.glob("*.cob")) +
        sorted(COBOL_DIR.glob("*.txt"))
    )


def normalize_text(s: str) -> str:
    return s.replace("\r\n", "\n").replace("\r", "\n")


# ---------------------------------------------------------------------
# PROMPTS (IR ONLY)
# ---------------------------------------------------------------------

def build_ir_prompt(cobol_text: str) -> str:
    return f"""
You are a COBOL batch program analyzer.

Return ONLY valid JSON (no markdown) describing the program in a stable intermediate representation ("Program IR").

REQUIREMENTS
- Output JSON only
- Must include all keys listed in SCHEMA
- If unknown, use null or empty arrays

SCHEMA
{{
  "programId": "string",
  "purpose": "string",
  "files": [],
  "copybooks": [],
  "workingStorage": {{
    "flags": [],
    "counters": [],
    "accumulators": [],
    "keyFields": []
  }},
  "controlFlow": {{
    "mainEntry": null,
    "termination": null,
    "paragraphs": [],
    "errorExits": []
  }},
  "patterns": {{
    "isTwoWayMerge": false,
    "isGroupSummarize": false,
    "isMasterUpdateMerge": false,
    "isFormatterHeadersTrailers": false,
    "isLookupEnrichment": false,
    "notes": []
  }}
}}

COBOL INPUT
--------------------------------------------------
{cobol_text}
--------------------------------------------------
""".strip()


def llm_json(prompt: str) -> Dict[str, Any]:
    resp = client.chat.completions.create(
        model=MODEL,
        messages=[{"role": "user", "content": prompt}],
        temperature=0,
    )
    return json.loads(resp.choices[0].message.content)


# ---------------------------------------------------------------------
# STRUCTURE-OWNED TASKLET TEMPLATE (NO LLM)
# ---------------------------------------------------------------------

def render_tasklet_skeleton(program_id: str, ir: Dict[str, Any]) -> str:
    """
    AUTHORITATIVE Java structure.
    LLMs do NOT control Java syntax here.
    """
    pattern_notes = ir.get("patterns", {}).get("notes", [])
    pattern_text = ", ".join(pattern_notes) if pattern_notes else "None detected"

    return f"""\
package {TASKLET_PKG};

import org.springframework.batch.core.StepContribution;
import org.springframework.batch.core.scope.context.ChunkContext;
import org.springframework.batch.core.step.tasklet.Tasklet;
import org.springframework.batch.repeat.RepeatStatus;

/**
 * Auto-generated Tasklet skeleton for COBOL program {program_id}.
 *
 * Patterns detected:
 *   {pattern_text}
 */
public class {program_id}Tasklet implements Tasklet {{

    /**
     * Program state holder.
     * Expanded in Layer 3E.
     */
    static class MergeState {{
        // TODO: flags, counters, cursors added later
    }}

    @Override
    public RepeatStatus execute(
            StepContribution contribution,
            ChunkContext chunkContext) throws Exception {{

        MergeState state = new MergeState();
        mainline(state);
        return RepeatStatus.FINISHED;
    }}

    /**
     * COBOL main entry point.
     * Control flow is normalized in Layer 3D.
     */
    private void mainline(MergeState state) {{
        // Implemented by Layer 3D
    }}

    // ======================================================
    // BEGIN GENERATED PARAGRAPHS (Layer 3C)
    // ======================================================

    // ======================================================
    // END GENERATED PARAGRAPHS (Layer 3C)
    // ======================================================
}}
"""


# ---------------------------------------------------------------------
# MAIN
# ---------------------------------------------------------------------

def main():
    cobol_paths = list_cobol_inputs()
    print("\n=== Layer 3A/3B: COBOL -> IR -> Tasklet skeleton ===\n")
    print(f"Found {len(cobol_paths)} COBOL files.")

    for cobol_path in cobol_paths:
        cobol_text = normalize_text(
            cobol_path.read_text(encoding="utf-8", errors="ignore")
        )

        # --- Layer 3A: IR ---
        ir = llm_json(build_ir_prompt(cobol_text))
        program_id = ir.get("programId") or cobol_path.stem

        ir_path = IR_OUT_DIR / f"{program_id}.json"
        ir_path.write_text(json.dumps(ir, indent=2), encoding="utf-8")
        print(f"IR written: {ir_path}")

        # --- Layer 3B: STRUCTURE-OWNED Tasklet ---
        java_code = render_tasklet_skeleton(program_id, ir)

        out_java = TASKLET_DIR / f"{program_id}Tasklet.java"
        out_java.write_text(java_code, encoding="utf-8")
        print(f"Tasklet skeleton written: {out_java}")

    print("\nLayer 3A/3B complete (IR + stable Tasklet skeletons).\n")


if __name__ == "__main__":
    main()
