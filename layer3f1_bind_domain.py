"""
Layer 3F.1 (FULLY AUTOMATIC):
Bind raw lines -> domain objects using auto-discovered POJO + FieldSpecs.

No manual JSON editing.
No LLM calls.

How it works:
- Reads Program IR (Layer 3A) from work/migration_ir/cobol/<PROGRAM>.json
- Scans generated POJOs (Layer 1) under: domain/copybook
- Scans FieldSpecs (Layer 1.5) under: parser/*FieldSpecs.java
- Uses deterministic heuristics to map file roles -> POJO -> FieldSpecs
- Injects only inside Layer 3F anchors:
  - DOMAIN STATE inside MergeState
  - DOMAIN BINDING methods inside class

Safe, idempotent, anchor-based.
"""

from __future__ import annotations

from pathlib import Path
import json
import re
import sys
from typing import Dict, List, Optional, Tuple

ROOT = Path(".")

JAVA_BASE = ROOT / "misc-1099/src/main/java"
PKG_BASE = "com.fordcredit.misc1099"

PROGRAM_DIR = JAVA_BASE / "com/fordcredit/misc1099/batch/program"
PARSER_DIR = JAVA_BASE / "com/fordcredit/misc1099/parser"
COPYBOOK_DIR = JAVA_BASE / "com/fordcredit/misc1099/domain/copybook"

IR_DIR = ROOT / "work/migration_ir/cobol"
BIND_DIR = ROOT / "work/migration_ir/bindings"
BIND_DIR.mkdir(parents=True, exist_ok=True)

BINDER_PKG_DIR = JAVA_BASE / "com/fordcredit/misc1099/batch/bind"
BINDER_PKG_DIR.mkdir(parents=True, exist_ok=True)
BINDER_JAVA_PATH = BINDER_PKG_DIR / "ReflectionDomainBinder.java"

# ----- Layer 3F anchors (from Layer 3F) -----
STATE_BEGIN = "        // BEGIN DOMAIN STATE (Layer 3F)"
STATE_END   = "        // END DOMAIN STATE (Layer 3F)"

BIND_BEGIN  = "    // BEGIN DOMAIN BINDING (Layer 3F)"
BIND_END    = "    // END DOMAIN BINDING (Layer 3F)"

# ----- Reflection binder (compile-safe) -----
BINDER_JAVA = f"""\
package {PKG_BASE}.batch.bind;

import java.lang.reflect.Method;

/**
 * Reflection-based binder (Layer 3F.1).
 *
 * This keeps compilation green while the runtime parser API stabilizes.
 * Layer 3F.2 can replace this with direct calls once the parser API is finalized.
 *
 * Contract:
 * - fieldSpecsClassName must have: public static java.util.List fields()
 * - pojoClassName must have a public no-arg constructor
 */
public final class ReflectionDomainBinder {{

    private ReflectionDomainBinder() {{}}

    public static Object bind(String rawLine, String pojoClassName, String fieldSpecsClassName) {{
        if (rawLine == null) return null;
        try {{
            Class<?> pojoClass = Class.forName(pojoClassName);
            Object pojo = pojoClass.getDeclaredConstructor().newInstance();

            Class<?> specsClass = Class.forName(fieldSpecsClassName);
            Method fieldsMethod = specsClass.getMethod("fields");
            Object specsObj = fieldsMethod.invoke(null);

            // Try known runtime binder candidates (optional)
            String[] binderCandidates = new String[] {{
                "{PKG_BASE}.util.fixedwidth.RuntimeFixedWidthBinder",
                "{PKG_BASE}.util.fixedwidth.FixedWidthBinder",
                "{PKG_BASE}.util.fixedwidth.FixedWidthRuntimeParser"
            }};

            for (String candidate : binderCandidates) {{
                try {{
                    Class<?> binder = Class.forName(candidate);

                    // bind(Object pojo, String line, List specs)
                    try {{
                        Method m = binder.getMethod("bind", Object.class, String.class, java.util.List.class);
                        m.invoke(null, pojo, rawLine, (java.util.List<?>) specsObj);
                        return pojo;
                    }} catch (NoSuchMethodException ignore) {{}}

                    // parse(String line, List specs, Class pojoClass)
                    try {{
                        Method m = binder.getMethod("parse", String.class, java.util.List.class, Class.class);
                        Object out = m.invoke(null, rawLine, (java.util.List<?>) specsObj, pojoClass);
                        if (out != null) return out;
                    }} catch (NoSuchMethodException ignore) {{}}

                }} catch (ClassNotFoundException ignore) {{
                    // try next
                }}
            }}

            // Fallback: return constructed POJO even if not populated yet
            return pojo;

        }} catch (Exception e) {{
            throw new RuntimeException("ReflectionDomainBinder.bind failed", e);
        }}
    }}
}}
"""

# -------------------------------
# Repo scanners
# -------------------------------

def list_java_classes_under(dir_path: Path) -> List[str]:
    """
    Return Java class names (file stems) for *.java files in a directory.
    """
    if not dir_path.exists():
        return []
    return sorted([p.stem for p in dir_path.glob("*.java") if p.is_file()])


def to_fqcn(package: str, class_name: str) -> str:
    return f"{package}.{class_name}"


def load_ir(program: str) -> dict:
    p = IR_DIR / f"{program}.json"
    if not p.exists():
        raise FileNotFoundError(f"Missing IR file: {p}. Run Layer 3A/3B first.")
    return json.loads(p.read_text(encoding="utf-8"))


def tokenize(s: str) -> List[str]:
    s = re.sub(r"[^A-Za-z0-9]+", " ", s).strip().lower()
    toks = [t for t in s.split() if t]
    return toks


def score_by_tokens(candidate: str, tokens: List[str]) -> int:
    """
    Score a candidate class name by token overlap.
    """
    c = candidate.lower()
    score = 0
    for t in tokens:
        if t in c:
            score += 1
    return score


# -------------------------------
# Binding resolution heuristics
# -------------------------------

def guess_roles_from_ir(ir: dict) -> Tuple[List[str], List[str]]:
    """
    Return tokens describing likely master/corporate inputs.
    Generic heuristic based on IR file metadata.
    """
    files = ir.get("files") or []
    # Build token bags for "master-like" and "corp-like"
    master_tokens: List[str] = []
    corp_tokens: List[str] = []

    for f in files:
        name = " ".join([
            str(f.get("selectName") or ""),
            str(f.get("fdName") or ""),
            str(f.get("recordPic") or ""),
        ]).strip()
        toks = tokenize(name)

        # Heuristic: if it contains "master" treat as master
        if any(t in toks for t in ["master", "m01", "mast"]):
            master_tokens.extend(toks)

        # Heuristic: if it contains "corp" treat as corporate
        if any(t in toks for t in ["corp", "corporate", "t01"]):
            corp_tokens.extend(toks)

    # Fallback if IR has no file tokens:
    if not master_tokens:
        master_tokens = ["master", "m01"]
    if not corp_tokens:
        corp_tokens = ["corporate", "corp", "t01"]

    return master_tokens, corp_tokens


def resolve_pojo_and_fields(program: str, ir: dict,
                            pojo_classes: List[str],
                            field_specs_classes: List[str]) -> dict:
    """
    Resolve best POJO + FieldSpecs for master and corporate.

    Strategy:
    - Prefer POJO classes mentioned in IR.copybooks if present
    - Otherwise token-score against master/corp tokens
    - Then map to FieldSpecs by prefix match: <Pojo>FieldSpecs
    """

    # 1) candidates from IR.copybooks (best signal if present)
    copybooks = ir.get("copybooks") or []
    copybooks = [Path(c).stem for c in copybooks]  # normalize

    pojo_set = set(pojo_classes)
    specs_set = set(field_specs_classes)

    def best_from_copybooks() -> List[str]:
        return [c for c in copybooks if c in pojo_set]

    from_copybooks = best_from_copybooks()

    master_tokens, corp_tokens = guess_roles_from_ir(ir)

    def pick_best(cands: List[str], tokens: List[str]) -> Optional[str]:
        if not cands:
            return None
        scored = [(score_by_tokens(c, tokens), c) for c in cands]
        scored.sort(key=lambda x: (-x[0], x[1]))
        return scored[0][1]

    # Choose master/corp POJO
    if len(from_copybooks) >= 2:
        master_pojo = pick_best(from_copybooks, master_tokens) or from_copybooks[0]
        remaining = [c for c in from_copybooks if c != master_pojo]
        corporate_pojo = pick_best(remaining, corp_tokens) or (remaining[0] if remaining else master_pojo)
    elif len(from_copybooks) == 1:
        # If only one copybook known, use token scoring over all POJOs for the other
        master_pojo = pick_best(from_copybooks, master_tokens) or from_copybooks[0]
        other_pool = [c for c in pojo_classes if c != master_pojo]
        corporate_pojo = pick_best(other_pool, corp_tokens) or (other_pool[0] if other_pool else master_pojo)
    else:
        # No copybooks detected: score all POJOs
        master_pojo = pick_best(pojo_classes, master_tokens) or (pojo_classes[0] if pojo_classes else None)
        corporate_pojo = pick_best([c for c in pojo_classes if c != master_pojo], corp_tokens) or (pojo_classes[1] if len(pojo_classes) > 1 else master_pojo)

    if not master_pojo or not corporate_pojo:
        raise RuntimeError(f"Could not resolve POJO classes for program {program}. Ensure Layer 1 output exists.")

    # Map POJO -> FieldSpecs
    def fields_for(pojo: str) -> Optional[str]:
        expected = f"{pojo}FieldSpecs"
        if expected in specs_set:
            return expected
        # fallback: best prefix match
        matches = [s for s in field_specs_classes if s.lower().startswith(pojo.lower())]
        return matches[0] if matches else None

    master_fields = fields_for(master_pojo)
    corporate_fields = fields_for(corporate_pojo)

    if not master_fields or not corporate_fields:
        raise RuntimeError(
            f"Could not resolve FieldSpecs for program {program}. "
            f"master={master_pojo} -> {master_fields}, corp={corporate_pojo} -> {corporate_fields}. "
            f"Ensure Layer 1.5 output exists."
        )

    return {
        "program": program,
        "masterPojoClass": to_fqcn(f"{PKG_BASE}.domain.copybook", master_pojo),
        "masterFieldSpecsClass": to_fqcn(f"{PKG_BASE}.parser", master_fields),
        "corporatePojoClass": to_fqcn(f"{PKG_BASE}.domain.copybook", corporate_pojo),
        "corporateFieldSpecsClass": to_fqcn(f"{PKG_BASE}.parser", corporate_fields),
        "resolution": {
            "masterPojo": master_pojo,
            "masterFieldSpecs": master_fields,
            "corporatePojo": corporate_pojo,
            "corporateFieldSpecs": corporate_fields,
            "masterTokens": master_tokens,
            "corporateTokens": corp_tokens,
            "copybooksInIR": copybooks,
        }
    }


# -------------------------------
# Anchor patching
# -------------------------------

def replace_between(text: str, begin: str, end: str, new_block: str) -> str:
    if begin not in text or end not in text:
        raise RuntimeError(f"Missing anchors: {begin} / {end}")
    pre, rest = text.split(begin, 1)
    _, post = rest.split(end, 1)
    return pre + begin + "\n" + new_block.rstrip() + "\n" + end + post


def ensure_binder_class() -> None:
    if not BINDER_JAVA_PATH.exists():
        BINDER_JAVA_PATH.write_text(BINDER_JAVA, encoding="utf-8")


def state_block(program: str, m: dict) -> str:
    return f"""\
        // BEGIN DOMAIN STATE (Layer 3F)

        // Auto-discovered bindings (Layer 3F.1)
        String masterPojoClass = "{m["masterPojoClass"]}";
        String masterFieldSpecsClass = "{m["masterFieldSpecsClass"]}";
        String corporatePojoClass = "{m["corporatePojoClass"]}";
        String corporateFieldSpecsClass = "{m["corporateFieldSpecsClass"]}";

        // Raw lines (Layer 3E.1 will actually set these)
        String masterRawLine;
        String corporateRawLine;

        // Bound records (typed later in Layer 3F.2)
        Object masterRecord;
        Object corporateRecord;

        // END DOMAIN STATE (Layer 3F)
"""


def binding_block() -> str:
    return f"""\
    // BEGIN DOMAIN BINDING (Layer 3F)

    private void bindMasterRecord(MergeState state) {{
        state.masterRecord = {PKG_BASE}.batch.bind.ReflectionDomainBinder.bind(
                state.masterRawLine,
                state.masterPojoClass,
                state.masterFieldSpecsClass
        );
    }}

    private void bindCorporateRecord(MergeState state) {{
        state.corporateRecord = {PKG_BASE}.batch.bind.ReflectionDomainBinder.bind(
                state.corporateRawLine,
                state.corporatePojoClass,
                state.corporateFieldSpecsClass
        );
    }}

    // END DOMAIN BINDING (Layer 3F)
"""


def apply_to_program(program: str) -> None:
    ir = load_ir(program)

    pojos = list_java_classes_under(COPYBOOK_DIR)
    specs = [c for c in list_java_classes_under(PARSER_DIR) if c.endswith("FieldSpecs")]

    if not pojos:
        raise RuntimeError("No POJOs found. Run Layer 1 first.")
    if not specs:
        raise RuntimeError("No FieldSpecs found. Run Layer 1.5 first.")

    m = resolve_pojo_and_fields(program, ir, pojos, specs)

    # Write audit bindings json (auto-generated)
    out = BIND_DIR / f"{program}.bindings.json"
    out.write_text(json.dumps(m, indent=2), encoding="utf-8")

    # Patch tasklet
    tasklet_path = PROGRAM_DIR / f"{program}Tasklet.java"
    if not tasklet_path.exists():
        raise FileNotFoundError(tasklet_path)

    java = tasklet_path.read_text(encoding="utf-8")

    java = replace_between(java, STATE_BEGIN, STATE_END, state_block(program, m))
    java = replace_between(java, BIND_BEGIN, BIND_END, binding_block())

    tasklet_path.write_text(java, encoding="utf-8")
    print(f"✔ 3F.1 applied: {program} (bindings written to {out})")


def main():
    ensure_binder_class()

    # Default behavior: apply to CCAC6340 only unless --all
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument("program", nargs="?", default="CCAC6340")
    parser.add_argument("--all", action="store_true")
    args = parser.parse_args()

    if args.all:
        tasklets = sorted(PROGRAM_DIR.glob("*Tasklet.java"))
        programs = [p.stem.replace("Tasklet", "") for p in tasklets]
        for prog in programs:
            apply_to_program(prog)
    else:
        apply_to_program(args.program.upper())

if __name__ == "__main__":
    main()
