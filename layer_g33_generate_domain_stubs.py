"""
Layer G.3.3: Generate Runtime Domain Stubs (SAFE, REPLACEABLE)

Purpose:
- Ensure Java execution never fails due to missing domain POJOs or parsers
- Generate minimal, runtime-safe stubs
- Deterministic and idempotent

This layer:
- Reads bindings from work/migration_ir/bindings/<PROGRAM>.bindings.json
- Generates:
  - domain POJO classes
  - parser stubs
  - FixedWidthParser compatibility shim (if missing)

This is a TEMPORARY runtime bridge.
"""

from pathlib import Path
import json
import sys

ROOT = Path(".")
BINDINGS_DIR = ROOT / "work/migration_ir/bindings"
JAVA_SRC = ROOT / "misc-1099/src/main/java"

# -------------------------------------------------------------------
# Helpers
# -------------------------------------------------------------------

def ensure_file(path: Path, content: str):
    path.parent.mkdir(parents=True, exist_ok=True)
    if not path.exists():
        path.write_text(content, encoding="utf-8")
        print(f"✔ Generated {path}")
    else:
        print(f"↷ Exists (skipped): {path}")

def class_path_to_file(fqcn: str) -> Path:
    parts = fqcn.split(".")
    return JAVA_SRC.joinpath(*parts).with_suffix(".java")

# -------------------------------------------------------------------
# Stub Templates
# -------------------------------------------------------------------

POJO_TEMPLATE = """\
package {package};

public class {class_name} {{
    /** Raw fixed-width line (stub) */
    public String rawLine;

    public {class_name}() {{
    }}

    public {class_name}(String rawLine) {{
        this.rawLine = rawLine;
    }}
}}
"""

FIELD_SPECS_TEMPLATE = """\
package {package};

import java.util.List;

public class {class_name} {{
    public static List<Object> getSpecs() {{
        return List.of(); // stub
    }}
}}
"""

FIXED_WIDTH_PARSER_TEMPLATE = """\
package {package};

import java.util.List;

public class FixedWidthParser {{
    public FixedWidthParser(List<?> specs) {{
        // stub constructor
    }}

    public Object parse(String line) {{
        return line;
    }}
}}
"""

# -------------------------------------------------------------------
# Main logic
# -------------------------------------------------------------------

def generate_stubs(program: str):
    bindings_file = BINDINGS_DIR / f"{program}.bindings.json"
    if not bindings_file.exists():
        raise RuntimeError(f"Bindings file missing: {bindings_file}")

    bindings = json.loads(bindings_file.read_text())

    classes = [
        bindings["masterPojoClass"],
        bindings["corporatePojoClass"],
    ]

    field_specs = [
        bindings["masterFieldSpecsClass"],
        bindings["corporateFieldSpecsClass"],
    ]

    print(f"\n=== Layer G.3.3: Generating domain stubs for {program} ===\n")

    # --- POJOs ---
    for fqcn in classes:
        pkg = ".".join(fqcn.split(".")[:-1])
        cls = fqcn.split(".")[-1]
        path = class_path_to_file(fqcn)
        ensure_file(
            path,
            POJO_TEMPLATE.format(package=pkg, class_name=cls)
        )

    # --- FieldSpecs ---
    for fqcn in field_specs:
        pkg = ".".join(fqcn.split(".")[:-1])
        cls = fqcn.split(".")[-1]
        path = class_path_to_file(fqcn)
        ensure_file(
            path,
            FIELD_SPECS_TEMPLATE.format(package=pkg, class_name=cls)
        )

    # --- FixedWidthParser (shared) ---
    parser_pkg = "com.fordcredit.misc1099.parser"
    parser_path = class_path_to_file(f"{parser_pkg}.FixedWidthParser")
    ensure_file(
        parser_path,
        FIXED_WIDTH_PARSER_TEMPLATE.format(package=parser_pkg)
    )

    print("\n✔ Layer G.3.3 complete\n")
    print("Next steps:")
    print("  cd misc-1099")
    print("  ./mvnw test")
    print(f"  python layer_g2_execute_and_compare.py {program}")

# -------------------------------------------------------------------
# Entrypoint
# -------------------------------------------------------------------

def main():
    if len(sys.argv) != 2:
        print("Usage: python layer_g33_generate_domain_stubs.py <PROGRAM>")
        sys.exit(1)

    program = sys.argv[1].upper()
    generate_stubs(program)

if __name__ == "__main__":
    main()
