"""
Layer G.3: Generate Deterministic Java Runner

Responsibility:
- Generate a simple Java main() class per program
- Runner directly invokes the Tasklet.execute()
- No Spring context
- No Job / Step / JobLauncher
- Deterministic, test-friendly execution

This layer is REQUIRED for golden-master validation.
"""

from pathlib import Path
import sys

# ---------------------------------------------------------------------
# PATHS
# ---------------------------------------------------------------------

ROOT = Path(".")
JAVA_BASE = ROOT / "misc-1099/src/main/java"

BASE_PACKAGE = "com.fordcredit.misc1099"
PROGRAM_PKG = f"{BASE_PACKAGE}.batch.program"
RUNNER_PKG  = f"{BASE_PACKAGE}.batch.runner"

PROGRAM_DIR = JAVA_BASE / Path(*PROGRAM_PKG.split("."))
RUNNER_DIR  = JAVA_BASE / Path(*RUNNER_PKG.split("."))

RUNNER_DIR.mkdir(parents=True, exist_ok=True)

# ---------------------------------------------------------------------
# JAVA TEMPLATE
# ---------------------------------------------------------------------

def render_runner(program: str) -> str:
    return f"""\
package {RUNNER_PKG};

import {PROGRAM_PKG}.{program}Tasklet;
import org.springframework.batch.repeat.RepeatStatus;

/**
 * Auto-generated deterministic runner for {program}.
 *
 * Used by Layer G.2 for golden-master validation.
 */
public class {program}Runner {{

    public static void main(String[] args) throws Exception {{
        {program}Tasklet tasklet = new {program}Tasklet();

        // Direct execution (no Spring Batch runtime)
        tasklet.execute(null, null);

        // Explicit exit for deterministic behavior
        System.exit(0);
    }}
}}
"""

# ---------------------------------------------------------------------
# MAIN
# ---------------------------------------------------------------------

def main():
    if len(sys.argv) != 2:
        print("Usage: python layer_g3_generate_runner.py <PROGRAM>")
        sys.exit(1)

    program = sys.argv[1].upper()

    tasklet_file = PROGRAM_DIR / f"{program}Tasklet.java"
    if not tasklet_file.exists():
        raise FileNotFoundError(
            f"Tasklet not found: {tasklet_file}\n"
            "Run Layers 3A–3F first."
        )

    runner_code = render_runner(program)
    runner_path = RUNNER_DIR / f"{program}Runner.java"
    runner_path.write_text(runner_code, encoding="utf-8")

    print(f"✔ Layer G.3 runner generated: {runner_path}")

if __name__ == "__main__":
    main()
