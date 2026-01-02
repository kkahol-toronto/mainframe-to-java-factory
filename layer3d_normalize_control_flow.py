"""
Layer 3D — Normalize COBOL Control Flow into Clean Java

Responsibilities:
- Rename illegal paragraph methods
- Introduce explicit state object
- Lift loops into execute()
- Preserve paragraph traceability via comments
"""

from pathlib import Path
import re
import argparse

BASE_DIR = Path("misc-1099/src/main/java/com/fordcredit/misc1099/batch/program")

METHOD_MAP = {
    "0000": "mainline",
    "1000": "initialize",
    "1010": "openFiles",
    "2000": "mainProcess",
    "2200": "processEntireMaster",
    "9000": "endOfJob",
    "9010": "wrapUp",
    "9999": "closeFiles",
}

STATE_CLASS = """
    static class MergeState {
        boolean masterEof = false;
        boolean corpEof = false;
    }
""".strip()


def normalize_methods(code: str):
    methods = {}

    for match in re.finditer(
        r"private void (\d{4})([A-Za-z0-9_]*)\s*\(\)\s*\{",
        code,
    ):
        para = match.group(1)
        rest = match.group(2)
        name = METHOD_MAP.get(para, f"p{para}{rest}")
        methods[match.group(0)] = (
            f"// COBOL {para}-{rest.upper()}\n"
            f"private void {name}(MergeState state) {{"
        )

    for old, new in methods.items():
        code = code.replace(old, new)

    return code


def replace_perform_calls(code: str):
    for para, name in METHOD_MAP.items():
        code = re.sub(
            rf"\b{para}[A-Za-z0-9_]*\(\)",
            f"{name}(state)",
            code,
        )
    return code


def patch_execute(code: str):
    execute_pattern = re.compile(
        r"public RepeatStatus execute.*?\{(.*?)return RepeatStatus.FINISHED;",
        re.S,
    )

    match = execute_pattern.search(code)
    if not match:
        raise RuntimeError("execute() not found")

    new_execute = """
    public RepeatStatus execute(StepContribution contribution, ChunkContext chunkContext) throws Exception {
        MergeState state = new MergeState();
        mainline(state);
        return RepeatStatus.FINISHED;
    }
""".strip()

    return execute_pattern.sub(new_execute, code)


def insert_state_class(code: str):
    if "class MergeState" in code:
        return code

    idx = code.find("{", code.find("class"))
    return code[: idx + 1] + "\n\n" + STATE_CLASS + "\n" + code[idx + 1 :]


def main(program: str):
    path = BASE_DIR / f"{program}Tasklet.java"
    if not path.exists():
        raise FileNotFoundError(path)

    code = path.read_text()

    code = normalize_methods(code)
    code = replace_perform_calls(code)
    code = patch_execute(code)
    code = insert_state_class(code)

    path.write_text(code)
    print(f"✔ Layer 3D applied to {path}")


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--program", required=True)
    args = parser.parse_args()
    main(args.program)
