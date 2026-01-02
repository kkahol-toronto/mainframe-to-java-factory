import zipfile, re, os, pathlib

MAINFRAME_ZIP = "cobol-misc-1099.zip"   # path to your uploaded zip locally
OUT = pathlib.Path("work/mainframe_clean")
OUT.mkdir(parents=True, exist_ok=True)

def clean_cobol_listing(text: str) -> str:
    cleaned = []
    for line in text.splitlines():
        # lines often look like: "000100 IDENTIFICATION DIVISION. .... 00010001"
        # Keep the middle area.
        if re.match(r"^\d{6}\s", line):
            core = line[7:]  # after "000100 "
            core = re.sub(r"\s\d{8}$", "", core)  # drop trailing seq like 00010001
            cleaned.append(core.rstrip())
        else:
            cleaned.append(line.rstrip())
    return "\n".join(cleaned).strip() + "\n"

def clean_jcl_browse(text: str) -> str:
    lines = []
    for line in text.splitlines():
        if line.startswith("BROWSE ") or line.startswith("Command ===>") or "Top of Data" in line or "Bottom of Data" in line:
            continue
        lines.append(line.rstrip())
    return "\n".join(lines).strip() + "\n"

with zipfile.ZipFile(MAINFRAME_ZIP) as z:
    for name in z.namelist():
        if name.endswith("/") or name.startswith("__MACOSX/"):
            continue

        raw = z.read(name).decode("latin-1", errors="ignore")

        if "/cobol/" in name:
            out_text = clean_cobol_listing(raw)
        elif "/jcl/" in name:
            out_text = clean_jcl_browse(raw)
        else:
            out_text = raw

        out_path = OUT / name.replace("FordCredit-MISC-1099/", "")
        out_path.parent.mkdir(parents=True, exist_ok=True)
        out_path.write_text(out_text, encoding="utf-8")

