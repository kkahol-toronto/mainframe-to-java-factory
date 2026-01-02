import re, json, pathlib

SRC = pathlib.Path("work/mainframe_clean/cobol")
CPY = pathlib.Path("work/mainframe_clean/copybook")
DOSSIER = pathlib.Path("work/dossier")
DOSSIER.mkdir(parents=True, exist_ok=True)

def parse_program(text: str, source_file: str, copybook_dir: pathlib.Path) -> dict:
    prog = {}
    prog["source_file"] = source_file
    
    m = re.search(r"PROGRAM-ID\.\s*([A-Z0-9-]+)\.", text, re.IGNORECASE)
    prog["program_id"] = m.group(1).upper() if m else None

    copies = sorted(set(re.findall(r"^\s*COPY\s+([A-Z0-9@#_$-]+)\.?", text, flags=re.MULTILINE|re.IGNORECASE)))
    prog["copies"] = copies
    
    # Check which copybooks actually exist
    available_copybooks = {f.stem.upper(): f.name for f in copybook_dir.glob("*.cpy")}
    prog["copybooks_available"] = [c for c in copies if c.upper() in available_copybooks]
    prog["copybooks_missing"] = [c for c in copies if c.upper() not in available_copybooks]
    
    prog["calls"]  = sorted(set(re.findall(r"^\s*CALL\s+'?\"?([A-Z0-9@#_$-]+)", text, flags=re.MULTILINE|re.IGNORECASE)))

    # SELECT <file> ASSIGN TO <ddname>
    selects = re.findall(r"SELECT\s+([A-Z0-9-]+)\s+.*?ASSIGN\s+TO\s+([A-Z0-9.-]+)", text, flags=re.IGNORECASE)
    prog["selects"] = [{"file": f, "assign": a} for f, a in selects]

    return prog

for p in SRC.glob("*.txt"):
    text = p.read_text(encoding="utf-8", errors="ignore")
    dossier = parse_program(text, p.name, CPY)
    (DOSSIER / f"{p.stem}.json").write_text(json.dumps(dossier, indent=2), encoding="utf-8")

