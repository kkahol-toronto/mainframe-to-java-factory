#!/usr/bin/env python3
"""
Test Case Generator for COBOL Programs

Generates test input/expected files for all COBOL programs based on their
file requirements (from dossiers) and record layouts.

Usage:
  python generate_testcases.py           # Generate for all programs
  python generate_testcases.py CCAC6250  # Generate for specific program
"""

import json
import sys
from pathlib import Path
from datetime import datetime

ROOT = Path(".")
DOSSIER_DIR = ROOT / "work/dossier"
TESTCASE_DIR = ROOT / "work/mainframe_clean/testcases"

# =============================================================================
# PROGRAM CONFIGURATIONS
# =============================================================================

# Each program has its specific test data configuration
PROGRAM_CONFIGS = {
    "CCAC6250": {
        "description": "Reformat reject/master/vendor files with headers/trailers",
        "pattern": "SEQUENTIAL",
        "files": {
            "input": {
                "reject.txt": {
                    "desc": "Edited 1099 reject file",
                    "records": [
                        "# 1099 Reject records (526 bytes each)",
                        "0001|001|12345678|20240101|N|I|B&CCW        |REJECT001|100.00",
                        "0002|002|23456789|20240115|M|E|DEFT         |REJECT002|200.00",
                    ]
                },
                "vendor.txt": {
                    "desc": "1099 vendor file",
                    "records": [
                        "# 1099 Vendor records (320 bytes each)",
                        "VENDOR001|ACME CORP|123 MAIN ST|ANYTOWN|MI|48001|100.00",
                        "VENDOR002|XYZ INC|456 OAK AVE|DETROIT|MI|48226|200.00",
                    ]
                },
                "master.txt": {
                    "desc": "Edited 1099 master file",
                    "records": [
                        "HDR|20240101",
                        "111111111|ALICE SMITH|100.00|N|I|",
                        "222222222|BOB JONES|200.00|M|E|",
                        "TRL|2|300.00",
                    ]
                },
                "control.txt": {
                    "desc": "Control card with trailer date",
                    "records": ["20240101"]
                },
            },
            "expected": {
                "reject_out.txt": ["HDR|20240101", "0001|001|12345678|20240101|N|I|B&CCW        |REJECT001|100.00", "0002|002|23456789|20240115|M|E|DEFT         |REJECT002|200.00", "TRL|2|300.00"],
                "vendor_out.txt": ["HDR|20240101", "VENDOR001|ACME CORP|123 MAIN ST|ANYTOWN|MI|48001|100.00", "VENDOR002|XYZ INC|456 OAK AVE|DETROIT|MI|48226|200.00", "TRL|2|300.00"],
                "master_out.txt": ["HDR|20240101", "111111111|ALICE SMITH|100.00|N|I|", "222222222|BOB JONES|200.00|M|E|", "TRL|2|300.00"],
                "sysout.txt": [],
            },
        },
    },
    "CCAC6310": {
        "description": "Determine tax type from entry code for multiple input sources",
        "pattern": "MULTI_INPUT",
        "files": {
            "input": {
                "misc_trans.txt": {
                    "desc": "Miscellaneous transaction file (Form 20071)",
                    "records": [
                        "# Misc transaction records (320 bytes)",
                        "MISC|001|111111111|ALICE SMITH|100.00|ENT001|",
                        "MISC|002|222222222|BOB JONES|200.00|ENT002|",
                    ]
                },
                "bccw.txt": {
                    "desc": "B&CCW issues file",
                    "records": [
                        "# BCCW records (320 bytes)",
                        "BCCW|001|333333333|CHARLIE BROWN|300.00|ENT003|",
                    ]
                },
                "deft.txt": {
                    "desc": "DEFT issues file",
                    "records": [
                        "# DEFT records (320 bytes)",
                        "DEFT|001|444444444|DIANA PRINCE|400.00|ENT004|",
                    ]
                },
                "reject_cycle.txt": {
                    "desc": "Previously rejected transactions",
                    "records": [
                        "# Reject cycle records (526 bytes)",
                        "REJ|001|555555555|EDWARD NORTON|500.00|ENT005|PREV_REJECT",
                    ]
                },
                "entry_code.txt": {
                    "desc": "1099 entry code table",
                    "records": [
                        "# Entry code to tax type mapping",
                        "ENT001|N|1099-MISC",
                        "ENT002|M|1099-NEC",
                        "ENT003|N|1099-MISC",
                        "ENT004|M|1099-NEC",
                        "ENT005|N|1099-MISC",
                    ]
                },
                "control.txt": {
                    "desc": "Control card",
                    "records": ["20240101"]
                },
            },
            "expected": {
                "misc_trans_out.txt": ["MISC|001|111111111|ALICE SMITH|100.00|ENT001|N|", "MISC|002|222222222|BOB JONES|200.00|ENT002|M|"],
                "bccw_out.txt": ["BCCW|001|333333333|CHARLIE BROWN|300.00|ENT003|N|"],
                "deft_out.txt": ["DEFT|001|444444444|DIANA PRINCE|400.00|ENT004|M|"],
                "reject_out.txt": ["REJ|001|555555555|EDWARD NORTON|500.00|ENT005|N|PREV_REJECT"],
                "sysout.txt": [],
            },
        },
    },
    "CCAC6320": {
        "description": "Validate and route 1099 transactions (foreign, excludes, rejects)",
        "pattern": "MULTI_INPUT",
        "files": {
            "input": {
                "misc_trans.txt": {
                    "desc": "Miscellaneous transaction file",
                    "records": [
                        "MISC|001|111111111|ALICE SMITH|USA|100.00|VALID|",
                        "MISC|002|222222222|BOB JONES|CAN|200.00|FOREIGN|",
                        "MISC|003|333333333|CHARLIE|USA|50.00|EXCLUDE|",
                    ]
                },
                "bccw.txt": {
                    "desc": "B&CCW file",
                    "records": ["BCCW|001|444444444|DIANA|USA|400.00|VALID|"]
                },
                "deft.txt": {
                    "desc": "DEFT file",
                    "records": ["DEFT|001|555555555|EDWARD|USA|500.00|VALID|"]
                },
                "reject_cycle.txt": {
                    "desc": "Reject cycle file",
                    "records": ["REJ|001|666666666|FRANK|USA|600.00|REJECT|"]
                },
                "entry_code.txt": {
                    "desc": "Entry code table",
                    "records": ["ENT001|N|1099-MISC"]
                },
                "control.txt": {
                    "desc": "Control card",
                    "records": ["20240101"]
                },
            },
            "expected": {
                "trans_out.txt": ["MISC|001|111111111|ALICE SMITH|USA|100.00|VALID|", "BCCW|001|444444444|DIANA|USA|400.00|VALID|", "DEFT|001|555555555|EDWARD|USA|500.00|VALID|"],
                "reject_out.txt": ["REJ|001|666666666|FRANK|USA|600.00|REJECT|"],
                "foreign_out.txt": ["MISC|002|222222222|BOB JONES|CAN|200.00|FOREIGN|"],
                "excludes_out.txt": ["MISC|003|333333333|CHARLIE|USA|50.00|EXCLUDE|"],
                "report.txt": ["CCAC6320 PROCESSING REPORT", "Valid: 3", "Foreign: 1", "Excludes: 1", "Rejects: 1"],
                "sysout.txt": [],
            },
        },
    },
    "CCAC6330": {
        "description": "Master update merge - apply transactions to master file",
        "pattern": "TWO_WAY_MERGE",
        "files": {
            "input": {
                "master.txt": {
                    "desc": "1099 Master file",
                    "records": [
                        "HDR|20240101",
                        "111111111|ALICE SMITH|100.00|",
                        "222222222|BOB JONES|200.00|",
                        "333333333|CHARLIE BROWN|300.00|",
                        "TRL|3|600.00",
                    ]
                },
                "transaction.txt": {
                    "desc": "Transaction file with updates",
                    "records": [
                        "222222222|UPDATE|+50.00",
                        "444444444|NEW|400.00",
                    ]
                },
                "master_update.txt": {
                    "desc": "Master update file",
                    "records": [
                        "111111111|DELETE|",
                    ]
                },
                "control.txt": {
                    "desc": "Control card",
                    "records": ["20240101"]
                },
            },
            "expected": {
                "master_out.txt": ["HDR|20240101", "222222222|BOB JONES|250.00|U", "333333333|CHARLIE BROWN|300.00|", "444444444|NEW RECORD|400.00|A", "TRL|3|950.00"],
                "duplicate.txt": [],
                "sysout.txt": [],
            },
        },
    },
    "CCAC6340": {
        "description": "Mechanized flag - match master SSNs to corporate extract",
        "pattern": "TWO_WAY_MERGE",
        "files": {
            "input": {
                "master.txt": {
                    "desc": "1099 Master file",
                    "records": [
                        "HDR|20240101",
                        "111111111|ALICE|100.00|",
                        "222222222|BOB|200.00|",
                        "TRL|2|300.00",
                    ]
                },
                "corporate.txt": {
                    "desc": "Corporate extract file (SSNs only)",
                    "records": ["222222222"]
                },
                "control.txt": {
                    "desc": "Control card",
                    "records": ["20240101"]
                },
            },
            "expected": {
                "master_out.txt": ["HDR|20240101", "111111111|ALICE|100.00|", "222222222|BOB|200.00|M", "TRL|2|300.00"],
                "sysout.txt": [],
            },
        },
    },
    "CCAC6350": {
        "description": "Year-end summary - summarize by location, SSN, tax type, branch",
        "pattern": "SUMMARIZATION",
        "files": {
            "input": {
                "master.txt": {
                    "desc": "1099 Master file to summarize",
                    "records": [
                        "HDR|20240101",
                        "0001|111111111|ALICE SMITH|N|I|100.00|",
                        "0001|111111111|ALICE SMITH|N|I|50.00|",
                        "0001|222222222|BOB JONES|M|E|200.00|",
                        "0002|333333333|CHARLIE BROWN|N|I|300.00|",
                        "TRL|4|650.00",
                    ]
                },
                "control.txt": {
                    "desc": "Control card with 1099 date",
                    "records": ["20240101"]
                },
            },
            "expected": {
                "summary.txt": [
                    "CCAC6350 YEAR-END 1099 SUMMARY",
                    "LOC|SSN|TAX_TYPE|TIN|TOTAL_AMT|COUNT",
                    "0001|111111111|N|I|150.00|2",
                    "0001|222222222|M|E|200.00|1",
                    "0002|333333333|N|I|300.00|1",
                    "TRAILER|0001|350.00|3",
                    "TRAILER|0002|300.00|1",
                    "GRAND_TOTAL|650.00|4",
                ],
                "sysout.txt": [],
            },
        },
    },
}


def generate_testcase(program: str) -> None:
    """Generate test case structure for a program."""
    if program not in PROGRAM_CONFIGS:
        print(f"⚠ No configuration for {program}, creating minimal structure")
        config = None
    else:
        config = PROGRAM_CONFIGS[program]
    
    testcase_dir = TESTCASE_DIR / program
    input_dir = testcase_dir / "input"
    output_dir = testcase_dir / "output"
    expected_dir = testcase_dir / "expected"
    
    # Create directories
    for d in [input_dir, output_dir, expected_dir]:
        d.mkdir(parents=True, exist_ok=True)
    
    if config:
        print(f"\n📁 {program}: {config['description']}")
        print(f"   Pattern: {config['pattern']}")
        
        # Generate input files
        for filename, file_config in config["files"].get("input", {}).items():
            filepath = input_dir / filename
            records = file_config.get("records", [])
            filepath.write_text("\n".join(records) + "\n", encoding="utf-8")
            print(f"   → input/{filename} ({len(records)} records)")
        
        # Generate expected files
        for filename, records in config["files"].get("expected", {}).items():
            filepath = expected_dir / filename
            filepath.write_text("\n".join(records) + ("\n" if records else ""), encoding="utf-8")
            print(f"   → expected/{filename} ({len(records)} records)")
        
        # Create empty output files
        for filename in config["files"].get("expected", {}).keys():
            (output_dir / filename).write_text("", encoding="utf-8")
    
    else:
        # Minimal structure from dossier
        dossier_path = DOSSIER_DIR / f"{program}.json"
        if dossier_path.exists():
            dossier = json.loads(dossier_path.read_text())
            
            # Create placeholders based on selects
            for select in dossier.get("selects", []):
                file_name = select.get("file", "")
                assign = select.get("assign", "")
                
                if "R" in assign[-5:].upper():
                    # Input file
                    simple_name = file_name.split("-")[-1].lower().replace("file", "") + ".txt"
                    (input_dir / simple_name).write_text("# Add test data\n", encoding="utf-8")
                    print(f"   → input/{simple_name} (placeholder)")
                else:
                    # Output file
                    simple_name = file_name.split("-")[-1].lower().replace("file", "") + ".txt"
                    (expected_dir / simple_name).write_text("", encoding="utf-8")
                    print(f"   → expected/{simple_name} (placeholder)")


def main():
    if len(sys.argv) > 1:
        programs = [sys.argv[1].upper()]
    else:
        programs = list(PROGRAM_CONFIGS.keys())
    
    print("\n" + "=" * 60)
    print("TEST CASE GENERATOR")
    print("=" * 60)
    
    for program in programs:
        generate_testcase(program)
    
    print("\n" + "=" * 60)
    print(f"Generated test cases for {len(programs)} program(s)")
    print("=" * 60 + "\n")


if __name__ == "__main__":
    main()

