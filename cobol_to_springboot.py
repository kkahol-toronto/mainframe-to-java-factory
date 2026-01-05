#!/usr/bin/env python3
"""
COBOL to Spring Boot Migration Pipeline (Generic Factory)
==========================================================

A factory-grade migration tool that converts ANY COBOL mainframe program
to Java Spring Boot applications. Automatically detects program patterns
and generates appropriate code.

SUPPORTED PATTERNS
------------------
- Two-way sorted merge (e.g., CCAC6340: master vs corporate)
- Multi-input aggregation (e.g., CCAC6310: multiple sources → single output)
- Summarization/grouping (e.g., CCAC6350: group by keys, summarize)
- Sequential file processing (read → transform → write)

ARCHITECTURE
------------
                    ┌─────────────────────────────────────────┐
                    │         COBOL Source + Copybooks        │
                    └─────────────────────────────────────────┘
                                       │
                    ┌──────────────────┼──────────────────────┐
                    ▼                  ▼                      ▼
           ┌─────────────┐   ┌─────────────────┐   ┌─────────────────┐
           │  Layer 1    │   │   Layer 1.5     │   │    Layer 2      │
           │ Copybooks   │   │   FieldSpecs    │   │  JCL → Spring   │
           │  → POJOs    │   │   (Metadata)    │   │     Batch       │
           └─────────────┘   └─────────────────┘   └─────────────────┘
                    │                  │                      │
                    └──────────────────┼──────────────────────┘
                                       ▼
                    ┌─────────────────────────────────────────┐
                    │     Layer 3A: Program IR (Analysis)     │
                    │  - Pattern Detection                    │
                    │  - File Role Classification             │
                    │  - Control Flow Analysis                │
                    └─────────────────────────────────────────┘
                                       │
                                       ▼
                    ┌─────────────────────────────────────────┐
                    │     Layer 3B: Tasklet Skeleton          │
                    │  - Pattern-specific template            │
                    │  - File binding from dossier            │
                    │  - State class generation               │
                    └─────────────────────────────────────────┘
                                       │
                                       ▼
                    ┌─────────────────────────────────────────┐
                    │     Layer 4: Business Logic Wiring      │
                    │  - Pattern-specific main loop           │
                    │  - Helper methods injection             │
                    └─────────────────────────────────────────┘
                                       │
                                       ▼
                    ┌─────────────────────────────────────────┐
                    │     Layer G: Test Harness Generation    │
                    │  - Test runner class                    │
                    │  - Testcase structure                   │
                    │  - Output manifest                      │
                    └─────────────────────────────────────────┘

USAGE
-----
  # Migrate a single program
  python cobol_to_springboot.py --program CCAC6340

  # Migrate ALL programs in work folder
  python cobol_to_springboot.py --all

  # Foundation only (copybooks, JCL)
  python cobol_to_springboot.py --foundation-only

  # Run with tests
  python cobol_to_springboot.py --program CCAC6340 --test
"""

from __future__ import annotations

import argparse
import json
import os
import re
import subprocess
import sys
from dataclasses import dataclass, field
from enum import Enum, auto
from pathlib import Path
from typing import Any, Dict, List, Optional, Tuple

from dotenv import load_dotenv
from openai import AzureOpenAI

# =============================================================================
# CONFIGURATION
# =============================================================================

load_dotenv()


class ProgramPattern(Enum):
    """Detected COBOL program patterns."""
    TWO_WAY_MERGE = auto()       # Master vs Transaction merge
    MULTI_INPUT = auto()         # Multiple inputs → single output
    SUMMARIZATION = auto()       # Group and summarize
    SEQUENTIAL = auto()          # Simple read → transform → write
    UNKNOWN = auto()


class FileRole(Enum):
    """Semantic role of a file in the program."""
    MASTER_INPUT = auto()
    TRANSACTION_INPUT = auto()
    CONTROL_INPUT = auto()
    LOOKUP_INPUT = auto()
    MASTER_OUTPUT = auto()
    REPORT_OUTPUT = auto()
    REJECT_OUTPUT = auto()
    SYSOUT = auto()
    UNKNOWN = auto()


@dataclass
class FileSpec:
    """Specification for a file in the program."""
    select_name: str
    assign_name: str
    role: FileRole
    record_length: int = 80
    is_required: bool = True


@dataclass
class ProgramAnalysis:
    """Analysis result for a COBOL program."""
    program_id: str
    pattern: ProgramPattern
    files: List[FileSpec]
    copybooks: List[str]
    has_headers_trailers: bool = True
    key_fields: List[str] = field(default_factory=list)
    purpose: str = ""


@dataclass
class Config:
    """Pipeline configuration."""
    
    # Azure OpenAI
    api_key: str = field(default_factory=lambda: os.getenv("AZURE_OPENAI_API_KEY", ""))
    api_endpoint: str = field(default_factory=lambda: os.getenv("AZURE_OPENAI_ENDPOINT", ""))
    deployment: str = field(default_factory=lambda: os.getenv("AZURE_OPENAI_DEPLOYMENT", ""))
    
    # Paths
    root: Path = field(default_factory=lambda: Path("."))
    
    @property
    def cobol_dir(self) -> Path:
        return self.root / "work/mainframe_clean/cobol"
    
    @property
    def copybook_dir(self) -> Path:
        return self.root / "work/mainframe_clean/copybook"
    
    @property
    def jcl_dir(self) -> Path:
        return self.root / "work/mainframe_clean/jcl"
    
    @property
    def dossier_dir(self) -> Path:
        return self.root / "work/dossier"
    
    @property
    def ir_dir(self) -> Path:
        return self.root / "work/migration_ir/cobol"
    
    @property
    def bindings_dir(self) -> Path:
        return self.root / "work/migration_ir/bindings"
    
    @property
    def runtime_dir(self) -> Path:
        return self.root / "work/migration_ir/runtime"
    
    @property
    def testcase_dir(self) -> Path:
        return self.root / "work/mainframe_clean/testcases"
    
    @property
    def java_base(self) -> Path:
        return self.root / "misc-1099/src/main/java"
    
    @property
    def base_package(self) -> str:
        return "com.fordcredit.misc1099"
    
    def validate(self) -> None:
        """Validate configuration."""
        if not self.api_key:
            raise ValueError("AZURE_OPENAI_API_KEY environment variable required")
        if not self.api_endpoint:
            raise ValueError("AZURE_OPENAI_ENDPOINT environment variable required")
        if not self.deployment:
            raise ValueError("AZURE_OPENAI_DEPLOYMENT environment variable required")


# =============================================================================
# LLM CLIENT
# =============================================================================

class LLMClient:
    """Azure OpenAI client wrapper."""
    
    def __init__(self, config: Config):
        self.config = config
        self.client = AzureOpenAI(
            api_key=config.api_key,
            api_version="2024-02-15-preview",
            azure_endpoint=config.api_endpoint,
        )
    
    def generate(self, prompt: str, temperature: float = 0) -> str:
        """Generate completion from prompt."""
        response = self.client.chat.completions.create(
            model=self.config.deployment,
            messages=[{"role": "user", "content": prompt}],
            temperature=temperature,
        )
        return response.choices[0].message.content
    
    def generate_json(self, prompt: str) -> Dict[str, Any]:
        """Generate JSON from prompt."""
        text = self.generate(prompt)
        text = self._strip_markdown(text)
        return json.loads(text)
    
    def generate_java(self, prompt: str) -> str:
        """Generate Java code from prompt with sanitization."""
        text = self.generate(prompt)
        return self._harden_java(text)
    
    @staticmethod
    def _strip_markdown(text: str) -> str:
        """Remove markdown code fences."""
        lines = text.strip().splitlines()
        if lines and lines[0].strip().startswith("```"):
            lines = lines[1:]
        if lines and lines[-1].strip().startswith("```"):
            lines = lines[:-1]
        return "\n".join(lines).strip()
    
    @staticmethod
    def _harden_java(text: str) -> str:
        """Clean and validate Java code from LLM output."""
        text = LLMClient._strip_markdown(text)
        
        lines = []
        for raw in text.splitlines():
            line = raw.rstrip()
            if line.strip().startswith("```"):
                continue
            line = (
                line.replace("`", "")
                    .replace("—", "-")
                    .replace("–", "-")
                    .replace(""", "\"")
                    .replace(""", "\"")
            )
            lines.append(line)
        
        package_line = next((l for l in lines if l.startswith("package ")), None)
        if not package_line:
            return ""
        
        import_lines = [l for l in lines if l.startswith("import ")]
        
        class_idx = None
        for i, l in enumerate(lines):
            if re.search(r"public\s+class\s+\w+", l):
                class_idx = i
                break
        
        if class_idx is None:
            return ""
        
        start = class_idx
        while start - 1 >= 0 and lines[start - 1].strip().startswith("@"):
            start -= 1
        
        end = None
        for i in range(len(lines) - 1, start, -1):
            if "}" in lines[i]:
                end = i
                break
        
        if end is None:
            return ""
        
        class_block = lines[start:end + 1]
        
        result = [package_line, ""]
        result.extend(import_lines)
        result.append("")
        result.extend(class_block)
        
        return "\n".join(result).strip() + "\n"


# =============================================================================
# PATTERN ANALYZER
# =============================================================================

class PatternAnalyzer:
    """Analyzes COBOL programs to detect patterns."""
    
    # Keywords that indicate specific patterns
    MERGE_KEYWORDS = ["MERGE", "MATCH", "COMPARE", "MECHANIZED", "CORPORATE"]
    SUMMARIZE_KEYWORDS = ["SUMMARIZE", "SUMMARY", "TOTAL", "ACCUMULATE", "GROUP"]
    MULTI_INPUT_KEYWORDS = ["BCCW", "DEFT", "REJECT", "MULTIPLE"]
    
    # File role patterns
    FILE_ROLE_PATTERNS = {
        FileRole.MASTER_INPUT: [r"MASTER.*IN", r"M01R", r"MASTER-FILE"],
        FileRole.TRANSACTION_INPUT: [r"TRANS", r"T0[1-9]R", r"CORPORATE", r"BCCW", r"DEFT"],
        FileRole.CONTROL_INPUT: [r"CONTROL", r"R01R", r"PARM"],
        FileRole.LOOKUP_INPUT: [r"ENTRY.*CD", r"TABLE", r"LOOKUP", r"T07R"],
        FileRole.MASTER_OUTPUT: [r"MASTER.*OUT", r"M01W"],
        FileRole.REPORT_OUTPUT: [r"SUMMARY", r"DISPLAY", r"C01W", r"E01W"],
        FileRole.REJECT_OUTPUT: [r"REJECT", r"T0[1-9]W.*REJ"],
        FileRole.SYSOUT: [r"SYSOUT", r"SYSPRINT"],
    }
    
    def __init__(self, config: Config):
        self.config = config
    
    def analyze(self, program_id: str) -> ProgramAnalysis:
        """Analyze a COBOL program and return its characteristics."""
        
        # Load dossier
        dossier = self._load_dossier(program_id)
        
        # Load COBOL source
        cobol_text = self._load_cobol(program_id)
        
        # Detect pattern
        pattern = self._detect_pattern(cobol_text, dossier)
        
        # Classify files
        files = self._classify_files(dossier.get("selects", []), cobol_text)
        
        # Extract purpose
        purpose = self._extract_purpose(cobol_text)
        
        # Detect header/trailer usage
        has_headers_trailers = self._detect_headers_trailers(cobol_text)
        
        # Extract key fields
        key_fields = self._extract_key_fields(cobol_text)
        
        return ProgramAnalysis(
            program_id=program_id,
            pattern=pattern,
            files=files,
            copybooks=dossier.get("copybooks_available", []),
            has_headers_trailers=has_headers_trailers,
            key_fields=key_fields,
            purpose=purpose,
        )
    
    def _load_dossier(self, program_id: str) -> Dict:
        dossier_path = self.config.dossier_dir / f"{program_id}.json"
        if dossier_path.exists():
            return json.loads(dossier_path.read_text(encoding="utf-8"))
        return {}
    
    def _load_cobol(self, program_id: str) -> str:
        for ext in [".txt", ".cbl", ".cob"]:
            path = self.config.cobol_dir / f"{program_id}{ext}"
            if path.exists():
                return path.read_text(encoding="utf-8", errors="ignore")
        return ""
    
    def _detect_pattern(self, cobol_text: str, dossier: Dict) -> ProgramPattern:
        """Detect the processing pattern from COBOL source."""
        text_upper = cobol_text.upper()
        
        # Count input/output files
        selects = dossier.get("selects", [])
        input_files = [s for s in selects if "R" in s.get("assign", "").upper()[-5:]]
        output_files = [s for s in selects if "W" in s.get("assign", "").upper()[-5:]]
        
        # Check for multi-input FIRST (higher priority if many inputs)
        # Programs with 4+ input files are typically multi-input processors
        if len(input_files) >= 4:
            return ProgramPattern.MULTI_INPUT
        
        # Check for two-way merge pattern indicators
        if any(kw in text_upper for kw in self.MERGE_KEYWORDS):
            # Two input files with COMPARE/MATCH is a merge
            if len(input_files) <= 3:
                return ProgramPattern.TWO_WAY_MERGE
        
        # Check for summarization (must have GROUP/SUMMARY keywords AND few inputs)
        summarize_score = sum(1 for kw in self.SUMMARIZE_KEYWORDS if kw in text_upper)
        if summarize_score >= 2 and len(input_files) <= 2:
            return ProgramPattern.SUMMARIZATION
        
        # Multi-input for 3+ input files
        if len(input_files) >= 3:
            return ProgramPattern.MULTI_INPUT
        
        # Default to sequential for simple programs
        return ProgramPattern.SEQUENTIAL
    
    def _classify_files(self, selects: List[Dict], cobol_text: str) -> List[FileSpec]:
        """Classify files by their semantic role."""
        files = []
        
        for select in selects:
            file_name = select.get("file", "")
            assign_name = select.get("assign", "")
            
            role = self._detect_file_role(file_name, assign_name)
            
            # Detect record length from COBOL
            record_length = self._detect_record_length(file_name, cobol_text)
            
            files.append(FileSpec(
                select_name=file_name,
                assign_name=assign_name,
                role=role,
                record_length=record_length,
            ))
        
        return files
    
    def _detect_file_role(self, file_name: str, assign_name: str) -> FileRole:
        """Detect the semantic role of a file."""
        combined = f"{file_name} {assign_name}".upper()
        
        for role, patterns in self.FILE_ROLE_PATTERNS.items():
            for pattern in patterns:
                if re.search(pattern, combined):
                    return role
        
        return FileRole.UNKNOWN
    
    def _detect_record_length(self, file_name: str, cobol_text: str) -> int:
        """Detect record length from FD section."""
        # Look for RECORD CONTAINS n CHARACTERS
        pattern = rf"FD\s+{re.escape(file_name)}.*?RECORD\s+CONTAINS\s+(\d+)"
        match = re.search(pattern, cobol_text, re.IGNORECASE | re.DOTALL)
        if match:
            return int(match.group(1))
        
        # Look for PIC X(n) in 01 level after FD
        fd_pattern = rf"FD\s+{re.escape(file_name)}.*?01\s+\S+\s+PIC\s+X\((\d+)\)"
        match = re.search(fd_pattern, cobol_text, re.IGNORECASE | re.DOTALL)
        if match:
            return int(match.group(1))
        
        return 80  # Default
    
    def _extract_purpose(self, cobol_text: str) -> str:
        """Extract program purpose from comments."""
        # Look for REMARKS section
        match = re.search(r"\*REMARKS\.?\s*\n(.*?)(?:\n\*{50}|\nENVIRONMENT)", 
                          cobol_text, re.DOTALL | re.IGNORECASE)
        if match:
            lines = match.group(1).splitlines()
            purpose_lines = [l.lstrip("*").strip() for l in lines if l.strip()]
            return " ".join(purpose_lines[:3])
        return ""
    
    def _detect_headers_trailers(self, cobol_text: str) -> bool:
        """Check if program uses headers/trailers."""
        return bool(re.search(r"(HEADER|TRAILER|HDR|TRL|HIGH-VALUES|LOW-VALUES)", 
                              cobol_text, re.IGNORECASE))
    
    def _extract_key_fields(self, cobol_text: str) -> List[str]:
        """Extract key fields used for matching/sorting."""
        keys = []
        
        # Look for common key field patterns
        patterns = [
            r"SOCIAL-SEC(?:URITY)?-(?:NUM(?:BER)?|NO)",
            r"SSN",
            r"EIN",
            r"ACCT-?(?:NUM|NO)",
            r"KEY(?:-FIELD)?",
        ]
        
        for pattern in patterns:
            matches = re.findall(rf"\b(\w*{pattern}\w*)\b", cobol_text, re.IGNORECASE)
            keys.extend(matches)
        
        return list(set(keys))[:5]  # Top 5 unique keys


# =============================================================================
# LAYER BASE CLASS
# =============================================================================

class Layer:
    """Base class for pipeline layers."""
    
    name: str = "Base Layer"
    
    def __init__(self, config: Config, llm: Optional[LLMClient] = None):
        self.config = config
        self.llm = llm
    
    def run(self, program: Optional[str] = None, analysis: Optional[ProgramAnalysis] = None) -> None:
        """Execute the layer."""
        raise NotImplementedError
    
    def _ensure_dir(self, path: Path) -> Path:
        """Ensure directory exists."""
        path.mkdir(parents=True, exist_ok=True)
        return path
    
    def _extract_class_name(self, java_code: str) -> Optional[str]:
        """Extract public class name from Java code."""
        m = re.search(r"public\s+class\s+([A-Za-z_][A-Za-z0-9_]*)", java_code)
        return m.group(1) if m else None


# =============================================================================
# LAYER 1: COPYBOOKS TO JAVA POJOS
# =============================================================================

class Layer1_CopybooksToPojos(Layer):
    """Convert COBOL copybooks to Java POJOs."""
    
    name = "Layer 1: Copybooks → POJOs"
    
    def run(self, program: Optional[str] = None, analysis: Optional[ProgramAnalysis] = None) -> None:
        print(f"\n=== {self.name} ===\n")
        
        out_dir = self._ensure_dir(
            self.config.java_base / "com/fordcredit/misc1099/domain/copybook"
        )
        
        copybooks = list(self.config.copybook_dir.glob("*.cpy"))
        if not copybooks:
            print("No copybooks found, skipping.")
            return
        
        for copybook in copybooks:
            out_path = out_dir / f"{copybook.stem}.java"
            if out_path.exists():
                print(f"  ✓ {copybook.name} (already exists)")
                continue
            
            print(f"Processing {copybook.name}...")
            
            cobol_text = copybook.read_text(encoding="utf-8", errors="ignore")
            prompt = self._build_prompt(cobol_text)
            
            java_code = self.llm.generate_java(prompt)
            class_name = self._extract_class_name(java_code)
            
            if not class_name:
                print(f"  ⚠ Could not extract class name, skipping")
                continue
            
            out_path = out_dir / f"{class_name}.java"
            out_path.write_text(java_code, encoding="utf-8")
            print(f"  → {out_path}")
        
        print(f"\n✔ {self.name} complete\n")
    
    def _build_prompt(self, copybook_text: str) -> str:
        return f"""
You are a legacy systems migration engine.

Convert the following COBOL copybook into a Java data model with byte-level fidelity.
Do NOT modernize, simplify, or infer business meaning.

TARGET ENVIRONMENT
- Java 21, Spring Boot 3.5.x
- No Lombok, no frameworks
- Pure Java domain model

STRICT RULES
1. Preserve field order exactly
2. Preserve COBOL hierarchy (01/05/10 levels → nested classes)
3. PIC X(...) → String
4. PIC 9/S9/V → BigDecimal with correct scale
5. OCCURS → List<T> or fixed-size array
6. REDEFINES must be modeled explicitly
7. No validation, defaults, or business logic
8. Output ONLY Java source code
9. One public top-level class per copybook
10. Do NOT wrap output in markdown fences

PACKAGE: com.fordcredit.misc1099.domain.copybook

COBOL COPYBOOK
--------------------------------------------------
{copybook_text}
--------------------------------------------------
""".strip()


# =============================================================================
# LAYER 1.5: COPYBOOKS TO FIELDSPECS
# =============================================================================

class Layer1_5_CopybooksToFieldSpecs(Layer):
    """Convert COBOL copybooks to FieldSpec metadata."""
    
    name = "Layer 1.5: Copybooks → FieldSpecs"
    
    def run(self, program: Optional[str] = None, analysis: Optional[ProgramAnalysis] = None) -> None:
        print(f"\n=== {self.name} ===\n")
        
        out_dir = self._ensure_dir(
            self.config.java_base / "com/fordcredit/misc1099/parser"
        )
        
        copybooks = list(self.config.copybook_dir.glob("*.cpy"))
        if not copybooks:
            print("No copybooks found, skipping.")
            return
        
        for copybook in copybooks:
            out_path = out_dir / f"{copybook.stem}FieldSpecs.java"
            if out_path.exists():
                print(f"  ✓ {copybook.name} (already exists)")
                continue
            
            print(f"Processing {copybook.name}...")
            
            cobol_text = copybook.read_text(encoding="utf-8", errors="ignore")
            prompt = self._build_prompt(cobol_text)
            
            java_code = self.llm.generate_java(prompt)
            class_name = self._extract_class_name(java_code)
            
            if not class_name:
                print(f"  ⚠ Could not extract class name, skipping")
                continue
            
            out_path = out_dir / f"{class_name}.java"
            out_path.write_text(java_code, encoding="utf-8")
            print(f"  → {out_path}")
        
        print(f"\n✔ {self.name} complete\n")
    
    def _build_prompt(self, copybook_text: str) -> str:
        return f"""
You are a legacy data layout compiler.

Generate Java fixed-width field layout metadata from the COBOL copybook.

STRICT RULES
- Generate ONLY layout metadata
- NO generics, NO record types, NO setters
- EXACTLY one public class
- Output MUST compile as-is

TARGET SHAPE:
public class <RecordName>FieldSpecs {{
    public static List<FieldSpec> fields() {{
        return List.of(
            FieldSpec.string("fieldName", start, length),
            FieldSpec.decimal("amount", start, length, scale)
        );
    }}
}}

MAPPING: PIC X → FieldSpec.string, PIC 9/S9/V → FieldSpec.decimal

PACKAGE: com.fordcredit.misc1099.parser

IMPORTS: java.util.List, com.fordcredit.misc1099.util.fixedwidth.FieldSpec

COBOL COPYBOOK
--------------------------------------------------
{copybook_text}
--------------------------------------------------
""".strip()


# =============================================================================
# LAYER 2: JCL TO SPRING BATCH
# =============================================================================

class Layer2_JclToSpringBatch(Layer):
    """Convert JCL files to Spring Batch configuration."""
    
    name = "Layer 2: JCL → Spring Batch"
    
    def run(self, program: Optional[str] = None, analysis: Optional[ProgramAnalysis] = None) -> None:
        print(f"\n=== {self.name} ===\n")
        
        job_dir = self._ensure_dir(
            self.config.java_base / "com/fordcredit/misc1099/batch/job"
        )
        step_dir = self._ensure_dir(
            self.config.java_base / "com/fordcredit/misc1099/batch/step"
        )
        
        jcl_files = list(self.config.jcl_dir.glob("*.txt"))
        if not jcl_files:
            print("No JCL files found, skipping.")
            return
        
        for jcl_file in jcl_files:
            print(f"Processing {jcl_file.name}...")
            jcl_text = jcl_file.read_text(encoding="utf-8", errors="ignore")
            
            # Generate JobConfig
            job_code = self.llm.generate_java(self._job_prompt(jcl_text))
            job_class = self._extract_class_name(job_code)
            if job_class:
                (job_dir / f"{job_class}.java").write_text(job_code, encoding="utf-8")
                print(f"  → JobConfig: {job_class}.java")
            
            # Generate StepsConfig
            steps_code = self.llm.generate_java(self._steps_prompt(jcl_text))
            steps_class = self._extract_class_name(steps_code)
            if steps_class:
                (step_dir / f"{steps_class}.java").write_text(steps_code, encoding="utf-8")
                print(f"  → StepsConfig: {steps_class}.java")
        
        print(f"\n✔ {self.name} complete\n")
    
    def _job_prompt(self, jcl: str) -> str:
        return f"""
Generate ONE Java Spring Batch JobConfig class.

RULES:
- Spring Batch 5 / Spring Boot 3 ONLY
- DO NOT use @EnableBatchProcessing
- Use @Configuration, JobBuilder + JobRepository
- No prose, no markdown

PACKAGE: com.fordcredit.misc1099.batch.job

JCL:
{jcl}
""".strip()
    
    def _steps_prompt(self, jcl: str) -> str:
        return f"""
Generate ONE Java Spring Batch StepsConfig class.

RULES:
- Spring Batch 5 / Spring Boot 3 ONLY
- DO NOT use @EnableBatchProcessing
- Use @Configuration, StepBuilder + JobRepository + PlatformTransactionManager
- One Step per EXEC, Tasklet placeholders only
- No prose, no markdown

PACKAGE: com.fordcredit.misc1099.batch.step

JCL:
{jcl}
""".strip()


# =============================================================================
# LAYER 3A: COBOL TO INTERMEDIATE REPRESENTATION
# =============================================================================

class Layer3A_CobolToIR(Layer):
    """Convert COBOL program to Intermediate Representation (JSON)."""
    
    name = "Layer 3A: COBOL → IR"
    
    def __init__(self, config: Config, llm: Optional[LLMClient] = None, 
                 analyzer: Optional[PatternAnalyzer] = None):
        super().__init__(config, llm)
        self.analyzer = analyzer or PatternAnalyzer(config)
    
    def run(self, program: Optional[str] = None, analysis: Optional[ProgramAnalysis] = None) -> ProgramAnalysis:
        print(f"\n=== {self.name} ===\n")
        
        ir_dir = self._ensure_dir(self.config.ir_dir)
        
        if not program:
            raise ValueError("Layer 3A requires --program")
        
        print(f"Analyzing {program}...")
        
        # Get pattern analysis
        analysis = self.analyzer.analyze(program)
        
        # Get LLM-enhanced IR
        cobol_text = self._load_cobol(program)
        ir = self.llm.generate_json(self._build_prompt(cobol_text))
        
        # Merge with pattern analysis
        ir["detectedPattern"] = analysis.pattern.name
        ir["fileRoles"] = [
            {"name": f.select_name, "role": f.role.name, "recordLength": f.record_length}
            for f in analysis.files
        ]
        ir["hasHeadersTrailers"] = analysis.has_headers_trailers
        ir["keyFields"] = analysis.key_fields
        
        # Save IR
        ir_path = ir_dir / f"{program}.json"
        ir_path.write_text(json.dumps(ir, indent=2), encoding="utf-8")
        print(f"  → IR: {ir_path}")
        print(f"  Pattern: {analysis.pattern.name}")
        print(f"  Files: {len(analysis.files)}")
        
        print(f"\n✔ {self.name} complete\n")
        
        return analysis
    
    def _load_cobol(self, program_id: str) -> str:
        for ext in [".txt", ".cbl", ".cob"]:
            path = self.config.cobol_dir / f"{program_id}{ext}"
            if path.exists():
                return path.read_text(encoding="utf-8", errors="ignore")
        return ""
    
    def _build_prompt(self, cobol_text: str) -> str:
        return f"""
You are a COBOL batch program analyzer.

Return ONLY valid JSON (no markdown) describing the program.

SCHEMA
{{
  "programId": "string",
  "purpose": "string (one sentence)",
  "files": [
    {{"selectName": "string", "fdName": "string", "recordPic": "string", "direction": "INPUT|OUTPUT"}}
  ],
  "copybooks": ["string"],
  "workingStorage": {{
    "flags": ["string"],
    "counters": ["string"],
    "accumulators": ["string"],
    "keyFields": ["string"]
  }},
  "controlFlow": {{
    "mainEntry": "paragraph name",
    "termination": "paragraph name",
    "paragraphs": ["string"],
    "errorExits": ["string"]
  }},
  "patterns": {{
    "isTwoWayMerge": boolean,
    "isGroupSummarize": boolean,
    "isMultiInput": boolean,
    "notes": ["string"]
  }}
}}

COBOL INPUT
--------------------------------------------------
{cobol_text[:8000]}
--------------------------------------------------
""".strip()


# =============================================================================
# LAYER 3B: GENERIC TASKLET SKELETON GENERATOR
# =============================================================================

class Layer3B_TaskletSkeleton(Layer):
    """Generate pattern-specific Tasklet skeleton."""
    
    name = "Layer 3B: Tasklet Skeleton"
    
    def run(self, program: Optional[str] = None, analysis: Optional[ProgramAnalysis] = None) -> None:
        if not program or not analysis:
            raise ValueError("Layer 3B requires --program and analysis")
        
        print(f"\n=== {self.name} ===\n")
        
        tasklet_dir = self._ensure_dir(
            self.config.java_base / "com/fordcredit/misc1099/batch/program"
        )
        
        print(f"Generating skeleton for {program}...")
        print(f"  Pattern: {analysis.pattern.name}")
        
        # Generate pattern-specific skeleton
        java_code = self._render_skeleton(program, analysis)
        
        out_path = tasklet_dir / f"{program}Tasklet.java"
        out_path.write_text(java_code, encoding="utf-8")
        print(f"  → {out_path}")
        
        # Post-process to fix common compilation issues
        try:
            from post_process_java import JavaPostProcessor
            processor = JavaPostProcessor()
            processed_code = processor.process(java_code)
            out_path.write_text(processed_code, encoding="utf-8")
            print(f"  → Post-processed (fixed I/O, types, etc.)")
        except Exception as e:
            print(f"  ⚠ Post-processing failed: {e}")
        
        print(f"\n✔ {self.name} complete\n")
    
    def _render_skeleton(self, program_id: str, analysis: ProgramAnalysis) -> str:
        pkg = f"{self.config.base_package}.batch.program"
        
        # Generate state fields based on files
        state_fields = self._generate_state_fields(analysis)
        
        # Generate file open/close methods
        open_files_method = self._generate_open_files(program_id, analysis)
        close_files_method = self._generate_close_files(analysis)
        
        # Generate read methods for each input file
        read_methods = self._generate_read_methods(analysis)
        
        # Generate write methods for each output file
        write_methods = self._generate_write_methods(analysis)
        
        # Generate pattern-specific main process
        main_process = self._generate_main_process(analysis)
        
        # Generate helper methods based on pattern
        helper_methods = self._generate_helper_methods(analysis)
        
        return f"""\
package {pkg};

import org.springframework.batch.core.StepContribution;
import org.springframework.batch.core.scope.context.ChunkContext;
import org.springframework.batch.core.step.tasklet.Tasklet;
import org.springframework.batch.repeat.RepeatStatus;

/**
 * Auto-generated Tasklet for COBOL program {program_id}.
 * 
 * Pattern: {analysis.pattern.name}
 * Purpose: {analysis.purpose}
 */
public class {program_id}Tasklet implements Tasklet {{

    /**
     * Program state holder.
     */
    static class ProgramState {{
{state_fields}
    }}

    @Override
    public RepeatStatus execute(
            StepContribution contribution,
            ChunkContext chunkContext) {{

        ProgramState state = new ProgramState();
        mainline(state);
        return RepeatStatus.FINISHED;
    }}

    private void mainline(ProgramState state) {{
        openFiles(state);
        initialization(state);
        mainProcess(state);
        endOfJob(state);
        closeFiles(state);
    }}

    // ======================================================
    // FILE I/O
    // ======================================================

{open_files_method}

{close_files_method}

{read_methods}

{write_methods}

    // ======================================================
    // BUSINESS LOGIC
    // ======================================================

    private void initialization(ProgramState state) {{
        // TODO: Initialize counters, accumulators, flags
    }}

{main_process}

    private void endOfJob(ProgramState state) {{
        // TODO: Write trailers, final reports
    }}

    // ======================================================
    // HELPER METHODS
    // ======================================================

{helper_methods}
}}
"""
    
    def _generate_state_fields(self, analysis: ProgramAnalysis) -> str:
        lines = []
        
        # EOF flags for input files
        for f in analysis.files:
            if f.role in [FileRole.MASTER_INPUT, FileRole.TRANSACTION_INPUT, 
                          FileRole.LOOKUP_INPUT, FileRole.CONTROL_INPUT]:
                var_name = self._to_var_name(f.select_name)
                lines.append(f"        java.io.BufferedReader {var_name}Reader;")
                lines.append(f"        boolean {var_name}Eof = false;")
                lines.append(f"        String {var_name}RawLine;")
        
        # Writers for output files
        for f in analysis.files:
            if f.role in [FileRole.MASTER_OUTPUT, FileRole.REPORT_OUTPUT, 
                          FileRole.REJECT_OUTPUT, FileRole.SYSOUT]:
                var_name = self._to_var_name(f.select_name)
                lines.append(f"        java.io.BufferedWriter {var_name}Writer;")
                lines.append(f"        String {var_name}OutLine;")
        
        # Pattern-specific fields
        if analysis.pattern == ProgramPattern.TWO_WAY_MERGE:
            lines.append("        String deleteIndicator;")
        elif analysis.pattern == ProgramPattern.SUMMARIZATION:
            lines.append("        java.math.BigDecimal totalAmount = java.math.BigDecimal.ZERO;")
            lines.append("        int recordCount = 0;")
        
        return "\n".join(lines)
    
    def _generate_open_files(self, program_id: str, analysis: ProgramAnalysis) -> str:
        lines = ["    private void openFiles(ProgramState state) {",
                 "        try {",
                 f'            String programName = "{program_id}";',
                 '            java.nio.file.Path testDir = java.nio.file.Paths.get(',
                 '                    "../work/mainframe_clean/testcases", programName);',
                 ""]
        
        # Open input files
        for f in analysis.files:
            if f.role in [FileRole.MASTER_INPUT, FileRole.TRANSACTION_INPUT,
                          FileRole.LOOKUP_INPUT, FileRole.CONTROL_INPUT]:
                var_name = self._to_var_name(f.select_name)
                file_name = self._to_file_name(f.select_name, f.role)
                lines.append(f'            java.nio.file.Path {var_name}Path = testDir.resolve("input/{file_name}");')
                lines.append(f'            if (java.nio.file.Files.exists({var_name}Path)) {{')
                lines.append(f'                state.{var_name}Reader = java.nio.file.Files.newBufferedReader({var_name}Path);')
                lines.append(f'            }}')
                lines.append("")
        
        # Open output files
        for f in analysis.files:
            if f.role in [FileRole.MASTER_OUTPUT, FileRole.REPORT_OUTPUT,
                          FileRole.REJECT_OUTPUT, FileRole.SYSOUT]:
                var_name = self._to_var_name(f.select_name)
                file_name = self._to_file_name(f.select_name, f.role)
                lines.append(f'            java.nio.file.Path {var_name}Path = testDir.resolve("output/{file_name}");')
                lines.append(f'            java.nio.file.Files.createDirectories({var_name}Path.getParent());')
                lines.append(f'            state.{var_name}Writer = java.nio.file.Files.newBufferedWriter({var_name}Path);')
                lines.append("")
        
        lines.append("        } catch (Exception e) {")
        lines.append('            throw new RuntimeException("Failed to open files", e);')
        lines.append("        }")
        lines.append("    }")
        
        return "\n".join(lines)
    
    def _generate_close_files(self, analysis: ProgramAnalysis) -> str:
        lines = ["    private void closeFiles(ProgramState state) {",
                 "        try {"]
        
        for f in analysis.files:
            var_name = self._to_var_name(f.select_name)
            if f.role in [FileRole.MASTER_INPUT, FileRole.TRANSACTION_INPUT,
                          FileRole.LOOKUP_INPUT, FileRole.CONTROL_INPUT]:
                lines.append(f'            if (state.{var_name}Reader != null) state.{var_name}Reader.close();')
            if f.role in [FileRole.MASTER_OUTPUT, FileRole.REPORT_OUTPUT,
                          FileRole.REJECT_OUTPUT, FileRole.SYSOUT]:
                lines.append(f'            if (state.{var_name}Writer != null) state.{var_name}Writer.close();')
        
        lines.append("        } catch (Exception e) {")
        lines.append('            throw new RuntimeException("Failed to close files", e);')
        lines.append("        }")
        lines.append("    }")
        
        return "\n".join(lines)
    
    def _generate_read_methods(self, analysis: ProgramAnalysis) -> str:
        methods = []
        
        for f in analysis.files:
            if f.role not in [FileRole.MASTER_INPUT, FileRole.TRANSACTION_INPUT,
                              FileRole.LOOKUP_INPUT, FileRole.CONTROL_INPUT]:
                continue
            
            var_name = self._to_var_name(f.select_name)
            method_name = f"read{var_name[0].upper()}{var_name[1:]}File"
            
            method = f"""    private void {method_name}(ProgramState state) {{
        try {{
            if (state.{var_name}Reader == null) {{
                state.{var_name}Eof = true;
                return;
            }}
            String line = state.{var_name}Reader.readLine();
            if (line == null || line.trim().isEmpty()) {{
                state.{var_name}Eof = true;
                state.{var_name}RawLine = null;
            }} else {{
                state.{var_name}RawLine = line;
            }}
        }} catch (Exception e) {{
            throw new RuntimeException("Error reading {var_name} file", e);
        }}
    }}"""
            methods.append(method)
        
        return "\n\n".join(methods)
    
    def _generate_write_methods(self, analysis: ProgramAnalysis) -> str:
        methods = []
        
        for f in analysis.files:
            if f.role not in [FileRole.MASTER_OUTPUT, FileRole.REPORT_OUTPUT,
                              FileRole.REJECT_OUTPUT, FileRole.SYSOUT]:
                continue
            
            var_name = self._to_var_name(f.select_name)
            method_name = f"write{var_name[0].upper()}{var_name[1:]}Line"
            
            method = f"""    private void {method_name}(ProgramState state, String line) {{
        if (state.{var_name}Writer == null || line == null) return;
        try {{
            state.{var_name}Writer.write(line);
            state.{var_name}Writer.newLine();
        }} catch (Exception e) {{
            throw new RuntimeException("Error writing {var_name} file", e);
        }}
    }}"""
            methods.append(method)
        
        return "\n\n".join(methods)
    
    def _generate_main_process(self, analysis: ProgramAnalysis) -> str:
        if analysis.pattern == ProgramPattern.TWO_WAY_MERGE:
            return self._generate_merge_main_process(analysis)
        elif analysis.pattern == ProgramPattern.SUMMARIZATION:
            return self._generate_summarize_main_process(analysis)
        elif analysis.pattern == ProgramPattern.MULTI_INPUT:
            return self._generate_multi_input_main_process(analysis)
        else:
            return self._generate_sequential_main_process(analysis)
    
    def _generate_merge_main_process(self, analysis: ProgramAnalysis) -> str:
        # Find master and transaction files
        master = next((f for f in analysis.files if f.role == FileRole.MASTER_INPUT), None)
        trans = next((f for f in analysis.files if f.role == FileRole.TRANSACTION_INPUT), None)
        output = next((f for f in analysis.files if f.role == FileRole.MASTER_OUTPUT), None)
        
        master_var = self._to_var_name(master.select_name) if master else "master"
        trans_var = self._to_var_name(trans.select_name) if trans else "transaction"
        output_var = self._to_var_name(output.select_name) if output else "masterOut"
        
        return f"""    private void mainProcess(ProgramState state) {{
        // Prime reads
        if (!state.{master_var}Eof && state.{master_var}RawLine == null) {{
            read{master_var[0].upper()}{master_var[1:]}File(state);
        }}
        if (!state.{trans_var}Eof && state.{trans_var}RawLine == null) {{
            read{trans_var[0].upper()}{trans_var[1:]}File(state);
        }}

        // Two-way merge loop
        while (!state.{master_var}Eof) {{
            state.deleteIndicator = null;
            state.{output_var}OutLine = null;

            // Pass-through headers/trailers
            if (isHeaderOrTrailer(state)) {{
                state.{output_var}OutLine = state.{master_var}RawLine;
                write{output_var[0].upper()}{output_var[1:]}Line(state, state.{output_var}OutLine);
                read{master_var[0].upper()}{master_var[1:]}File(state);
                continue;
            }}

            // If transaction exhausted, copy master through
            if (state.{trans_var}Eof || state.{trans_var}RawLine == null) {{
                prepareOutputFromMaster(state);
                write{output_var[0].upper()}{output_var[1:]}Line(state, state.{output_var}OutLine);
                read{master_var[0].upper()}{master_var[1:]}File(state);
                continue;
            }}

            int cmp = compareKeys(state);

            if (cmp == 0) {{
                // MATCH: set flag, advance both
                setMatchFlag(state);
                prepareOutputFromMaster(state);
                write{output_var[0].upper()}{output_var[1:]}Line(state, state.{output_var}OutLine);
                read{master_var[0].upper()}{master_var[1:]}File(state);
                read{trans_var[0].upper()}{trans_var[1:]}File(state);
            }} else if (cmp < 0) {{
                // Master < Transaction: write master, advance master
                prepareOutputFromMaster(state);
                write{output_var[0].upper()}{output_var[1:]}Line(state, state.{output_var}OutLine);
                read{master_var[0].upper()}{master_var[1:]}File(state);
            }} else {{
                // Master > Transaction: advance transaction
                read{trans_var[0].upper()}{trans_var[1:]}File(state);
            }}
        }}
    }}"""
    
    def _generate_summarize_main_process(self, analysis: ProgramAnalysis) -> str:
        # Find first input file (prefer MASTER_INPUT, fallback to TRANSACTION_INPUT)
        primary = next((f for f in analysis.files if f.role == FileRole.MASTER_INPUT), None)
        if not primary:
            primary = next((f for f in analysis.files if f.role == FileRole.TRANSACTION_INPUT), None)
        if not primary:
            primary = next((f for f in analysis.files if f.role in [FileRole.MASTER_INPUT, FileRole.TRANSACTION_INPUT, FileRole.LOOKUP_INPUT]), None)
        
        output = next((f for f in analysis.files if f.role in [FileRole.MASTER_OUTPUT, FileRole.REPORT_OUTPUT]), None)
        
        primary_var = self._to_var_name(primary.select_name) if primary else "input"
        output_var = self._to_var_name(output.select_name) if output else "output"
        
        return f"""    private void mainProcess(ProgramState state) {{
        // Prime read
        read{primary_var[0].upper()}{primary_var[1:]}File(state);

        String currentKey = null;

        while (!state.{primary_var}Eof) {{
            // Skip headers/trailers
            if (isHeaderOrTrailer(state)) {{
                read{primary_var[0].upper()}{primary_var[1:]}File(state);
                continue;
            }}

            String key = extractKey(state);

            if (currentKey == null) {{
                currentKey = key;
                initializeAccumulators(state);
            }}

            if (!key.equals(currentKey)) {{
                // Key break: write summary, reset
                writeSummary(state, currentKey);
                currentKey = key;
                initializeAccumulators(state);
            }}

            accumulate(state);
            read{primary_var[0].upper()}{primary_var[1:]}File(state);
        }}

        // Write final group
        if (currentKey != null) {{
            writeSummary(state, currentKey);
        }}
    }}"""
    
    def _generate_multi_input_main_process(self, analysis: ProgramAnalysis) -> str:
        input_files = [f for f in analysis.files 
                       if f.role in [FileRole.MASTER_INPUT, FileRole.TRANSACTION_INPUT]]
        
        lines = ["    private void mainProcess(ProgramState state) {"]
        
        # Process each input file
        for f in input_files:
            var_name = self._to_var_name(f.select_name)
            lines.append(f"        // Process {f.select_name}")
            lines.append(f"        process{var_name[0].upper()}{var_name[1:]}(state);")
            lines.append("")
        
        lines.append("    }")
        
        return "\n".join(lines)
    
    def _generate_sequential_main_process(self, analysis: ProgramAnalysis) -> str:
        master = next((f for f in analysis.files if f.role == FileRole.MASTER_INPUT), None)
        output = next((f for f in analysis.files if f.role == FileRole.MASTER_OUTPUT), None)
        
        master_var = self._to_var_name(master.select_name) if master else "master"
        output_var = self._to_var_name(output.select_name) if output else "output"
        
        return f"""    private void mainProcess(ProgramState state) {{
        // Prime read
        read{master_var[0].upper()}{master_var[1:]}File(state);

        while (!state.{master_var}Eof) {{
            processRecord(state);
            read{master_var[0].upper()}{master_var[1:]}File(state);
        }}
    }}"""
    
    def _generate_helper_methods(self, analysis: ProgramAnalysis) -> str:
        helpers = []
        
        # Common helpers - find the primary input file
        primary = next((f for f in analysis.files if f.role == FileRole.MASTER_INPUT), None)
        if not primary:
            primary = next((f for f in analysis.files if f.role == FileRole.TRANSACTION_INPUT), None)
        if not primary:
            primary = next((f for f in analysis.files if f.role in [FileRole.MASTER_INPUT, FileRole.TRANSACTION_INPUT, FileRole.LOOKUP_INPUT]), None)
        
        primary_var = self._to_var_name(primary.select_name) if primary else "input"
        output = next((f for f in analysis.files if f.role == FileRole.MASTER_OUTPUT), None)
        output_var = self._to_var_name(output.select_name) if output else "output"
        
        helpers.append(f"""    private boolean isHeaderOrTrailer(ProgramState state) {{
        if (state.{primary_var}RawLine == null) return false;
        return state.{primary_var}RawLine.startsWith("HDR") 
            || state.{primary_var}RawLine.startsWith("TRL")
            || state.{primary_var}RawLine.charAt(0) == '\\u0000'
            || state.{primary_var}RawLine.charAt(0) == '\\u00FF';
    }}""")
        
        if analysis.pattern == ProgramPattern.TWO_WAY_MERGE:
            trans = next((f for f in analysis.files if f.role == FileRole.TRANSACTION_INPUT), None)
            trans_var = self._to_var_name(trans.select_name) if trans else "transaction"
            
            helpers.append(f"""    private String masterKey(ProgramState state) {{
        if (state.{primary_var}RawLine == null) return null;
        // Extract key - customize based on record layout
        String[] parts = state.{primary_var}RawLine.split("\\\\|", -1);
        return parts.length > 0 ? parts[0].trim() : null;
    }}

    private String transactionKey(ProgramState state) {{
        if (state.{trans_var}RawLine == null) return null;
        return state.{trans_var}RawLine.trim();
    }}

    private int compareKeys(ProgramState state) {{
        String mk = masterKey(state);
        String tk = transactionKey(state);
        if (mk == null && tk == null) return 0;
        if (mk == null) return -1;
        if (tk == null) return 1;
        return mk.compareTo(tk);
    }}

    private void setMatchFlag(ProgramState state) {{
        state.deleteIndicator = "M";
    }}

    private void prepareOutputFromMaster(ProgramState state) {{
        state.{output_var}OutLine = state.{primary_var}RawLine;
        if ("M".equals(state.deleteIndicator)) {{
            if (!state.{output_var}OutLine.endsWith("|")) {{
                state.{output_var}OutLine = state.{output_var}OutLine + "|";
            }}
            state.{output_var}OutLine = state.{output_var}OutLine + "M";
        }}
    }}""")
        
        elif analysis.pattern == ProgramPattern.SUMMARIZATION:
            # Use the primary input variable (same logic as main process)
            primary = next((f for f in analysis.files if f.role == FileRole.MASTER_INPUT), None)
            if not primary:
                primary = next((f for f in analysis.files if f.role == FileRole.TRANSACTION_INPUT), None)
            if not primary:
                primary = next((f for f in analysis.files if f.role in [FileRole.MASTER_INPUT, FileRole.TRANSACTION_INPUT, FileRole.LOOKUP_INPUT]), None)
            primary_var = self._to_var_name(primary.select_name) if primary else "input"
            
            helpers.append(f"""    private String extractKey(ProgramState state) {{
        if (state.{primary_var}RawLine == null) return "";
        // Extract grouping key - customize based on record layout
        String[] parts = state.{primary_var}RawLine.split("\\\\|", -1);
        return parts.length > 0 ? parts[0].trim() : "";
    }}

    private void initializeAccumulators(ProgramState state) {{
        state.totalAmount = java.math.BigDecimal.ZERO;
        state.recordCount = 0;
    }}

    private void accumulate(ProgramState state) {{
        state.recordCount++;
        // TODO: Add amount accumulation based on record layout
    }}

    private void writeSummary(ProgramState state, String key) {{
        // TODO: Format and write summary record
    }}""")
        
        elif analysis.pattern == ProgramPattern.MULTI_INPUT:
            for f in analysis.files:
                if f.role in [FileRole.MASTER_INPUT, FileRole.TRANSACTION_INPUT]:
                    var_name = self._to_var_name(f.select_name)
                    helpers.append(f"""    private void process{var_name[0].upper()}{var_name[1:]}(ProgramState state) {{
        read{var_name[0].upper()}{var_name[1:]}File(state);
        while (!state.{var_name}Eof) {{
            // TODO: Process record from {f.select_name}
            read{var_name[0].upper()}{var_name[1:]}File(state);
        }}
    }}""")
        
        else:
            helpers.append(f"""    private void processRecord(ProgramState state) {{
        // TODO: Transform and write record
    }}""")
        
        return "\n\n".join(helpers)
    
    def _to_var_name(self, select_name: str) -> str:
        """Convert SELECT name to Java variable name."""
        # Remove common prefixes
        name = re.sub(r"^(TEN99|CCAC|CC|WS)-", "", select_name)
        # Convert to camelCase
        parts = name.replace("_", "-").split("-")
        return parts[0].lower() + "".join(p.capitalize() for p in parts[1:])
    
    def _to_file_name(self, select_name: str, role: FileRole) -> str:
        """Generate testcase file name from SELECT name and role."""
        role_names = {
            FileRole.MASTER_INPUT: "master",
            FileRole.TRANSACTION_INPUT: "corporate",
            FileRole.CONTROL_INPUT: "control",
            FileRole.LOOKUP_INPUT: "lookup",
            FileRole.MASTER_OUTPUT: "master_out",
            FileRole.REPORT_OUTPUT: "report",
            FileRole.REJECT_OUTPUT: "reject",
            FileRole.SYSOUT: "sysout",
        }
        return f"{role_names.get(role, 'data')}.txt"


# =============================================================================
# LAYER G.1: TESTCASE STRUCTURE
# =============================================================================

class LayerG1_TestcaseStructure(Layer):
    """Create testcase directory structure."""
    
    name = "Layer G.1: Testcase Structure"
    
    def run(self, program: Optional[str] = None, analysis: Optional[ProgramAnalysis] = None) -> None:
        if not program or not analysis:
            raise ValueError("Layer G.1 requires --program and analysis")
        
        print(f"\n=== {self.name} ===\n")
        
        testcase_dir = self._ensure_dir(self.config.testcase_dir / program)
        input_dir = self._ensure_dir(testcase_dir / "input")
        output_dir = self._ensure_dir(testcase_dir / "output")
        expected_dir = self._ensure_dir(testcase_dir / "expected")
        
        # Create placeholder input files
        for f in analysis.files:
            if f.role in [FileRole.MASTER_INPUT, FileRole.TRANSACTION_INPUT,
                          FileRole.CONTROL_INPUT, FileRole.LOOKUP_INPUT]:
                file_name = self._get_file_name(f.role)
                placeholder = input_dir / file_name
                if not placeholder.exists():
                    placeholder.write_text("# Placeholder - add test data\n", encoding="utf-8")
                    print(f"  → Created input placeholder: {file_name}")
        
        # Create placeholder expected files
        for f in analysis.files:
            if f.role in [FileRole.MASTER_OUTPUT, FileRole.REPORT_OUTPUT,
                          FileRole.REJECT_OUTPUT, FileRole.SYSOUT]:
                file_name = self._get_file_name(f.role)
                placeholder = expected_dir / file_name
                if not placeholder.exists():
                    placeholder.write_text("", encoding="utf-8")
                    print(f"  → Created expected placeholder: {file_name}")
        
        print(f"\n✔ {self.name} complete\n")
    
    def _get_file_name(self, role: FileRole) -> str:
        names = {
            FileRole.MASTER_INPUT: "master.txt",
            FileRole.TRANSACTION_INPUT: "corporate.txt",
            FileRole.CONTROL_INPUT: "control.txt",
            FileRole.LOOKUP_INPUT: "lookup.txt",
            FileRole.MASTER_OUTPUT: "master_out.txt",
            FileRole.REPORT_OUTPUT: "report.txt",
            FileRole.REJECT_OUTPUT: "reject.txt",
            FileRole.SYSOUT: "sysout.txt",
        }
        return names.get(role, "data.txt")


# =============================================================================
# LAYER G.3: GENERATE TEST RUNNER
# =============================================================================

class LayerG3_GenerateRunner(Layer):
    """Generate test runner class."""
    
    name = "Layer G.3: Generate Runner"
    
    def run(self, program: Optional[str] = None, analysis: Optional[ProgramAnalysis] = None) -> None:
        if not program:
            raise ValueError("Layer G.3 requires --program")
        
        print(f"\n=== {self.name} ===\n")
        
        runner_dir = self._ensure_dir(
            self.config.java_base / "com/fordcredit/misc1099/batch/runner"
        )
        
        runner_code = self._render_runner(program)
        out_path = runner_dir / f"{program}Runner.java"
        out_path.write_text(runner_code, encoding="utf-8")
        
        # Create output manifest
        outputs = {}
        if analysis:
            for f in analysis.files:
                if f.role in [FileRole.MASTER_OUTPUT, FileRole.REPORT_OUTPUT,
                              FileRole.REJECT_OUTPUT, FileRole.SYSOUT]:
                    file_name = self._get_file_name(f.role)
                    outputs[file_name.replace(".txt", "")] = str(
                        self.config.testcase_dir / program / "output" / file_name
                    )
        else:
            outputs = {
                "master_out": str(self.config.testcase_dir / program / "output/master_out.txt"),
                "sysout": str(self.config.testcase_dir / program / "output/sysout.txt"),
            }
        
        manifest = {
            "program": program,
            "outputDir": str(self.config.testcase_dir / program / "output"),
            "outputs": outputs,
        }
        
        runtime_dir = self._ensure_dir(self.config.runtime_dir)
        manifest_path = runtime_dir / f"{program}.outputs.json"
        manifest_path.write_text(json.dumps(manifest, indent=2), encoding="utf-8")
        
        print(f"  → Runner: {out_path}")
        print(f"  → Manifest: {manifest_path}")
        print(f"\n✔ {self.name} complete\n")
    
    def _render_runner(self, program: str) -> str:
        pkg = f"{self.config.base_package}.batch.runner"
        tasklet_pkg = f"{self.config.base_package}.batch.program"
        
        return f"""\
package {pkg};

/**
 * Standalone runner for {program}Tasklet.
 * Used for golden-master testing outside Spring context.
 */
public class {program}Runner {{

    public static void main(String[] args) {{
        System.out.println("▶ Running {program}Tasklet...");
        
        try {{
            {tasklet_pkg}.{program}Tasklet tasklet = new {tasklet_pkg}.{program}Tasklet();
            tasklet.execute(null, null);
            System.out.println("✔ {program}Tasklet completed successfully");
        }} catch (Exception e) {{
            System.err.println("✖ {program}Tasklet failed: " + e.getMessage());
            e.printStackTrace();
            System.exit(1);
        }}
    }}
}}
"""
    
    def _get_file_name(self, role: FileRole) -> str:
        names = {
            FileRole.MASTER_OUTPUT: "master_out.txt",
            FileRole.REPORT_OUTPUT: "report.txt",
            FileRole.REJECT_OUTPUT: "reject.txt",
            FileRole.SYSOUT: "sysout.txt",
        }
        return names.get(role, "output.txt")


# =============================================================================
# PIPELINE ORCHESTRATOR
# =============================================================================

class Pipeline:
    """Orchestrates the migration pipeline."""
    
    def __init__(self, config: Config):
        self.config = config
        self.llm = LLMClient(config)
        self.analyzer = PatternAnalyzer(config)
        
        # Foundation layers
        self.foundation_layers: List[Layer] = [
            Layer1_CopybooksToPojos(config, self.llm),
            Layer1_5_CopybooksToFieldSpecs(config, self.llm),
            Layer2_JclToSpringBatch(config, self.llm),
        ]
    
    def run_foundation(self) -> None:
        """Run foundation layers (copybooks, JCL conversion)."""
        print("\n" + "=" * 60)
        print("FOUNDATION LAYERS")
        print("=" * 60)
        
        for layer in self.foundation_layers:
            layer.run()
    
    def run_program(self, program: str) -> ProgramAnalysis:
        """Run program-specific layers."""
        print("\n" + "=" * 60)
        print(f"PROGRAM LAYERS: {program}")
        print("=" * 60)
        
        # Layer 3A: Analyze and generate IR
        layer_3a = Layer3A_CobolToIR(self.config, self.llm, self.analyzer)
        analysis = layer_3a.run(program)
        
        # Layer 3B: Generate Tasklet skeleton
        layer_3b = Layer3B_TaskletSkeleton(self.config)
        layer_3b.run(program, analysis)
        
        # Layer G.1: Create testcase structure
        layer_g1 = LayerG1_TestcaseStructure(self.config)
        layer_g1.run(program, analysis)
        
        # Layer G.3: Generate runner
        layer_g3 = LayerG3_GenerateRunner(self.config)
        layer_g3.run(program, analysis)
        
        return analysis
    
    def run_all_programs(self) -> None:
        """Run pipeline for all COBOL programs."""
        programs = self._discover_programs()
        
        print("\n" + "=" * 60)
        print(f"MIGRATING {len(programs)} PROGRAMS")
        print("=" * 60)
        
        for program in programs:
            try:
                self.run_program(program)
            except Exception as e:
                print(f"✖ Error migrating {program}: {e}")
    
    def run_test(self, program: str) -> bool:
        """Execute and compare test results."""
        print("\n" + "=" * 60)
        print(f"TEST EXECUTION: {program}")
        print("=" * 60)
        
        java_project = self.config.root / "misc-1099"
        
        result = subprocess.run(
            [
                "./mvnw", "-q", "-DskipTests", "exec:java",
                f"-Dexec.mainClass=com.fordcredit.misc1099.batch.runner.{program}Runner",
            ],
            cwd=java_project,
        )
        
        if result.returncode != 0:
            print("✖ Java execution failed")
            return False
        
        print("✔ Java execution completed")
        
        # Compare outputs
        testcase_dir = self.config.testcase_dir / program
        expected_dir = testcase_dir / "expected"
        output_dir = testcase_dir / "output"
        
        if not expected_dir.exists():
            print("⚠ No expected directory found")
            return True
        
        mismatches = 0
        for expected_file in expected_dir.glob("*"):
            output_file = output_dir / expected_file.name
            
            if not output_file.exists():
                print(f"✖ Missing output: {expected_file.name}")
                mismatches += 1
                continue
            
            expected_lines = expected_file.read_text().splitlines()
            output_lines = output_file.read_text().splitlines()
            
            if expected_lines == output_lines:
                print(f"✔ {expected_file.name} matches")
            else:
                print(f"✖ {expected_file.name} mismatch")
                mismatches += 1
        
        return mismatches == 0
    
    def _discover_programs(self) -> List[str]:
        """Discover all COBOL programs in work folder."""
        programs = set()
        
        for ext in ["*.txt", "*.cbl", "*.cob"]:
            for path in self.config.cobol_dir.glob(ext):
                programs.add(path.stem.upper())
        
        return sorted(programs)


# =============================================================================
# MAIN
# =============================================================================

def main():
    parser = argparse.ArgumentParser(
        description="COBOL to Spring Boot Migration Pipeline (Generic Factory)",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  python cobol_to_springboot.py --program CCAC6340        # Migrate one program
  python cobol_to_springboot.py --all                     # Migrate ALL programs
  python cobol_to_springboot.py --foundation-only         # Only copybooks & JCL
  python cobol_to_springboot.py --program CCAC6340 --test # With test execution
  python cobol_to_springboot.py --list                    # List all programs
        """
    )
    
    parser.add_argument(
        "--program", "-p",
        help="Program ID to migrate (e.g., CCAC6340)"
    )
    parser.add_argument(
        "--all", "-a",
        action="store_true",
        help="Migrate ALL programs in work folder"
    )
    parser.add_argument(
        "--foundation-only",
        action="store_true",
        help="Run only foundation layers (copybooks, JCL)"
    )
    parser.add_argument(
        "--skip-foundation",
        action="store_true",
        help="Skip foundation layers"
    )
    parser.add_argument(
        "--test", "-t",
        action="store_true",
        help="Run tests after migration"
    )
    parser.add_argument(
        "--list", "-l",
        action="store_true",
        help="List all discovered programs"
    )
    
    args = parser.parse_args()
    
    # Initialize
    config = Config()
    
    # List mode
    if args.list:
        pipeline = Pipeline(config)
        programs = pipeline._discover_programs()
        print("\nDiscovered COBOL programs:")
        for p in programs:
            print(f"  - {p}")
        print(f"\nTotal: {len(programs)} programs")
        return
    
    # Validate arguments
    if not args.foundation_only and not args.program and not args.all:
        parser.error("Specify --program, --all, or --foundation-only")
    
    config.validate()
    
    pipeline = Pipeline(config)
    
    print("\n" + "=" * 60)
    print("COBOL TO SPRING BOOT MIGRATION PIPELINE")
    print("=" * 60)
    
    # Run foundation layers
    if not args.skip_foundation and (args.foundation_only or args.all or 
                                      (args.program and not args.skip_foundation)):
        pipeline.run_foundation()
    
    if args.foundation_only:
        print("\n" + "=" * 60)
        print("FOUNDATION COMPLETE")
        print("=" * 60 + "\n")
        return
    
    # Run program layers
    if args.all:
        pipeline.run_all_programs()
    elif args.program:
        program = args.program.upper()
        pipeline.run_program(program)
        
        if args.test:
            success = pipeline.run_test(program)
            sys.exit(0 if success else 1)
    
    print("\n" + "=" * 60)
    print("MIGRATION COMPLETE")
    print("=" * 60 + "\n")


if __name__ == "__main__":
    main()
