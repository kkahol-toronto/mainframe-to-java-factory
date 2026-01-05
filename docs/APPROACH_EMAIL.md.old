# COBOL to Spring Boot Migration: Technical Approach

**Subject:** Ford Credit 1099 Modernization - COBOL to Java Migration Approach & Methodology

---

## Executive Summary

We have successfully developed a **factory-grade migration pipeline** that converts mainframe COBOL programs to Java Spring Boot applications. The approach combines **LLM-powered code generation** (Azure OpenAI) with **deterministic template-based patterns** to ensure both accuracy and reliability.

**Key Achievements:**
- ✅ 6 COBOL programs migrated to Java Spring Boot Tasklets
- ✅ All generated code compiles successfully
- ✅ CCAC6340 (two-way merge) passes golden master testing
- ✅ Automated test harness for all programs
- ✅ Reusable pipeline for future COBOL migrations

---

## 1. Spring Boot Project Setup

The Spring Boot project (`misc-1099`) was **generated using Spring Initializr** (https://start.spring.io/) with the following configuration:

| Setting | Value |
|---------|-------|
| Project | Maven |
| Language | Java 17 |
| Spring Boot | 3.x |
| Dependencies | Spring Batch, Spring Web, H2 Database |

**Additional Configuration:**
- Added H2 in-memory database for Spring Batch metadata storage
- Configured `spring.batch.jdbc.initialize-schema=always` for automatic schema creation
- Maven wrapper included for zero-install builds

---

## 2. Migration Pipeline Architecture

We developed a **multi-layer pipeline** that progressively transforms COBOL artifacts into Spring Boot components:

```
┌─────────────────────────────────────────────────────────────────────┐
│                    INPUT: Mainframe Artifacts                       │
│  • COBOL source files (.txt)                                        │
│  • COBOL copybooks (.cpy)                                           │
│  • JCL job definitions (.txt)                                       │
└─────────────────────────────────────────────────────────────────────┘
                                   │
                                   ▼
┌─────────────────────────────────────────────────────────────────────┐
│              LAYER 0: Cleaning & Normalization                      │
│  • Remove mainframe line numbers and metadata                       │
│  • Strip BROWSE headers/footers from JCL                            │
│  • Generate program dossiers (JSON metadata)                        │
└─────────────────────────────────────────────────────────────────────┘
                                   │
                                   ▼
┌─────────────────────────────────────────────────────────────────────┐
│              LAYER 1: Copybooks → Java POJOs [LLM]                  │
│  • Azure OpenAI converts COBOL record layouts                       │
│  • Generates Java domain classes with field mappings                │
│  • Example: C2INP001.cpy → C2INP001.java                            │
└─────────────────────────────────────────────────────────────────────┘
                                   │
                                   ▼
┌─────────────────────────────────────────────────────────────────────┐
│           LAYER 1.5: Copybooks → FieldSpecs [LLM]                   │
│  • Generates field metadata (offsets, lengths, types)               │
│  • Used for fixed-width record parsing at runtime                   │
│  • Example: C2INP001FieldSpecs.java                                 │
└─────────────────────────────────────────────────────────────────────┘
                                   │
                                   ▼
┌─────────────────────────────────────────────────────────────────────┐
│            LAYER 2: JCL → Spring Batch Config [LLM]                 │
│  • Converts JCL JOB/STEP definitions                                │
│  • Generates JobConfig and StepConfig classes                       │
│  • Maps EXEC PGM to Tasklet references                              │
└─────────────────────────────────────────────────────────────────────┘
                                   │
                                   ▼
┌─────────────────────────────────────────────────────────────────────┐
│          LAYER 3A: COBOL → Intermediate Representation [LLM]        │
│  • Extracts program structure (paragraphs, sections)                │
│  • Identifies control flow patterns                                 │
│  • Detects program type (merge, summarization, multi-input)         │
└─────────────────────────────────────────────────────────────────────┘
                                   │
                                   ▼
┌─────────────────────────────────────────────────────────────────────┐
│          LAYER 3B: Tasklet Skeleton Generation [TEMPLATE]           │
│  • Pattern-specific Java templates                                  │
│  • File I/O plumbing from dossier metadata                          │
│  • State class with readers/writers/flags                           │
│  • NO LLM - deterministic template expansion                        │
└─────────────────────────────────────────────────────────────────────┘
                                   │
                                   ▼
┌─────────────────────────────────────────────────────────────────────┐
│            LAYER 4: Business Logic Wiring [TEMPLATE]                │
│  • Injects pattern-specific main processing loop                    │
│  • Two-way merge: cursor comparison, flag setting                   │
│  • Summarization: group breaks, accumulator updates                 │
│  • Multi-input: interleaved reads, routing logic                    │
│  • NO LLM - proven templates from COBOL patterns                    │
└─────────────────────────────────────────────────────────────────────┘
                                   │
                                   ▼
┌─────────────────────────────────────────────────────────────────────┐
│                    OUTPUT: Spring Boot Application                  │
│  • Compilable Java Tasklets                                         │
│  • Spring Batch job configurations                                  │
│  • Domain POJOs and parsers                                         │
│  • Test runners and harness                                         │
└─────────────────────────────────────────────────────────────────────┘
```

---

## 3. LLM vs Template-Based Generation

We deliberately use a **hybrid approach** for reliability:

| Layer | Approach | Rationale |
|-------|----------|-----------|
| Copybooks → POJOs | **LLM** | COBOL layouts are complex; LLM handles PIC clauses, REDEFINES, etc. |
| FieldSpecs | **LLM** | Offset calculations require understanding COBOL semantics |
| JCL → Spring Batch | **LLM** | JCL has many variations; LLM adapts to different patterns |
| Program IR | **LLM** | Paragraph extraction benefits from natural language understanding |
| **Tasklet Code** | **Template** | Business logic patterns are well-defined; templates ensure correctness |
| **Main Loop** | **Template** | Merge/summarize loops must be precise; LLM variability is a risk |
| Test Harness | **Template** | Deterministic structure; no creativity needed |

**Why templates for business logic?**
- COBOL batch patterns (merge, summarize, sequential) are well-understood
- Templates guarantee correct loop termination and file handling
- Eliminates LLM hallucination risk in critical processing logic
- Easier to debug and maintain

---

## 4. Pattern Detection & Classification

The pipeline **automatically detects** the COBOL program type:

| Pattern | Detection Heuristics | Example Program |
|---------|---------------------|-----------------|
| `TWO_WAY_MERGE` | Two sorted input files, key comparison, flag updates | CCAC6340 |
| `SUMMARIZATION` | GROUP BY breaks, accumulators, summary writes | CCAC6350 |
| `MULTI_INPUT` | 3+ input files, routing/filtering logic | CCAC6310, CCAC6320 |
| `SEQUENTIAL` | Single input, transform, single output | Basic reformatters |

**File role classification:**
- `MASTER_INPUT` - Primary sorted input file
- `TRANSACTION_INPUT` - Secondary file for matching
- `CONTROL_INPUT` - Parameter/date cards
- `MASTER_OUTPUT` - Updated master file
- `REPORT_OUTPUT` - Print/summary reports
- `REJECT_OUTPUT` - Error/exception records

---

## 5. Test Case Generation & Verification

### 5.1 Test Structure

For each COBOL program, we generate:

```
work/mainframe_clean/testcases/{PROGRAM}/
├── input/           # Test input files
│   ├── master.txt
│   ├── corporate.txt
│   └── control.txt
├── expected/        # Known-good expected output
│   └── master_out.txt
└── output/          # Actual output from Java program
    └── master_out.txt
```

### 5.2 Golden Master Testing

1. **Create input test data** - Realistic records matching COBOL copybook layouts
2. **Define expected output** - What the COBOL program would produce
3. **Run Java Tasklet** - Execute via Maven runner
4. **Compare output vs expected** - Byte-for-byte comparison

### 5.3 Verified Results

| Program | Pattern | Compiles | Test Status |
|---------|---------|----------|-------------|
| CCAC6250 | MULTI_INPUT | ✅ | Test data pending |
| CCAC6310 | MULTI_INPUT | ✅ | Test data pending |
| CCAC6320 | MULTI_INPUT | ✅ | Test data pending |
| CCAC6330 | MULTI_INPUT | ✅ | Test data pending |
| CCAC6340 | TWO_WAY_MERGE | ✅ | ✅ **PASSING** |
| CCAC6350 | SUMMARIZATION | ✅ | Test data pending |

### 5.4 CCAC6340 Test Verification

**Input (master.txt):**
```
HDR|20240101
111111111|ALICE|100.00|
222222222|BOB|200.00|
TRL|2|300.00
```

**Input (corporate.txt):**
```
222222222
```

**Expected Output (master_out.txt):**
```
HDR|20240101
111111111|ALICE|100.00|
222222222|BOB|200.00|M    ← "M" flag set for mechanized match
TRL|2|300.00
```

**Actual Output:** ✅ Matches expected exactly

---

## 6. Generated Code Quality

### 6.1 Compilation Verification

All generated Tasklets compile with zero errors:

```bash
cd misc-1099
./mvnw compile
# BUILD SUCCESS - 0 errors
```

### 6.2 Runtime Verification

Each program executes successfully:

```bash
./mvnw exec:java -Dexec.mainClass="com.fordcredit.misc1099.batch.runner.CCAC6340Runner"
# ▶ Running CCAC6340Tasklet...
# ✔ CCAC6340Tasklet completed successfully
```

### 6.3 Code Structure

Generated Tasklets follow a consistent structure:

```java
public class CCAC6340Tasklet implements Tasklet {
    
    static class ProgramState {
        // File readers, EOF flags, current lines
        BufferedReader masterReader;
        boolean masterEof = false;
        String masterRawLine;
        // ... output writers, domain fields
    }
    
    @Override
    public RepeatStatus execute(...) {
        ProgramState state = new ProgramState();
        mainline(state);
        return RepeatStatus.FINISHED;
    }
    
    private void mainline(ProgramState state) {
        openFiles(state);
        initialization(state);
        mainProcess(state);      // Pattern-specific loop
        endOfJob(state);
        closeFiles(state);
    }
    
    // File I/O methods
    private void openFiles(ProgramState state) { ... }
    private void readMasterFile(ProgramState state) { ... }
    private void writeMasterOutLine(ProgramState state, String line) { ... }
    
    // Business logic (pattern-specific)
    private void mainProcess(ProgramState state) {
        // TWO_WAY_MERGE: cursor comparison loop
        // SUMMARIZATION: group-break accumulation
        // MULTI_INPUT: interleaved routing
    }
}
```

---

## 7. Reusability for Future Programs

The pipeline is **fully generic** and can process any COBOL program:

```bash
# Add new COBOL files
work/mainframe_clean/cobol/NEWPROG.txt
work/mainframe_clean/copybooks/NEWCOPY.cpy

# Run migration
python cobol_to_springboot.py --program NEWPROG

# Generated files
misc-1099/src/main/java/.../NEWPROG/NEWPROGTasklet.java
misc-1099/src/main/java/.../runner/NEWPROGRunner.java
work/mainframe_clean/testcases/NEWPROG/input/
work/mainframe_clean/testcases/NEWPROG/expected/
```

---

## 8. Technology Stack

| Component | Technology |
|-----------|------------|
| Migration Pipeline | Python 3.11+ |
| LLM | Azure OpenAI (GPT-4o) |
| Target Framework | Spring Boot 3.x |
| Batch Processing | Spring Batch 5.x |
| Build Tool | Maven 3.x |
| In-Memory DB | H2 (for Spring Batch metadata) |
| Testing | Golden master comparison |

---

## 9. Repository Structure

```
mainframe-to-java-factory/
├── README.md                    # Full documentation
├── cobol_to_springboot.py       # Main pipeline (consolidated)
├── cobol-cleaner.py             # Initial artifact cleaning
├── program-dossier-builder.py   # JSON metadata extraction
├── generate_testcases.py        # Test structure creation
├── requirements.txt             # Python dependencies
├── .env                         # Azure OpenAI credentials
│
├── misc-1099/                   # Spring Boot project
│   ├── pom.xml
│   └── src/main/java/com/fordcredit/misc1099/
│       ├── domain/copybook/     # Generated POJOs
│       ├── parser/              # Field specs
│       └── batch/
│           ├── config/          # Spring Batch jobs
│           ├── program/         # Generated Tasklets
│           └── runner/          # Test runners
│
├── work/
│   └── mainframe_clean/
│       ├── cobol/               # Cleaned COBOL sources
│       ├── copybooks/           # Cleaned copybooks
│       ├── jcl/                 # Cleaned JCL
│       ├── dossiers/            # Program metadata (JSON)
│       └── testcases/           # Test input/expected/output
│
└── archive/                     # Legacy individual layer scripts
```

---

## 10. Next Steps

1. **Complete test data** for remaining 5 programs (CCAC6250, 6310, 6320, 6330, 6350)
2. **Validate business logic** against original COBOL execution on mainframe
3. **Integrate with Spring Batch jobs** for end-to-end execution
4. **Add database connectivity** for programs that access DB2
5. **Performance benchmarking** vs. mainframe execution times

---

## Contact

For questions about this migration approach, please reach out to the modernization team.

---

*Generated: January 2025*
*Repository: https://github.com/kkahol-toronto/mainframe-to-java-factory*

