# COBOL to Spring Boot Migration Factory

A factory-grade migration tool that converts mainframe COBOL programs to Java Spring Boot applications. Automatically detects program patterns and generates appropriate code.

## Architecture

```
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
           │   (LLM)     │   │     (LLM)       │   │     (LLM)       │
           └─────────────┘   └─────────────────┘   └─────────────────┘
                    │                  │                      │
                    └──────────────────┼──────────────────────┘
                                       ▼
                    ┌─────────────────────────────────────────┐
                    │     Layer 3A: Program IR (Analysis)     │
                    │  - Pattern Detection (LLM-assisted)     │
                    │  - File Role Classification             │
                    │  - Control Flow Analysis                │
                    └─────────────────────────────────────────┘
                                       │
                                       ▼
                    ┌─────────────────────────────────────────┐
                    │     Layer 3B: Tasklet Generation        │
                    │  - Pattern-specific templates           │
                    │  - File I/O plumbing                    │
                    │  - State class generation               │
                    │       (TEMPLATE-BASED, NO LLM)          │
                    └─────────────────────────────────────────┘
                                       │
                                       ▼
                    ┌─────────────────────────────────────────┐
                    │     Layer 4: Business Logic Wiring      │
                    │  - Merge loops, summarization, etc.     │
                    │  - Helper methods injection             │
                    │       (TEMPLATE-BASED, NO LLM)          │
                    └─────────────────────────────────────────┘
                                       │
                                       ▼
                    ┌─────────────────────────────────────────┐
                    │     Layer G: Test Harness Generation    │
                    │  - Test runner class                    │
                    │  - Testcase directory structure         │
                    │  - Input/expected/output files          │
                    └─────────────────────────────────────────┘
```

## Supported Patterns

| Pattern | Description | Example |
|---------|-------------|---------|
| `TWO_WAY_MERGE` | Master/transaction merge with flag updates | CCAC6340 |
| `SUMMARIZATION` | Group-by and aggregate records | CCAC6350 |
| `MULTI_INPUT` | Multiple input files → single output | CCAC6310, CCAC6320 |
| `SEQUENTIAL` | Simple read → transform → write | Basic file processing |

## Prerequisites

### 1. Python Environment

```bash
# Create virtual environment
python3 -m venv venv
source venv/bin/activate

# Install dependencies
pip install -r requirements.txt
```

### 2. Azure OpenAI Credentials

Create a `.env` file in the project root:

```env
AZURE_OPENAI_API_KEY=your-api-key-here
AZURE_OPENAI_ENDPOINT=https://your-resource.openai.azure.com/
AZURE_OPENAI_DEPLOYMENT=gpt-4o
```

### 3. Java Environment

```bash
# Verify Java 17+ is installed
java -version

# The Spring Boot project uses Maven wrapper (no install needed)
```

## Directory Structure

```
1099-converted/
├── cobol_to_springboot.py      # Main migration pipeline
├── requirements.txt            # Python dependencies
├── .env                        # Azure OpenAI credentials (not in git)
│
├── work/
│   └── mainframe_clean/
│       ├── cobol/              # Cleaned COBOL source files
│       │   ├── CCAC6250.txt
│       │   ├── CCAC6310.txt
│       │   └── ...
│       ├── copybooks/          # Cleaned COBOL copybooks
│       │   ├── C2INP001.cpy
│       │   └── ...
│       ├── jcl/                # Cleaned JCL files
│       ├── dossiers/           # Generated JSON metadata
│       └── testcases/          # Test input/expected/output
│           ├── CCAC6340/
│           │   ├── input/
│           │   ├── expected/
│           │   └── output/
│           └── ...
│
└── misc-1099/                  # Spring Boot project
    ├── pom.xml
    └── src/main/java/com/fordcredit/misc1099/
        ├── domain/copybook/    # Generated POJOs (Layer 1)
        ├── parser/             # Generated field specs (Layer 1.5)
        └── batch/
            ├── config/         # Spring Batch job configs (Layer 2)
            ├── program/        # Generated Tasklets (Layer 3B/4)
            │   ├── CCAC6340Tasklet.java
            │   └── ...
            └── runner/         # Test runners (Layer G)
```

## Usage

### Migrate a Single Program

```bash
source venv/bin/activate
python cobol_to_springboot.py --program CCAC6340
```

### Migrate All Programs

```bash
python cobol_to_springboot.py --all
```

### Foundation Only (Copybooks + JCL)

```bash
python cobol_to_springboot.py --foundation-only
```

### Skip Foundation (Program Only)

```bash
python cobol_to_springboot.py --program CCAC6340 --skip-foundation
```

### Run with Tests

```bash
python cobol_to_springboot.py --program CCAC6340 --test
```

### List All Available Programs

```bash
python cobol_to_springboot.py --list
```

## Running Generated Java Code

### Compile and Run a Specific Program

```bash
cd misc-1099
./mvnw compile exec:java -Dexec.mainClass="com.fordcredit.misc1099.batch.runner.CCAC6340Runner"
```

### Run Tests (Compare Output vs Expected)

```bash
# After running a program, compare output
diff ../work/mainframe_clean/testcases/CCAC6340/output/master_out.txt \
     ../work/mainframe_clean/testcases/CCAC6340/expected/master_out.txt
```

## Adding a New COBOL Program

1. **Add source files:**
   ```
   work/mainframe_clean/cobol/NEWPROG.txt
   work/mainframe_clean/copybooks/NEWCOPY.cpy  (if any new copybooks)
   work/mainframe_clean/jcl/NEWPROG.txt        (optional)
   ```

2. **Run migration:**
   ```bash
   python cobol_to_springboot.py --program NEWPROG
   ```

3. **Create test data:**
   ```
   work/mainframe_clean/testcases/NEWPROG/input/master.txt
   work/mainframe_clean/testcases/NEWPROG/expected/master_out.txt
   ```

4. **Run and verify:**
   ```bash
   cd misc-1099
   ./mvnw compile exec:java -Dexec.mainClass="com.fordcredit.misc1099.batch.runner.NEWPROGRunner"
   ```

## How It Works

### LLM-Powered Layers (Azure OpenAI)

| Layer | Input | Output | LLM Role |
|-------|-------|--------|----------|
| Layer 1 | COBOL Copybooks | Java POJOs | Converts COBOL record layouts to Java classes |
| Layer 1.5 | COBOL Copybooks | FieldSpecs | Generates field metadata (offsets, lengths) |
| Layer 2 | JCL | Spring Batch Config | Converts JCL jobs/steps to Spring Batch |
| Layer 3A | COBOL Source | Intermediate Rep (IR) | Extracts paragraphs, control flow, business rules |

### Template-Based Layers (No LLM)

| Layer | Input | Output | How It Works |
|-------|-------|--------|--------------|
| Layer 3B | IR + Dossier | Tasklet Skeleton | Pattern detection → template selection |
| Layer 4 | Skeleton | Complete Tasklet | Injects merge/summarize/multi-input loops |
| Layer G | Tasklet | Test Harness | Generates runner and testcase structure |

### Pattern Detection

The pipeline analyzes COBOL source to detect patterns:

```python
# Two-way merge indicators
- PERFORM ... UNTIL MASTER-EOF AND CORPORATE-EOF
- COMPARE key fields

# Summarization indicators  
- AT END OF group
- ADD TO accumulators
- WRITE summary records

# Multi-input indicators
- Multiple FILE-CONTROL entries for input
- MERGE or interleaved reads
```

## Troubleshooting

### "AZURE_OPENAI_API_KEY environment variable required"

Create `.env` file with your Azure OpenAI credentials.

### Compilation errors in generated Java

Check the detected pattern matches your program:
```bash
python cobol_to_springboot.py --list
```

If pattern is wrong, you may need to adjust the analyzer heuristics in `cobol_to_springboot.py`.

### Test output doesn't match expected

1. Check input data format matches COBOL record layout
2. Verify key extraction logic in `masterKey()` / `transactionKey()`
3. Check header/trailer detection in `isHeaderOrTrailer()`

## License

Internal use only - Ford Credit 1099 Modernization Project
