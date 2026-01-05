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

## Running Generated Java Programs

### Prerequisites

1. **Compile the project first:**
   ```bash
   cd misc-1099
   ./mvnw compile
   ```

2. **Prepare test data** (if not already present):
   ```bash
   # Test data should be in:
   work/mainframe_clean/testcases/<PROGRAM_NAME>/input/
   ```

### Running a Program

Each converted COBOL program has a corresponding Runner class. Use one of these methods:

#### Method 1: Using Maven Exec Plugin (Recommended)

```bash
cd misc-1099

# Run CCAC6320
./mvnw exec:java -Dexec.mainClass="com.fordcredit.misc1099.batch.runner.CCAC6320Runner"

# Run CCAC6340
./mvnw exec:java -Dexec.mainClass="com.fordcredit.misc1099.batch.runner.CCAC6340Runner"

# Run CCAC6310
./mvnw exec:java -Dexec.mainClass="com.fordcredit.misc1099.batch.runner.CCAC6310Runner"

# Run CCAC6330
./mvnw exec:java -Dexec.mainClass="com.fordcredit.misc1099.batch.runner.CCAC6330Runner"

# Run CCAC6350
./mvnw exec:java -Dexec.mainClass="com.fordcredit.misc1099.batch.runner.CCAC6350Runner"

# Run CCAC6250
./mvnw exec:java -Dexec.mainClass="com.fordcredit.misc1099.batch.runner.CCAC6250Runner"
```

#### Method 2: Using Java Directly (After Compilation)

```bash
cd misc-1099

# Compile first
./mvnw compile

# Run using java command
java -cp target/classes:$(./mvnw dependency:build-classpath -q -DincludeScope=compile) \
     com.fordcredit.misc1099.batch.runner.CCAC6320Runner
```

#### Method 3: With Custom Test Data Path

Some runners accept a custom path as the first argument:

```bash
cd misc-1099

# Run with custom test data path
./mvnw exec:java \
  -Dexec.mainClass="com.fordcredit.misc1099.batch.runner.CCAC6320Runner" \
  -Dexec.args="../custom/testcases/CCAC6320"
```

### Available Programs

| Program | Runner Class | Test Data Path |
|---------|--------------|----------------|
| CCAC6250 | `CCAC6250Runner` | `work/mainframe_clean/testcases/CCAC6250` |
| CCAC6310 | `CCAC6310Runner` | `work/mainframe_clean/testcases/CCAC6310` |
| CCAC6320 | `CCAC6320Runner` | `work/mainframe_clean/testcases/CCAC6320` |
| CCAC6330 | `CCAC6330Runner` | `work/mainframe_clean/testcases/CCAC6330` |
| CCAC6340 | `CCAC6340Runner` | `work/mainframe_clean/testcases/CCAC6340` |
| CCAC6350 | `CCAC6350Runner` | `work/mainframe_clean/testcases/CCAC6350` |

### Understanding the Output

1. **Console Output:**
   - Success: `✔ CCAC6320Tasklet completed successfully`
   - Failure: `✘ CCAC6320Tasklet failed: <error message>`

2. **Output Files:**
   - Generated files are written to: `work/mainframe_clean/testcases/<PROGRAM>/output/`
   - Compare with expected: `work/mainframe_clean/testcases/<PROGRAM>/expected/`

3. **Example Output Structure:**
   ```
   work/mainframe_clean/testcases/CCAC6320/
   ├── input/
   │   ├── ten99T01rMiscTransFile.txt
   │   ├── control.txt
   │   └── ...
   ├── output/
   │   ├── ten99T01wTransOutputFile.txt
   │   ├── ten99T02wRejCycleOutFile.txt
   │   └── ...
   └── expected/
       ├── ten99T01wTransOutputFile.txt
       └── ...
   ```

### Verifying Results

```bash
# Compare output with expected results
cd work/mainframe_clean/testcases/CCAC6320

# Compare a specific output file
diff output/ten99T01wTransOutputFile.txt expected/ten99T01wTransOutputFile.txt

# Compare all output files
for file in output/*; do
    filename=$(basename "$file")
    echo "Comparing $filename..."
    diff "$file" "expected/$filename" || echo "  ✗ Differences found in $filename"
done
```

### Troubleshooting Java Execution

#### "ClassNotFoundException" or "NoClassDefFoundError"

```bash
# Make sure you compiled first
cd misc-1099
./mvnw clean compile
```

#### "FileNotFoundException" or "Input file not found"

- Check that test data exists in the expected location
- Verify the path: `work/mainframe_clean/testcases/<PROGRAM>/input/`
- Some programs require specific input files (check the program's documentation)

#### "RuntimeException: COBOL STOP RUN triggered"

- This indicates the program encountered an error condition
- Check the console output for error messages
- Verify input data format matches COBOL record layouts
- Check that required input files are present

#### Program runs but produces no output

- Check that output directory exists and is writable
- Verify file permissions: `chmod -R u+w work/mainframe_clean/testcases/<PROGRAM>/output/`
- Check console for error messages

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
