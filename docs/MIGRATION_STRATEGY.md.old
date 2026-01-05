# Migration Strategy: Complete All COBOL Programs

## Current Status

### What Works
- ✅ CCAC6340 (Two-Way Merge) - Fully working, passes tests
- ✅ JCL parsing - All 6 JCL jobs mapped to steps and COBOL programs
- ✅ Basic Tasklet structure generation
- ✅ Spring Batch Job configs

### What Needs Improvement
- ⚠️ LLM paragraph translation generates references to undefined variables
- ⚠️ Need better variable extraction from WORKING-STORAGE
- ⚠️ Need comprehensive hardening/validation

## JCL → COBOL Mapping

| JCL Job | COBOL Programs | Steps | Purpose |
|---------|----------------|-------|---------|
| CCAC@625 | CCAC6250 | 6 | File Preparation (headers/trailers) |
| CCAC@626 | CCAC6250 | 8 | More File Prep |
| CCAC@630 | CCAC6310, CCAC6320, CCAC6330, CCAC6340 | 28 | Main Processing |
| CCAC@635 | CCAC6350 | 17 | Year-End Summary |
| CCAC@640 | (none) | 4 | Data Transfer (SORT/IEBGENER) |
| CCAC@641 | (none) | 5 | FTP Transfer |

## Recommended Approach per Program

### Tier 1: Template-Based (Proven)
These use well-understood patterns that work reliably:

| Program | Pattern | Approach | Effort |
|---------|---------|----------|--------|
| **CCAC6340** | Two-Way Merge | ✅ Done | Complete |

### Tier 2: Template + Light LLM
These have clear patterns with some business logic:

| Program | Pattern | Approach | Effort |
|---------|---------|----------|--------|
| **CCAC6250** | Reformatter | Template (read/write) + LLM (header/trailer creation) | 4h |
| **CCAC6330** | Update Merge | Template (merge loop) + LLM (update logic) | 4h |

### Tier 3: LLM + Heavy Review
These have complex, unique logic:

| Program | Pattern | Approach | Effort |
|---------|---------|----------|--------|
| **CCAC6350** | Summarization | LLM (control breaks) + manual review | 6h |
| **CCAC6310** | Tax Type Lookup | LLM (table search) + manual review | 6h |
| **CCAC6320** | Transaction Router | LLM + heavy manual review | 10h |

## Recommended Execution Order

```
Phase 1: Foundation (Done)
  ✅ JCL parsing
  ✅ COBOL parsing
  ✅ Tasklet skeleton generation
  ✅ Spring Batch Job configs

Phase 2: Template Programs
  1. CCAC6250 - Simple reformatter
  2. CCAC6330 - Similar to working CCAC6340

Phase 3: Complex Programs
  3. CCAC6350 - Summarization with control breaks
  4. CCAC6310 - Table lookup
  5. CCAC6320 - Complex router (save for last)
```

## Key Variables to Extract per Program

### CCAC6250 (Reformatter)
```
Counters:
- WS-TOT-REJ-RECDS-READ
- WS-TOT-VEND-RECDS-READ
- WS-TOT-MSTR-RECDS-READ
- WS-TOT-REJ-WRITTEN-CNT

Headers/Trailers:
- WS-REJ-HDR-ID-LIT
- WS-VEN-HDR-ID-LIT
- WS-MSTR-HDR-ID-LIT

Date fields:
- WS-DATE-MONTH, WS-DATE-DAY, WS-DATE-YEAR
```

### CCAC6350 (Summarization)
```
Control break keys:
- WS-PREV-OPER-LOC-CODE
- WS-PREV-REC-KEY (SSN + TAX-TYPE + TIN)
- WS-PREV-BR-DEPT

Accumulators:
- WS-CALC-TOT-AMOUNT
- WS-TOT-RECDS-READ
```

### CCAC6310 (Tax Type Lookup)
```
Table:
- WS-1099-ENTRY-CODE-TABLE (150 entries)
- ENTRY-CODE-MATCH

Output routing:
- Write to BCCW-OUT, DEFT-OUT, MISC-TRANS-OUT, REJ-CYCLE-OUT
```

## LLM Prompt Strategy

### Good Prompt (Focused)
```
Convert this specific COBOL paragraph to Java.
Keep the exact logic, use state.xxx for variables.

Paragraph: 3100-BREAK-FOR-BRANCH
[code]

Available state variables:
- state.prevBrDept (String)
- state.currentBrDept (String)
- state.prevCheckType (String)
- ...
```

### Bad Prompt (Too Broad)
```
Convert this COBOL program to Java.
[entire 1000-line program]
```

## Hardening Checklist

After LLM translation, verify:

1. [ ] All referenced variables exist in ProgramState
2. [ ] No duplicate method definitions
3. [ ] Correct method signatures (take ProgramState parameter)
4. [ ] Try/catch for file operations
5. [ ] EOF flag checks match file reader names
6. [ ] Write operations use correct writer names
7. [ ] Counters are incremented correctly

## Manual Override Points

For each program, identify 2-3 "critical paragraphs" that MUST be manually verified:

| Program | Critical Paragraphs |
|---------|---------------------|
| CCAC6250 | 1250-CREATE-REJ-HEADER, 5100-CREATE-REJ-TRAILER |
| CCAC6310 | 4000-SEARCH-1099-ENTRY-TABLE |
| CCAC6320 | 3000-ROUTE-TRANSACTION |
| CCAC6330 | 3100-UPDATE-MASTER |
| CCAC6350 | 3200-BREAK-FOR-SOC-SEC-NUM, 3300-BREAK-FOR-OPER-LOC |

## Next Steps

1. **Generate comprehensive ProgramState** by extracting ALL WS variables
2. **Improve LLM prompts** to reference available state variables
3. **Add variable mapping** from COBOL names to Java names
4. **Implement per-program manual overrides** for critical paragraphs

