# Documentation Index

This folder contains documentation for the COBOL to Java migration project.

## Main Documents

### 📧 [ENGINEER_HANDOFF_EMAIL.md](./ENGINEER_HANDOFF_EMAIL.md) / [PDF](./ENGINEER_HANDOFF_EMAIL.pdf)
**Purpose:** Comprehensive email for engineering team handoff  
**Contents:**
- Executive summary of accomplishments
- Conversion approach (Hybrid LLM + Template)
- Iterative refinement system (three-tier)
- Technical details and architecture
- Testing requirements
- Next steps

**Use:** Send to engineering team for testing and handoff

---

### 📊 [MIGRATION_STATUS.md](./MIGRATION_STATUS.md)
**Purpose:** Current status of all converted programs  
**Contents:**
- All 6 programs status
- JCL jobs status
- Pipeline architecture
- Iterative fix system overview
- Next steps

**Use:** Quick reference for project status

---

### 🔧 [PIPELINE_FIXES.md](./PIPELINE_FIXES.md)
**Purpose:** Documentation of post-processing fixes  
**Contents:**
- What gets fixed automatically
- How to extend the post-processor
- Legacy fix scripts organization

**Use:** Reference for understanding automatic fixes

---

### 📖 [JCL_TO_JAVA_EXPLANATION.md](./JCL_TO_JAVA_EXPLANATION.md)
**Purpose:** Simple explanation of JCL vs COBOL programs  
**Contents:**
- What JCL jobs are (625, 626, 630)
- What COBOL programs are
- What's converted vs what's not
- Current state and limitations

**Use:** For non-mainframe people to understand what was done

---

## Obsolete Documents

The following documents are kept for historical reference but may contain outdated information:

- `APPROACH_EMAIL.md` - Older version, superseded by ENGINEER_HANDOFF_EMAIL.md
- `MIGRATION_STRATEGY.md` - Early strategy document, information now in other docs

---

## Quick Links

- **Main README:** [../README.md](../README.md) - Full project documentation
- **Repository:** https://github.com/kkahol-toronto/mainframe-to-java-factory
- **Main Pipeline:** `../cobol_to_springboot.py`
- **Post-Processor:** `../post_process_java.py`

---

## Document Status

| Document | Status | Last Updated |
|----------|--------|--------------|
| ENGINEER_HANDOFF_EMAIL.md | ✅ Current | January 2025 |
| MIGRATION_STATUS.md | ✅ Current | January 2025 |
| PIPELINE_FIXES.md | ✅ Current | January 2025 |
| JCL_TO_JAVA_EXPLANATION.md | ✅ Current | January 2025 |
| APPROACH_EMAIL.md | ⚠️ Superseded | (Historical) |
| MIGRATION_STRATEGY.md | ⚠️ Superseded | (Historical) |

