# Documentation Index

This folder contains documentation for the COBOL to Java migration project.

## Main Documents

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
| MIGRATION_STATUS.md | ✅ Current | January 2025 |
| PIPELINE_FIXES.md | ✅ Current | January 2025 |
| JCL_TO_JAVA_EXPLANATION.md | ✅ Current | January 2025 |

