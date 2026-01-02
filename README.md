# Mainframe to Java Migration Factory

This repository implements a **layered, deterministic approach** for migrating **COBOL / Mainframe batch systems** to **Java (Spring Boot / Spring Batch)** using **LLMs as code generators**, not runtime dependencies.

The core philosophy is:

> **LLMs generate source code.  
> Java executes deterministically.  
> Every step is versioned, testable, and repeatable.**

This is **not** a one-off conversion.  
It is a **migration factory** that can be reused across mainframe applications.

---

## High-Level Architecture

```
COBOL / JCL / Copybooks
        |
        v
+----------------------+
| Layer 1              |  Copybooks → Java POJOs
+----------------------+
        |
        v
+----------------------+
| Layer 1.5            |  Copybooks → Field Layout Metadata
+----------------------+
        |
        v
+----------------------+
| Layer 1.6            |  Runtime Parsing (Fixed Width / EBCDIC)
+----------------------+
        |
        v
+----------------------+
| Layer 2              |  JCL → Spring Batch Jobs & Steps
+----------------------+
        |
        v
+----------------------+
| Layer 2B             |  Infrastructure Tasklets
+----------------------+
        |
        v
+----------------------+
| Layer 3              |  COBOL Programs → Step Logic
+----------------------+
        |
        v
+----------------------+
| Validation            |  Golden-Master Tests
+----------------------+
```

Each layer has a **single responsibility** and a **strict contract** with the next layer.

---

## Why This Layered Approach?

Mainframe systems combine **data layout**, **file I/O**, **job orchestration**, and **business logic** into monolithic programs.

Modern Java systems separate these concerns.

This repository **intentionally decomposes** the migration into layers so that:

* Each layer can be validated independently
* LLM output is constrained and deterministic
* Failures are localized and debuggable
* The same approach works across multiple applications
* Infrastructure concerns are separated from business logic

---

## Layer 1 — Copybooks → Java POJOs

**Purpose**

Convert COBOL copybooks into **pure Java data structures**.

---

## Layer 1.5 — Copybooks → Field Layout Metadata

**Purpose**

Extract **fixed-width layout information** from copybooks in a form Java can use at runtime.

---

## Layer 1.6 — Runtime Fixed-Width Parsing

**Purpose**

Convert raw fixed-width records into Java objects at runtime.

---

## Layer 2 — JCL → Spring Batch Jobs & Steps

**Purpose**

Translate **mainframe job orchestration (JCL)** into **Spring Batch job structure**.

Spring Batch job auto-execution is **explicitly disabled**:

```yaml
spring:
  batch:
    job:
      enabled: false
```

---

## Layer 2B — Infrastructure Tasklets

**Purpose**

Model **non-business mainframe utilities** as reusable Java components.

---

## Layer 3 — COBOL Programs → Step Logic (Next)

**Purpose**

Convert COBOL program logic into **Spring Batch step implementations**.

---

## Current Status

| Layer    | Status |
|---------|--------|
| Layer 1 | ✅ Complete |
| Layer 1.5 | ✅ Complete |
| Layer 1.6 | ✅ Complete |
| Layer 2 | ✅ Complete |
| Layer 2B | ✅ Complete |
| Layer 3 | 🔜 Planned |

---

## Final Note

This approach mirrors how **successful mainframe modernization programs** actually work:

* Decompose
* Validate
* Automate
* Iterate
