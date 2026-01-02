
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
| Layer 2              |  JCL → Spring Batch Jobs
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

---

## Layer 1 — Copybooks → Java POJOs

**Purpose**

Convert COBOL copybooks into **pure Java data structures**.

**What it produces**

* One Java class per COBOL 01-level record
* Fields preserve:

  * Order
  * Length
  * Numeric precision
  * Hierarchy (nested structures)

**What it does NOT do**

* No parsing logic
* No business rules
* No validation
* No annotations

**Why**

This layer establishes **structural truth**.
These classes are the Java equivalent of COBOL copybooks in memory.

**Output example**

```
src/main/java/.../domain/copybook/C2INP001.java
```

---

## Layer 1.5 — Copybooks → Field Layout Metadata

**Purpose**

Extract **fixed-width layout information** from copybooks in a form Java can use at runtime.

**Key design decision**

> Layer 1.5 produces **layout metadata only**, not Java binding logic.

This avoids:

* LLMs guessing Java setter names
* LLMs guessing record types
* Compiler-breaking output

**What it produces**

* One `FieldSpecs` class per copybook
* Each class returns:

  ```java
  List<FieldSpec>
  ```

**What a FieldSpec represents**

* Field name
* Start offset
* Length
* Scale (for decimals)
* Field kind (STRING / DECIMAL)

**Output example**

```java
public class C2INP001FieldSpecs {

    public static List<FieldSpec> fields() {
        return List.of(
            FieldSpec.string("payerTin", 0, 9),
            FieldSpec.decimal("interestAmt", 9, 9, 2),
            FieldSpec.string("payerName", 18, 30)
        );
    }
}
```

**Why this matters**

This makes layout:

* Explicit
* Testable
* Independent of Java binding details

---

## Layer 1.6 — Runtime Fixed-Width Parsing

**Purpose**

Convert raw fixed-width records into Java objects at runtime.

**How it works**

* Uses `FieldSpec` metadata
* Uses reflection to bind fields to Java POJOs
* Handles:

  * Strings
  * Decimals
  * Scale
  * Trimming
  * Defaults

**Important constraint**

> No LLMs are used at runtime.

All runtime behavior is:

* Deterministic
* Testable
* JVM-only

**Why reflection is acceptable here**

* Happens at batch scale, not per request
* Controlled input formats
* Much safer than LLM-generated runtime logic

---

## Layer 2 — JCL → Spring Batch Jobs (Planned)

**Purpose**

Translate mainframe job orchestration (JCL) into **Spring Batch jobs**.

**Mapping**

| Mainframe | Java                   |
| --------- | ---------------------- |
| JOB       | Job                    |
| STEP      | Step                   |
| PROC      | Reusable Job Component |
| DD        | Reader / Writer        |

**What this layer produces**

* Spring Batch `Job` definitions
* Step ordering
* File wiring
* Restart semantics

---

## Layer 3 — COBOL Programs → Step Logic (Planned)

**Purpose**

Convert COBOL program logic into **Step implementations**.

**Typical mappings**

| COBOL Pattern | Java                           |
| ------------- | ------------------------------ |
| READ loop     | ItemReader                     |
| COMPUTE       | ItemProcessor                  |
| WRITE         | ItemWriter                     |
| Control-break | Composite / Stateful Processor |

LLMs are used **only to generate source code**, never for execution.

---

## Validation — Golden-Master Testing

**Purpose**

Ensure Java output matches mainframe output **exactly**.

**Approach**

* Same input files
* Same execution order
* Byte-for-byte output comparison

This allows:

* Parallel runs
* Incremental cutover
* Confidence in correctness

---

## Why Use LLMs This Way?

This repository treats LLMs as:

> **Code generators with guardrails**, not reasoning engines.

Key principles:

* LLMs generate *structure*
* Python enforces *syntax*
* Java enforces *runtime correctness*
* Git enforces *history and review*

This dramatically reduces hallucination risk.

---

## What This Repository Is (and Is Not)

**This IS**

* A reusable migration factory
* Deterministic
* Enterprise-safe
* Auditable
* Incrementally adoptable

**This is NOT**

* A one-click COBOL converter
* Runtime AI system
* Heuristic or best-effort rewrite

---

## Current Status

| Layer      | Status     |
| ---------- | ---------- |
| Layer 1    | ✅ Complete |
| Layer 1.5  | ✅ Complete |
| Layer 1.6  | ✅ Complete |
| Layer 2    | 🔜 Planned |
| Layer 3    | 🔜 Planned |
| Validation | 🔜 Planned |

---

## How to Extend This Repo

This structure is intentionally generic.

To reuse it for another application:

1. Replace copybooks under `work/mainframe_clean/copybook`
2. Re-run Layer 1
3. Re-run Layer 1.5
4. Wire readers/writers
5. Add batch jobs

No changes to core infrastructure required.

---

## Final Note

This approach mirrors how **real mainframe modernization programs** succeed:

* Decompose
* Validate
* Automate
* Iterate

If you reached a green build, you’ve already crossed the hardest milestone.

