# Archive

This folder contains legacy files that have been superseded by the consolidated migration pipeline.

## Contents

### `legacy_layers/`
Individual Python scripts that were used during iterative development. These have been consolidated into the main `cobol_to_springboot.py` pipeline:

| File | Original Purpose | Now In |
|------|------------------|--------|
| `layer1_copybooks_to_java.py` | Copybooks → POJOs | `cobol_to_springboot.py` Layer 1 |
| `layer1_5_copybooks_to_fieldspecs.py` | Copybooks → FieldSpecs | `cobol_to_springboot.py` Layer 1.5 |
| `layer2_jcl_to_springbatch.py` | JCL → Spring Batch | `cobol_to_springboot.py` Layer 2 |
| `layer3*.py` | COBOL → Tasklets | `cobol_to_springboot.py` Layer 3 |
| `layer4*.py` | Business logic wiring | `cobol_to_springboot.py` Layer 4 |
| `layer_g*.py` | Test harness generation | `cobol_to_springboot.py` Layer G |

### `final/`
Reference implementation of `CCAC6340Tasklet.java` that was hand-tuned during debugging. The current generated version in `misc-1099/` is equivalent and passes all tests.

## Note
These files are preserved for reference only. Use `cobol_to_springboot.py` for all migrations.

