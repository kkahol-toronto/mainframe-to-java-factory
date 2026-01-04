package com.fordcredit.misc1099.batch.program;

import org.springframework.batch.core.StepContribution;
import org.springframework.batch.core.scope.context.ChunkContext;
import org.springframework.batch.core.step.tasklet.Tasklet;
import org.springframework.batch.repeat.RepeatStatus;

public class CCAC6340Tasklet implements Tasklet {

    /**
     * Program state holder.
     * Expanded in Layer 3E/3F.
     */
    static class MergeState {
        // File readers (simplified to BufferedReader)
        java.io.BufferedReader masterReader;
        java.io.BufferedReader corporateReader;

        boolean masterEof = false;
        boolean corporateEof = false;

        String masterRawLine;
        String corporateRawLine;
        String masterOutLine;
        String sysoutLine;

        // BEGIN DOMAIN STATE (Layer 3F)
        String deleteIndicator;

        Object masterRecord;
        Object corporateRecord;
        // END DOMAIN STATE (Layer 3F)
    }

    @Override
    public RepeatStatus execute(
            StepContribution contribution,
            ChunkContext chunkContext) {

        MergeState state = new MergeState();
        mainline(state);
        return RepeatStatus.FINISHED;
    }

    private void mainline(MergeState state) {
        // COBOL execution order
        openFiles(state);
        initialization(state);
    
        // MAIN LOOP
        mainProcess(state);
    
        endOfJob(state);
        wrapUp(state);
        closeOutputWriter();
    }

    // ======================================================
    // BEGIN GENERATED PARAGRAPHS (Layer 3C)
    // ======================================================

    private void initialization(MergeState state) { }

    private void openFiles(MergeState state) {
        try {
            String programName = "CCAC6340";
            // Path relative to project root (one level up from misc-1099)
            java.nio.file.Path testDir = java.nio.file.Paths.get(
                    "../work/mainframe_clean/testcases", programName);

            // Initialize master reader
            java.nio.file.Path masterPath = testDir.resolve("input/master.txt");
            if (java.nio.file.Files.exists(masterPath)) {
                state.masterReader = java.nio.file.Files.newBufferedReader(masterPath);
            }

            // Initialize corporate reader
            java.nio.file.Path corpPath = testDir.resolve("input/corporate.txt");
            if (java.nio.file.Files.exists(corpPath)) {
                state.corporateReader = java.nio.file.Files.newBufferedReader(corpPath);
            }

            // Initialize output writer
            initOutputWriter(programName);

        } catch (Exception e) {
            throw new RuntimeException("Failed to open files", e);
        }
    }

    private void initializeSysout(MergeState state) { }

    private void mainProcess(MergeState state) {

        if (!state.masterEof && state.masterRawLine == null) {
            readMasterFile(state);
        }
        if (!state.corporateEof && state.corporateRawLine == null) {
            readCorporateFile(state);
        }

        while (!state.masterEof) {

            state.masterOutLine = null;

            if (isHeaderOrTrailer(state)) {
                prepareOutputFromMaster(state);
                writeMasterOut(state);
                readMasterFile(state);
                continue;
            }

            if (state.corporateEof || state.corporateRawLine == null) {
                prepareOutputFromMaster(state);
                writeMasterOut(state);
                readMasterFile(state);
                continue;
            }

            int cmp = compareKeys(state);

            if (cmp == 0) {
                setMechanized(state);
                prepareOutputFromMaster(state);
                writeMasterOut(state);
                readMasterFile(state);
                readCorporateFile(state);
            } else if (cmp < 0) {
                prepareOutputFromMaster(state);
                writeMasterOut(state);
                readMasterFile(state);
            } else {
                readCorporateFile(state);
            }
        }
    }

    private void processEntireMaster(MergeState state) { }

    private void readControlCard(MergeState state) { }

    private void readCorporateFile(MergeState state) {
        try {
            if (state.corporateReader == null) {
                state.corporateEof = true;
                return;
            }

            String line = state.corporateReader.readLine();
            if (line == null || line.trim().isEmpty()) {
                state.corporateEof = true;
                state.corporateRawLine = null;
            } else {
                state.corporateRawLine = line;
            }
        } catch (Exception e) {
            throw new RuntimeException("Error reading corporate file", e);
        }
    }
    

    private void readMasterFile(MergeState state) {
        try {
            if (state.masterReader == null) {
                state.masterEof = true;
                return;
            }

            String line = state.masterReader.readLine();
            if (line == null || line.trim().isEmpty()) {
                state.masterEof = true;
                state.masterRawLine = null;
            } else {
                state.masterRawLine = line;
            }
        } catch (Exception e) {
            throw new RuntimeException("Error reading master file", e);
        }
    }

    // ❌ REMOVED duplicate stub writeMasterOut from Layer 3C

    private void writeNewTrailerOut(MergeState state) { }

    private void endOfJob(MergeState state) { }

    private void wrapUp(MergeState state) { }

    // ======================================================
    // BEGIN IO PLUMBING (Layer 3E)
    // ======================================================

    private java.io.BufferedWriter masterOutWriter;

    private void initOutputWriter(String programName) {
        try {
            java.nio.file.Path outPath =
                    java.nio.file.Paths.get(
                            "../work/mainframe_clean/testcases",
                            programName,
                            "output",
                            "master_out.txt"
                    );
            java.nio.file.Files.createDirectories(outPath.getParent());
            masterOutWriter = java.nio.file.Files.newBufferedWriter(outPath);
        } catch (Exception e) {
            throw new RuntimeException("Failed to initialize master output writer", e);
        }
    }

    // ✅ ADDED (missing method)
    private void writeMasterLine(String line) {
        if (masterOutWriter == null || line == null) return;
        try {
            masterOutWriter.write(line);
            masterOutWriter.newLine();
        } catch (Exception e) {
            throw new RuntimeException("Failed writing master output", e);
        }
    }

    // ✅ SINGLE authoritative writeMasterOut
    private void writeMasterOut(MergeState state) {
        if (state.masterOutLine == null) {
            state.masterOutLine = state.masterRawLine;
        }

        writeMasterLine(state.masterOutLine);

        // Layer 4.4 — reset COBOL per-record state AFTER write
        state.deleteIndicator = null;
    }

    private void closeOutputWriter() {
        try {
            if (masterOutWriter != null) masterOutWriter.close();
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    // ======================================================
    // BEGIN DOMAIN BINDING (Layer 3F)
    // ======================================================

    private void bindMasterRecord(MergeState state) {
        state.masterRecord =
                com.fordcredit.misc1099.batch.bind.ReflectionDomainBinder.bind(
                        state.masterRawLine,
                        "com.fordcredit.misc1099.domain.copybook.C2INP001",
                        "com.fordcredit.misc1099.parser.C2INP001FieldSpecs"
                );
    }

    private void bindCorporateRecord(MergeState state) {
        state.corporateRecord =
                com.fordcredit.misc1099.batch.bind.ReflectionDomainBinder.bind(
                        state.corporateRawLine,
                        "com.fordcredit.misc1099.domain.copybook.C2INP003",
                        "com.fordcredit.misc1099.parser.C2INP003FieldSpecs"
                );
    }

    // ======================================================
    // CURSOR MERGE HELPERS (Layer 4.3)
    // ======================================================

    private boolean isHeaderOrTrailer(MergeState state) {
        return state.masterRawLine != null &&
                (state.masterRawLine.startsWith("HDR")
                        || state.masterRawLine.startsWith("TRL"));
    }

    private String masterKey(MergeState state) {
        if (state.masterRawLine == null) return null;
        return state.masterRawLine.split("\\|", -1)[0].trim();
    }

    private String corporateKey(MergeState state) {
        return state.corporateRawLine == null ? null : state.corporateRawLine.trim();
    }

    private int compareKeys(MergeState state) {
        String mk = masterKey(state);
        String ck = corporateKey(state);
        if (mk == null && ck == null) return 0;
        if (mk == null) return -1;
        if (ck == null) return 1;
        return mk.compareTo(ck);
    }

    private void setMechanized(MergeState state) {
        state.deleteIndicator = "M";
    }

    private void prepareOutputFromMaster(MergeState state) {
        state.masterOutLine = state.masterRawLine;
    
        if ("M".equals(state.deleteIndicator)) {
            if (!state.masterOutLine.endsWith("|")) {
                state.masterOutLine = state.masterOutLine + "|";
            }
            state.masterOutLine = state.masterOutLine + "M";
        }
    }
}
