package com.fordcredit.misc1099.batch.program;

import org.springframework.batch.core.StepContribution;
import org.springframework.batch.core.scope.context.ChunkContext;
import org.springframework.batch.core.step.tasklet.Tasklet;
import org.springframework.batch.repeat.RepeatStatus;

/**
 * Auto-generated Tasklet for COBOL program CCAC6340.
 * 
 * Pattern: TWO_WAY_MERGE
 * Purpose: 
 */
public class CCAC6340Tasklet implements Tasklet {

    /**
     * Program state holder.
     */
    static class ProgramState {
        java.io.BufferedReader t01rCorporateFileReader;
        boolean t01rCorporateFileEof = false;
        String t01rCorporateFileRawLine;
        java.io.BufferedReader m01rMasterInFileReader;
        boolean m01rMasterInFileEof = false;
        String m01rMasterInFileRawLine;
        java.io.BufferedReader r01rControlFileReader;
        boolean r01rControlFileEof = false;
        String r01rControlFileRawLine;
        java.io.BufferedWriter m01wMasterOutFileWriter;
        String m01wMasterOutFileOutLine;
        String deleteIndicator;
    }

    @Override
    public RepeatStatus execute(
            StepContribution contribution,
            ChunkContext chunkContext) {

        ProgramState state = new ProgramState();
        mainline(state);
        return RepeatStatus.FINISHED;
    }

    private void mainline(ProgramState state) {
        openFiles(state);
        initialization(state);
        mainProcess(state);
        endOfJob(state);
        closeFiles(state);
    }

    // ======================================================
    // FILE I/O
    // ======================================================

    private void openFiles(ProgramState state) {
        try {
            String programName = "CCAC6340";
            java.nio.file.Path testDir = java.nio.file.Paths.get(
                    "../work/mainframe_clean/testcases", programName);

            java.nio.file.Path t01rCorporateFilePath = testDir.resolve("input/corporate.txt");
            if (java.nio.file.Files.exists(t01rCorporateFilePath)) {
                state.t01rCorporateFileReader = java.nio.file.Files.newBufferedReader(t01rCorporateFilePath);
            }

            java.nio.file.Path m01rMasterInFilePath = testDir.resolve("input/master.txt");
            if (java.nio.file.Files.exists(m01rMasterInFilePath)) {
                state.m01rMasterInFileReader = java.nio.file.Files.newBufferedReader(m01rMasterInFilePath);
            }

            java.nio.file.Path r01rControlFilePath = testDir.resolve("input/control.txt");
            if (java.nio.file.Files.exists(r01rControlFilePath)) {
                state.r01rControlFileReader = java.nio.file.Files.newBufferedReader(r01rControlFilePath);
            }

            java.nio.file.Path m01wMasterOutFilePath = testDir.resolve("output/master_out.txt");
            java.nio.file.Files.createDirectories(m01wMasterOutFilePath.getParent());
            state.m01wMasterOutFileWriter = java.nio.file.Files.newBufferedWriter(m01wMasterOutFilePath);

        } catch (Exception e) {
            throw new RuntimeException("Failed to open files", e);
        }
    }

    private void closeFiles(ProgramState state) {
        try {
            if (state.t01rCorporateFileReader != null) state.t01rCorporateFileReader.close();
            if (state.m01rMasterInFileReader != null) state.m01rMasterInFileReader.close();
            if (state.r01rControlFileReader != null) state.r01rControlFileReader.close();
            if (state.m01wMasterOutFileWriter != null) state.m01wMasterOutFileWriter.close();
        } catch (Exception e) {
            throw new RuntimeException("Failed to close files", e);
        }
    }

    private void readT01rCorporateFileFile(ProgramState state) {
        try {
            if (state.t01rCorporateFileReader == null) {
                state.t01rCorporateFileEof = true;
                return;
            }
            String line = state.t01rCorporateFileReader.readLine();
            if (line == null || line.trim().isEmpty()) {
                state.t01rCorporateFileEof = true;
                state.t01rCorporateFileRawLine = null;
            } else {
                state.t01rCorporateFileRawLine = line;
            }
        } catch (Exception e) {
            throw new RuntimeException("Error reading t01rCorporateFile file", e);
        }
    }

    private void readM01rMasterInFileFile(ProgramState state) {
        try {
            if (state.m01rMasterInFileReader == null) {
                state.m01rMasterInFileEof = true;
                return;
            }
            String line = state.m01rMasterInFileReader.readLine();
            if (line == null || line.trim().isEmpty()) {
                state.m01rMasterInFileEof = true;
                state.m01rMasterInFileRawLine = null;
            } else {
                state.m01rMasterInFileRawLine = line;
            }
        } catch (Exception e) {
            throw new RuntimeException("Error reading m01rMasterInFile file", e);
        }
    }

    private void readR01rControlFileFile(ProgramState state) {
        try {
            if (state.r01rControlFileReader == null) {
                state.r01rControlFileEof = true;
                return;
            }
            String line = state.r01rControlFileReader.readLine();
            if (line == null || line.trim().isEmpty()) {
                state.r01rControlFileEof = true;
                state.r01rControlFileRawLine = null;
            } else {
                state.r01rControlFileRawLine = line;
            }
        } catch (Exception e) {
            throw new RuntimeException("Error reading r01rControlFile file", e);
        }
    }

    private void writeM01wMasterOutFileLine(ProgramState state, String line) {
        if (state.m01wMasterOutFileWriter == null || line == null) return;
        try {
            state.m01wMasterOutFileWriter.write(line);
            state.m01wMasterOutFileWriter.newLine();
        } catch (Exception e) {
            throw new RuntimeException("Error writing m01wMasterOutFile file", e);
        }
    }

    // ======================================================
    // BUSINESS LOGIC
    // ======================================================

    private void initialization(ProgramState state) {
        // TODO: Initialize counters, accumulators, flags
    }

    private void mainProcess(ProgramState state) {
        // Prime reads
        if (!state.m01rMasterInFileEof && state.m01rMasterInFileRawLine == null) {
            readM01rMasterInFileFile(state);
        }
        if (!state.t01rCorporateFileEof && state.t01rCorporateFileRawLine == null) {
            readT01rCorporateFileFile(state);
        }

        // Two-way merge loop
        while (!state.m01rMasterInFileEof) {
            state.deleteIndicator = null;
            state.m01wMasterOutFileOutLine = null;

            // Pass-through headers/trailers
            if (isHeaderOrTrailer(state)) {
                state.m01wMasterOutFileOutLine = state.m01rMasterInFileRawLine;
                writeM01wMasterOutFileLine(state, state.m01wMasterOutFileOutLine);
                readM01rMasterInFileFile(state);
                continue;
            }

            // If transaction exhausted, copy master through
            if (state.t01rCorporateFileEof || state.t01rCorporateFileRawLine == null) {
                prepareOutputFromMaster(state);
                writeM01wMasterOutFileLine(state, state.m01wMasterOutFileOutLine);
                readM01rMasterInFileFile(state);
                continue;
            }

            int cmp = compareKeys(state);

            if (cmp == 0) {
                // MATCH: set flag, advance both
                setMatchFlag(state);
                prepareOutputFromMaster(state);
                writeM01wMasterOutFileLine(state, state.m01wMasterOutFileOutLine);
                readM01rMasterInFileFile(state);
                readT01rCorporateFileFile(state);
            } else if (cmp < 0) {
                // Master < Transaction: write master, advance master
                prepareOutputFromMaster(state);
                writeM01wMasterOutFileLine(state, state.m01wMasterOutFileOutLine);
                readM01rMasterInFileFile(state);
            } else {
                // Master > Transaction: advance transaction
                readT01rCorporateFileFile(state);
            }
        }
    }

    private void endOfJob(ProgramState state) {
        // TODO: Write trailers, final reports
    }

    // ======================================================
    // HELPER METHODS
    // ======================================================

    private boolean isHeaderOrTrailer(ProgramState state) {
        if (state.m01rMasterInFileRawLine == null) return false;
        return state.m01rMasterInFileRawLine.startsWith("HDR") 
            || state.m01rMasterInFileRawLine.startsWith("TRL")
            || state.m01rMasterInFileRawLine.charAt(0) == '\u0000'
            || state.m01rMasterInFileRawLine.charAt(0) == '\u00FF';
    }

    private String masterKey(ProgramState state) {
        if (state.m01rMasterInFileRawLine == null) return null;
        // Extract key - customize based on record layout
        String[] parts = state.m01rMasterInFileRawLine.split("\\|", -1);
        return parts.length > 0 ? parts[0].trim() : null;
    }

    private String transactionKey(ProgramState state) {
        if (state.t01rCorporateFileRawLine == null) return null;
        return state.t01rCorporateFileRawLine.trim();
    }

    private int compareKeys(ProgramState state) {
        String mk = masterKey(state);
        String tk = transactionKey(state);
        if (mk == null && tk == null) return 0;
        if (mk == null) return -1;
        if (tk == null) return 1;
        return mk.compareTo(tk);
    }

    private void setMatchFlag(ProgramState state) {
        state.deleteIndicator = "M";
    }

    private void prepareOutputFromMaster(ProgramState state) {
        state.m01wMasterOutFileOutLine = state.m01rMasterInFileRawLine;
        if ("M".equals(state.deleteIndicator)) {
            if (!state.m01wMasterOutFileOutLine.endsWith("|")) {
                state.m01wMasterOutFileOutLine = state.m01wMasterOutFileOutLine + "|";
            }
            state.m01wMasterOutFileOutLine = state.m01wMasterOutFileOutLine + "M";
        }
    }
}
