package com.fordcredit.misc1099.batch.program;

import org.springframework.batch.core.StepContribution;
import org.springframework.batch.core.scope.context.ChunkContext;
import org.springframework.batch.core.step.tasklet.Tasklet;
import org.springframework.batch.repeat.RepeatStatus;
import java.io.*;
import java.nio.file.*;
import java.math.BigDecimal;

public class CCAC6340Tasklet implements Tasklet {

    private final String basePath;

    public CCAC6340Tasklet(String basePath) {
        this.basePath = basePath;
    }

    // =========== PROGRAM STATE ===========
    static class ProgramState {
        // File I/O
        BufferedReader ten99T01rCorporateFileReader;
        String ten99T01rCorporateFileLine;
        boolean ten99T01rCorporateFileEof = false;
        BufferedWriter ten99T01rCorporateFileWriter;
        BufferedReader ten99M01rMasterInFileReader;
        String ten99M01rMasterInFileLine;
        boolean ten99M01rMasterInFileEof = false;
        BufferedWriter ten99M01rMasterInFileWriter;
        BufferedReader ten99R01rControlFileReader;
        String ten99R01rControlFileLine;
        boolean ten99R01rControlFileEof = false;
        BufferedWriter ten99R01rControlFileWriter;
        BufferedReader ten99M01wMasterOutFileReader;
        String ten99M01wMasterOutFileLine;
        boolean ten99M01wMasterOutFileEof = false;
        BufferedWriter ten99M01wMasterOutFileWriter;

        // WORKING-STORAGE variables
        String panValet = "006CCAC6340";
        String pups2000 = "";
        String ten99M01rMasterInDtl = "";
        String tmmidCheckNumber = "";
        String tmmidDepartmentCode = "";
        int tmmidSocialSecNumber = 0;
        String tmmidSocialSecRemainder = "";
        String tmmidOperationLoc = "";
        String tmmidSource = "";
        String tmmidName = "";
        String tmmidAddress1 = "";
        String tmmidAddress2 = "";
        String tmmidCity = "";
        String tmmidState = "";
        int tmmidZipCode = 0;
        String tmmidDeleteIndicator = "";
        // 88 nonDeleteInput: tmmidDeleteIndicator.equals("")
        // 88 businessInput: tmmidDeleteIndicator.equals("B")
        // 88 deleteInput: tmmidDeleteIndicator.equals("D")
        // 88 mechanizedInput: tmmidDeleteIndicator.equals("M")
        java.math.BigDecimal tmmidTotalAmount = java.math.BigDecimal.ZERO;
        java.math.BigDecimal tmmid1099Amount = java.math.BigDecimal.ZERO;
        String tmmidCheckDate = "";
        String tmmidProcessDate = "";
        int tmmidNumDisbRecords = 0;
        String[] tmmidDisbRecord = new String[5];
        String tmmidDisbEntryCode = "";
        String tmmidDisb1099Indicator = "";
        java.math.BigDecimal tmmidDisbAmount = java.math.BigDecimal.ZERO;
        String tmmidComments = "";
        String tmmidLineNumber = "";
        String tmmidTaxType = "";
        String tmmidTinInd = "";
        String ten99M01rMastInHdr = "";
        String tmmihHeadSource = "";
        // 88 mastHeader: tmmihHeadSource.equals("LOW-VALUES")
        String tmmihHeadId = "";
        String tmmihHeadDate = "";
        int tmmihHeadCentury = 0;
        int tmmihHeadYear = 0;
        int tmmihHeadMonth = 0;
        String ten99M01rMastInTlr = "";
        String tmmitTrailSource = "";
        // 88 mastTrailer: tmmitTrailSource.equals("HIGH-VALUES")
        int tmmitNonDeleteTrailCnt = 0;
        java.math.BigDecimal tmmitNonDeleteTrailAmt = java.math.BigDecimal.ZERO;
        int tmmitDeleteTrailCnt = 0;
        java.math.BigDecimal tmmitDeleteTrailAmt = java.math.BigDecimal.ZERO;
        String ten99T01rCorporateDtl = "";
        int ttcdSocialSecNumber = 0;
        String ttcdName = "";
        String ten99R01rControlCardDtl = "";
        String trccdControlDate = "";
        int trccdControlCentury = 0;
        int trccdControlYear = 0;
        int trccdControlMonth = 0;
        String ten99M01wMasterOutDtl = "";
        String tmmodCheckNumber = "";
        String tmmodDepartmentCode = "";
        int tmmodSocialSecNumber = 0;
        String tmmodSocialSecRemainder = "";
        String tmmodOperationLoc = "";
        String tmmodSource = "";
        String tmmodName = "";
        String tmmodAddress1 = "";
        String tmmodAddress2 = "";
        String tmmodCity = "";
        String tmmodState = "";
        String tmmodZipCode = "";
        String tmmodDeleteIndicator = "";
        // 88 nonDeleteOutput: tmmodDeleteIndicator.equals("")
        // 88 businessOutput: tmmodDeleteIndicator.equals("B")
        // 88 deleteOutput: tmmodDeleteIndicator.equals("D")
        // 88 mechanizedOutput: tmmodDeleteIndicator.equals("M")
        java.math.BigDecimal tmmodTotalAmount = java.math.BigDecimal.ZERO;
        java.math.BigDecimal tmmod1099Amount = java.math.BigDecimal.ZERO;
        String tmmodCheckDate = "";
        String tmmodProcessDate = "";
        int tmmodNumDisbRecords = 0;
        String[] tmmodDisbRecords = new String[5];
        String tmmodDisbEntryCode = "";
        String tmmodDisb1099Indicator = "";
        java.math.BigDecimal tmmodDisbAmount = java.math.BigDecimal.ZERO;
        String tmmodComments = "";
        String tmmodLineNumber = "";
        String tmmodTaxType = "";
        String tmmodTinInd = "";
        String ten99M01wMastOutHdr = "";
        String tmmohHeadSource = "";
        // 88 mastOutHead: tmmohHeadSource.equals("LOW-VALUES")
        String tmmohHeadId = "";
        String tmmohHeadDate = "";
        int tmmohHeadCentury = 0;
        int tmmohHeadYear = 0;
        int tmmohHeadMonth = 0;
        String ten99M01wMastOutTlr = "";
        String tmmotTrailSource = "";
        // 88 mastOutTrailer: tmmotTrailSource.equals("HIGH-VALUES")
        int tmmotTrailCnt = 0;
        java.math.BigDecimal tmmotTrailAmt = java.math.BigDecimal.ZERO;
        String wsCounters = "";
        int wsMastInCnt = 0;
        int wsMastOutCnt = 0;
        int wsOriginalInCntTotal = 0;
        String wsAccumulators = "";
        java.math.BigDecimal wsMastInAmt = java.math.BigDecimal.ZERO;
        java.math.BigDecimal wsMastOutAmt = java.math.BigDecimal.ZERO;
        java.math.BigDecimal wsOriginalInAmtTotal = java.math.BigDecimal.ZERO;
        String wsFlags = "";
        String wsControlCardFlg = "N";
        // 88 ccEof: wsControlCardFlg.equals("Y")
        String wsCorporateFlg = "N";
        // 88 corpEof: wsCorporateFlg.equals("Y")
        String wsMasterFlg = "N";
        // 88 masterEof: wsMasterFlg.equals("Y")
        String wsTrailerReached = "N";
        // 88 trailerReached: wsTrailerReached.equals("Y")
        String wsSysoutMessages = "";
        String wsMsg1 = "";
        String wsErrorMessages = "";
        String wsErrorMsg1 = "";
        String wsErrorMsg2 = "";
        String wsErrorMsg3 = "";
        String wsErrorMsg4 = "";
        String wsErrorMsg5 = "";
        String wsErrorMsg6 = "";
        String wsErrorMsg7 = "";
        String wsErrorMsg8 = "";
        String wsEojSysoutMessages = "";
        String wsEojMsg1 = "";
        String wsEojMsg2 = "";
        java.math.BigDecimal wsMsg2Cnt = java.math.BigDecimal.ZERO;
        String wsEojMsg3 = "";
        String wsEojMsg4 = "";
        java.math.BigDecimal wsMsg4Cnt = java.math.BigDecimal.ZERO;
        String wsEojMsg5 = "";
        String wsEojMsg6 = "";
        java.math.BigDecimal wsMsg6Cnt = java.math.BigDecimal.ZERO;
        String wsParagraphLits = "";
        String ws1000Init = "";
        String ws2000Main = "";
        String ws2200Proc = "";
        String ws9000Eoj = "";
        String wsMiscLits = "";
        String wsHighKeyLit = "";
        String wsLowKeyLit = "";
        String wsHeaderLit = "";
        int wsOneLit = 0;
        String wsMechLit = "";
        String ten99T01rCorporateRcd = "";
        String ten99M01rMasterInRcd = "";
        String ten99R01rControlRcd = "";
        String ten99M01wMasterOutRcd = "";

        // Auto-added fields
        String ccE01wDisplayRcd = "";
        String ccEof = "";
        String corpEof = "";
        String mastHeader = "";
        String masterEof = "";
        String sarParagraph = "";
        String scrCount = "";
        String sysoutCountRcd = "";
        String tmmid1099Amount = "";
        String tmmitDeleteTrailAmt = "";
        String tmmitNonDeleteTrailAmt = "";
        String tmmod1099Amount = "";
        String tmmotTrailAmt = "";
        String wsMastInAmt = "";
        String wsMastOutAmt = "";
        String wsMsg2Cnt = "";
        String wsMsg4Cnt = "";
        String wsMsg6Cnt = "";
        String wsOriginalInAmtTotal = "";
    }

    @Override
    public RepeatStatus execute(StepContribution contribution, ChunkContext chunkContext) throws Exception {
        ProgramState state = new ProgramState();
        mainline(state);
        return RepeatStatus.FINISHED;
    }

    // =========== FILE I/O ===========
    private void readTen99t01rcorporatefile(ProgramState state) {
        try {
            if (state.ten99T01rCorporateFileReader == null) { state.ten99T01rCorporateFileEof = true; return; }
            state.ten99T01rCorporateFileLine = state.ten99T01rCorporateFileReader.readLine();
            if (state.ten99T01rCorporateFileLine == null) state.ten99T01rCorporateFileEof = true;
        } catch (Exception e) { throw new RuntimeException("Read error", e); }
    }

    private void writeTen99t01rcorporatefile(ProgramState state, String line) {
        try {
            if (state.ten99T01rCorporateFileWriter != null && line != null) {
                state.ten99T01rCorporateFileWriter.write(line);
                state.ten99T01rCorporateFileWriter.newLine();
            }
        } catch (Exception e) { throw new RuntimeException("Write error", e); }
    }

    private void readTen99m01rmasterinfile(ProgramState state) {
        try {
            if (state.ten99M01rMasterInFileReader == null) { state.ten99M01rMasterInFileEof = true; return; }
            state.ten99M01rMasterInFileLine = state.ten99M01rMasterInFileReader.readLine();
            if (state.ten99M01rMasterInFileLine == null) state.ten99M01rMasterInFileEof = true;
        } catch (Exception e) { throw new RuntimeException("Read error", e); }
    }

    private void writeTen99m01rmasterinfile(ProgramState state, String line) {
        try {
            if (state.ten99M01rMasterInFileWriter != null && line != null) {
                state.ten99M01rMasterInFileWriter.write(line);
                state.ten99M01rMasterInFileWriter.newLine();
            }
        } catch (Exception e) { throw new RuntimeException("Write error", e); }
    }

    private void readTen99r01rcontrolfile(ProgramState state) {
        try {
            if (state.ten99R01rControlFileReader == null) { state.ten99R01rControlFileEof = true; return; }
            state.ten99R01rControlFileLine = state.ten99R01rControlFileReader.readLine();
            if (state.ten99R01rControlFileLine == null) state.ten99R01rControlFileEof = true;
        } catch (Exception e) { throw new RuntimeException("Read error", e); }
    }

    private void writeTen99r01rcontrolfile(ProgramState state, String line) {
        try {
            if (state.ten99R01rControlFileWriter != null && line != null) {
                state.ten99R01rControlFileWriter.write(line);
                state.ten99R01rControlFileWriter.newLine();
            }
        } catch (Exception e) { throw new RuntimeException("Write error", e); }
    }

    private void readTen99m01wmasteroutfile(ProgramState state) {
        try {
            if (state.ten99M01wMasterOutFileReader == null) { state.ten99M01wMasterOutFileEof = true; return; }
            state.ten99M01wMasterOutFileLine = state.ten99M01wMasterOutFileReader.readLine();
            if (state.ten99M01wMasterOutFileLine == null) state.ten99M01wMasterOutFileEof = true;
        } catch (Exception e) { throw new RuntimeException("Read error", e); }
    }

    private void writeTen99m01wmasteroutfile(ProgramState state, String line) {
        try {
            if (state.ten99M01wMasterOutFileWriter != null && line != null) {
                state.ten99M01wMasterOutFileWriter.write(line);
                state.ten99M01wMasterOutFileWriter.newLine();
            }
        } catch (Exception e) { throw new RuntimeException("Write error", e); }
    }

    // =========== BUSINESS LOGIC ===========

    private void mainline(ProgramState state) {
        // ===== Original COBOL: 0000-MAINLINE =====
        // 030900*=================================================================03090001
        // 031000*   PROGRAM CCAC6340 MAINLINE                                     03100001
        // 031100*   MAIN PROCESSING PARAGRAPH                                     03110001
        // 031200*=================================================================03120001
        // PERFORM 1000-INITIALIZATION
        // PERFORM 2000-MAIN-PROCESS UNTIL MASTER-EOF
        // PERFORM 9000-END-OF-JOB
        // 031600*OQ01 - "GOBACK" FLAGGED PER STOP-RUN OPTION                      03160001
        // GOBACK
        // .
        // EJECT
        // ===== End COBOL =====
        
        try {
            initialization(state);
            while (!state.masterEof) { mainProcess(state); }
            endOfJob(state);
        } catch (Exception e) {
            throw new RuntimeException("Error in mainline: " + e.getMessage(), e);
        }
    }


    private void initialization(ProgramState state) {
        // ===== Original COBOL: 1000-INITIALIZATION =====
        // 032200*=================================================================03220001
        // 032300*   THIS PARAGRAPH INITIALIZES THE READ AND COUNTERS AND CHECKS   03230001
        // 032400*   ALL HEADERS.                                                  03240001
        // 032500*=================================================================03250001
        // PERFORM 1010-OPEN-FILES
        // PERFORM 1020-INITIALIZE-SYSOUT
        // INITIALIZE WS-COUNTERS WS-ACCUMULATORS
        // INITIALIZE TEN99-M01W-MASTER-OUT-DTL
        // 033000*--------------------------------------------------------------   03300001
        // 033100*  READ OF THE CONTROL CARD                                       03310001
        // 033200*--------------------------------------------------------------   03320001
        // PERFORM 7000-READ-CONTROL-CARD
        // IF CC-EOF
        // MOVE WS-ERROR-MSG-1 TO CC-E01W-DISPLAY-RCD
        // MOVE WS-1000-INIT TO SAR-PARAGRAPH
        // PERFORM 8999-WRITE-SYSOUT
        // PERFORM 9998-COREDUMP
        // END-IF
        // 034000*--------------------------------------------------------------   03400001
        // 034100* INITIAL READS OF MASTER FILE FOR HEADER                         03410001
        // 034200*--------------------------------------------------------------   03420001
        // PERFORM 7200-READ-MASTER-FILE
        // IF MASTER-EOF
        // MOVE WS-ERROR-MSG-2 TO CC-E01W-DISPLAY-RCD
        // MOVE WS-1000-INIT TO SAR-PARAGRAPH
        // PERFORM 8999-WRITE-SYSOUT
        // PERFORM 9998-COREDUMP
        // ELSE
        // IF MAST-HEADER
        // IF TRCCD-CONTROL-DATE = TMMIH-HEAD-DATE
        // PERFORM 8000-WRITE-MASTER-OUT
        // PERFORM 7200-READ-MASTER-FILE
        // IF MASTER-EOF
        // MOVE WS-ERROR-MSG-5 TO CC-E01W-DISPLAY-RCD
        // MOVE WS-1000-INIT TO SAR-PARAGRAPH
        // PERFORM 8999-WRITE-SYSOUT
        // PERFORM 9998-COREDUMP
        // ELSE
        // CONTINUE
        // END-IF
        // ELSE
        // MOVE WS-ERROR-MSG-3 TO CC-E01W-DISPLAY-RCD
        // MOVE WS-1000-INIT TO SAR-PARAGRAPH
        // PERFORM 8999-WRITE-SYSOUT
        // PERFORM 9998-COREDUMP
        // END-IF
        // ELSE
        // MOVE WS-ERROR-MSG-4 TO CC-E01W-DISPLAY-RCD
        // MOVE WS-1000-INIT TO SAR-PARAGRAPH
        // PERFORM 8999-WRITE-SYSOUT
        // PERFORM 9998-COREDUMP
        // END-IF
        // END-IF
        // 037500*--------------------------------------------------------------   03750001
        // 037600*  INITIAL READ OF CORPORATE FILE FOR FIRST RECORD                03760001
        // 037700*--------------------------------------------------------------   03770001
        // PERFORM 7100-READ-CORPORATE-FILE
        // IF CORP-EOF
        // MOVE WS-MSG-1 TO CC-E01W-DISPLAY-RCD
        // PERFORM 8999-WRITE-SYSOUT
        // PERFORM 2200-PROCESS-ENTIRE-MASTER UNTIL MASTER-EOF
        // ELSE
        // CONTINUE
        // END-IF
        // .
        // EJECT
        // ===== End COBOL =====
        
        try {
            openFiles(state);
            initializeSysout(state);
            state.wsCounters = "";
            state.wsAccumulators = "";
            state.ten99M01wMasterOutDtl = "";
            readControlCard(state);
            if (state.ccEof) {
                state.ccE01wDisplayRcd = state.wsErrorMsg1;
                state.sarParagraph = state.ws1000Init;
                writeSysout(state);
                coreDump(state);
            }
            readMasterFile(state);
            if (state.masterEof) {
                state.ccE01wDisplayRcd = state.wsErrorMsg2;
                state.sarParagraph = state.ws1000Init;
                writeSysout(state);
                coreDump(state);
            } else {
                if (state.mastHeader) {
                    if (state.trccdControlDate.equals(state.tmmihHeadDate)) {
                        writeMasterOut(state);
                        readMasterFile(state);
                        if (state.masterEof) {
                            state.ccE01wDisplayRcd = state.wsErrorMsg5;
                            state.sarParagraph = state.ws1000Init;
                            writeSysout(state);
                            coreDump(state);
                        }
                    } else {
                        state.ccE01wDisplayRcd = state.wsErrorMsg3;
                        state.sarParagraph = state.ws1000Init;
                        writeSysout(state);
                        coreDump(state);
                    }
                } else {
                    state.ccE01wDisplayRcd = state.wsErrorMsg4;
                    state.sarParagraph = state.ws1000Init;
                    writeSysout(state);
                    coreDump(state);
                }
            }
            readCorporateFile(state);
            if (state.corpEof) {
                state.ccE01wDisplayRcd = state.wsMsg1;
                writeSysout(state);
                while (!state.masterEof) {
                    processEntireMaster(state);
                }
            }
        } catch (Exception e) {
            throw new RuntimeException("Error in initialization: " + e.getMessage(), e);
        }
    }


    private void openFiles(ProgramState state) {
        // ===== Original COBOL: 1010-OPEN-FILES =====
        // 039000*==============================================================*  03900001
        // 039100*    THIS PARAGRAPH OPENS ALL OF THE INPUT AND OUTPUT FILES.   *  03910001
        // 039200*==============================================================*  03920001
        // OPEN INPUT TEN99-M01R-MASTER-IN-FILE
        // TEN99-T01R-CORPORATE-FILE
        // TEN99-R01R-CONTROL-FILE
        // OPEN OUTPUT TEN99-M01W-MASTER-OUT-FILE
        // CC-E01W-DISPLAY-FILE
        // .
        // EJECT
        // ===== End COBOL =====
        
        try {
            openFiles(state);
        } catch (Exception e) {
            throw new RuntimeException("Error in openFiles: " + e.getMessage(), e);
        }
    }


    private void initializeSysout(ProgramState state) {
        // ===== Original COBOL: 1020-INITIALIZE-SYSOUT =====
        // 040200*=================================================================04020001
        // 040300*  THIS PARAGRAPH INITIALIZES THE SYSOUT DISPLAY AREA.            04030001
        // 040400*  THE CODE CAN BE FOUND IN COPYLIB C2INP001.                     04040001
        // 040500*=================================================================04050001
        // 040600*++INCLUDE C2INZ001                                               04060001
        // COPY C2INZ001.
        // EJECT
        // ===== End COBOL =====
        
        try {
            initializeSysout(state);
        } catch (Exception e) {
            throw new RuntimeException("Error in initializeSysout: " + e.getMessage(), e);
        }
    }


    private void mainProcess(ProgramState state) {
        // ===== Original COBOL: 2000-MAIN-PROCESS =====
        // 041100*=================================================================04110001
        // 041200*  THIS PARAGRAPH IS THE MAIN CONTROL OF THE PROGRAM PROCESSING.  04120001
        // 041300*  IT WILL BE EXECUTED UNTIL EOF OF THE MASTER FILE IS REACHED    04130001
        // 041400*=================================================================04140001
        // IF MAST-TRAILER
        // COMPUTE WS-ORIGINAL-IN-CNT-TOTAL =
        // (TMMIT-NON-DELETE-TRAIL-CNT + TMMIT-DELETE-TRAIL-CNT)
        // COMPUTE WS-ORIGINAL-IN-AMT-TOTAL =
        // (TMMIT-NON-DELETE-TRAIL-AMT + TMMIT-DELETE-TRAIL-AMT)
        // PERFORM 7200-READ-MASTER-FILE
        // ELSE
        // IF BUSINESS-OUTPUT
        // OR DELETE-OUTPUT
        // OR MECHANIZED-OUTPUT
        // ADD WS-ONE-LIT TO WS-MAST-IN-CNT
        // ADD TMMID-1099-AMOUNT TO WS-MAST-IN-AMT
        // PERFORM 8000-WRITE-MASTER-OUT
        // PERFORM 7200-READ-MASTER-FILE
        // ELSE
        // IF TMMID-SOCIAL-SEC-NUMBER = TTCD-SOCIAL-SEC-NUMBER
        // MOVE WS-MECH-LIT TO TMMID-DELETE-INDICATOR
        // ADD WS-ONE-LIT TO WS-MAST-IN-CNT
        // ADD TMMID-1099-AMOUNT TO WS-MAST-IN-AMT
        // PERFORM 8000-WRITE-MASTER-OUT
        // PERFORM 7200-READ-MASTER-FILE
        // ELSE
        // IF TMMID-SOCIAL-SEC-NUMBER < TTCD-SOCIAL-SEC-NUMBER
        // ADD WS-ONE-LIT TO WS-MAST-IN-CNT
        // ADD TMMID-1099-AMOUNT TO WS-MAST-IN-AMT
        // PERFORM 8000-WRITE-MASTER-OUT
        // PERFORM 7200-READ-MASTER-FILE
        // ELSE
        // PERFORM 7100-READ-CORPORATE-FILE
        // IF CORP-EOF
        // PERFORM 2200-PROCESS-ENTIRE-MASTER
        // UNTIL MASTER-EOF
        // ELSE
        // CONTINUE
        // END-IF
        // END-IF
        // END-IF
        // END-IF
        // END-IF
        // .
        // EJECT
        // ===== End COBOL =====
        
        try {
            if ("MAST-TRAILER".equals(state.wsMasterFlg)) {
                state.wsOriginalInCntTotal = state.tmmitNonDeleteTrailCnt + state.tmmitDeleteTrailCnt;
                state.wsOriginalInAmtTotal = state.tmmitNonDeleteTrailAmt.add(state.tmmitDeleteTrailAmt);
                readMasterFile(state);
            } else {
                if ("BUSINESS-OUTPUT".equals(state.wsMasterFlg)
                    || "DELETE-OUTPUT".equals(state.wsMasterFlg)
                    || "MECHANIZED-OUTPUT".equals(state.wsMasterFlg)) {
                    state.wsMastInCnt = state.wsMastInCnt + 1;
                    state.wsMastInAmt = state.wsMastInAmt.add(state.tmmid1099Amount);
                    writeMasterOut(state);
                    readMasterFile(state);
                } else {
                    if (state.tmmidSocialSecNumber == state.ttcdSocialSecNumber) {
                        state.tmmidDeleteIndicator = "MECH";
                        state.wsMastInCnt = state.wsMastInCnt + 1;
                        state.wsMastInAmt = state.wsMastInAmt.add(state.tmmid1099Amount);
                        writeMasterOut(state);
                        readMasterFile(state);
                    } else {
                        if (state.tmmidSocialSecNumber < state.ttcdSocialSecNumber) {
                            state.wsMastInCnt = state.wsMastInCnt + 1;
                            state.wsMastInAmt = state.wsMastInAmt.add(state.tmmid1099Amount);
                            writeMasterOut(state);
                            readMasterFile(state);
                        } else {
                            readCorporateFile(state);
                            if ("CORP-EOF".equals(state.wsCorporateFlg)) {
                                while (!"MASTER-EOF".equals(state.wsMasterFlg)) {
                                    processEntireMaster(state);
                                }
                            } else {
                                // CONTINUE
                            }
                        }
                    }
                }
            }
        } catch (Exception e) {
            throw new RuntimeException("Error in mainProcess: " + e.getMessage(), e);
        }
    }


    private void processEntireMaster(ProgramState state) {
        // ===== Original COBOL: 2200-PROCESS-ENTIRE-MASTER =====
        // 045800*=================================================================04580001
        // 045900*  THIS PARAGRAPH READS AND WRITES THE UNCHANGED MASTER RECORDS   04590001
        // 046000*  WHEN THE END OF THE CORPORATE FILE RECORDS ARE REACHED.        04600001
        // 046100*=================================================================04610001
        // IF MAST-TRAILER
        // COMPUTE WS-ORIGINAL-IN-CNT-TOTAL =
        // (TMMIT-NON-DELETE-TRAIL-CNT + TMMIT-DELETE-TRAIL-CNT)
        // COMPUTE WS-ORIGINAL-IN-AMT-TOTAL =
        // (TMMIT-NON-DELETE-TRAIL-AMT + TMMIT-DELETE-TRAIL-AMT)
        // ELSE
        // ADD WS-ONE-LIT TO WS-MAST-IN-CNT
        // ADD TMMID-1099-AMOUNT TO WS-MAST-IN-AMT
        // PERFORM 8000-WRITE-MASTER-OUT
        // END-IF
        // PERFORM 7200-READ-MASTER-FILE
        // .
        // EJECT
        // ===== End COBOL =====
        
        try {
            if ("MAST-TRAILER".equals(state.tmmidDisbEntryCode)) {
                state.wsOriginalInCntTotal = state.tmmitNonDeleteTrailCnt + state.tmmitDeleteTrailCnt;
                state.wsOriginalInAmtTotal = state.tmmitNonDeleteTrailAmt.add(state.tmmitDeleteTrailAmt);
            } else {
                state.wsMastInCnt = state.wsMastInCnt + 1;
                state.wsMastInAmt = state.wsMastInAmt.add(state.tmmid1099Amount);
                writeMasterOut(state);
            }
            readMasterFile(state);
        } catch (Exception e) {
            throw new RuntimeException("Error in processEntireMaster: " + e.getMessage(), e);
        }
    }


    private void readControlCard(ProgramState state) {
        // ===== Original COBOL: 7000-READ-CONTROL-CARD =====
        // 047700*=================================================================04770001
        // 047800*  THIS PARAGRAPH READS THE CONTROL CARD.                         04780001
        // 047900*=================================================================04790001
        // READ TEN99-R01R-CONTROL-FILE
        // INTO TEN99-R01R-CONTROL-CARD-DTL
        // AT END
        // SET CC-EOF TO TRUE
        // END-READ
        // .
        // EJECT
        // ===== End COBOL =====
        
        try {
            readTen99r01rcontrolfile(state);
            if (state.ten99R01rControlCardDtl == null) {
                state.wsControlCardFlg = "EOF";
            }
        } catch (Exception e) {
            throw new RuntimeException("Error in readControlCard: " + e.getMessage(), e);
        }
    }


    private void readCorporateFile(ProgramState state) {
        // ===== Original COBOL: 7100-READ-CORPORATE-FILE =====
        // 048900*=================================================================04890001
        // 049000*  THIS PARAGRAPH READS THE CORPORATE FILE                        04900001
        // 049100*=================================================================04910001
        // READ TEN99-T01R-CORPORATE-FILE
        // INTO TEN99-T01R-CORPORATE-DTL
        // AT END
        // SET CORP-EOF TO TRUE
        // END-READ
        // .
        // EJECT
        // ===== End COBOL =====
        
        try {
            readTen99t01rcorporatefile(state);
            if (state.ten99T01rCorporateDtl == null) {
                state.wsCorporateFlg = "TRUE";
            }
        } catch (Exception e) {
            throw new RuntimeException("Error in readCorporateFile: " + e.getMessage(), e);
        }
    }


    private void readMasterFile(ProgramState state) {
        // ===== Original COBOL: 7200-READ-MASTER-FILE =====
        // 050100*=================================================================05010001
        // 050200*  THIS PARAGRAPH READS THE MASTER FILE                           05020001
        // 050300*=================================================================05030001
        // READ TEN99-M01R-MASTER-IN-FILE
        // INTO TEN99-M01R-MASTER-IN-DTL
        // AT END
        // SET MASTER-EOF TO TRUE
        // END-READ
        // .
        // EJECT
        // ===== End COBOL =====
        
        try {
            readTen99m01rmasterinfile(state);
            if (state.ten99M01rMasterInDtl == null) {
                state.wsMasterFlg = "TRUE";
            }
        } catch (Exception e) {
            throw new RuntimeException("Error in readMasterFile: " + e.getMessage(), e);
        }
    }


    private void writeMasterOut(ProgramState state) {
        // ===== Original COBOL: 8000-WRITE-MASTER-OUT =====
        // 051300*=================================================================05130001
        // 051400*  THIS PARAGRAPH WRITES THE MASTER FILE RECORD                   05140001
        // 051500*=================================================================05150001
        // MOVE TEN99-M01R-MASTER-IN-DTL
        // TO TEN99-M01W-MASTER-OUT-DTL
        // WRITE TEN99-M01W-MASTER-OUT-RCD
        // FROM TEN99-M01W-MASTER-OUT-DTL
        // IF MAST-OUT-HEAD OR MAST-OUT-TRAILER
        // CONTINUE
        // ELSE
        // ADD WS-ONE-LIT TO WS-MAST-OUT-CNT
        // ADD TMMOD-1099-AMOUNT TO WS-MAST-OUT-AMT
        // END-IF
        // INITIALIZE TEN99-M01W-MASTER-OUT-DTL
        // .
        // ===== End COBOL =====
        
        try {
            state.ten99M01wMasterOutDtl = state.ten99M01rMasterInDtl;
            writeTen99m01wmasteroutfile(state);
            if (state.wsMasterFlg.equals("HEAD") || state.wsMasterFlg.equals("TRAILER")) {
                // CONTINUE
            } else {
                state.wsMastOutCnt = state.wsMastOutCnt + 1;
                state.wsMastOutAmt = state.wsMastOutAmt.add(state.tmmod1099Amount);
            }
            state.ten99M01wMasterOutDtl = "";
        } catch (Exception e) {
            throw new RuntimeException("Error in writeMasterOut: " + e.getMessage(), e);
        }
    }


    private void writeNewTrailerOut(ProgramState state) {
        // ===== Original COBOL: 8500-WRITE-NEW-TRAILER-OUT =====
        // 053100*=================================================================05310001
        // 053200*  THIS PARAGRAPH WRITES THE NEW MASTER TRAILER RECORD            05320001
        // 053300*=================================================================05330001
        // WRITE TEN99-M01W-MASTER-OUT-RCD
        // FROM TEN99-M01W-MASTER-OUT-DTL
        // INITIALIZE TEN99-M01W-MASTER-OUT-DTL
        // .
        // 053900*8999-WRITE-SYSOUT.                                               05390001
        // 054000*=================================================================05400001
        // 054100*  THIS PARAGRAPH WRITE THE SYSOUT MESSAGES                       05410001
        // 054200*=================================================================05420001
        // 054300*++INCLUDE C2INZ002                                               05430001
        // COPY C2INZ002.
        // EJECT
        // 054600*    .                                                            05460001
        // ===== End COBOL =====
        
        try {
            writeTen99m01wmasteroutfile(state);
            state.ten99M01wMasterOutDtl = "";
        } catch (Exception e) {
            throw new RuntimeException("Error in writeNewTrailerOut: " + e.getMessage(), e);
        }
    }


    private void endOfJob(ProgramState state) {
        // ===== Original COBOL: 9000-END-OF-JOB =====
        // 054800*=================================================================05480001
        // 054900*  THIS PARAGRAPH COMPARES TOTAL COUNTS AND AMOUNTS FROM MASTER   05490001
        // 055000*  TRAILER RECORD WITH CCAC6340INPUT/OUTPUT COUNTS AND AMOUNTS    05500001
        // 055100*=================================================================05510001
        // IF WS-MAST-IN-CNT = WS-ORIGINAL-IN-CNT-TOTAL
        // AND WS-MAST-IN-AMT = WS-ORIGINAL-IN-AMT-TOTAL
        // AND WS-MAST-IN-CNT = WS-MAST-OUT-CNT
        // AND WS-MAST-IN-AMT = WS-MAST-OUT-AMT
        // MOVE HIGH-VALUES TO TMMOT-TRAIL-SOURCE
        // MOVE WS-MAST-OUT-CNT TO TMMOT-TRAIL-CNT
        // MOVE WS-MAST-OUT-AMT TO TMMOT-TRAIL-AMT
        // PERFORM 8500-WRITE-NEW-TRAILER-OUT
        // PERFORM 9010-WRAP-UP
        // PERFORM 9999-CLOSE-FILES
        // ELSE
        // MOVE WS-ERROR-MSG-7 TO CC-E01W-DISPLAY-RCD
        // MOVE WS-9000-EOJ TO SAR-PARAGRAPH
        // PERFORM 8999-WRITE-SYSOUT
        // PERFORM 9010-WRAP-UP
        // PERFORM 9998-COREDUMP
        // END-IF
        // .
        // EJECT
        // ===== End COBOL =====
        
        try {
            if (state.wsMastInCnt == state.wsOriginalInCntTotal
                && state.wsMastInAmt.equals(state.wsOriginalInAmtTotal)
                && state.wsMastInCnt == state.wsMastOutCnt
                && state.wsMastInAmt.equals(state.wsMastOutAmt)) {
                state.tmmotTrailSource = "\uFFFF";
                state.tmmotTrailCnt = state.wsMastOutCnt;
                state.tmmotTrailAmt = state.wsMastOutAmt;
                writeNewTrailerOut(state);
                wrapUp(state);
                endOfJob(state);
            } else {
                state.ccE01wDisplayRcd = state.wsErrorMsg7;
                state.sarParagraph = state.ws9000Eoj;
                initializeSysout(state);
                wrapUp(state);
                // 9998-COREDUMP not defined, so omitted
            }
        } catch (Exception e) {
            throw new RuntimeException("Error in endOfJob: " + e.getMessage(), e);
        }
    }


    private void wrapUp(ProgramState state) {
        // ===== Original COBOL: 9010-WRAP-UP =====
        // 057300*=================================================================05730001
        // 057400*  THIS PARAGRAPH WRITE FINAL MESSAGES TO THE SYSOUT              05740001
        // 057500*=================================================================05750001
        // 057600*  THE FIRST SIX MESSAGES DISPLAY THE RECORD COUNTS ON THE SYSOUT 05760001
        // 057700*-----------------------------------------------------------------05770001
        // MOVE WS-EOJ-MSG-1 TO CC-E01W-DISPLAY-RCD
        // PERFORM 8999-WRITE-SYSOUT
        // MOVE WS-ORIGINAL-IN-CNT-TOTAL TO SCR-COUNT
        // MOVE SYSOUT-COUNT-RCD TO CC-E01W-DISPLAY-RCD
        // PERFORM 8999-WRITE-SYSOUT
        // MOVE WS-EOJ-MSG-3 TO CC-E01W-DISPLAY-RCD
        // PERFORM 8999-WRITE-SYSOUT
        // MOVE WS-MAST-IN-CNT TO SCR-COUNT
        // MOVE SYSOUT-COUNT-RCD TO CC-E01W-DISPLAY-RCD
        // PERFORM 8999-WRITE-SYSOUT
        // MOVE WS-EOJ-MSG-5 TO CC-E01W-DISPLAY-RCD
        // PERFORM 8999-WRITE-SYSOUT
        // MOVE WS-MAST-OUT-CNT TO SCR-COUNT
        // MOVE SYSOUT-COUNT-RCD TO CC-E01W-DISPLAY-RCD
        // PERFORM 8999-WRITE-SYSOUT
        // 059300*=================================================================05930001
        // 059400*  THE NEXT SIX MESSAGES DISPLAY THE TOTAL AMOUNTS ON THE SYSOUT  05940001
        // 059500*-----------------------------------------------------------------05950001
        // MOVE WS-ORIGINAL-IN-AMT-TOTAL TO WS-MSG-2-CNT
        // MOVE WS-EOJ-MSG-2 TO CC-E01W-DISPLAY-RCD
        // PERFORM 8999-WRITE-SYSOUT
        // MOVE WS-MAST-IN-AMT TO WS-MSG-4-CNT
        // MOVE WS-EOJ-MSG-4 TO CC-E01W-DISPLAY-RCD
        // PERFORM 8999-WRITE-SYSOUT
        // MOVE WS-MAST-OUT-AMT TO WS-MSG-6-CNT
        // MOVE WS-EOJ-MSG-6 TO CC-E01W-DISPLAY-RCD
        // PERFORM 8999-WRITE-SYSOUT
        // 060500*++INCLUDE C2INZ003                                               06050001
        // COPY C2INZ003.
        // EJECT
        // 061000*9998-COREDUMP.                                                   06100001
        // 061100*=================================================================06110001
        // 061200*  THIS PARAGRAPH CALLS COREDUMP TO STOP PROCESSING               06120001
        // 061300*=================================================================06130001
        // 061400*++INCLUDE C2INZ004                                               06140001
        // COPY C2INZ004.
        // 061700*9999-CLOSE-FILES.                                                06170001
        // 061800*=================================================================06180001
        // 061900*  THIS PARAGRAPH CLOSES ALL FILES                                06190001
        // 062000*=================================================================06200001
        // 062100*++INCLUDE C2INZ005                                               06210001
        // COPY C2INZ005.
        // CLOSE TEN99-M01R-MASTER-IN-FILE
        // CLOSE TEN99-M01W-MASTER-OUT-FILE
        // CLOSE TEN99-R01R-CONTROL-FILE
        // CLOSE TEN99-T01R-CORPORATE-FILE
        // .
        // END PROGRAM CCAC6340.
        // 
        // ===== End COBOL =====
        
        try {
            state.ccE01wDisplayRcd = state.wsEojMsg1;
            writeSysout(state);
            state.scrCount = state.wsOriginalInCntTotal;
            state.ccE01wDisplayRcd = state.sysoutCountRcd;
            writeSysout(state);
            state.ccE01wDisplayRcd = state.wsEojMsg3;
            writeSysout(state);
            state.scrCount = state.wsMastInCnt;
            state.ccE01wDisplayRcd = state.sysoutCountRcd;
            writeSysout(state);
            state.ccE01wDisplayRcd = state.wsEojMsg5;
            writeSysout(state);
            state.scrCount = state.wsMastOutCnt;
            state.ccE01wDisplayRcd = state.sysoutCountRcd;
            writeSysout(state);
            state.wsMsg2Cnt = state.wsOriginalInAmtTotal;
            state.ccE01wDisplayRcd = state.wsEojMsg2;
            writeSysout(state);
            state.wsMsg4Cnt = state.wsMastInAmt;
            state.ccE01wDisplayRcd = state.wsEojMsg4;
            writeSysout(state);
            state.wsMsg6Cnt = state.wsMastOutAmt;
            state.ccE01wDisplayRcd = state.wsEojMsg6;
            writeSysout(state);
            c2inz003(state);
            c2inz004(state);
            c2inz005(state);
            closeTen99m01rmasterinfile(state);
            closeTen99m01wmasteroutfile(state);
            closeTen99r01rcontrolfile(state);
            closeTen99t01rcorporatefile(state);
        } catch (Exception e) {
            throw new RuntimeException("Error in wrapUp: " + e.getMessage(), e);
        }
    }

}