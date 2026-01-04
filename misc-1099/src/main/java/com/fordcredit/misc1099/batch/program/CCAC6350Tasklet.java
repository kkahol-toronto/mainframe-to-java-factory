package com.fordcredit.misc1099.batch.program;

import org.springframework.batch.core.StepContribution;
import org.springframework.batch.core.scope.context.ChunkContext;
import org.springframework.batch.core.step.tasklet.Tasklet;
import org.springframework.batch.repeat.RepeatStatus;
import java.io.*;
import java.nio.file.*;
import java.math.BigDecimal;

public class CCAC6350Tasklet implements Tasklet {

    private final String basePath;

    public CCAC6350Tasklet(String basePath) {
        this.basePath = basePath;
    }

    // =========== PROGRAM STATE ===========
    static class ProgramState {
        // File I/O
        BufferedReader ten99M01r1099MasterFileReader;
        String ten99M01r1099MasterFileLine;
        boolean ten99M01r1099MasterFileEof = false;
        BufferedWriter ten99M01r1099MasterFileWriter;
        BufferedReader ten99R01rControlCardReader;
        String ten99R01rControlCardLine;
        boolean ten99R01rControlCardEof = false;
        BufferedWriter ten99R01rControlCardWriter;
        BufferedReader ten99C01w1099SummaryFileReader;
        String ten99C01w1099SummaryFileLine;
        boolean ten99C01w1099SummaryFileEof = false;
        BufferedWriter ten99C01w1099SummaryFileWriter;
        BufferedReader ccE01wDisplayFileReader;
        String ccE01wDisplayFileLine;
        boolean ccE01wDisplayFileEof = false;
        BufferedWriter ccE01wDisplayFileWriter;

        // WORKING-STORAGE variables
        String panValet = "002CCAC6350";
        String pups2000 = "";
        String wsM01rMasterFile = "";
        String wsM01rCheckNumber = "";
        // 88 masterHeader: wsM01rCheckNumber.equals("LOW-VALUES")
        // 88 masterTrailer: wsM01rCheckNumber.equals("HIGH-VALUES")
        String wsM01rChk = "";
        String wsM01rCheckType = "";
        String wsM01rBrDept = "";
        int wsM01rSocSecNum = 0;
        int wsM01rSocSecRem = 0;
        int wsM01rOperLocCode = 0;
        String wsM01rSource = "";
        String wsM01rEmpName = "";
        String wsM01rAddress1 = "";
        String wsM01rAddress2 = "";
        String wsM01rAddr2 = "";
        String wsM01rAddr2Out = "";
        String wsM01rAddrOvfl = "";
        String wsM01rCity = "";
        String wsM01rState = "";
        int wsM01rZip = 0;
        String wsM01rDeleteInd = "";
        java.math.BigDecimal wsM01rTotAmount = java.math.BigDecimal.ZERO;
        java.math.BigDecimal wsM01r1099Amount = java.math.BigDecimal.ZERO;
        String wsM01r1099Amt = "";
        int wsM01r1099AmtOvfl = 0;
        java.math.BigDecimal wsM01r1099AmtOut = java.math.BigDecimal.ZERO;
        int wsM01rCheckDate = 0;
        int wsM01rMonthendDate = 0;
        int wsM01rDistRecNum = 0;
        String[] wsM01rDistRec = new String[5];
        String wsM01rCompassCode = "";
        String wsM01rDist1099Ind = "";
        java.math.BigDecimal wsM01rDistAmount = java.math.BigDecimal.ZERO;
        String wsM01rComments = "";
        String wsM01rLineNbr = "";
        String wsM01rTaxType = "";
        String wsM01rTinInd = "";
        String wsPrevMasterFile = "";
        String wsPrevCheckNumber = "";
        String wsPrevChk = "";
        String wsPrevCheckType = "";
        String wsPrevBrDept = "";
        int wsPrevSocSecNum = 0;
        int wsPrevSocSecRem = 0;
        int wsPrevOperLocCode = 0;
        String wsPrevSource = "";
        String wsPrevEmpName = "";
        String wsPrevAddress1 = "";
        String wsPrevAddress2 = "";
        String wsPrevAddr2 = "";
        String wsPrevAddr2Out = "";
        String wsPrevAddrOvfl = "";
        String wsPrevCity = "";
        String wsPrevState = "";
        int wsPrevZip = 0;
        String wsPrevDeleteInd = "";
        java.math.BigDecimal wsPrevTotAmount = java.math.BigDecimal.ZERO;
        java.math.BigDecimal wsPrev1099Amount = java.math.BigDecimal.ZERO;
        String wsPrev1099Amt = "";
        int wsPrev1099AmtOvfl = 0;
        java.math.BigDecimal wsPrev1099AmtOut = java.math.BigDecimal.ZERO;
        int wsPrevCheckDate = 0;
        int wsPrevMonthendDate = 0;
        int wsPrevDistRecNum = 0;
        String[] wsPrevDistRec = new String[5];
        String wsPrevCompassCode = "";
        String wsPrevDist1099Ind = "";
        java.math.BigDecimal wsPrevDistAmount = java.math.BigDecimal.ZERO;
        String wsPrevComments = "";
        String wsPrevLineNbr = "";
        String wsPrevMstrTaxType = "";
        String wsPrevMstrTinInd = "";
        String wsDate = "";
        int wsDateMonth = 0;
        int wsDateDay = 0;
        int wsDateYear = 0;
        String wsC01wOutputFile = "";
        int wsC01wOperLocCode = 0;
        String wsC01wType = "";
        int wsC01wSocSecNum = 0;
        String wsC01wReferenceNum = "";
        java.math.BigDecimal wsC01wTotAmount = java.math.BigDecimal.ZERO;
        int wsC01wPayYear = 0;
        String wsC01wEmpName = "";
        String wsC01wAddress1 = "";
        String wsC01wAddress2 = "";
        String wsC01wCityMaster = "";
        String wsC01wCityOvfl = "";
        String wsC01wState = "";
        int wsC01wZip = 0;
        String wsC01wDescription = "";
        String wsC01wDescBrDept = "";
        String wsC01wDescCheckNum = "";
        String wsC01wDescNum = "";
        String wsC01wFormNum = "";
        String wsC01wComments = "";
        int wsC01wAmountInd = 0;
        String wsC01wSsnInd = "";
        String wsC01wDealerCode = "";
        String wsC01wTransCode = "";
        String wsC01wOutputTrailer = "";
        int wsC01wTrlrOperLocCode = 0;
        int wsC01wTrlrBatchNum = 0;
        int wsC01wTrlrTotRecords = 0;
        java.math.BigDecimal wsC01wTrlrTotAmount = java.math.BigDecimal.ZERO;
        String wsC01wTrlrTransCode = "";
        String wsR01rControlCard = "";
        String wsR01rCcRunDate = "";
        String wsR01rCcCc = "";
        String wsR01rCcYy = "";
        String wsR01rCcMm = "";
        String wsR01rCcDd = "";
        String wsCounters = "";
        int wsTotRecdsRead = 0;
        int wsTotWrittenCnt = 0;
        int ws1099TrlrCnt = 0;
        java.math.BigDecimal wsTot1099Amt = new java.math.BigDecimal("ZERO");
        java.math.BigDecimal ws1099TrlrAmount = new java.math.BigDecimal("ZERO");
        int wsTotBatchNum = 0;
        String wsSwitches = "";
        String wsEofSw = "SPACE";
        // 88 endOfInput: wsEofSw.equals("Y")
        String wsRecKeyArea = "";
        String wsCurrRecKey = "";
        String wsCurrTaxid = "";
        String wsCurrTaxType = "";
        String wsCurrTaxInd = "";
        String wsPrevRecKey = "";
        String wsPrevTaxid = "";
        String wsPrevTaxType = "";
        String wsPrevTinInd = "";
        String wsParagraphLiterals = "";
        int ws99Lit = 99;
        int ws1Lit = 1;
        int ws7Lit = 7;
        String wsMiscAlphaLiterals = "";
        String wsYesLit = "Y";
        String wsNoLit = "N";
        String wsManualCheckLit = "M";
        String ws1xLit = "1";
        String ws2Lit = "2";
        String wsTLit = "T";
        String wsALit = "A";
        String ws20071Lit = "";
        String wsSpaces4Lit = "";
        String wsSpaces5Lit = "";
        int wsZeroes4Lit = 000;
        int wsZeroes5Lit = 0000;
        String wsAllAsterisks = "ALL";
        String wsSourceSystemLit = "1099";
        String wsErrorMessLiterals = "";
        String wsEmptyCcrdLit = "";
        String wsEmptyFileLit = "";
        String wsMoneyErrorLit = "";
        String wsZipZeroesLit = "";
        String wsZipSpacesLit = "";
        String wsEojMessLiterals = "";
        String wsEojMsg1 = "";
        String wsEojMsg2 = "";
        String wsMiscArea = "";
        String wsOutputRecordLine = "";
        java.math.BigDecimal wsCalcTotAmount = java.math.BigDecimal.ZERO;
        String wsCalcTotAmt = "";
        int wsCalcTotAmtOvfl = 0;
        java.math.BigDecimal wsCalcTotAmtOut = java.math.BigDecimal.ZERO;
        java.math.BigDecimal wsManualTotAmount = java.math.BigDecimal.ZERO;
        String wsManualTotAmt = "";
        int wsManualTotAmtOvf = 0;
        java.math.BigDecimal wsManualTotAmtOut = java.math.BigDecimal.ZERO;
        int wsReferenceNum = 0;
        String wsZipAreas = "";
        String wsZipCheck = "";
        String wsZipCheck1 = "";
        int wsZipCheckNum = 0;
        String wsEvalNumZip = "";
        int wsEvalNumZip4 = 0;
        int wsEvalNumZip5 = 0;
        String wsEvalNumZipSec = "";
        int wsEvalNum2Zip5 = 0;
        int wsEvalNum2Zip4 = 0;
        String wsEvalAlphaZip = "";
        int wsEvalAlphaZip4 = 0;
        int wsEvalAlphaZip5 = 0;
        String wsEvalAlphaZipSec = "";
        int wsEvalAlpha2Zip5 = 0;
        int wsEvalAlpha2Zip4 = 0;
        String wsZipOk = "";
        String wsZipOk5 = "";
        String wsZipOk4 = "";
        String wsZipOkNum = "";
        int wsZipOkNum5 = 0;
        int wsZipOkNum4 = 0;
        String ten99M01r1099MasterRcd = "";
        String ten99C01w1099SummaryRcd = "";
        String ten99R01rControlCardRcd = "";
        String ccE01wDisplayRcd = "";

        // Auto-added fields
        String atEndTen99r01rcontrolcard = "";
        String endOfInput = "";
        String masterHeader = "";
        String masterTrailer = "";
        String returnCode = "";
        String scrCount = "";
        String swaCurrDa = "";
        String swaCurrMo = "";
        String swaCurrYr = "";
        String sysoutCountRcd = "";
    }

    @Override
    public RepeatStatus execute(StepContribution contribution, ChunkContext chunkContext) throws Exception {
        ProgramState state = new ProgramState();
        mainline(state);
        return RepeatStatus.FINISHED;
    }

    // =========== FILE I/O ===========
    private void readTen99m01r1099masterfile(ProgramState state) {
        try {
            if (state.ten99M01r1099MasterFileReader == null) { state.ten99M01r1099MasterFileEof = true; return; }
            state.ten99M01r1099MasterFileLine = state.ten99M01r1099MasterFileReader.readLine();
            if (state.ten99M01r1099MasterFileLine == null) state.ten99M01r1099MasterFileEof = true;
        } catch (Exception e) { throw new RuntimeException("Read error", e); }
    }

    private void writeTen99m01r1099masterfile(ProgramState state, String line) {
        try {
            if (state.ten99M01r1099MasterFileWriter != null && line != null) {
                state.ten99M01r1099MasterFileWriter.write(line);
                state.ten99M01r1099MasterFileWriter.newLine();
            }
        } catch (Exception e) { throw new RuntimeException("Write error", e); }
    }

    private void readTen99r01rcontrolcard(ProgramState state) {
        try {
            if (state.ten99R01rControlCardReader == null) { state.ten99R01rControlCardEof = true; return; }
            state.ten99R01rControlCardLine = state.ten99R01rControlCardReader.readLine();
            if (state.ten99R01rControlCardLine == null) state.ten99R01rControlCardEof = true;
        } catch (Exception e) { throw new RuntimeException("Read error", e); }
    }

    private void writeTen99r01rcontrolcard(ProgramState state, String line) {
        try {
            if (state.ten99R01rControlCardWriter != null && line != null) {
                state.ten99R01rControlCardWriter.write(line);
                state.ten99R01rControlCardWriter.newLine();
            }
        } catch (Exception e) { throw new RuntimeException("Write error", e); }
    }

    private void readTen99c01w1099summaryfile(ProgramState state) {
        try {
            if (state.ten99C01w1099SummaryFileReader == null) { state.ten99C01w1099SummaryFileEof = true; return; }
            state.ten99C01w1099SummaryFileLine = state.ten99C01w1099SummaryFileReader.readLine();
            if (state.ten99C01w1099SummaryFileLine == null) state.ten99C01w1099SummaryFileEof = true;
        } catch (Exception e) { throw new RuntimeException("Read error", e); }
    }

    private void writeTen99c01w1099summaryfile(ProgramState state, String line) {
        try {
            if (state.ten99C01w1099SummaryFileWriter != null && line != null) {
                state.ten99C01w1099SummaryFileWriter.write(line);
                state.ten99C01w1099SummaryFileWriter.newLine();
            }
        } catch (Exception e) { throw new RuntimeException("Write error", e); }
    }

    private void readCce01wdisplayfile(ProgramState state) {
        try {
            if (state.ccE01wDisplayFileReader == null) { state.ccE01wDisplayFileEof = true; return; }
            state.ccE01wDisplayFileLine = state.ccE01wDisplayFileReader.readLine();
            if (state.ccE01wDisplayFileLine == null) state.ccE01wDisplayFileEof = true;
        } catch (Exception e) { throw new RuntimeException("Read error", e); }
    }

    private void writeCce01wdisplayfile(ProgramState state, String line) {
        try {
            if (state.ccE01wDisplayFileWriter != null && line != null) {
                state.ccE01wDisplayFileWriter.write(line);
                state.ccE01wDisplayFileWriter.newLine();
            }
        } catch (Exception e) { throw new RuntimeException("Write error", e); }
    }

    // =========== BUSINESS LOGIC ===========

    private void mainline(ProgramState state) {
        // ===== Original COBOL: 0000-MAINLINE =====
        // 039200***************************************************************** 03920001
        // 039300*                                                               * 03930001
        // 039400* THIS PARAGRAPH CONTROLS MAIN PROCESSING LOGIC                 * 03940001
        // 039500*                                                               * 03950001
        // 039600* PERFORMS:  1000-GENERAL-HOUSEKEEPING                          * 03960001
        // 039700*            3000-PROCESS-MASTER-FILE                           * 03970001
        // 039800*            9000-CLOSING-ROUTINE                               * 03980001
        // 039900*                                                               * 03990001
        // 040000***************************************************************** 04000001
        // PERFORM 1000-GENERAL-HOUSEKEEPING
        // PERFORM 3000-PROCESS-MASTER-FILE  UNTIL
        // END-OF-INPUT
        // PERFORM 9000-CLOSING-ROUTINE
        // 040900*OQ01 - "GOBACK" FLAGGED PER STOP-RUN OPTION                      04090001
        // GOBACK
        // .
        // EJECT
        // 041300***************************************************************** 04130001
        // 041400* 1000-GENERAL-HOUSEKEEPING.                                      04140001
        // 041500*                                                                 04150001
        // 041600* OPEN AND INITIALIZE FILES.  DO FIRST READ ON MASTER FILE.       04160001
        // 041700*                                                                 04170001
        // 041800* PERFORMED FROM 0000-MAINLINE.                                   04180001
        // 041900***************************************************************** 04190001
        // ===== End COBOL =====
        
        try {
            generalHousekeeping(state);
            while (!Boolean.TRUE.equals(state.endOfInput)) { processMasterFile(state); }
            closingRoutine(state);
        } catch (Exception e) {
            throw new RuntimeException("Error in mainline: " + e.getMessage(), e);
        }
    }


    private void generalHousekeeping(ProgramState state) {
        // ===== Original COBOL: 1000-GENERAL-HOUSEKEEPING =====
        // PERFORM 1400-OPEN-FILES
        // PERFORM 1600-INITIALIZATION
        // MOVE SWA-CURR-MO                  TO WS-DATE-MONTH
        // MOVE SWA-CURR-DA                  TO WS-DATE-DAY
        // MOVE SWA-CURR-YR                  TO WS-DATE-YEAR
        // PERFORM 7500-READ-MASTER-FILE
        // IF END-OF-INPUT
        // MOVE WS-EMPTY-FILE-LIT         TO CC-E01W-DISPLAY-RCD
        // MOVE WS-99-LIT                 TO RETURN-CODE
        // PERFORM 8999-WRITE-SYSOUT
        // PERFORM 9998-COREDUMP
        // ELSE
        // IF MASTER-HEADER
        // PERFORM 7500-READ-MASTER-FILE
        // IF END-OF-INPUT
        // MOVE WS-EMPTY-FILE-LIT  TO CC-E01W-DISPLAY-RCD
        // MOVE WS-99-LIT          TO RETURN-CODE
        // PERFORM 8999-WRITE-SYSOUT
        // PERFORM 9998-COREDUMP
        // END-IF
        // END-IF
        // END-IF
        // MOVE WS-M01R-MASTER-FILE   TO WS-PREV-MASTER-FILE
        // PERFORM 3900-SET-RECORD-KEY
        // .
        // EJECT
        // 045100***************************************************************** 04510001
        // 045200* 1400-OPEN-FILES.                                                04520001
        // 045300*                                                                 04530001
        // 045400* OPEN FILES.                                                     04540001
        // 045500*                                                                 04550001
        // 045600* PEFORMED FROM 1000-GENERAL-HOUSEKEEPING.                        04560001
        // 045700***************************************************************** 04570001
        // ===== End COBOL =====
        
        try {
            openFiles(state);
            initialization(state);
            state.wsDateMonth = Integer.parseInt(state.swaCurrMo);
            state.wsDateDay = Integer.parseInt(state.swaCurrDa);
            state.wsDateYear = Integer.parseInt(state.swaCurrYr);
            readMasterFile(state);
            if (Boolean.TRUE.equals(state.endOfInput)) {
                state.ccE01wDisplayRcd = state.wsEmptyFileLit;
                state.returnCode = String.valueOf(state.ws99Lit);
                // TODO: unknown method
                // TODO: unknown method
            } else {
                if (Boolean.TRUE.equals(state.masterHeader)) {
                    readMasterFile(state);
                    if (Boolean.TRUE.equals(state.endOfInput)) {
                        state.ccE01wDisplayRcd = state.wsEmptyFileLit;
                        state.returnCode = String.valueOf(state.ws99Lit);
                        // TODO: unknown method
                        // TODO: unknown method
                    }
                }
            }
            state.wsPrevMasterFile = state.wsM01rMasterFile;
            setRecordKey(state);
        } catch (Exception e) {
            throw new RuntimeException("Error in generalHousekeeping: " + e.getMessage(), e);
        }
    }


    private void openFiles(ProgramState state) {
        // ===== Original COBOL: 1400-OPEN-FILES =====
        // OPEN INPUT  TEN99-M01R-1099-MASTER-FILE
        // TEN99-R01R-CONTROL-CARD
        // OUTPUT TEN99-C01W-1099-SUMMARY-FILE
        // CC-E01W-DISPLAY-FILE
        // .
        // EJECT
        // 046700***************************************************************** 04670001
        // 046800* 1600-INITIALIZATION.                                            04680001
        // 046900*                                                                 04690001
        // 047000* READ CONTROL CARD AND WRITE FIRST DISPLAY RECORD.               04700001
        // 047100*                                                                 04710001
        // 047200* PEFORMED FROM 1000-GENERAL-HOUSEKEEPING.                        04720001
        // 047300***************************************************************** 04730001
        // ===== End COBOL =====
        
        try {
            readTen99m01r1099masterfile(state);
            readTen99r01rcontrolcard(state);
            writeTen99c01w1099summaryfile(state, state.wsC01wOutputFile != null ? state.wsC01wOutputFile : "");
            writeCce01wdisplayfile(state, state.wsC01wOutputFile != null ? state.wsC01wOutputFile : "");
        } catch (Exception e) {
            throw new RuntimeException("Error in openFiles: " + e.getMessage(), e);
        }
    }


    private void initialization(ProgramState state) {
        // ===== Original COBOL: 1600-INITIALIZATION =====
        // INITIALIZE WS-OUTPUT-RECORD-LINE
        // INITIALIZE WS-C01W-OUTPUT-FILE
        // PERFORM 1700-INIT-FIRST-DISPLAY-RCD
        // PERFORM 7000-READ-CONTROL-CARD
        // .
        // EJECT
        // 048200***************************************************************** 04820001
        // 048300* 1700-INIT-FIRST-DISPLAY-RCD.                                    04830001
        // 048400*                                                                 04840001
        // 048500* CREATE FIRST DISPLAY RECORD.                                    04850001
        // 048600*                                                                 04860001
        // 048700* PEFORMED FROM 1600-INITIALIZATION.                              04870001
        // 048800***************************************************************** 04880001
        // ===== End COBOL =====
        
        try {
            state.wsC01wOutputFile = "";
            initFirstDisplayRcd(state);
            readControlCard(state);
        } catch (Exception e) {
            throw new RuntimeException("Error in initialization: " + e.getMessage(), e);
        }
    }


    private void initFirstDisplayRcd(ProgramState state) {
        // ===== Original COBOL: 1700-INIT-FIRST-DISPLAY-RCD =====
        // 049100*++INCLUDE C2INZ001                                               04910001
        // COPY C2INZ001.
        // EJECT
        // 049500***************************************************************** 04950001
        // 049600* 3000-PROCESS-MASTER-FILE.                                       04960001
        // 049700*                                                                 04970001
        // 049800* SUMMARIZE THE 1099'S FOR OUTPUT.                                04980001
        // 049900*                                                                 04990001
        // 050000* PEFORMED FROM 0000-MAINLINE.                                    05000001
        // 050100***************************************************************** 05010001
        // ===== End COBOL =====
        
        try {
            initFirstDisplayRcd(state);
        } catch (Exception e) {
            throw new RuntimeException("Error in initFirstDisplayRcd: " + e.getMessage(), e);
        }
    }


    private void processMasterFile(ProgramState state) {
        // ===== Original COBOL: 3000-PROCESS-MASTER-FILE =====
        // ADD WS-1-LIT            TO WS-TOT-RECDS-READ
        // IF WS-PREV-OPER-LOC-CODE = WS-M01R-OPER-LOC-CODE
        // IF WS-PREV-REC-KEY = WS-CURR-REC-KEY
        // IF WS-PREV-BR-DEPT = WS-M01R-BR-DEPT
        // IF WS-M01R-CHECK-TYPE = WS-MANUAL-CHECK-LIT
        // PERFORM 4100-CREATE-MANUAL-OUT-RECORD
        // ELSE
        // CONTINUE
        // END-IF
        // ELSE
        // PERFORM 3100-BREAK-FOR-BRANCH
        // END-IF
        // ELSE
        // PERFORM 3200-BREAK-FOR-SOC-SEC-NUM
        // END-IF
        // ELSE
        // PERFORM 3300-BREAK-FOR-OPER-LOC-CODE
        // END-IF
        // EVALUATE TRUE
        // WHEN MASTER-TRAILER
        // CONTINUE
        // WHEN WS-M01R-CHECK-TYPE = WS-MANUAL-CHECK-LIT
        // CONTINUE
        // WHEN OTHER
        // ADD WS-M01R-1099-AMT-OUT   TO WS-CALC-TOT-AMOUNT
        // MOVE WS-M01R-MASTER-FILE   TO WS-PREV-MASTER-FILE
        // END-EVALUATE
        // PERFORM 7500-READ-MASTER-FILE
        // .
        // EJECT
        // 054000************************************************************      05400001
        // 054100* 3100-BREAK-FOR-BRANCH.                                          05410001
        // 054200*                                                                 05420001
        // 054300* CHECK FOR MANUAL CHECK WHEN THE BRANCH CHANGES.                 05430001
        // 054400*                                                                 05440001
        // 054500* PERFORMED FROM 3000-PROCESS-MASTER-FILE.                        05450001
        // 054600************************************************************      05460001
        // ===== End COBOL =====
        
        try {
            state.wsTotRecdsRead = state.wsTotRecdsRead + state.ws1Lit;
            if (state.wsPrevOperLocCode == state.wsM01rOperLocCode) {
                if (state.wsPrevRecKey.equals(state.wsCurrRecKey)) {
                    if (state.wsPrevBrDept.equals(state.wsM01rBrDept)) {
                        if (state.wsM01rCheckType.equals(state.wsManualCheckLit)) {
                            createManualOutRecord(state);
                        }
                    } else {
                        breakForBranch(state);
                    }
                } else {
                    breakForSocSecNum(state);
                }
            } else {
                breakForOperLocCode(state);
            }
            if (Boolean.TRUE.equals(state.masterTrailer)) {
                // CONTINUE
            } else if (state.wsM01rCheckType.equals(state.wsManualCheckLit)) {
                // CONTINUE
            } else {
                state.wsCalcTotAmount = state.wsCalcTotAmount.add(state.wsM01r1099AmtOut);
                state.wsPrevMasterFile = state.wsM01rMasterFile;
            }
            readMasterFile(state);
        } catch (Exception e) {
            throw new RuntimeException("Error in processMasterFile: " + e.getMessage(), e);
        }
    }


    private void breakForBranch(ProgramState state) {
        // ===== Original COBOL: 3100-BREAK-FOR-BRANCH =====
        // IF WS-PREV-CHECK-TYPE = WS-MANUAL-CHECK-LIT
        // IF WS-M01R-CHECK-TYPE = WS-MANUAL-CHECK-LIT
        // PERFORM 4100-CREATE-MANUAL-OUT-RECORD
        // ELSE
        // CONTINUE
        // END-IF
        // ELSE
        // PERFORM 4000-CREATE-SUMM-OUT-RECORD
        // IF WS-M01R-CHECK-TYPE = WS-MANUAL-CHECK-LIT
        // PERFORM 4100-CREATE-MANUAL-OUT-RECORD
        // ELSE
        // CONTINUE
        // END-IF
        // END-IF
        // .
        // EJECT
        // 056600************************************************************      05660001
        // 056700* 3200-BREAK-FOR-SOC-SEC-NUM.                                     05670001
        // 056800*                                                                 05680001
        // 056900* CHECK FOR MANUAL CHECK WHEN THE SOCIAL SECURITY NUMBER          05690001
        // 057000* CHANGES.                                                        05700001
        // 057100*                                                                 05710001
        // 057200* PERFORMED FROM 3000-PROCESS-MASTER-FILE.                        05720001
        // 057300************************************************************      05730001
        // ===== End COBOL =====
        
        try {
            if ("MANUAL".equals(state.wsPrevCheckType)) {
                if ("MANUAL".equals(state.wsM01rCheckType)) {
                    createManualOutRecord(state);
                } else {
                    // CONTINUE
                }
            } else {
                createSummOutRecord(state);
                if ("MANUAL".equals(state.wsM01rCheckType)) {
                    createManualOutRecord(state);
                } else {
                    // CONTINUE
                }
            }
        } catch (Exception e) {
            throw new RuntimeException("Error in breakForBranch: " + e.getMessage(), e);
        }
    }


    private void breakForSocSecNum(ProgramState state) {
        // ===== Original COBOL: 3200-BREAK-FOR-SOC-SEC-NUM =====
        // IF WS-PREV-CHECK-TYPE = WS-MANUAL-CHECK-LIT
        // IF WS-M01R-CHECK-TYPE = WS-MANUAL-CHECK-LIT
        // PERFORM 4100-CREATE-MANUAL-OUT-RECORD
        // ELSE
        // CONTINUE
        // END-IF
        // ELSE
        // PERFORM 4000-CREATE-SUMM-OUT-RECORD
        // IF WS-M01R-CHECK-TYPE = WS-MANUAL-CHECK-LIT
        // PERFORM 4100-CREATE-MANUAL-OUT-RECORD
        // MOVE WS-M01R-MASTER-FILE TO WS-PREV-MASTER-FILE
        // ELSE
        // CONTINUE
        // END-IF
        // END-IF
        // .
        // EJECT
        // 059400************************************************************      05940001
        // 059500* 3300-BREAK-FOR-OPER-LOC-CODE.                                   05950001
        // 059600*                                                                 05960001
        // 059700* CHECK FOR MANUAL CHECK WHEN THE OPERATION LOCATION CODE         05970001
        // 059800* CHANGES.                                                        05980001
        // 059900*                                                                 05990001
        // 060000* PERFORMED FROM 3000-PROCESS-MASTER-FILE.                        06000001
        // 060100************************************************************      06010001
        // ===== End COBOL =====
        
        try {
            if ("MANUAL".equals(state.wsPrevCheckType)) {
                if ("MANUAL".equals(state.wsM01rCheckType)) {
                    createManualOutRecord(state);
                } else {
                    // CONTINUE
                }
            } else {
                createSummOutRecord(state);
                if ("MANUAL".equals(state.wsM01rCheckType)) {
                    createManualOutRecord(state);
                    state.wsPrevMasterFile = state.wsM01rMasterFile;
                } else {
                    // CONTINUE
                }
            }
        } catch (Exception e) {
            throw new RuntimeException("Error in breakForSocSecNum: " + e.getMessage(), e);
        }
    }


    private void breakForOperLocCode(ProgramState state) {
        // ===== Original COBOL: 3300-BREAK-FOR-OPER-LOC-CODE =====
        // IF WS-PREV-CHECK-TYPE = WS-MANUAL-CHECK-LIT
        // PERFORM 5000-PROCESS-TRAILER
        // IF WS-M01R-CHECK-TYPE = WS-MANUAL-CHECK-LIT
        // PERFORM 4100-CREATE-MANUAL-OUT-RECORD
        // ELSE
        // CONTINUE
        // END-IF
        // ELSE
        // PERFORM 4000-CREATE-SUMM-OUT-RECORD
        // PERFORM 5000-PROCESS-TRAILER
        // IF WS-M01R-CHECK-TYPE = WS-MANUAL-CHECK-LIT
        // PERFORM 4100-CREATE-MANUAL-OUT-RECORD
        // MOVE WS-M01R-MASTER-FILE TO WS-PREV-MASTER-FILE
        // ELSE
        // CONTINUE
        // END-IF
        // END-IF
        // .
        // ===== End COBOL =====
        
        try {
            if ("MANUAL".equals(state.wsPrevCheckType)) {
                processTrailer(state);
                if ("MANUAL".equals(state.wsM01rCheckType)) {
                    createManualOutRecord(state);
                }
            } else {
                createSummOutRecord(state);
                processTrailer(state);
                if ("MANUAL".equals(state.wsM01rCheckType)) {
                    createManualOutRecord(state);
                    state.wsPrevMasterFile = state.wsM01rMasterFile;
                }
            }
        } catch (Exception e) {
            throw new RuntimeException("Error in breakForOperLocCode: " + e.getMessage(), e);
        }
    }


    private void setRecordKey(ProgramState state) {
        // ===== Original COBOL: 3900-SET-RECORD-KEY =====
        // MOVE WS-M01R-SOC-SEC-NUM       TO WS-CURR-TAXID
        // MOVE WS-M01R-TAX-TYPE          TO WS-CURR-TAX-TYPE
        // MOVE WS-M01R-TIN-IND           TO WS-CURR-TAX-IND
        // MOVE WS-PREV-SOC-SEC-NUM       TO WS-PREV-TAXID
        // MOVE WS-PREV-MSTR-TAX-TYPE     TO WS-PREV-TAX-TYPE
        // MOVE WS-PREV-MSTR-TIN-IND      TO WS-PREV-TIN-IND
        // .
        // 062400************************************************************      06240001
        // 062500* 4000-CREATE-NEW-OUTPUT-RECORD.                                  06250001
        // 062600*                                                                 06260001
        // 062700* CREATE SUMMARIZED 1099 RECORD FROM MASTER FILE.                 06270001
        // 062800*                                                                 06280001
        // 062900* PERFORMED FROM 3000-PROCESS-MASTER-FILE.                        06290001
        // 063000************************************************************      06300001
        // ===== End COBOL =====
        
        try {
            state.wsCurrTaxid = String.valueOf(state.wsM01rSocSecNum);
            state.wsCurrTaxType = state.wsM01rTaxType;
            state.wsCurrTaxInd = state.wsM01rTinInd;
            state.wsPrevTaxid = String.valueOf(state.wsPrevSocSecNum);
            state.wsPrevTaxType = state.wsPrevMstrTaxType;
            state.wsPrevTinInd = state.wsPrevMstrTinInd;
        } catch (Exception e) {
            throw new RuntimeException("Error in setRecordKey: " + e.getMessage(), e);
        }
    }


    private void createSummOutRecord(ProgramState state) {
        // ===== Original COBOL: 4000-CREATE-SUMM-OUT-RECORD =====
        // MOVE WS-PREV-OPER-LOC-CODE TO WS-C01W-OPER-LOC-CODE
        // MOVE WS-PREV-SOC-SEC-NUM   TO WS-C01W-SOC-SEC-NUM
        // MOVE WS-R01R-CC-YY         TO WS-C01W-PAY-YEAR
        // MOVE WS-PREV-EMP-NAME      TO WS-C01W-EMP-NAME
        // MOVE WS-PREV-ADDRESS-1     TO WS-C01W-ADDRESS-1
        // MOVE WS-PREV-ADDR2-OUT     TO WS-C01W-ADDRESS-2
        // MOVE WS-PREV-CITY          TO WS-C01W-CITY-MASTER
        // MOVE WS-PREV-STATE         TO WS-C01W-STATE
        // MOVE WS-PREV-BR-DEPT       TO WS-C01W-DESC-BR-DEPT
        // MOVE WS-PREV-CHECK-NUMBER  TO WS-C01W-DESC-CHECK-NUM
        // MOVE WS-PREV-MSTR-TAX-TYPE TO WS-C01W-AMOUNT-IND
        // IF (WS-PREV-MSTR-TIN-IND = WS-1X-LIT) OR
        // (WS-PREV-MSTR-TIN-IND = WS-2-LIT)
        // MOVE WS-PREV-MSTR-TIN-IND TO WS-C01W-SSN-IND
        // ELSE MOVE WS-1X-LIT            TO WS-C01W-SSN-IND
        // END-IF
        // MOVE WS-A-LIT              TO WS-C01W-TYPE
        // MOVE WS-A-LIT              TO WS-C01W-TRANS-CODE
        // IF WS-CALC-TOT-AMT-OVFL > ZEROES
        // MOVE WS-MONEY-ERROR-LIT  TO CC-E01W-DISPLAY-RCD
        // MOVE WS-99-LIT           TO RETURN-CODE
        // PERFORM 8999-WRITE-SYSOUT
        // PERFORM 9998-COREDUMP
        // ELSE
        // MOVE WS-CALC-TOT-AMT-OUT TO WS-C01W-TOT-AMOUNT
        // END-IF
        // INITIALIZE WS-ZIP-CHECK
        // MOVE WS-PREV-ZIP            TO WS-ZIP-CHECK
        // PERFORM 4200-CHECK-ZIP-CODE
        // MOVE WS-ZIP-OK-NUM          TO WS-C01W-ZIP
        // ADD WS-CALC-TOT-AMT-OUT     TO WS-1099-TRLR-AMOUNT
        // ADD WS-1-LIT                TO WS-1099-TRLR-CNT
        // PERFORM 8700-WRITE-OUTPUT-RECORD
        // INITIALIZE WS-CALC-TOT-AMOUNT
        // .
        // EJECT
        // 066900************************************************************      06690001
        // 067000* 4100-CREATE-MANUAL-OUT-RECORD.                                  06700001
        // 067100*                                                                 06710001
        // 067200* CREATE MANUAL CHECK 1099 RECORD FROM MASTER FILE.               06720001
        // 067300*                                                                 06730001
        // 067400* PERFORMED FROM 3000-PROCESS-MASTER-FILE.                        06740001
        // 067500************************************************************      06750001
        // ===== End COBOL =====
        
        try {
            state.wsC01wOperLocCode = state.wsPrevOperLocCode;
            state.wsC01wSocSecNum = state.wsPrevSocSecNum;
            state.wsC01wPayYear = Integer.parseInt(state.wsR01rCcYy);
            state.wsC01wEmpName = state.wsPrevEmpName;
            state.wsC01wAddress1 = state.wsPrevAddress1;
            state.wsC01wAddress2 = state.wsPrevAddr2Out;
            state.wsC01wCityMaster = state.wsPrevCity;
            state.wsC01wState = state.wsPrevState;
            state.wsC01wDescBrDept = state.wsPrevBrDept;
            state.wsC01wDescCheckNum = state.wsPrevCheckNumber;
            state.wsC01wAmountInd = Integer.parseInt(state.wsPrevMstrTaxType);
            if ("1X".equals(state.wsPrevMstrTinInd) || "2".equals(state.wsPrevMstrTinInd)) {
                state.wsC01wSsnInd = state.wsPrevMstrTinInd;
            } else {
                state.wsC01wSsnInd = "1X";
            }
            state.wsC01wType = "A";
            state.wsC01wTransCode = "A";
            if (state.wsCalcTotAmtOvfl > 0) {
                state.ccE01wDisplayRcd = "MONEY ERROR";
                state.returnCode = String.valueOf(99);
                // TODO: unknown method
                // TODO: unknown method
            } else {
                state.wsC01wTotAmount = state.wsCalcTotAmtOut;
            }
            state.wsZipCheck = String.valueOf(0);
            state.wsZipCheck = String.valueOf(state.wsPrevZip);
            checkZipCode(state);
            state.wsC01wZip = Integer.parseInt(state.wsZipOkNum);
            state.ws1099TrlrAmount = state.ws1099TrlrAmount.add(state.wsCalcTotAmtOut);
            state.ws1099TrlrCnt = state.ws1099TrlrCnt + 1;
            writeOutputRecord(state);
            state.wsCalcTotAmount = java.math.BigDecimal.ZERO;
        } catch (Exception e) {
            throw new RuntimeException("Error in createSummOutRecord: " + e.getMessage(), e);
        }
    }


    private void createManualOutRecord(ProgramState state) {
        // ===== Original COBOL: 4100-CREATE-MANUAL-OUT-RECORD =====
        // MOVE WS-M01R-OPER-LOC-CODE TO WS-C01W-OPER-LOC-CODE
        // MOVE WS-M01R-SOC-SEC-NUM   TO WS-C01W-SOC-SEC-NUM
        // MOVE WS-R01R-CC-YY         TO WS-C01W-PAY-YEAR
        // MOVE WS-M01R-EMP-NAME      TO WS-C01W-EMP-NAME
        // MOVE WS-M01R-ADDRESS-1     TO WS-C01W-ADDRESS-1
        // MOVE WS-M01R-ADDR2-OUT     TO WS-C01W-ADDRESS-2
        // MOVE WS-M01R-CITY          TO WS-C01W-CITY-MASTER
        // MOVE WS-M01R-STATE         TO WS-C01W-STATE
        // MOVE WS-M01R-BR-DEPT       TO WS-C01W-DESC-BR-DEPT
        // MOVE WS-M01R-COMMENTS      TO WS-C01W-COMMENTS
        // MOVE WS-20071-LIT          TO WS-C01W-FORM-NUM
        // MOVE WS-M01R-TAX-TYPE      TO WS-C01W-AMOUNT-IND
        // IF (WS-M01R-TIN-IND = WS-1X-LIT) OR
        // (WS-M01R-TIN-IND = WS-2-LIT)
        // MOVE WS-M01R-TIN-IND    TO WS-C01W-SSN-IND
        // ELSE MOVE WS-1X-LIT           TO WS-C01W-SSN-IND
        // END-IF
        // MOVE WS-A-LIT              TO WS-C01W-TYPE
        // MOVE WS-A-LIT              TO WS-C01W-TRANS-CODE
        // IF WS-M01R-1099-AMT-OVFL > ZEROES
        // MOVE WS-MONEY-ERROR-LIT   TO CC-E01W-DISPLAY-RCD
        // MOVE WS-99-LIT            TO RETURN-CODE
        // PERFORM 8999-WRITE-SYSOUT
        // PERFORM 9998-COREDUMP
        // ELSE
        // MOVE WS-M01R-1099-AMT-OUT TO WS-C01W-TOT-AMOUNT
        // END-IF
        // INITIALIZE WS-ZIP-CHECK
        // MOVE WS-M01R-ZIP             TO WS-ZIP-CHECK
        // PERFORM 4200-CHECK-ZIP-CODE
        // MOVE WS-ZIP-OK-NUM           TO WS-C01W-ZIP
        // ADD WS-M01R-1099-AMT-OUT     TO WS-1099-TRLR-AMOUNT
        // ADD WS-1-LIT                 TO WS-1099-TRLR-CNT
        // PERFORM 8700-WRITE-OUTPUT-RECORD
        // .
        // EJECT
        // 071400************************************************************      07140001
        // 071500* 4200-CHECK-ZIP-CODE.                                            07150001
        // 071600*                                                                 07160001
        // 071700* CHECK THE ZIP CODE.  THE ZIP CODE SHOULD BE LEFT JUSTIFIED      07170001
        // 071800* AND HAVE SPACES IN THE FOUR RIGHT CHARACTERS IF THE ZIP IS      07180001
        // 071900* ONLY FIVE CHARACTERS LONG.                                      07190001
        // 072000*                                                                 07200001
        // 072100* PERFORMED FROM 4000-CREATE-NEW-OUTPUT-RECORD.                   07210001
        // 072200*                4100-CREATE-MANUAL-OUT-RECORD.                   07220001
        // 072300************************************************************      07230001
        // ===== End COBOL =====
        
        try {
            state.wsC01wOperLocCode = state.wsM01rOperLocCode;
            state.wsC01wSocSecNum = state.wsM01rSocSecNum;
            state.wsC01wPayYear = Integer.parseInt(state.wsR01rCcYy);
            state.wsC01wEmpName = state.wsM01rEmpName;
            state.wsC01wAddress1 = state.wsM01rAddress1;
            state.wsC01wAddress2 = state.wsM01rAddr2Out;
            state.wsC01wCityMaster = state.wsM01rCity;
            state.wsC01wState = state.wsM01rState;
            state.wsC01wDescBrDept = state.wsM01rBrDept;
            state.wsC01wComments = state.wsM01rComments;
            state.wsC01wFormNum = state.ws20071Lit;
            state.wsC01wAmountInd = Integer.parseInt(state.wsM01rTaxType);
            if ("1X".equals(state.wsM01rTinInd) || "2".equals(state.wsM01rTinInd)) {
                state.wsC01wSsnInd = state.wsM01rTinInd;
            } else {
                state.wsC01wSsnInd = "1X";
            }
            state.wsC01wType = "A";
            state.wsC01wTransCode = "A";
            if (state.wsM01r1099AmtOvfl > 0) {
                state.ccE01wDisplayRcd = state.wsMoneyErrorLit;
                state.returnCode = String.valueOf(state.ws99Lit);
                // TODO: unknown method
                // TODO: unknown method
            } else {
                state.wsC01wTotAmount = state.wsM01r1099AmtOut;
            }
            state.wsZipCheck = String.valueOf(0);
            state.wsZipCheck = String.valueOf(state.wsM01rZip);
            checkZipCode(state);
            state.wsC01wZip = Integer.parseInt(String.valueOf(state.wsZipOkNum));
            state.ws1099TrlrAmount = state.ws1099TrlrAmount.add(state.wsM01r1099AmtOut);
            state.ws1099TrlrCnt = state.ws1099TrlrCnt + state.ws1Lit;
            writeOutputRecord(state);
        } catch (Exception e) {
            throw new RuntimeException("Error in createManualOutRecord: " + e.getMessage(), e);
        }
    }


    private void checkZipCode(ProgramState state) {
        // ===== Original COBOL: 4200-CHECK-ZIP-CODE =====
        // INITIALIZE WS-EVAL-NUM-ZIP
        // WS-EVAL-ALPHA-ZIP
        // WS-ZIP-OK
        // EVALUATE TRUE
        // WHEN WS-ZIP-CHECK-NUM IS NUMERIC
        // MOVE WS-ZIP-CHECK-NUM TO WS-EVAL-NUM-ZIP
        // PERFORM 4250-CHECK-NUMERIC-ZIP
        // WHEN OTHER
        // MOVE WS-ZIP-CHECK TO WS-EVAL-ALPHA-ZIP
        // PERFORM 4260-CHECK-ALPHA-ZIP
        // END-EVALUATE
        // .
        // EJECT
        // 074000************************************************************      07400001
        // 074100* 4250-CHECK-NUMERIC-ZIP.                                         07410001
        // 074200*                                                                 07420001
        // 074300* CHECK NUMERIC ZIP FOR CORRECT POSITION.                         07430001
        // 074400*                                                                 07440001
        // 074500*                                                                 07450001
        // 074600* PERFORMED FROM 4200-CHECK-ZIP-CODE.                             07460001
        // 074700************************************************************      07470001
        // ===== End COBOL =====
        
        try {
            state.wsEvalNumZip = null;
            state.wsEvalAlphaZip = null;
            state.wsZipOk = null;
            if (state.wsZipCheckNum >= 0 && String.valueOf(state.wsZipCheckNum).matches("\\d+")) {
                state.wsEvalNumZip = String.valueOf(state.wsZipCheckNum);
                checkNumericZip(state);
            } else {
                state.wsEvalAlphaZip = state.wsZipCheck;
                checkAlphaZip(state);
            }
        } catch (Exception e) {
            throw new RuntimeException("Error in checkZipCode: " + e.getMessage(), e);
        }
    }


    private void checkNumericZip(ProgramState state) {
        // ===== Original COBOL: 4250-CHECK-NUMERIC-ZIP =====
        // IF WS-EVAL-NUM-ZIP-5 > WS-ZEROES-5-LIT
        // IF WS-EVAL-NUM-ZIP-4 > WS-ZEROES-4-LIT
        // MOVE WS-EVAL-NUM-ZIP TO WS-ZIP-OK-NUM
        // ELSE MOVE WS-SPACES-4-LIT TO WS-ZIP-OK-4
        // MOVE WS-EVAL-NUM-ZIP-5 TO WS-ZIP-OK-NUM-5
        // END-IF
        // ELSE IF WS-EVAL-NUM2-ZIP-5 > WS-ZEROES-5-LIT
        // MOVE WS-EVAL-NUM2-ZIP-5 TO WS-ZIP-OK-5
        // ELSE MOVE WS-ZIP-ZEROES-LIT TO CC-E01W-DISPLAY-RCD
        // MOVE WS-99-LIT         TO RETURN-CODE
        // PERFORM 8999-WRITE-SYSOUT
        // END-IF
        // END-IF
        // IF WS-ZIP-OK-NUM-4 = WS-ZEROES-4-LIT
        // MOVE WS-SPACES-4-LIT TO WS-ZIP-OK-4
        // END-IF
        // .
        // EJECT
        // 076900************************************************************      07690001
        // 077000* 4260-CHECK-ALPHA-ZIP.                                           07700001
        // 077100*                                                                 07710001
        // 077200* CHECK ALPHANUM ZIP FOR CORRECT POSITION.                        07720001
        // 077300*                                                                 07730001
        // 077400*                                                                 07740001
        // 077500* PERFORMED FROM 4200-CHECK-ZIP-CODE.                             07750001
        // 077600************************************************************      07760001
        // ===== End COBOL =====
        
        try {
            if (state.wsEvalNumZip5 > state.wsZeroes5Lit) {
                if (state.wsEvalNumZip4 > state.wsZeroes4Lit) {
                    state.wsZipOkNum = state.wsEvalNumZip;
                } else {
                    state.wsZipOk4 = state.wsSpaces4Lit;
                    state.wsZipOkNum5 = state.wsEvalNumZip5;
                }
            } else if (state.wsEvalNum2Zip5 > state.wsZeroes5Lit) {
                state.wsZipOk5 = String.valueOf(state.wsEvalNum2Zip5);
            } else {
                state.ccE01wDisplayRcd = state.wsZipZeroesLit;
                state.returnCode = String.valueOf(state.ws99Lit);
                // TODO: unknown method
            }
            if (state.wsZipOkNum4 == state.wsZeroes4Lit) {
                state.wsZipOk4 = state.wsSpaces4Lit;
            }
        } catch (Exception e) {
            throw new RuntimeException("Error in checkNumericZip: " + e.getMessage(), e);
        }
    }


    private void checkAlphaZip(ProgramState state) {
        // ===== Original COBOL: 4260-CHECK-ALPHA-ZIP =====
        // IF WS-EVAL-ALPHA-ZIP-4 = WS-SPACES-4-LIT
        // IF WS-EVAL-ALPHA-ZIP-5 > WS-SPACES-5-LIT
        // MOVE WS-EVAL-ALPHA-ZIP-5 TO WS-ZIP-OK-5
        // MOVE WS-SPACES-4-LIT     TO WS-ZIP-OK-4
        // ELSE MOVE WS-SPACES-4-LIT TO WS-ZIP-OK-4
        // MOVE WS-EVAL-NUM-ZIP-5 TO WS-ZIP-OK-5
        // END-IF
        // ELSE IF WS-EVAL-ALPHA2-ZIP-5 > WS-SPACES-5-LIT
        // MOVE WS-EVAL-ALPHA-ZIP TO WS-ZIP-OK
        // ELSE MOVE WS-ZIP-SPACES-LIT TO CC-E01W-DISPLAY-RCD
        // MOVE WS-99-LIT        TO RETURN-CODE
        // PERFORM 8999-WRITE-SYSOUT
        // END-IF
        // END-IF
        // .
        // EJECT
        // 079500************************************************************      07950001
        // 079600* 5000-PROCESS-TRAILER.                                           07960001
        // 079700*                                                                 07970001
        // 079800* CREATE TRAILER RECORD WITH RECORD COUNT AND 1099 SUMMARIZED     07980001
        // 079900* MONRY AMOUNT.                                                   07990001
        // 080000*                                                                 08000001
        // 080100* PERFORMED FROM 0000-MAINLINE.                                   08010001
        // 080200************************************************************      08020001
        // ===== End COBOL =====
        
        try {
            if ("    ".equals(state.wsEvalAlphaZip4)) {
                if (String.valueOf(state.wsEvalAlphaZip5).compareTo("     ") > 0) {
                    state.wsZipOk5 = String.valueOf(state.wsEvalAlphaZip5);
                    state.wsZipOk4 = "    ";
                } else {
                    state.wsZipOk4 = "    ";
                    state.wsZipOk5 = String.valueOf(state.wsEvalNumZip5);
                }
            } else if (String.valueOf(state.wsEvalAlpha2Zip5).compareTo("     ") > 0) {
                state.wsZipOk = state.wsEvalAlphaZip;
            } else {
                state.ccE01wDisplayRcd = state.wsZipSpacesLit;
                state.returnCode = String.valueOf(state.ws99Lit);
                // TODO: unknown method
            }
        } catch (Exception e) {
            throw new RuntimeException("Error in checkAlphaZip: " + e.getMessage(), e);
        }
    }


    private void processTrailer(ProgramState state) {
        // ===== Original COBOL: 5000-PROCESS-TRAILER =====
        // INITIALIZE WS-C01W-OUTPUT-TRAILER
        // MOVE WS-PREV-OPER-LOC-CODE  TO WS-C01W-TRLR-OPER-LOC-CODE
        // MOVE WS-1099-TRLR-CNT       TO WS-C01W-TRLR-TOT-RECORDS
        // MOVE WS-1099-TRLR-AMOUNT    TO WS-C01W-TRLR-TOT-AMOUNT
        // MOVE WS-T-LIT               TO WS-C01W-TRLR-TRANS-CODE
        // ADD WS-1-LIT                TO WS-TOT-BATCH-NUM
        // MOVE WS-TOT-BATCH-NUM       TO WS-C01W-TRLR-BATCH-NUM
        // PERFORM 8700-WRITE-OUTPUT-RECORD
        // INITIALIZE                     WS-1099-TRLR-CNT
        // WS-1099-TRLR-AMOUNT
        // .
        // EJECT
        // 082200***************************************************************** 08220001
        // 082300* 7000-READ-CONTROL-CARD.                                         08230001
        // 082400*                                                                 08240001
        // 082500* READ THE CONTROL CARD.                                          08250001
        // 082600*                                                                 08260001
        // 082700* PEFORMED FROM 1600-INITIALIZATION.                              08270001
        // 082800***************************************************************** 08280001
        // ===== End COBOL =====
        
        try {
            state.wsC01wOutputFile = "";
            state.wsC01wOperLocCode = state.wsPrevOperLocCode;
            state.wsC01wType = String.valueOf(state.ws1099TrlrCnt);
            state.wsC01wSocSecNum = state.ws1099TrlrAmount.intValue();
            state.wsC01wTransCode = state.wsTLit;
            state.wsTotBatchNum = state.wsTotBatchNum + state.ws1Lit;
            state.wsC01wReferenceNum = String.valueOf(state.wsTotBatchNum);
            writeOutputRecord(state);
            state.ws1099TrlrCnt = 0;
            state.ws1099TrlrAmount = java.math.BigDecimal.ZERO;
        } catch (Exception e) {
            throw new RuntimeException("Error in processTrailer: " + e.getMessage(), e);
        }
    }


    private void readControlCard(ProgramState state) {
        // ===== Original COBOL: 7000-READ-CONTROL-CARD =====
        // READ TEN99-R01R-CONTROL-CARD INTO WS-R01R-CONTROL-CARD
        // AT END
        // MOVE WS-EMPTY-CCRD-LIT        TO CC-E01W-DISPLAY-RCD
        // MOVE WS-99-LIT                TO RETURN-CODE
        // PERFORM 8999-WRITE-SYSOUT
        // PERFORM 9998-COREDUMP
        // END-READ
        // .
        // EJECT
        // 084000************************************************************      08400001
        // 084100* 7500-READ-MASTER-FILE.                                          08410001
        // 084200*                                                                 08420001
        // 084300* READ A RECORD FROM THE MASTER FILE AND SET END OF FILE          08430001
        // 084400* INDICATOR WHEN DONE.                                            08440001
        // 084500*                                                                 08450001
        // 084600* PERFORMED FROM 1600-INITIALIZATION                              08460001
        // 084700*                3000-PROCESS-MASTER-FILE.                        08470001
        // 084800************************************************************      08480001
        // ===== End COBOL =====
        
        try {
            readTen99r01rcontrolcard(state);
            if (Boolean.parseBoolean(state.atEndTen99r01rcontrolcard)) {
                state.ccE01wDisplayRcd = state.wsEmptyCcrdLit;
                state.returnCode = String.valueOf(state.ws99Lit);
                // TODO: unknown method
                // TODO: unknown method
            }
        } catch (Exception e) {
            throw new RuntimeException("Error in readControlCard: " + e.getMessage(), e);
        }
    }


    private void readMasterFile(ProgramState state) {
        // ===== Original COBOL: 7500-READ-MASTER-FILE =====
        // READ TEN99-M01R-1099-MASTER-FILE INTO WS-M01R-MASTER-FILE
        // AT END
        // MOVE WS-YES-LIT TO WS-EOF-SW
        // END-READ
        // IF END-OF-INPUT
        // CONTINUE
        // ELSE
        // PERFORM 3900-SET-RECORD-KEY
        // END-IF
        // .
        // EJECT
        // 085700***************************************************************** 08570001
        // 085800* 8700-WRITE-OUTPUT-RECORD.                                       08580001
        // 085900*                                                                 08590001
        // 086000* WRITE OUT THE SUMMARIZED 1099 RECORD.                           08600001
        // 086100*                                                                 08610001
        // 086200* PERFORMED FROM 4000-CREATE-OUTPUT-RECORD.                       08620001
        // 086300*                4100-CREATE-MANUAL-RECORD.                       08630001
        // 086400***************************************************************** 08640001
        // ===== End COBOL =====
        
        try {
            readTen99m01r1099masterfile(state);
            if ("YES".equals(state.wsEofSw)) {
                // AT END logic already handled by readTen99m01r1099masterfile
            }
            if (Boolean.parseBoolean(String.valueOf(state.endOfInput))) {
                // CONTINUE
            } else {
                setRecordKey(state);
            }
        } catch (Exception e) {
            throw new RuntimeException("Error in readMasterFile: " + e.getMessage(), e);
        }
    }


    private void writeOutputRecord(ProgramState state) {
        // ===== Original COBOL: 8700-WRITE-OUTPUT-RECORD =====
        // MOVE WS-C01W-OUTPUT-FILE   TO WS-OUTPUT-RECORD-LINE
        // WRITE TEN99-C01W-1099-SUMMARY-RCD
        // FROM WS-OUTPUT-RECORD-LINE
        // END-WRITE
        // ADD WS-1-LIT             TO WS-TOT-WRITTEN-CNT
        // INITIALIZE                  WS-OUTPUT-RECORD-LINE
        // WS-C01W-OUTPUT-FILE
        // .
        // EJECT
        // 088000*++INCLUDE C2INZ002                                               08800001
        // COPY C2INZ002.
        // EJECT
        // 088400***************************************************************** 08840001
        // 088500* 9000-CLOSING-ROUTINE.                                           08850001
        // 088600*                                                                 08860001
        // 088700* PERFORM END OF PROGRAM FUNCTIONS.                               08870001
        // 088800*                                                                 08880001
        // 088900* PEFORMED FROM 0000-MAINLINE.                                    08890001
        // 089000***************************************************************** 08900001
        // ===== End COBOL =====
        
        try {
            state.wsC01wOutputFile = state.wsC01wOutputFile;
            writeTen99c01w1099summaryfile(state, state.wsC01wOutputFile != null ? state.wsC01wOutputFile : "");
            state.wsTotWrittenCnt = state.wsTotWrittenCnt + state.ws1Lit;
            state.wsOutputRecordLine = "";
            state.wsC01wOutputFile = "";
        } catch (Exception e) {
            throw new RuntimeException("Error in writeOutputRecord: " + e.getMessage(), e);
        }
    }


    private void closingRoutine(ProgramState state) {
        // ===== Original COBOL: 9000-CLOSING-ROUTINE =====
        // PERFORM 9100-WRITE-LAST-DISPLAY-RCD
        // PERFORM 9999-CLOSE-FILES
        // .
        // EJECT
        // 089800***************************************************************** 08980001
        // 089900* 9100-WRITE-LAST-DISPLAY-RCD.                                    08990001
        // 090000*                                                                 09000001
        // 090100* WRITE LAST RECORD TO DISPLAY FILE WITH END OF RUN  STATISTICS.  09010001
        // 090200*                                                                 09020001
        // 090300* PEFORMED FROM 9000-CLOSING-ROUTINE.                             09030001
        // 090400***************************************************************** 09040001
        // ===== End COBOL =====
        
        try {
            writeLastDisplayRcd(state);
            closingRoutine(state);
        } catch (Exception e) {
            throw new RuntimeException("Error in closingRoutine: " + e.getMessage(), e);
        }
    }


    private void writeLastDisplayRcd(ProgramState state) {
        // ===== Original COBOL: 9100-WRITE-LAST-DISPLAY-RCD =====
        // MOVE WS-EOJ-MSG-1           TO CC-E01W-DISPLAY-RCD
        // PERFORM 8999-WRITE-SYSOUT
        // MOVE WS-TOT-RECDS-READ      TO SCR-COUNT
        // MOVE SYSOUT-COUNT-RCD       TO CC-E01W-DISPLAY-RCD
        // PERFORM 8999-WRITE-SYSOUT
        // MOVE WS-EOJ-MSG-2           TO CC-E01W-DISPLAY-RCD
        // PERFORM 8999-WRITE-SYSOUT
        // MOVE WS-TOT-WRITTEN-CNT     TO SCR-COUNT
        // MOVE SYSOUT-COUNT-RCD       TO CC-E01W-DISPLAY-RCD
        // PERFORM 8999-WRITE-SYSOUT
        // 092100*++INCLUDE C2INZ003                                               09210001
        // COPY C2INZ003.
        // EJECT
        // 092500***************************************************************** 09250001
        // 092600* 9998-COREDUMP.                                                  09260001
        // 092700*                                                                 09270001
        // 092800* TO DUMP THE PROGRAM FOR FATAL ERRORS.                           09280001
        // 092900*                                                                 09290001
        // 093000* PEFORMED FROM 1000-GENERAL-HOUSEKEEPING.                        09300001
        // 093100*               7000-READ-CONTROL-CARD.                           09310001
        // 093200***************************************************************** 09320001
        // 093400*++INCLUDE C2INZ004                                               09340001
        // COPY C2INZ004.
        // EJECT
        // 093700***************************************************************** 09370001
        // 093800* 9999-CLOSE-FILES.                                               09380001
        // 093900*                                                                 09390001
        // 094000* THIS WILL CLOSE ALL THE FILES.                                  09400001
        // 094100*                                                                 09410001
        // 094200* PEFORMED FROM 9000-CLOSING-ROUTINE.                             09420001
        // 094300***************************************************************** 09430001
        // 094500*++INCLUDE C2INZ005                                               09450001
        // COPY C2INZ005.
        // TEN99-M01R-1099-MASTER-FILE
        // TEN99-C01W-1099-SUMMARY-FILE
        // TEN99-R01R-CONTROL-CARD
        // .
        // EJECT
        // 095200******************************************************************09520001
        // 095300*                PROGRAM CCAC6350 END OF SOURCE                  *09530001
        // 095400******************************************************************09540001
        // END PROGRAM CCAC6350.
        // 
        // ===== End COBOL =====
        
        try {
            state.ccE01wDisplayRcd = state.wsEojMsg1;
            // TODO: unknown method
            state.scrCount = String.valueOf(state.wsTotRecdsRead);
            // TODO: unknown symbol sysoutCountRcd
            // TODO: unknown method
            state.ccE01wDisplayRcd = state.wsEojMsg2;
            // TODO: unknown method
            state.scrCount = String.valueOf(state.wsTotWrittenCnt);
            // TODO: unknown symbol sysoutCountRcd
            // TODO: unknown method
        } catch (Exception e) {
            throw new RuntimeException("Error in writeLastDisplayRcd: " + e.getMessage(), e);
        }
    }

}