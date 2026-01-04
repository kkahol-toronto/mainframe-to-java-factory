package com.fordcredit.misc1099.batch.program;

import org.springframework.batch.core.StepContribution;
import org.springframework.batch.core.scope.context.ChunkContext;
import org.springframework.batch.core.step.tasklet.Tasklet;
import org.springframework.batch.repeat.RepeatStatus;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Objects;

/**
 * Complete Tasklet for COBOL program CCAC6350
 * Generated with full business logic translation
 */
public class CCAC6350Tasklet implements Tasklet {

    static class ProgramState {
        BufferedReader ten99M01r1099MasterFileReader;
        boolean ten99M01r1099MasterFileEof = false;
        String ten99M01r1099MasterFileLine;
        BufferedReader ten99R01rControlCardReader;
        boolean ten99R01rControlCardEof = false;
        String ten99R01rControlCardLine;
        BufferedWriter ten99C01w1099SummaryFileWriter;
        boolean ten99C01w1099SummaryFileEof = false;
        String ten99C01w1099SummaryFileLine;
        BufferedWriter ccE01wDisplayFileWriter;
        boolean ccE01wDisplayFileEof = false;
        String ccE01wDisplayFileLine;

        // Processing state
        boolean endOfInput = false;
        String returnCode = "0";
        String recordsRead = "0";
        String recordsWritten = "0";

        // WORKING-STORAGE variables
        String panValet = "002CCAC6350  01/12/96";
        String pups2000 = "";
        String wsM01rMasterFile = "";
        String wsM01rCheckNumber = "";
        String masterHeader = "LOW-VALUES";
        String masterTrailer = "HIGH-VALUES";
        String wsM01rChk = "";
        String wsM01rCheckType = "";
        String wsM01rBrDept = "";
        String wsM01rSocSecNum = "";
        String wsM01rSocSecRem = "";
        String wsM01rOperLocCode = "";
        String wsM01rSource = "";
        String wsM01rEmpName = "";
        String wsM01rAddress1 = "";
        String wsM01rAddress2 = "";
        String wsM01rAddr2 = "";
        String wsM01rAddr2Out = "";
        String wsM01rAddrOvfl = "";
        String wsM01rCity = "";
        String wsM01rState = "";
        String wsM01rZip = "";
        String wsM01rDeleteInd = "";
        String wsM01rTotAmount = "";
        String wsM01r1099Amount = "";
        String wsM01r1099Amt = "";
        String wsM01r1099AmtOvfl = "";
        String wsM01r1099AmtOut = "";
        String wsM01rCheckDate = "";
        String wsM01rMonthendDate = "";
        String wsM01rDistRecNum = "";
        String wsM01rDistRec = "";
        String wsM01rCompassCode = "";
        String wsM01rDist1099Ind = "";
        String wsM01rDistAmount = "";
        String wsM01rComments = "";
        String wsM01rLineNbr = "";
        String wsM01rTaxType = "";
        String wsM01rTinInd = "";
        String wsPrevMasterFile = "";
        String wsPrevCheckNumber = "";
        String wsPrevChk = "";
        String wsPrevCheckType = "";
        String wsPrevBrDept = "";
        String wsPrevSocSecNum = "";
        String wsPrevSocSecRem = "";
        String wsPrevOperLocCode = "";
        String wsPrevSource = "";
        String wsPrevEmpName = "";
        String wsPrevAddress1 = "";
        String wsPrevAddress2 = "";
        String wsPrevAddr2 = "";
        String wsPrevAddr2Out = "";
        String wsPrevAddrOvfl = "";
        String wsPrevCity = "";
        String wsPrevState = "";
        String wsPrevZip = "";
        String wsPrevDeleteInd = "";
        String wsPrevTotAmount = "";
        String wsPrev1099Amount = "";
        String wsPrev1099Amt = "";
        String wsPrev1099AmtOvfl = "";
        String wsPrev1099AmtOut = "";
        String wsPrevCheckDate = "";
        String wsPrevMonthendDate = "";
        String wsPrevDistRecNum = "";
        String wsPrevDistRec = "";
        String wsPrevCompassCode = "";
        String wsPrevDist1099Ind = "";
        String wsPrevDistAmount = "";
        String wsPrevComments = "";
        String wsPrevLineNbr = "";
        String wsPrevMstrTaxType = "";
        String wsPrevMstrTinInd = "";
        String wsDate = "";
        String wsDateMonth = "";
        String wsDateDay = "";
        String wsDateYear = "";
        String wsC01wOutputFile = "";
        String wsC01wOperLocCode = "";
        String wsC01wType = "";
        String wsC01wSocSecNum = "";
        String wsC01wReferenceNum = "";
        String wsC01wTotAmount = "";
        String wsC01wPayYear = "";
        String wsC01wEmpName = "";
        String wsFiller1 = "";
        String wsC01wAddress1 = "";
        String wsC01wAddress2 = "";
        String wsC01wCityMaster = "";
        String wsC01wCityOvfl = "";
        String wsC01wState = "";
        String wsC01wZip = "";
        String wsC01wDescription = "";
        String wsC01wDescBrDept = "";
        String wsC01wDescCheckNum = "";
        String wsC01wDescNum = "";
        String wsC01wFormNum = "";
        String wsC01wComments = "";
        String wsC01wAmountInd = "";
        String wsC01wSsnInd = "";
        String wsC01wDealerCode = "";
        String wsFiller2 = "";
        String wsC01wTransCode = "";
        String wsC01wOutputTrailer = "";
        String wsC01wTrlrOperLocCode = "";
        String wsFiller3 = "";
        String wsC01wTrlrBatchNum = "";
        String wsC01wTrlrTotRecords = "";
        String wsC01wTrlrTotAmount = "";
        String wsFiller4 = "";
        String wsC01wTrlrTransCode = "";
        String wsR01rControlCard = "";
        String wsR01rCcRunDate = "";
        String wsR01rCcCc = "";
        String wsR01rCcYy = "";
        String wsR01rCcMm = "";
        String wsR01rCcDd = "";
        String wsCounters = "";
        String wsTotRecdsRead = "ZERO";
        String wsTotWrittenCnt = "ZERO";
        String ws1099TrlrCnt = "ZERO";
        String wsTot1099Amt = "ZERO";
        String ws1099TrlrAmount = "ZERO";
        String wsTotBatchNum = "ZERO";
        String wsSwitches = "";
        String wsEofSw = "SPACE";
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
        String ws99Lit = "99";
        String ws1Lit = "01";
        String ws7Lit = "07";
        String wsMiscAlphaLiterals = "";
        String wsYesLit = "Y";
        String wsNoLit = "N";
        String wsManualCheckLit = "M";
        String ws1xLit = "1";
        String ws2Lit = "2";
        String wsTLit = "T";
        String wsALit = "A";
        String ws20071Lit = "  #20071";
        String wsSpaces4Lit = "    ";
        String wsSpaces5Lit = "     ";
        String wsZeroes4Lit = "0000";
        String wsZeroes5Lit = "00000";
        String wsAllAsterisks = "ALL *";
        String wsSourceSystemLit = "1099    ";
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
        String wsCalcTotAmount = "";
        String wsCalcTotAmt = "";
        String wsCalcTotAmtOvfl = "";
        String wsCalcTotAmtOut = "";
        String wsManualTotAmount = "";
        String wsManualTotAmt = "";
        String wsManualTotAmtOvf = "";
        String wsManualTotAmtOut = "";
        String wsReferenceNum = "ZEROES";
        String wsZipAreas = "";
        String wsZipCheck = "";
        String wsZipCheck1 = "";
        String wsZipCheckNum = "";
        String wsEvalNumZip = "";
        String wsEvalNumZip4 = "";
        String wsEvalNumZip5 = "";
        String wsEvalNumZipSec = "";
        String wsEvalNum2Zip5 = "";
        String wsEvalNum2Zip4 = "";
        String wsEvalAlphaZip = "";
        String wsEvalAlphaZip4 = "";
        String wsEvalAlphaZip5 = "";
        String wsEvalAlphaZipSec = "";
        String wsEvalAlpha2Zip5 = "";
        String wsEvalAlpha2Zip4 = "";
        String wsZipOk = "";
        String wsZipOk5 = "";
        String wsZipOk4 = "";
        String wsZipOkNum = "";
        String wsZipOkNum5 = "";
        String wsZipOkNum4 = "";
    }

    @Override
    public RepeatStatus execute(StepContribution contribution, ChunkContext chunkContext) {
        ProgramState state = new ProgramState();
        try {
            mainline(state);
        } catch (Exception e) {
            throw new RuntimeException("Error executing CCAC6350", e);
        }
        return RepeatStatus.FINISHED;
    }

    private void openFiles(ProgramState state) {
        try {
            Path testDir = Paths.get("../work/mainframe_clean/testcases", "CCAC6350");
            Path ten99M01r1099MasterFilePath = testDir.resolve("input/master.txt");
            if (Files.exists(ten99M01r1099MasterFilePath)) state.ten99M01r1099MasterFileReader = Files.newBufferedReader(ten99M01r1099MasterFilePath);
            Path ten99R01rControlCardPath = testDir.resolve("input/control.txt");
            if (Files.exists(ten99R01rControlCardPath)) state.ten99R01rControlCardReader = Files.newBufferedReader(ten99R01rControlCardPath);
            Path ten99C01w1099SummaryFilePath = testDir.resolve("output/summary_out.txt");
            Files.createDirectories(ten99C01w1099SummaryFilePath.getParent());
            state.ten99C01w1099SummaryFileWriter = Files.newBufferedWriter(ten99C01w1099SummaryFilePath);
            Path ccE01wDisplayFilePath = testDir.resolve("output/sysout.txt");
            Files.createDirectories(ccE01wDisplayFilePath.getParent());
            state.ccE01wDisplayFileWriter = Files.newBufferedWriter(ccE01wDisplayFilePath);
        } catch (Exception e) { throw new RuntimeException("Open files failed", e); }
    }

    private void closeFiles(ProgramState state) {
        try {
            if (state.ten99M01r1099MasterFileReader != null) state.ten99M01r1099MasterFileReader.close();
            if (state.ten99R01rControlCardReader != null) state.ten99R01rControlCardReader.close();
            if (state.ten99C01w1099SummaryFileWriter != null) state.ten99C01w1099SummaryFileWriter.close();
            if (state.ccE01wDisplayFileWriter != null) state.ccE01wDisplayFileWriter.close();
        } catch (Exception e) { throw new RuntimeException("Close files failed", e); }
    }

    private void readTen99M01r1099MasterFile(ProgramState state) {
        try {
            if (state.ten99M01r1099MasterFileReader == null) { state.ten99M01r1099MasterFileEof = true; return; }
            state.ten99M01r1099MasterFileLine = state.ten99M01r1099MasterFileReader.readLine();
            if (state.ten99M01r1099MasterFileLine == null) state.ten99M01r1099MasterFileEof = true;
        } catch (Exception e) { throw new RuntimeException("Read error", e); }
    }

    private void readTen99R01rControlCard(ProgramState state) {
        try {
            if (state.ten99R01rControlCardReader == null) { state.ten99R01rControlCardEof = true; return; }
            state.ten99R01rControlCardLine = state.ten99R01rControlCardReader.readLine();
            if (state.ten99R01rControlCardLine == null) state.ten99R01rControlCardEof = true;
        } catch (Exception e) { throw new RuntimeException("Read error", e); }
    }

    private void writeTen99C01w1099SummaryFile(ProgramState state, String line) {
        try {
            if (state.ten99C01w1099SummaryFileWriter != null && line != null) {
                state.ten99C01w1099SummaryFileWriter.write(line);
                state.ten99C01w1099SummaryFileWriter.newLine();
            }
        } catch (Exception e) { throw new RuntimeException("Write error", e); }
    }

    private void writeCcE01wDisplayFile(ProgramState state, String line) {
        try {
            if (state.ccE01wDisplayFileWriter != null && line != null) {
                state.ccE01wDisplayFileWriter.write(line);
                state.ccE01wDisplayFileWriter.newLine();
            }
        } catch (Exception e) { throw new RuntimeException("Write error", e); }
    }


    // ========================================
    // TRANSLATED BUSINESS LOGIC
    // ========================================

    private void mainline(ProgramState state) {
        // TODO: Translate COBOL paragraph '0000-MAINLINE'
        // Original COBOL:
        // 039200***************************************************************** 03920001
        // 039300*                                                               * 03930001
        // 039400* THIS PARAGRAPH CONTROLS MAIN PROCESSING LOGIC                 * 03940001
        // 039500*                                                               * 03950001
        // 039600* PERFORMS:  1000-GENERAL-HOUSEKEEPING                          * 03960001
        // 039700*            3000-PROCESS-MASTER-FILE                           * 03970001
        // 039800*            9000-CLOSING-ROUTINE                               * 03980001
        // 039900*                                                               * 03990001
        // 040000***************************************************************** 04000001
        //     PERFORM 1000-GENERAL-HOUSEKEEPING
        //     PERFORM 3000-PROCESS-MASTER-FILE  UNTIL
        //                  END-OF-INPUT
        //     PERFORM 9000-CLOSING-ROUTINE
        // 040900*OQ01 - "GOBACK" FLAGGED PER STOP-RUN OPTION                      04090001
        //     GOBACK
        //     .
        //     EJECT
        // 041300***************************************************************** 04130001
        // 041400* 1000-GENERAL-HOUSEKEEPING.                                      04140001
        // 041500*                                                                 04150001
        // 041600* OPEN AND INITIALIZE FILES.  DO FIRST READ ON MASTER FILE.       04160001
        // 041700*                                                                 04170001
        // 041800* PERFORMED FROM 0000-MAINLINE.                                   04180001
        // 041900***************************************************************** 04190001
        generalHousekeeping(state);
        processMasterFile(state);
        closingRoutine(state);
    }
    private void generalHousekeeping(ProgramState state) {
        // TODO: Translate COBOL paragraph '1000-GENERAL-HOUSEKEEPING'
        // Original COBOL:
        // PERFORM 1400-OPEN-FILES
        //     PERFORM 1600-INITIALIZATION
        //     MOVE SWA-CURR-MO                  TO WS-DATE-MONTH
        //     MOVE SWA-CURR-DA                  TO WS-DATE-DAY
        //     MOVE SWA-CURR-YR                  TO WS-DATE-YEAR
        //     PERFORM 7500-READ-MASTER-FILE
        //     IF END-OF-INPUT
        //        MOVE WS-EMPTY-FILE-LIT         TO CC-E01W-DISPLAY-RCD
        //        MOVE WS-99-LIT                 TO RETURN-CODE
        //        PERFORM 8999-WRITE-SYSOUT
        //        PERFORM 9998-COREDUMP
        //     ELSE
        //         IF MASTER-HEADER
        //            PERFORM 7500-READ-MASTER-FILE
        //            IF END-OF-INPUT
        //               MOVE WS-EMPTY-FILE-LIT  TO CC-E01W-DISPLAY-RCD
        //               MOVE WS-99-LIT          TO RETURN-CODE
        //               PERFORM 8999-WRITE-SYSOUT
        //               PERFORM 9998-COREDUMP
        //            END-IF
        //         END-IF
        //     END-IF
        //     MOVE WS-M01R-MASTER-FILE   TO WS-PREV-MASTER-FILE
        //     PERFORM 3900-SET-RECORD-KEY
        //     .
        //     EJECT
        // 045100***************************************************************** 04510001
        // 045200* 1400-OPEN-FILES.                                                04520001
        // 045300*                                                                 04530001
        // 045400* OPEN FILES.                                                     04540001
        // 045500*                                                                 04550001
        // 045600* PEFORMED FROM 1000-GENERAL-HOUSEKEEPING.                        04560001
        // 045700***************************************************************** 04570001
        openFiles(state);
        initialization(state);
        readMasterFile(state);
        writeSysout(state);
        coredump(state);
        readMasterFile(state);
        writeSysout(state);
        coredump(state);
        setRecordKey(state);
    }
    private void initialization(ProgramState state) {
        // TODO: Translate COBOL paragraph '1600-INITIALIZATION'
        // Original COBOL:
        // INITIALIZE WS-OUTPUT-RECORD-LINE
        //     INITIALIZE WS-C01W-OUTPUT-FILE
        //     PERFORM 1700-INIT-FIRST-DISPLAY-RCD
        //     PERFORM 7000-READ-CONTROL-CARD
        //     .
        //     EJECT
        // 048200***************************************************************** 04820001
        // 048300* 1700-INIT-FIRST-DISPLAY-RCD.                                    04830001
        // 048400*                                                                 04840001
        // 048500* CREATE FIRST DISPLAY RECORD.                                    04850001
        // 048600*                                                                 04860001
        // 048700* PEFORMED FROM 1600-INITIALIZATION.                              04870001
        // 048800***************************************************************** 04880001
        initFirstDisplayRcd(state);
        readControlCard(state);
    }
    private void initFirstDisplayRcd(ProgramState state) {
        // TODO: Translate COBOL paragraph '1700-INIT-FIRST-DISPLAY-RCD'
        // Original COBOL:
        // 049100*++INCLUDE C2INZ001                                               04910001
        // COPY C2INZ001.
        //   EJECT
        // 049500***************************************************************** 04950001
        // 049600* 3000-PROCESS-MASTER-FILE.                                       04960001
        // 049700*                                                                 04970001
        // 049800* SUMMARIZE THE 1099'S FOR OUTPUT.                                04980001
        // 049900*                                                                 04990001
        // 050000* PEFORMED FROM 0000-MAINLINE.                                    05000001
        // 050100***************************************************************** 05010001

    }
    private void processMasterFile(ProgramState state) {
        // TODO: Translate COBOL paragraph '3000-PROCESS-MASTER-FILE'
        // Original COBOL:
        // ADD WS-1-LIT            TO WS-TOT-RECDS-READ
        //     IF WS-PREV-OPER-LOC-CODE = WS-M01R-OPER-LOC-CODE
        //        IF WS-PREV-REC-KEY = WS-CURR-REC-KEY
        //           IF WS-PREV-BR-DEPT = WS-M01R-BR-DEPT
        //              IF WS-M01R-CHECK-TYPE = WS-MANUAL-CHECK-LIT
        //                 PERFORM 4100-CREATE-MANUAL-OUT-RECORD
        //              ELSE
        //                 CONTINUE
        //              END-IF
        //           ELSE
        //              PERFORM 3100-BREAK-FOR-BRANCH
        //           END-IF
        //        ELSE
        //           PERFORM 3200-BREAK-FOR-SOC-SEC-NUM
        //        END-IF
        //     ELSE
        //        PERFORM 3300-BREAK-FOR-OPER-LOC-CODE
        //     END-IF
        //     EVALUATE TRUE
        //     WHEN MASTER-TRAILER
        //          CONTINUE
        //     WHEN WS-M01R-CHECK-TYPE = WS-MANUAL-CHECK-LIT
        //          CONTINUE
        //     WHEN OTHER
        //          ADD WS-M01R-1099-AMT-OUT   TO WS-CALC-TOT-AMOUNT
        //          MOVE WS-M01R-MASTER-FILE   TO WS-PREV-MASTER-FILE
        //     END-EVALUATE
        //     PERFORM 7500-READ-MASTER-FILE
        //     .
        //     EJECT
        // 054000************************************************************      05400001
        // 054100* 3100-BREAK-FOR-BRANCH.                                          05410001
        // 054200*                                                                 05420001
        // 054300* CHECK FOR MANUAL CHECK WHEN THE BRANCH CHANGES.                 05430001
        // 054400*                                                                 05440001
        // 054500* PERFORMED FROM 3000-PROCESS-MASTER-FILE.                        05450001
        // 054600************************************************************      05460001
        createManualOutRecord(state);
        breakForBranch(state);
        breakForSocSecNum(state);
        breakForOperLocCode(state);
        readMasterFile(state);
    }
    private void breakForBranch(ProgramState state) {
        // TODO: Translate COBOL paragraph '3100-BREAK-FOR-BRANCH'
        // Original COBOL:
        // IF WS-PREV-CHECK-TYPE = WS-MANUAL-CHECK-LIT
        //        IF WS-M01R-CHECK-TYPE = WS-MANUAL-CHECK-LIT
        //           PERFORM 4100-CREATE-MANUAL-OUT-RECORD
        //        ELSE
        //           CONTINUE
        //        END-IF
        //     ELSE
        //        PERFORM 4000-CREATE-SUMM-OUT-RECORD
        //        IF WS-M01R-CHECK-TYPE = WS-MANUAL-CHECK-LIT
        //           PERFORM 4100-CREATE-MANUAL-OUT-RECORD
        //        ELSE
        //           CONTINUE
        //        END-IF
        //     END-IF
        //     .
        //     EJECT
        // 056600************************************************************      05660001
        // 056700* 3200-BREAK-FOR-SOC-SEC-NUM.                                     05670001
        // 056800*                                                                 05680001
        // 056900* CHECK FOR MANUAL CHECK WHEN THE SOCIAL SECURITY NUMBER          05690001
        // 057000* CHANGES.                                                        05700001
        // 057100*                                                                 05710001
        // 057200* PERFORMED FROM 3000-PROCESS-MASTER-FILE.                        05720001
        // 057300************************************************************      05730001
        createManualOutRecord(state);
        createSummOutRecord(state);
        createManualOutRecord(state);
    }
    private void breakForSocSecNum(ProgramState state) {
        // TODO: Translate COBOL paragraph '3200-BREAK-FOR-SOC-SEC-NUM'
        // Original COBOL:
        // IF WS-PREV-CHECK-TYPE = WS-MANUAL-CHECK-LIT
        //        IF WS-M01R-CHECK-TYPE = WS-MANUAL-CHECK-LIT
        //           PERFORM 4100-CREATE-MANUAL-OUT-RECORD
        //        ELSE
        //           CONTINUE
        //        END-IF
        //     ELSE
        //        PERFORM 4000-CREATE-SUMM-OUT-RECORD
        //        IF WS-M01R-CHECK-TYPE = WS-MANUAL-CHECK-LIT
        //           PERFORM 4100-CREATE-MANUAL-OUT-RECORD
        //           MOVE WS-M01R-MASTER-FILE TO WS-PREV-MASTER-FILE
        //        ELSE
        //           CONTINUE
        //        END-IF
        //     END-IF
        //     .
        //     EJECT
        // 059400************************************************************      05940001
        // 059500* 3300-BREAK-FOR-OPER-LOC-CODE.                                   05950001
        // 059600*                                                                 05960001
        // 059700* CHECK FOR MANUAL CHECK WHEN THE OPERATION LOCATION CODE         05970001
        // 059800* CHANGES.                                                        05980001
        // 059900*                                                                 05990001
        // 060000* PERFORMED FROM 3000-PROCESS-MASTER-FILE.                        06000001
        // 060100************************************************************      06010001
        createManualOutRecord(state);
        createSummOutRecord(state);
        createManualOutRecord(state);
    }
    private void breakForOperLocCode(ProgramState state) {
        // TODO: Translate COBOL paragraph '3300-BREAK-FOR-OPER-LOC-CODE'
        // Original COBOL:
        // IF WS-PREV-CHECK-TYPE = WS-MANUAL-CHECK-LIT
        //        PERFORM 5000-PROCESS-TRAILER
        //        IF WS-M01R-CHECK-TYPE = WS-MANUAL-CHECK-LIT
        //           PERFORM 4100-CREATE-MANUAL-OUT-RECORD
        //        ELSE
        //           CONTINUE
        //        END-IF
        //     ELSE
        //        PERFORM 4000-CREATE-SUMM-OUT-RECORD
        //        PERFORM 5000-PROCESS-TRAILER
        //        IF WS-M01R-CHECK-TYPE = WS-MANUAL-CHECK-LIT
        //           PERFORM 4100-CREATE-MANUAL-OUT-RECORD
        //           MOVE WS-M01R-MASTER-FILE TO WS-PREV-MASTER-FILE
        //        ELSE
        //           CONTINUE
        //        END-IF
        //     END-IF
        //     .
        processTrailer(state);
        createManualOutRecord(state);
        createSummOutRecord(state);
        processTrailer(state);
        createManualOutRecord(state);
    }
    private void setRecordKey(ProgramState state) {
        // TODO: Translate COBOL paragraph '3900-SET-RECORD-KEY'
        // Original COBOL:
        // MOVE WS-M01R-SOC-SEC-NUM       TO WS-CURR-TAXID
        //     MOVE WS-M01R-TAX-TYPE          TO WS-CURR-TAX-TYPE
        //     MOVE WS-M01R-TIN-IND           TO WS-CURR-TAX-IND
        //     MOVE WS-PREV-SOC-SEC-NUM       TO WS-PREV-TAXID
        //     MOVE WS-PREV-MSTR-TAX-TYPE     TO WS-PREV-TAX-TYPE
        //     MOVE WS-PREV-MSTR-TIN-IND      TO WS-PREV-TIN-IND
        //     .
        // 062400************************************************************      06240001
        // 062500* 4000-CREATE-NEW-OUTPUT-RECORD.                                  06250001
        // 062600*                                                                 06260001
        // 062700* CREATE SUMMARIZED 1099 RECORD FROM MASTER FILE.                 06270001
        // 062800*                                                                 06280001
        // 062900* PERFORMED FROM 3000-PROCESS-MASTER-FILE.                        06290001
        // 063000************************************************************      06300001

    }
    private void createSummOutRecord(ProgramState state) {
        // TODO: Translate COBOL paragraph '4000-CREATE-SUMM-OUT-RECORD'
        // Original COBOL:
        // MOVE WS-PREV-OPER-LOC-CODE TO WS-C01W-OPER-LOC-CODE
        //     MOVE WS-PREV-SOC-SEC-NUM   TO WS-C01W-SOC-SEC-NUM
        //     MOVE WS-R01R-CC-YY         TO WS-C01W-PAY-YEAR
        //     MOVE WS-PREV-EMP-NAME      TO WS-C01W-EMP-NAME
        //     MOVE WS-PREV-ADDRESS-1     TO WS-C01W-ADDRESS-1
        //     MOVE WS-PREV-ADDR2-OUT     TO WS-C01W-ADDRESS-2
        //     MOVE WS-PREV-CITY          TO WS-C01W-CITY-MASTER
        //     MOVE WS-PREV-STATE         TO WS-C01W-STATE
        //     MOVE WS-PREV-BR-DEPT       TO WS-C01W-DESC-BR-DEPT
        //     MOVE WS-PREV-CHECK-NUMBER  TO WS-C01W-DESC-CHECK-NUM
        //     MOVE WS-PREV-MSTR-TAX-TYPE TO WS-C01W-AMOUNT-IND
        //     IF (WS-PREV-MSTR-TIN-IND = WS-1X-LIT) OR
        //        (WS-PREV-MSTR-TIN-IND = WS-2-LIT)
        //          MOVE WS-PREV-MSTR-TIN-IND TO WS-C01W-SSN-IND
        //     ELSE MOVE WS-1X-LIT            TO WS-C01W-SSN-IND
        //     END-IF
        //     MOVE WS-A-LIT              TO WS-C01W-TYPE
        //     MOVE WS-A-LIT              TO WS-C01W-TRANS-CODE
        //     IF WS-CALC-TOT-AMT-OVFL > ZEROES
        //        MOVE WS-MONEY-ERROR-LIT  TO CC-E01W-DISPLAY-RCD
        //        MOVE WS-99-LIT           TO RETURN-CODE
        //        PERFORM 8999-WRITE-SYSOUT
        //        PERFORM 9998-COREDUMP
        //     ELSE
        //        MOVE WS-CALC-TOT-AMT-OUT TO WS-C01W-TOT-AMOUNT
        //     END-IF
        //     INITIALIZE WS-ZIP-CHECK
        //     MOVE WS-PREV-ZIP            TO WS-ZIP-CHECK
        //     PERFORM 4200-CHECK-ZIP-CODE
        //     MOVE WS-ZIP-OK-NUM          TO WS-C01W-ZIP
        //     ADD WS-CALC-TOT-AMT-OUT     TO WS-1099-TRLR-AMOUNT
        //     ADD WS-1-LIT                TO WS-1099-TRLR-CNT
        //     PERFORM 8700-WRITE-OUTPUT-RECORD
        //     INITIALIZE WS-CALC-TOT-AMOUNT
        //     .
        //     EJECT
        // 066900************************************************************      06690001
        // 067000* 4100-CREATE-MANUAL-OUT-RECORD.                                  06700001
        // 067100*                                                                 06710001
        // 067200* CREATE MANUAL CHECK 1099 RECORD FROM MASTER FILE.               06720001
        // 067300*                                                                 06730001
        // 067400* PERFORMED FROM 3000-PROCESS-MASTER-FILE.                        06740001
        // 067500************************************************************      06750001
        writeSysout(state);
        coredump(state);
        checkZipCode(state);
        writeOutputRecord(state);
    }
    private void createManualOutRecord(ProgramState state) {
        // TODO: Translate COBOL paragraph '4100-CREATE-MANUAL-OUT-RECORD'
        // Original COBOL:
        // MOVE WS-M01R-OPER-LOC-CODE TO WS-C01W-OPER-LOC-CODE
        //     MOVE WS-M01R-SOC-SEC-NUM   TO WS-C01W-SOC-SEC-NUM
        //     MOVE WS-R01R-CC-YY         TO WS-C01W-PAY-YEAR
        //     MOVE WS-M01R-EMP-NAME      TO WS-C01W-EMP-NAME
        //     MOVE WS-M01R-ADDRESS-1     TO WS-C01W-ADDRESS-1
        //     MOVE WS-M01R-ADDR2-OUT     TO WS-C01W-ADDRESS-2
        //     MOVE WS-M01R-CITY          TO WS-C01W-CITY-MASTER
        //     MOVE WS-M01R-STATE         TO WS-C01W-STATE
        //     MOVE WS-M01R-BR-DEPT       TO WS-C01W-DESC-BR-DEPT
        //     MOVE WS-M01R-COMMENTS      TO WS-C01W-COMMENTS
        //     MOVE WS-20071-LIT          TO WS-C01W-FORM-NUM
        //     MOVE WS-M01R-TAX-TYPE      TO WS-C01W-AMOUNT-IND
        //     IF (WS-M01R-TIN-IND = WS-1X-LIT) OR
        //        (WS-M01R-TIN-IND = WS-2-LIT)
        //          MOVE WS-M01R-TIN-IND    TO WS-C01W-SSN-IND
        //     ELSE MOVE WS-1X-LIT           TO WS-C01W-SSN-IND
        //     END-IF
        //     MOVE WS-A-LIT              TO WS-C01W-TYPE
        //     MOVE WS-A-LIT              TO WS-C01W-TRANS-CODE
        //     IF WS-M01R-1099-AMT-OVFL > ZEROES
        //        MOVE WS-MONEY-ERROR-LIT   TO CC-E01W-DISPLAY-RCD
        //        MOVE WS-99-LIT            TO RETURN-CODE
        //        PERFORM 8999-WRITE-SYSOUT
        //        PERFORM 9998-COREDUMP
        //     ELSE
        //        MOVE WS-M01R-1099-AMT-OUT TO WS-C01W-TOT-AMOUNT
        //     END-IF
        //     INITIALIZE WS-ZIP-CHECK
        //     MOVE WS-M01R-ZIP             TO WS-ZIP-CHECK
        //     PERFORM 4200-CHECK-ZIP-CODE
        //     MOVE WS-ZIP-OK-NUM           TO WS-C01W-ZIP
        //     ADD WS-M01R-1099-AMT-OUT     TO WS-1099-TRLR-AMOUNT
        //     ADD WS-1-LIT                 TO WS-1099-TRLR-CNT
        //     PERFORM 8700-WRITE-OUTPUT-RECORD
        //     .
        //     EJECT
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
        writeSysout(state);
        coredump(state);
        checkZipCode(state);
        writeOutputRecord(state);
    }
    private void checkZipCode(ProgramState state) {
        // TODO: Translate COBOL paragraph '4200-CHECK-ZIP-CODE'
        // Original COBOL:
        // INITIALIZE WS-EVAL-NUM-ZIP
        //                WS-EVAL-ALPHA-ZIP
        //                WS-ZIP-OK
        //     EVALUATE TRUE
        //     WHEN WS-ZIP-CHECK-NUM IS NUMERIC
        //          MOVE WS-ZIP-CHECK-NUM TO WS-EVAL-NUM-ZIP
        //          PERFORM 4250-CHECK-NUMERIC-ZIP
        //     WHEN OTHER
        //          MOVE WS-ZIP-CHECK TO WS-EVAL-ALPHA-ZIP
        //          PERFORM 4260-CHECK-ALPHA-ZIP
        //     END-EVALUATE
        //     .
        //     EJECT
        // 074000************************************************************      07400001
        // 074100* 4250-CHECK-NUMERIC-ZIP.                                         07410001
        // 074200*                                                                 07420001
        // 074300* CHECK NUMERIC ZIP FOR CORRECT POSITION.                         07430001
        // 074400*                                                                 07440001
        // 074500*                                                                 07450001
        // 074600* PERFORMED FROM 4200-CHECK-ZIP-CODE.                             07460001
        // 074700************************************************************      07470001
        checkNumericZip(state);
        checkAlphaZip(state);
    }
    private void checkNumericZip(ProgramState state) {
        // TODO: Translate COBOL paragraph '4250-CHECK-NUMERIC-ZIP'
        // Original COBOL:
        // IF WS-EVAL-NUM-ZIP-5 > WS-ZEROES-5-LIT
        //        IF WS-EVAL-NUM-ZIP-4 > WS-ZEROES-4-LIT
        //           MOVE WS-EVAL-NUM-ZIP TO WS-ZIP-OK-NUM
        //        ELSE MOVE WS-SPACES-4-LIT TO WS-ZIP-OK-4
        //             MOVE WS-EVAL-NUM-ZIP-5 TO WS-ZIP-OK-NUM-5
        //        END-IF
        //     ELSE IF WS-EVAL-NUM2-ZIP-5 > WS-ZEROES-5-LIT
        //             MOVE WS-EVAL-NUM2-ZIP-5 TO WS-ZIP-OK-5
        //          ELSE MOVE WS-ZIP-ZEROES-LIT TO CC-E01W-DISPLAY-RCD
        //               MOVE WS-99-LIT         TO RETURN-CODE
        //               PERFORM 8999-WRITE-SYSOUT
        //          END-IF
        //     END-IF
        //     IF WS-ZIP-OK-NUM-4 = WS-ZEROES-4-LIT
        //        MOVE WS-SPACES-4-LIT TO WS-ZIP-OK-4
        //     END-IF
        //     .
        //     EJECT
        // 076900************************************************************      07690001
        // 077000* 4260-CHECK-ALPHA-ZIP.                                           07700001
        // 077100*                                                                 07710001
        // 077200* CHECK ALPHANUM ZIP FOR CORRECT POSITION.                        07720001
        // 077300*                                                                 07730001
        // 077400*                                                                 07740001
        // 077500* PERFORMED FROM 4200-CHECK-ZIP-CODE.                             07750001
        // 077600************************************************************      07760001
        writeSysout(state);
    }
    private void checkAlphaZip(ProgramState state) {
        // TODO: Translate COBOL paragraph '4260-CHECK-ALPHA-ZIP'
        // Original COBOL:
        // IF WS-EVAL-ALPHA-ZIP-4 = WS-SPACES-4-LIT
        //        IF WS-EVAL-ALPHA-ZIP-5 > WS-SPACES-5-LIT
        //           MOVE WS-EVAL-ALPHA-ZIP-5 TO WS-ZIP-OK-5
        //           MOVE WS-SPACES-4-LIT     TO WS-ZIP-OK-4
        //        ELSE MOVE WS-SPACES-4-LIT TO WS-ZIP-OK-4
        //             MOVE WS-EVAL-NUM-ZIP-5 TO WS-ZIP-OK-5
        //        END-IF
        //     ELSE IF WS-EVAL-ALPHA2-ZIP-5 > WS-SPACES-5-LIT
        //             MOVE WS-EVAL-ALPHA-ZIP TO WS-ZIP-OK
        //          ELSE MOVE WS-ZIP-SPACES-LIT TO CC-E01W-DISPLAY-RCD
        //               MOVE WS-99-LIT        TO RETURN-CODE
        //               PERFORM 8999-WRITE-SYSOUT
        //          END-IF
        //     END-IF
        //     .
        //     EJECT
        // 079500************************************************************      07950001
        // 079600* 5000-PROCESS-TRAILER.                                           07960001
        // 079700*                                                                 07970001
        // 079800* CREATE TRAILER RECORD WITH RECORD COUNT AND 1099 SUMMARIZED     07980001
        // 079900* MONRY AMOUNT.                                                   07990001
        // 080000*                                                                 08000001
        // 080100* PERFORMED FROM 0000-MAINLINE.                                   08010001
        // 080200************************************************************      08020001
        writeSysout(state);
    }
    private void processTrailer(ProgramState state) {
        // TODO: Translate COBOL paragraph '5000-PROCESS-TRAILER'
        // Original COBOL:
        // INITIALIZE WS-C01W-OUTPUT-TRAILER
        //     MOVE WS-PREV-OPER-LOC-CODE  TO WS-C01W-TRLR-OPER-LOC-CODE
        //     MOVE WS-1099-TRLR-CNT       TO WS-C01W-TRLR-TOT-RECORDS
        //     MOVE WS-1099-TRLR-AMOUNT    TO WS-C01W-TRLR-TOT-AMOUNT
        //     MOVE WS-T-LIT               TO WS-C01W-TRLR-TRANS-CODE
        //     ADD WS-1-LIT                TO WS-TOT-BATCH-NUM
        //     MOVE WS-TOT-BATCH-NUM       TO WS-C01W-TRLR-BATCH-NUM
        //     PERFORM 8700-WRITE-OUTPUT-RECORD
        //     INITIALIZE                     WS-1099-TRLR-CNT
        //                                    WS-1099-TRLR-AMOUNT
        //     .
        //     EJECT
        // 082200***************************************************************** 08220001
        // 082300* 7000-READ-CONTROL-CARD.                                         08230001
        // 082400*                                                                 08240001
        // 082500* READ THE CONTROL CARD.                                          08250001
        // 082600*                                                                 08260001
        // 082700* PEFORMED FROM 1600-INITIALIZATION.                              08270001
        // 082800***************************************************************** 08280001
        writeOutputRecord(state);
    }
    private void readControlCard(ProgramState state) {
        // TODO: Translate COBOL paragraph '7000-READ-CONTROL-CARD'
        // Original COBOL:
        // READ TEN99-R01R-CONTROL-CARD INTO WS-R01R-CONTROL-CARD
        //     AT END
        //        MOVE WS-EMPTY-CCRD-LIT        TO CC-E01W-DISPLAY-RCD
        //        MOVE WS-99-LIT                TO RETURN-CODE
        //        PERFORM 8999-WRITE-SYSOUT
        //        PERFORM 9998-COREDUMP
        //     END-READ
        //     .
        //     EJECT
        // 084000************************************************************      08400001
        // 084100* 7500-READ-MASTER-FILE.                                          08410001
        // 084200*                                                                 08420001
        // 084300* READ A RECORD FROM THE MASTER FILE AND SET END OF FILE          08430001
        // 084400* INDICATOR WHEN DONE.                                            08440001
        // 084500*                                                                 08450001
        // 084600* PERFORMED FROM 1600-INITIALIZATION                              08460001
        // 084700*                3000-PROCESS-MASTER-FILE.                        08470001
        // 084800************************************************************      08480001
        writeSysout(state);
        coredump(state);
    }
    private void readMasterFile(ProgramState state) {
        // TODO: Translate COBOL paragraph '7500-READ-MASTER-FILE'
        // Original COBOL:
        // READ TEN99-M01R-1099-MASTER-FILE INTO WS-M01R-MASTER-FILE
        //         AT END
        //             MOVE WS-YES-LIT TO WS-EOF-SW
        //     END-READ
        //     IF END-OF-INPUT
        //          CONTINUE
        //     ELSE
        //          PERFORM 3900-SET-RECORD-KEY
        //     END-IF
        //     .
        //     EJECT
        // 085700***************************************************************** 08570001
        // 085800* 8700-WRITE-OUTPUT-RECORD.                                       08580001
        // 085900*                                                                 08590001
        // 086000* WRITE OUT THE SUMMARIZED 1099 RECORD.                           08600001
        // 086100*                                                                 08610001
        // 086200* PERFORMED FROM 4000-CREATE-OUTPUT-RECORD.                       08620001
        // 086300*                4100-CREATE-MANUAL-RECORD.                       08630001
        // 086400***************************************************************** 08640001
        setRecordKey(state);
    }
    private void writeOutputRecord(ProgramState state) {
        // TODO: Translate COBOL paragraph '8700-WRITE-OUTPUT-RECORD'
        // Original COBOL:
        // MOVE WS-C01W-OUTPUT-FILE   TO WS-OUTPUT-RECORD-LINE
        //     WRITE TEN99-C01W-1099-SUMMARY-RCD
        //                     FROM WS-OUTPUT-RECORD-LINE
        //     END-WRITE
        //     ADD WS-1-LIT             TO WS-TOT-WRITTEN-CNT
        //     INITIALIZE                  WS-OUTPUT-RECORD-LINE
        //                                 WS-C01W-OUTPUT-FILE
        //     .
        //     EJECT
        // 088000*++INCLUDE C2INZ002                                               08800001
        // COPY C2INZ002.
        //     EJECT
        // 088400***************************************************************** 08840001
        // 088500* 9000-CLOSING-ROUTINE.                                           08850001
        // 088600*                                                                 08860001
        // 088700* PERFORM END OF PROGRAM FUNCTIONS.                               08870001
        // 088800*                                                                 08880001
        // 088900* PEFORMED FROM 0000-MAINLINE.                                    08890001
        // 089000***************************************************************** 08900001
        end(state);
    }
    private void closingRoutine(ProgramState state) {
        // TODO: Translate COBOL paragraph '9000-CLOSING-ROUTINE'
        // Original COBOL:
        // PERFORM 9100-WRITE-LAST-DISPLAY-RCD
        //     PERFORM 9999-CLOSE-FILES
        //     .
        //     EJECT
        // 089800***************************************************************** 08980001
        // 089900* 9100-WRITE-LAST-DISPLAY-RCD.                                    08990001
        // 090000*                                                                 09000001
        // 090100* WRITE LAST RECORD TO DISPLAY FILE WITH END OF RUN  STATISTICS.  09010001
        // 090200*                                                                 09020001
        // 090300* PEFORMED FROM 9000-CLOSING-ROUTINE.                             09030001
        // 090400***************************************************************** 09040001
        writeLastDisplayRcd(state);
        closeFiles(state);
    }
    private void writeLastDisplayRcd(ProgramState state) {
        // TODO: Translate COBOL paragraph '9100-WRITE-LAST-DISPLAY-RCD'
        // Original COBOL:
        // MOVE WS-EOJ-MSG-1           TO CC-E01W-DISPLAY-RCD
        //     PERFORM 8999-WRITE-SYSOUT
        //     MOVE WS-TOT-RECDS-READ      TO SCR-COUNT
        //     MOVE SYSOUT-COUNT-RCD       TO CC-E01W-DISPLAY-RCD
        //     PERFORM 8999-WRITE-SYSOUT
        //     MOVE WS-EOJ-MSG-2           TO CC-E01W-DISPLAY-RCD
        //     PERFORM 8999-WRITE-SYSOUT
        //     MOVE WS-TOT-WRITTEN-CNT     TO SCR-COUNT
        //     MOVE SYSOUT-COUNT-RCD       TO CC-E01W-DISPLAY-RCD
        //     PERFORM 8999-WRITE-SYSOUT
        // 092100*++INCLUDE C2INZ003                                               09210001
        // COPY C2INZ003.
        //   EJECT
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
        //     EJECT
        // 093700***************************************************************** 09370001
        // 093800* 9999-CLOSE-FILES.                                               09380001
        // 093900*                                                                 09390001
        // 094000* THIS WILL CLOSE ALL THE FILES.                                  09400001
        // 094100*                                                                 09410001
        // 094200* PEFORMED FROM 9000-CLOSING-ROUTINE.                             09420001
        // 094300***************************************************************** 09430001
        // 094500*++INCLUDE C2INZ005                                               09450001
        // COPY C2INZ005.
        //            TEN99-M01R-1099-MASTER-FILE
        //            TEN99-C01W-1099-SUMMARY-FILE
        //            TEN99-R01R-CONTROL-CARD
        //     .
        //     EJECT
        // 095200******************************************************************09520001
        // 095300*                PROGRAM CCAC6350 END OF SOURCE                  *09530001
        // 095400******************************************************************09540001
        // END PROGRAM CCAC6350.
        // 
        writeSysout(state);
        writeSysout(state);
        writeSysout(state);
        writeSysout(state);
    }

    private void writeSysout(ProgramState state) {
        // Stub for 8999-WRITE-SYSOUT
    }


    private void end(ProgramState state) {
        // Stub for END
    }


    private void coredump(ProgramState state) {
        // Stub for 9998-COREDUMP
    }

}
