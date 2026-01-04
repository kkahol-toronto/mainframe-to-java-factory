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
 * Complete Tasklet for COBOL program CCAC6330
 * Generated with full business logic translation
 */
public class CCAC6330Tasklet implements Tasklet {

    static class ProgramState {
        BufferedReader ten99T01rTransactionFileReader;
        boolean ten99T01rTransactionFileEof = false;
        String ten99T01rTransactionFileLine;
        BufferedReader ten99M01rMasterInFileReader;
        boolean ten99M01rMasterInFileEof = false;
        String ten99M01rMasterInFileLine;
        BufferedReader ten99T02rMasterUpdateFileReader;
        boolean ten99T02rMasterUpdateFileEof = false;
        String ten99T02rMasterUpdateFileLine;
        BufferedReader ten99R01rControlFileReader;
        boolean ten99R01rControlFileEof = false;
        String ten99R01rControlFileLine;
        BufferedWriter ten99T01wDuplicateTranFileWriter;
        boolean ten99T01wDuplicateTranFileEof = false;
        String ten99T01wDuplicateTranFileLine;
        BufferedWriter ten99M01wMasterOutFileWriter;
        boolean ten99M01wMasterOutFileEof = false;
        String ten99M01wMasterOutFileLine;

        // Processing state
        boolean endOfInput = false;
        String returnCode = "0";
        String recordsRead = "0";
        String recordsWritten = "0";

        // WORKING-STORAGE variables
        String panValet = "002CCAC6330  12/08/97";
        String pups2000 = "";
        String ten99T01rTransDtl = "";
        String tttdKeyCheckNum = "";
        String tranHeader = "LOW-VALUES";
        String tranTrailer = "HIGH-VALUES";
        String tttdKeyTaxType = "";
        String tttdKeyTinInd = "";
        String tttdSourceCode = "";
        String miscFormIssue = "MISC FORM    ";
        String tttdTransIndicator = "";
        String tttdDepartmentCode = "";
        String tttdName = "";
        String tttdAddress1 = "";
        String tttdAddress2 = "";
        String tttdCity = "";
        String tttdStateCode = "";
        String tttdZipCode = "";
        String tttdSocialSecNumber = "";
        String tttdSocialSecRemainder = "";
        String tttdOperLocation = "";
        String tttdCheckNum = "";
        String tttdCheckDate = "";
        String tttdTotalAmount = "";
        String tttd1099Amount = "";
        String tttdComments = "";
        String tttdCommentRest = "";
        String tttdNumberDisbRecords = "";
        String tttdDisbRecords = "";
        String tttdDisbEntryCode = "";
        String tttdDisb1099Indicator = "";
        String tttdDisbAmount = "";
        String tttdLineNbr = "";
        String ten99T01rTransHdr = "";
        String ttthHeadSource = "";
        String ttthHeadId = "";
        String ttthHeadDate = "";
        String ttthHeadCentury = "";
        String ttthHeadYear = "";
        String ttthHeadMonth = "";
        String ten99T01rTransTlr = "";
        String ttttTrailSource = "";
        String ttttTrailCnt = "";
        String ttttTrailAmt = "";
        String ten99M01rMasterInDtl = "";
        String tmmidCheckNumber = "";
        String tmmidDepartmentCode = "";
        String tmmidSocialSecNumber = "";
        String tmmidSocialSecRemainder = "";
        String tmmidOperationLoc = "";
        String tmmidSource = "";
        String tmmidName = "";
        String tmmidAddress1 = "";
        String tmmidAddress2 = "";
        String tmmidCity = "";
        String tmmidState = "";
        String tmmidZipCode = "";
        String tmmidDeleteIndicator = "";
        String nonDeleteInput = " ";
        String deleteInput = "D";
        String tmmidTotalAmount = "";
        String tmmid1099Amount = "";
        String tmmidCheckDate = "";
        String tmmidProcessDate = "";
        String tmmidNumDisbRecords = "";
        String tmmidDisbRecord = "";
        String tmmidDisbEntryCode = "";
        String tmmidDisb1099Indicator = "";
        String tmmidDisbAmount = "";
        String tmmidComments = "";
        String tmmidLineNbr = "";
        String tmmidTaxType = "";
        String tmmidTinInd = "";
        String ten99M01rMastInHdr = "";
        String tmmihHeadSource = "";
        String mastHeader = "LOW-VALUES";
        String tmmihHeadId = "";
        String tmmihHeadDate = "";
        String tmmihHeadCentury = "";
        String tmmihHeadYear = "";
        String tmmihHeadMonth = "";
        String ten99M01rMastInTlr = "";
        String tmmitTrailSource = "";
        String mastTrailer = "HIGH-VALUES";
        String tmmitNonDeleteTrailCnt = "";
        String tmmitNonDeleteTrailAmt = "";
        String tmmitDeleteTrailCnt = "";
        String tmmitDeleteTrailAmt = "";
        String ten99T02rMasterUpdateDtl = "";
        String ttmudCheckNumber = "";
        String ttmudDepartmentCode = "";
        String ttmudSocialSecNumber = "";
        String ttmudSocialSecRemainder = "";
        String ttmudOperationLoc = "";
        String ttmudSource = "";
        String ttmudName = "";
        String ttmudAddress1 = "";
        String ttmudAddress2 = "";
        String ttmudCity = "";
        String ttmudState = "";
        String ttmudZipCode = "";
        String ttmudDeleteChangeIndicator = "";
        String changeUpdate = "C";
        String deleteUpdate = "D";
        String ttmudTotalAmount = "";
        String ttmud1099Amount = "";
        String ttmudCheckDate = "";
        String ttmudProcessDate = "";
        String ttmudNumDisbRecords = "";
        String ttmudDisbRecords = "";
        String ttmudDisbEntryCode = "";
        String ttmudDisb1099Indicator = "";
        String ttmudDisbAmount = "";
        String ttmudComments = "";
        String ttmudLineNbr = "";
        String tmmudTaxType = "";
        String tmmudTinInd = "";
        String ten99T02rMastUpdtHdr = "";
        String ttmuhHeadSource = "";
        String updateHeader = "LOW-VALUES";
        String ttmuhHeadId = "";
        String ttmuhHeadDate = "";
        String ttmuhHeadCentury = "";
        String ttmuhHeadYear = "";
        String ttmuhHeadMonth = "";
        String ten99T02rMastUpdtTlr = "";
        String ttmutTrailSource = "";
        String updateTrailer = "HIGH-VALUES";
        String ttmutNonDeleteTrailCnt = "";
        String ttmutNonDeleteTrailAmt = "";
        String ttmutDeleteTrailCnt = "";
        String ttmutDeleteTrailAmt = "";
        String ten99R01rControlCardDtl = "";
        String trccdControlDate = "";
        String trccdControlCentury = "";
        String trccdControlYear = "";
        String trccdControlMonth = "";
        String yearEnd = "13 THRU 15";
        String newYear = "01";
        String regMonth = "02 THRU 15";
        String ten99M01wMasterOutDtl = "";
        String tmmodCheckNumber = "";
        String tmmodDepartmentCode = "";
        String tmmodSocialSecNumber = "";
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
        String nonDeleteOutput = " ";
        String deleteOutput = "D";
        String tmmodTotalAmount = "";
        String tmmod1099Amount = "";
        String tmmodCheckDate = "";
        String tmmodProcessDate = "";
        String tmmodNumDisbRecords = "";
        String tmmodDisbRecords = "";
        String tmmodDisbEntryCode = "";
        String tmmodDisb1099Indicator = "";
        String tmmodDisbAmount = "";
        String tmmodComments = "";
        String tmmodLineNbr = "";
        String tmmodTaxType = "";
        String tmmodTinInd = "";
        String ten99M01wMastOutHdr = "";
        String tmmohHeadSource = "";
        String mastOutHead = "LOW-VALUES";
        String tmmohHeadId = "";
        String tmmohHeadDate = "";
        String tmmohHeadCentury = "";
        String tmmohHeadYear = "";
        String tmmohHeadMonth = "";
        String ten99M01wMastOutTlr = "";
        String tmmotTrailSource = "";
        String mastOutTrailer = "HIGH-VALUES";
        String tmmotNonDeleteTrailCnt = "";
        String tmmotNonDeleteTrailAmt = "";
        String tmmotDeleteTrailCnt = "";
        String tmmotDeleteTrailAmt = "";
        String ten99T01wDuplicateDtl = "";
        String ttddKeyCheckNum = "";
        String ttddSourceCode = "";
        String ttddTransIndicator = "";
        String ttddDepartmentCode = "";
        String ttddName = "";
        String ttddAddress1 = "";
        String ttddAddress2 = "";
        String ttddCity = "";
        String ttddStateCode = "";
        String ttddZipCode = "";
        String ttddSocialSecNumber = "";
        String ttddSocialSecRemainder = "";
        String ttddOperLocation = "";
        String ttddCheckNum = "";
        String ttddCheckDate = "";
        String ttddTotalAmount = "";
        String ttdd1099Amount = "";
        String ttddComments = "";
        String ttddCommentRest = "";
        String ttddNumberDisbRecords = "";
        String ttddDisbributionRecords = "";
        String ttddDisbEntryCode = "";
        String ttddDisb1099Indicator = "";
        String ttddDisbAmount = "";
        String ttddLineNbr = "";
        String ttddTaxType = "";
        String ttddTinInd = "";
        String wsCounters = "";
        String wsMasterInCnt = "";
        String wsMastDeleteInCnt = "";
        String wsTransInCnt = "";
        String wsUpdatesInCnt = "";
        String wsMastOutCnt = "";
        String wsMastDeleteOutCnt = "";
        String wsDuplicateOutCnt = "";
        String wsAccumulators = "";
        String wsTransAmt = "";
        String wsDuplicateAmt = "";
        String wsMastDeleteInAmt = "";
        String wsMastInAmt = "";
        String wsMastDeleteUpAmt = "";
        String wsMastUpAmt = "";
        String wsMastDeleteOutAmt = "";
        String wsMastOutAmt = "";
        String wsFlags = "";
        String wsControlCardFlg = "N";
        String ccEof = "Y";
        String wsTransactionFlg = "N";
        String tranEof = "Y";
        String wsMasterFlg = "N";
        String masterEof = "Y";
        String wsUpdateFlg = "N";
        String updateEof = "Y";
        String wsAllInputsFlg = "N";
        String allEof = "Y";
        String disbSub = "";
        String wsSysoutMessages = "";
        String wsMsg1 = "";
        String wsMsg2 = "";
        String wsMsg3 = "";
        String wsEojSysoutMessages = "";
        String wsEojMsg1 = "";
        String wsEojMsg2 = "";
        String wsEojMsg3 = "";
        String wsEojMsg4 = "";
        String wsEojMsg5 = "";
        String wsEojMsg6 = "";
        String wsEojMsg7 = "";
        String wsMsg7Cnt = "";
        String wsEojMsg8 = "";
        String wsMsg8Cnt = "";
        String wsEojMsg9 = "";
        String wsMsg9Cnt = "";
        String wsEojMsg10 = "";
        String wsMsg10Cnt = "";
        String wsEojMsg11 = "";
        String wsMsg11Cnt = "";
        String wsEojMsg12 = "";
        String wsMsg12Cnt = "";
        String wsEojMsg13 = "";
        String wsMsg13Cnt = "";
        String wsEojMsg14 = "";
        String wsMsg14Cnt = "";
        String wsErrorMessages = "";
        String wsErrorMsg1 = "";
        String wsErrorMsg2 = "";
        String wsErrorMsg3 = "";
        String wsErrorMsg4 = "";
        String wsErrorMsg5 = "";
        String wsErrorMsg6 = "";
        String wsErrorMsg7 = "";
        String wsErrorMsg8 = "";
        String wsErrorMsg9 = "";
        String wsErrorMsg11 = "";
        String wsErrorMsg12 = "";
        String wsErrorMsg13 = "";
        String wsParagraphLits = "";
        String ws1000Init = "";
        String ws2000Main = "";
        String ws2200Proc = "";
        String ws2300Check = "";
        String ws7100Read = "";
        String ws9010Wrap = "";
        String wsMiscLits = "";
        String wsHighKeyLit = "";
        String wsLowKeyLit = "";
        String wsHeaderLit = "";
        String wsOneLit = "";
        String wsHoldValues = "";
        String wsHoldKeyCurrent = "";
        String wsHoldCurrentCheck = "";
        String wsHoldCurrentDept = "";
        String wsHoldCurrentTaxType = "";
        String wsHoldCurrentTin = "";
        String wsHoldKeyPrevious = "";
        String wsHoldTransKey = "";
        String wsHoldTransCheck = "";
        String wsHoldTransDept = "";
        String wsHoldTransTaxType = "";
        String wsHoldTransTin = "";
        String wsHoldLastWriteKey = "";
        String wsHoldLastWriteCheck = "";
        String wsHoldLastWriteDept = "";
        String wsHoldLastWriteTaxType = "";
        String wsHoldLastWriteTin = "";
        String wsUpdateKey = "";
        String wsUpdateCheck = "";
        String wsUpdateDept = "";
        String wsUpdateTaxType = "";
        String wsUpdateTin = "";
    }

    @Override
    public RepeatStatus execute(StepContribution contribution, ChunkContext chunkContext) {
        ProgramState state = new ProgramState();
        try {
            mainline(state);
        } catch (Exception e) {
            throw new RuntimeException("Error executing CCAC6330", e);
        }
        return RepeatStatus.FINISHED;
    }

    private void openFiles(ProgramState state) {
        try {
            Path testDir = Paths.get("../work/mainframe_clean/testcases", "CCAC6330");
            Path ten99T01rTransactionFilePath = testDir.resolve("input/ten99T01rTransactionFile.txt");
            if (Files.exists(ten99T01rTransactionFilePath)) state.ten99T01rTransactionFileReader = Files.newBufferedReader(ten99T01rTransactionFilePath);
            Path ten99M01rMasterInFilePath = testDir.resolve("input/master.txt");
            if (Files.exists(ten99M01rMasterInFilePath)) state.ten99M01rMasterInFileReader = Files.newBufferedReader(ten99M01rMasterInFilePath);
            Path ten99T02rMasterUpdateFilePath = testDir.resolve("input/master.txt");
            if (Files.exists(ten99T02rMasterUpdateFilePath)) state.ten99T02rMasterUpdateFileReader = Files.newBufferedReader(ten99T02rMasterUpdateFilePath);
            Path ten99R01rControlFilePath = testDir.resolve("input/control.txt");
            if (Files.exists(ten99R01rControlFilePath)) state.ten99R01rControlFileReader = Files.newBufferedReader(ten99R01rControlFilePath);
            Path ten99T01wDuplicateTranFilePath = testDir.resolve("output/ten99T01wDuplicateTranFile_out.txt");
            Files.createDirectories(ten99T01wDuplicateTranFilePath.getParent());
            state.ten99T01wDuplicateTranFileWriter = Files.newBufferedWriter(ten99T01wDuplicateTranFilePath);
            Path ten99M01wMasterOutFilePath = testDir.resolve("output/master_out.txt");
            Files.createDirectories(ten99M01wMasterOutFilePath.getParent());
            state.ten99M01wMasterOutFileWriter = Files.newBufferedWriter(ten99M01wMasterOutFilePath);
        } catch (Exception e) { throw new RuntimeException("Open files failed", e); }
    }

    private void closeFiles(ProgramState state) {
        try {
            if (state.ten99T01rTransactionFileReader != null) state.ten99T01rTransactionFileReader.close();
            if (state.ten99M01rMasterInFileReader != null) state.ten99M01rMasterInFileReader.close();
            if (state.ten99T02rMasterUpdateFileReader != null) state.ten99T02rMasterUpdateFileReader.close();
            if (state.ten99R01rControlFileReader != null) state.ten99R01rControlFileReader.close();
            if (state.ten99T01wDuplicateTranFileWriter != null) state.ten99T01wDuplicateTranFileWriter.close();
            if (state.ten99M01wMasterOutFileWriter != null) state.ten99M01wMasterOutFileWriter.close();
        } catch (Exception e) { throw new RuntimeException("Close files failed", e); }
    }

    private void readTen99T01rTransactionFile(ProgramState state) {
        try {
            if (state.ten99T01rTransactionFileReader == null) { state.ten99T01rTransactionFileEof = true; return; }
            state.ten99T01rTransactionFileLine = state.ten99T01rTransactionFileReader.readLine();
            if (state.ten99T01rTransactionFileLine == null) state.ten99T01rTransactionFileEof = true;
        } catch (Exception e) { throw new RuntimeException("Read error", e); }
    }

    private void readTen99M01rMasterInFile(ProgramState state) {
        try {
            if (state.ten99M01rMasterInFileReader == null) { state.ten99M01rMasterInFileEof = true; return; }
            state.ten99M01rMasterInFileLine = state.ten99M01rMasterInFileReader.readLine();
            if (state.ten99M01rMasterInFileLine == null) state.ten99M01rMasterInFileEof = true;
        } catch (Exception e) { throw new RuntimeException("Read error", e); }
    }

    private void readTen99T02rMasterUpdateFile(ProgramState state) {
        try {
            if (state.ten99T02rMasterUpdateFileReader == null) { state.ten99T02rMasterUpdateFileEof = true; return; }
            state.ten99T02rMasterUpdateFileLine = state.ten99T02rMasterUpdateFileReader.readLine();
            if (state.ten99T02rMasterUpdateFileLine == null) state.ten99T02rMasterUpdateFileEof = true;
        } catch (Exception e) { throw new RuntimeException("Read error", e); }
    }

    private void readTen99R01rControlFile(ProgramState state) {
        try {
            if (state.ten99R01rControlFileReader == null) { state.ten99R01rControlFileEof = true; return; }
            state.ten99R01rControlFileLine = state.ten99R01rControlFileReader.readLine();
            if (state.ten99R01rControlFileLine == null) state.ten99R01rControlFileEof = true;
        } catch (Exception e) { throw new RuntimeException("Read error", e); }
    }

    private void writeTen99T01wDuplicateTranFile(ProgramState state, String line) {
        try {
            if (state.ten99T01wDuplicateTranFileWriter != null && line != null) {
                state.ten99T01wDuplicateTranFileWriter.write(line);
                state.ten99T01wDuplicateTranFileWriter.newLine();
            }
        } catch (Exception e) { throw new RuntimeException("Write error", e); }
    }

    private void writeTen99M01wMasterOutFile(ProgramState state, String line) {
        try {
            if (state.ten99M01wMasterOutFileWriter != null && line != null) {
                state.ten99M01wMasterOutFileWriter.write(line);
                state.ten99M01wMasterOutFileWriter.newLine();
            }
        } catch (Exception e) { throw new RuntimeException("Write error", e); }
    }


    // ========================================
    // TRANSLATED BUSINESS LOGIC
    // ========================================

    private void mainline(ProgramState state) {
        // TODO: Translate COBOL paragraph '0000-MAINLINE'
        // Original COBOL:
        // 052600*=================================================================05260001
        // 052700*   PROGRAM CCAC6330 MAINLINE                                     05270001
        // 052800*   MAIN PROCESSING PARAGRAPH                                     05280001
        // 052900*=================================================================05290001
        //     PERFORM 1000-INITIALIZATION
        //     PERFORM 2000-MAIN-PROCESS UNTIL ALL-EOF
        //     PERFORM 9000-END-OF-JOB
        // 053300*OQ01 - "GOBACK" FLAGGED PER STOP-RUN OPTION                      05330001
        //     GOBACK
        //     .
        //     EJECT
        initialization(state);
        mainProcess(state);
        endOfJob(state);
    }
    private void initialization(ProgramState state) {
        // TODO: Translate COBOL paragraph '1000-INITIALIZATION'
        // Original COBOL:
        // 054000*=================================================================05400001
        // 054100*   THIS PARAGRAPH INITIALIZES THE READ AND COUNTERS AND CHECKS   05410001
        // 054200*   ALL HEADERS.                                                  05420001
        // 054300*=================================================================05430001
        //     PERFORM 1010-OPEN-FILES
        //     PERFORM 1020-INITIALIZE-SYSOUT
        //     INITIALIZE WS-COUNTERS WS-ACCUMULATORS
        //     INITIALIZE TEN99-M01W-MASTER-OUT-DTL TEN99-T01W-DUPLICATE-DTL05470001
        // 054800*--------------------------------------------------------------   05480001
        // 054900*  READ OF THE CONTROL CARD                                       05490001
        // 055000*--------------------------------------------------------------   05500001
        //     PERFORM 7000-READ-CONTROL-CARD
        //     IF CC-EOF
        //        MOVE WS-ERROR-MSG-1 TO CC-E01W-DISPLAY-RCD
        //        MOVE WS-1000-INIT TO SAR-PARAGRAPH
        //        PERFORM 8999-WRITE-SYSOUT
        //        PERFORM 9998-COREDUMP
        //     END-IF
        // 055800*--------------------------------------------------------------   05580001
        // 055900*  INITIAL READS OF TRANSACTION FILE FOR HEADER AND FIRST REC     05590001
        // 056000*--------------------------------------------------------------   05600001
        //     PERFORM 7100-READ-TRANSACTION-FILE
        //     IF TRAN-EOF AND YEAR-END
        //        MOVE WS-MSG-3 TO CC-E01W-DISPLAY-RCD
        //        PERFORM 8999-WRITE-SYSOUT
        //     ELSE
        //        IF TRAN-EOF
        //           MOVE WS-ERROR-MSG-2 TO CC-E01W-DISPLAY-RCD
        //           MOVE WS-1000-INIT TO SAR-PARAGRAPH
        //           PERFORM 8999-WRITE-SYSOUT
        //           PERFORM 9998-COREDUMP
        //        END-IF
        //     END-IF
        //     IF TRAN-HEADER
        //        IF TRCCD-CONTROL-DATE = TTTH-HEAD-DATE
        //           CONTINUE
        //        ELSE
        //           MOVE WS-ERROR-MSG-5 TO CC-E01W-DISPLAY-RCD
        //           MOVE WS-1000-INIT TO SAR-PARAGRAPH
        //           PERFORM 8999-WRITE-SYSOUT
        //           PERFORM 9998-COREDUMP
        //        END-IF
        //     ELSE
        //        MOVE WS-ERROR-MSG-6 TO CC-E01W-DISPLAY-RCD
        //        MOVE WS-1000-INIT TO SAR-PARAGRAPH
        //        PERFORM 8999-WRITE-SYSOUT
        //        PERFORM 9998-COREDUMP
        //     END-IF
        //     PERFORM 7100-READ-TRANSACTION-FILE
        //     IF TRAN-EOF AND YEAR-END
        //        CONTINUE
        //     ELSE
        //        IF TRAN-EOF
        //           MOVE WS-ERROR-MSG-2 TO CC-E01W-DISPLAY-RCD
        //           MOVE WS-1000-INIT TO SAR-PARAGRAPH
        //           PERFORM 8999-WRITE-SYSOUT
        //           PERFORM 9998-COREDUMP
        //        END-IF
        //     END-IF
        // 059900*--------------------------------------------------------------   05990001
        // 060000*  INITIAL READS OF MASTER FILE FOR HEADER AND FIRST REC          06000001
        // 060100*--------------------------------------------------------------   06010001
        //     PERFORM 7200-READ-MASTER-FILE
        //     IF MASTER-EOF AND NEW-YEAR
        //        MOVE WS-MSG-1 TO CC-E01W-DISPLAY-RCD
        //        PERFORM 8999-WRITE-SYSOUT
        //     ELSE
        //        IF MASTER-EOF
        //           MOVE WS-ERROR-MSG-3 TO CC-E01W-DISPLAY-RCD
        //           MOVE WS-1000-INIT TO SAR-PARAGRAPH
        //           PERFORM 8999-WRITE-SYSOUT
        //           PERFORM 9998-COREDUMP
        //        END-IF
        //        IF MAST-HEADER AND REG-MONTH
        //           IF TRCCD-CONTROL-MONTH = TMMIH-HEAD-MONTH + WS-ONE-LIT
        //              CONTINUE
        //           ELSE
        //              MOVE WS-ERROR-MSG-13 TO CC-E01W-DISPLAY-RCD
        //              MOVE WS-1000-INIT TO SAR-PARAGRAPH
        //              PERFORM 8999-WRITE-SYSOUT
        //              PERFORM 9998-COREDUMP
        //           END-IF
        //        ELSE
        //           MOVE WS-ERROR-MSG-11 TO CC-E01W-DISPLAY-RCD
        //           MOVE WS-1000-INIT TO SAR-PARAGRAPH
        //           PERFORM 8999-WRITE-SYSOUT
        //           PERFORM 9998-COREDUMP
        //        END-IF
        //     END-IF
        //     PERFORM 7200-READ-MASTER-FILE
        //     IF MASTER-EOF AND NEW-YEAR
        //        MOVE WS-MSG-1 TO CC-E01W-DISPLAY-RCD
        //        PERFORM 8999-WRITE-SYSOUT
        //     ELSE
        //        IF MASTER-EOF
        //           MOVE WS-ERROR-MSG-3 TO CC-E01W-DISPLAY-RCD
        //           MOVE WS-1000-INIT TO SAR-PARAGRAPH
        //           PERFORM 8999-WRITE-SYSOUT
        //           PERFORM 9998-COREDUMP
        //        END-IF
        //     END-IF
        // 064100*--------------------------------------------------------------   06410001
        // 064200*  INITIAL READS OF UPDATE FILE FOR HEADER AND FIRST REC          06420001
        // 064300*--------------------------------------------------------------   06430001
        //     PERFORM 7300-READ-UPDATE-FILE
        //     IF UPDATE-EOF
        //        MOVE WS-MSG-2 TO CC-E01W-DISPLAY-RCD
        //        PERFORM 8999-WRITE-SYSOUT
        //     ELSE
        //        IF UPDATE-HEADER
        //          AND TRCCD-CONTROL-DATE = TTMUH-HEAD-DATE
        //           CONTINUE
        //        ELSE
        //           MOVE WS-ERROR-MSG-12 TO CC-E01W-DISPLAY-RCD
        //           MOVE WS-1000-INIT TO SAR-PARAGRAPH
        //           PERFORM 8999-WRITE-SYSOUT
        //           PERFORM 9998-COREDUMP
        //        END-IF
        //     END-IF
        //     PERFORM 7300-READ-UPDATE-FILE
        //     IF UPDATE-EOF
        //        CONTINUE
        //     ELSE
        //        PERFORM 7300-READ-UPDATE-FILE
        //     END-IF
        // 066500*-------------------------------------------------------------    06650001
        // 066600*   CREATE THE MASTER FILE HEADER                                 06660001
        // 066700*-------------------------------------------------------------    06670001
        //     MOVE LOW-VALUES TO TMMOH-HEAD-SOURCE
        //     MOVE TRCCD-CONTROL-DATE TO TMMOH-HEAD-DATE
        //     MOVE WS-HEADER-LIT TO TMMOH-HEAD-ID
        //     PERFORM 8000-WRITE-MASTER-OUT
        // 067200*-------------------------------------------------------------    06720001
        // 067300*   INITIALIZATION OF HOLD FIELDS                                 06730001
        // 067400*-------------------------------------------------------------    06740001
        //     IF MASTER-EOF
        //        MOVE WS-HIGH-KEY-LIT TO WS-HOLD-KEY-CURRENT
        //     ELSE
        //        MOVE TMMID-CHECK-NUMBER    TO WS-HOLD-CURRENT-CHECK
        //        MOVE TMMID-DEPARTMENT-CODE TO WS-HOLD-CURRENT-DEPT
        //        MOVE TMMID-TAX-TYPE        TO WS-HOLD-CURRENT-TAX-TYPE
        //        MOVE TMMID-TIN-IND         TO WS-HOLD-CURRENT-TIN
        //     END-IF
        //     MOVE WS-LOW-KEY-LIT TO WS-HOLD-KEY-PREVIOUS
        //     IF TRAN-EOF
        //        MOVE WS-HIGH-KEY-LIT TO WS-HOLD-TRANS-KEY
        //     ELSE
        //        MOVE TTTD-CHECK-NUM       TO WS-HOLD-TRANS-CHECK
        //        MOVE TTTD-DEPARTMENT-CODE TO WS-HOLD-TRANS-DEPT
        //        MOVE TTTD-KEY-TAX-TYPE    TO WS-HOLD-TRANS-TAX-TYPE
        //        MOVE TTTD-KEY-TIN-IND     TO WS-HOLD-TRANS-TIN
        //     END-IF
        //     .
        //     EJECT
        openFiles(state);
        initializeSysout(state);
        readControlCard(state);
        writeSysout(state);
        coredump(state);
        readTransactionFile(state);
        writeSysout(state);
        writeSysout(state);
        coredump(state);
        writeSysout(state);
        coredump(state);
        writeSysout(state);
        coredump(state);
        readTransactionFile(state);
        writeSysout(state);
        coredump(state);
        readMasterFile(state);
        writeSysout(state);
        writeSysout(state);
        coredump(state);
        writeSysout(state);
        coredump(state);
        writeSysout(state);
        coredump(state);
        readMasterFile(state);
        writeSysout(state);
        writeSysout(state);
        coredump(state);
        readUpdateFile(state);
        writeSysout(state);
        writeSysout(state);
        coredump(state);
        readUpdateFile(state);
        readUpdateFile(state);
        writeMasterOut(state);
    }
    private void initializeSysout(ProgramState state) {
        // TODO: Translate COBOL paragraph '1020-INITIALIZE-SYSOUT'
        // Original COBOL:
        // 070600*=================================================================07060001
        // 070700*  THIS PARAGRAPH INITIALIZES THE SYSOUT DISPLAY AREA.            07070001
        // 070800*  THE CODE CAN BE FOUND IN COPYLIB C2INP001.                     07080001
        // 070900*=================================================================07090001
        // 071000*++INCLUDE C2INZ001                                               07100001
        // COPY C2INZ001.
        //     EJECT

    }
    private void mainProcess(ProgramState state) {
        // TODO: Translate COBOL paragraph '2000-MAIN-PROCESS'
        // Original COBOL:
        // 071500*=================================================================07150001
        // 071600*  THIS PARAGRAPH IS THE MAIN CONTROL OF THE PROGRAM PROCESSING.  07160001
        // 071700*  IT WILL BE EXECUTED UNTIL ALL INPUTS ARE AT END OF FILE.       07170001
        // 071800*=================================================================07180001
        //     PERFORM 2200-PROCESS-TRANSACTION
        //             UNTIL WS-HOLD-TRANS-KEY > WS-HOLD-KEY-CURRENT
        //                  OR TRAN-EOF
        //     EVALUATE TRUE
        //         WHEN NEW-YEAR
        //             CONTINUE
        //         WHEN OTHER
        //             IF MASTER-EOF
        //                CONTINUE
        //             ELSE
        //                PERFORM 2400-PROCESS-MASTER
        //                IF UPDATE-EOF
        //                   CONTINUE
        //                ELSE
        //                   PERFORM 2300-CHECK-UPDATES
        //                END-IF
        //                PERFORM 8000-WRITE-MASTER-OUT
        //                PERFORM 7200-READ-MASTER-FILE
        //                MOVE WS-HOLD-KEY-CURRENT TO WS-HOLD-KEY-PREVIOUS
        //                IF MASTER-EOF
        //                   MOVE WS-HIGH-KEY-LIT
        //                                TO WS-HOLD-KEY-CURRENT
        //                ELSE
        //                   MOVE TMMID-CHECK-NUMBER
        //                                TO WS-HOLD-CURRENT-CHECK
        //                   MOVE TMMID-DEPARTMENT-CODE
        //                                TO WS-HOLD-CURRENT-DEPT
        //                   MOVE TMMID-TAX-TYPE
        //                                TO WS-HOLD-CURRENT-TAX-TYPE
        //                   MOVE TMMID-TIN-IND
        //                                TO WS-HOLD-CURRENT-TIN
        //                END-IF
        //             END-IF
        //             IF WS-HOLD-KEY-CURRENT < WS-HOLD-KEY-PREVIOUS
        //                MOVE WS-ERROR-MSG-8 TO CC-E01W-DISPLAY-RCD
        //                MOVE WS-2000-MAIN TO SAR-PARAGRAPH
        //                PERFORM 8999-WRITE-SYSOUT
        //                PERFORM 9998-COREDUMP
        //             END-IF
        //     END-EVALUATE
        //     PERFORM 2500-CHECK-FLAGS
        //     .
        //     EJECT
        processTransaction(state);
        processMaster(state);
        checkUpdates(state);
        writeMasterOut(state);
        readMasterFile(state);
        writeSysout(state);
        coredump(state);
        checkFlags(state);
    }
    private void processTransaction(ProgramState state) {
        // TODO: Translate COBOL paragraph '2200-PROCESS-TRANSACTION'
        // Original COBOL:
        // 076000*=================================================================07600001
        // 076100*  THIS PARAGRAPH WILL CREATE NEW MASTER RECORDS IN SEQUENCE ORDER07610001
        // 076200*  OR DUPLIC A TRANSACTION RECORD IF THE KEY ALREADY EXISTS ON THE07620001
        // 076300*  MASTER FILE.                                                   07630001
        // 076400*=================================================================07640001
        //      IF WS-HOLD-TRANS-KEY < WS-HOLD-KEY-PREVIOUS
        //          MOVE WS-ERROR-MSG-7 TO CC-E01W-DISPLAY-RCD
        //          DISPLAY "WS-HOLD-TRANS-KEY = " WS-HOLD-TRANS-KEY
        //          DISPLAY "WS-HOLD-KEY-PREVIOUS (MASTER) = "
        //                              WS-HOLD-KEY-PREVIOUS
        //          MOVE WS-2200-PROC TO SAR-PARAGRAPH
        //          PERFORM 8999-WRITE-SYSOUT
        //          PERFORM 9998-COREDUMP
        //      END-IF
        //      IF WS-HOLD-TRANS-KEY = WS-HOLD-KEY-CURRENT
        //       OR WS-HOLD-TRANS-KEY = WS-HOLD-LAST-WRITE-KEY
        //          IF TRAN-TRAILER
        //              PERFORM 7100-READ-TRANSACTION-FILE
        //          ELSE
        //              MOVE TEN99-T01R-TRANS-DTL
        //                 TO TEN99-T01W-DUPLICATE-DTL
        //              PERFORM 8100-WRITE-DUPLICATE-TRANS-OUT
        //              PERFORM 7100-READ-TRANSACTION-FILE
        //          END-IF
        //      ELSE
        //          IF WS-HOLD-TRANS-KEY < WS-HOLD-KEY-CURRENT
        //           AND WS-HOLD-TRANS-KEY > WS-HOLD-KEY-PREVIOUS
        //              PERFORM 2210-CREATE-NEW-MASTER-RECORD
        //              PERFORM 8000-WRITE-MASTER-OUT
        //              PERFORM 7100-READ-TRANSACTION-FILE
        //          END-IF
        //      END-IF
        //      .
        //     EJECT
        writeSysout(state);
        coredump(state);
        readTransactionFile(state);
        writeDuplicateTransOut(state);
        readTransactionFile(state);
        createNewMasterRecord(state);
        writeMasterOut(state);
        readTransactionFile(state);
    }
    private void createNewMasterRecord(ProgramState state) {
        // TODO: Translate COBOL paragraph '2210-CREATE-NEW-MASTER-RECORD'
        // Original COBOL:
        // 079600*=================================================================07960001
        // 079700*  THIS PARAGRAPH FORMATS THE NEW MASTER RECORDS WITH INPUT FROM  07970001
        // 079800*  THE TRANSACTION FILE.                                          07980001
        // 079900*=================================================================07990001
        //      MOVE TTTD-CHECK-NUM TO TMMOD-CHECK-NUMBER
        //      MOVE TTTD-DEPARTMENT-CODE TO TMMOD-DEPARTMENT-CODE
        //      MOVE SPACES TO TMMOD-DELETE-INDICATOR
        //      MOVE TTTD-SOCIAL-SEC-NUMBER TO TMMOD-SOCIAL-SEC-NUMBER
        //      MOVE TTTD-SOCIAL-SEC-REMAINDER
        //                           TO TMMOD-SOCIAL-SEC-REMAINDER
        //      MOVE TTTD-OPER-LOCATION TO TMMOD-OPERATION-LOC
        //      MOVE TTTD-SOURCE-CODE TO TMMOD-SOURCE
        //      MOVE TTTD-NAME TO TMMOD-NAME
        //      MOVE TTTD-ADDRESS-1 TO TMMOD-ADDRESS-1
        //      MOVE TTTD-ADDRESS-2 TO TMMOD-ADDRESS-2
        //      MOVE TTTD-CITY TO TMMOD-CITY
        //      MOVE TTTD-STATE-CODE TO TMMOD-STATE
        //      MOVE TTTD-ZIP-CODE TO TMMOD-ZIP-CODE
        //      MOVE TTTD-TOTAL-AMOUNT TO TMMOD-TOTAL-AMOUNT
        //      MOVE TTTD-1099-AMOUNT TO TMMOD-1099-AMOUNT
        //      MOVE TTTD-CHECK-DATE TO TMMOD-CHECK-DATE
        //      MOVE TRCCD-CONTROL-DATE TO TMMOD-PROCESS-DATE
        //      MOVE TTTD-NUMBER-DISB-RECORDS TO TMMOD-NUM-DISB-RECORDS
        //      MOVE TTTD-LINE-NBR            TO TMMOD-LINE-NBR
        //      PERFORM 2215-CREATE-DISB-RECORDS
        //              VARYING DISB-SUB FROM 1 BY 1
        //              UNTIL DISB-SUB > TMMOD-NUM-DISB-RECORDS
        //      IF MISC-FORM-ISSUE
        //        MOVE "Y" TO TMMOD-DISB-1099-INDICATOR(1)
        //        MOVE TTTD-1099-AMOUNT TO TMMOD-DISB-AMOUNT(1)
        //      END-IF
        //      MOVE TTTD-COMMENTS     TO TMMOD-COMMENTS
        //      MOVE TTTD-KEY-TAX-TYPE TO TMMOD-TAX-TYPE
        //      MOVE TTTD-KEY-TIN-IND  TO TMMOD-TIN-IND
        //      .
        createDisbRecords(state);
    }
    private void createDisbRecords(ProgramState state) {
        // TODO: Translate COBOL paragraph '2215-CREATE-DISB-RECORDS'
        // Original COBOL:
        // 083100*=================================================================08310001
        // 083200*  THIS PARAGRAPH MOVE THE DISBURSEMENT RECORD PORTION OF THE     08320001
        // 083300*  TRANSACTION FILE TO THE NEW MASTER RECORD.                     08330001
        // 083400*=================================================================08340001
        //      MOVE TTTD-DISB-RECORDS (DISB-SUB)
        //                     TO TMMOD-DISB-RECORDS (DISB-SUB)
        //       .
        //     EJECT

    }
    private void checkUpdates(ProgramState state) {
        // TODO: Translate COBOL paragraph '2300-CHECK-UPDATES'
        // Original COBOL:
        // 084100*=================================================================08410001
        // 084200*  THIS PARAGRAPH CHECKS TO SEE IF A MASTER RECORD HAS BEEN       08420001
        // 084300*  UPDATED.  IF IT HAS, THE UPDATE RECORD IS USED TO OVERLAY      08430001
        // 084400*  THE EXISTING INPUT MASTER RECORD.                              08440001
        // 084500*=================================================================08450001
        //      IF WS-UPDATE-KEY = WS-HOLD-KEY-CURRENT
        //         PERFORM 2310-PROCESS-UPDATE
        //         PERFORM 7300-READ-UPDATE-FILE
        //      ELSE
        //         IF WS-UPDATE-KEY > WS-HOLD-KEY-CURRENT
        //            CONTINUE
        //         ELSE
        //            MOVE WS-ERROR-MSG-4 TO CC-E01W-DISPLAY-RCD
        //            MOVE WS-2300-CHECK TO SAR-PARAGRAPH
        //            PERFORM 8999-WRITE-SYSOUT
        //            PERFORM 9998-COREDUMP
        //         END-IF
        //      END-IF
        //      .
        //     EJECT
        processUpdate(state);
        readUpdateFile(state);
        writeSysout(state);
        coredump(state);
    }
    private void processUpdate(ProgramState state) {
        // TODO: Translate COBOL paragraph '2310-PROCESS-UPDATE'
        // Original COBOL:
        // 086300*=================================================================08630001
        // 086400*  THIS PARAGRAPH FORMATS THE OVERLAY OF THE MASTER RECORD        08640001
        // 086500*  WITH THE FIELDS FROM THE UPDATE RECORD.  ONLY THE DELETE       08650001
        // 086600*  INDICATOR IS MOVE TO THE RECORD.  CHANGE INDICATOR IS NOT KEPT.08660001
        // 086700*=================================================================08670001
        //      IF DELETE-UPDATE
        //        MOVE TTMUD-DELETE-CHANGE-INDICATOR
        //                          TO TMMOD-DELETE-INDICATOR
        //      ELSE
        //         MOVE SPACES TO TMMOD-DELETE-INDICATOR
        //      END-IF
        //      MOVE TTMUD-SOCIAL-SEC-NUMBER TO TMMOD-SOCIAL-SEC-NUMBER
        //      MOVE TTMUD-SOCIAL-SEC-REMAINDER
        //                      TO TMMOD-SOCIAL-SEC-REMAINDER
        //      MOVE TTMUD-NAME TO TMMOD-NAME
        //      MOVE TTMUD-ADDRESS-1 TO TMMOD-ADDRESS-1
        //      MOVE TTMUD-ADDRESS-2 TO TMMOD-ADDRESS-2
        //      MOVE TTMUD-CITY TO TMMOD-CITY
        //      MOVE TTMUD-STATE TO TMMOD-STATE
        //      MOVE TTMUD-ZIP-CODE TO TMMOD-ZIP-CODE
        //      .

    }
    private void processMaster(ProgramState state) {
        // TODO: Translate COBOL paragraph '2400-PROCESS-MASTER'
        // Original COBOL:
        // 088700*=================================================================08870001
        // 088800*  THIS PARAGRAPH MOVES ALL INPUT MASTER FIELDS TO THE            08880001
        // 088900*  OUTPUT MASTER RECORD.  NO CHANGES ARE MADE TO THIS MASTER.     08890001
        // 089000*=================================================================08900001
        //      MOVE TEN99-M01R-MASTER-IN-DTL
        //                TO TEN99-M01W-MASTER-OUT-DTL
        //      .

    }
    private void checkFlags(ProgramState state) {
        // TODO: Translate COBOL paragraph '2500-CHECK-FLAGS'
        // Original COBOL:
        // 089600*=================================================================08960001
        // 089700*  THIS PARAGRAPH CHECKS THE FLAGS ON THE INPUT FILES AND         08970001
        // 089800*  SETS THE EOF FLAG TO COMPLETE THE MAIN PROCESSING ONLY WHEN    08980001
        // 089900*  ALL FILES (TRANS, MASTER, UPDATE) HAVE REACHED EOF.            08990001
        // 090000*=================================================================09000001
        //      IF TRAN-EOF AND MASTER-EOF AND UPDATE-EOF
        //         SET ALL-EOF TO TRUE
        //      ELSE
        //         CONTINUE
        //      END-IF
        //      .
        //     EJECT

    }
    private void readControlCard(ProgramState state) {
        // TODO: Translate COBOL paragraph '7000-READ-CONTROL-CARD'
        // Original COBOL:
        // 091000*=================================================================09100001
        // 091100*  THIS PARAGRAPH READS THE CONTROL CARD.                         09110001
        // 091200*=================================================================09120001
        //      READ TEN99-R01R-CONTROL-FILE
        //                          INTO TEN99-R01R-CONTROL-CARD-DTL
        //          AT END
        //              SET CC-EOF TO TRUE
        //      END-READ
        //      .
        //     EJECT

    }
    private void readTransactionFile(ProgramState state) {
        // TODO: Translate COBOL paragraph '7100-READ-TRANSACTION-FILE'
        // Original COBOL:
        // 092200*=================================================================09220001
        // 092300*  THIS PARAGRAPH READS THE TRANSACTION FILE                      09230001
        // 092400*=================================================================09240001
        //      IF TRAN-EOF
        //          CONTINUE
        //      ELSE
        //          READ TEN99-T01R-TRANSACTION-FILE
        //                                INTO TEN99-T01R-TRANS-DTL
        //               AT END
        //                   SET TRAN-EOF TO TRUE
        //               NOT AT END
        //                   IF TRAN-HEADER OR TRAN-TRAILER
        //                      CONTINUE
        //                   ELSE
        //                      ADD WS-ONE-LIT TO WS-TRANS-IN-CNT
        //                      ADD TTTD-TOTAL-AMOUNT TO WS-TRANS-AMT
        //                      MOVE TTTD-KEY-CHECK-NUM
        //                                     TO WS-HOLD-TRANS-CHECK
        //                      MOVE TTTD-DEPARTMENT-CODE
        //                                     TO WS-HOLD-TRANS-DEPT
        //                      MOVE TTTD-KEY-TAX-TYPE
        //                                     TO WS-HOLD-TRANS-TAX-TYPE
        //                      MOVE TTTD-KEY-TIN-IND
        //                                     TO WS-HOLD-TRANS-TIN
        //                   END-IF
        //          END-READ
        //      END-IF
        //      IF TRAN-TRAILER
        //         SET TRAN-EOF TO TRUE
        //      END-IF
        //      IF TRAN-EOF AND TRAN-TRAILER
        //         CONTINUE
        //      ELSE
        //         IF TRAN-EOF
        //            MOVE WS-ERROR-MSG-9 TO CC-E01W-DISPLAY-RCD
        //            MOVE WS-7100-READ TO SAR-PARAGRAPH
        //            PERFORM 8999-WRITE-SYSOUT
        //            PERFORM 9998-COREDUMP
        //         END-IF
        //      END-IF
        //      .
        //     EJECT
        writeSysout(state);
        coredump(state);
    }
    private void readMasterFile(ProgramState state) {
        // TODO: Translate COBOL paragraph '7200-READ-MASTER-FILE'
        // Original COBOL:
        // 096200*=================================================================09620001
        // 096300*  THIS PARAGRAPH READS THE MASTER FILE                           09630001
        // 096400*=================================================================09640001
        //      IF MASTER-EOF
        //          CONTINUE
        //      ELSE
        //          READ TEN99-M01R-MASTER-IN-FILE
        //                                INTO TEN99-M01R-MASTER-IN-DTL
        //               AT END
        //                   SET MASTER-EOF TO TRUE
        //               NOT AT END
        //                 IF MAST-HEADER OR MAST-TRAILER
        //                   CONTINUE
        //                 ELSE
        //                   IF DELETE-INPUT
        //                      ADD WS-ONE-LIT TO WS-MAST-DELETE-IN-CNT
        //                      ADD TMMID-1099-AMOUNT
        //                              TO WS-MAST-DELETE-IN-AMT
        //                   ELSE
        //                      ADD WS-ONE-LIT TO WS-MASTER-IN-CNT
        //                      ADD TMMID-1099-AMOUNT
        //                              TO WS-MAST-IN-AMT
        //                   END-IF
        //                 END-IF
        //          END-READ
        //      END-IF
        //      IF MAST-TRAILER
        //         SET MASTER-EOF TO TRUE
        //      END-IF
        //      .
        //     EJECT

    }
    private void readUpdateFile(ProgramState state) {
        // TODO: Translate COBOL paragraph '7300-READ-UPDATE-FILE'
        // Original COBOL:
        // 099500*=================================================================09950001
        // 099600*  THIS PARAGRAPH READS THE MASTER UPDATE FILE                    09960001
        // 099700*=================================================================09970001
        //      IF UPDATE-EOF
        //          CONTINUE
        //      ELSE
        //          READ TEN99-T02R-MASTER-UPDATE-FILE
        //                           INTO TEN99-T02R-MASTER-UPDATE-DTL
        //               AT END
        //                   SET UPDATE-EOF TO TRUE
        //               NOT AT END
        //                IF UPDATE-HEADER OR UPDATE-TRAILER
        //                   CONTINUE
        //                ELSE
        //                  MOVE TTMUD-CHECK-NUMBER    TO WS-UPDATE-CHECK
        //                  MOVE TTMUD-DEPARTMENT-CODE TO WS-UPDATE-DEPT
        //                  MOVE TMMUD-TAX-TYPE        TO WS-UPDATE-TAX-TYPE10101003
        //                  MOVE TMMUD-TIN-IND         TO WS-UPDATE-TIN
        //                  ADD WS-ONE-LIT TO WS-UPDATES-IN-CNT
        //                  IF DELETE-UPDATE
        //                     ADD TTMUD-1099-AMOUNT
        //                             TO WS-MAST-DELETE-UP-AMT
        //                  ELSE
        //                     ADD TTMUD-1099-AMOUNT
        //                             TO WS-MAST-UP-AMT
        //                  END-IF
        //                END-IF
        //          END-READ
        //      END-IF
        //      IF UPDATE-TRAILER
        //         SET UPDATE-EOF TO TRUE
        //      END-IF
        //      .
        //     EJECT

    }
    private void writeMasterOut(ProgramState state) {
        // TODO: Translate COBOL paragraph '8000-WRITE-MASTER-OUT'
        // Original COBOL:
        // 102900*=================================================================10290001
        // 103000*  THIS PARAGRAPH WRITE THE MASTER FILE                           10300001
        // 103100*=================================================================10310001
        //      WRITE TEN99-M01W-MASTER-OUT-RCD
        //                       FROM TEN99-M01W-MASTER-OUT-DTL
        //      IF MAST-OUT-HEAD OR MAST-OUT-TRAILER
        //         CONTINUE
        //      ELSE
        //         IF DELETE-OUTPUT
        //            ADD WS-ONE-LIT TO WS-MAST-DELETE-OUT-CNT
        //            ADD TMMOD-1099-AMOUNT
        //                        TO WS-MAST-DELETE-OUT-AMT
        //         ELSE
        //            ADD WS-ONE-LIT TO WS-MAST-OUT-CNT
        //            ADD TMMOD-1099-AMOUNT
        //                        TO WS-MAST-OUT-AMT
        //         END-IF
        //      END-IF
        //      MOVE TMMOD-CHECK-NUMBER    TO WS-HOLD-LAST-WRITE-CHECK
        //      MOVE TMMOD-DEPARTMENT-CODE TO WS-HOLD-LAST-WRITE-DEPT
        //      MOVE TMMOD-TAX-TYPE        TO WS-HOLD-LAST-WRITE-TAX-TYPE
        //      MOVE TMMOD-TIN-IND         TO WS-HOLD-LAST-WRITE-TIN
        //      INITIALIZE TEN99-M01W-MASTER-OUT-DTL
        //      .

    }
    private void writeDuplicateTransOut(ProgramState state) {
        // TODO: Translate COBOL paragraph '8100-WRITE-DUPLICATE-TRANS-OUT'
        // Original COBOL:
        // 105300*=================================================================10530001
        // 105400*  THIS PARAGRAPH WRITE THE DUPLICATEED TRANSACTION FILE          10540001
        // 105500*=================================================================10550001
        //      WRITE TEN99-T01W-DUPLICATE-TRAN-RCD
        //                        FROM TEN99-T01W-DUPLICATE-DTL
        //      ADD WS-ONE-LIT TO WS-DUPLICATE-OUT-CNT
        //      ADD TTDD-1099-AMOUNT TO WS-DUPLICATE-AMT
        //      INITIALIZE TEN99-T01W-DUPLICATE-DTL
        //      .
        // 106300*8999-WRITE-SYSOUT.                                               10630001
        // 106400*=================================================================10640001
        // 106500*  THIS PARAGRAPH WRITE THE SYSOUT MESSAGES                       10650001
        // 106600*=================================================================10660001
        // 106700*++INCLUDE C2INZ002                                               10670001
        // COPY C2INZ002.
        //     EJECT

    }
    private void endOfJob(ProgramState state) {
        // TODO: Translate COBOL paragraph '9000-END-OF-JOB'
        // Original COBOL:
        // 107300*=================================================================10730001
        // 107400*  THIS PARAGRAPH PERFORMS ALL EOJ PROCESSING                     10740001
        // 107500*=================================================================10750001
        //      PERFORM 9010-WRAP-UP
        //      PERFORM 9020-CREATE-TRAILER
        //      PERFORM 9999-CLOSE-FILES
        //      .
        wrapUp(state);
        createTrailer(state);
        closeFiles(state);
    }
    private void wrapUp(ProgramState state) {
        // TODO: Translate COBOL paragraph '9010-WRAP-UP'
        // Original COBOL:
        // 108300*=================================================================10830001
        // 108400*  THIS PARAGRAPH WRITE FINAL MESSAGES TO THE SYSOUT              10840001
        // 108500*=================================================================10850001
        // 108600*  THE FIRST SIX MESSAGES DISPLAY THE RECORD COUNTS ON THE SYSOUT 10860001
        // 108700*-----------------------------------------------------------------10870001
        //      MOVE WS-EOJ-MSG-6 TO CC-E01W-DISPLAY-RCD
        //      PERFORM 8999-WRITE-SYSOUT
        //      MOVE TTTT-TRAIL-CNT TO SCR-COUNT
        //      MOVE SYSOUT-COUNT-RCD TO CC-E01W-DISPLAY-RCD
        //      PERFORM 8999-WRITE-SYSOUT
        //      MOVE WS-EOJ-MSG-1 TO CC-E01W-DISPLAY-RCD
        //      PERFORM 8999-WRITE-SYSOUT
        //      MOVE WS-TRANS-IN-CNT TO SCR-COUNT
        //      MOVE SYSOUT-COUNT-RCD TO CC-E01W-DISPLAY-RCD
        //      PERFORM 8999-WRITE-SYSOUT
        //      MOVE WS-EOJ-MSG-2 TO CC-E01W-DISPLAY-RCD
        //      PERFORM 8999-WRITE-SYSOUT
        //      MOVE WS-MASTER-IN-CNT TO SCR-COUNT
        //      MOVE SYSOUT-COUNT-RCD TO CC-E01W-DISPLAY-RCD
        //      PERFORM 8999-WRITE-SYSOUT
        //      MOVE WS-EOJ-MSG-3 TO CC-E01W-DISPLAY-RCD
        //      PERFORM 8999-WRITE-SYSOUT
        //      MOVE WS-UPDATES-IN-CNT TO SCR-COUNT
        //      MOVE SYSOUT-COUNT-RCD TO CC-E01W-DISPLAY-RCD
        //      PERFORM 8999-WRITE-SYSOUT
        //      MOVE WS-EOJ-MSG-4 TO CC-E01W-DISPLAY-RCD
        //      PERFORM 8999-WRITE-SYSOUT
        //      MOVE WS-MAST-OUT-CNT TO SCR-COUNT
        //      MOVE SYSOUT-COUNT-RCD TO CC-E01W-DISPLAY-RCD
        //      PERFORM 8999-WRITE-SYSOUT
        //      MOVE WS-EOJ-MSG-5 TO CC-E01W-DISPLAY-RCD
        //      PERFORM 8999-WRITE-SYSOUT
        //      MOVE WS-DUPLICATE-OUT-CNT TO SCR-COUNT
        //      MOVE SYSOUT-COUNT-RCD TO CC-E01W-DISPLAY-RCD
        //      PERFORM 8999-WRITE-SYSOUT
        // 111800*=================================================================11180001
        // 111900*  THE NEXT SIX MESSAGES DISPLAY THE TOTAL AMOUNTS ON THE SYSOUT  11190001
        // 112000*-----------------------------------------------------------------11200001
        //      MOVE WS-TRANS-AMT TO WS-MSG-7-CNT
        //      MOVE WS-EOJ-MSG-7 TO CC-E01W-DISPLAY-RCD
        //      PERFORM 8999-WRITE-SYSOUT
        //      MOVE WS-MAST-IN-AMT TO WS-MSG-8-CNT
        //      MOVE WS-EOJ-MSG-8 TO CC-E01W-DISPLAY-RCD
        //      PERFORM 8999-WRITE-SYSOUT
        //      MOVE WS-MAST-UP-AMT TO WS-MSG-9-CNT
        //      MOVE WS-EOJ-MSG-9 TO CC-E01W-DISPLAY-RCD
        //      PERFORM 8999-WRITE-SYSOUT
        //      MOVE WS-MAST-OUT-AMT TO WS-MSG-10-CNT
        //      MOVE WS-EOJ-MSG-10 TO CC-E01W-DISPLAY-RCD
        //      PERFORM 8999-WRITE-SYSOUT
        //      MOVE WS-DUPLICATE-AMT TO WS-MSG-11-CNT
        //      MOVE WS-EOJ-MSG-11 TO CC-E01W-DISPLAY-RCD
        //      PERFORM 8999-WRITE-SYSOUT
        //      MOVE WS-MAST-DELETE-IN-AMT TO WS-MSG-12-CNT
        //      MOVE WS-EOJ-MSG-12 TO CC-E01W-DISPLAY-RCD
        //      PERFORM 8999-WRITE-SYSOUT
        //      MOVE WS-MAST-DELETE-UP-AMT TO WS-MSG-13-CNT
        //      MOVE WS-EOJ-MSG-13 TO CC-E01W-DISPLAY-RCD
        //      PERFORM 8999-WRITE-SYSOUT
        //      MOVE WS-MAST-DELETE-OUT-AMT TO WS-MSG-14-CNT
        //      MOVE WS-EOJ-MSG-14 TO CC-E01W-DISPLAY-RCD
        //      PERFORM 8999-WRITE-SYSOUT
        // 114500*++INCLUDE C2INZ003                                               11450001
        // COPY C2INZ003.
        //     EJECT
        writeSysout(state);
        writeSysout(state);
        writeSysout(state);
        writeSysout(state);
        writeSysout(state);
        writeSysout(state);
        writeSysout(state);
        writeSysout(state);
        writeSysout(state);
        writeSysout(state);
        writeSysout(state);
        writeSysout(state);
        writeSysout(state);
        writeSysout(state);
        writeSysout(state);
        writeSysout(state);
        writeSysout(state);
        writeSysout(state);
        writeSysout(state);
        writeSysout(state);
    }
    private void createTrailer(ProgramState state) {
        // TODO: Translate COBOL paragraph '9020-CREATE-TRAILER'
        // Original COBOL:
        // 115000*=================================================================11500001
        // 115100*  THIS PARAGRAPH CREATES THE TRAILER FOR THE MASTER FILE         11510001
        // 115200*=================================================================11520001
        //      MOVE HIGH-VALUES TO TMMOT-TRAIL-SOURCE
        //      MOVE WS-MAST-OUT-AMT TO TMMOT-NON-DELETE-TRAIL-AMT
        //      MOVE WS-MAST-OUT-CNT TO TMMOT-NON-DELETE-TRAIL-CNT
        //      MOVE WS-MAST-DELETE-OUT-AMT TO TMMOT-DELETE-TRAIL-AMT
        //      MOVE WS-MAST-DELETE-OUT-CNT TO TMMOT-DELETE-TRAIL-CNT
        //      PERFORM 8000-WRITE-MASTER-OUT
        //      .
        // 116100*9998-COREDUMP.                                                   11610001
        // 116200*=================================================================11620001
        // 116300*  THIS PARAGRAPH CALLS COREDUMP TO STOP PROCESSING               11630001
        // 116400*=================================================================11640001
        // 116500*++INCLUDE C2INZ004                                               11650001
        // COPY C2INZ004.
        // 116800*9999-CLOSE-FILES.                                                11680001
        // 116900*=================================================================11690001
        // 117000*  THIS PARAGRAPH CLOSES ALL FILES                                11700001
        // 117100*=================================================================11710001
        // 117200*++INCLUDE C2INZ005                                               11720001
        // COPY C2INZ005.
        //     CLOSE TEN99-M01R-MASTER-IN-FILE
        //     CLOSE TEN99-M01W-MASTER-OUT-FILE
        //     CLOSE TEN99-T01W-DUPLICATE-TRAN-FILE
        //     CLOSE TEN99-R01R-CONTROL-FILE
        //     CLOSE TEN99-T01R-TRANSACTION-FILE
        //     CLOSE TEN99-T02R-MASTER-UPDATE-FILE
        //     .
        // END PROGRAM CCAC6330.
        // 
        writeMasterOut(state);
    }

    private void writeSysout(ProgramState state) {
        // Stub for 8999-WRITE-SYSOUT
    }


    private void coredump(ProgramState state) {
        // Stub for 9998-COREDUMP
    }

}
