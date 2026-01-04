package com.fordcredit.misc1099.batch.program;

import org.springframework.batch.core.StepContribution;
import org.springframework.batch.core.scope.context.ChunkContext;
import org.springframework.batch.core.step.tasklet.Tasklet;
import org.springframework.batch.repeat.RepeatStatus;
import java.io.*;
import java.nio.file.*;
import java.math.BigDecimal;

public class CCAC6330Tasklet implements Tasklet {

    private final String basePath;

    public CCAC6330Tasklet(String basePath) {
        this.basePath = basePath;
    }

    // =========== PROGRAM STATE ===========
    static class ProgramState {
        // File I/O
        BufferedReader ten99T01rTransactionFileReader;
        String ten99T01rTransactionFileLine;
        boolean ten99T01rTransactionFileEof = false;
        BufferedWriter ten99T01rTransactionFileWriter;
        BufferedReader ten99M01rMasterInFileReader;
        String ten99M01rMasterInFileLine;
        boolean ten99M01rMasterInFileEof = false;
        BufferedWriter ten99M01rMasterInFileWriter;
        BufferedReader ten99T02rMasterUpdateFileReader;
        String ten99T02rMasterUpdateFileLine;
        boolean ten99T02rMasterUpdateFileEof = false;
        BufferedWriter ten99T02rMasterUpdateFileWriter;
        BufferedReader ten99R01rControlFileReader;
        String ten99R01rControlFileLine;
        boolean ten99R01rControlFileEof = false;
        BufferedWriter ten99R01rControlFileWriter;
        BufferedReader ten99T01wDuplicateTranFileReader;
        String ten99T01wDuplicateTranFileLine;
        boolean ten99T01wDuplicateTranFileEof = false;
        BufferedWriter ten99T01wDuplicateTranFileWriter;
        BufferedReader ten99M01wMasterOutFileReader;
        String ten99M01wMasterOutFileLine;
        boolean ten99M01wMasterOutFileEof = false;
        BufferedWriter ten99M01wMasterOutFileWriter;

        // WORKING-STORAGE variables
        String panValet = "002CCAC6330";
        String pups2000 = "";
        String ten99T01rTransDtl = "";
        String tttdKeyCheckNum = "";
        // 88 tranHeader: tttdKeyCheckNum.equals("LOW-VALUES")
        // 88 tranTrailer: tttdKeyCheckNum.equals("HIGH-VALUES")
        String tttdKeyTaxType = "";
        String tttdKeyTinInd = "";
        String tttdSourceCode = "";
        // 88 miscFormIssue: tttdSourceCode.equals("MISC")
        String tttdTransIndicator = "";
        String tttdDepartmentCode = "";
        String tttdName = "";
        String tttdAddress1 = "";
        String tttdAddress2 = "";
        String tttdCity = "";
        String tttdStateCode = "";
        int tttdZipCode = 0;
        int tttdSocialSecNumber = 0;
        String tttdSocialSecRemainder = "";
        String tttdOperLocation = "";
        String tttdCheckNum = "";
        String tttdCheckDate = "";
        java.math.BigDecimal tttdTotalAmount = java.math.BigDecimal.ZERO;
        java.math.BigDecimal tttd1099Amount = java.math.BigDecimal.ZERO;
        String tttdComments = "";
        String tttdCommentRest = "";
        int tttdNumberDisbRecords = 0;
        String[] tttdDisbRecords = new String[5];
        String tttdDisbEntryCode = "";
        String tttdDisb1099Indicator = "";
        java.math.BigDecimal tttdDisbAmount = java.math.BigDecimal.ZERO;
        String tttdLineNbr = "";
        String ten99T01rTransHdr = "";
        String ttthHeadSource = "";
        String ttthHeadId = "";
        String ttthHeadDate = "";
        int ttthHeadCentury = 0;
        int ttthHeadYear = 0;
        int ttthHeadMonth = 0;
        String ten99T01rTransTlr = "";
        String ttttTrailSource = "";
        int ttttTrailCnt = 0;
        java.math.BigDecimal ttttTrailAmt = java.math.BigDecimal.ZERO;
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
        // 88 deleteInput: tmmidDeleteIndicator.equals("D")
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
        String tmmidLineNbr = "";
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
        String ten99T02rMasterUpdateDtl = "";
        String ttmudCheckNumber = "";
        String ttmudDepartmentCode = "";
        int ttmudSocialSecNumber = 0;
        String ttmudSocialSecRemainder = "";
        String ttmudOperationLoc = "";
        String ttmudSource = "";
        String ttmudName = "";
        String ttmudAddress1 = "";
        String ttmudAddress2 = "";
        String ttmudCity = "";
        String ttmudState = "";
        int ttmudZipCode = 0;
        String ttmudDeleteChangeIndicator = "";
        // 88 changeUpdate: ttmudDeleteChangeIndicator.equals("C")
        // 88 deleteUpdate: ttmudDeleteChangeIndicator.equals("D")
        java.math.BigDecimal ttmudTotalAmount = java.math.BigDecimal.ZERO;
        java.math.BigDecimal ttmud1099Amount = java.math.BigDecimal.ZERO;
        String ttmudCheckDate = "";
        String ttmudProcessDate = "";
        int ttmudNumDisbRecords = 0;
        String[] ttmudDisbRecords = new String[5];
        String ttmudDisbEntryCode = "";
        String ttmudDisb1099Indicator = "";
        java.math.BigDecimal ttmudDisbAmount = java.math.BigDecimal.ZERO;
        String ttmudComments = "";
        String ttmudLineNbr = "";
        String tmmudTaxType = "";
        String tmmudTinInd = "";
        String ten99T02rMastUpdtHdr = "";
        String ttmuhHeadSource = "";
        // 88 updateHeader: ttmuhHeadSource.equals("LOW-VALUES")
        String ttmuhHeadId = "";
        String ttmuhHeadDate = "";
        int ttmuhHeadCentury = 0;
        int ttmuhHeadYear = 0;
        int ttmuhHeadMonth = 0;
        String ten99T02rMastUpdtTlr = "";
        String ttmutTrailSource = "";
        // 88 updateTrailer: ttmutTrailSource.equals("HIGH-VALUES")
        int ttmutNonDeleteTrailCnt = 0;
        java.math.BigDecimal ttmutNonDeleteTrailAmt = java.math.BigDecimal.ZERO;
        int ttmutDeleteTrailCnt = 0;
        java.math.BigDecimal ttmutDeleteTrailAmt = java.math.BigDecimal.ZERO;
        String ten99R01rControlCardDtl = "";
        String trccdControlDate = "";
        int trccdControlCentury = 0;
        int trccdControlYear = 0;
        int trccdControlMonth = 0;
        // 88 yearEnd: trccdControlMonth.equals("13")
        // 88 newYear: trccdControlMonth.equals("01")
        // 88 regMonth: trccdControlMonth.equals("02")
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
        // 88 deleteOutput: tmmodDeleteIndicator.equals("D")
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
        String tmmodLineNbr = "";
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
        int tmmotNonDeleteTrailCnt = 0;
        java.math.BigDecimal tmmotNonDeleteTrailAmt = java.math.BigDecimal.ZERO;
        int tmmotDeleteTrailCnt = 0;
        java.math.BigDecimal tmmotDeleteTrailAmt = java.math.BigDecimal.ZERO;
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
        int ttddZipCode = 0;
        int ttddSocialSecNumber = 0;
        String ttddSocialSecRemainder = "";
        String ttddOperLocation = "";
        String ttddCheckNum = "";
        String ttddCheckDate = "";
        java.math.BigDecimal ttddTotalAmount = java.math.BigDecimal.ZERO;
        java.math.BigDecimal ttdd1099Amount = java.math.BigDecimal.ZERO;
        String ttddComments = "";
        String ttddCommentRest = "";
        int ttddNumberDisbRecords = 0;
        String[] ttddDisbributionRecords = new String[5];
        String ttddDisbEntryCode = "";
        String ttddDisb1099Indicator = "";
        java.math.BigDecimal ttddDisbAmount = java.math.BigDecimal.ZERO;
        String ttddLineNbr = "";
        String ttddTaxType = "";
        String ttddTinInd = "";
        String wsCounters = "";
        int wsMasterInCnt = 0;
        int wsMastDeleteInCnt = 0;
        int wsTransInCnt = 0;
        int wsUpdatesInCnt = 0;
        int wsMastOutCnt = 0;
        int wsMastDeleteOutCnt = 0;
        int wsDuplicateOutCnt = 0;
        String wsAccumulators = "";
        java.math.BigDecimal wsTransAmt = java.math.BigDecimal.ZERO;
        java.math.BigDecimal wsDuplicateAmt = java.math.BigDecimal.ZERO;
        java.math.BigDecimal wsMastDeleteInAmt = java.math.BigDecimal.ZERO;
        java.math.BigDecimal wsMastInAmt = java.math.BigDecimal.ZERO;
        java.math.BigDecimal wsMastDeleteUpAmt = java.math.BigDecimal.ZERO;
        java.math.BigDecimal wsMastUpAmt = java.math.BigDecimal.ZERO;
        java.math.BigDecimal wsMastDeleteOutAmt = java.math.BigDecimal.ZERO;
        java.math.BigDecimal wsMastOutAmt = java.math.BigDecimal.ZERO;
        String wsFlags = "";
        String wsControlCardFlg = "N";
        // 88 ccEof: wsControlCardFlg.equals("Y")
        String wsTransactionFlg = "N";
        // 88 tranEof: wsTransactionFlg.equals("Y")
        String wsMasterFlg = "N";
        // 88 masterEof: wsMasterFlg.equals("Y")
        String wsUpdateFlg = "N";
        // 88 updateEof: wsUpdateFlg.equals("Y")
        String wsAllInputsFlg = "N";
        // 88 allEof: wsAllInputsFlg.equals("Y")
        int disbSub = 0;
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
        java.math.BigDecimal wsMsg7Cnt = java.math.BigDecimal.ZERO;
        String wsEojMsg8 = "";
        java.math.BigDecimal wsMsg8Cnt = java.math.BigDecimal.ZERO;
        String wsEojMsg9 = "";
        java.math.BigDecimal wsMsg9Cnt = java.math.BigDecimal.ZERO;
        String wsEojMsg10 = "";
        java.math.BigDecimal wsMsg10Cnt = java.math.BigDecimal.ZERO;
        String wsEojMsg11 = "";
        java.math.BigDecimal wsMsg11Cnt = java.math.BigDecimal.ZERO;
        String wsEojMsg12 = "";
        java.math.BigDecimal wsMsg12Cnt = java.math.BigDecimal.ZERO;
        String wsEojMsg13 = "";
        java.math.BigDecimal wsMsg13Cnt = java.math.BigDecimal.ZERO;
        String wsEojMsg14 = "";
        java.math.BigDecimal wsMsg14Cnt = java.math.BigDecimal.ZERO;
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
        int wsOneLit = 0;
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
        String ten99T01rTransactionRcd = "";
        String ten99M01rMasterInRcd = "";
        String ten99T02rMasterUpdateRcd = "";
        String ten99R01rControlRcd = "";
        String ten99T01wDuplicateTranRcd = "";
        String ten99M01wMasterOutRcd = "";

        // Auto-added fields
        String allEof = "";
        String atEnd = "";
        String atEndTen99m01rmasterinfile = "";
        String ccE01wDisplayRcd = "";
        String ccEof = "";
        String deleteInput = "";
        String deleteOutput = "";
        String deleteUpdate = "";
        String mastHeader = "";
        String mastOutHead = "";
        String mastOutTrailer = "";
        String mastTrailer = "";
        String masterEof = "";
        String newYear = "";
        String regMonth = "";
        String sarParagraph = "";
        String scrCount = "";
        String sysoutCountRcd = "";
        String ten99R01rControlFile = "";
        String tranEof = "";
        String tranHeader = "";
        String tranTrailer = "";
        String updateEof = "";
        String updateHeader = "";
        String updateTrailer = "";
        String yearEnd = "";
    }

    @Override
    public RepeatStatus execute(StepContribution contribution, ChunkContext chunkContext) throws Exception {
        ProgramState state = new ProgramState();
        mainline(state);
        return RepeatStatus.FINISHED;
    }

    // =========== FILE I/O ===========
    private void readTen99t01rtransactionfile(ProgramState state) {
        try {
            if (state.ten99T01rTransactionFileReader == null) { state.ten99T01rTransactionFileEof = true; return; }
            state.ten99T01rTransactionFileLine = state.ten99T01rTransactionFileReader.readLine();
            if (state.ten99T01rTransactionFileLine == null) state.ten99T01rTransactionFileEof = true;
        } catch (Exception e) { throw new RuntimeException("Read error", e); }
    }

    private void writeTen99t01rtransactionfile(ProgramState state, String line) {
        try {
            if (state.ten99T01rTransactionFileWriter != null && line != null) {
                state.ten99T01rTransactionFileWriter.write(line);
                state.ten99T01rTransactionFileWriter.newLine();
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

    private void readTen99t02rmasterupdatefile(ProgramState state) {
        try {
            if (state.ten99T02rMasterUpdateFileReader == null) { state.ten99T02rMasterUpdateFileEof = true; return; }
            state.ten99T02rMasterUpdateFileLine = state.ten99T02rMasterUpdateFileReader.readLine();
            if (state.ten99T02rMasterUpdateFileLine == null) state.ten99T02rMasterUpdateFileEof = true;
        } catch (Exception e) { throw new RuntimeException("Read error", e); }
    }

    private void writeTen99t02rmasterupdatefile(ProgramState state, String line) {
        try {
            if (state.ten99T02rMasterUpdateFileWriter != null && line != null) {
                state.ten99T02rMasterUpdateFileWriter.write(line);
                state.ten99T02rMasterUpdateFileWriter.newLine();
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

    private void readTen99t01wduplicatetranfile(ProgramState state) {
        try {
            if (state.ten99T01wDuplicateTranFileReader == null) { state.ten99T01wDuplicateTranFileEof = true; return; }
            state.ten99T01wDuplicateTranFileLine = state.ten99T01wDuplicateTranFileReader.readLine();
            if (state.ten99T01wDuplicateTranFileLine == null) state.ten99T01wDuplicateTranFileEof = true;
        } catch (Exception e) { throw new RuntimeException("Read error", e); }
    }

    private void writeTen99t01wduplicatetranfile(ProgramState state, String line) {
        try {
            if (state.ten99T01wDuplicateTranFileWriter != null && line != null) {
                state.ten99T01wDuplicateTranFileWriter.write(line);
                state.ten99T01wDuplicateTranFileWriter.newLine();
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
        // 052600*=================================================================05260001
        // 052700*   PROGRAM CCAC6330 MAINLINE                                     05270001
        // 052800*   MAIN PROCESSING PARAGRAPH                                     05280001
        // 052900*=================================================================05290001
        // PERFORM 1000-INITIALIZATION
        // PERFORM 2000-MAIN-PROCESS UNTIL ALL-EOF
        // PERFORM 9000-END-OF-JOB
        // 053300*OQ01 - "GOBACK" FLAGGED PER STOP-RUN OPTION                      05330001
        // GOBACK
        // .
        // EJECT
        // ===== End COBOL =====
        
        try {
            initialization(state);
            while (!Boolean.TRUE.equals(state.allEof)) {
                mainProcess(state);
            }
            wrapUp(state);
            return;
        } catch (Exception e) {
            throw new RuntimeException("Error in mainline: " + e.getMessage(), e);
        }
    }


    private void initialization(ProgramState state) {
        // ===== Original COBOL: 1000-INITIALIZATION =====
        // 054000*=================================================================05400001
        // 054100*   THIS PARAGRAPH INITIALIZES THE READ AND COUNTERS AND CHECKS   05410001
        // 054200*   ALL HEADERS.                                                  05420001
        // 054300*=================================================================05430001
        // PERFORM 1010-OPEN-FILES
        // PERFORM 1020-INITIALIZE-SYSOUT
        // INITIALIZE WS-COUNTERS WS-ACCUMULATORS
        // INITIALIZE TEN99-M01W-MASTER-OUT-DTL TEN99-T01W-DUPLICATE-DTL05470001
        // 054800*--------------------------------------------------------------   05480001
        // 054900*  READ OF THE CONTROL CARD                                       05490001
        // 055000*--------------------------------------------------------------   05500001
        // PERFORM 7000-READ-CONTROL-CARD
        // IF CC-EOF
        // MOVE WS-ERROR-MSG-1 TO CC-E01W-DISPLAY-RCD
        // MOVE WS-1000-INIT TO SAR-PARAGRAPH
        // PERFORM 8999-WRITE-SYSOUT
        // PERFORM 9998-COREDUMP
        // END-IF
        // 055800*--------------------------------------------------------------   05580001
        // 055900*  INITIAL READS OF TRANSACTION FILE FOR HEADER AND FIRST REC     05590001
        // 056000*--------------------------------------------------------------   05600001
        // PERFORM 7100-READ-TRANSACTION-FILE
        // IF TRAN-EOF AND YEAR-END
        // MOVE WS-MSG-3 TO CC-E01W-DISPLAY-RCD
        // PERFORM 8999-WRITE-SYSOUT
        // ELSE
        // IF TRAN-EOF
        // MOVE WS-ERROR-MSG-2 TO CC-E01W-DISPLAY-RCD
        // MOVE WS-1000-INIT TO SAR-PARAGRAPH
        // PERFORM 8999-WRITE-SYSOUT
        // PERFORM 9998-COREDUMP
        // END-IF
        // END-IF
        // IF TRAN-HEADER
        // IF TRCCD-CONTROL-DATE = TTTH-HEAD-DATE
        // CONTINUE
        // ELSE
        // MOVE WS-ERROR-MSG-5 TO CC-E01W-DISPLAY-RCD
        // MOVE WS-1000-INIT TO SAR-PARAGRAPH
        // PERFORM 8999-WRITE-SYSOUT
        // PERFORM 9998-COREDUMP
        // END-IF
        // ELSE
        // MOVE WS-ERROR-MSG-6 TO CC-E01W-DISPLAY-RCD
        // MOVE WS-1000-INIT TO SAR-PARAGRAPH
        // PERFORM 8999-WRITE-SYSOUT
        // PERFORM 9998-COREDUMP
        // END-IF
        // PERFORM 7100-READ-TRANSACTION-FILE
        // IF TRAN-EOF AND YEAR-END
        // CONTINUE
        // ELSE
        // IF TRAN-EOF
        // MOVE WS-ERROR-MSG-2 TO CC-E01W-DISPLAY-RCD
        // MOVE WS-1000-INIT TO SAR-PARAGRAPH
        // PERFORM 8999-WRITE-SYSOUT
        // PERFORM 9998-COREDUMP
        // END-IF
        // END-IF
        // 059900*--------------------------------------------------------------   05990001
        // 060000*  INITIAL READS OF MASTER FILE FOR HEADER AND FIRST REC          06000001
        // 060100*--------------------------------------------------------------   06010001
        // PERFORM 7200-READ-MASTER-FILE
        // IF MASTER-EOF AND NEW-YEAR
        // MOVE WS-MSG-1 TO CC-E01W-DISPLAY-RCD
        // PERFORM 8999-WRITE-SYSOUT
        // ELSE
        // IF MASTER-EOF
        // MOVE WS-ERROR-MSG-3 TO CC-E01W-DISPLAY-RCD
        // MOVE WS-1000-INIT TO SAR-PARAGRAPH
        // PERFORM 8999-WRITE-SYSOUT
        // PERFORM 9998-COREDUMP
        // END-IF
        // IF MAST-HEADER AND REG-MONTH
        // IF TRCCD-CONTROL-MONTH = TMMIH-HEAD-MONTH + WS-ONE-LIT
        // CONTINUE
        // ELSE
        // MOVE WS-ERROR-MSG-13 TO CC-E01W-DISPLAY-RCD
        // MOVE WS-1000-INIT TO SAR-PARAGRAPH
        // PERFORM 8999-WRITE-SYSOUT
        // PERFORM 9998-COREDUMP
        // END-IF
        // ELSE
        // MOVE WS-ERROR-MSG-11 TO CC-E01W-DISPLAY-RCD
        // MOVE WS-1000-INIT TO SAR-PARAGRAPH
        // PERFORM 8999-WRITE-SYSOUT
        // PERFORM 9998-COREDUMP
        // END-IF
        // END-IF
        // PERFORM 7200-READ-MASTER-FILE
        // IF MASTER-EOF AND NEW-YEAR
        // MOVE WS-MSG-1 TO CC-E01W-DISPLAY-RCD
        // PERFORM 8999-WRITE-SYSOUT
        // ELSE
        // IF MASTER-EOF
        // MOVE WS-ERROR-MSG-3 TO CC-E01W-DISPLAY-RCD
        // MOVE WS-1000-INIT TO SAR-PARAGRAPH
        // PERFORM 8999-WRITE-SYSOUT
        // PERFORM 9998-COREDUMP
        // END-IF
        // END-IF
        // 064100*--------------------------------------------------------------   06410001
        // 064200*  INITIAL READS OF UPDATE FILE FOR HEADER AND FIRST REC          06420001
        // 064300*--------------------------------------------------------------   06430001
        // PERFORM 7300-READ-UPDATE-FILE
        // IF UPDATE-EOF
        // MOVE WS-MSG-2 TO CC-E01W-DISPLAY-RCD
        // PERFORM 8999-WRITE-SYSOUT
        // ELSE
        // IF UPDATE-HEADER
        // AND TRCCD-CONTROL-DATE = TTMUH-HEAD-DATE
        // CONTINUE
        // ELSE
        // MOVE WS-ERROR-MSG-12 TO CC-E01W-DISPLAY-RCD
        // MOVE WS-1000-INIT TO SAR-PARAGRAPH
        // PERFORM 8999-WRITE-SYSOUT
        // PERFORM 9998-COREDUMP
        // END-IF
        // END-IF
        // PERFORM 7300-READ-UPDATE-FILE
        // IF UPDATE-EOF
        // CONTINUE
        // ELSE
        // PERFORM 7300-READ-UPDATE-FILE
        // END-IF
        // 066500*-------------------------------------------------------------    06650001
        // 066600*   CREATE THE MASTER FILE HEADER                                 06660001
        // 066700*-------------------------------------------------------------    06670001
        // MOVE LOW-VALUES TO TMMOH-HEAD-SOURCE
        // MOVE TRCCD-CONTROL-DATE TO TMMOH-HEAD-DATE
        // MOVE WS-HEADER-LIT TO TMMOH-HEAD-ID
        // PERFORM 8000-WRITE-MASTER-OUT
        // 067200*-------------------------------------------------------------    06720001
        // 067300*   INITIALIZATION OF HOLD FIELDS                                 06730001
        // 067400*-------------------------------------------------------------    06740001
        // IF MASTER-EOF
        // MOVE WS-HIGH-KEY-LIT TO WS-HOLD-KEY-CURRENT
        // ELSE
        // MOVE TMMID-CHECK-NUMBER    TO WS-HOLD-CURRENT-CHECK
        // MOVE TMMID-DEPARTMENT-CODE TO WS-HOLD-CURRENT-DEPT
        // MOVE TMMID-TAX-TYPE        TO WS-HOLD-CURRENT-TAX-TYPE
        // MOVE TMMID-TIN-IND         TO WS-HOLD-CURRENT-TIN
        // END-IF
        // MOVE WS-LOW-KEY-LIT TO WS-HOLD-KEY-PREVIOUS
        // IF TRAN-EOF
        // MOVE WS-HIGH-KEY-LIT TO WS-HOLD-TRANS-KEY
        // ELSE
        // MOVE TTTD-CHECK-NUM       TO WS-HOLD-TRANS-CHECK
        // MOVE TTTD-DEPARTMENT-CODE TO WS-HOLD-TRANS-DEPT
        // MOVE TTTD-KEY-TAX-TYPE    TO WS-HOLD-TRANS-TAX-TYPE
        // MOVE TTTD-KEY-TIN-IND     TO WS-HOLD-TRANS-TIN
        // END-IF
        // .
        // EJECT
        // ===== End COBOL =====
        
        try {
            openFiles(state);
            initializeSysout(state);
            state.wsCounters = null;
            state.wsAccumulators = null;
            state.ten99M01wMasterOutDtl = null;
            state.ten99T01wDuplicateDtl = null;
            readControlCard(state);
            if (Boolean.TRUE.equals(state.ccEof)) {
                state.ccE01wDisplayRcd = state.wsErrorMsg1;
                state.sarParagraph = state.ws1000Init;
                initializeSysout(state);
                // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method
            }
            readTransactionFile(state);
            if (Boolean.TRUE.equals(state.tranEof) && Boolean.TRUE.equals(state.yearEnd)) {
                state.ccE01wDisplayRcd = state.wsMsg3;
                initializeSysout(state);
            } else {
                if (Boolean.TRUE.equals(state.tranEof)) {
                    state.ccE01wDisplayRcd = state.wsErrorMsg2;
                    state.sarParagraph = state.ws1000Init;
                    initializeSysout(state);
                    // TODO: unknown method
                }
            }
            if (Boolean.TRUE.equals(state.tranHeader)) {
                if (state.trccdControlDate.equals(state.ttthHeadDate)) {
                    // CONTINUE
                } else {
                    state.ccE01wDisplayRcd = state.wsErrorMsg5;
                    state.sarParagraph = state.ws1000Init;
                    // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method initializeSysout(state);
                    // TODO: unknown method
                }
            } else {
                state.ccE01wDisplayRcd = state.wsErrorMsg6;
                state.sarParagraph = state.ws1000Init;
                initializeSysout(state);
                // TODO: unknown method
            }
            readTransactionFile(state);
            if (Boolean.TRUE.equals(state.tranEof) && Boolean.TRUE.equals(state.yearEnd)) {
                // CONTINUE
            } else {
                if (Boolean.TRUE.equals(state.tranEof)) {
                    state.ccE01wDisplayRcd = state.wsErrorMsg2;
                    state.sarParagraph = state.ws1000Init;
                    initializeSysout(state);
                    // TODO: unknown method
                }
            }
            readMasterFile(state);
            if (Boolean.TRUE.equals(state.masterEof) && Boolean.TRUE.equals(state.newYear)) {
                state.ccE01wDisplayRcd = state.wsMsg1;
                initializeSysout(state);
            } else {
                if (Boolean.TRUE.equals(state.masterEof)) {
                    state.ccE01wDisplayRcd = state.wsErrorMsg3;
                    state.sarParagraph = state.ws1000Init;
                    initializeSysout(state);
                    // TODO: unknown method
                }
                if (Boolean.TRUE.equals(state.mastHeader) && Boolean.TRUE.equals(state.regMonth)) {
                    if (state.trccdControlMonth == state.tmmihHeadMonth + state.wsOneLit) {
                        // CONTINUE
                    } else {
                        state.ccE01wDisplayRcd = state.wsErrorMsg13;
                        state.sarParagraph = state.ws1000Init;
                        initializeSysout(state);
                        // TODO: unknown method
                    }
                } else {
                    state.ccE01wDisplayRcd = state.wsErrorMsg11;
                    state.sarParagraph = state.ws1000Init;
                    initializeSysout(state);
                    // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method
                }
            }
            readMasterFile(state);
            if (Boolean.TRUE.equals(state.masterEof) && Boolean.TRUE.equals(state.newYear)) {
                state.ccE01wDisplayRcd = state.wsMsg1;
                // TODO: unknown method
            } else {
                if (Boolean.TRUE.equals(state.masterEof)) {
                    state.ccE01wDisplayRcd = state.wsErrorMsg3;
                    state.sarParagraph = state.ws1000Init;
                    // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method
                    // TODO: unknown method
                }
            }
            readUpdateFile(state);
            if (Boolean.TRUE.equals(state.updateEof)) {
                state.ccE01wDisplayRcd = state.wsMsg2;
                // TODO: unknown method
            } else {
                if (Boolean.TRUE.equals(state.updateHeader) && state.trccdControlDate.equals(state.ttmuhHeadDate)) {
                    // CONTINUE
                } else {
                    state.ccE01wDisplayRcd = state.wsErrorMsg12;
                    state.sarParagraph = state.ws1000Init;
                    // TODO: unknown method
                    // TODO: unknown method
                }
            }
            readUpdateFile(state);
            if (Boolean.TRUE.equals(state.updateEof)) {
                // CONTINUE
            } else {
                readUpdateFile(state);
            }
            state.tmmohHeadSource = "\u0000";
            state.tmmohHeadDate = state.trccdControlDate;
            state.tmmohHeadId = state.wsHeaderLit;
            writeMasterOut(state);
            if (Boolean.TRUE.equals(state.masterEof)) {
                state.wsHoldKeyCurrent = state.wsHighKeyLit;
            } else {
                state.wsHoldCurrentCheck = state.tmmidCheckNumber;
                state.wsHoldCurrentDept = state.tmmidDepartmentCode;
                state.wsHoldCurrentTaxType = state.tmmidTaxType;
                state.wsHoldCurrentTin = state.tmmidTinInd;
            }
            state.wsHoldKeyPrevious = state.wsLowKeyLit;
            if (Boolean.TRUE.equals(state.tranEof)) {
                state.wsHoldTransKey = state.wsHighKeyLit;
            } else {
                state.wsHoldTransCheck = state.tttdCheckNum;
                state.wsHoldTransDept = state.tttdDepartmentCode;
                state.wsHoldTransTaxType = state.tttdKeyTaxType;
                state.wsHoldTransTin = state.tttdKeyTinInd;
            }
        } catch (Exception e) {
            throw new RuntimeException("Error in initialization: " + e.getMessage(), e);
        }
    }


    private void openFiles(ProgramState state) {
        // ===== Original COBOL: 1010-OPEN-FILES =====
        // 069200*==============================================================*  06920001
        // 069300*    THIS PARAGRAPH OPENS ALL OF THE INPUT AND OUTPUT FILES.   *  06930001
        // 069400*==============================================================*  06940001
        // OPEN INPUT TEN99-M01R-MASTER-IN-FILE
        // TEN99-R01R-CONTROL-FILE
        // TEN99-T01R-TRANSACTION-FILE
        // TEN99-T02R-MASTER-UPDATE-FILE
        // OPEN OUTPUT TEN99-M01W-MASTER-OUT-FILE
        // TEN99-T01W-DUPLICATE-TRAN-FILE
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
        // 070600*=================================================================07060001
        // 070700*  THIS PARAGRAPH INITIALIZES THE SYSOUT DISPLAY AREA.            07070001
        // 070800*  THE CODE CAN BE FOUND IN COPYLIB C2INP001.                     07080001
        // 070900*=================================================================07090001
        // 071000*++INCLUDE C2INZ001                                               07100001
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
        // 071500*=================================================================07150001
        // 071600*  THIS PARAGRAPH IS THE MAIN CONTROL OF THE PROGRAM PROCESSING.  07160001
        // 071700*  IT WILL BE EXECUTED UNTIL ALL INPUTS ARE AT END OF FILE.       07170001
        // 071800*=================================================================07180001
        // PERFORM 2200-PROCESS-TRANSACTION
        // UNTIL WS-HOLD-TRANS-KEY > WS-HOLD-KEY-CURRENT
        // OR TRAN-EOF
        // EVALUATE TRUE
        // WHEN NEW-YEAR
        // CONTINUE
        // WHEN OTHER
        // IF MASTER-EOF
        // CONTINUE
        // ELSE
        // PERFORM 2400-PROCESS-MASTER
        // IF UPDATE-EOF
        // CONTINUE
        // ELSE
        // PERFORM 2300-CHECK-UPDATES
        // END-IF
        // PERFORM 8000-WRITE-MASTER-OUT
        // PERFORM 7200-READ-MASTER-FILE
        // MOVE WS-HOLD-KEY-CURRENT TO WS-HOLD-KEY-PREVIOUS
        // IF MASTER-EOF
        // MOVE WS-HIGH-KEY-LIT
        // TO WS-HOLD-KEY-CURRENT
        // ELSE
        // MOVE TMMID-CHECK-NUMBER
        // TO WS-HOLD-CURRENT-CHECK
        // MOVE TMMID-DEPARTMENT-CODE
        // TO WS-HOLD-CURRENT-DEPT
        // MOVE TMMID-TAX-TYPE
        // TO WS-HOLD-CURRENT-TAX-TYPE
        // MOVE TMMID-TIN-IND
        // TO WS-HOLD-CURRENT-TIN
        // END-IF
        // END-IF
        // IF WS-HOLD-KEY-CURRENT < WS-HOLD-KEY-PREVIOUS
        // MOVE WS-ERROR-MSG-8 TO CC-E01W-DISPLAY-RCD
        // MOVE WS-2000-MAIN TO SAR-PARAGRAPH
        // PERFORM 8999-WRITE-SYSOUT
        // PERFORM 9998-COREDUMP
        // END-IF
        // END-EVALUATE
        // PERFORM 2500-CHECK-FLAGS
        // .
        // EJECT
        // ===== End COBOL =====
        
        try {
            while (!(state.wsHoldTransKey.compareTo(state.wsHoldKeyCurrent) > 0 || Boolean.TRUE.equals(state.tranEof))) {
                processTransaction(state);
            }
            if (Boolean.TRUE.equals(state.newYear)) {
                // CONTINUE
            } else {
                if (Boolean.TRUE.equals(state.masterEof)) {
                    // CONTINUE
                } else {
                    processMaster(state);
                    if (Boolean.TRUE.equals(state.updateEof)) {
                        // CONTINUE
                    } else {
                        checkUpdates(state);
                    }
                    writeMasterOut(state);
                    readMasterFile(state);
                    state.wsHoldKeyPrevious = state.wsHoldKeyCurrent;
                    if (Boolean.TRUE.equals(state.masterEof)) {
                        state.wsHoldKeyCurrent = state.wsHighKeyLit;
                    } else {
                        state.wsHoldCurrentCheck = state.tmmidCheckNumber;
                        state.wsHoldCurrentDept = state.tmmidDepartmentCode;
                        state.wsHoldCurrentTaxType = state.tmmidTaxType;
                        state.wsHoldCurrentTin = state.tmmidTinInd;
                    }
                }
                if (state.wsHoldKeyCurrent.compareTo(state.wsHoldKeyPrevious) < 0) {
                    state.ccE01wDisplayRcd = state.wsErrorMsg8;
                    state.sarParagraph = state.ws2000Main;
                    // TODO: unknown method
                    // TODO: unknown method
                }
            }
            checkFlags(state);
        } catch (Exception e) {
            throw new RuntimeException("Error in mainProcess: " + e.getMessage(), e);
        }
    }


    private void processTransaction(ProgramState state) {
        // ===== Original COBOL: 2200-PROCESS-TRANSACTION =====
        // 076000*=================================================================07600001
        // 076100*  THIS PARAGRAPH WILL CREATE NEW MASTER RECORDS IN SEQUENCE ORDER07610001
        // 076200*  OR DUPLIC A TRANSACTION RECORD IF THE KEY ALREADY EXISTS ON THE07620001
        // 076300*  MASTER FILE.                                                   07630001
        // 076400*=================================================================07640001
        // IF WS-HOLD-TRANS-KEY < WS-HOLD-KEY-PREVIOUS
        // MOVE WS-ERROR-MSG-7 TO CC-E01W-DISPLAY-RCD
        // DISPLAY "WS-HOLD-TRANS-KEY = " WS-HOLD-TRANS-KEY
        // DISPLAY "WS-HOLD-KEY-PREVIOUS (MASTER) = "
        // WS-HOLD-KEY-PREVIOUS
        // MOVE WS-2200-PROC TO SAR-PARAGRAPH
        // PERFORM 8999-WRITE-SYSOUT
        // PERFORM 9998-COREDUMP
        // END-IF
        // IF WS-HOLD-TRANS-KEY = WS-HOLD-KEY-CURRENT
        // OR WS-HOLD-TRANS-KEY = WS-HOLD-LAST-WRITE-KEY
        // IF TRAN-TRAILER
        // PERFORM 7100-READ-TRANSACTION-FILE
        // ELSE
        // MOVE TEN99-T01R-TRANS-DTL
        // TO TEN99-T01W-DUPLICATE-DTL
        // PERFORM 8100-WRITE-DUPLICATE-TRANS-OUT
        // PERFORM 7100-READ-TRANSACTION-FILE
        // END-IF
        // ELSE
        // IF WS-HOLD-TRANS-KEY < WS-HOLD-KEY-CURRENT
        // AND WS-HOLD-TRANS-KEY > WS-HOLD-KEY-PREVIOUS
        // PERFORM 2210-CREATE-NEW-MASTER-RECORD
        // PERFORM 8000-WRITE-MASTER-OUT
        // PERFORM 7100-READ-TRANSACTION-FILE
        // END-IF
        // END-IF
        // .
        // EJECT
        // ===== End COBOL =====
        
        try {
            if (state.wsHoldTransKey.compareTo(state.wsHoldKeyPrevious) < 0) {
                state.ccE01wDisplayRcd = state.wsErrorMsg7;
                System.out.println("WS-HOLD-TRANS-KEY = " + state.wsHoldTransKey);
                System.out.println("WS-HOLD-KEY-PREVIOUS (MASTER) = " + state.wsHoldKeyPrevious);
                state.sarParagraph = state.ws2200Proc;
                // TODO: unknown method
                // TODO: unknown method
            }
            if (state.wsHoldTransKey.equals(state.wsHoldKeyCurrent) || state.wsHoldTransKey.equals(state.wsHoldLastWriteKey)) {
                if (Boolean.TRUE.equals(state.tranTrailer)) {
                    readTransactionFile(state);
                } else {
                    state.ten99T01wDuplicateDtl = state.ten99T01rTransDtl;
                    writeDuplicateTransOut(state);
                    readTransactionFile(state);
                }
            } else {
                if (state.wsHoldTransKey.compareTo(state.wsHoldKeyCurrent) < 0 && state.wsHoldTransKey.compareTo(state.wsHoldKeyPrevious) > 0) {
                    createNewMasterRecord(state);
                    writeMasterOut(state);
                    readTransactionFile(state);
                }
            }
        } catch (Exception e) {
            throw new RuntimeException("Error in processTransaction: " + e.getMessage(), e);
        }
    }


    private void createNewMasterRecord(ProgramState state) {
        // ===== Original COBOL: 2210-CREATE-NEW-MASTER-RECORD =====
        // 079600*=================================================================07960001
        // 079700*  THIS PARAGRAPH FORMATS THE NEW MASTER RECORDS WITH INPUT FROM  07970001
        // 079800*  THE TRANSACTION FILE.                                          07980001
        // 079900*=================================================================07990001
        // MOVE TTTD-CHECK-NUM TO TMMOD-CHECK-NUMBER
        // MOVE TTTD-DEPARTMENT-CODE TO TMMOD-DEPARTMENT-CODE
        // MOVE SPACES TO TMMOD-DELETE-INDICATOR
        // MOVE TTTD-SOCIAL-SEC-NUMBER TO TMMOD-SOCIAL-SEC-NUMBER
        // MOVE TTTD-SOCIAL-SEC-REMAINDER
        // TO TMMOD-SOCIAL-SEC-REMAINDER
        // MOVE TTTD-OPER-LOCATION TO TMMOD-OPERATION-LOC
        // MOVE TTTD-SOURCE-CODE TO TMMOD-SOURCE
        // MOVE TTTD-NAME TO TMMOD-NAME
        // MOVE TTTD-ADDRESS-1 TO TMMOD-ADDRESS-1
        // MOVE TTTD-ADDRESS-2 TO TMMOD-ADDRESS-2
        // MOVE TTTD-CITY TO TMMOD-CITY
        // MOVE TTTD-STATE-CODE TO TMMOD-STATE
        // MOVE TTTD-ZIP-CODE TO TMMOD-ZIP-CODE
        // MOVE TTTD-TOTAL-AMOUNT TO TMMOD-TOTAL-AMOUNT
        // MOVE TTTD-1099-AMOUNT TO TMMOD-1099-AMOUNT
        // MOVE TTTD-CHECK-DATE TO TMMOD-CHECK-DATE
        // MOVE TRCCD-CONTROL-DATE TO TMMOD-PROCESS-DATE
        // MOVE TTTD-NUMBER-DISB-RECORDS TO TMMOD-NUM-DISB-RECORDS
        // MOVE TTTD-LINE-NBR            TO TMMOD-LINE-NBR
        // PERFORM 2215-CREATE-DISB-RECORDS
        // VARYING DISB-SUB FROM 1 BY 1
        // UNTIL DISB-SUB > TMMOD-NUM-DISB-RECORDS
        // IF MISC-FORM-ISSUE
        // MOVE "Y" TO TMMOD-DISB-1099-INDICATOR(1)
        // MOVE TTTD-1099-AMOUNT TO TMMOD-DISB-AMOUNT(1)
        // END-IF
        // MOVE TTTD-COMMENTS     TO TMMOD-COMMENTS
        // MOVE TTTD-KEY-TAX-TYPE TO TMMOD-TAX-TYPE
        // MOVE TTTD-KEY-TIN-IND  TO TMMOD-TIN-IND
        // .
        // ===== End COBOL =====
        
        try {
            state.tmmidCheckNumber = state.tttdCheckNum;
            state.tmmidDepartmentCode = state.tttdDepartmentCode;
            state.tmmidDeleteIndicator = " ";
            state.tmmidSocialSecNumber = state.tttdSocialSecNumber;
            state.tmmidSocialSecRemainder = state.tttdSocialSecRemainder;
            state.tmmidOperationLoc = state.tttdOperLocation;
            state.tmmidSource = state.tttdSourceCode;
            state.tmmidName = state.tttdName;
            state.tmmidAddress1 = state.tttdAddress1;
            state.tmmidAddress2 = state.tttdAddress2;
            state.tmmidCity = state.tttdCity;
            state.tmmidState = state.tttdStateCode;
            state.tmmidZipCode = state.tttdZipCode;
            state.tmmidTotalAmount = state.tttdTotalAmount;
            state.tmmid1099Amount = state.tttd1099Amount;
            state.tmmidCheckDate = state.tttdCheckDate;
            state.tmmidProcessDate = state.panValet;
            state.tmmidNumDisbRecords = state.tttdNumberDisbRecords;
            state.tmmidLineNbr = state.tttdLineNbr;
            int disbSub = 1;
            while (disbSub <= state.tmmidNumDisbRecords) {
                createDisbRecords(state);
                disbSub = disbSub + 1;
            }
            if (state.pups2000 != null && state.pups2000.equals("MISC-FORM-ISSUE")) {
                state.tmmidDisb1099Indicator = "Y";
                state.tmmidDisbAmount = state.tttd1099Amount;
            }
            state.tmmidComments = state.tttdComments;
            state.tmmidTaxType = state.tttdKeyTaxType;
            state.tmmidTinInd = state.tttdKeyTinInd;
        } catch (Exception e) {
            throw new RuntimeException("Error in createNewMasterRecord: " + e.getMessage(), e);
        }
    }


    private void createDisbRecords(ProgramState state) {
        // ===== Original COBOL: 2215-CREATE-DISB-RECORDS =====
        // 083100*=================================================================08310001
        // 083200*  THIS PARAGRAPH MOVE THE DISBURSEMENT RECORD PORTION OF THE     08320001
        // 083300*  TRANSACTION FILE TO THE NEW MASTER RECORD.                     08330001
        // 083400*=================================================================08340001
        // MOVE TTTD-DISB-RECORDS (DISB-SUB)
        // TO TMMOD-DISB-RECORDS (DISB-SUB)
        // .
        // EJECT
        // ===== End COBOL =====
        
        try {
            state.tmmidDisbRecord = state.tttdDisbRecords;
        } catch (Exception e) {
            throw new RuntimeException("Error in createDisbRecords: " + e.getMessage(), e);
        }
    }


    private void checkUpdates(ProgramState state) {
        // ===== Original COBOL: 2300-CHECK-UPDATES =====
        // 084100*=================================================================08410001
        // 084200*  THIS PARAGRAPH CHECKS TO SEE IF A MASTER RECORD HAS BEEN       08420001
        // 084300*  UPDATED.  IF IT HAS, THE UPDATE RECORD IS USED TO OVERLAY      08430001
        // 084400*  THE EXISTING INPUT MASTER RECORD.                              08440001
        // 084500*=================================================================08450001
        // IF WS-UPDATE-KEY = WS-HOLD-KEY-CURRENT
        // PERFORM 2310-PROCESS-UPDATE
        // PERFORM 7300-READ-UPDATE-FILE
        // ELSE
        // IF WS-UPDATE-KEY > WS-HOLD-KEY-CURRENT
        // CONTINUE
        // ELSE
        // MOVE WS-ERROR-MSG-4 TO CC-E01W-DISPLAY-RCD
        // MOVE WS-2300-CHECK TO SAR-PARAGRAPH
        // PERFORM 8999-WRITE-SYSOUT
        // PERFORM 9998-COREDUMP
        // END-IF
        // END-IF
        // .
        // EJECT
        // ===== End COBOL =====
        
        try {
            if (state.wsUpdateKey.equals(state.wsHoldKeyCurrent)) {
                processUpdate(state);
                readUpdateFile(state);
            } else {
                if (state.wsUpdateKey.compareTo(state.wsHoldKeyCurrent) > 0) {
                    // CONTINUE
                } else {
                    state.ccE01wDisplayRcd = state.wsErrorMsg4;
                    state.sarParagraph = state.ws2300Check;
                    // TODO: unknown method // TODO: unknown method
                    // TODO: unknown method // TODO: unknown method
                }
            }
        } catch (Exception e) {
            throw new RuntimeException("Error in checkUpdates: " + e.getMessage(), e);
        }
    }


    private void processUpdate(ProgramState state) {
        // ===== Original COBOL: 2310-PROCESS-UPDATE =====
        // 086300*=================================================================08630001
        // 086400*  THIS PARAGRAPH FORMATS THE OVERLAY OF THE MASTER RECORD        08640001
        // 086500*  WITH THE FIELDS FROM THE UPDATE RECORD.  ONLY THE DELETE       08650001
        // 086600*  INDICATOR IS MOVE TO THE RECORD.  CHANGE INDICATOR IS NOT KEPT.08660001
        // 086700*=================================================================08670001
        // IF DELETE-UPDATE
        // MOVE TTMUD-DELETE-CHANGE-INDICATOR
        // TO TMMOD-DELETE-INDICATOR
        // ELSE
        // MOVE SPACES TO TMMOD-DELETE-INDICATOR
        // END-IF
        // MOVE TTMUD-SOCIAL-SEC-NUMBER TO TMMOD-SOCIAL-SEC-NUMBER
        // MOVE TTMUD-SOCIAL-SEC-REMAINDER
        // TO TMMOD-SOCIAL-SEC-REMAINDER
        // MOVE TTMUD-NAME TO TMMOD-NAME
        // MOVE TTMUD-ADDRESS-1 TO TMMOD-ADDRESS-1
        // MOVE TTMUD-ADDRESS-2 TO TMMOD-ADDRESS-2
        // MOVE TTMUD-CITY TO TMMOD-CITY
        // MOVE TTMUD-STATE TO TMMOD-STATE
        // MOVE TTMUD-ZIP-CODE TO TMMOD-ZIP-CODE
        // .
        // ===== End COBOL =====
        
        try {
            if ("DELETE".equals(state.ttmudDeleteChangeIndicator)) {
                state.tmmidDeleteIndicator = state.ttmudDeleteChangeIndicator;
            } else {
                state.tmmidDeleteIndicator = " ";
            }
            state.tmmidSocialSecNumber = state.ttmudSocialSecNumber;
            state.tmmidSocialSecRemainder = state.ttmudSocialSecRemainder;
            state.tmmidName = state.ttmudName;
            state.tmmidAddress1 = state.ttmudAddress1;
            state.tmmidAddress2 = state.ttmudAddress2;
            state.tmmidCity = state.ttmudCity;
            state.tmmidState = state.ttmudState;
            state.tmmidZipCode = state.ttmudZipCode;
        } catch (Exception e) {
            throw new RuntimeException("Error in processUpdate: " + e.getMessage(), e);
        }
    }


    private void processMaster(ProgramState state) {
        // ===== Original COBOL: 2400-PROCESS-MASTER =====
        // 088700*=================================================================08870001
        // 088800*  THIS PARAGRAPH MOVES ALL INPUT MASTER FIELDS TO THE            08880001
        // 088900*  OUTPUT MASTER RECORD.  NO CHANGES ARE MADE TO THIS MASTER.     08890001
        // 089000*=================================================================08900001
        // MOVE TEN99-M01R-MASTER-IN-DTL
        // TO TEN99-M01W-MASTER-OUT-DTL
        // .
        // ===== End COBOL =====
        
        try {
            state.ten99M01wMasterOutDtl = state.ten99M01rMasterInDtl;
        } catch (Exception e) {
            throw new RuntimeException("Error in processMaster: " + e.getMessage(), e);
        }
    }


    private void checkFlags(ProgramState state) {
        // ===== Original COBOL: 2500-CHECK-FLAGS =====
        // 089600*=================================================================08960001
        // 089700*  THIS PARAGRAPH CHECKS THE FLAGS ON THE INPUT FILES AND         08970001
        // 089800*  SETS THE EOF FLAG TO COMPLETE THE MAIN PROCESSING ONLY WHEN    08980001
        // 089900*  ALL FILES (TRANS, MASTER, UPDATE) HAVE REACHED EOF.            08990001
        // 090000*=================================================================09000001
        // IF TRAN-EOF AND MASTER-EOF AND UPDATE-EOF
        // SET ALL-EOF TO TRUE
        // ELSE
        // CONTINUE
        // END-IF
        // .
        // EJECT
        // ===== End COBOL =====
        
        try {
            if (Boolean.TRUE.equals(state.tranEof) && Boolean.TRUE.equals(state.masterEof) && Boolean.TRUE.equals(state.updateEof)) {
                state.allEof = "true";
            }
        } catch (Exception e) {
            throw new RuntimeException("Error in checkFlags: " + e.getMessage(), e);
        }
    }


    private void readControlCard(ProgramState state) {
        // ===== Original COBOL: 7000-READ-CONTROL-CARD =====
        // 091000*=================================================================09100001
        // 091100*  THIS PARAGRAPH READS THE CONTROL CARD.                         09110001
        // 091200*=================================================================09120001
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
            if (state.ten99R01rControlFile == null) {
                state.ccEof = "true";
            }
        } catch (Exception e) {
            throw new RuntimeException("Error in readControlCard: " + e.getMessage(), e);
        }
    }


    private void readTransactionFile(ProgramState state) {
        // ===== Original COBOL: 7100-READ-TRANSACTION-FILE =====
        // 092200*=================================================================09220001
        // 092300*  THIS PARAGRAPH READS THE TRANSACTION FILE                      09230001
        // 092400*=================================================================09240001
        // IF TRAN-EOF
        // CONTINUE
        // ELSE
        // READ TEN99-T01R-TRANSACTION-FILE
        // INTO TEN99-T01R-TRANS-DTL
        // AT END
        // SET TRAN-EOF TO TRUE
        // NOT AT END
        // IF TRAN-HEADER OR TRAN-TRAILER
        // CONTINUE
        // ELSE
        // ADD WS-ONE-LIT TO WS-TRANS-IN-CNT
        // ADD TTTD-TOTAL-AMOUNT TO WS-TRANS-AMT
        // MOVE TTTD-KEY-CHECK-NUM
        // TO WS-HOLD-TRANS-CHECK
        // MOVE TTTD-DEPARTMENT-CODE
        // TO WS-HOLD-TRANS-DEPT
        // MOVE TTTD-KEY-TAX-TYPE
        // TO WS-HOLD-TRANS-TAX-TYPE
        // MOVE TTTD-KEY-TIN-IND
        // TO WS-HOLD-TRANS-TIN
        // END-IF
        // END-READ
        // END-IF
        // IF TRAN-TRAILER
        // SET TRAN-EOF TO TRUE
        // END-IF
        // IF TRAN-EOF AND TRAN-TRAILER
        // CONTINUE
        // ELSE
        // IF TRAN-EOF
        // MOVE WS-ERROR-MSG-9 TO CC-E01W-DISPLAY-RCD
        // MOVE WS-7100-READ TO SAR-PARAGRAPH
        // PERFORM 8999-WRITE-SYSOUT
        // PERFORM 9998-COREDUMP
        // END-IF
        // END-IF
        // .
        // EJECT
        // ===== End COBOL =====
        
        try {
            if (Boolean.TRUE.equals(state.tranEof)) {
                // CONTINUE
            } else {
                readTen99t01rtransactionfile(state);
                if (Boolean.TRUE.equals(state.atEnd)) {
                    state.tranEof = "true";
                } else {
                    if (Boolean.TRUE.equals(state.tranHeader) || Boolean.TRUE.equals(state.tranTrailer)) {
                        // CONTINUE
                    } else {
                        state.wsTransInCnt = state.wsTransInCnt + state.wsOneLit;
                        state.wsTransAmt = state.wsTransAmt.add(state.tttdTotalAmount);
                        state.wsHoldTransCheck = state.tttdKeyCheckNum;
                        state.wsHoldTransDept = state.tttdDepartmentCode;
                        state.wsHoldTransTaxType = state.tttdKeyTaxType;
                        state.wsHoldTransTin = state.tttdKeyTinInd;
                    }
                }
            }
            if (Boolean.TRUE.equals(state.tranTrailer)) {
                state.tranEof = "true";
            }
            if (Boolean.TRUE.equals(state.tranEof) && Boolean.TRUE.equals(state.tranTrailer)) {
                // CONTINUE
            } else {
                if (Boolean.TRUE.equals(state.tranEof)) {
                    state.ccE01wDisplayRcd = state.wsErrorMsg9;
                    state.sarParagraph = state.ws7100Read;
                    // TODO: unknown method
                    // TODO: unknown method
                }
            }
        } catch (Exception e) {
            throw new RuntimeException("Error in readTransactionFile: " + e.getMessage(), e);
        }
    }


    private void readMasterFile(ProgramState state) {
        // ===== Original COBOL: 7200-READ-MASTER-FILE =====
        // 096200*=================================================================09620001
        // 096300*  THIS PARAGRAPH READS THE MASTER FILE                           09630001
        // 096400*=================================================================09640001
        // IF MASTER-EOF
        // CONTINUE
        // ELSE
        // READ TEN99-M01R-MASTER-IN-FILE
        // INTO TEN99-M01R-MASTER-IN-DTL
        // AT END
        // SET MASTER-EOF TO TRUE
        // NOT AT END
        // IF MAST-HEADER OR MAST-TRAILER
        // CONTINUE
        // ELSE
        // IF DELETE-INPUT
        // ADD WS-ONE-LIT TO WS-MAST-DELETE-IN-CNT
        // ADD TMMID-1099-AMOUNT
        // TO WS-MAST-DELETE-IN-AMT
        // ELSE
        // ADD WS-ONE-LIT TO WS-MASTER-IN-CNT
        // ADD TMMID-1099-AMOUNT
        // TO WS-MAST-IN-AMT
        // END-IF
        // END-IF
        // END-READ
        // END-IF
        // IF MAST-TRAILER
        // SET MASTER-EOF TO TRUE
        // END-IF
        // .
        // EJECT
        // ===== End COBOL =====
        
        try {
            if (Boolean.TRUE.equals(state.masterEof)) {
                // CONTINUE
            } else {
                readTen99m01rmasterinfile(state);
                if (Boolean.TRUE.equals(state.atEndTen99m01rmasterinfile)) {
                    state.masterEof = "true";
                } else {
                    if (Boolean.TRUE.equals(state.mastHeader) || Boolean.TRUE.equals(state.mastTrailer)) {
                        // CONTINUE
                    } else {
                        if (Boolean.TRUE.equals(state.deleteInput)) {
                            state.wsMastDeleteInCnt = state.wsMastDeleteInCnt + state.wsOneLit;
                            state.wsMastDeleteInAmt = state.wsMastDeleteInAmt.add(state.tmmid1099Amount);
                        } else {
                            state.wsMasterInCnt = state.wsMasterInCnt + state.wsOneLit;
                            state.wsMastInAmt = state.wsMastInAmt.add(state.tmmid1099Amount);
                        }
                    }
                }
            }
            if (Boolean.TRUE.equals(state.mastTrailer)) {
                state.masterEof = "true";
            }
        } catch (Exception e) {
            throw new RuntimeException("Error in readMasterFile: " + e.getMessage(), e);
        }
    }


    private void readUpdateFile(ProgramState state) {
        // ===== Original COBOL: 7300-READ-UPDATE-FILE =====
        // 099500*=================================================================09950001
        // 099600*  THIS PARAGRAPH READS THE MASTER UPDATE FILE                    09960001
        // 099700*=================================================================09970001
        // IF UPDATE-EOF
        // CONTINUE
        // ELSE
        // READ TEN99-T02R-MASTER-UPDATE-FILE
        // INTO TEN99-T02R-MASTER-UPDATE-DTL
        // AT END
        // SET UPDATE-EOF TO TRUE
        // NOT AT END
        // IF UPDATE-HEADER OR UPDATE-TRAILER
        // CONTINUE
        // ELSE
        // MOVE TTMUD-CHECK-NUMBER    TO WS-UPDATE-CHECK
        // MOVE TTMUD-DEPARTMENT-CODE TO WS-UPDATE-DEPT
        // MOVE TMMUD-TAX-TYPE        TO WS-UPDATE-TAX-TYPE10101003
        // MOVE TMMUD-TIN-IND         TO WS-UPDATE-TIN
        // ADD WS-ONE-LIT TO WS-UPDATES-IN-CNT
        // IF DELETE-UPDATE
        // ADD TTMUD-1099-AMOUNT
        // TO WS-MAST-DELETE-UP-AMT
        // ELSE
        // ADD TTMUD-1099-AMOUNT
        // TO WS-MAST-UP-AMT
        // END-IF
        // END-IF
        // END-READ
        // END-IF
        // IF UPDATE-TRAILER
        // SET UPDATE-EOF TO TRUE
        // END-IF
        // .
        // EJECT
        // ===== End COBOL =====
        
        try {
            if (Boolean.TRUE.equals(state.updateEof)) {
                // CONTINUE
            } else {
                readTen99t02rmasterupdatefile(state);
                if ("true".equals(state.atEnd)) {
                    state.updateEof = "true";
                } else {
                    if ("true".equals(state.updateHeader) || "true".equals(state.updateTrailer)) {
                        // CONTINUE
                    } else {
                        state.wsUpdateCheck = state.ttmudCheckNumber;
                        state.wsUpdateDept = state.ttmudDepartmentCode;
                        state.wsUpdateTaxType = state.tmmudTaxType;
                        state.wsUpdateTin = state.tmmudTinInd;
                        state.wsUpdatesInCnt = state.wsUpdatesInCnt + state.wsOneLit;
                        if ("true".equals(state.deleteUpdate)) {
                            state.wsMastDeleteUpAmt = state.wsMastDeleteUpAmt.add(state.ttmud1099Amount);
                        } else {
                            state.wsMastUpAmt = state.wsMastUpAmt.add(state.ttmud1099Amount);
                        }
                    }
                }
            }
            if ("true".equals(state.updateTrailer)) {
                state.updateEof = "true";
            }
        } catch (Exception e) {
            throw new RuntimeException("Error in readUpdateFile: " + e.getMessage(), e);
        }
    }


    private void writeMasterOut(ProgramState state) {
        // ===== Original COBOL: 8000-WRITE-MASTER-OUT =====
        // 102900*=================================================================10290001
        // 103000*  THIS PARAGRAPH WRITE THE MASTER FILE                           10300001
        // 103100*=================================================================10310001
        // WRITE TEN99-M01W-MASTER-OUT-RCD
        // FROM TEN99-M01W-MASTER-OUT-DTL
        // IF MAST-OUT-HEAD OR MAST-OUT-TRAILER
        // CONTINUE
        // ELSE
        // IF DELETE-OUTPUT
        // ADD WS-ONE-LIT TO WS-MAST-DELETE-OUT-CNT
        // ADD TMMOD-1099-AMOUNT
        // TO WS-MAST-DELETE-OUT-AMT
        // ELSE
        // ADD WS-ONE-LIT TO WS-MAST-OUT-CNT
        // ADD TMMOD-1099-AMOUNT
        // TO WS-MAST-OUT-AMT
        // END-IF
        // END-IF
        // MOVE TMMOD-CHECK-NUMBER    TO WS-HOLD-LAST-WRITE-CHECK
        // MOVE TMMOD-DEPARTMENT-CODE TO WS-HOLD-LAST-WRITE-DEPT
        // MOVE TMMOD-TAX-TYPE        TO WS-HOLD-LAST-WRITE-TAX-TYPE
        // MOVE TMMOD-TIN-IND         TO WS-HOLD-LAST-WRITE-TIN
        // INITIALIZE TEN99-M01W-MASTER-OUT-DTL
        // .
        // ===== End COBOL =====
        
        try {
            writeTen99m01wmasteroutfile(state, state.ten99M01rMasterInDtl);
            if ("true".equals(state.mastOutHead) || "true".equals(state.mastOutTrailer)) {
                // CONTINUE
            } else {
                if ("true".equals(state.deleteOutput)) {
                    state.wsMastDeleteOutCnt = state.wsMastDeleteOutCnt + state.wsOneLit;
                    state.wsMastDeleteOutAmt = state.wsMastDeleteOutAmt.add(state.tmmod1099Amount);
                } else {
                    state.wsMastOutCnt = state.wsMastOutCnt + state.wsOneLit;
                    state.wsMastOutAmt = state.wsMastOutAmt.add(state.tmmod1099Amount);
                }
            }
            state.wsHoldLastWriteCheck = state.tmmodCheckNumber;
            state.wsHoldLastWriteDept = state.tmmodDepartmentCode;
            state.wsHoldLastWriteTaxType = state.tmmodTaxType;
            state.wsHoldLastWriteTin = state.tmmodTinInd;
            initialization(state);
        } catch (Exception e) {
            throw new RuntimeException("Error in writeMasterOut: " + e.getMessage(), e);
        }
    }


    private void writeDuplicateTransOut(ProgramState state) {
        // ===== Original COBOL: 8100-WRITE-DUPLICATE-TRANS-OUT =====
        // 105300*=================================================================10530001
        // 105400*  THIS PARAGRAPH WRITE THE DUPLICATEED TRANSACTION FILE          10540001
        // 105500*=================================================================10550001
        // WRITE TEN99-T01W-DUPLICATE-TRAN-RCD
        // FROM TEN99-T01W-DUPLICATE-DTL
        // ADD WS-ONE-LIT TO WS-DUPLICATE-OUT-CNT
        // ADD TTDD-1099-AMOUNT TO WS-DUPLICATE-AMT
        // INITIALIZE TEN99-T01W-DUPLICATE-DTL
        // .
        // 106300*8999-WRITE-SYSOUT.                                               10630001
        // 106400*=================================================================10640001
        // 106500*  THIS PARAGRAPH WRITE THE SYSOUT MESSAGES                       10650001
        // 106600*=================================================================10660001
        // 106700*++INCLUDE C2INZ002                                               10670001
        // COPY C2INZ002.
        // EJECT
        // ===== End COBOL =====
        
        try {
            writeDuplicateTransOut(state);
            state.wsDuplicateOutCnt = state.wsDuplicateOutCnt + state.wsOneLit;
            state.wsDuplicateAmt = state.wsDuplicateAmt.add(state.tttd1099Amount);
            state.ten99T01wDuplicateDtl = "";
        } catch (Exception e) {
            throw new RuntimeException("Error in writeDuplicateTransOut: " + e.getMessage(), e);
        }
    }


    private void endOfJob(ProgramState state) {
        // ===== Original COBOL: 9000-END-OF-JOB =====
        // 107300*=================================================================10730001
        // 107400*  THIS PARAGRAPH PERFORMS ALL EOJ PROCESSING                     10740001
        // 107500*=================================================================10750001
        // PERFORM 9010-WRAP-UP
        // PERFORM 9020-CREATE-TRAILER
        // PERFORM 9999-CLOSE-FILES
        // .
        // ===== End COBOL =====
        
        try {
            wrapUp(state);
            createTrailer(state);
            // TODO: unknown method closeFiles
        } catch (Exception e) {
            throw new RuntimeException("Error in endOfJob: " + e.getMessage(), e);
        }
    }


    private void wrapUp(ProgramState state) {
        // ===== Original COBOL: 9010-WRAP-UP =====
        // 108300*=================================================================10830001
        // 108400*  THIS PARAGRAPH WRITE FINAL MESSAGES TO THE SYSOUT              10840001
        // 108500*=================================================================10850001
        // 108600*  THE FIRST SIX MESSAGES DISPLAY THE RECORD COUNTS ON THE SYSOUT 10860001
        // 108700*-----------------------------------------------------------------10870001
        // MOVE WS-EOJ-MSG-6 TO CC-E01W-DISPLAY-RCD
        // PERFORM 8999-WRITE-SYSOUT
        // MOVE TTTT-TRAIL-CNT TO SCR-COUNT
        // MOVE SYSOUT-COUNT-RCD TO CC-E01W-DISPLAY-RCD
        // PERFORM 8999-WRITE-SYSOUT
        // MOVE WS-EOJ-MSG-1 TO CC-E01W-DISPLAY-RCD
        // PERFORM 8999-WRITE-SYSOUT
        // MOVE WS-TRANS-IN-CNT TO SCR-COUNT
        // MOVE SYSOUT-COUNT-RCD TO CC-E01W-DISPLAY-RCD
        // PERFORM 8999-WRITE-SYSOUT
        // MOVE WS-EOJ-MSG-2 TO CC-E01W-DISPLAY-RCD
        // PERFORM 8999-WRITE-SYSOUT
        // MOVE WS-MASTER-IN-CNT TO SCR-COUNT
        // MOVE SYSOUT-COUNT-RCD TO CC-E01W-DISPLAY-RCD
        // PERFORM 8999-WRITE-SYSOUT
        // MOVE WS-EOJ-MSG-3 TO CC-E01W-DISPLAY-RCD
        // PERFORM 8999-WRITE-SYSOUT
        // MOVE WS-UPDATES-IN-CNT TO SCR-COUNT
        // MOVE SYSOUT-COUNT-RCD TO CC-E01W-DISPLAY-RCD
        // PERFORM 8999-WRITE-SYSOUT
        // MOVE WS-EOJ-MSG-4 TO CC-E01W-DISPLAY-RCD
        // PERFORM 8999-WRITE-SYSOUT
        // MOVE WS-MAST-OUT-CNT TO SCR-COUNT
        // MOVE SYSOUT-COUNT-RCD TO CC-E01W-DISPLAY-RCD
        // PERFORM 8999-WRITE-SYSOUT
        // MOVE WS-EOJ-MSG-5 TO CC-E01W-DISPLAY-RCD
        // PERFORM 8999-WRITE-SYSOUT
        // MOVE WS-DUPLICATE-OUT-CNT TO SCR-COUNT
        // MOVE SYSOUT-COUNT-RCD TO CC-E01W-DISPLAY-RCD
        // PERFORM 8999-WRITE-SYSOUT
        // 111800*=================================================================11180001
        // 111900*  THE NEXT SIX MESSAGES DISPLAY THE TOTAL AMOUNTS ON THE SYSOUT  11190001
        // 112000*-----------------------------------------------------------------11200001
        // MOVE WS-TRANS-AMT TO WS-MSG-7-CNT
        // MOVE WS-EOJ-MSG-7 TO CC-E01W-DISPLAY-RCD
        // PERFORM 8999-WRITE-SYSOUT
        // MOVE WS-MAST-IN-AMT TO WS-MSG-8-CNT
        // MOVE WS-EOJ-MSG-8 TO CC-E01W-DISPLAY-RCD
        // PERFORM 8999-WRITE-SYSOUT
        // MOVE WS-MAST-UP-AMT TO WS-MSG-9-CNT
        // MOVE WS-EOJ-MSG-9 TO CC-E01W-DISPLAY-RCD
        // PERFORM 8999-WRITE-SYSOUT
        // MOVE WS-MAST-OUT-AMT TO WS-MSG-10-CNT
        // MOVE WS-EOJ-MSG-10 TO CC-E01W-DISPLAY-RCD
        // PERFORM 8999-WRITE-SYSOUT
        // MOVE WS-DUPLICATE-AMT TO WS-MSG-11-CNT
        // MOVE WS-EOJ-MSG-11 TO CC-E01W-DISPLAY-RCD
        // PERFORM 8999-WRITE-SYSOUT
        // MOVE WS-MAST-DELETE-IN-AMT TO WS-MSG-12-CNT
        // MOVE WS-EOJ-MSG-12 TO CC-E01W-DISPLAY-RCD
        // PERFORM 8999-WRITE-SYSOUT
        // MOVE WS-MAST-DELETE-UP-AMT TO WS-MSG-13-CNT
        // MOVE WS-EOJ-MSG-13 TO CC-E01W-DISPLAY-RCD
        // PERFORM 8999-WRITE-SYSOUT
        // MOVE WS-MAST-DELETE-OUT-AMT TO WS-MSG-14-CNT
        // MOVE WS-EOJ-MSG-14 TO CC-E01W-DISPLAY-RCD
        // PERFORM 8999-WRITE-SYSOUT
        // 114500*++INCLUDE C2INZ003                                               11450001
        // COPY C2INZ003.
        // EJECT
        // ===== End COBOL =====
        
        try {
            state.ccE01wDisplayRcd = state.wsEojMsg6;
            // TODO: unknown method writeSysout
            state.scrCount = String.valueOf(state.ttttTrailCnt);
            state.ccE01wDisplayRcd = state.sysoutCountRcd;
            // TODO: unknown method writeSysout
            state.ccE01wDisplayRcd = state.wsEojMsg1;
            // TODO: unknown method writeSysout
            state.scrCount = String.valueOf(state.wsTransInCnt);
            state.ccE01wDisplayRcd = state.sysoutCountRcd;
            // TODO: unknown method writeSysout
            state.ccE01wDisplayRcd = state.wsEojMsg2;
            // TODO: unknown method writeSysout
            state.scrCount = String.valueOf(state.wsMasterInCnt);
            state.ccE01wDisplayRcd = state.sysoutCountRcd;
            // TODO: unknown method writeSysout
            state.ccE01wDisplayRcd = state.wsEojMsg3;
            // TODO: unknown method writeSysout
            state.scrCount = String.valueOf(state.wsUpdatesInCnt);
            state.ccE01wDisplayRcd = state.sysoutCountRcd;
            initializeSysout(state);
            state.ccE01wDisplayRcd = state.wsEojMsg4;
            initializeSysout(state);
            state.scrCount = String.valueOf(state.wsMastOutCnt);
            state.ccE01wDisplayRcd = state.sysoutCountRcd;
            initializeSysout(state);
            state.ccE01wDisplayRcd = state.wsEojMsg5;
            initializeSysout(state);
            state.scrCount = String.valueOf(state.wsDuplicateOutCnt);
            state.ccE01wDisplayRcd = state.sysoutCountRcd;
            initializeSysout(state);
            state.wsMsg7Cnt = state.wsTransAmt;
            state.ccE01wDisplayRcd = state.wsEojMsg7;
            initializeSysout(state);
            state.wsMsg8Cnt = state.wsMastInAmt;
            state.ccE01wDisplayRcd = state.wsEojMsg8;
            initializeSysout(state);
            state.wsMsg9Cnt = state.wsMastUpAmt;
            state.ccE01wDisplayRcd = state.wsEojMsg9;
            initializeSysout(state);
            state.wsMsg10Cnt = state.wsMastOutAmt;
            state.ccE01wDisplayRcd = state.wsEojMsg10;
            initializeSysout(state);
            state.wsMsg11Cnt = state.wsDuplicateAmt;
            state.ccE01wDisplayRcd = state.wsEojMsg11;
            initializeSysout(state);
            state.wsMsg12Cnt = state.wsMastDeleteInAmt;
            state.ccE01wDisplayRcd = state.wsEojMsg12;
            initializeSysout(state);
            state.wsMsg13Cnt = state.wsMastDeleteUpAmt;
            state.ccE01wDisplayRcd = state.wsEojMsg13;
            initializeSysout(state);
            state.wsMsg14Cnt = state.wsMastDeleteOutAmt;
            state.ccE01wDisplayRcd = state.wsEojMsg14;
            initializeSysout(state);
        } catch (Exception e) {
            throw new RuntimeException("Error in wrapUp: " + e.getMessage(), e);
        }
    }


    private void createTrailer(ProgramState state) {
        // ===== Original COBOL: 9020-CREATE-TRAILER =====
        // 115000*=================================================================11500001
        // 115100*  THIS PARAGRAPH CREATES THE TRAILER FOR THE MASTER FILE         11510001
        // 115200*=================================================================11520001
        // MOVE HIGH-VALUES TO TMMOT-TRAIL-SOURCE
        // MOVE WS-MAST-OUT-AMT TO TMMOT-NON-DELETE-TRAIL-AMT
        // MOVE WS-MAST-OUT-CNT TO TMMOT-NON-DELETE-TRAIL-CNT
        // MOVE WS-MAST-DELETE-OUT-AMT TO TMMOT-DELETE-TRAIL-AMT
        // MOVE WS-MAST-DELETE-OUT-CNT TO TMMOT-DELETE-TRAIL-CNT
        // PERFORM 8000-WRITE-MASTER-OUT
        // .
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
        // CLOSE TEN99-M01R-MASTER-IN-FILE
        // CLOSE TEN99-M01W-MASTER-OUT-FILE
        // CLOSE TEN99-T01W-DUPLICATE-TRAN-FILE
        // CLOSE TEN99-R01R-CONTROL-FILE
        // CLOSE TEN99-T01R-TRANSACTION-FILE
        // CLOSE TEN99-T02R-MASTER-UPDATE-FILE
        // .
        // END PROGRAM CCAC6330.
        // 
        // ===== End COBOL =====
        
        try {
            state.tmmitTrailSource = "\uFFFF";
            state.tmmitNonDeleteTrailAmt = state.wsMastOutAmt;
            state.tmmitNonDeleteTrailCnt = state.wsMastOutCnt;
            state.tmmitDeleteTrailAmt = state.wsMastDeleteOutAmt;
            state.tmmitDeleteTrailCnt = state.wsMastDeleteOutCnt;
            writeMasterOut(state);
        } catch (Exception e) {
            throw new RuntimeException("Error in createTrailer: " + e.getMessage(), e);
        }
    }

}