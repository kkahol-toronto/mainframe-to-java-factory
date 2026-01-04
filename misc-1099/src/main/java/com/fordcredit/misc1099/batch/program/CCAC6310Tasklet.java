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
 * Complete Tasklet for COBOL program CCAC6310
 * Generated with full business logic translation
 */
public class CCAC6310Tasklet implements Tasklet {

    static class ProgramState {
        BufferedReader ten99T01rMiscTransFileReader;
        boolean ten99T01rMiscTransFileEof = false;
        String ten99T01rMiscTransFileLine;
        BufferedReader ten99T03rBccwFileReader;
        boolean ten99T03rBccwFileEof = false;
        String ten99T03rBccwFileLine;
        BufferedReader ten99T04rDeftFileReader;
        boolean ten99T04rDeftFileEof = false;
        String ten99T04rDeftFileLine;
        BufferedReader ten99T05rRejCycleFileReader;
        boolean ten99T05rRejCycleFileEof = false;
        String ten99T05rRejCycleFileLine;
        BufferedReader wsT07r1099entryCdFileReader;
        boolean wsT07r1099entryCdFileEof = false;
        String wsT07r1099entryCdFileLine;
        BufferedWriter ten99T01wMiscTransOutFileWriter;
        boolean ten99T01wMiscTransOutFileEof = false;
        String ten99T01wMiscTransOutFileLine;
        BufferedWriter ten99T03wBccwOutFileWriter;
        boolean ten99T03wBccwOutFileEof = false;
        String ten99T03wBccwOutFileLine;
        BufferedWriter ten99T04wDeftOutFileWriter;
        boolean ten99T04wDeftOutFileEof = false;
        String ten99T04wDeftOutFileLine;
        BufferedWriter ten99T05wRejCycleOutFileWriter;
        boolean ten99T05wRejCycleOutFileEof = false;
        String ten99T05wRejCycleOutFileLine;
        BufferedReader ccR01rControlCardReader;
        boolean ccR01rControlCardEof = false;
        String ccR01rControlCardLine;

        // Processing state
        boolean endOfInput = false;
        String returnCode = "0";
        String recordsRead = "0";
        String recordsWritten = "0";

        // WORKING-STORAGE variables
        String panValet = "024CCAC6310  05/19/05";
        String pups2000 = "";
        String wsVariableArea = "";
        String wsCec = "";
        String wsTaxType = "";
        String wsT01rMiscFormRcd = "";
        String wsT01rMiscFormOperLoc = "";
        String wsT01rMiscForm1099Type = "";
        String wsT01rMiscFormSsn = "";
        String wsT01rMiscFormReference = "";
        String wsT01rMiscFormAmt = "";
        String wsT01rMiscFormPayYy = "";
        String wsT01rMiscFormName = "";
        String wsT01rMiscFormNameLast = "";
        String wsT01rMiscFormNameFi = "";
        String wsT01rMiscFormNameMi = "";
        String wsT01rMiscFormAddr1 = "";
        String wsT01rMiscFormAddr2 = "";
        String wsT01rMiscFormCity = "";
        String wsT01rMiscFormState = "";
        String wsT01rMiscFormPostcode = "";
        String wsT01rMiscFormBranch = "";
        String wsT01rMiscFormDescription = "";
        String wsT01rMiscFormAmtInd = "";
        String wsT01rMiscFormSsnInd = "";
        String wsT01rMiscFormTransCode = "";
        String wsT01rMiscFormTaxType = "";
        String wsT01rMiscFormValidTax = "3 7";
        String wsT01rMiscFormTinInd = "";
        String wsT01rMiscFormHdr = "";
        String wsT01rMiscFormHdrValue = "";
        String wsT01rMiscFormHdrName = "";
        String wsT01rMiscFormHdrDate = "";
        String wsT01rMiscFormHdrCc = "";
        String wsT01rMiscFormHdrYy = "";
        String wsT01rMiscFormHdrMm = "";
        String wsT01rMiscFormTrl = "";
        String wsT01rMiscFormTrlValue = "";
        String wsT01rMiscFormTrlCount = "";
        String wsT01rMiscFormTrlAmount = "";
        String wsT04rDeftRcd = "";
        String wsT04rDeftBusCode = "";
        String wsT04rDeftLabelCode = "";
        String wsT04rDeftBranchCode = "";
        String wsT04rDeftFiller1 = "";
        String wsT04rDeftBatchNum = "";
        String wsT04rDeftName = "";
        String wsT04rDeftAddress1 = "";
        String wsT04rDeftAddress2 = "";
        String wsT04rDeftCity = "";
        String wsT04rDeftState = "";
        String wsT04rDeftZipcode = "";
        String wsT04rDeftCompassCode = "";
        String wsT04rDeftDesc = "";
        String wsT04rDeftDisbAmt = "";
        String wsT04rDeftDisbSeq = "";
        String wsT04rDeftSsn = "";
        String wsT04rDeftProdType = "";
        String wsT04rDeftBusUnit = "";
        String wsT04rDeftOpLoc = "";
        String wsT04rDeftLineNbr = "";
        String wsT04rDeftBnkStlmtDt = "";
        String wsT04rDeftBnkStlmtCc = "";
        String wsT04rDeftBnkStlmtYy = "";
        String wsT04rDeftBnkStlmtMm = "";
        String wsT04rDeftBnkStlmtDd = "";
        String wsT04rDeftFiller2 = "";
        String wsT04rDeftTaxType = "";
        String wsT04rDeftValidTaxType = "3 7";
        String wsT04rDeftTinInd = "";
        String wsT04rDeftAConstant = "A";
        String wsT04rDeftHdr = "";
        String wsT04rDeftHdrValue = "";
        String wsT04rDeftHdrFiller1 = "";
        String wsT04rDeftHdrId = "";
        String wsT04rDeftHdrFiller2 = "";
        String wsT04rDeftHdrDate = "";
        String wsT04rDeftHdrCc = "";
        String wsT04rDeftHdrYy = "";
        String wsT04rDeftHdrMm = "";
        String wsT04rDeftHdrDd = "";
        String wsT04rDeftHdrFiller3 = "";
        String wsT04rDeftTrl = "";
        String wsT04rDeftTrlValue = "";
        String wsT04rDeftTrlFiller1 = "";
        String wsT04rDeftTrlCnt = "";
        String wsT04rDeftTrlFiller2 = "";
        String wsT04rDeftTrlAmt = "";
        String wsT04rDeftTrlFiller3 = "";
        String wsT05rRejectRecyclingRcd = "";
        String wsT05rRejectKey = "";
        String wsT05rRejectKeyOpLoc = "";
        String wsT05rRejectKeyBrDeptCd = "";
        String wsT05rRejectKeyCheckNum = "";
        String wsT05rCheckPrefix = "";
        String wsT05rCheckMonth = "";
        String wsT05rCheckSeq = "";
        String wsT05rRejectKeyCheckDt = "";
        String wsT05rRejectTaxType = "";
        String wsT05rRejectValidTaxType = "3 7";
        String wsT05rRejectTinInd = "";
        String wsT05rRejectSourceCode = "";
        String wsT05rRejectDetail = "";
        String wsT05rRejectTransDetail = "";
        String wsT05rOutputDetail = "";
        String wsT05rBranch = "";
        String wsT05rName = "";
        String wsT05rAddr1 = "";
        String wsT05rAddr2 = "";
        String wsT05rCity = "";
        String wsT05rState = "";
        String wsT05rPostcode = "";
        String wsT05rSsn = "";
        String wsT05rSsnUs = "";
        String wsT05rSsnRemainder = "";
        String wsT05rOperLoc = "";
        String wsT05rCheckNum = "";
        String wsT05rCheckDate = "";
        String wsT05rCheckDateCc = "";
        String wsT05rCheckDateYy = "";
        String wsT05rCheckDateMm = "";
        String wsT05rCheckDateDd = "";
        String wsT05rTotalAmt = "";
        String wsT05r1099Amt = "";
        String wsT05rComment = "";
        String wsT05rNbrDistRcd = "";
        String wsT05rDistRcd = "";
        String wsT05rDistCompassCode = "";
        String wsT05rDist1099Indic = "";
        String wsT05rDistAmt = "";
        String wsT05rRejectMiscDetail = "";
        String wsT05rMiscFormOperLoc = "";
        String wsT05rMiscForm1099Type = "";
        String wsT05rMiscFormSsn = "";
        String wsT05rMiscFormReference = "";
        String wsT05rMiscFormAmt = "";
        String wsT05rMiscFormPayYy = "";
        String wsT05rMiscFormName = "";
        String wsT05rMiscFormNameLast = "";
        String wsT05rMiscFormNameFi = "";
        String wsT05rMiscFormNameMi = "";
        String wsT05rMiscFormAddr1 = "";
        String wsT05rMiscFormAddr2 = "";
        String wsT05rMiscFormCity = "";
        String wsT05rMiscFormState = "";
        String wsT05rMiscFormPostcode = "";
        String wsT05rMiscFormBranch = "";
        String wsT05rMiscFormDescription = "";
        String wsT05rMiscFormAmtInd = "";
        String wsT05rMiscFormSsnInd = "";
        String wsT05rMiscFormTransCode = "";
        String wsT05rRejectRecyclingHdr = "";
        String wsT05rRejCycleHdrValue = "";
        String wsT05rRejCycleHdrId = "";
        String wsT05rRejCycleHdrDate = "";
        String wsT05rRejCycleHdrCc = "";
        String wsT05rRejCycleHdrYy = "";
        String wsT05rRejCycleHdrMm = "";
        String wsT05rRejectRecyclingTrl = "";
        String wsT05rRejCycleTrlValue = "";
        String wsT05rRejCycleTrlCount = "";
        String wsT05rRejCycleTrlAmt = "";
        String wsT07r1099entryCodeTbl = "";
        String wsT07r1099entryCodes = "";
        String wsT07r1099entryCode = "";
        String wsT07r1099entryTaxType = "";
        String wsT07r1099entryDesc = "";
        String wsT07r1099entryEffDt = "";
        String controlCardIn1 = "";
        String ccTaxYearDate1 = "";
        String ccTaxCc1 = "";
        String ccTaxYy1 = "";
        String ccTaxMm1 = "";
        String ccTaxDd1 = "";
        String wsOutputTransRcd = "";
        String wsOutputTransHdr = "";
        String wsOutputHdrValue = "";
        String wsOutputHdrId = "";
        String wsOutputHdrDate = "";
        String wsOutputHdrCc = "";
        String wsOutputHdrYy = "";
        String wsOutputHdrMm = "";
        String wsOutputTransTrl = "";
        String wsOutputTrlValue = "";
        String wsOutputTrlCnt = "";
        String wsOutputTrlAmt = "";
        String cntCounters = "";
        String wsMiscFormTransCounter = "0";
        String wsMiscFormSeqCnt = "0";
        String wsBccwTransCounter = "0";
        String wsFileRecCounter = "0";
        String wsBccwTrlCounter = "0";
        String wsDeftTransCounter = "0";
        String wsDeftTrlCounter = "0";
        String wsRejCycleTransCounter = "0";
        String wsOutputTransCnt = "0";
        String wsRejTransCnt = "0";
        String wsTotalOutputTransCnt = "0";
        String wsT03rBccwTrlCnt = "0";
        String swSwitches = "";
        String swBccwBuildComplete = "N";
        String swBccwBuildCompleteYes = "Y";
        String swDeftBuildComplete = "N";
        String swDeftBuildCompleteYes = "Y";
        String wsIndicators = "";
        String rejectIndicators = "";
        String rejectFlag = "SPACE";
        String wsUsInd = "N";
        String wsUsIndYes = "Y";
        String wsCdnInd = "N";
        String wsCdnIndYes = "Y";
        String ten99T01rMiscFormEof = "N";
        String ten99T01rMiscFormEofYes = "Y";
        String ten99T03rBccwEof = "N";
        String ten99T03rBccwEofYes = "Y";
        String ten99T04rDeftEof = "N";
        String ten99T04rDeftEofYes = "Y";
        String ten99T05rRejCycleEof = "N";
        String ten99T05rRejCycleEofYes = "Y";
        String endOfBccwDetail = "N";
        String endOfBccwDetailYes = "Y";
        String endOfBccwHeaders = "N";
        String endOfBccwHeadersYes = "Y";
        String endOfDeftDetail = "N";
        String endOfDeftDetailYes = "Y";
        String endOfDeftHeaders = "N";
        String endOfDeftHeadersYes = "Y";
        String eofControlCard = "N";
        String eofControlCardYes = "Y";
        String eof1099entryTable = "N";
        String eof1099entryTableYes = "Y";
        String errorFlag = "N";
        String foreignInd = "";
        String foreignIndNo = "N";
        String foreignIndYes = "Y";
        String wsBccwCheckNum = "";
        String wsDeftBatchNum = "";
        String wsBusUnit = "";
        String wsBusUnitCan = "6572  3798 ";
        String litLiterals = "";
        String litGeneral = "";
        String litNoControlCard = "";
        String litTransTlsDoNotAgree = "";
        String litCdn = "C";
        String litPr = "P";
        String litCanada = "";
        String litPuertoRico = "";
        String litGenDel = "";
        String litErrorFlagEqualsYes = "";
        String lit1 = "1";
        String lit3 = "3";
        String lit5 = "5";
        String lit7 = "7";
        String lit095 = "095";
        String lit048 = "048";
        String litNumOf1099Codes = "150";
        String litTw = "TW";
        String litPrStateCode = "PR";
        String litZero = "0";
        String litZeroSsn = "";
        String litA = "A";
        String litTaxMonthJan = "1";
        String litTaxMonthDec = "12";
        String litTaxMonthYearEnd = "13";
        String litGrandTotal = "";
        String litMiscFormLiterals = "";
        String litMiscForm = "";
        String litMiscFormHdr = "";
        String litMiscInputSuccess = "";
        String litForMiscellaneousInput = "";
        String litMiscFormControlTotals = "";
        String litM = "M";
        String litRejectRecycleLiterals = "";
        String litRejCycleSuccess = "";
        String litForRejectCycleInput = "";
        String litRejCycleControlTotals = "";
        String litUnkwnSourceCodeType = "";
        String litBccwLiterals = "";
        String litBccw = "";
        String litBccwSuccess = "";
        String litForBccwInput = "";
        String litBccwControlTotals = "";
        String litBccwFile = "";
        String litDeftLiterals = "";
        String litDeft = "";
        String litDeftSuccess = "";
        String litForDeftInput = "";
        String litDeftControlTotals = "";
        String litDeftFile = "";
        String lit1099entryLiterals = "";
        String litIss = "";
        String litIncorrect1099entRcdType = "";
        String litNo1099entryTable = "";
        String litDistAmountErr2 = "";
        String litOutputFileLiterals = "";
        String litTransFile = "";
        String litRejectFile = "";
        String litTen99RejectTrans = "";
        String msgNoData = "";
        String msgLiteral = "";
        String msgNoDataFileName = "";
        String msgIncorrectDate = "";
        String msgFileName = "";
        String msgTrailerCountDisplay = "";
        String msgLitFileCount = "";
        String msgFileCount = "";
        String msgLitTrailerCount = "";
        String msgTrailerCount = "";
        String msgMiscFormSummary = "";
        String msgLitMiscFileCount = "";
        String msgMiscFileCount = "";
        String msgLitMiscFileAmt = "";
        String msgMiscFileAmount = "";
        String msgTrailerDollarDisplay = "";
        String msgLitFileAmount = "";
        String msgFileAmount = "";
        String msgLitTrailerAmount = "";
        String msgTrailerAmount = "";
        String msgControlTotals = "";
        String msgNumRcds = "";
        String msgLitWrittenTo = "";
        String msgOutputFileName = "SPACE";
        String msgLitAmtOfRcd = "";
        String msgAmount = "";
        String msgBadFile = "";
        String msgBadFileMsg = "";
        String msgFileId = "SPACE";
        String accAccumulators = "";
        String wsMiscFormTransAmtAccum = "0";
        String wsBccwTransAmtAccum = "0";
        String wsFileAmtAccum = "0";
        String wsBccwTrlAmtAccum = "0";
        String wsDeftTransAmtAccum = "0";
        String wsDeftTrlAmtAccum = "0";
        String wsRejCycleTransAmtAccum = "0";
        String wsOutputTransAmt = "0";
        String wsRejTransAmt = "0";
        String wsTotalOutputTransAmt = "0";
    }

    @Override
    public RepeatStatus execute(StepContribution contribution, ChunkContext chunkContext) {
        ProgramState state = new ProgramState();
        try {
            mainline(state);
        } catch (Exception e) {
            throw new RuntimeException("Error executing CCAC6310", e);
        }
        return RepeatStatus.FINISHED;
    }

    private void openFiles(ProgramState state) {
        try {
            Path testDir = Paths.get("../work/mainframe_clean/testcases", "CCAC6310");
            Path ten99T01rMiscTransFilePath = testDir.resolve("input/ten99T01rMiscTransFile.txt");
            if (Files.exists(ten99T01rMiscTransFilePath)) state.ten99T01rMiscTransFileReader = Files.newBufferedReader(ten99T01rMiscTransFilePath);
            Path ten99T03rBccwFilePath = testDir.resolve("input/ten99T03rBccwFile.txt");
            if (Files.exists(ten99T03rBccwFilePath)) state.ten99T03rBccwFileReader = Files.newBufferedReader(ten99T03rBccwFilePath);
            Path ten99T04rDeftFilePath = testDir.resolve("input/ten99T04rDeftFile.txt");
            if (Files.exists(ten99T04rDeftFilePath)) state.ten99T04rDeftFileReader = Files.newBufferedReader(ten99T04rDeftFilePath);
            Path ten99T05rRejCycleFilePath = testDir.resolve("input/ten99T05rRejCycleFile.txt");
            if (Files.exists(ten99T05rRejCycleFilePath)) state.ten99T05rRejCycleFileReader = Files.newBufferedReader(ten99T05rRejCycleFilePath);
            Path wsT07r1099entryCdFilePath = testDir.resolve("input/wsT07r1099entryCdFile.txt");
            if (Files.exists(wsT07r1099entryCdFilePath)) state.wsT07r1099entryCdFileReader = Files.newBufferedReader(wsT07r1099entryCdFilePath);
            Path ten99T01wMiscTransOutFilePath = testDir.resolve("output/ten99T01wMiscTransOutFile_out.txt");
            Files.createDirectories(ten99T01wMiscTransOutFilePath.getParent());
            state.ten99T01wMiscTransOutFileWriter = Files.newBufferedWriter(ten99T01wMiscTransOutFilePath);
            Path ten99T03wBccwOutFilePath = testDir.resolve("output/ten99T03wBccwOutFile_out.txt");
            Files.createDirectories(ten99T03wBccwOutFilePath.getParent());
            state.ten99T03wBccwOutFileWriter = Files.newBufferedWriter(ten99T03wBccwOutFilePath);
            Path ten99T04wDeftOutFilePath = testDir.resolve("output/ten99T04wDeftOutFile_out.txt");
            Files.createDirectories(ten99T04wDeftOutFilePath.getParent());
            state.ten99T04wDeftOutFileWriter = Files.newBufferedWriter(ten99T04wDeftOutFilePath);
            Path ten99T05wRejCycleOutFilePath = testDir.resolve("output/ten99T05wRejCycleOutFile_out.txt");
            Files.createDirectories(ten99T05wRejCycleOutFilePath.getParent());
            state.ten99T05wRejCycleOutFileWriter = Files.newBufferedWriter(ten99T05wRejCycleOutFilePath);
            Path ccR01rControlCardPath = testDir.resolve("input/control.txt");
            if (Files.exists(ccR01rControlCardPath)) state.ccR01rControlCardReader = Files.newBufferedReader(ccR01rControlCardPath);
        } catch (Exception e) { throw new RuntimeException("Open files failed", e); }
    }

    private void closeFiles(ProgramState state) {
        try {
            if (state.ten99T01rMiscTransFileReader != null) state.ten99T01rMiscTransFileReader.close();
            if (state.ten99T03rBccwFileReader != null) state.ten99T03rBccwFileReader.close();
            if (state.ten99T04rDeftFileReader != null) state.ten99T04rDeftFileReader.close();
            if (state.ten99T05rRejCycleFileReader != null) state.ten99T05rRejCycleFileReader.close();
            if (state.wsT07r1099entryCdFileReader != null) state.wsT07r1099entryCdFileReader.close();
            if (state.ten99T01wMiscTransOutFileWriter != null) state.ten99T01wMiscTransOutFileWriter.close();
            if (state.ten99T03wBccwOutFileWriter != null) state.ten99T03wBccwOutFileWriter.close();
            if (state.ten99T04wDeftOutFileWriter != null) state.ten99T04wDeftOutFileWriter.close();
            if (state.ten99T05wRejCycleOutFileWriter != null) state.ten99T05wRejCycleOutFileWriter.close();
            if (state.ccR01rControlCardReader != null) state.ccR01rControlCardReader.close();
        } catch (Exception e) { throw new RuntimeException("Close files failed", e); }
    }

    private void readTen99T01rMiscTransFile(ProgramState state) {
        try {
            if (state.ten99T01rMiscTransFileReader == null) { state.ten99T01rMiscTransFileEof = true; return; }
            state.ten99T01rMiscTransFileLine = state.ten99T01rMiscTransFileReader.readLine();
            if (state.ten99T01rMiscTransFileLine == null) state.ten99T01rMiscTransFileEof = true;
        } catch (Exception e) { throw new RuntimeException("Read error", e); }
    }

    private void readTen99T03rBccwFile(ProgramState state) {
        try {
            if (state.ten99T03rBccwFileReader == null) { state.ten99T03rBccwFileEof = true; return; }
            state.ten99T03rBccwFileLine = state.ten99T03rBccwFileReader.readLine();
            if (state.ten99T03rBccwFileLine == null) state.ten99T03rBccwFileEof = true;
        } catch (Exception e) { throw new RuntimeException("Read error", e); }
    }

    private void readTen99T04rDeftFile(ProgramState state) {
        try {
            if (state.ten99T04rDeftFileReader == null) { state.ten99T04rDeftFileEof = true; return; }
            state.ten99T04rDeftFileLine = state.ten99T04rDeftFileReader.readLine();
            if (state.ten99T04rDeftFileLine == null) state.ten99T04rDeftFileEof = true;
        } catch (Exception e) { throw new RuntimeException("Read error", e); }
    }

    private void readTen99T05rRejCycleFile(ProgramState state) {
        try {
            if (state.ten99T05rRejCycleFileReader == null) { state.ten99T05rRejCycleFileEof = true; return; }
            state.ten99T05rRejCycleFileLine = state.ten99T05rRejCycleFileReader.readLine();
            if (state.ten99T05rRejCycleFileLine == null) state.ten99T05rRejCycleFileEof = true;
        } catch (Exception e) { throw new RuntimeException("Read error", e); }
    }

    private void readWsT07r1099entryCdFile(ProgramState state) {
        try {
            if (state.wsT07r1099entryCdFileReader == null) { state.wsT07r1099entryCdFileEof = true; return; }
            state.wsT07r1099entryCdFileLine = state.wsT07r1099entryCdFileReader.readLine();
            if (state.wsT07r1099entryCdFileLine == null) state.wsT07r1099entryCdFileEof = true;
        } catch (Exception e) { throw new RuntimeException("Read error", e); }
    }

    private void readCcR01rControlCard(ProgramState state) {
        try {
            if (state.ccR01rControlCardReader == null) { state.ccR01rControlCardEof = true; return; }
            state.ccR01rControlCardLine = state.ccR01rControlCardReader.readLine();
            if (state.ccR01rControlCardLine == null) state.ccR01rControlCardEof = true;
        } catch (Exception e) { throw new RuntimeException("Read error", e); }
    }

    private void writeTen99T01wMiscTransOutFile(ProgramState state, String line) {
        try {
            if (state.ten99T01wMiscTransOutFileWriter != null && line != null) {
                state.ten99T01wMiscTransOutFileWriter.write(line);
                state.ten99T01wMiscTransOutFileWriter.newLine();
            }
        } catch (Exception e) { throw new RuntimeException("Write error", e); }
    }

    private void writeTen99T03wBccwOutFile(ProgramState state, String line) {
        try {
            if (state.ten99T03wBccwOutFileWriter != null && line != null) {
                state.ten99T03wBccwOutFileWriter.write(line);
                state.ten99T03wBccwOutFileWriter.newLine();
            }
        } catch (Exception e) { throw new RuntimeException("Write error", e); }
    }

    private void writeTen99T04wDeftOutFile(ProgramState state, String line) {
        try {
            if (state.ten99T04wDeftOutFileWriter != null && line != null) {
                state.ten99T04wDeftOutFileWriter.write(line);
                state.ten99T04wDeftOutFileWriter.newLine();
            }
        } catch (Exception e) { throw new RuntimeException("Write error", e); }
    }

    private void writeTen99T05wRejCycleOutFile(ProgramState state, String line) {
        try {
            if (state.ten99T05wRejCycleOutFileWriter != null && line != null) {
                state.ten99T05wRejCycleOutFileWriter.write(line);
                state.ten99T05wRejCycleOutFileWriter.newLine();
            }
        } catch (Exception e) { throw new RuntimeException("Write error", e); }
    }


    // ========================================
    // TRANSLATED BUSINESS LOGIC
    // ========================================

    private void mainline(ProgramState state) {
        // TODO: Translate COBOL paragraph '0000-MAINLINE'
        // Original COBOL:
        // 076300***************************************************************** 12630001
        //     PERFORM 1000-INITIALIZATION
        //     PERFORM 3000-MAIN-PROCESS
        //     PERFORM 9000-TERMINATION-ROUTINE
        //     GOBACK
        //     .
        // EJECT
        // 077000***************************************************************** 12700001
        initialization(state);
        mainProcess(state);
        terminationRoutine(state);
    }
    private void initialization(ProgramState state) {
        // TODO: Translate COBOL paragraph '1000-INITIALIZATION'
        // Original COBOL:
        // 077200***************************************************************** 12720001
        //     OPEN INPUT
        //        TEN99-T01R-MISC-TRANS-FILE
        //        TEN99-T03R-BCCW-FILE
        //        TEN99-T04R-DEFT-FILE
        //        TEN99-T05R-REJ-CYCLE-FILE
        //        WS-T07R-1099ENTRY-CD-FILE
        //        CC-R01R-CONTROL-CARD
        //     OUTPUT
        //        TEN99-T01W-MISC-TRANS-OUT-FILE
        //        TEN99-T03W-BCCW-OUT-FILE
        //        TEN99-T04W-DEFT-OUT-FILE
        //        TEN99-T05W-REJ-CYCLE-OUT-FILE
        //        CC-E01W-DISPLAY-FILE
        //     PERFORM 1130-INCLUDE-SYSOUT-DISPLAY
        //     PERFORM 7900-READ-CONTROL-CARD-IN
        //     INITIALIZE             WS-OUTPUT-TRANS-RCD
        //     .
        // 079300***************************************************************** 13310001
        includeSysoutDisplay(state);
        readControlCardIn(state);
    }
    private void includeSysoutDisplay(ProgramState state) {
        // TODO: Translate COBOL paragraph '1130-INCLUDE-SYSOUT-DISPLAY'
        // Original COBOL:
        // 079500***************************************************************** 13330001
        // 079600*    ++INCLUDE C2INZ001                                         * 13340001
        // 079700*      COMMON INITIAL SYSOUT DISPLAYS                           * 13350001
        // 079800***************************************************************** 13360001
        // COPY C2INZ001.
        // EJECT
        // 080200***************************************************************** 13400001

    }
    private void mainProcess(ProgramState state) {
        // TODO: Translate COBOL paragraph '3000-MAIN-PROCESS'
        // Original COBOL:
        // 080400***************************************************************** 13420001
        // 080500*      MAIN PROCESSING ROUTINE                                  * 13430001
        // 080600***************************************************************** 13440001
        //     PERFORM 7910-READ-1099ENTRY-TABLE
        //        VARYING CODE-INDEX FROM 1 BY 1
        //        UNTIL CODE-INDEX GREATER THAN LIT-NUM-OF-1099-CODES
        // 081000***************************************************************** 13480001
        // 081100*      DETERMINE THE PROCESSING SCHEDULE BASED ON TAX MONTH     * 13490001
        // 081200*      VERIFY EACH INPUT HEADER RECORD TO ENSURE CORRECT FILES  * 13500001
        // 081300*      PROCESS THE DETAIL AND TRAILER FOR EACH FILE             * 13510001
        // 081400***************************************************************** 13520001
        //     EVALUATE TRUE
        //     WHEN CC-TAX-MM1 = LIT-TAX-MONTH-JAN
        //        PERFORM 3002-JAN-MONTH-PROCESSING
        //     WHEN CC-TAX-MM1 > LIT-TAX-MONTH-JAN AND
        //                     < LIT-TAX-MONTH-YEAR-END
        //        PERFORM 3004-FEB-DEC-MONTH-PROCESSING
        //     WHEN CC-TAX-MM1 > LIT-TAX-MONTH-DEC
        //        PERFORM 3006-YEAR-END-PROCESSING
        //     END-EVALUATE
        //     .
        //     EJECT
        // 082800***************************************************************** 13660001
        read1099entryTable(state);
        janMonthProcessing(state);
        febDecMonthProcessing(state);
        yearEndProcessing(state);
    }
    private void janMonthProcessing(ProgramState state) {
        // TODO: Translate COBOL paragraph '3002-JAN-MONTH-PROCESSING'
        // Original COBOL:
        // 083000***************************************************************** 13680001
        // 083100*   PROCESSING SCHEDULE FOR JANUARY                             * 13690001
        // 083200***************************************************************** 13700001
        //        PERFORM 4000-MISC-FORM-VERIFY-PROCESS
        //        PERFORM 4200-BCCW-VERIFY-PROCESS
        //        PERFORM 4300-DEFT-VERIFY-PROCESS
        //        IF ERROR-FLAG = "Y"
        //           MOVE LIT-ERROR-FLAG-EQUALS-YES TO CC-E01W-DISPLAY-RCD
        //           PERFORM 8999-WRITE-SYSOUT
        //           PERFORM 9998-COREDUMP
        //        END-IF
        //        PERFORM 5000-MISC-FORM-PROCESS
        //        PERFORM 5200-BCCW-PROCESS
        //        PERFORM 5400-DEFT-PROCESS
        //        .
        // EJECT
        // 084600***************************************************************** 13840001
        miscFormVerifyProcess(state);
        bccwVerifyProcess(state);
        deftVerifyProcess(state);
        writeSysout(state);
        coredump(state);
        miscFormProcess(state);
        bccwProcess(state);
        deftProcess(state);
    }
    private void febDecMonthProcessing(ProgramState state) {
        // TODO: Translate COBOL paragraph '3004-FEB-DEC-MONTH-PROCESSING'
        // Original COBOL:
        // 084800***************************************************************** 13860001
        // 084900*   PROCESSING SCHEDULE FOR FEBRUARY THROUGH DECEMBER           * 13870001
        // 085000***************************************************************** 13880001
        //        PERFORM 4000-MISC-FORM-VERIFY-PROCESS
        //        PERFORM 4200-BCCW-VERIFY-PROCESS
        //        PERFORM 4300-DEFT-VERIFY-PROCESS
        //        PERFORM 4500-REJ-CYCLE-VERIFY-PROCESS
        //        IF ERROR-FLAG = "Y"
        //           MOVE LIT-ERROR-FLAG-EQUALS-YES TO CC-E01W-DISPLAY-RCD
        //           PERFORM 8999-WRITE-SYSOUT
        //           PERFORM 9998-COREDUMP
        //        END-IF
        //        PERFORM 5000-MISC-FORM-PROCESS
        //        PERFORM 5200-BCCW-PROCESS
        //        PERFORM 5400-DEFT-PROCESS
        //        PERFORM 5500-REJ-CYCLE-PROCESS
        //        .
        // EJECT
        // 086700***************************************************************** 14050001
        miscFormVerifyProcess(state);
        bccwVerifyProcess(state);
        deftVerifyProcess(state);
        rejCycleVerifyProcess(state);
        writeSysout(state);
        coredump(state);
        miscFormProcess(state);
        bccwProcess(state);
        deftProcess(state);
        rejCycleProcess(state);
    }
    private void yearEndProcessing(ProgramState state) {
        // TODO: Translate COBOL paragraph '3006-YEAR-END-PROCESSING'
        // Original COBOL:
        // 086900***************************************************************** 14070001
        // 087000*   PROCESSING SCHEDULE FOR YEAR-END                            * 14080001
        // 087100***************************************************************** 14090001
        //        PERFORM 4000-MISC-FORM-VERIFY-PROCESS
        //        PERFORM 4500-REJ-CYCLE-VERIFY-PROCESS
        //        IF ERROR-FLAG = "Y"
        //           MOVE LIT-ERROR-FLAG-EQUALS-YES TO CC-E01W-DISPLAY-RCD
        //           PERFORM 8999-WRITE-SYSOUT
        //           PERFORM 9998-COREDUMP
        //        END-IF
        //        PERFORM 5000-MISC-FORM-PROCESS
        //        PERFORM 5500-REJ-CYCLE-PROCESS
        //        .
        // EJECT
        // 088300***************************************************************** 14210001
        miscFormVerifyProcess(state);
        rejCycleVerifyProcess(state);
        writeSysout(state);
        coredump(state);
        miscFormProcess(state);
        rejCycleProcess(state);
    }
    private void miscFormVerifyProcess(ProgramState state) {
        // TODO: Translate COBOL paragraph '4000-MISC-FORM-VERIFY-PROCESS'
        // Original COBOL:
        // 088500***************************************************************** 14230001
        // 088600*   TEST MISCELLANEOUS FORM 20071 INPUT HEADER FOR BAD HEADER   * 14240001
        // 088700*   OR BAD CONTROL DATE.                                        * 14250001
        // 088800***************************************************************** 14260001
        //     PERFORM 7000-MISC-FORM-READ-RCD
        //     IF TEN99-T01R-MISC-FORM-EOF-YES
        //        MOVE LIT-MISC-FORM TO MSG-NO-DATA-FILE-NAME
        //        MOVE MSG-NO-DATA   TO CC-E01W-DISPLAY-RCD
        //        PERFORM 8999-WRITE-SYSOUT
        //        MOVE "Y" TO ERROR-FLAG
        //     ELSE
        //        IF WS-T01R-MISC-FORM-HDR-VALUE = LOW-VALUES AND
        //           WS-T01R-MISC-FORM-HDR-NAME  = LIT-MISC-FORM-HDR
        //           IF WS-T01R-MISC-FORM-HDR-YY = CC-TAX-YY1 AND
        //              WS-T01R-MISC-FORM-HDR-MM = CC-TAX-MM1
        //              MOVE WS-T01R-MISC-FORM-RCD TO WS-OUTPUT-TRANS-RCD
        //              PERFORM 8010-WRITE-MISC-TRANS-RCD
        //           ELSE
        //              MOVE LIT-MISC-FORM      TO MSG-FILE-NAME
        //              MOVE MSG-INCORRECT-DATE TO CC-E01W-DISPLAY-RCD
        //              PERFORM 8999-WRITE-SYSOUT
        //              MOVE "Y" TO ERROR-FLAG
        //           END-IF
        //        ELSE
        //           MOVE LIT-MISC-FORM TO MSG-FILE-ID
        //           MOVE MSG-BAD-FILE  TO CC-E01W-DISPLAY-RCD
        //           PERFORM 8999-WRITE-SYSOUT
        //           MOVE "Y" TO ERROR-FLAG
        //        END-IF
        //     END-IF
        //     .
        //     EJECT
        // 091700***************************************************************** 14540001
        // 091800***************************************************************** 14550001
        miscFormReadRcd(state);
        writeSysout(state);
        writeMiscTransRcd(state);
        writeSysout(state);
        writeSysout(state);
    }
    private void bccwVerifyProcess(ProgramState state) {
        // TODO: Translate COBOL paragraph '4200-BCCW-VERIFY-PROCESS'
        // Original COBOL:
        // 092000***************************************************************** 14570001
        // 092100*   TEST B&CCW FILE FOR NO DATA OR NO HEADER                    * 14580001
        // 092200***************************************************************** 14590001
        //     PERFORM 7200-BCCW-READ-RCD
        //     IF TEN99-T03R-BCCW-EOF-YES
        //        MOVE LIT-BCCW    TO MSG-NO-DATA-FILE-NAME
        //        MOVE MSG-NO-DATA TO CC-E01W-DISPLAY-RCD
        //        MOVE "Y"         TO ERROR-FLAG
        //        PERFORM 8999-WRITE-SYSOUT
        //     ELSE
        //        PERFORM 4210-BCCW-VERIFY
        //        PERFORM 7200-BCCW-READ-RCD
        //     END-IF
        //     .
        // 093400***************************************************************** 14730001
        bccwReadRcd(state);
        writeSysout(state);
        bccwVerify(state);
        bccwReadRcd(state);
    }
    private void bccwVerify(ProgramState state) {
        // TODO: Translate COBOL paragraph '4210-BCCW-VERIFY'
        // Original COBOL:
        // 093600***************************************************************** 14750001
        // 093700*   TEST THE HEADERS FOR NO DATA OR BAD CONTROL DATE            * 14760001
        // 093800*   NOTE:THERE ARE MULTIPLE HEADERS & TRAILERS ON B&CCW INPUT   * 14760001
        // 093900*        FILE. THE HEADERS & TRAILERS ARE DISPERSED THROUGHOUT  * 14760001
        // 094000*        THE FILE.                                              * 14760001
        // 094100***************************************************************** 14780001
        //     IF TEN99-FILE-ID-HDR      = LIT-BCCW-FILE
        //        IF TEN99-YY-HDR        = CC-TAX-YY1 AND
        //           TEN99-CC-HDR        = CC-TAX-CC1 AND
        //           TEN99-MM-HDR        = CC-TAX-MM1
        //           MOVE TEN99-RECORD-HDR TO WS-OUTPUT-TRANS-RCD
        //           INITIALIZE TEN99-RECORD-HDR
        //        ELSE
        //           MOVE LIT-BCCW           TO MSG-FILE-NAME
        //           MOVE MSG-INCORRECT-DATE TO CC-E01W-DISPLAY-RCD
        //           MOVE "Y"                TO ERROR-FLAG
        //           PERFORM 8999-WRITE-SYSOUT
        //           MOVE TEN99-RECORD-HDR TO WS-OUTPUT-TRANS-RCD
        //        END-IF
        //     ELSE
        //        MOVE LIT-BCCW     TO MSG-FILE-ID
        //        MOVE MSG-BAD-FILE TO CC-E01W-DISPLAY-RCD
        //        MOVE "Y"          TO ERROR-FLAG
        //        PERFORM 8999-WRITE-SYSOUT
        //        MOVE TEN99-RECORD-HDR TO WS-OUTPUT-TRANS-RCD
        //     END-IF
        //     PERFORM 8020-WRITE-BCCW-TRANS-RCD
        //     .
        // 096400***************************************************************** 14990001
        writeSysout(state);
        writeSysout(state);
        writeBccwTransRcd(state);
    }
    private void deftVerifyProcess(ProgramState state) {
        // TODO: Translate COBOL paragraph '4300-DEFT-VERIFY-PROCESS'
        // Original COBOL:
        // 096600***************************************************************** 15010001
        // 096700*   TEST DEFT  FILE FOR NO DATA OR NO HEADER                    * 15020001
        // 096800***************************************************************** 15030001
        //     PERFORM 7300-DEFT-READ-RCD
        //     IF TEN99-T04R-DEFT-EOF-YES
        //        MOVE LIT-DEFT    TO MSG-NO-DATA-FILE-NAME
        //        MOVE MSG-NO-DATA TO CC-E01W-DISPLAY-RCD
        //        MOVE "Y"         TO ERROR-FLAG
        //        PERFORM 8999-WRITE-SYSOUT
        //     ELSE
        //        PERFORM 4310-DEFT-VERIFY
        //        PERFORM 7300-DEFT-READ-RCD
        //     END-IF
        //     .
        // 098000***************************************************************** 15170001
        deftReadRcd(state);
        writeSysout(state);
        deftVerify(state);
        deftReadRcd(state);
    }
    private void deftVerify(ProgramState state) {
        // TODO: Translate COBOL paragraph '4310-DEFT-VERIFY'
        // Original COBOL:
        // 098200***************************************************************** 15190001
        // 098300*   TEST THE HEADER FOR NO DATA OR BAD CONTROL DATE             * 15200001
        // 098400***************************************************************** 15210001
        //     IF WS-T04R-DEFT-HDR-ID    = LIT-DEFT-FILE
        //        IF WS-T04R-DEFT-HDR-YY = CC-TAX-YY1 AND
        //           WS-T04R-DEFT-HDR-CC = CC-TAX-CC1 AND
        //           WS-T04R-DEFT-HDR-MM = CC-TAX-MM1
        //           MOVE WS-T04R-DEFT-HDR TO WS-OUTPUT-TRANS-RCD
        //           PERFORM 8040-WRITE-DEFT-TRANS-RCD
        //        ELSE
        //           MOVE LIT-DEFT           TO MSG-FILE-NAME
        //           MOVE MSG-INCORRECT-DATE TO CC-E01W-DISPLAY-RCD
        //           MOVE "Y"                TO ERROR-FLAG
        //           PERFORM 8999-WRITE-SYSOUT
        //        END-IF
        //     ELSE
        //        MOVE LIT-DEFT              TO MSG-FILE-ID
        //        MOVE MSG-BAD-FILE          TO CC-E01W-DISPLAY-RCD
        //        MOVE "Y"                   TO ERROR-FLAG
        //        PERFORM 8999-WRITE-SYSOUT
        //     END-IF
        //     .
        //     EJECT
        // 100500***************************************************************** 15420001
        writeDeftTransRcd(state);
        writeSysout(state);
        writeSysout(state);
    }
    private void rejCycleVerifyProcess(ProgramState state) {
        // TODO: Translate COBOL paragraph '4500-REJ-CYCLE-VERIFY-PROCESS'
        // Original COBOL:
        // 100700***************************************************************** 15440001
        // 100800*   TEST FOR EMPTY FILE OR BAD CONTROL DATE                     * 15450001
        // 100900***************************************************************** 15460001
        //     PERFORM 7500-REJ-CYCLE-READ-RCD
        //     IF TEN99-T05R-REJ-CYCLE-EOF-YES
        //        MOVE LIT-REJECT-FILE TO MSG-NO-DATA-FILE-NAME
        //        MOVE MSG-NO-DATA TO CC-E01W-DISPLAY-RCD
        //        MOVE "Y"         TO ERROR-FLAG
        //        PERFORM 8999-WRITE-SYSOUT
        //     ELSE
        //        IF WS-T05R-REJ-CYCLE-HDR-VALUE = LOW-VALUES AND
        //           WS-T05R-REJ-CYCLE-HDR-ID = LIT-TEN99-REJECT-TRANS
        //           IF WS-T05R-REJ-CYCLE-HDR-YY = CC-TAX-YY1 AND
        //              WS-T05R-REJ-CYCLE-HDR-CC = CC-TAX-CC1 AND
        //              WS-T05R-REJ-CYCLE-HDR-MM = CC-TAX-MM1
        //              MOVE WS-T05R-REJECT-RECYCLING-RCD TO
        //                   TEN99-T05W-REJ-CYCLE-OUT-RCD
        //              PERFORM 8030-WRITE-REJECT-RCD
        //           ELSE
        //              MOVE LIT-REJECT-FILE    TO MSG-FILE-NAME
        //              MOVE MSG-INCORRECT-DATE TO CC-E01W-DISPLAY-RCD
        //              MOVE "Y"                TO ERROR-FLAG
        //              PERFORM 8999-WRITE-SYSOUT
        //           END-IF
        //        ELSE
        //           MOVE LIT-REJECT-FILE TO MSG-FILE-ID
        //           MOVE MSG-BAD-FILE    TO CC-E01W-DISPLAY-RCD
        //           MOVE "Y"             TO ERROR-FLAG
        //           PERFORM 8999-WRITE-SYSOUT
        //        END-IF
        //     END-IF
        //     .
        //     EJECT
        // 104000***************************************************************** 15750001
        rejCycleReadRcd(state);
        writeSysout(state);
        writeRejectRcd(state);
        writeSysout(state);
        writeSysout(state);
    }
    private void miscFormProcess(ProgramState state) {
        // TODO: Translate COBOL paragraph '5000-MISC-FORM-PROCESS'
        // Original COBOL:
        // 104200***************************************************************** 15770001
        // 104300*      PRIMING READ AND EOF TEST FOR MISC FORM INPUT            * 15780001
        // 104400*      EMPTY FILE SINCE 5/2009                                  * 15780001
        // 104500*      WRITE TRAILER RECORD                                     * 15780001
        // 104600***************************************************************** 15790001
        //     INITIALIZE WS-MISC-FORM-SEQ-CNT
        //     PERFORM 6500-INITIAL-OUTPUT-COUNT
        //     PERFORM 7000-MISC-FORM-READ-RCD
        // 105000***************************************************************** 15830001
        // 105100*   TEST FOR NO DATA ON FILE                                    * 15840001
        // 105200***************************************************************** 15850001
        //     IF TEN99-T01R-MISC-FORM-EOF-YES OR
        //        WS-T01R-MISC-FORM-TRL-VALUE = HIGH-VALUES
        //       MOVE LIT-MISC-FORM TO MSG-NO-DATA-FILE-NAME
        //       MOVE MSG-NO-DATA TO CC-E01W-DISPLAY-RCD
        //       PERFORM 8999-WRITE-SYSOUT
        //       PERFORM 5040-MISC-FORM-RESULTS-PROCESS
        //       IF WS-T01R-MISC-FORM-TRL-VALUE = HIGH-VALUES
        //          MOVE WS-T01R-MISC-FORM-RCD      TO WS-OUTPUT-TRANS-RCD
        //          PERFORM 8010-WRITE-MISC-TRANS-RCD
        //       END-IF
        //     ELSE
        //       PERFORM 5010-MISC-FORM-PROCESS-RCD UNTIL
        //               TEN99-T01R-MISC-FORM-EOF-YES  OR
        //               WS-T01R-MISC-FORM-TRL-VALUE = HIGH-VALUES
        //       PERFORM 5040-MISC-FORM-RESULTS-PROCESS
        //     END-IF
        //     .
        //     EJECT
        // 107200***************************************************************** 16010001
        initialOutputCount(state);
        miscFormReadRcd(state);
        writeSysout(state);
        miscFormResultsProcess(state);
        writeMiscTransRcd(state);
        miscFormProcessRcd(state);
        miscFormResultsProcess(state);
    }
    private void miscFormProcessRcd(ProgramState state) {
        // TODO: Translate COBOL paragraph '5010-MISC-FORM-PROCESS-RCD'
        // Original COBOL:
        // 107400***************************************************************** 16030001
        // 107500*   PROCESS THE DETAIL RECORDS FOR THE MISC FORM INPUT            16040001
        // 107600*   INCREMENT COUNTERS FOR TRAILER TESTING                        16050001
        // 107700***************************************************************** 16060001
        //     ADD LIT-1                 TO WS-MISC-FORM-TRANS-COUNTER
        //     ADD WS-T01R-MISC-FORM-AMT TO WS-MISC-FORM-TRANS-AMT-ACCUM
        //     PERFORM 5020-MISC-FORM-SET-TAX-TYPE
        //     PERFORM 7000-MISC-FORM-READ-RCD
        //     .
        // EJECT
        // 108500***************************************************************** 16150001
        miscFormSetTaxType(state);
        miscFormReadRcd(state);
    }
    private void miscFormSetTaxType(ProgramState state) {
        // TODO: Translate COBOL paragraph '5020-MISC-FORM-SET-TAX-TYPE'
        // Original COBOL:
        // 108700***************************************************************** 16170001
        // 108800*      UPDATE THE TAX TYPE FOR DETAIL RECORDS                   * 16180001
        // 108900***************************************************************** 16190001
        //     EVALUATE TRUE
        //       WHEN WS-T01R-MISC-FORM-HDR-VALUE = LOW-VALUES
        //            CONTINUE
        //       WHEN WS-T01R-MISC-FORM-TRL-VALUE = HIGH-VALUES
        //            CONTINUE
        //       WHEN WS-T01R-MISC-FORM-VALID-TAX
        //            CONTINUE
        //       WHEN OTHER
        //            MOVE LIT-3 TO WS-T01R-MISC-FORM-TAX-TYPE
        //     END-EVALUATE
        //     MOVE WS-T01R-MISC-FORM-RCD      TO WS-OUTPUT-TRANS-RCD
        //     PERFORM 8010-WRITE-MISC-TRANS-RCD
        //     .
        // 110400***************************************************************** 17020001
        writeMiscTransRcd(state);
    }
    private void miscFormResultsProcess(ProgramState state) {
        // TODO: Translate COBOL paragraph '5040-MISC-FORM-RESULTS-PROCESS'
        // Original COBOL:
        // 110600***************************************************************** 17040001
        // 110700*      DISPLAY TRAILER AND ACCUM RESULTS                        * 17050001
        // 110800***************************************************************** 17060001
        //     MOVE LIT-MISC-FORM TO MSG-OUTPUT-FILE-NAME
        //     IF WS-MISC-FORM-TRANS-COUNTER   =
        //        WS-T01R-MISC-FORM-TRL-COUNT      AND
        //        WS-MISC-FORM-TRANS-AMT-ACCUM =
        //        WS-T01R-MISC-FORM-TRL-AMOUNT
        //         MOVE LIT-MISC-INPUT-SUCCESS   TO CC-E01W-DISPLAY-RCD
        //         PERFORM 8999-WRITE-SYSOUT
        //         PERFORM 8999-WRITE-SYSOUT
        //         MOVE WS-MISC-FORM-TRANS-COUNTER TO MSG-MISC-FILE-COUNT
        //         MOVE WS-MISC-FORM-TRANS-AMT-ACCUM TO MSG-MISC-FILE-AMOUNT17150001
        //         MOVE MSG-MISC-FORM-SUMMARY TO CC-E01W-DISPLAY-RCD
        //         PERFORM 8999-WRITE-SYSOUT
        //         PERFORM 8999-WRITE-SYSOUT
        //         MOVE LIT-MISC-FORM-CONTROL-TOTALS TO CC-E01W-DISPLAY-RCD
        //         PERFORM 8999-WRITE-SYSOUT
        //     END-IF
        //     IF WS-MISC-FORM-TRANS-COUNTER   =
        //        WS-T01R-MISC-FORM-TRL-COUNT
        //         CONTINUE
        //     ELSE
        //         MOVE LIT-TRANS-TLS-DO-NOT-AGREE  TO
        //              CC-E01W-DISPLAY-RCD
        //         PERFORM 8999-WRITE-SYSOUT
        //         MOVE LIT-FOR-MISCELLANEOUS-INPUT TO
        //              CC-E01W-DISPLAY-RCD
        //         PERFORM 8999-WRITE-SYSOUT
        //         MOVE WS-MISC-FORM-TRANS-COUNTER  TO MSG-FILE-COUNT
        //         MOVE WS-T01R-MISC-FORM-TRL-COUNT TO MSG-TRAILER-COUNT
        //         MOVE MSG-TRAILER-COUNT-DISPLAY TO
        //              CC-E01W-DISPLAY-RCD
        //         PERFORM 8999-WRITE-SYSOUT
        //     END-IF
        //     IF WS-MISC-FORM-TRANS-AMT-ACCUM =
        //        WS-T01R-MISC-FORM-TRL-AMOUNT
        //         CONTINUE
        //     ELSE
        //         MOVE LIT-TRANS-TLS-DO-NOT-AGREE   TO
        //              CC-E01W-DISPLAY-RCD
        //         PERFORM 8999-WRITE-SYSOUT
        //         MOVE LIT-FOR-MISCELLANEOUS-INPUT  TO
        //              CC-E01W-DISPLAY-RCD
        //         PERFORM 8999-WRITE-SYSOUT
        //         MOVE WS-MISC-FORM-TRANS-AMT-ACCUM TO MSG-FILE-COUNT
        //         MOVE WS-T01R-MISC-FORM-TRL-AMOUNT TO MSG-TRAILER-COUNT
        //         MOVE MSG-TRAILER-COUNT-DISPLAY    TO
        //              CC-E01W-DISPLAY-RCD
        //         PERFORM 8999-WRITE-SYSOUT
        //     END-IF
        //     PERFORM 8999-WRITE-SYSOUT
        //     MOVE WS-MISC-FORM-TRANS-COUNTER    TO  WS-OUTPUT-TRANS-CNT
        //     MOVE WS-MISC-FORM-TRANS-AMT-ACCUM  TO  WS-OUTPUT-TRANS-AMT
        //     PERFORM 6600-OUTPUT-BALANCING-PROCESS
        //     .
        // EJECT
        // 116300***************************************************************** 17580001
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
        outputBalancingProcess(state);
    }
    private void bccwProcess(ProgramState state) {
        // TODO: Translate COBOL paragraph '5200-BCCW-PROCESS'
        // Original COBOL:
        // 116500***************************************************************** 17600001
        // 116600*      PRIMING READ AND EOF TEST FOR B&CCW INPUT                * 17610001
        // 116700***************************************************************** 17620001
        //     PERFORM 6500-INITIAL-OUTPUT-COUNT
        // 116900***************************************************************** 17640001
        // 117000*   TEST FOR NO DATA ON FILE                                    * 17650001
        // 117100***************************************************************** 17660001
        //     IF TEN99-T03R-BCCW-EOF-YES
        //        MOVE LIT-BCCW    TO MSG-NO-DATA-FILE-NAME
        //        MOVE MSG-NO-DATA TO CC-E01W-DISPLAY-RCD
        //        PERFORM 8999-WRITE-SYSOUT
        //        PERFORM 5240-BCCW-END-PROCESS
        //        MOVE "Y"         TO ERROR-FLAG
        //     ELSE
        //        PERFORM 5210-BCCW-PROCESS-RCD UNTIL
        //                TEN99-T03R-BCCW-EOF-YES
        //     END-IF
        //     IF TEN99-T03R-BCCW-EOF-YES
        //         PERFORM 5240-BCCW-END-PROCESS
        //     END-IF
        //     .
        // 118600***************************************************************** 17850001
        initialOutputCount(state);
        writeSysout(state);
        bccwEndProcess(state);
        bccwProcessRcd(state);
        bccwEndProcess(state);
    }
    private void bccwProcessRcd(ProgramState state) {
        // TODO: Translate COBOL paragraph '5210-BCCW-PROCESS-RCD'
        // Original COBOL:
        // 118800***************************************************************** 17870001
        // 118900*      PROCESS THE B&CCW RECORDS                                * 17880001
        // 119000*        HEADERS - VERIFY DATE                                  * 17880001
        // 119100*        DETAIL  - SET TAX TYPE                                 * 17880001
        // 119200*        TRAILER - ACCUM COUNT AND AMOUNTS FOR VERIFICATION     * 17880001
        // 119300*      ALL RECORDS ARE WRITTEN TO THE OUTPUT FILE               * 17900001
        // 119400*                                                               * 17920001
        // 119500***************************************************************** 17950001
        //     IF TEN99-LOW-VALUE-HDR = LOW-VALUES
        //           PERFORM 4210-BCCW-VERIFY
        //           MOVE SPACES TO TEN99-LOW-VALUE-HDR
        //     ELSE
        //        IF TEN99-HIGH-VALUE-TLR = HIGH-VALUES
        //           MOVE TEN99-RECORD-TLR TO WS-OUTPUT-TRANS-RCD
        //           PERFORM 8020-WRITE-BCCW-TRANS-RCD
        //           PERFORM 5250-BCCW-TRAILER-ACCUM
        //           MOVE SPACES TO TEN99-HIGH-VALUE-TLR
        //        ELSE
        //           PERFORM 5220-BCCW-SET-TAX-TYPE
        //        END-IF
        //     END-IF
        //     PERFORM 7200-BCCW-READ-RCD
        //     .
        // EJECT
        // 121300***************************************************************** 18940001
        bccwVerify(state);
        writeBccwTransRcd(state);
        bccwTrailerAccum(state);
        bccwSetTaxType(state);
        bccwReadRcd(state);
    }
    private void bccwSetTaxType(ProgramState state) {
        // TODO: Translate COBOL paragraph '5220-BCCW-SET-TAX-TYPE'
        // Original COBOL:
        // 121500***************************************************************** 18960001
        // 121600*      SET UP AND SEARCH 1099 TABLE                             * 18970001
        // 121700*      RECORDS WITH BRANCH = 048 ARE SPECIAL OT CLOC CHECKS     * 18970001
        // 121800*                               CLOC TAX TYPE SET TO 3          * 18970001
        // 121900*      DEFAULT TAX TYPE IS 3 - SHOULD BE INVESTIGATED           * 18970001
        // 122000*      RECORDS WHERE THE CEC IS SPACES ARE WRITTEN TO THE SYSOUT* 18970001
        // 122100*      THE TAX TYPE IS NOT SET FOR FOREIGN RECORDS (1/2011)     * 18970001
        // 122200*          FOREIGN RECS - CANADA AND PUERTO RICO                * 18970001
        // 122300***************************************************************** 18980001
        //     ADD LIT-1                       TO WS-BCCW-TRANS-COUNTER
        //     ADD TEN99-RPT-DISB-AMT-DATA     TO WS-BCCW-TRANS-AMT-ACCUM
        //     SET FOREIGN-IND-NO              TO TRUE
        //     MOVE TEN99-COMPASS-ENT-CDE-DATA TO WS-CEC
        //     MOVE TEN99-PS-BUS-UNIT          TO WS-BUS-UNIT
        //     INITIALIZE WS-TAX-TYPE
        //     IF (WS-BUS-UNIT-CAN)
        //       OR (TEN99-ISSUING-BR-CODE-DATA = LIT-TW)
        //       OR (TEN99-STATE-DATA = LIT-PR-STATE-CODE)
        //          SET FOREIGN-IND-YES        TO TRUE
        //     ELSE
        //       IF TEN99-COMPASS-ENT-CDE-DATA = SPACES
        //          IF TEN99-ISSUING-BR-CODE-DATA = LIT-048
        //               MOVE LIT-3 TO WS-TAX-TYPE
        //          ELSE
        //               MOVE TEN99-RCD TO CC-E01W-DISPLAY-RCD
        //               PERFORM 8999-WRITE-SYSOUT
        //          END-IF
        //       ELSE
        //         PERFORM 6000-SEARCH-1099-TABLE
        //       END-IF
        //     END-IF
        //     MOVE WS-TAX-TYPE  TO TEN99-TAX-TYPE
        //     MOVE TEN99-RCD    TO WS-OUTPUT-TRANS-RCD
        //     PERFORM 8020-WRITE-BCCW-TRANS-RCD
        //     .
        // EJECT
        // 125300***************************************************************** 19820001
        writeSysout(state);
        search1099Table(state);
        writeBccwTransRcd(state);
    }
    private void bccwEndProcess(ProgramState state) {
        // TODO: Translate COBOL paragraph '5240-BCCW-END-PROCESS'
        // Original COBOL:
        // 125500***************************************************************** 19840001
        // 125600*      TEST TRAILER TOTALS AND DISPLAY RESULTS                  * 19850001
        // 125700***************************************************************** 19860001
        //     PERFORM 8999-WRITE-SYSOUT
        //     MOVE LIT-BCCW TO MSG-OUTPUT-FILE-NAME
        //     IF WS-BCCW-TRANS-COUNTER = WS-BCCW-TRL-COUNTER AND
        //        WS-BCCW-TRANS-AMT-ACCUM = WS-BCCW-TRL-AMT-ACCUM
        //        MOVE LIT-BCCW-SUCCESS TO CC-E01W-DISPLAY-RCD
        //        PERFORM 8999-WRITE-SYSOUT
        //        MOVE WS-BCCW-TRANS-COUNTER TO MSG-FILE-COUNT
        //        MOVE WS-BCCW-TRL-COUNTER TO MSG-TRAILER-COUNT
        //        MOVE MSG-TRAILER-COUNT-DISPLAY TO
        //             CC-E01W-DISPLAY-RCD
        //        PERFORM 8999-WRITE-SYSOUT
        //        MOVE WS-BCCW-TRANS-AMT-ACCUM TO MSG-FILE-AMOUNT
        //        MOVE WS-BCCW-TRL-AMT-ACCUM TO MSG-TRAILER-AMOUNT
        //        MOVE MSG-TRAILER-DOLLAR-DISPLAY TO
        //             CC-E01W-DISPLAY-RCD
        //        PERFORM 8999-WRITE-SYSOUT
        //        PERFORM 8999-WRITE-SYSOUT
        //     ELSE
        //        IF WS-BCCW-TRANS-COUNTER = WS-BCCW-TRL-COUNTER
        //           CONTINUE
        //        ELSE
        //           MOVE LIT-TRANS-TLS-DO-NOT-AGREE TO
        //                CC-E01W-DISPLAY-RCD
        //           PERFORM 8999-WRITE-SYSOUT
        //           MOVE LIT-FOR-BCCW-INPUT TO
        //                CC-E01W-DISPLAY-RCD
        //           PERFORM 8999-WRITE-SYSOUT
        //           MOVE WS-BCCW-TRANS-COUNTER TO MSG-FILE-COUNT
        //           MOVE WS-BCCW-TRL-COUNTER TO MSG-TRAILER-COUNT
        //           MOVE MSG-TRAILER-COUNT-DISPLAY TO
        //                CC-E01W-DISPLAY-RCD
        //           PERFORM 8999-WRITE-SYSOUT
        //        END-IF
        //        IF WS-BCCW-TRANS-AMT-ACCUM =
        //           WS-BCCW-TRL-AMT-ACCUM
        //           CONTINUE
        //        ELSE
        //           MOVE LIT-TRANS-TLS-DO-NOT-AGREE TO
        //                CC-E01W-DISPLAY-RCD
        //           PERFORM 8999-WRITE-SYSOUT
        //           MOVE LIT-FOR-BCCW-INPUT TO
        //                CC-E01W-DISPLAY-RCD
        //           PERFORM 8999-WRITE-SYSOUT
        //           MOVE WS-BCCW-TRANS-AMT-ACCUM TO MSG-FILE-AMOUNT
        //           MOVE WS-BCCW-TRL-AMT-ACCUM TO MSG-TRAILER-AMOUNT
        //           MOVE MSG-TRAILER-DOLLAR-DISPLAY TO
        //                CC-E01W-DISPLAY-RCD
        //           PERFORM 8999-WRITE-SYSOUT
        //        END-IF
        //        PERFORM 8999-WRITE-SYSOUT
        //     END-IF
        //     MOVE LIT-BCCW-CONTROL-TOTALS TO CC-E01W-DISPLAY-RCD
        //     PERFORM 8999-WRITE-SYSOUT
        //     MOVE WS-BCCW-TRANS-COUNTER    TO WS-OUTPUT-TRANS-CNT
        //     MOVE WS-BCCW-TRANS-AMT-ACCUM  TO WS-OUTPUT-TRANS-AMT
        //     PERFORM 6600-OUTPUT-BALANCING-PROCESS
        //     PERFORM 8999-WRITE-SYSOUT
        //     .
        // EJECT
        // 131700***************************************************************** 20460001
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
        outputBalancingProcess(state);
        writeSysout(state);
    }
    private void bccwTrailerAccum(ProgramState state) {
        // TODO: Translate COBOL paragraph '5250-BCCW-TRAILER-ACCUM'
        // Original COBOL:
        // 131900***************************************************************** 20480001
        // 132000*      ACCUMULATE BCCW TRAILER TOTALS                           * 20490001
        // 132100*      NOTE: THERE ARE MULTIPLE TRAILER RECORDS ON THE BCCW     * 20500001
        // 132200*      FILE. THE TRAILERS ARE ACCUMULATED TO BALANCE THE DATA.  * 20510001
        // 132300*                                                               * 20520001
        // 132400***************************************************************** 20530001
        //     IF TEN99-WRITTEN-CNT-TLR NUMERIC
        //          MOVE TEN99-WRITTEN-CNT-TLR TO WS-T03R-BCCW-TRL-CNT
        //     ELSE MOVE ZEROES                TO WS-T03R-BCCW-TRL-CNT
        //     END-IF
        //     ADD WS-T03R-BCCW-TRL-CNT        TO WS-BCCW-TRL-COUNTER
        //     ADD TEN99-TOTAL-DOLLAR-AMT-TLR  TO WS-BCCW-TRL-AMT-ACCUM
        //     .
        // 133300***************************************************************** 20590001
        // EJECT
        // 133600***************************************************************** 20620001

    }
    private void deftProcess(ProgramState state) {
        // TODO: Translate COBOL paragraph '5400-DEFT-PROCESS'
        // Original COBOL:
        // 133800***************************************************************** 20640001
        // 133900*      PRIMING READ AND EOF TEST FOR DEFT  INPUT                * 20650001
        // 134000***************************************************************** 20660001
        //     PERFORM 6500-INITIAL-OUTPUT-COUNT
        // 134200***************************************************************** 20680001
        // 134300*   TEST FOR NO DATA ON FILE                                    * 20690001
        // 134400***************************************************************** 20700001
        //     IF TEN99-T04R-DEFT-EOF-YES OR
        //        END-OF-DEFT-DETAIL-YES
        //        MOVE LIT-DEFT               TO MSG-NO-DATA-FILE-NAME
        //        MOVE MSG-NO-DATA            TO CC-E01W-DISPLAY-RCD
        //        PERFORM 8999-WRITE-SYSOUT
        //       IF WS-T04R-DEFT-TRL-VALUE = HIGH-VALUES
        //          MOVE WS-T04R-DEFT-TRL TO WS-OUTPUT-TRANS-RCD
        //          PERFORM 8040-WRITE-DEFT-TRANS-RCD
        //          PERFORM 5450-DEFT-TRAILER-ACCUM
        //       END-IF
        //        PERFORM 5440-DEFT-TRAILER-PROCESS
        //     ELSE
        //        PERFORM 5410-DEFT-PROCESS-RCD
        //          UNTIL TEN99-T04R-DEFT-EOF-YES
        //        PERFORM 5440-DEFT-TRAILER-PROCESS
        //     END-IF
        //     .
        // 136200***************************************************************** 20880001
        initialOutputCount(state);
        writeSysout(state);
        writeDeftTransRcd(state);
        deftTrailerAccum(state);
        deftTrailerProcess(state);
        deftProcessRcd(state);
        deftTrailerProcess(state);
    }
    private void deftProcessRcd(ProgramState state) {
        // TODO: Translate COBOL paragraph '5410-DEFT-PROCESS-RCD'
        // Original COBOL:
        // 136400***************************************************************** 20900001
        // 136500*      PROCESS THE DETAIL AND TRAILER FOR THE DEFT  ISSUES      * 20910001
        // 136600*      INCREMENT COUNTERS FOR PRIMING READ                      * 20920001
        // 136700*      ONE HEADER & ONE TRAILER FOR THE DEFT FILE               * 20940001
        // 136800***************************************************************** 20970001
        //     IF WS-T04R-DEFT-TRL-VALUE = HIGH-VALUES
        //        MOVE WS-T04R-DEFT-TRL TO WS-OUTPUT-TRANS-RCD
        //        PERFORM 8040-WRITE-DEFT-TRANS-RCD
        //        PERFORM 5450-DEFT-TRAILER-ACCUM
        //     ELSE
        //        PERFORM 5420-DEFT-SET-TAX-TYPE
        //     END-IF
        //     PERFORM 7300-DEFT-READ-RCD
        //     .
        // 137900***************************************************************** 21810001
        writeDeftTransRcd(state);
        deftTrailerAccum(state);
        deftSetTaxType(state);
        deftReadRcd(state);
    }
    private void deftSetTaxType(ProgramState state) {
        // TODO: Translate COBOL paragraph '5420-DEFT-SET-TAX-TYPE'
        // Original COBOL:
        // 138100***************************************************************** 21830001
        // 138200*      SET UP AND SEARCH 1099 TABLE                             * 21840001
        // 138300*      DEFAULT TAX TYPE IS SET TO 3 - S/B INVESTIGATED          * 21840001
        // 138400***************************************************************** 21850001
        //     ADD LIT-1                       TO WS-DEFT-TRANS-COUNTER
        //     ADD WS-T04R-DEFT-DISB-AMT       TO WS-DEFT-TRANS-AMT-ACCUM
        //     MOVE WS-T04R-DEFT-COMPASS-CODE  TO WS-CEC
        //     INITIALIZE WS-TAX-TYPE
        //     IF WS-T04R-DEFT-COMPASS-CODE = SPACES
        //          CONTINUE
        //     ELSE
        //          PERFORM 6000-SEARCH-1099-TABLE
        //     END-IF
        //     IF WS-TAX-TYPE = SPACES
        //          MOVE WS-T04R-DEFT-RCD TO CC-E01W-DISPLAY-RCD
        //          PERFORM 8999-WRITE-SYSOUT
        //          MOVE LIT-3            TO WS-TAX-TYPE
        //     END-IF
        //     MOVE WS-TAX-TYPE           TO WS-T04R-DEFT-TAX-TYPE
        //     MOVE WS-T04R-DEFT-RCD      TO WS-OUTPUT-TRANS-RCD
        //     PERFORM 8040-WRITE-DEFT-TRANS-RCD
        //     .
        // 140400***************************************************************** 22200001
        // 140500***************************************************************** 22720001
        search1099Table(state);
        writeSysout(state);
        writeDeftTransRcd(state);
    }
    private void deftTrailerProcess(ProgramState state) {
        // TODO: Translate COBOL paragraph '5440-DEFT-TRAILER-PROCESS'
        // Original COBOL:
        // 140700***************************************************************** 22740001
        // 140800*      TEST TRAILER TOTALS AND DISPLAY RESULTS                  * 22750001
        // 140900***************************************************************** 22760001
        //     PERFORM 8999-WRITE-SYSOUT
        //     MOVE LIT-DEFT TO MSG-OUTPUT-FILE-NAME
        //     IF WS-DEFT-TRANS-COUNTER   = WS-DEFT-TRL-COUNTER AND
        //        WS-DEFT-TRANS-AMT-ACCUM = WS-DEFT-TRL-AMT-ACCUM
        //        MOVE LIT-DEFT-SUCCESS           TO CC-E01W-DISPLAY-RCD
        //        PERFORM 8999-WRITE-SYSOUT
        //        MOVE WS-DEFT-TRANS-COUNTER      TO MSG-FILE-COUNT
        //        MOVE WS-DEFT-TRL-COUNTER        TO MSG-TRAILER-COUNT
        //        MOVE MSG-TRAILER-COUNT-DISPLAY  TO CC-E01W-DISPLAY-RCD
        //        PERFORM 8999-WRITE-SYSOUT
        //        MOVE WS-DEFT-TRANS-AMT-ACCUM    TO MSG-FILE-AMOUNT
        //        MOVE WS-DEFT-TRL-AMT-ACCUM      TO MSG-TRAILER-AMOUNT
        //        MOVE MSG-TRAILER-DOLLAR-DISPLAY TO CC-E01W-DISPLAY-RCD
        //        PERFORM 8999-WRITE-SYSOUT
        //     ELSE
        //        IF WS-DEFT-TRANS-COUNTER = WS-DEFT-TRL-COUNTER
        //           CONTINUE
        //        ELSE
        //           MOVE LIT-TRANS-TLS-DO-NOT-AGREE TO CC-E01W-DISPLAY-RCD
        //           PERFORM 8999-WRITE-SYSOUT
        //           MOVE LIT-FOR-DEFT-INPUT         TO CC-E01W-DISPLAY-RCD
        //           PERFORM 8999-WRITE-SYSOUT
        //           MOVE WS-DEFT-TRANS-COUNTER      TO MSG-FILE-COUNT
        //           MOVE WS-DEFT-TRL-COUNTER        TO MSG-TRAILER-COUNT
        //           MOVE MSG-TRAILER-COUNT-DISPLAY  TO CC-E01W-DISPLAY-RCD
        //           PERFORM 8999-WRITE-SYSOUT
        //        END-IF
        //        IF WS-DEFT-TRANS-AMT-ACCUM = WS-DEFT-TRL-AMT-ACCUM
        //           CONTINUE
        //        ELSE
        //           MOVE LIT-TRANS-TLS-DO-NOT-AGREE TO CC-E01W-DISPLAY-RCD
        //           PERFORM 8999-WRITE-SYSOUT
        //           MOVE LIT-FOR-DEFT-INPUT         TO CC-E01W-DISPLAY-RCD
        //           PERFORM 8999-WRITE-SYSOUT
        //           MOVE WS-DEFT-TRANS-AMT-ACCUM    TO MSG-FILE-AMOUNT
        //           MOVE WS-DEFT-TRL-AMT-ACCUM      TO MSG-TRAILER-AMOUNT
        //           MOVE MSG-TRAILER-DOLLAR-DISPLAY TO CC-E01W-DISPLAY-RCD
        //           PERFORM 8999-WRITE-SYSOUT
        //        END-IF
        //        PERFORM 8999-WRITE-SYSOUT
        //     END-IF
        //     MOVE LIT-DEFT-CONTROL-TOTALS          TO CC-E01W-DISPLAY-RCD
        //     PERFORM 8999-WRITE-SYSOUT
        //     MOVE WS-DEFT-TRANS-COUNTER    TO WS-OUTPUT-TRANS-CNT
        //     MOVE WS-DEFT-TRANS-AMT-ACCUM  TO WS-OUTPUT-TRANS-AMT
        //     PERFORM 6600-OUTPUT-BALANCING-PROCESS
        //     PERFORM 8999-WRITE-SYSOUT
        //     .
        // EJECT
        // 146200***************************************************************** 23320001
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
        outputBalancingProcess(state);
        writeSysout(state);
    }
    private void deftTrailerAccum(ProgramState state) {
        // TODO: Translate COBOL paragraph '5450-DEFT-TRAILER-ACCUM'
        // Original COBOL:
        // 146400***************************************************************** 23340001
        // 146500*      ACCUMULATE DEFT TRAILER TOTALS                           * 23350001
        // 146600***************************************************************** 23360001
        //     ADD WS-T04R-DEFT-TRL-CNT TO WS-DEFT-TRL-COUNTER
        //     ADD WS-T04R-DEFT-TRL-AMT TO WS-DEFT-TRL-AMT-ACCUM
        //     .
        // EJECT
        // 147100***************************************************************** 23420001

    }
    private void rejCycleProcess(ProgramState state) {
        // TODO: Translate COBOL paragraph '5500-REJ-CYCLE-PROCESS'
        // Original COBOL:
        // 147300***************************************************************** 23440001
        // 147400*      PRIMING READ AND EOF TEST FOR REJECT RECYCLED INPUT      * 23450001
        // 147500***************************************************************** 23460001
        //     PERFORM 6500-INITIAL-OUTPUT-COUNT
        //     PERFORM 7500-REJ-CYCLE-READ-RCD
        //     IF TEN99-T05R-REJ-CYCLE-EOF-YES OR
        //        WS-T05R-REJ-CYCLE-TRL-VALUE = HIGH-VALUES
        // 148000***************************************************************** 23510001
        // 148100*   TEST FOR NO DATA ON FILE                                    * 23520001
        // 148200***************************************************************** 23530001
        //       MOVE LIT-REJECT-FILE TO MSG-NO-DATA-FILE-NAME
        //       MOVE MSG-NO-DATA TO CC-E01W-DISPLAY-RCD
        //       PERFORM 8999-WRITE-SYSOUT
        //       MOVE WS-T05R-REJECT-RECYCLING-RCD TO
        //            TEN99-T05W-REJ-CYCLE-OUT-RCD
        //       PERFORM 8030-WRITE-REJECT-RCD
        //       PERFORM 5540-REJ-CYCLE-TRAILER-PROCESS
        //     ELSE
        //       PERFORM 5510-REJ-CYCLE-PROCESS-RCD UNTIL
        //                TEN99-T05R-REJ-CYCLE-EOF-YES
        //     END-IF
        //     .
        //     EJECT
        // 149600***************************************************************** 23640001
        initialOutputCount(state);
        rejCycleReadRcd(state);
        writeSysout(state);
        writeRejectRcd(state);
        rejCycleTrailerProcess(state);
        rejCycleProcessRcd(state);
    }
    private void rejCycleProcessRcd(ProgramState state) {
        // TODO: Translate COBOL paragraph '5510-REJ-CYCLE-PROCESS-RCD'
        // Original COBOL:
        // 149800***************************************************************** 23660001
        // 149900*      PROCESS THE DETAIL AND TRAILER FOR REJECT RECYCLED INPUT * 23670001
        // 150000***************************************************************** 23680001
        //     IF WS-T05R-REJ-CYCLE-TRL-VALUE = HIGH-VALUES
        //        MOVE WS-T05R-REJECT-RECYCLING-RCD TO
        //             TEN99-T05W-REJ-CYCLE-OUT-RCD
        //        PERFORM 8030-WRITE-REJECT-RCD
        //     ELSE
        // 150600***************************************************************** 23730001
        // 150700*   INCREMENT COUNTERS FOR TRAILER TESTING                      * 23740001
        // 150800***************************************************************** 23750001
        //        ADD 1 TO WS-REJ-CYCLE-TRANS-COUNTER
        //        PERFORM 5520-REJ-CYCLE-SET-TAX-TYPE
        //     END-IF
        //     PERFORM 7500-REJ-CYCLE-READ-RCD
        //     .
        // 151400***************************************************************** 23810001
        writeRejectRcd(state);
        rejCycleSetTaxType(state);
        rejCycleReadRcd(state);
    }
    private void rejCycleSetTaxType(ProgramState state) {
        // TODO: Translate COBOL paragraph '5520-REJ-CYCLE-SET-TAX-TYPE'
        // Original COBOL:
        // 151600***************************************************************** 23830001
        // 151700*      STRIP OFF THE REJECT KEY AND PROCESS ACCORDING TO        * 23840001
        // 151800*      THE SOURCE CODE.                                         * 23850001
        // 151900***************************************************************** 23860001
        //     INITIALIZE REJECT-INDICATORS
        //                WS-OUTPUT-TRANS-RCD
        //     EVALUATE TRUE
        //       WHEN WS-T05R-REJ-CYCLE-HDR-VALUE = LOW-VALUES
        //            CONTINUE
        //       WHEN WS-T05R-REJ-CYCLE-TRL-VALUE = HIGH-VALUES
        //            CONTINUE
        //       WHEN OTHER
        //            IF WS-T05R-REJECT-VALID-TAX-TYPE
        //                 CONTINUE
        //            ELSE MOVE LIT-3         TO WS-T05R-REJECT-TAX-TYPE
        //                 MOVE WS-T05R-REJECT-RECYCLING-RCD TO
        //                                       CC-E01W-DISPLAY-RCD
        //                 PERFORM 8999-WRITE-SYSOUT
        //            END-IF
        //     END-EVALUATE
        //     MOVE WS-T05R-REJECT-RECYCLING-RCD  TO
        //                                    TEN99-T05W-REJ-CYCLE-OUT-RCD
        //     PERFORM 8030-WRITE-REJECT-RCD
        //     .
        // 154100***************************************************************** 24830001
        writeSysout(state);
        writeRejectRcd(state);
    }
    private void rejCycleTrailerProcess(ProgramState state) {
        // TODO: Translate COBOL paragraph '5540-REJ-CYCLE-TRAILER-PROCESS'
        // Original COBOL:
        // 154300***************************************************************** 24850001
        // 154400*      TEST TRAILER TOTALS AND DISPLAY RESULTS                  * 24860001
        // 154500***************************************************************** 24870001
        //     PERFORM 8999-WRITE-SYSOUT
        //     PERFORM 8999-WRITE-SYSOUT
        //     MOVE LIT-REJECT-FILE TO MSG-OUTPUT-FILE-NAME
        //     IF WS-REJ-CYCLE-TRANS-COUNTER =
        //        WS-T05R-REJ-CYCLE-TRL-COUNT AND
        //        WS-REJ-CYCLE-TRANS-AMT-ACCUM =
        //        WS-T05R-REJ-CYCLE-TRL-AMT
        //        MOVE LIT-REJ-CYCLE-SUCCESS  TO CC-E01W-DISPLAY-RCD
        //        PERFORM 8999-WRITE-SYSOUT
        //        MOVE WS-REJ-CYCLE-TRANS-COUNTER TO MSG-FILE-COUNT
        //        MOVE WS-T05R-REJ-CYCLE-TRL-COUNT TO MSG-TRAILER-COUNT
        //        MOVE MSG-TRAILER-COUNT-DISPLAY TO
        //             CC-E01W-DISPLAY-RCD
        //        PERFORM 8999-WRITE-SYSOUT
        //        MOVE WS-REJ-CYCLE-TRANS-AMT-ACCUM TO MSG-FILE-AMOUNT
        //        MOVE WS-T05R-REJ-CYCLE-TRL-AMT TO MSG-TRAILER-AMOUNT
        //        MOVE MSG-TRAILER-DOLLAR-DISPLAY TO
        //             CC-E01W-DISPLAY-RCD
        //        PERFORM 8999-WRITE-SYSOUT
        //        PERFORM 8999-WRITE-SYSOUT
        //     ELSE
        //        IF WS-REJ-CYCLE-TRANS-COUNTER NOT EQUAL
        //           WS-T05R-REJ-CYCLE-TRL-COUNT
        //           MOVE LIT-TRANS-TLS-DO-NOT-AGREE TO
        //                CC-E01W-DISPLAY-RCD
        //           PERFORM 8999-WRITE-SYSOUT
        //           MOVE LIT-FOR-REJECT-CYCLE-INPUT TO
        //                CC-E01W-DISPLAY-RCD
        //           PERFORM 8999-WRITE-SYSOUT
        //           MOVE WS-REJ-CYCLE-TRANS-COUNTER TO MSG-FILE-COUNT
        //           MOVE WS-T05R-REJ-CYCLE-TRL-COUNT TO MSG-TRAILER-COUNT
        //           MOVE MSG-TRAILER-COUNT-DISPLAY TO
        //                CC-E01W-DISPLAY-RCD
        //           PERFORM 8999-WRITE-SYSOUT
        //           MOVE "Y" TO ERROR-FLAG
        //        END-IF
        //        IF WS-REJ-CYCLE-TRANS-AMT-ACCUM NOT EQUAL
        //           WS-T05R-REJ-CYCLE-TRL-AMT
        //           MOVE LIT-TRANS-TLS-DO-NOT-AGREE TO
        //                CC-E01W-DISPLAY-RCD
        //           PERFORM 8999-WRITE-SYSOUT
        //           MOVE LIT-FOR-REJECT-CYCLE-INPUT TO
        //                CC-E01W-DISPLAY-RCD
        //           PERFORM 8999-WRITE-SYSOUT
        //           MOVE WS-REJ-CYCLE-TRANS-AMT-ACCUM TO MSG-FILE-AMOUNT
        //           MOVE WS-T05R-REJ-CYCLE-TRL-AMT TO MSG-TRAILER-AMOUNT
        //           MOVE MSG-TRAILER-DOLLAR-DISPLAY TO
        //                CC-E01W-DISPLAY-RCD
        //           PERFORM 8999-WRITE-SYSOUT
        //           MOVE "Y" TO ERROR-FLAG
        //        END-IF
        //        PERFORM 8999-WRITE-SYSOUT
        //     END-IF
        //     PERFORM 8999-WRITE-SYSOUT
        //     MOVE LIT-REJ-CYCLE-CONTROL-TOTALS TO CC-E01W-DISPLAY-RCD
        //     PERFORM 8999-WRITE-SYSOUT
        //     MOVE WS-REJ-CYCLE-TRANS-COUNTER    TO WS-OUTPUT-TRANS-CNT
        //     MOVE WS-REJ-CYCLE-TRANS-AMT-ACCUM TO WS-OUTPUT-TRANS-AMT
        //     PERFORM 6600-OUTPUT-BALANCING-PROCESS
        //     MOVE "Y" TO TEN99-T05R-REJ-CYCLE-EOF
        //     .
        // 160800***************************************************************** 25470001
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
        outputBalancingProcess(state);
    }
    private void search1099Table(ProgramState state) {
        // TODO: Translate COBOL paragraph '6000-SEARCH-1099-TABLE'
        // Original COBOL:
        // 161000***************************************************************** 25490001
        // 161100*   SET WS-CEC PRIOR TO PERFORM                                 * 25500001
        // 161200***************************************************************** 25510001
        //     SET CODE-INDEX TO 1
        //     SEARCH WS-T07R-1099ENTRY-CODES AT END
        //                                    CONTINUE
        //     WHEN WS-CEC = WS-T07R-1099ENTRY-CODE (CODE-INDEX)
        //          MOVE WS-T07R-1099ENTRY-TAX-TYPE (CODE-INDEX) TO
        //                                                 WS-TAX-TYPE
        //     .
        // 162000***************************************************************** 25870001
        // 162100***************************************************************** 27810001

    }
    private void initialOutputCount(ProgramState state) {
        // TODO: Translate COBOL paragraph '6500-INITIAL-OUTPUT-COUNT'
        // Original COBOL:
        // 162300***************************************************************** 27830001
        // 162400*    INITIALIZE OUTPUT COUNTERS                                 * 27840001
        // 162500***************************************************************** 27850001
        //      INITIALIZE WS-OUTPUT-TRANS-CNT
        //                 WS-OUTPUT-TRANS-AMT
        //                 WS-FILE-REC-COUNTER
        //                 WS-FILE-AMT-ACCUM
        //                 WS-REJ-TRANS-CNT
        //                 WS-REJ-TRANS-AMT
        //      .
        // EJECT
        // 163400***************************************************************** 27960001

    }
    private void outputBalancingProcess(ProgramState state) {
        // TODO: Translate COBOL paragraph '6600-OUTPUT-BALANCING-PROCESS'
        // Original COBOL:
        // 163600***************************************************************** 27980001
        // 163700*    DISPLAY CONTROL TOTALS FOR ALL OUTPUTS                     * 27990001
        // 163800***************************************************************** 28000001
        //      MOVE WS-OUTPUT-TRANS-CNT TO MSG-NUM-RCDS
        //      MOVE WS-OUTPUT-TRANS-AMT TO MSG-AMOUNT
        //      MOVE MSG-CONTROL-TOTALS  TO CC-E01W-DISPLAY-RCD
        //      PERFORM 8999-WRITE-SYSOUT
        //      .
        // 164500***************************************************************** 28260001
        writeSysout(state);
    }
    private void miscFormReadRcd(ProgramState state) {
        // TODO: Translate COBOL paragraph '7000-MISC-FORM-READ-RCD'
        // Original COBOL:
        // 164700***************************************************************** 28280001
        // 164800*      READ ROUTINE FOR ALL MISC FORM FILE READS                * 28290001
        // 164900***************************************************************** 28300001
        //     INITIALIZE WS-T01R-MISC-FORM-RCD
        //     READ TEN99-T01R-MISC-TRANS-FILE INTO
        //          WS-T01R-MISC-FORM-RCD
        //        AT END
        //        MOVE "Y" TO TEN99-T01R-MISC-FORM-EOF
        //     END-READ
        //     .
        // EJECT
        // 165800***************************************************************** 28390001

    }
    private void bccwReadRcd(ProgramState state) {
        // TODO: Translate COBOL paragraph '7200-BCCW-READ-RCD'
        // Original COBOL:
        // 166000***************************************************************** 28410001
        // 166100*      READ ROUTINE FOR B&CCW ISSUES                            * 28420001
        // 166200*      NOTE: THERE ARE MULTIPLE HEADERS & TRAILERS              * 28420001
        // 166300***************************************************************** 28430001
        //     READ TEN99-T03R-BCCW-FILE         INTO TEN99-RCD
        //        AT END
        //        MOVE "Y" TO TEN99-T03R-BCCW-EOF
        //     END-READ
        //     MOVE TEN99-RCD TO TEN99-RECORD-HDR TEN99-RECORD-TLR
        //     EVALUATE TRUE
        //       WHEN TEN99-T03R-BCCW-EOF-YES
        //          CONTINUE
        //       WHEN TEN99-LOW-VALUE-HDR    = LOW-VALUES
        //            MOVE TEN99-RECORD-HDR TO WS-OUTPUT-TRANS-RCD
        //            INITIALIZE TEN99-RCD
        //       WHEN TEN99-HIGH-VALUE-TLR   = HIGH-VALUES
        //            MOVE TEN99-RECORD-TLR TO WS-OUTPUT-TRANS-RCD
        //            INITIALIZE TEN99-RCD
        //       WHEN OTHER
        //           CONTINUE
        //     END-EVALUATE
        //     .
        // EJECT
        // 168300***************************************************************** 28590001

    }
    private void deftReadRcd(ProgramState state) {
        // TODO: Translate COBOL paragraph '7300-DEFT-READ-RCD'
        // Original COBOL:
        // 168500***************************************************************** 28610001
        // 168600*      READ ROUTINE FOR DEFT  ISSUES                            * 28620001
        // 168700***************************************************************** 28630001
        //     INITIALIZE WS-T04R-DEFT-RCD
        //     READ TEN99-T04R-DEFT-FILE         INTO WS-T04R-DEFT-RCD
        //        AT END
        //        MOVE "Y" TO TEN99-T04R-DEFT-EOF
        //     END-READ
        //     MOVE WS-T04R-DEFT-RCD TO WS-OUTPUT-TRANS-RCD
        //     EVALUATE TRUE
        //       WHEN WS-T04R-DEFT-HDR-VALUE = LOW-VALUES
        //            CONTINUE
        //       WHEN TEN99-T04R-DEFT-EOF-YES
        //            CONTINUE
        //       WHEN WS-T04R-DEFT-HDR-VALUE = HIGH-VALUES
        //            MOVE "Y" TO END-OF-DEFT-DETAIL
        //       WHEN OTHER
        //            MOVE "Y" TO END-OF-DEFT-HEADERS
        //     END-EVALUATE
        //     .
        // EJECT
        // 170700***************************************************************** 28810001

    }
    private void rejCycleReadRcd(ProgramState state) {
        // TODO: Translate COBOL paragraph '7500-REJ-CYCLE-READ-RCD'
        // Original COBOL:
        // 170900***************************************************************** 28830001
        // 171000*      READ ROUTINE FOR ALL MISC FORM FILE READS                * 28840001
        // 171100***************************************************************** 28850001
        //     INITIALIZE WS-T05R-REJECT-RECYCLING-RCD
        //     READ TEN99-T05R-REJ-CYCLE-FILE INTO
        //             WS-T05R-REJECT-RECYCLING-RCD
        //        AT END
        //        MOVE "Y" TO TEN99-T05R-REJ-CYCLE-EOF
        //     END-READ
        //     .
        // EJECT
        // 172000***************************************************************** 28940001

    }
    private void readControlCardIn(ProgramState state) {
        // TODO: Translate COBOL paragraph '7900-READ-CONTROL-CARD-IN'
        // Original COBOL:
        // 172200***************************************************************** 28960001
        // 172300*      READ ROUTINE FOR CONTROL CARD                            * 28970001
        // 172400***************************************************************** 28980001
        //     READ CC-R01R-CONTROL-CARD INTO CONTROL-CARD-IN1
        //         AT END
        //            MOVE "Y" TO EOF-CONTROL-CARD
        //     END-READ
        //     IF EOF-CONTROL-CARD-YES
        //        MOVE LIT-NO-CONTROL-CARD TO SAR-PARAGRAPH
        //        PERFORM 8999-WRITE-SYSOUT
        //        PERFORM 9998-COREDUMP
        //     END-IF
        //     .
        // EJECT
        // 173600***************************************************************** 29100001
        writeSysout(state);
        coredump(state);
    }
    private void read1099entryTable(ProgramState state) {
        // TODO: Translate COBOL paragraph '7910-READ-1099ENTRY-TABLE'
        // Original COBOL:
        // 173800***************************************************************** 29120001
        // 173900*      READ ROUTINE FOR 1099 REPORTABLE ENTRY CODE TABLE        * 29130001
        // 174000***************************************************************** 29140001
        //     READ WS-T07R-1099ENTRY-CD-FILE INTO
        //          WS-T07R-1099ENTRY-CODES(CODE-INDEX)
        //         AT END
        //            MOVE "Y" TO EOF-1099ENTRY-TABLE
        //     END-READ
        //     IF WS-T07R-1099ENTRY-CODE (CODE-INDEX) = HIGH-VALUES
        //        SET CODE-INDEX TO LIT-NUM-OF-1099-CODES
        //     END-IF
        //     IF EOF-1099ENTRY-TABLE-YES
        //        MOVE LIT-NO-1099ENTRY-TABLE TO SAR-PARAGRAPH
        //        PERFORM 8999-WRITE-SYSOUT
        //        PERFORM 9998-COREDUMP
        //     END-IF
        //     .
        // EJECT
        // 175600***************************************************************** 29440001
        writeSysout(state);
        coredump(state);
    }
    private void writeMiscTransRcd(ProgramState state) {
        // TODO: Translate COBOL paragraph '8010-WRITE-MISC-TRANS-RCD'
        // Original COBOL:
        // 175800***************************************************************** 29460001
        // 175900*      WRITE MISC TRANS RECORD                                  * 29470001
        // 176000***************************************************************** 29480001
        //     ADD LIT-1  TO WS-FILE-REC-COUNTER
        //     IF (WS-OUTPUT-HDR-VALUE = LOW-VALUES)
        //     OR (WS-OUTPUT-TRL-VALUE = HIGH-VALUES)
        //          CONTINUE
        //     ELSE
        //          ADD LIT-1                  TO WS-TOTAL-OUTPUT-TRANS-CNT
        //          ADD WS-T01R-MISC-FORM-AMT  TO WS-TOTAL-OUTPUT-TRANS-AMT
        //                                        WS-FILE-AMT-ACCUM
        //     END-IF
        //     WRITE TEN99-T01W-MISC-TRANS-OUT-RCD
        //           FROM WS-OUTPUT-TRANS-RCD
        //     MOVE SPACES TO WS-OUTPUT-TRANS-RCD
        //     INITIALIZE REJECT-INDICATORS
        //     .
        // EJECT
        // 177600***************************************************************** 29600001

    }
    private void writeBccwTransRcd(ProgramState state) {
        // TODO: Translate COBOL paragraph '8020-WRITE-BCCW-TRANS-RCD'
        // Original COBOL:
        // 177800***************************************************************** 29620001
        // 177900*      WRITE MISC TRANS RECORD                                  * 29630001
        // 178000***************************************************************** 29640001
        //     ADD LIT-1  TO WS-FILE-REC-COUNTER
        //     IF   (WS-OUTPUT-HDR-VALUE = LOW-VALUES)
        //       OR (WS-OUTPUT-TRL-VALUE = HIGH-VALUES)
        //              CONTINUE
        //     ELSE
        //          ADD LIT-1                   TO WS-TOTAL-OUTPUT-TRANS-CNT29670001
        //          ADD TEN99-RPT-DISB-AMT-DATA TO WS-TOTAL-OUTPUT-TRANS-AMT29680001
        //                                         WS-FILE-AMT-ACCUM
        //     END-IF
        //     WRITE TEN99-T03W-BCCW-OUT-RCD
        //           FROM WS-OUTPUT-TRANS-RCD
        //     MOVE SPACES TO WS-OUTPUT-TRANS-RCD
        //     .
        // 179400***************************************************************** 29750001

    }
    private void writeRejectRcd(ProgramState state) {
        // TODO: Translate COBOL paragraph '8030-WRITE-REJECT-RCD'
        // Original COBOL:
        // 179600***************************************************************** 29770001
        // 179700*      WRITE REJECT FILE                                        * 29780001
        // 179800***************************************************************** 29790001
        // 179900
        //     WRITE TEN99-T05W-REJ-CYCLE-OUT-RCD
        //     .
        // 180200***************************************************************** 29870001

    }
    private void writeDeftTransRcd(ProgramState state) {
        // TODO: Translate COBOL paragraph '8040-WRITE-DEFT-TRANS-RCD'
        // Original COBOL:
        // 180400***************************************************************** 29890001
        // 180500*      WRITE FOREIGN TRANSACTION RECORD                         * 29900001
        // 180600***************************************************************** 29910001
        //     ADD LIT-1  TO WS-FILE-REC-COUNTER
        //     IF   (WS-OUTPUT-HDR-VALUE = LOW-VALUES)
        //       OR (WS-OUTPUT-TRL-VALUE = HIGH-VALUES)
        //              CONTINUE
        //     ELSE
        //         ADD LIT-1                   TO WS-TOTAL-OUTPUT-TRANS-CNT
        //         ADD WS-T04R-DEFT-DISB-AMT   TO WS-TOTAL-OUTPUT-TRANS-AMT
        //                                        WS-FILE-AMT-ACCUM
        //     END-IF
        //     WRITE TEN99-T04W-DEFT-OUT-RCD FROM WS-OUTPUT-TRANS-RCD
        //     MOVE SPACES TO WS-OUTPUT-TRANS-RCD
        //     INITIALIZE REJECT-INDICATORS
        //     .
        // 182100***************************************************************** 30020001
        // 182200*8999-WRITE-SYSOUT.                                               30190001
        // 182300***************************************************************** 30200001
        // 182400*    ++INCLUDE C2INZ002                                         * 30210001
        // 182500*      COMMON PARAGRAPH TO WRITE A SYSOUT RECORD                * 30220001
        // 182600*  8999-WRITE-SYSOUT.                                           * 30230001
        // 182700***************************************************************** 30240001
        // COPY C2INZ002.
        // EJECT

    }
    private void terminationRoutine(ProgramState state) {
        // TODO: Translate COBOL paragraph '9000-TERMINATION-ROUTINE'
        // Original COBOL:
        // 183100***************************************************************** 30280001
        // 183200*      CHECK FOR PROCESSING ERROR FLAG SET TO "Y".              * 30290001
        // 183300*      PREPARE GRAND TOTALS.                                    * 30310001
        // 183400***************************************************************** 30320001
        //     IF ERROR-FLAG = "Y"
        //        PERFORM 9998-COREDUMP
        //     ELSE
        // 183800***************************************************************** 30730001
        // 183900*  MOVE TOTALS FOR GRAND TOTAL DISPLAY                          * 30740001
        // 184000***************************************************************** 30750001
        //        MOVE WS-TOTAL-OUTPUT-TRANS-CNT TO WS-OUTPUT-TRANS-CNT
        //        MOVE WS-TOTAL-OUTPUT-TRANS-AMT TO WS-OUTPUT-TRANS-AMT
        //        PERFORM 8999-WRITE-SYSOUT
        //        PERFORM 8999-WRITE-SYSOUT
        //        MOVE LIT-GRAND-TOTAL TO CC-E01W-DISPLAY-RCD
        //        PERFORM 8999-WRITE-SYSOUT
        //        MOVE LIT-TRANS-FILE TO MSG-OUTPUT-FILE-NAME
        //        PERFORM 6600-OUTPUT-BALANCING-PROCESS
        //     END-IF
        //     PERFORM 9010-ENDING-SYSOUT-MESSAGES
        //     PERFORM 9999-CLOSE-FILES
        //     .
        // EJECT
        coredump(state);
        writeSysout(state);
        writeSysout(state);
        writeSysout(state);
        outputBalancingProcess(state);
        endingSysoutMessages(state);
        closeFiles(state);
    }
    private void endingSysoutMessages(ProgramState state) {
        // TODO: Translate COBOL paragraph '9010-ENDING-SYSOUT-MESSAGES'
        // Original COBOL:
        // 185500***************************************************************** 30970001
        // 185600*    ++INCLUDE C2INZ003                                         * 30980001
        // 185700*      COMMON ENDING SYSOUT MESSAGES                            * 30990001
        // 185800***************************************************************** 31000001
        // COPY C2INZ003.
        // EJECT
        // 186300*9998-COREDUMP.                                                   31050001
        // 186400***************************************************************** 31060001
        // 186500*    ++INCLUDE C2INZ004                                         * 31070001
        // 186600*      COMMON ABEND INFORMATION PARAGRAPH                       * 31080001
        // 186700*      9998-COREDUMP                                            * 31090001
        // 186800***************************************************************** 31100001
        // COPY C2INZ004.
        // EJECT
        // 187200*9999-CLOSE-FILES.                                                31140001
        // 187300***************************************************************** 31150001
        // 187400*    ++INCLUDE C2INZ005                                         * 31160001
        // 187500*      COMMON CLOSE FILES PARAGRAPH                             * 31170001
        // 187600*      PERFORM 9999-CLOSE-FILES                                 * 31180001
        // 187700***************************************************************** 31190001
        // COPY C2INZ005.
        //     CLOSE
        //           TEN99-T01R-MISC-TRANS-FILE
        //           TEN99-T03R-BCCW-FILE
        //           TEN99-T04R-DEFT-FILE
        //           TEN99-T05R-REJ-CYCLE-FILE
        //           WS-T07R-1099ENTRY-CD-FILE
        //           CC-R01R-CONTROL-CARD
        //           TEN99-T01W-MISC-TRANS-OUT-FILE
        //           TEN99-T03W-BCCW-OUT-FILE
        //           TEN99-T04W-DEFT-OUT-FILE
        //           TEN99-T05W-REJ-CYCLE-OUT-FILE
        //           .
        // 189100***************************************************************** 31340001
        // 189200*   END PROGRAM CCAC6310                                        * 31350001
        // 189300***************************************************************** 31360001
        // 
        closeFiles(state);
    }

    private void coredump(ProgramState state) {
        // Stub for 9998-COREDUMP
    }


    private void writeSysout(ProgramState state) {
        // Stub for 8999-WRITE-SYSOUT
    }

}
