package com.fordcredit.misc1099.batch.program;

import org.springframework.batch.core.StepContribution;
import org.springframework.batch.core.scope.context.ChunkContext;
import org.springframework.batch.core.step.tasklet.Tasklet;
import org.springframework.batch.repeat.RepeatStatus;
import java.io.*;
import java.nio.file.*;
import java.math.BigDecimal;

public class CCAC6310Tasklet implements Tasklet {

    private final String basePath;

    public CCAC6310Tasklet(String basePath) {
        this.basePath = basePath;
    }

    // =========== PROGRAM STATE ===========
    static class ProgramState {
        // File I/O
        BufferedReader ten99T01rMiscTransFileReader;
        String ten99T01rMiscTransFileLine;
        boolean ten99T01rMiscTransFileEof = false;
        BufferedWriter ten99T01rMiscTransFileWriter;
        BufferedReader ten99T03rBccwFileReader;
        String ten99T03rBccwFileLine;
        boolean ten99T03rBccwFileEof = false;
        BufferedWriter ten99T03rBccwFileWriter;
        BufferedReader ten99T04rDeftFileReader;
        String ten99T04rDeftFileLine;
        boolean ten99T04rDeftFileEof = false;
        BufferedWriter ten99T04rDeftFileWriter;
        BufferedReader ten99T05rRejCycleFileReader;
        String ten99T05rRejCycleFileLine;
        boolean ten99T05rRejCycleFileEof = false;
        BufferedWriter ten99T05rRejCycleFileWriter;
        BufferedReader wsT07r1099entryCdFileReader;
        String wsT07r1099entryCdFileLine;
        boolean wsT07r1099entryCdFileEof = false;
        BufferedWriter wsT07r1099entryCdFileWriter;
        BufferedReader ten99T01wMiscTransOutFileReader;
        String ten99T01wMiscTransOutFileLine;
        boolean ten99T01wMiscTransOutFileEof = false;
        BufferedWriter ten99T01wMiscTransOutFileWriter;
        BufferedReader ten99T03wBccwOutFileReader;
        String ten99T03wBccwOutFileLine;
        boolean ten99T03wBccwOutFileEof = false;
        BufferedWriter ten99T03wBccwOutFileWriter;
        BufferedReader ten99T04wDeftOutFileReader;
        String ten99T04wDeftOutFileLine;
        boolean ten99T04wDeftOutFileEof = false;
        BufferedWriter ten99T04wDeftOutFileWriter;
        BufferedReader ten99T05wRejCycleOutFileReader;
        String ten99T05wRejCycleOutFileLine;
        boolean ten99T05wRejCycleOutFileEof = false;
        BufferedWriter ten99T05wRejCycleOutFileWriter;
        BufferedReader ccR01rControlCardReader;
        String ccR01rControlCardLine;
        boolean ccR01rControlCardEof = false;
        BufferedWriter ccR01rControlCardWriter;

        // WORKING-STORAGE variables
        String panValet = "024CCAC6310";
        String pups2000 = "";
        String wsVariableArea = "";
        String wsCec = "";
        String wsTaxType = "";
        String wsT01rMiscFormRcd = "";
        String wsT01rMiscFormOperLoc = "";
        String wsT01rMiscForm1099Type = "";
        String wsT01rMiscFormSsn = "";
        String wsT01rMiscFormReference = "";
        java.math.BigDecimal wsT01rMiscFormAmt = java.math.BigDecimal.ZERO;
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
        // 88 wsT01rMiscFormValidTax: wsT01rMiscFormTaxType.equals("3")
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
        int wsT01rMiscFormTrlCount = 0;
        java.math.BigDecimal wsT01rMiscFormTrlAmount = java.math.BigDecimal.ZERO;
        String wsT04rDeftRcd = "";
        String wsT04rDeftBusCode = "";
        int wsT04rDeftLabelCode = 0;
        String wsT04rDeftBranchCode = "";
        String wsT04rDeftBatchNum = "";
        String wsT04rDeftName = "";
        String wsT04rDeftAddress1 = "";
        String wsT04rDeftAddress2 = "";
        String wsT04rDeftCity = "";
        String wsT04rDeftState = "";
        String wsT04rDeftZipcode = "";
        String wsT04rDeftCompassCode = "";
        String wsT04rDeftDesc = "";
        java.math.BigDecimal wsT04rDeftDisbAmt = java.math.BigDecimal.ZERO;
        int wsT04rDeftDisbSeq = 0;
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
        String wsT04rDeftTaxType = "";
        // 88 wsT04rDeftValidTaxType: wsT04rDeftTaxType.equals("3")
        String wsT04rDeftTinInd = "";
        String wsT04rDeftAConstant = "A";
        String wsT04rDeftHdr = "";
        String wsT04rDeftHdrValue = "";
        String wsT04rDeftHdrId = "";
        String wsT04rDeftHdrDate = "";
        String wsT04rDeftHdrCc = "";
        String wsT04rDeftHdrYy = "";
        String wsT04rDeftHdrMm = "";
        String wsT04rDeftHdrDd = "";
        String wsT04rDeftTrl = "";
        String wsT04rDeftTrlValue = "";
        int wsT04rDeftTrlCnt = 0;
        java.math.BigDecimal wsT04rDeftTrlAmt = java.math.BigDecimal.ZERO;
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
        // 88 wsT05rRejectValidTaxType: wsT05rRejectTaxType.equals("3")
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
        java.math.BigDecimal wsT05rTotalAmt = java.math.BigDecimal.ZERO;
        java.math.BigDecimal wsT05r1099Amt = java.math.BigDecimal.ZERO;
        String wsT05rComment = "";
        int wsT05rNbrDistRcd = 0;
        String[] wsT05rDistRcd = new String[5];
        String wsT05rDistCompassCode = "";
        String wsT05rDist1099Indic = "";
        java.math.BigDecimal wsT05rDistAmt = java.math.BigDecimal.ZERO;
        String wsT05rRejectMiscDetail = "";
        String wsT05rMiscFormOperLoc = "";
        String wsT05rMiscForm1099Type = "";
        String wsT05rMiscFormSsn = "";
        String wsT05rMiscFormReference = "";
        java.math.BigDecimal wsT05rMiscFormAmt = java.math.BigDecimal.ZERO;
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
        int wsT05rRejCycleTrlCount = 0;
        java.math.BigDecimal wsT05rRejCycleTrlAmt = java.math.BigDecimal.ZERO;
        String wsT07r1099entryCodeTbl = "";
        String[] wsT07r1099entryCodes = new String[150];
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
        int wsOutputTrlCnt = 0;
        java.math.BigDecimal wsOutputTrlAmt = java.math.BigDecimal.ZERO;
        String cntCounters = "";
        int wsMiscFormTransCounter = 0;
        int wsMiscFormSeqCnt = 0;
        int wsBccwTransCounter = 0;
        int wsFileRecCounter = 0;
        int wsBccwTrlCounter = 0;
        int wsDeftTransCounter = 0;
        int wsDeftTrlCounter = 0;
        int wsRejCycleTransCounter = 0;
        int wsOutputTransCnt = 0;
        int wsRejTransCnt = 0;
        int wsTotalOutputTransCnt = 0;
        int wsT03rBccwTrlCnt = 0;
        String swSwitches = "";
        String swBccwBuildComplete = "N";
        // 88 swBccwBuildCompleteYes: swBccwBuildComplete.equals("Y")
        String swDeftBuildComplete = "N";
        // 88 swDeftBuildCompleteYes: swDeftBuildComplete.equals("Y")
        String wsIndicators = "";
        String rejectIndicators = "";
        String rejectFlag = "SPACE";
        String wsUsInd = "N";
        // 88 wsUsIndYes: wsUsInd.equals("Y")
        String wsCdnInd = "N";
        // 88 wsCdnIndYes: wsCdnInd.equals("Y")
        String ten99T01rMiscFormEof = "N";
        // 88 ten99T01rMiscFormEofYes: ten99T01rMiscFormEof.equals("Y")
        String ten99T03rBccwEof = "N";
        // 88 ten99T03rBccwEofYes: ten99T03rBccwEof.equals("Y")
        String ten99T04rDeftEof = "N";
        // 88 ten99T04rDeftEofYes: ten99T04rDeftEof.equals("Y")
        String ten99T05rRejCycleEof = "N";
        // 88 ten99T05rRejCycleEofYes: ten99T05rRejCycleEof.equals("Y")
        String endOfBccwDetail = "N";
        // 88 endOfBccwDetailYes: endOfBccwDetail.equals("Y")
        String endOfBccwHeaders = "N";
        // 88 endOfBccwHeadersYes: endOfBccwHeaders.equals("Y")
        String endOfDeftDetail = "N";
        // 88 endOfDeftDetailYes: endOfDeftDetail.equals("Y")
        String endOfDeftHeaders = "N";
        // 88 endOfDeftHeadersYes: endOfDeftHeaders.equals("Y")
        String eofControlCard = "N";
        // 88 eofControlCardYes: eofControlCard.equals("Y")
        String eof1099entryTable = "N";
        // 88 eof1099entryTableYes: eof1099entryTable.equals("Y")
        String errorFlag = "N";
        String foreignInd = "";
        // 88 foreignIndNo: foreignInd.equals("N")
        // 88 foreignIndYes: foreignInd.equals("Y")
        String wsBccwCheckNum = "";
        String wsDeftBatchNum = "";
        String wsBusUnit = "";
        // 88 wsBusUnitCan: wsBusUnit.equals("6572")
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
        int lit1 = 1;
        int lit3 = 3;
        int lit5 = 5;
        int lit7 = 7;
        int lit095 = 95;
        int lit048 = 48;
        int litNumOf1099Codes = 150;
        String litTw = "TW";
        String litPrStateCode = "PR";
        int litZero = 0;
        String litZeroSsn = "";
        String litA = "A";
        int litTaxMonthJan = 1;
        int litTaxMonthDec = 12;
        int litTaxMonthYearEnd = 13;
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
        java.math.BigDecimal wsMiscFormTransAmtAccum = new java.math.BigDecimal("0");
        java.math.BigDecimal wsBccwTransAmtAccum = new java.math.BigDecimal("0");
        java.math.BigDecimal wsFileAmtAccum = new java.math.BigDecimal("0");
        java.math.BigDecimal wsBccwTrlAmtAccum = new java.math.BigDecimal("0");
        java.math.BigDecimal wsDeftTransAmtAccum = new java.math.BigDecimal("0");
        java.math.BigDecimal wsDeftTrlAmtAccum = new java.math.BigDecimal("0");
        java.math.BigDecimal wsRejCycleTransAmtAccum = new java.math.BigDecimal("0");
        java.math.BigDecimal wsOutputTransAmt = new java.math.BigDecimal("0");
        java.math.BigDecimal wsRejTransAmt = new java.math.BigDecimal("0");
        java.math.BigDecimal wsTotalOutputTransAmt = new java.math.BigDecimal("0");
        String ten99T01rMiscFormRcd = "";
        String ten99T03rBccwRcd = "";
        String ten99T04rDeftRcd = "";
        String ten99T05rRejCycleRcd = "";
        String ccR01rControlCardRec = "";
        String wsT07r1099entryCdRcd = "";
        String ten99T01wMiscTransOutRcd = "";
        String ten99T03wBccwOutRcd = "";
        String ten99T04wDeftOutRcd = "";
        String ten99T05wRejCycleOutRcd = "";

        // Auto-added fields
        String ccE01wDisplayRcd = "";
        String codeIndex = "";
        String endOfDeftDetailYes = "";
        String foreignIndNo = "";
        String foreignIndYes = "";
        String litRejectFileName = "";
        String sarParagraph = "";
        String ten99CcHdr = "";
        String ten99CompassEntCdeData = "";
        String ten99FileIdHdr = "";
        String ten99HighValueTlr = "";
        String ten99IssuingBrCodeData = "";
        String ten99LowValueHdr = "";
        String ten99MmHdr = "";
        String ten99PsBusUnit = "";
        String ten99Rcd = "";
        String ten99RecordHdr = "";
        String ten99RecordTlr = "";
        String ten99RptDisbAmtData = "";
        String ten99StateData = "";
        String ten99T01rMiscFormEofYes = "";
        String ten99T03rBccwEofYes = "";
        String ten99T04rDeftEofYes = "";
        String ten99T05rRejCycleEofYes = "";
        String ten99TaxType = "";
        String ten99TotalDollarAmtTlr = "";
        String ten99WrittenCntTlr = "";
        String ten99YyHdr = "";
        String wsBusUnitCan = "";
        String wsT01rMiscFormValidTax = "";
        String wsT05rRejectHdrValue = "";
        String wsT05rRejectTrlValue = "";
        String wsT05rRejectValidTaxType = "";
        String wsT07r1099EntryCodes = "";
        String wsT07r1099EntryTaxTypes = "";
        String wsTotalOutputTransAmt29680001 = "";
        String wsTotalOutputTransCnt29670001 = "";
    }

    @Override
    public RepeatStatus execute(StepContribution contribution, ChunkContext chunkContext) throws Exception {
        ProgramState state = new ProgramState();
        mainline(state);
        return RepeatStatus.FINISHED;
    }

    // =========== FILE I/O ===========
    private void readTen99t01rmisctransfile(ProgramState state) {
        try {
            if (state.ten99T01rMiscTransFileReader == null) { state.ten99T01rMiscTransFileEof = true; return; }
            state.ten99T01rMiscTransFileLine = state.ten99T01rMiscTransFileReader.readLine();
            if (state.ten99T01rMiscTransFileLine == null) state.ten99T01rMiscTransFileEof = true;
        } catch (Exception e) { throw new RuntimeException("Read error", e); }
    }

    private void writeTen99t01rmisctransfile(ProgramState state, String line) {
        try {
            if (state.ten99T01rMiscTransFileWriter != null && line != null) {
                state.ten99T01rMiscTransFileWriter.write(line);
                state.ten99T01rMiscTransFileWriter.newLine();
            }
        } catch (Exception e) { throw new RuntimeException("Write error", e); }
    }

    private void readTen99t03rbccwfile(ProgramState state) {
        try {
            if (state.ten99T03rBccwFileReader == null) { state.ten99T03rBccwFileEof = true; return; }
            state.ten99T03rBccwFileLine = state.ten99T03rBccwFileReader.readLine();
            if (state.ten99T03rBccwFileLine == null) state.ten99T03rBccwFileEof = true;
        } catch (Exception e) { throw new RuntimeException("Read error", e); }
    }

    private void writeTen99t03rbccwfile(ProgramState state, String line) {
        try {
            if (state.ten99T03rBccwFileWriter != null && line != null) {
                state.ten99T03rBccwFileWriter.write(line);
                state.ten99T03rBccwFileWriter.newLine();
            }
        } catch (Exception e) { throw new RuntimeException("Write error", e); }
    }

    private void readTen99t04rdeftfile(ProgramState state) {
        try {
            if (state.ten99T04rDeftFileReader == null) { state.ten99T04rDeftFileEof = true; return; }
            state.ten99T04rDeftFileLine = state.ten99T04rDeftFileReader.readLine();
            if (state.ten99T04rDeftFileLine == null) state.ten99T04rDeftFileEof = true;
        } catch (Exception e) { throw new RuntimeException("Read error", e); }
    }

    private void writeTen99t04rdeftfile(ProgramState state, String line) {
        try {
            if (state.ten99T04rDeftFileWriter != null && line != null) {
                state.ten99T04rDeftFileWriter.write(line);
                state.ten99T04rDeftFileWriter.newLine();
            }
        } catch (Exception e) { throw new RuntimeException("Write error", e); }
    }

    private void readTen99t05rrejcyclefile(ProgramState state) {
        try {
            if (state.ten99T05rRejCycleFileReader == null) { state.ten99T05rRejCycleFileEof = true; return; }
            state.ten99T05rRejCycleFileLine = state.ten99T05rRejCycleFileReader.readLine();
            if (state.ten99T05rRejCycleFileLine == null) state.ten99T05rRejCycleFileEof = true;
        } catch (Exception e) { throw new RuntimeException("Read error", e); }
    }

    private void writeTen99t05rrejcyclefile(ProgramState state, String line) {
        try {
            if (state.ten99T05rRejCycleFileWriter != null && line != null) {
                state.ten99T05rRejCycleFileWriter.write(line);
                state.ten99T05rRejCycleFileWriter.newLine();
            }
        } catch (Exception e) { throw new RuntimeException("Write error", e); }
    }

    private void readWst07r1099entrycdfile(ProgramState state) {
        try {
            if (state.wsT07r1099entryCdFileReader == null) { state.wsT07r1099entryCdFileEof = true; return; }
            state.wsT07r1099entryCdFileLine = state.wsT07r1099entryCdFileReader.readLine();
            if (state.wsT07r1099entryCdFileLine == null) state.wsT07r1099entryCdFileEof = true;
        } catch (Exception e) { throw new RuntimeException("Read error", e); }
    }

    private void writeWst07r1099entrycdfile(ProgramState state, String line) {
        try {
            if (state.wsT07r1099entryCdFileWriter != null && line != null) {
                state.wsT07r1099entryCdFileWriter.write(line);
                state.wsT07r1099entryCdFileWriter.newLine();
            }
        } catch (Exception e) { throw new RuntimeException("Write error", e); }
    }

    private void readTen99t01wmisctransoutfile(ProgramState state) {
        try {
            if (state.ten99T01wMiscTransOutFileReader == null) { state.ten99T01wMiscTransOutFileEof = true; return; }
            state.ten99T01wMiscTransOutFileLine = state.ten99T01wMiscTransOutFileReader.readLine();
            if (state.ten99T01wMiscTransOutFileLine == null) state.ten99T01wMiscTransOutFileEof = true;
        } catch (Exception e) { throw new RuntimeException("Read error", e); }
    }

    private void writeTen99t01wmisctransoutfile(ProgramState state, String line) {
        try {
            if (state.ten99T01wMiscTransOutFileWriter != null && line != null) {
                state.ten99T01wMiscTransOutFileWriter.write(line);
                state.ten99T01wMiscTransOutFileWriter.newLine();
            }
        } catch (Exception e) { throw new RuntimeException("Write error", e); }
    }

    private void readTen99t03wbccwoutfile(ProgramState state) {
        try {
            if (state.ten99T03wBccwOutFileReader == null) { state.ten99T03wBccwOutFileEof = true; return; }
            state.ten99T03wBccwOutFileLine = state.ten99T03wBccwOutFileReader.readLine();
            if (state.ten99T03wBccwOutFileLine == null) state.ten99T03wBccwOutFileEof = true;
        } catch (Exception e) { throw new RuntimeException("Read error", e); }
    }

    private void writeTen99t03wbccwoutfile(ProgramState state, String line) {
        try {
            if (state.ten99T03wBccwOutFileWriter != null && line != null) {
                state.ten99T03wBccwOutFileWriter.write(line);
                state.ten99T03wBccwOutFileWriter.newLine();
            }
        } catch (Exception e) { throw new RuntimeException("Write error", e); }
    }

    private void readTen99t04wdeftoutfile(ProgramState state) {
        try {
            if (state.ten99T04wDeftOutFileReader == null) { state.ten99T04wDeftOutFileEof = true; return; }
            state.ten99T04wDeftOutFileLine = state.ten99T04wDeftOutFileReader.readLine();
            if (state.ten99T04wDeftOutFileLine == null) state.ten99T04wDeftOutFileEof = true;
        } catch (Exception e) { throw new RuntimeException("Read error", e); }
    }

    private void writeTen99t04wdeftoutfile(ProgramState state, String line) {
        try {
            if (state.ten99T04wDeftOutFileWriter != null && line != null) {
                state.ten99T04wDeftOutFileWriter.write(line);
                state.ten99T04wDeftOutFileWriter.newLine();
            }
        } catch (Exception e) { throw new RuntimeException("Write error", e); }
    }

    private void readTen99t05wrejcycleoutfile(ProgramState state) {
        try {
            if (state.ten99T05wRejCycleOutFileReader == null) { state.ten99T05wRejCycleOutFileEof = true; return; }
            state.ten99T05wRejCycleOutFileLine = state.ten99T05wRejCycleOutFileReader.readLine();
            if (state.ten99T05wRejCycleOutFileLine == null) state.ten99T05wRejCycleOutFileEof = true;
        } catch (Exception e) { throw new RuntimeException("Read error", e); }
    }

    private void writeTen99t05wrejcycleoutfile(ProgramState state, String line) {
        try {
            if (state.ten99T05wRejCycleOutFileWriter != null && line != null) {
                state.ten99T05wRejCycleOutFileWriter.write(line);
                state.ten99T05wRejCycleOutFileWriter.newLine();
            }
        } catch (Exception e) { throw new RuntimeException("Write error", e); }
    }

    private void readCcr01rcontrolcard(ProgramState state) {
        try {
            if (state.ccR01rControlCardReader == null) { state.ccR01rControlCardEof = true; return; }
            state.ccR01rControlCardLine = state.ccR01rControlCardReader.readLine();
            if (state.ccR01rControlCardLine == null) state.ccR01rControlCardEof = true;
        } catch (Exception e) { throw new RuntimeException("Read error", e); }
    }

    private void writeCcr01rcontrolcard(ProgramState state, String line) {
        try {
            if (state.ccR01rControlCardWriter != null && line != null) {
                state.ccR01rControlCardWriter.write(line);
                state.ccR01rControlCardWriter.newLine();
            }
        } catch (Exception e) { throw new RuntimeException("Write error", e); }
    }

    // =========== BUSINESS LOGIC ===========

    private void mainline(ProgramState state) {
        // ===== Original COBOL: 0000-MAINLINE =====
        // 076300***************************************************************** 12630001
        // PERFORM 1000-INITIALIZATION
        // PERFORM 3000-MAIN-PROCESS
        // PERFORM 9000-TERMINATION-ROUTINE
        // GOBACK
        // .
        // EJECT
        // 077000***************************************************************** 12700001
        // ===== End COBOL =====
        
        try {
            initialization(state);
            mainProcess(state);
            terminationRoutine(state);
            return;
        } catch (Exception e) {
            throw new RuntimeException("Error in mainline: " + e.getMessage(), e);
        }
    }


    private void initialization(ProgramState state) {
        // ===== Original COBOL: 1000-INITIALIZATION =====
        // 077200***************************************************************** 12720001
        // OPEN INPUT
        // TEN99-T01R-MISC-TRANS-FILE
        // TEN99-T03R-BCCW-FILE
        // TEN99-T04R-DEFT-FILE
        // TEN99-T05R-REJ-CYCLE-FILE
        // WS-T07R-1099ENTRY-CD-FILE
        // CC-R01R-CONTROL-CARD
        // OUTPUT
        // TEN99-T01W-MISC-TRANS-OUT-FILE
        // TEN99-T03W-BCCW-OUT-FILE
        // TEN99-T04W-DEFT-OUT-FILE
        // TEN99-T05W-REJ-CYCLE-OUT-FILE
        // CC-E01W-DISPLAY-FILE
        // PERFORM 1130-INCLUDE-SYSOUT-DISPLAY
        // PERFORM 7900-READ-CONTROL-CARD-IN
        // INITIALIZE             WS-OUTPUT-TRANS-RCD
        // .
        // 079300***************************************************************** 13310001
        // ===== End COBOL =====
        
        try {
            includeSysoutDisplay(state);
            readControlCardIn(state);
            state.wsOutputTransRcd = null;
        } catch (Exception e) {
            throw new RuntimeException("Error in initialization: " + e.getMessage(), e);
        }
    }


    private void includeSysoutDisplay(ProgramState state) {
        // ===== Original COBOL: 1130-INCLUDE-SYSOUT-DISPLAY =====
        // 079500***************************************************************** 13330001
        // 079600*    ++INCLUDE C2INZ001                                         * 13340001
        // 079700*      COMMON INITIAL SYSOUT DISPLAYS                           * 13350001
        // 079800***************************************************************** 13360001
        // COPY C2INZ001.
        // EJECT
        // 080200***************************************************************** 13400001
        // ===== End COBOL =====
        
        try {
            initialization(state);
        } catch (Exception e) {
            throw new RuntimeException("Error in includeSysoutDisplay: " + e.getMessage(), e);
        }
    }


    private void mainProcess(ProgramState state) {
        // ===== Original COBOL: 3000-MAIN-PROCESS =====
        // 080400***************************************************************** 13420001
        // 080500*      MAIN PROCESSING ROUTINE                                  * 13430001
        // 080600***************************************************************** 13440001
        // PERFORM 7910-READ-1099ENTRY-TABLE
        // VARYING CODE-INDEX FROM 1 BY 1
        // UNTIL CODE-INDEX GREATER THAN LIT-NUM-OF-1099-CODES
        // 081000***************************************************************** 13480001
        // 081100*      DETERMINE THE PROCESSING SCHEDULE BASED ON TAX MONTH     * 13490001
        // 081200*      VERIFY EACH INPUT HEADER RECORD TO ENSURE CORRECT FILES  * 13500001
        // 081300*      PROCESS THE DETAIL AND TRAILER FOR EACH FILE             * 13510001
        // 081400***************************************************************** 13520001
        // EVALUATE TRUE
        // WHEN CC-TAX-MM1 = LIT-TAX-MONTH-JAN
        // PERFORM 3002-JAN-MONTH-PROCESSING
        // WHEN CC-TAX-MM1 > LIT-TAX-MONTH-JAN AND
        // < LIT-TAX-MONTH-YEAR-END
        // PERFORM 3004-FEB-DEC-MONTH-PROCESSING
        // WHEN CC-TAX-MM1 > LIT-TAX-MONTH-DEC
        // PERFORM 3006-YEAR-END-PROCESSING
        // END-EVALUATE
        // .
        // EJECT
        // 082800***************************************************************** 13660001
        // ===== End COBOL =====
        
        try {
            for (int codeIndex = 1; codeIndex <= state.litNumOf1099Codes; codeIndex++) {
                // TODO: unknown method read1099EntryTable(state, codeIndex);
            }
            if ("JAN".equals(state.ccTaxMm1)) {
                janMonthProcessing(state);
            } else if (state.ccTaxMm1.compareTo("JAN") > 0 && state.ccTaxMm1.compareTo("YEAR-END") < 0) {
                febDecMonthProcessing(state);
            } else if (state.ccTaxMm1.compareTo("DEC") > 0) {
                yearEndProcessing(state);
            }
        } catch (Exception e) {
            throw new RuntimeException("Error in mainProcess: " + e.getMessage(), e);
        }
    }


    private void janMonthProcessing(ProgramState state) {
        // ===== Original COBOL: 3002-JAN-MONTH-PROCESSING =====
        // 083000***************************************************************** 13680001
        // 083100*   PROCESSING SCHEDULE FOR JANUARY                             * 13690001
        // 083200***************************************************************** 13700001
        // PERFORM 4000-MISC-FORM-VERIFY-PROCESS
        // PERFORM 4200-BCCW-VERIFY-PROCESS
        // PERFORM 4300-DEFT-VERIFY-PROCESS
        // IF ERROR-FLAG = "Y"
        // MOVE LIT-ERROR-FLAG-EQUALS-YES TO CC-E01W-DISPLAY-RCD
        // PERFORM 8999-WRITE-SYSOUT
        // PERFORM 9998-COREDUMP
        // END-IF
        // PERFORM 5000-MISC-FORM-PROCESS
        // PERFORM 5200-BCCW-PROCESS
        // PERFORM 5400-DEFT-PROCESS
        // .
        // EJECT
        // 084600***************************************************************** 13840001
        // ===== End COBOL =====
        
        try {
            miscFormVerifyProcess(state);
            bccwVerifyProcess(state);
            deftVerifyProcess(state);
            if ("Y".equals(state.errorFlag)) {
                state.ccE01wDisplayRcd = state.litErrorFlagEqualsYes;
                includeSysoutDisplay(state);
                mainline(state); // Assuming 9998-COREDUMP maps to mainline or similar error handling
            }
            miscFormProcess(state);
            bccwProcess(state);
            deftProcess(state);
        } catch (Exception e) {
            throw new RuntimeException("Error in janMonthProcessing: " + e.getMessage(), e);
        }
    }


    private void febDecMonthProcessing(ProgramState state) {
        // ===== Original COBOL: 3004-FEB-DEC-MONTH-PROCESSING =====
        // 084800***************************************************************** 13860001
        // 084900*   PROCESSING SCHEDULE FOR FEBRUARY THROUGH DECEMBER           * 13870001
        // 085000***************************************************************** 13880001
        // PERFORM 4000-MISC-FORM-VERIFY-PROCESS
        // PERFORM 4200-BCCW-VERIFY-PROCESS
        // PERFORM 4300-DEFT-VERIFY-PROCESS
        // PERFORM 4500-REJ-CYCLE-VERIFY-PROCESS
        // IF ERROR-FLAG = "Y"
        // MOVE LIT-ERROR-FLAG-EQUALS-YES TO CC-E01W-DISPLAY-RCD
        // PERFORM 8999-WRITE-SYSOUT
        // PERFORM 9998-COREDUMP
        // END-IF
        // PERFORM 5000-MISC-FORM-PROCESS
        // PERFORM 5200-BCCW-PROCESS
        // PERFORM 5400-DEFT-PROCESS
        // PERFORM 5500-REJ-CYCLE-PROCESS
        // .
        // EJECT
        // 086700***************************************************************** 14050001
        // ===== End COBOL =====
        
        try {
            miscFormVerifyProcess(state);
            bccwVerifyProcess(state);
            deftVerifyProcess(state);
            rejCycleVerifyProcess(state);
            if ("Y".equals(state.errorFlag)) {
                state.ccE01wDisplayRcd = state.litErrorFlagEqualsYes;
                includeSysoutDisplay(state);
                yearEndProcessing(state);
            }
            miscFormProcess(state);
            bccwProcess(state);
            deftProcess(state);
            rejCycleProcess(state);
        } catch (Exception e) {
            throw new RuntimeException("Error in febDecMonthProcessing: " + e.getMessage(), e);
        }
    }


    private void yearEndProcessing(ProgramState state) {
        // ===== Original COBOL: 3006-YEAR-END-PROCESSING =====
        // 086900***************************************************************** 14070001
        // 087000*   PROCESSING SCHEDULE FOR YEAR-END                            * 14080001
        // 087100***************************************************************** 14090001
        // PERFORM 4000-MISC-FORM-VERIFY-PROCESS
        // PERFORM 4500-REJ-CYCLE-VERIFY-PROCESS
        // IF ERROR-FLAG = "Y"
        // MOVE LIT-ERROR-FLAG-EQUALS-YES TO CC-E01W-DISPLAY-RCD
        // PERFORM 8999-WRITE-SYSOUT
        // PERFORM 9998-COREDUMP
        // END-IF
        // PERFORM 5000-MISC-FORM-PROCESS
        // PERFORM 5500-REJ-CYCLE-PROCESS
        // .
        // EJECT
        // 088300***************************************************************** 14210001
        // ===== End COBOL =====
        
        try {
            miscFormVerifyProcess(state);
            rejCycleVerifyProcess(state);
            if ("Y".equals(state.errorFlag)) {
                state.ccE01wDisplayRcd = state.litErrorFlagEqualsYes;
                includeSysoutDisplay(state);
                // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method coredump(state);
            }
            miscFormProcess(state);
            rejCycleProcess(state);
        } catch (Exception e) {
            throw new RuntimeException("Error in yearEndProcessing: " + e.getMessage(), e);
        }
    }


    private void miscFormVerifyProcess(ProgramState state) {
        // ===== Original COBOL: 4000-MISC-FORM-VERIFY-PROCESS =====
        // 088500***************************************************************** 14230001
        // 088600*   TEST MISCELLANEOUS FORM 20071 INPUT HEADER FOR BAD HEADER   * 14240001
        // 088700*   OR BAD CONTROL DATE.                                        * 14250001
        // 088800***************************************************************** 14260001
        // PERFORM 7000-MISC-FORM-READ-RCD
        // IF TEN99-T01R-MISC-FORM-EOF-YES
        // MOVE LIT-MISC-FORM TO MSG-NO-DATA-FILE-NAME
        // MOVE MSG-NO-DATA   TO CC-E01W-DISPLAY-RCD
        // PERFORM 8999-WRITE-SYSOUT
        // MOVE "Y" TO ERROR-FLAG
        // ELSE
        // IF WS-T01R-MISC-FORM-HDR-VALUE = LOW-VALUES AND
        // WS-T01R-MISC-FORM-HDR-NAME  = LIT-MISC-FORM-HDR
        // IF WS-T01R-MISC-FORM-HDR-YY = CC-TAX-YY1 AND
        // WS-T01R-MISC-FORM-HDR-MM = CC-TAX-MM1
        // MOVE WS-T01R-MISC-FORM-RCD TO WS-OUTPUT-TRANS-RCD
        // PERFORM 8010-WRITE-MISC-TRANS-RCD
        // ELSE
        // MOVE LIT-MISC-FORM      TO MSG-FILE-NAME
        // MOVE MSG-INCORRECT-DATE TO CC-E01W-DISPLAY-RCD
        // PERFORM 8999-WRITE-SYSOUT
        // MOVE "Y" TO ERROR-FLAG
        // END-IF
        // ELSE
        // MOVE LIT-MISC-FORM TO MSG-FILE-ID
        // MOVE MSG-BAD-FILE  TO CC-E01W-DISPLAY-RCD
        // PERFORM 8999-WRITE-SYSOUT
        // MOVE "Y" TO ERROR-FLAG
        // END-IF
        // END-IF
        // .
        // EJECT
        // 091700***************************************************************** 14540001
        // 091800***************************************************************** 14550001
        // ===== End COBOL =====
        
        try {
            miscFormReadRcd(state);
            if ("Y".equals(state.ten99T01rMiscFormEofYes)) {
                state.msgNoDataFileName = state.litMiscForm;
                state.ccE01wDisplayRcd = state.msgNoData;
                // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method includeSysoutDisplay(state);
                state.errorFlag = "Y";
            } else {
                if ("\u0000".equals(state.wsT01rMiscFormHdrValue) && state.wsT01rMiscFormHdrName.equals(state.litMiscFormHdr)) {
                    if (state.wsT01rMiscFormHdrYy.equals(state.ccTaxYy1) && state.wsT01rMiscFormHdrMm.equals(state.ccTaxMm1)) {
                        state.wsOutputTransRcd = state.wsT01rMiscFormRcd;
                        writeMiscTransRcd(state);
                    } else {
                        state.msgFileName = state.litMiscForm;
                        state.ccE01wDisplayRcd = state.msgIncorrectDate;
                        includeSysoutDisplay(state);
                        state.errorFlag = "Y";
                    }
                } else {
                    state.msgFileId = state.litMiscForm;
                    state.ccE01wDisplayRcd = state.msgBadFile;
                    includeSysoutDisplay(state);
                    state.errorFlag = "Y";
                }
            }
        } catch (Exception e) {
            throw new RuntimeException("Error in miscFormVerifyProcess: " + e.getMessage(), e);
        }
    }


    private void bccwVerifyProcess(ProgramState state) {
        // ===== Original COBOL: 4200-BCCW-VERIFY-PROCESS =====
        // 092000***************************************************************** 14570001
        // 092100*   TEST B&CCW FILE FOR NO DATA OR NO HEADER                    * 14580001
        // 092200***************************************************************** 14590001
        // PERFORM 7200-BCCW-READ-RCD
        // IF TEN99-T03R-BCCW-EOF-YES
        // MOVE LIT-BCCW    TO MSG-NO-DATA-FILE-NAME
        // MOVE MSG-NO-DATA TO CC-E01W-DISPLAY-RCD
        // MOVE "Y"         TO ERROR-FLAG
        // PERFORM 8999-WRITE-SYSOUT
        // ELSE
        // PERFORM 4210-BCCW-VERIFY
        // PERFORM 7200-BCCW-READ-RCD
        // END-IF
        // .
        // 093400***************************************************************** 14730001
        // ===== End COBOL =====
        
        try {
            bccwProcessRcd(state);
            if ("Y".equals(state.ten99T03rBccwEofYes)) {
                state.msgNoDataFileName = state.litBccw;
                state.ccE01wDisplayRcd = state.msgNoData;
                state.errorFlag = "Y";
                includeSysoutDisplay(state);
            } else {
                bccwVerify(state);
                bccwProcessRcd(state);
            }
        } catch (Exception e) {
            throw new RuntimeException("Error in bccwVerifyProcess: " + e.getMessage(), e);
        }
    }


    private void bccwVerify(ProgramState state) {
        // ===== Original COBOL: 4210-BCCW-VERIFY =====
        // 093600***************************************************************** 14750001
        // 093700*   TEST THE HEADERS FOR NO DATA OR BAD CONTROL DATE            * 14760001
        // 093800*   NOTE:THERE ARE MULTIPLE HEADERS & TRAILERS ON B&CCW INPUT   * 14760001
        // 093900*        FILE. THE HEADERS & TRAILERS ARE DISPERSED THROUGHOUT  * 14760001
        // 094000*        THE FILE.                                              * 14760001
        // 094100***************************************************************** 14780001
        // IF TEN99-FILE-ID-HDR      = LIT-BCCW-FILE
        // IF TEN99-YY-HDR        = CC-TAX-YY1 AND
        // TEN99-CC-HDR        = CC-TAX-CC1 AND
        // TEN99-MM-HDR        = CC-TAX-MM1
        // MOVE TEN99-RECORD-HDR TO WS-OUTPUT-TRANS-RCD
        // INITIALIZE TEN99-RECORD-HDR
        // ELSE
        // MOVE LIT-BCCW           TO MSG-FILE-NAME
        // MOVE MSG-INCORRECT-DATE TO CC-E01W-DISPLAY-RCD
        // MOVE "Y"                TO ERROR-FLAG
        // PERFORM 8999-WRITE-SYSOUT
        // MOVE TEN99-RECORD-HDR TO WS-OUTPUT-TRANS-RCD
        // END-IF
        // ELSE
        // MOVE LIT-BCCW     TO MSG-FILE-ID
        // MOVE MSG-BAD-FILE TO CC-E01W-DISPLAY-RCD
        // MOVE "Y"          TO ERROR-FLAG
        // PERFORM 8999-WRITE-SYSOUT
        // MOVE TEN99-RECORD-HDR TO WS-OUTPUT-TRANS-RCD
        // END-IF
        // PERFORM 8020-WRITE-BCCW-TRANS-RCD
        // .
        // 096400***************************************************************** 14990001
        // ===== End COBOL =====
        
        try {
            if ("LIT_BCCW_FILE".equals(state.ten99FileIdHdr)) {
                if ("CC_TAX_YY1".equals(state.ten99YyHdr) && "CC_TAX_CC1".equals(state.ten99CcHdr) && "CC_TAX_MM1".equals(state.ten99MmHdr)) {
                    state.wsOutputTransRcd = state.ten99RecordHdr;
                    initialization(state);
                } else {
                    state.msgFileName = "LIT_BCCW";
                    state.ccE01wDisplayRcd = state.msgIncorrectDate;
                    state.errorFlag = "Y";
                    includeSysoutDisplay(state);
                    state.wsOutputTransRcd = state.ten99RecordHdr;
                }
            } else {
                state.msgFileId = "LIT_BCCW";
                state.ccE01wDisplayRcd = state.msgBadFile;
                state.errorFlag = "Y";
                includeSysoutDisplay(state);
                state.wsOutputTransRcd = state.ten99RecordHdr;
            }
            mainProcess(state);
        } catch (Exception e) {
            throw new RuntimeException("Error in bccwVerify: " + e.getMessage(), e);
        }
    }


    private void deftVerifyProcess(ProgramState state) {
        // ===== Original COBOL: 4300-DEFT-VERIFY-PROCESS =====
        // 096600***************************************************************** 15010001
        // 096700*   TEST DEFT  FILE FOR NO DATA OR NO HEADER                    * 15020001
        // 096800***************************************************************** 15030001
        // PERFORM 7300-DEFT-READ-RCD
        // IF TEN99-T04R-DEFT-EOF-YES
        // MOVE LIT-DEFT    TO MSG-NO-DATA-FILE-NAME
        // MOVE MSG-NO-DATA TO CC-E01W-DISPLAY-RCD
        // MOVE "Y"         TO ERROR-FLAG
        // PERFORM 8999-WRITE-SYSOUT
        // ELSE
        // PERFORM 4310-DEFT-VERIFY
        // PERFORM 7300-DEFT-READ-RCD
        // END-IF
        // .
        // 098000***************************************************************** 15170001
        // ===== End COBOL =====
        
        try {
            deftReadRcd(state);
            if ("Y".equals(state.ten99T04rDeftEof)) {
                state.msgNoDataFileName = state.litDeft;
                state.ccE01wDisplayRcd = state.msgNoData;
                state.errorFlag = "Y";
                includeSysoutDisplay(state);
            } else {
                deftVerify(state);
                deftReadRcd(state);
            }
        } catch (Exception e) {
            throw new RuntimeException("Error in deftVerifyProcess: " + e.getMessage(), e);
        }
    }


    private void deftVerify(ProgramState state) {
        // ===== Original COBOL: 4310-DEFT-VERIFY =====
        // 098200***************************************************************** 15190001
        // 098300*   TEST THE HEADER FOR NO DATA OR BAD CONTROL DATE             * 15200001
        // 098400***************************************************************** 15210001
        // IF WS-T04R-DEFT-HDR-ID    = LIT-DEFT-FILE
        // IF WS-T04R-DEFT-HDR-YY = CC-TAX-YY1 AND
        // WS-T04R-DEFT-HDR-CC = CC-TAX-CC1 AND
        // WS-T04R-DEFT-HDR-MM = CC-TAX-MM1
        // MOVE WS-T04R-DEFT-HDR TO WS-OUTPUT-TRANS-RCD
        // PERFORM 8040-WRITE-DEFT-TRANS-RCD
        // ELSE
        // MOVE LIT-DEFT           TO MSG-FILE-NAME
        // MOVE MSG-INCORRECT-DATE TO CC-E01W-DISPLAY-RCD
        // MOVE "Y"                TO ERROR-FLAG
        // PERFORM 8999-WRITE-SYSOUT
        // END-IF
        // ELSE
        // MOVE LIT-DEFT              TO MSG-FILE-ID
        // MOVE MSG-BAD-FILE          TO CC-E01W-DISPLAY-RCD
        // MOVE "Y"                   TO ERROR-FLAG
        // PERFORM 8999-WRITE-SYSOUT
        // END-IF
        // .
        // EJECT
        // 100500***************************************************************** 15420001
        // ===== End COBOL =====
        
        try {
            if ("LIT-DEFT-FILE".equals(state.wsT04rDeftHdrId)) {
                if ("CC-TAX-YY1".equals(state.wsT04rDeftHdrYy) && "CC-TAX-CC1".equals(state.wsT04rDeftHdrCc) && "CC-TAX-MM1".equals(state.wsT04rDeftHdrMm)) {
                    state.wsOutputTransRcd = state.wsT04rDeftHdr;
                    writeDeftTransRcd(state);
                } else {
                    state.msgFileName = "LIT-DEFT";
                    state.ccE01wDisplayRcd = "MSG-INCORRECT-DATE";
                    state.errorFlag = "Y";
                    includeSysoutDisplay(state);
                }
            } else {
                state.msgFileId = "LIT-DEFT";
                state.ccE01wDisplayRcd = "MSG-BAD-FILE";
                state.errorFlag = "Y";
                includeSysoutDisplay(state);
            }
        } catch (Exception e) {
            throw new RuntimeException("Error in deftVerify: " + e.getMessage(), e);
        }
    }


    private void rejCycleVerifyProcess(ProgramState state) {
        // ===== Original COBOL: 4500-REJ-CYCLE-VERIFY-PROCESS =====
        // 100700***************************************************************** 15440001
        // 100800*   TEST FOR EMPTY FILE OR BAD CONTROL DATE                     * 15450001
        // 100900***************************************************************** 15460001
        // PERFORM 7500-REJ-CYCLE-READ-RCD
        // IF TEN99-T05R-REJ-CYCLE-EOF-YES
        // MOVE LIT-REJECT-FILE TO MSG-NO-DATA-FILE-NAME
        // MOVE MSG-NO-DATA TO CC-E01W-DISPLAY-RCD
        // MOVE "Y"         TO ERROR-FLAG
        // PERFORM 8999-WRITE-SYSOUT
        // ELSE
        // IF WS-T05R-REJ-CYCLE-HDR-VALUE = LOW-VALUES AND
        // WS-T05R-REJ-CYCLE-HDR-ID = LIT-TEN99-REJECT-TRANS
        // IF WS-T05R-REJ-CYCLE-HDR-YY = CC-TAX-YY1 AND
        // WS-T05R-REJ-CYCLE-HDR-CC = CC-TAX-CC1 AND
        // WS-T05R-REJ-CYCLE-HDR-MM = CC-TAX-MM1
        // MOVE WS-T05R-REJECT-RECYCLING-RCD TO
        // TEN99-T05W-REJ-CYCLE-OUT-RCD
        // PERFORM 8030-WRITE-REJECT-RCD
        // ELSE
        // MOVE LIT-REJECT-FILE    TO MSG-FILE-NAME
        // MOVE MSG-INCORRECT-DATE TO CC-E01W-DISPLAY-RCD
        // MOVE "Y"                TO ERROR-FLAG
        // PERFORM 8999-WRITE-SYSOUT
        // END-IF
        // ELSE
        // MOVE LIT-REJECT-FILE TO MSG-FILE-ID
        // MOVE MSG-BAD-FILE    TO CC-E01W-DISPLAY-RCD
        // MOVE "Y"             TO ERROR-FLAG
        // PERFORM 8999-WRITE-SYSOUT
        // END-IF
        // END-IF
        // .
        // EJECT
        // 104000***************************************************************** 15750001
        // ===== End COBOL =====
        
        try {
            rejCycleProcessRcd(state);
            if ("Y".equals(state.ten99T05rRejCycleEofYes)) {
                state.msgNoDataFileName = state.litRejectFile;
                state.ccE01wDisplayRcd = state.msgNoData;
                state.errorFlag = "Y";
                includeSysoutDisplay(state);
            } else {
                if ("\u0000".equals(state.wsT05rRejCycleHdrValue) && state.wsT05rRejCycleHdrId.equals(state.litTen99RejectTrans)) {
                    if (state.wsT05rRejCycleHdrYy.equals(state.ccTaxYy1) && state.wsT05rRejCycleHdrCc.equals(state.ccTaxCc1) && state.wsT05rRejCycleHdrMm.equals(state.ccTaxMm1)) {
                        state.ten99T05wRejCycleOutRcd = state.wsT05rRejectRecyclingRcd;
                        rejCycleProcess(state);
                    } else {
                        state.msgFileName = state.litRejectFile;
                        state.ccE01wDisplayRcd = state.msgIncorrectDate;
                        state.errorFlag = "Y";
                        includeSysoutDisplay(state);
                    }
                } else {
                    state.msgFileId = state.litRejectFile;
                    state.ccE01wDisplayRcd = state.msgBadFile;
                    state.errorFlag = "Y";
                    includeSysoutDisplay(state);
                }
            }
        } catch (Exception e) {
            throw new RuntimeException("Error in rejCycleVerifyProcess: " + e.getMessage(), e);
        }
    }


    private void miscFormProcess(ProgramState state) {
        // ===== Original COBOL: 5000-MISC-FORM-PROCESS =====
        // 104200***************************************************************** 15770001
        // 104300*      PRIMING READ AND EOF TEST FOR MISC FORM INPUT            * 15780001
        // 104400*      EMPTY FILE SINCE 5/2009                                  * 15780001
        // 104500*      WRITE TRAILER RECORD                                     * 15780001
        // 104600***************************************************************** 15790001
        // INITIALIZE WS-MISC-FORM-SEQ-CNT
        // PERFORM 6500-INITIAL-OUTPUT-COUNT
        // PERFORM 7000-MISC-FORM-READ-RCD
        // 105000***************************************************************** 15830001
        // 105100*   TEST FOR NO DATA ON FILE                                    * 15840001
        // 105200***************************************************************** 15850001
        // IF TEN99-T01R-MISC-FORM-EOF-YES OR
        // WS-T01R-MISC-FORM-TRL-VALUE = HIGH-VALUES
        // MOVE LIT-MISC-FORM TO MSG-NO-DATA-FILE-NAME
        // MOVE MSG-NO-DATA TO CC-E01W-DISPLAY-RCD
        // PERFORM 8999-WRITE-SYSOUT
        // PERFORM 5040-MISC-FORM-RESULTS-PROCESS
        // IF WS-T01R-MISC-FORM-TRL-VALUE = HIGH-VALUES
        // MOVE WS-T01R-MISC-FORM-RCD      TO WS-OUTPUT-TRANS-RCD
        // PERFORM 8010-WRITE-MISC-TRANS-RCD
        // END-IF
        // ELSE
        // PERFORM 5010-MISC-FORM-PROCESS-RCD UNTIL
        // TEN99-T01R-MISC-FORM-EOF-YES  OR
        // WS-T01R-MISC-FORM-TRL-VALUE = HIGH-VALUES
        // PERFORM 5040-MISC-FORM-RESULTS-PROCESS
        // END-IF
        // .
        // EJECT
        // 107200***************************************************************** 16010001
        // ===== End COBOL =====
        
        try {
            state.wsMiscFormSeqCnt = 0;
            initialOutputCount(state);
            miscFormReadRcd(state);
            if ("Y".equals(state.ten99T01rMiscFormEofYes) || "\uFFFF".equals(state.wsT01rMiscFormTrlValue)) {
                state.msgNoDataFileName = state.litMiscForm;
                state.ccE01wDisplayRcd = state.msgNoData;
                includeSysoutDisplay(state);
                miscFormResultsProcess(state);
                if ("\uFFFF".equals(state.wsT01rMiscFormTrlValue)) {
                    state.wsOutputTransRcd = state.wsT01rMiscFormRcd;
                    writeMiscTransRcd(state);
                }
            } else {
                while (!"Y".equals(state.ten99T01rMiscFormEofYes) && !"\uFFFF".equals(state.wsT01rMiscFormTrlValue)) {
                    miscFormProcessRcd(state);
                }
                miscFormResultsProcess(state);
            }
        } catch (Exception e) {
            throw new RuntimeException("Error in miscFormProcess: " + e.getMessage(), e);
        }
    }


    private void miscFormProcessRcd(ProgramState state) {
        // ===== Original COBOL: 5010-MISC-FORM-PROCESS-RCD =====
        // 107400***************************************************************** 16030001
        // 107500*   PROCESS THE DETAIL RECORDS FOR THE MISC FORM INPUT            16040001
        // 107600*   INCREMENT COUNTERS FOR TRAILER TESTING                        16050001
        // 107700***************************************************************** 16060001
        // ADD LIT-1                 TO WS-MISC-FORM-TRANS-COUNTER
        // ADD WS-T01R-MISC-FORM-AMT TO WS-MISC-FORM-TRANS-AMT-ACCUM
        // PERFORM 5020-MISC-FORM-SET-TAX-TYPE
        // PERFORM 7000-MISC-FORM-READ-RCD
        // .
        // EJECT
        // 108500***************************************************************** 16150001
        // ===== End COBOL =====
        
        try {
            state.wsMiscFormTransCounter = state.wsMiscFormTransCounter + 1;
            state.wsMiscFormTransAmtAccum = state.wsMiscFormTransAmtAccum.add(state.wsT01rMiscFormAmt);
            miscFormSetTaxType(state);
            miscFormReadRcd(state);
        } catch (Exception e) {
            throw new RuntimeException("Error in miscFormProcessRcd: " + e.getMessage(), e);
        }
    }


    private void miscFormSetTaxType(ProgramState state) {
        // ===== Original COBOL: 5020-MISC-FORM-SET-TAX-TYPE =====
        // 108700***************************************************************** 16170001
        // 108800*      UPDATE THE TAX TYPE FOR DETAIL RECORDS                   * 16180001
        // 108900***************************************************************** 16190001
        // EVALUATE TRUE
        // WHEN WS-T01R-MISC-FORM-HDR-VALUE = LOW-VALUES
        // CONTINUE
        // WHEN WS-T01R-MISC-FORM-TRL-VALUE = HIGH-VALUES
        // CONTINUE
        // WHEN WS-T01R-MISC-FORM-VALID-TAX
        // CONTINUE
        // WHEN OTHER
        // MOVE LIT-3 TO WS-T01R-MISC-FORM-TAX-TYPE
        // END-EVALUATE
        // MOVE WS-T01R-MISC-FORM-RCD      TO WS-OUTPUT-TRANS-RCD
        // PERFORM 8010-WRITE-MISC-TRANS-RCD
        // .
        // 110400***************************************************************** 17020001
        // ===== End COBOL =====
        
        try {
            if ("\u0000".equals(state.wsT01rMiscFormHdrValue)) {
                // CONTINUE
            } else if ("\uFFFF".equals(state.wsT01rMiscFormTrlValue)) {
                // CONTINUE
            // TODO: unknown method or variable wsT01rMiscFormValidTax
                // CONTINUE
            } else {
                state.wsT01rMiscFormTaxType = String.valueOf(state.lit3);
            }
            state.wsOutputTransRcd = state.wsT01rMiscFormRcd;
            writeMiscTransRcd(state);
        } catch (Exception e) {
            throw new RuntimeException("Error in miscFormSetTaxType: " + e.getMessage(), e);
        }
    }


    private void miscFormResultsProcess(ProgramState state) {
        // ===== Original COBOL: 5040-MISC-FORM-RESULTS-PROCESS =====
        // 110600***************************************************************** 17040001
        // 110700*      DISPLAY TRAILER AND ACCUM RESULTS                        * 17050001
        // 110800***************************************************************** 17060001
        // MOVE LIT-MISC-FORM TO MSG-OUTPUT-FILE-NAME
        // IF WS-MISC-FORM-TRANS-COUNTER   =
        // WS-T01R-MISC-FORM-TRL-COUNT      AND
        // WS-MISC-FORM-TRANS-AMT-ACCUM =
        // WS-T01R-MISC-FORM-TRL-AMOUNT
        // MOVE LIT-MISC-INPUT-SUCCESS   TO CC-E01W-DISPLAY-RCD
        // PERFORM 8999-WRITE-SYSOUT
        // PERFORM 8999-WRITE-SYSOUT
        // MOVE WS-MISC-FORM-TRANS-COUNTER TO MSG-MISC-FILE-COUNT
        // MOVE WS-MISC-FORM-TRANS-AMT-ACCUM TO MSG-MISC-FILE-AMOUNT17150001
        // MOVE MSG-MISC-FORM-SUMMARY TO CC-E01W-DISPLAY-RCD
        // PERFORM 8999-WRITE-SYSOUT
        // PERFORM 8999-WRITE-SYSOUT
        // MOVE LIT-MISC-FORM-CONTROL-TOTALS TO CC-E01W-DISPLAY-RCD
        // PERFORM 8999-WRITE-SYSOUT
        // END-IF
        // IF WS-MISC-FORM-TRANS-COUNTER   =
        // WS-T01R-MISC-FORM-TRL-COUNT
        // CONTINUE
        // ELSE
        // MOVE LIT-TRANS-TLS-DO-NOT-AGREE  TO
        // CC-E01W-DISPLAY-RCD
        // PERFORM 8999-WRITE-SYSOUT
        // MOVE LIT-FOR-MISCELLANEOUS-INPUT TO
        // CC-E01W-DISPLAY-RCD
        // PERFORM 8999-WRITE-SYSOUT
        // MOVE WS-MISC-FORM-TRANS-COUNTER  TO MSG-FILE-COUNT
        // MOVE WS-T01R-MISC-FORM-TRL-COUNT TO MSG-TRAILER-COUNT
        // MOVE MSG-TRAILER-COUNT-DISPLAY TO
        // CC-E01W-DISPLAY-RCD
        // PERFORM 8999-WRITE-SYSOUT
        // END-IF
        // IF WS-MISC-FORM-TRANS-AMT-ACCUM =
        // WS-T01R-MISC-FORM-TRL-AMOUNT
        // CONTINUE
        // ELSE
        // MOVE LIT-TRANS-TLS-DO-NOT-AGREE   TO
        // CC-E01W-DISPLAY-RCD
        // PERFORM 8999-WRITE-SYSOUT
        // MOVE LIT-FOR-MISCELLANEOUS-INPUT  TO
        // CC-E01W-DISPLAY-RCD
        // PERFORM 8999-WRITE-SYSOUT
        // MOVE WS-MISC-FORM-TRANS-AMT-ACCUM TO MSG-FILE-COUNT
        // MOVE WS-T01R-MISC-FORM-TRL-AMOUNT TO MSG-TRAILER-COUNT
        // MOVE MSG-TRAILER-COUNT-DISPLAY    TO
        // CC-E01W-DISPLAY-RCD
        // PERFORM 8999-WRITE-SYSOUT
        // END-IF
        // PERFORM 8999-WRITE-SYSOUT
        // MOVE WS-MISC-FORM-TRANS-COUNTER    TO  WS-OUTPUT-TRANS-CNT
        // MOVE WS-MISC-FORM-TRANS-AMT-ACCUM  TO  WS-OUTPUT-TRANS-AMT
        // PERFORM 6600-OUTPUT-BALANCING-PROCESS
        // .
        // EJECT
        // 116300***************************************************************** 17580001
        // ===== End COBOL =====
        
        try {
            state.msgOutputFileName = state.litMiscForm;
            if (state.wsMiscFormTransCounter == state.wsT01rMiscFormTrlCount && state.wsMiscFormTransAmtAccum.compareTo(state.wsT01rMiscFormTrlAmount) == 0) {
                state.ccE01wDisplayRcd = state.litMiscInputSuccess;
                includeSysoutDisplay(state);
                includeSysoutDisplay(state);
                state.msgMiscFileCount = String.valueOf(state.wsMiscFormTransCounter);
                state.msgMiscFileAmount = String.valueOf(state.wsMiscFormTransAmtAccum);
                state.ccE01wDisplayRcd = state.msgMiscFormSummary;
                includeSysoutDisplay(state);
                includeSysoutDisplay(state);
                state.ccE01wDisplayRcd = state.litMiscFormControlTotals;
                includeSysoutDisplay(state);
            }
            if (state.wsMiscFormTransCounter == state.wsT01rMiscFormTrlCount) {
                // CONTINUE
            } else {
                state.ccE01wDisplayRcd = state.litTransTlsDoNotAgree;
                includeSysoutDisplay(state);
                state.ccE01wDisplayRcd = state.litForMiscellaneousInput;
                includeSysoutDisplay(state);
                state.msgFileCount = String.valueOf(state.wsMiscFormTransCounter);
                state.msgTrailerCount = String.valueOf(state.wsT01rMiscFormTrlCount);
                state.ccE01wDisplayRcd = state.msgTrailerCountDisplay;
                includeSysoutDisplay(state);
            }
            if (state.wsMiscFormTransAmtAccum.compareTo(state.wsT01rMiscFormTrlAmount) == 0) {
                // CONTINUE
            } else {
                state.ccE01wDisplayRcd = state.litTransTlsDoNotAgree;
                includeSysoutDisplay(state);
                state.ccE01wDisplayRcd = state.litForMiscellaneousInput;
                includeSysoutDisplay(state);
                state.msgFileCount = String.valueOf(state.wsMiscFormTransAmtAccum);
                state.msgTrailerCount = String.valueOf(state.wsT01rMiscFormTrlAmount);
                state.ccE01wDisplayRcd = state.msgTrailerCountDisplay;
                includeSysoutDisplay(state);
            }
            includeSysoutDisplay(state);
            state.wsOutputTransCnt = state.wsMiscFormTransCounter;
            state.wsOutputTransAmt = state.wsMiscFormTransAmtAccum;
            outputBalancingProcess(state);
        } catch (Exception e) {
            throw new RuntimeException("Error in miscFormResultsProcess: " + e.getMessage(), e);
        }
    }


    private void bccwProcess(ProgramState state) {
        // ===== Original COBOL: 5200-BCCW-PROCESS =====
        // 116500***************************************************************** 17600001
        // 116600*      PRIMING READ AND EOF TEST FOR B&CCW INPUT                * 17610001
        // 116700***************************************************************** 17620001
        // PERFORM 6500-INITIAL-OUTPUT-COUNT
        // 116900***************************************************************** 17640001
        // 117000*   TEST FOR NO DATA ON FILE                                    * 17650001
        // 117100***************************************************************** 17660001
        // IF TEN99-T03R-BCCW-EOF-YES
        // MOVE LIT-BCCW    TO MSG-NO-DATA-FILE-NAME
        // MOVE MSG-NO-DATA TO CC-E01W-DISPLAY-RCD
        // PERFORM 8999-WRITE-SYSOUT
        // PERFORM 5240-BCCW-END-PROCESS
        // MOVE "Y"         TO ERROR-FLAG
        // ELSE
        // PERFORM 5210-BCCW-PROCESS-RCD UNTIL
        // TEN99-T03R-BCCW-EOF-YES
        // END-IF
        // IF TEN99-T03R-BCCW-EOF-YES
        // PERFORM 5240-BCCW-END-PROCESS
        // END-IF
        // .
        // 118600***************************************************************** 17850001
        // ===== End COBOL =====
        
        try {
            initialOutputCount(state);
            if (Boolean.TRUE.equals(state.ten99T03rBccwEofYes)) {
                state.msgNoDataFileName = state.litBccw;
                state.ccE01wDisplayRcd = state.msgNoData;
                // TODO: unknown method
                bccwEndProcess(state);
                state.errorFlag = "Y";
            } else {
                while (!Boolean.TRUE.equals(state.ten99T03rBccwEofYes)) {
                    bccwProcessRcd(state);
                }
            }
            if (Boolean.TRUE.equals(state.ten99T03rBccwEofYes)) {
                bccwEndProcess(state);
            }
        } catch (Exception e) {
            throw new RuntimeException("Error in bccwProcess: " + e.getMessage(), e);
        }
    }


    private void bccwProcessRcd(ProgramState state) {
        // ===== Original COBOL: 5210-BCCW-PROCESS-RCD =====
        // 118800***************************************************************** 17870001
        // 118900*      PROCESS THE B&CCW RECORDS                                * 17880001
        // 119000*        HEADERS - VERIFY DATE                                  * 17880001
        // 119100*        DETAIL  - SET TAX TYPE                                 * 17880001
        // 119200*        TRAILER - ACCUM COUNT AND AMOUNTS FOR VERIFICATION     * 17880001
        // 119300*      ALL RECORDS ARE WRITTEN TO THE OUTPUT FILE               * 17900001
        // 119400*                                                               * 17920001
        // 119500***************************************************************** 17950001
        // IF TEN99-LOW-VALUE-HDR = LOW-VALUES
        // PERFORM 4210-BCCW-VERIFY
        // MOVE SPACES TO TEN99-LOW-VALUE-HDR
        // ELSE
        // IF TEN99-HIGH-VALUE-TLR = HIGH-VALUES
        // MOVE TEN99-RECORD-TLR TO WS-OUTPUT-TRANS-RCD
        // PERFORM 8020-WRITE-BCCW-TRANS-RCD
        // PERFORM 5250-BCCW-TRAILER-ACCUM
        // MOVE SPACES TO TEN99-HIGH-VALUE-TLR
        // ELSE
        // PERFORM 5220-BCCW-SET-TAX-TYPE
        // END-IF
        // END-IF
        // PERFORM 7200-BCCW-READ-RCD
        // .
        // EJECT
        // 121300***************************************************************** 18940001
        // ===== End COBOL =====
        
        try {
            if ("\u0000".equals(state.ten99LowValueHdr)) {
                bccwVerify(state);
                state.ten99LowValueHdr = " ";
            } else {
                if ("\uFFFF".equals(state.ten99HighValueTlr)) {
                    state.wsOutputTransRcd = state.ten99RecordTlr;
                    writeBccwTransRcd(state);
                    bccwTrailerAccum(state);
                    state.ten99HighValueTlr = " ";
                } else {
                    bccwSetTaxType(state);
                }
            }
            bccwReadRcd(state);
        } catch (Exception e) {
            throw new RuntimeException("Error in bccwProcessRcd: " + e.getMessage(), e);
        }
    }


    private void bccwSetTaxType(ProgramState state) {
        // ===== Original COBOL: 5220-BCCW-SET-TAX-TYPE =====
        // 121500***************************************************************** 18960001
        // 121600*      SET UP AND SEARCH 1099 TABLE                             * 18970001
        // 121700*      RECORDS WITH BRANCH = 048 ARE SPECIAL OT CLOC CHECKS     * 18970001
        // 121800*                               CLOC TAX TYPE SET TO 3          * 18970001
        // 121900*      DEFAULT TAX TYPE IS 3 - SHOULD BE INVESTIGATED           * 18970001
        // 122000*      RECORDS WHERE THE CEC IS SPACES ARE WRITTEN TO THE SYSOUT* 18970001
        // 122100*      THE TAX TYPE IS NOT SET FOR FOREIGN RECORDS (1/2011)     * 18970001
        // 122200*          FOREIGN RECS - CANADA AND PUERTO RICO                * 18970001
        // 122300***************************************************************** 18980001
        // ADD LIT-1                       TO WS-BCCW-TRANS-COUNTER
        // ADD TEN99-RPT-DISB-AMT-DATA     TO WS-BCCW-TRANS-AMT-ACCUM
        // SET FOREIGN-IND-NO              TO TRUE
        // MOVE TEN99-COMPASS-ENT-CDE-DATA TO WS-CEC
        // MOVE TEN99-PS-BUS-UNIT          TO WS-BUS-UNIT
        // INITIALIZE WS-TAX-TYPE
        // IF (WS-BUS-UNIT-CAN)
        // OR (TEN99-ISSUING-BR-CODE-DATA = LIT-TW)
        // OR (TEN99-STATE-DATA = LIT-PR-STATE-CODE)
        // SET FOREIGN-IND-YES        TO TRUE
        // ELSE
        // IF TEN99-COMPASS-ENT-CDE-DATA = SPACES
        // IF TEN99-ISSUING-BR-CODE-DATA = LIT-048
        // MOVE LIT-3 TO WS-TAX-TYPE
        // ELSE
        // MOVE TEN99-RCD TO CC-E01W-DISPLAY-RCD
        // PERFORM 8999-WRITE-SYSOUT
        // END-IF
        // ELSE
        // PERFORM 6000-SEARCH-1099-TABLE
        // END-IF
        // END-IF
        // MOVE WS-TAX-TYPE  TO TEN99-TAX-TYPE
        // MOVE TEN99-RCD    TO WS-OUTPUT-TRANS-RCD
        // PERFORM 8020-WRITE-BCCW-TRANS-RCD
        // .
        // EJECT
        // 125300***************************************************************** 19820001
        // ===== End COBOL =====
        
        try {
            state.wsBccwTransCounter = state.wsBccwTransCounter + state.lit1;
            state.wsBccwTransAmtAccum = state.wsBccwTransAmtAccum.add(new java.math.BigDecimal(state.ten99RptDisbAmtData));
            state.foreignIndNo = "Y";
            state.wsCec = state.ten99CompassEntCdeData;
            state.wsBusUnit = state.ten99PsBusUnit;
            initialization(state);
            if (Boolean.TRUE.equals(state.wsBusUnitCan) || "TW".equals(state.ten99IssuingBrCodeData) || "PR".equals(state.ten99StateData)) {
                state.foreignIndYes = "Y";
            } else {
                if (" ".equals(state.ten99CompassEntCdeData)) {
                    if ("048".equals(state.ten99IssuingBrCodeData)) {
                        state.wsTaxType = "3";
                    } else {
                        state.ccE01wDisplayRcd = state.ten99Rcd;
                        includeSysoutDisplay(state);
                    }
                } else {
                    bccwVerify(state);
                }
            }
            state.ten99TaxType = state.wsTaxType;
            state.wsOutputTransRcd = state.ten99Rcd;
            bccwProcessRcd(state);
        } catch (Exception e) {
            throw new RuntimeException("Error in bccwSetTaxType: " + e.getMessage(), e);
        }
    }


    private void bccwEndProcess(ProgramState state) {
        // ===== Original COBOL: 5240-BCCW-END-PROCESS =====
        // 125500***************************************************************** 19840001
        // 125600*      TEST TRAILER TOTALS AND DISPLAY RESULTS                  * 19850001
        // 125700***************************************************************** 19860001
        // PERFORM 8999-WRITE-SYSOUT
        // MOVE LIT-BCCW TO MSG-OUTPUT-FILE-NAME
        // IF WS-BCCW-TRANS-COUNTER = WS-BCCW-TRL-COUNTER AND
        // WS-BCCW-TRANS-AMT-ACCUM = WS-BCCW-TRL-AMT-ACCUM
        // MOVE LIT-BCCW-SUCCESS TO CC-E01W-DISPLAY-RCD
        // PERFORM 8999-WRITE-SYSOUT
        // MOVE WS-BCCW-TRANS-COUNTER TO MSG-FILE-COUNT
        // MOVE WS-BCCW-TRL-COUNTER TO MSG-TRAILER-COUNT
        // MOVE MSG-TRAILER-COUNT-DISPLAY TO
        // CC-E01W-DISPLAY-RCD
        // PERFORM 8999-WRITE-SYSOUT
        // MOVE WS-BCCW-TRANS-AMT-ACCUM TO MSG-FILE-AMOUNT
        // MOVE WS-BCCW-TRL-AMT-ACCUM TO MSG-TRAILER-AMOUNT
        // MOVE MSG-TRAILER-DOLLAR-DISPLAY TO
        // CC-E01W-DISPLAY-RCD
        // PERFORM 8999-WRITE-SYSOUT
        // PERFORM 8999-WRITE-SYSOUT
        // ELSE
        // IF WS-BCCW-TRANS-COUNTER = WS-BCCW-TRL-COUNTER
        // CONTINUE
        // ELSE
        // MOVE LIT-TRANS-TLS-DO-NOT-AGREE TO
        // CC-E01W-DISPLAY-RCD
        // PERFORM 8999-WRITE-SYSOUT
        // MOVE LIT-FOR-BCCW-INPUT TO
        // CC-E01W-DISPLAY-RCD
        // PERFORM 8999-WRITE-SYSOUT
        // MOVE WS-BCCW-TRANS-COUNTER TO MSG-FILE-COUNT
        // MOVE WS-BCCW-TRL-COUNTER TO MSG-TRAILER-COUNT
        // MOVE MSG-TRAILER-COUNT-DISPLAY TO
        // CC-E01W-DISPLAY-RCD
        // PERFORM 8999-WRITE-SYSOUT
        // END-IF
        // IF WS-BCCW-TRANS-AMT-ACCUM =
        // WS-BCCW-TRL-AMT-ACCUM
        // CONTINUE
        // ELSE
        // MOVE LIT-TRANS-TLS-DO-NOT-AGREE TO
        // CC-E01W-DISPLAY-RCD
        // PERFORM 8999-WRITE-SYSOUT
        // MOVE LIT-FOR-BCCW-INPUT TO
        // CC-E01W-DISPLAY-RCD
        // PERFORM 8999-WRITE-SYSOUT
        // MOVE WS-BCCW-TRANS-AMT-ACCUM TO MSG-FILE-AMOUNT
        // MOVE WS-BCCW-TRL-AMT-ACCUM TO MSG-TRAILER-AMOUNT
        // MOVE MSG-TRAILER-DOLLAR-DISPLAY TO
        // CC-E01W-DISPLAY-RCD
        // PERFORM 8999-WRITE-SYSOUT
        // END-IF
        // PERFORM 8999-WRITE-SYSOUT
        // END-IF
        // MOVE LIT-BCCW-CONTROL-TOTALS TO CC-E01W-DISPLAY-RCD
        // PERFORM 8999-WRITE-SYSOUT
        // MOVE WS-BCCW-TRANS-COUNTER    TO WS-OUTPUT-TRANS-CNT
        // MOVE WS-BCCW-TRANS-AMT-ACCUM  TO WS-OUTPUT-TRANS-AMT
        // PERFORM 6600-OUTPUT-BALANCING-PROCESS
        // PERFORM 8999-WRITE-SYSOUT
        // .
        // EJECT
        // 131700***************************************************************** 20460001
        // ===== End COBOL =====
        
        try {
            // TODO: unknown method
            state.msgOutputFileName = state.litBccw;
            if (state.wsBccwTransCounter == state.wsBccwTrlCounter && state.wsBccwTransAmtAccum.compareTo(state.wsBccwTrlAmtAccum) == 0) {
                state.ccE01wDisplayRcd = state.litBccwSuccess;
                includeSysoutDisplay(state);
                state.msgFileCount = String.valueOf(state.wsBccwTransCounter);
                state.msgTrailerCount = String.valueOf(state.wsBccwTrlCounter);
                state.ccE01wDisplayRcd = state.msgTrailerCountDisplay;
                includeSysoutDisplay(state);
                state.msgFileAmount = String.valueOf(state.wsBccwTransAmtAccum);
                state.msgTrailerAmount = String.valueOf(state.wsBccwTrlAmtAccum);
                state.ccE01wDisplayRcd = state.msgTrailerDollarDisplay;
                includeSysoutDisplay(state);
                includeSysoutDisplay(state);
            } else {
                if (state.wsBccwTransCounter == state.wsBccwTrlCounter) {
                    // CONTINUE
                } else {
                    state.ccE01wDisplayRcd = state.litTransTlsDoNotAgree;
                    includeSysoutDisplay(state);
                    state.ccE01wDisplayRcd = state.litForBccwInput;
                    includeSysoutDisplay(state);
                    state.msgFileCount = String.valueOf(state.wsBccwTransCounter);
                    state.msgTrailerCount = String.valueOf(state.wsBccwTrlCounter);
                    state.ccE01wDisplayRcd = state.msgTrailerCountDisplay;
                    includeSysoutDisplay(state);
                }
                if (state.wsBccwTransAmtAccum.compareTo(state.wsBccwTrlAmtAccum) == 0) {
                    // CONTINUE
                } else {
                    state.ccE01wDisplayRcd = state.litTransTlsDoNotAgree;
                    includeSysoutDisplay(state);
                    state.ccE01wDisplayRcd = state.litForBccwInput;
                    includeSysoutDisplay(state);
                    state.msgFileAmount = String.valueOf(state.wsBccwTransAmtAccum);
                    state.msgTrailerAmount = String.valueOf(state.wsBccwTrlAmtAccum);
                    state.ccE01wDisplayRcd = state.msgTrailerDollarDisplay;
                    includeSysoutDisplay(state);
                }
                includeSysoutDisplay(state);
            }
            state.ccE01wDisplayRcd = state.litBccwControlTotals;
            includeSysoutDisplay(state);
            state.wsOutputTransCnt = state.wsBccwTransCounter;
            state.wsOutputTransAmt = state.wsBccwTransAmtAccum;
            outputBalancingProcess(state);
            // TODO: unknown method
        } catch (Exception e) {
            throw new RuntimeException("Error in bccwEndProcess: " + e.getMessage(), e);
        }
    }


    private void bccwTrailerAccum(ProgramState state) {
        // ===== Original COBOL: 5250-BCCW-TRAILER-ACCUM =====
        // 131900***************************************************************** 20480001
        // 132000*      ACCUMULATE BCCW TRAILER TOTALS                           * 20490001
        // 132100*      NOTE: THERE ARE MULTIPLE TRAILER RECORDS ON THE BCCW     * 20500001
        // 132200*      FILE. THE TRAILERS ARE ACCUMULATED TO BALANCE THE DATA.  * 20510001
        // 132300*                                                               * 20520001
        // 132400***************************************************************** 20530001
        // IF TEN99-WRITTEN-CNT-TLR NUMERIC
        // MOVE TEN99-WRITTEN-CNT-TLR TO WS-T03R-BCCW-TRL-CNT
        // ELSE MOVE ZEROES                TO WS-T03R-BCCW-TRL-CNT
        // END-IF
        // ADD WS-T03R-BCCW-TRL-CNT        TO WS-BCCW-TRL-COUNTER
        // ADD TEN99-TOTAL-DOLLAR-AMT-TLR  TO WS-BCCW-TRL-AMT-ACCUM
        // .
        // 133300***************************************************************** 20590001
        // EJECT
        // 133600***************************************************************** 20620001
        // ===== End COBOL =====
        
        try {
            if (state.ten99WrittenCntTlr != null && state.ten99WrittenCntTlr.matches("\\d+")) {
                state.wsT03rBccwTrlCnt = Integer.parseInt(state.ten99WrittenCntTlr);
            } else {
                state.wsT03rBccwTrlCnt = 0;
            }
            state.wsBccwTrlCounter = state.wsBccwTrlCounter + state.wsT03rBccwTrlCnt;
            state.wsBccwTrlAmtAccum = state.wsBccwTrlAmtAccum.add(new java.math.BigDecimal(state.ten99TotalDollarAmtTlr));
        } catch (Exception e) {
            throw new RuntimeException("Error in bccwTrailerAccum: " + e.getMessage(), e);
        }
    }


    private void deftProcess(ProgramState state) {
        // ===== Original COBOL: 5400-DEFT-PROCESS =====
        // 133800***************************************************************** 20640001
        // 133900*      PRIMING READ AND EOF TEST FOR DEFT  INPUT                * 20650001
        // 134000***************************************************************** 20660001
        // PERFORM 6500-INITIAL-OUTPUT-COUNT
        // 134200***************************************************************** 20680001
        // 134300*   TEST FOR NO DATA ON FILE                                    * 20690001
        // 134400***************************************************************** 20700001
        // IF TEN99-T04R-DEFT-EOF-YES OR
        // END-OF-DEFT-DETAIL-YES
        // MOVE LIT-DEFT               TO MSG-NO-DATA-FILE-NAME
        // MOVE MSG-NO-DATA            TO CC-E01W-DISPLAY-RCD
        // PERFORM 8999-WRITE-SYSOUT
        // IF WS-T04R-DEFT-TRL-VALUE = HIGH-VALUES
        // MOVE WS-T04R-DEFT-TRL TO WS-OUTPUT-TRANS-RCD
        // PERFORM 8040-WRITE-DEFT-TRANS-RCD
        // PERFORM 5450-DEFT-TRAILER-ACCUM
        // END-IF
        // PERFORM 5440-DEFT-TRAILER-PROCESS
        // ELSE
        // PERFORM 5410-DEFT-PROCESS-RCD
        // UNTIL TEN99-T04R-DEFT-EOF-YES
        // PERFORM 5440-DEFT-TRAILER-PROCESS
        // END-IF
        // .
        // 136200***************************************************************** 20880001
        // ===== End COBOL =====
        
        try {
            initialOutputCount(state);
            if (Boolean.TRUE.equals(state.ten99T04rDeftEofYes) || Boolean.TRUE.equals(state.endOfDeftDetailYes)) {
                state.msgNoDataFileName = state.litDeft;
                state.ccE01wDisplayRcd = state.msgNoData;
                // TODO: unknown method
                if ("\uFFFF".equals(state.wsT04rDeftTrlValue)) {
                    state.wsOutputTransRcd = state.wsT04rDeftTrl;
                    writeDeftTransRcd(state);
                    deftTrailerAccum(state);
                }
                deftTrailerProcess(state);
            } else {
                while (!Boolean.TRUE.equals(state.ten99T04rDeftEofYes)) {
                    deftProcessRcd(state);
                }
                deftTrailerProcess(state);
            }
        } catch (Exception e) {
            throw new RuntimeException("Error in deftProcess: " + e.getMessage(), e);
        }
    }


    private void deftProcessRcd(ProgramState state) {
        // ===== Original COBOL: 5410-DEFT-PROCESS-RCD =====
        // 136400***************************************************************** 20900001
        // 136500*      PROCESS THE DETAIL AND TRAILER FOR THE DEFT  ISSUES      * 20910001
        // 136600*      INCREMENT COUNTERS FOR PRIMING READ                      * 20920001
        // 136700*      ONE HEADER & ONE TRAILER FOR THE DEFT FILE               * 20940001
        // 136800***************************************************************** 20970001
        // IF WS-T04R-DEFT-TRL-VALUE = HIGH-VALUES
        // MOVE WS-T04R-DEFT-TRL TO WS-OUTPUT-TRANS-RCD
        // PERFORM 8040-WRITE-DEFT-TRANS-RCD
        // PERFORM 5450-DEFT-TRAILER-ACCUM
        // ELSE
        // PERFORM 5420-DEFT-SET-TAX-TYPE
        // END-IF
        // PERFORM 7300-DEFT-READ-RCD
        // .
        // 137900***************************************************************** 21810001
        // ===== End COBOL =====
        
        try {
            if ("\uFFFF".equals(state.wsT04rDeftTrlValue)) {
                state.wsOutputTransRcd = state.wsT04rDeftTrl;
                writeDeftTransRcd(state);
                deftTrailerAccum(state);
            } else {
                deftSetTaxType(state);
            }
            deftReadRcd(state);
        } catch (Exception e) {
            throw new RuntimeException("Error in deftProcessRcd: " + e.getMessage(), e);
        }
    }


    private void deftSetTaxType(ProgramState state) {
        // ===== Original COBOL: 5420-DEFT-SET-TAX-TYPE =====
        // 138100***************************************************************** 21830001
        // 138200*      SET UP AND SEARCH 1099 TABLE                             * 21840001
        // 138300*      DEFAULT TAX TYPE IS SET TO 3 - S/B INVESTIGATED          * 21840001
        // 138400***************************************************************** 21850001
        // ADD LIT-1                       TO WS-DEFT-TRANS-COUNTER
        // ADD WS-T04R-DEFT-DISB-AMT       TO WS-DEFT-TRANS-AMT-ACCUM
        // MOVE WS-T04R-DEFT-COMPASS-CODE  TO WS-CEC
        // INITIALIZE WS-TAX-TYPE
        // IF WS-T04R-DEFT-COMPASS-CODE = SPACES
        // CONTINUE
        // ELSE
        // PERFORM 6000-SEARCH-1099-TABLE
        // END-IF
        // IF WS-TAX-TYPE = SPACES
        // MOVE WS-T04R-DEFT-RCD TO CC-E01W-DISPLAY-RCD
        // PERFORM 8999-WRITE-SYSOUT
        // MOVE LIT-3            TO WS-TAX-TYPE
        // END-IF
        // MOVE WS-TAX-TYPE           TO WS-T04R-DEFT-TAX-TYPE
        // MOVE WS-T04R-DEFT-RCD      TO WS-OUTPUT-TRANS-RCD
        // PERFORM 8040-WRITE-DEFT-TRANS-RCD
        // .
        // 140400***************************************************************** 22200001
        // 140500***************************************************************** 22720001
        // ===== End COBOL =====
        
        try {
            state.wsDeftTransCounter = state.wsDeftTransCounter + 1;
            state.wsDeftTransAmtAccum = state.wsDeftTransAmtAccum.add(state.wsT04rDeftDisbAmt);
            state.wsCec = state.wsT04rDeftCompassCode;
            state.wsTaxType = "";
            if (!" ".equals(state.wsT04rDeftCompassCode)) {
                search1099Table(state);
            }
            if (" ".equals(state.wsTaxType)) {
                state.ccE01wDisplayRcd = state.wsT04rDeftRcd;
                // TODO: unknown method
                state.wsTaxType = "3";
            }
            state.wsT04rDeftTaxType = state.wsTaxType;
            state.wsOutputTransRcd = state.wsT04rDeftRcd;
            writeDeftTransRcd(state);
        } catch (Exception e) {
            throw new RuntimeException("Error in deftSetTaxType: " + e.getMessage(), e);
        }
    }


    private void deftTrailerProcess(ProgramState state) {
        // ===== Original COBOL: 5440-DEFT-TRAILER-PROCESS =====
        // 140700***************************************************************** 22740001
        // 140800*      TEST TRAILER TOTALS AND DISPLAY RESULTS                  * 22750001
        // 140900***************************************************************** 22760001
        // PERFORM 8999-WRITE-SYSOUT
        // MOVE LIT-DEFT TO MSG-OUTPUT-FILE-NAME
        // IF WS-DEFT-TRANS-COUNTER   = WS-DEFT-TRL-COUNTER AND
        // WS-DEFT-TRANS-AMT-ACCUM = WS-DEFT-TRL-AMT-ACCUM
        // MOVE LIT-DEFT-SUCCESS           TO CC-E01W-DISPLAY-RCD
        // PERFORM 8999-WRITE-SYSOUT
        // MOVE WS-DEFT-TRANS-COUNTER      TO MSG-FILE-COUNT
        // MOVE WS-DEFT-TRL-COUNTER        TO MSG-TRAILER-COUNT
        // MOVE MSG-TRAILER-COUNT-DISPLAY  TO CC-E01W-DISPLAY-RCD
        // PERFORM 8999-WRITE-SYSOUT
        // MOVE WS-DEFT-TRANS-AMT-ACCUM    TO MSG-FILE-AMOUNT
        // MOVE WS-DEFT-TRL-AMT-ACCUM      TO MSG-TRAILER-AMOUNT
        // MOVE MSG-TRAILER-DOLLAR-DISPLAY TO CC-E01W-DISPLAY-RCD
        // PERFORM 8999-WRITE-SYSOUT
        // ELSE
        // IF WS-DEFT-TRANS-COUNTER = WS-DEFT-TRL-COUNTER
        // CONTINUE
        // ELSE
        // MOVE LIT-TRANS-TLS-DO-NOT-AGREE TO CC-E01W-DISPLAY-RCD
        // PERFORM 8999-WRITE-SYSOUT
        // MOVE LIT-FOR-DEFT-INPUT         TO CC-E01W-DISPLAY-RCD
        // PERFORM 8999-WRITE-SYSOUT
        // MOVE WS-DEFT-TRANS-COUNTER      TO MSG-FILE-COUNT
        // MOVE WS-DEFT-TRL-COUNTER        TO MSG-TRAILER-COUNT
        // MOVE MSG-TRAILER-COUNT-DISPLAY  TO CC-E01W-DISPLAY-RCD
        // PERFORM 8999-WRITE-SYSOUT
        // END-IF
        // IF WS-DEFT-TRANS-AMT-ACCUM = WS-DEFT-TRL-AMT-ACCUM
        // CONTINUE
        // ELSE
        // MOVE LIT-TRANS-TLS-DO-NOT-AGREE TO CC-E01W-DISPLAY-RCD
        // PERFORM 8999-WRITE-SYSOUT
        // MOVE LIT-FOR-DEFT-INPUT         TO CC-E01W-DISPLAY-RCD
        // PERFORM 8999-WRITE-SYSOUT
        // MOVE WS-DEFT-TRANS-AMT-ACCUM    TO MSG-FILE-AMOUNT
        // MOVE WS-DEFT-TRL-AMT-ACCUM      TO MSG-TRAILER-AMOUNT
        // MOVE MSG-TRAILER-DOLLAR-DISPLAY TO CC-E01W-DISPLAY-RCD
        // PERFORM 8999-WRITE-SYSOUT
        // END-IF
        // PERFORM 8999-WRITE-SYSOUT
        // END-IF
        // MOVE LIT-DEFT-CONTROL-TOTALS          TO CC-E01W-DISPLAY-RCD
        // PERFORM 8999-WRITE-SYSOUT
        // MOVE WS-DEFT-TRANS-COUNTER    TO WS-OUTPUT-TRANS-CNT
        // MOVE WS-DEFT-TRANS-AMT-ACCUM  TO WS-OUTPUT-TRANS-AMT
        // PERFORM 6600-OUTPUT-BALANCING-PROCESS
        // PERFORM 8999-WRITE-SYSOUT
        // .
        // EJECT
        // 146200***************************************************************** 23320001
        // ===== End COBOL =====
        
        try {
            includeSysoutDisplay(state);
            state.msgOutputFileName = state.litDeft;
            if (state.wsT04rDeftTrlCnt == state.wsT04rDeftDisbSeq && state.wsT04rDeftTrlAmt.compareTo(state.wsT04rDeftDisbAmt) == 0) {
                state.ccE01wDisplayRcd = state.litDeftSuccess;
                includeSysoutDisplay(state);
                state.msgFileCount = String.valueOf(state.wsT04rDeftTrlCnt);
                state.msgTrailerCount = String.valueOf(state.wsT04rDeftDisbSeq);
                state.ccE01wDisplayRcd = state.msgTrailerCountDisplay;
                includeSysoutDisplay(state);
                state.msgFileAmount = String.valueOf(state.wsT04rDeftTrlAmt);
                state.msgTrailerAmount = String.valueOf(state.wsT04rDeftDisbAmt);
                state.ccE01wDisplayRcd = state.msgTrailerDollarDisplay;
                includeSysoutDisplay(state);
            } else {
                if (state.wsT04rDeftTrlCnt == state.wsT04rDeftDisbSeq) {
                    // CONTINUE
                } else {
                    state.ccE01wDisplayRcd = state.litTransTlsDoNotAgree;
                    includeSysoutDisplay(state);
                    state.ccE01wDisplayRcd = state.litForDeftInput;
                    includeSysoutDisplay(state);
                    state.msgFileCount = String.valueOf(state.wsT04rDeftTrlCnt);
                    state.msgTrailerCount = String.valueOf(state.wsT04rDeftDisbSeq);
                    state.ccE01wDisplayRcd = state.msgTrailerCountDisplay;
                    // TODO: unknown method
                }
                if (state.wsT04rDeftTrlAmt.compareTo(state.wsT04rDeftDisbAmt) == 0) {
                    // CONTINUE
                } else {
                    state.ccE01wDisplayRcd = state.litTransTlsDoNotAgree;
                    // TODO: unknown method
                    state.ccE01wDisplayRcd = state.litForDeftInput;
                    // TODO: unknown method
                    state.msgFileAmount = String.valueOf(state.wsT04rDeftTrlAmt);
                    state.msgTrailerAmount = String.valueOf(state.wsT04rDeftDisbAmt);
                    state.ccE01wDisplayRcd = state.msgTrailerDollarDisplay;
                    // TODO: unknown method
                }
                // TODO: unknown method
            }
            state.ccE01wDisplayRcd = state.litDeftControlTotals;
            // TODO: unknown method
            state.wsOutputTransCnt = state.wsT04rDeftTrlCnt;
            state.wsOutputTransAmt = state.wsT04rDeftTrlAmt;
            outputBalancingProcess(state);
            // TODO: unknown method
        } catch (Exception e) {
            throw new RuntimeException("Error in deftTrailerProcess: " + e.getMessage(), e);
        }
    }


    private void deftTrailerAccum(ProgramState state) {
        // ===== Original COBOL: 5450-DEFT-TRAILER-ACCUM =====
        // 146400***************************************************************** 23340001
        // 146500*      ACCUMULATE DEFT TRAILER TOTALS                           * 23350001
        // 146600***************************************************************** 23360001
        // ADD WS-T04R-DEFT-TRL-CNT TO WS-DEFT-TRL-COUNTER
        // ADD WS-T04R-DEFT-TRL-AMT TO WS-DEFT-TRL-AMT-ACCUM
        // .
        // EJECT
        // 147100***************************************************************** 23420001
        // ===== End COBOL =====
        
        try {
            state.wsDeftTrlCounter = state.wsDeftTrlCounter + state.wsT04rDeftTrlCnt;
            state.wsDeftTrlAmtAccum = state.wsDeftTrlAmtAccum.add(state.wsT04rDeftTrlAmt);
        } catch (Exception e) {
            throw new RuntimeException("Error in deftTrailerAccum: " + e.getMessage(), e);
        }
    }


    private void rejCycleProcess(ProgramState state) {
        // ===== Original COBOL: 5500-REJ-CYCLE-PROCESS =====
        // 147300***************************************************************** 23440001
        // 147400*      PRIMING READ AND EOF TEST FOR REJECT RECYCLED INPUT      * 23450001
        // 147500***************************************************************** 23460001
        // PERFORM 6500-INITIAL-OUTPUT-COUNT
        // PERFORM 7500-REJ-CYCLE-READ-RCD
        // IF TEN99-T05R-REJ-CYCLE-EOF-YES OR
        // WS-T05R-REJ-CYCLE-TRL-VALUE = HIGH-VALUES
        // 148000***************************************************************** 23510001
        // 148100*   TEST FOR NO DATA ON FILE                                    * 23520001
        // 148200***************************************************************** 23530001
        // MOVE LIT-REJECT-FILE TO MSG-NO-DATA-FILE-NAME
        // MOVE MSG-NO-DATA TO CC-E01W-DISPLAY-RCD
        // PERFORM 8999-WRITE-SYSOUT
        // MOVE WS-T05R-REJECT-RECYCLING-RCD TO
        // TEN99-T05W-REJ-CYCLE-OUT-RCD
        // PERFORM 8030-WRITE-REJECT-RCD
        // PERFORM 5540-REJ-CYCLE-TRAILER-PROCESS
        // ELSE
        // PERFORM 5510-REJ-CYCLE-PROCESS-RCD UNTIL
        // TEN99-T05R-REJ-CYCLE-EOF-YES
        // END-IF
        // .
        // EJECT
        // 149600***************************************************************** 23640001
        // ===== End COBOL =====
        
        try {
            initialOutputCount(state);
            rejCycleReadRcd(state);
            if ("Y".equals(state.ten99T05rRejCycleEofYes) || "\uFFFF".equals(state.wsT05rRejCycleTrlValue)) {
                state.msgNoDataFileName = state.litRejectFile;
                state.ccE01wDisplayRcd = state.msgNoData;
                // TODO: unknown method
                state.ten99T05wRejCycleOutRcd = state.wsT05rRejectRecyclingRcd;
                writeRejectRcd(state);
                rejCycleTrailerProcess(state);
            } else {
                while (!Boolean.TRUE.equals(state.ten99T05rRejCycleEofYes)) {
                    rejCycleProcessRcd(state);
                }
            }
        } catch (Exception e) {
            throw new RuntimeException("Error in rejCycleProcess: " + e.getMessage(), e);
        }
    }


    private void rejCycleProcessRcd(ProgramState state) {
        // ===== Original COBOL: 5510-REJ-CYCLE-PROCESS-RCD =====
        // 149800***************************************************************** 23660001
        // 149900*      PROCESS THE DETAIL AND TRAILER FOR REJECT RECYCLED INPUT * 23670001
        // 150000***************************************************************** 23680001
        // IF WS-T05R-REJ-CYCLE-TRL-VALUE = HIGH-VALUES
        // MOVE WS-T05R-REJECT-RECYCLING-RCD TO
        // TEN99-T05W-REJ-CYCLE-OUT-RCD
        // PERFORM 8030-WRITE-REJECT-RCD
        // ELSE
        // 150600***************************************************************** 23730001
        // 150700*   INCREMENT COUNTERS FOR TRAILER TESTING                      * 23740001
        // 150800***************************************************************** 23750001
        // ADD 1 TO WS-REJ-CYCLE-TRANS-COUNTER
        // PERFORM 5520-REJ-CYCLE-SET-TAX-TYPE
        // END-IF
        // PERFORM 7500-REJ-CYCLE-READ-RCD
        // .
        // 151400***************************************************************** 23810001
        // ===== End COBOL =====
        
        try {
            if ("\uFFFF".equals(state.wsT05rRejCycleTrlValue)) {
                state.ten99T05wRejCycleOutRcd = state.wsT05rRejectRecyclingRcd;
                writeRejectRcd(state);
            } else {
                state.wsRejCycleTransCounter = state.wsRejCycleTransCounter + 1;
                rejCycleSetTaxType(state);
            }
            rejCycleReadRcd(state);
        } catch (Exception e) {
            throw new RuntimeException("Error in rejCycleProcessRcd: " + e.getMessage(), e);
        }
    }


    private void rejCycleSetTaxType(ProgramState state) {
        // ===== Original COBOL: 5520-REJ-CYCLE-SET-TAX-TYPE =====
        // 151600***************************************************************** 23830001
        // 151700*      STRIP OFF THE REJECT KEY AND PROCESS ACCORDING TO        * 23840001
        // 151800*      THE SOURCE CODE.                                         * 23850001
        // 151900***************************************************************** 23860001
        // INITIALIZE REJECT-INDICATORS
        // WS-OUTPUT-TRANS-RCD
        // EVALUATE TRUE
        // WHEN WS-T05R-REJ-CYCLE-HDR-VALUE = LOW-VALUES
        // CONTINUE
        // WHEN WS-T05R-REJ-CYCLE-TRL-VALUE = HIGH-VALUES
        // CONTINUE
        // WHEN OTHER
        // IF WS-T05R-REJECT-VALID-TAX-TYPE
        // CONTINUE
        // ELSE MOVE LIT-3         TO WS-T05R-REJECT-TAX-TYPE
        // MOVE WS-T05R-REJECT-RECYCLING-RCD TO
        // CC-E01W-DISPLAY-RCD
        // PERFORM 8999-WRITE-SYSOUT
        // END-IF
        // END-EVALUATE
        // MOVE WS-T05R-REJECT-RECYCLING-RCD  TO
        // TEN99-T05W-REJ-CYCLE-OUT-RCD
        // PERFORM 8030-WRITE-REJECT-RCD
        // .
        // 154100***************************************************************** 24830001
        // ===== End COBOL =====
        
        try {
            initialization(state);
            if ("\u0000".equals(state.wsT05rRejectHdrValue)) {
                // CONTINUE
            } else if ("\uFFFF".equals(state.wsT05rRejectTrlValue)) {
                // CONTINUE
            } else {
                if ("true".equals(String.valueOf(state.wsT05rRejectValidTaxType))) {
                    // CONTINUE
                } else {
                    state.wsT05rRejectTaxType = "3";
                    state.ccE01wDisplayRcd = state.wsT05rRejectRecyclingRcd;
                    includeSysoutDisplay(state);
                }
            }
            state.ten99T05wRejCycleOutRcd = state.wsT05rRejectRecyclingRcd;
            rejCycleProcess(state);
        } catch (Exception e) {
            throw new RuntimeException("Error in rejCycleSetTaxType: " + e.getMessage(), e);
        }
    }


    private void rejCycleTrailerProcess(ProgramState state) {
        // ===== Original COBOL: 5540-REJ-CYCLE-TRAILER-PROCESS =====
        // 154300***************************************************************** 24850001
        // 154400*      TEST TRAILER TOTALS AND DISPLAY RESULTS                  * 24860001
        // 154500***************************************************************** 24870001
        // PERFORM 8999-WRITE-SYSOUT
        // PERFORM 8999-WRITE-SYSOUT
        // MOVE LIT-REJECT-FILE TO MSG-OUTPUT-FILE-NAME
        // IF WS-REJ-CYCLE-TRANS-COUNTER =
        // WS-T05R-REJ-CYCLE-TRL-COUNT AND
        // WS-REJ-CYCLE-TRANS-AMT-ACCUM =
        // WS-T05R-REJ-CYCLE-TRL-AMT
        // MOVE LIT-REJ-CYCLE-SUCCESS  TO CC-E01W-DISPLAY-RCD
        // PERFORM 8999-WRITE-SYSOUT
        // MOVE WS-REJ-CYCLE-TRANS-COUNTER TO MSG-FILE-COUNT
        // MOVE WS-T05R-REJ-CYCLE-TRL-COUNT TO MSG-TRAILER-COUNT
        // MOVE MSG-TRAILER-COUNT-DISPLAY TO
        // CC-E01W-DISPLAY-RCD
        // PERFORM 8999-WRITE-SYSOUT
        // MOVE WS-REJ-CYCLE-TRANS-AMT-ACCUM TO MSG-FILE-AMOUNT
        // MOVE WS-T05R-REJ-CYCLE-TRL-AMT TO MSG-TRAILER-AMOUNT
        // MOVE MSG-TRAILER-DOLLAR-DISPLAY TO
        // CC-E01W-DISPLAY-RCD
        // PERFORM 8999-WRITE-SYSOUT
        // PERFORM 8999-WRITE-SYSOUT
        // ELSE
        // IF WS-REJ-CYCLE-TRANS-COUNTER NOT EQUAL
        // WS-T05R-REJ-CYCLE-TRL-COUNT
        // MOVE LIT-TRANS-TLS-DO-NOT-AGREE TO
        // CC-E01W-DISPLAY-RCD
        // PERFORM 8999-WRITE-SYSOUT
        // MOVE LIT-FOR-REJECT-CYCLE-INPUT TO
        // CC-E01W-DISPLAY-RCD
        // PERFORM 8999-WRITE-SYSOUT
        // MOVE WS-REJ-CYCLE-TRANS-COUNTER TO MSG-FILE-COUNT
        // MOVE WS-T05R-REJ-CYCLE-TRL-COUNT TO MSG-TRAILER-COUNT
        // MOVE MSG-TRAILER-COUNT-DISPLAY TO
        // CC-E01W-DISPLAY-RCD
        // PERFORM 8999-WRITE-SYSOUT
        // MOVE "Y" TO ERROR-FLAG
        // END-IF
        // IF WS-REJ-CYCLE-TRANS-AMT-ACCUM NOT EQUAL
        // WS-T05R-REJ-CYCLE-TRL-AMT
        // MOVE LIT-TRANS-TLS-DO-NOT-AGREE TO
        // CC-E01W-DISPLAY-RCD
        // PERFORM 8999-WRITE-SYSOUT
        // MOVE LIT-FOR-REJECT-CYCLE-INPUT TO
        // CC-E01W-DISPLAY-RCD
        // PERFORM 8999-WRITE-SYSOUT
        // MOVE WS-REJ-CYCLE-TRANS-AMT-ACCUM TO MSG-FILE-AMOUNT
        // MOVE WS-T05R-REJ-CYCLE-TRL-AMT TO MSG-TRAILER-AMOUNT
        // MOVE MSG-TRAILER-DOLLAR-DISPLAY TO
        // CC-E01W-DISPLAY-RCD
        // PERFORM 8999-WRITE-SYSOUT
        // MOVE "Y" TO ERROR-FLAG
        // END-IF
        // PERFORM 8999-WRITE-SYSOUT
        // END-IF
        // PERFORM 8999-WRITE-SYSOUT
        // MOVE LIT-REJ-CYCLE-CONTROL-TOTALS TO CC-E01W-DISPLAY-RCD
        // PERFORM 8999-WRITE-SYSOUT
        // MOVE WS-REJ-CYCLE-TRANS-COUNTER    TO WS-OUTPUT-TRANS-CNT
        // MOVE WS-REJ-CYCLE-TRANS-AMT-ACCUM TO WS-OUTPUT-TRANS-AMT
        // PERFORM 6600-OUTPUT-BALANCING-PROCESS
        // MOVE "Y" TO TEN99-T05R-REJ-CYCLE-EOF
        // .
        // 160800***************************************************************** 25470001
        // ===== End COBOL =====
        
        try {
            // TODO: unknown method
            // TODO: unknown method
            state.msgOutputFileName = state.litRejectFileName;
            if (state.wsRejCycleTransCounter == state.wsT05rRejCycleTrlCount && state.wsRejCycleTransAmtAccum.compareTo(state.wsT05rRejCycleTrlAmt) == 0) {
                state.ccE01wDisplayRcd = state.litRejCycleSuccess;
                // TODO: unknown method
                state.msgFileCount = String.valueOf(state.wsRejCycleTransCounter);
                state.msgTrailerCount = String.valueOf(state.wsT05rRejCycleTrlCount);
                state.ccE01wDisplayRcd = state.msgTrailerCountDisplay;
                // TODO: unknown method
                state.msgFileAmount = String.valueOf(state.wsRejCycleTransAmtAccum);
                state.msgTrailerAmount = String.valueOf(state.wsT05rRejCycleTrlAmt);
                state.ccE01wDisplayRcd = state.msgTrailerDollarDisplay;
                // TODO: unknown method
                // TODO: unknown method
            } else {
                if (state.wsRejCycleTransCounter != state.wsT05rRejCycleTrlCount) {
                    state.ccE01wDisplayRcd = state.litTransTlsDoNotAgree;
                    // TODO: unknown method
                    state.ccE01wDisplayRcd = state.litForRejectCycleInput;
                    // TODO: unknown method
                    state.msgFileCount = String.valueOf(state.wsRejCycleTransCounter);
                    state.msgTrailerCount = String.valueOf(state.wsT05rRejCycleTrlCount);
                    state.ccE01wDisplayRcd = state.msgTrailerCountDisplay;
                    // TODO: unknown method
                    state.errorFlag = "Y";
                }
                if (state.wsRejCycleTransAmtAccum.compareTo(state.wsT05rRejCycleTrlAmt) != 0) {
                    state.ccE01wDisplayRcd = state.litTransTlsDoNotAgree;
                    // TODO: unknown method
                    state.ccE01wDisplayRcd = state.litForRejectCycleInput;
                    // TODO: unknown method
                    state.msgFileAmount = String.valueOf(state.wsRejCycleTransAmtAccum);
                    state.msgTrailerAmount = String.valueOf(state.wsT05rRejCycleTrlAmt);
                    state.ccE01wDisplayRcd = state.msgTrailerDollarDisplay;
                    // TODO: unknown method
                    state.errorFlag = "Y";
                }
                // TODO: unknown method
            }
            // TODO: unknown method
            state.ccE01wDisplayRcd = state.litRejCycleControlTotals;
            // TODO: unknown method
            state.wsOutputTransCnt = state.wsRejCycleTransCounter;
            state.wsOutputTransAmt = state.wsRejCycleTransAmtAccum;
            outputBalancingProcess(state);
            state.ten99T05rRejCycleEof = "Y";
        } catch (Exception e) {
            throw new RuntimeException("Error in rejCycleTrailerProcess: " + e.getMessage(), e);
        }
    }


    private void search1099Table(ProgramState state) {
        // ===== Original COBOL: 6000-SEARCH-1099-TABLE =====
        // 161000***************************************************************** 25490001
        // 161100*   SET WS-CEC PRIOR TO PERFORM                                 * 25500001
        // 161200***************************************************************** 25510001
        // SET CODE-INDEX TO 1
        // SEARCH WS-T07R-1099ENTRY-CODES AT END
        // CONTINUE
        // WHEN WS-CEC = WS-T07R-1099ENTRY-CODE (CODE-INDEX)
        // MOVE WS-T07R-1099ENTRY-TAX-TYPE (CODE-INDEX) TO
        // WS-TAX-TYPE
        // .
        // 162000***************************************************************** 25870001
        // 162100***************************************************************** 27810001
        // ===== End COBOL =====
        
        try {
            int codeIndex = 0;
            boolean found = false;
            while (codeIndex < state.wsT07r1099EntryCodes.length() && !found) {
                if (state.wsCec != null && state.wsCec.equals(String.valueOf(state.wsT07r1099EntryCodes.charAt(codeIndex)))) {
                    state.wsTaxType = String.valueOf(state.wsT07r1099EntryTaxTypes.charAt(codeIndex));
                    found = true;
                }
                codeIndex++;
            }
        } catch (Exception e) {
            throw new RuntimeException("Error in search1099Table: " + e.getMessage(), e);
        }
    }


    private void initialOutputCount(ProgramState state) {
        // ===== Original COBOL: 6500-INITIAL-OUTPUT-COUNT =====
        // 162300***************************************************************** 27830001
        // 162400*    INITIALIZE OUTPUT COUNTERS                                 * 27840001
        // 162500***************************************************************** 27850001
        // INITIALIZE WS-OUTPUT-TRANS-CNT
        // WS-OUTPUT-TRANS-AMT
        // WS-FILE-REC-COUNTER
        // WS-FILE-AMT-ACCUM
        // WS-REJ-TRANS-CNT
        // WS-REJ-TRANS-AMT
        // .
        // EJECT
        // 163400***************************************************************** 27960001
        // ===== End COBOL =====
        
        try {
            state.wsOutputTransCnt = 0;
            state.wsOutputTransAmt = java.math.BigDecimal.ZERO;
            state.wsFileRecCounter = 0;
            state.wsFileAmtAccum = java.math.BigDecimal.ZERO;
            state.wsRejTransCnt = 0;
            state.wsRejTransAmt = java.math.BigDecimal.ZERO;
        } catch (Exception e) {
            throw new RuntimeException("Error in initialOutputCount: " + e.getMessage(), e);
        }
    }


    private void outputBalancingProcess(ProgramState state) {
        // ===== Original COBOL: 6600-OUTPUT-BALANCING-PROCESS =====
        // 163600***************************************************************** 27980001
        // 163700*    DISPLAY CONTROL TOTALS FOR ALL OUTPUTS                     * 27990001
        // 163800***************************************************************** 28000001
        // MOVE WS-OUTPUT-TRANS-CNT TO MSG-NUM-RCDS
        // MOVE WS-OUTPUT-TRANS-AMT TO MSG-AMOUNT
        // MOVE MSG-CONTROL-TOTALS  TO CC-E01W-DISPLAY-RCD
        // PERFORM 8999-WRITE-SYSOUT
        // .
        // 164500***************************************************************** 28260001
        // ===== End COBOL =====
        
        try {
            state.msgNumRcds = String.valueOf(state.wsOutputTransCnt);
            state.msgAmount = String.valueOf(state.wsOutputTransAmt);
            state.ccE01wDisplayRcd = state.msgControlTotals;
            includeSysoutDisplay(state);
        } catch (Exception e) {
            throw new RuntimeException("Error in outputBalancingProcess: " + e.getMessage(), e);
        }
    }


    private void miscFormReadRcd(ProgramState state) {
        // ===== Original COBOL: 7000-MISC-FORM-READ-RCD =====
        // 164700***************************************************************** 28280001
        // 164800*      READ ROUTINE FOR ALL MISC FORM FILE READS                * 28290001
        // 164900***************************************************************** 28300001
        // INITIALIZE WS-T01R-MISC-FORM-RCD
        // READ TEN99-T01R-MISC-TRANS-FILE INTO
        // WS-T01R-MISC-FORM-RCD
        // AT END
        // MOVE "Y" TO TEN99-T01R-MISC-FORM-EOF
        // END-READ
        // .
        // EJECT
        // 165800***************************************************************** 28390001
        // ===== End COBOL =====
        
        try {
            initialization(state);
            readTen99t01rmisctransfile(state);
            if ("Y".equals(state.ten99T01rMiscFormEof)) {
                // AT END logic already handled by setting EOF flag
            }
        } catch (Exception e) {
            throw new RuntimeException("Error in miscFormReadRcd: " + e.getMessage(), e);
        }
    }


    private void bccwReadRcd(ProgramState state) {
        // ===== Original COBOL: 7200-BCCW-READ-RCD =====
        // 166000***************************************************************** 28410001
        // 166100*      READ ROUTINE FOR B&CCW ISSUES                            * 28420001
        // 166200*      NOTE: THERE ARE MULTIPLE HEADERS & TRAILERS              * 28420001
        // 166300***************************************************************** 28430001
        // READ TEN99-T03R-BCCW-FILE         INTO TEN99-RCD
        // AT END
        // MOVE "Y" TO TEN99-T03R-BCCW-EOF
        // END-READ
        // MOVE TEN99-RCD TO TEN99-RECORD-HDR TEN99-RECORD-TLR
        // EVALUATE TRUE
        // WHEN TEN99-T03R-BCCW-EOF-YES
        // CONTINUE
        // WHEN TEN99-LOW-VALUE-HDR    = LOW-VALUES
        // MOVE TEN99-RECORD-HDR TO WS-OUTPUT-TRANS-RCD
        // INITIALIZE TEN99-RCD
        // WHEN TEN99-HIGH-VALUE-TLR   = HIGH-VALUES
        // MOVE TEN99-RECORD-TLR TO WS-OUTPUT-TRANS-RCD
        // INITIALIZE TEN99-RCD
        // WHEN OTHER
        // CONTINUE
        // END-EVALUATE
        // .
        // EJECT
        // 168300***************************************************************** 28590001
        // ===== End COBOL =====
        
        try {
            readTen99t03rbccwfile(state);
            if ("Y".equals(state.ten99T03rBccwEof)) {
                // CONTINUE
            } else if ("\u0000".equals(state.ten99LowValueHdr)) {
                state.wsOutputTransRcd = state.ten99RecordHdr;
                state.ten99Rcd = null;
            } else if ("\uFFFF".equals(state.ten99HighValueTlr)) {
                state.wsOutputTransRcd = state.ten99RecordTlr;
                state.ten99Rcd = null;
            } else {
                // CONTINUE
            }
        } catch (Exception e) {
            throw new RuntimeException("Error in bccwReadRcd: " + e.getMessage(), e);
        }
    }


    private void deftReadRcd(ProgramState state) {
        // ===== Original COBOL: 7300-DEFT-READ-RCD =====
        // 168500***************************************************************** 28610001
        // 168600*      READ ROUTINE FOR DEFT  ISSUES                            * 28620001
        // 168700***************************************************************** 28630001
        // INITIALIZE WS-T04R-DEFT-RCD
        // READ TEN99-T04R-DEFT-FILE         INTO WS-T04R-DEFT-RCD
        // AT END
        // MOVE "Y" TO TEN99-T04R-DEFT-EOF
        // END-READ
        // MOVE WS-T04R-DEFT-RCD TO WS-OUTPUT-TRANS-RCD
        // EVALUATE TRUE
        // WHEN WS-T04R-DEFT-HDR-VALUE = LOW-VALUES
        // CONTINUE
        // WHEN TEN99-T04R-DEFT-EOF-YES
        // CONTINUE
        // WHEN WS-T04R-DEFT-HDR-VALUE = HIGH-VALUES
        // MOVE "Y" TO END-OF-DEFT-DETAIL
        // WHEN OTHER
        // MOVE "Y" TO END-OF-DEFT-HEADERS
        // END-EVALUATE
        // .
        // EJECT
        // 170700***************************************************************** 28810001
        // ===== End COBOL =====
        
        try {
            initialization(state);
            readTen99t04wdeftoutfile(state);
            if ("Y".equals(state.ten99T04rDeftEof)) {
                // AT END
            }
            state.wsOutputTransRcd = state.wsT04rDeftRcd;
            if ("\u0000".equals(state.wsT04rDeftHdrValue)) {
                // CONTINUE
            } else if ("Y".equals(state.ten99T04rDeftEofYes)) {
                // CONTINUE
            } else if ("\uFFFF".equals(state.wsT04rDeftHdrValue)) {
                state.endOfDeftDetail = "Y";
            } else {
                state.endOfDeftHeaders = "Y";
            }
        } catch (Exception e) {
            throw new RuntimeException("Error in deftReadRcd: " + e.getMessage(), e);
        }
    }


    private void rejCycleReadRcd(ProgramState state) {
        // ===== Original COBOL: 7500-REJ-CYCLE-READ-RCD =====
        // 170900***************************************************************** 28830001
        // 171000*      READ ROUTINE FOR ALL MISC FORM FILE READS                * 28840001
        // 171100***************************************************************** 28850001
        // INITIALIZE WS-T05R-REJECT-RECYCLING-RCD
        // READ TEN99-T05R-REJ-CYCLE-FILE INTO
        // WS-T05R-REJECT-RECYCLING-RCD
        // AT END
        // MOVE "Y" TO TEN99-T05R-REJ-CYCLE-EOF
        // END-READ
        // .
        // EJECT
        // 172000***************************************************************** 28940001
        // ===== End COBOL =====
        
        try {
            state.wsT05rRejectRecyclingRcd = null;
            readTen99t05wrejcycleoutfile(state);
            if ("Y".equals(state.ten99T05rRejCycleEof)) {
                state.ten99T05rRejCycleEof = "Y";
            }
        } catch (Exception e) {
            throw new RuntimeException("Error in rejCycleReadRcd: " + e.getMessage(), e);
        }
    }


    private void readControlCardIn(ProgramState state) {
        // ===== Original COBOL: 7900-READ-CONTROL-CARD-IN =====
        // 172200***************************************************************** 28960001
        // 172300*      READ ROUTINE FOR CONTROL CARD                            * 28970001
        // 172400***************************************************************** 28980001
        // READ CC-R01R-CONTROL-CARD INTO CONTROL-CARD-IN1
        // AT END
        // MOVE "Y" TO EOF-CONTROL-CARD
        // END-READ
        // IF EOF-CONTROL-CARD-YES
        // MOVE LIT-NO-CONTROL-CARD TO SAR-PARAGRAPH
        // PERFORM 8999-WRITE-SYSOUT
        // PERFORM 9998-COREDUMP
        // END-IF
        // .
        // EJECT
        // 173600***************************************************************** 29100001
        // ===== End COBOL =====
        
        try {
            readCcr01rcontrolcard(state);
            if ("Y".equals(state.eofControlCard)) {
                state.sarParagraph = state.litNoControlCard;
                // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method includeSysoutDisplay(state);
                coredump(state);
            }
        } catch (Exception e) {
            throw new RuntimeException("Error in readControlCardIn: " + e.getMessage(), e);
        }
    }


    private void read1099entryTable(ProgramState state) {
        // ===== Original COBOL: 7910-READ-1099ENTRY-TABLE =====
        // 173800***************************************************************** 29120001
        // 173900*      READ ROUTINE FOR 1099 REPORTABLE ENTRY CODE TABLE        * 29130001
        // 174000***************************************************************** 29140001
        // READ WS-T07R-1099ENTRY-CD-FILE INTO
        // WS-T07R-1099ENTRY-CODES(CODE-INDEX)
        // AT END
        // MOVE "Y" TO EOF-1099ENTRY-TABLE
        // END-READ
        // IF WS-T07R-1099ENTRY-CODE (CODE-INDEX) = HIGH-VALUES
        // SET CODE-INDEX TO LIT-NUM-OF-1099-CODES
        // END-IF
        // IF EOF-1099ENTRY-TABLE-YES
        // MOVE LIT-NO-1099ENTRY-TABLE TO SAR-PARAGRAPH
        // PERFORM 8999-WRITE-SYSOUT
        // PERFORM 9998-COREDUMP
        // END-IF
        // .
        // EJECT
        // 175600***************************************************************** 29440001
        // ===== End COBOL =====
        
        try {
            // TODO: unknown method readWsT07r1099entryCdFile(state);
            if ("Y".equals(state.eof1099entryTable)) {
                state.sarParagraph = state.litNo1099entryTable;
                includeSysoutDisplay(state);
                coredump(state);
            }
            if ("\uFFFF".equals(String.valueOf(state.wsT07r1099entryCode.charAt(Integer.parseInt(state.codeIndex))))) {
                state.codeIndex = String.valueOf(state.litNumOf1099Codes);
            }
        } catch (Exception e) {
            throw new RuntimeException("Error in read1099entryTable: " + e.getMessage(), e);
        }
    }


    private void writeMiscTransRcd(ProgramState state) {
        // ===== Original COBOL: 8010-WRITE-MISC-TRANS-RCD =====
        // 175800***************************************************************** 29460001
        // 175900*      WRITE MISC TRANS RECORD                                  * 29470001
        // 176000***************************************************************** 29480001
        // ADD LIT-1  TO WS-FILE-REC-COUNTER
        // IF (WS-OUTPUT-HDR-VALUE = LOW-VALUES)
        // OR (WS-OUTPUT-TRL-VALUE = HIGH-VALUES)
        // CONTINUE
        // ELSE
        // ADD LIT-1                  TO WS-TOTAL-OUTPUT-TRANS-CNT
        // ADD WS-T01R-MISC-FORM-AMT  TO WS-TOTAL-OUTPUT-TRANS-AMT
        // WS-FILE-AMT-ACCUM
        // END-IF
        // WRITE TEN99-T01W-MISC-TRANS-OUT-RCD
        // FROM WS-OUTPUT-TRANS-RCD
        // MOVE SPACES TO WS-OUTPUT-TRANS-RCD
        // INITIALIZE REJECT-INDICATORS
        // .
        // EJECT
        // 177600***************************************************************** 29600001
        // ===== End COBOL =====
        
        try {
            state.wsFileRecCounter = state.wsFileRecCounter + 1;
            if ("\u0000".equals(state.wsOutputHdrValue) || "\uFFFF".equals(state.wsOutputTrlValue)) {
                // CONTINUE
            } else {
                state.wsTotalOutputTransCnt = state.wsTotalOutputTransCnt + 1;
                state.wsTotalOutputTransAmt = state.wsTotalOutputTransAmt.add(state.wsT01rMiscFormAmt);
                state.wsFileAmtAccum = state.wsFileAmtAccum.add(state.wsT01rMiscFormAmt);
            }
            writeMiscTransRcd(state);
            state.wsOutputTransRcd = " ";
            // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method // TODO: unknown method initializeRejectIndicators(state);
        } catch (Exception e) {
            throw new RuntimeException("Error in writeMiscTransRcd: " + e.getMessage(), e);
        }
    }


    private void writeBccwTransRcd(ProgramState state) {
        // ===== Original COBOL: 8020-WRITE-BCCW-TRANS-RCD =====
        // 177800***************************************************************** 29620001
        // 177900*      WRITE MISC TRANS RECORD                                  * 29630001
        // 178000***************************************************************** 29640001
        // ADD LIT-1  TO WS-FILE-REC-COUNTER
        // IF   (WS-OUTPUT-HDR-VALUE = LOW-VALUES)
        // OR (WS-OUTPUT-TRL-VALUE = HIGH-VALUES)
        // CONTINUE
        // ELSE
        // ADD LIT-1                   TO WS-TOTAL-OUTPUT-TRANS-CNT29670001
        // ADD TEN99-RPT-DISB-AMT-DATA TO WS-TOTAL-OUTPUT-TRANS-AMT29680001
        // WS-FILE-AMT-ACCUM
        // END-IF
        // WRITE TEN99-T03W-BCCW-OUT-RCD
        // FROM WS-OUTPUT-TRANS-RCD
        // MOVE SPACES TO WS-OUTPUT-TRANS-RCD
        // .
        // 179400***************************************************************** 29750001
        // ===== End COBOL =====
        
        try {
            state.wsFileRecCounter = state.wsFileRecCounter + 1;
            if ("\u0000".equals(state.wsOutputHdrValue) || "\uFFFF".equals(state.wsOutputTrlValue)) {
                // CONTINUE
            } else {
                state.wsTotalOutputTransCnt29670001 = state.wsTotalOutputTransCnt29670001 + 1;
                // TODO: unknown method or variable ten99RptDisbAmtData
                state.wsFileAmtAccum = state.wsFileAmtAccum.add(new java.math.BigDecimal(state.ten99RptDisbAmtData));
            }
            writeBccwTransRcd(state);
            state.wsOutputTransRcd = " ";
        } catch (Exception e) {
            throw new RuntimeException("Error in writeBccwTransRcd: " + e.getMessage(), e);
        }
    }


    private void writeRejectRcd(ProgramState state) {
        // ===== Original COBOL: 8030-WRITE-REJECT-RCD =====
        // 179600***************************************************************** 29770001
        // 179700*      WRITE REJECT FILE                                        * 29780001
        // 179800***************************************************************** 29790001
        // 179900
        // WRITE TEN99-T05W-REJ-CYCLE-OUT-RCD
        // .
        // 180200***************************************************************** 29870001
        // ===== End COBOL =====
        
        try {
            writeWst07r1099entrycdfile(state, state.wsOutputTransRcd);
        } catch (Exception e) {
            throw new RuntimeException("Error in writeRejectRcd: " + e.getMessage(), e);
        }
    }


    private void writeDeftTransRcd(ProgramState state) {
        // ===== Original COBOL: 8040-WRITE-DEFT-TRANS-RCD =====
        // 180400***************************************************************** 29890001
        // 180500*      WRITE FOREIGN TRANSACTION RECORD                         * 29900001
        // 180600***************************************************************** 29910001
        // ADD LIT-1  TO WS-FILE-REC-COUNTER
        // IF   (WS-OUTPUT-HDR-VALUE = LOW-VALUES)
        // OR (WS-OUTPUT-TRL-VALUE = HIGH-VALUES)
        // CONTINUE
        // ELSE
        // ADD LIT-1                   TO WS-TOTAL-OUTPUT-TRANS-CNT
        // ADD WS-T04R-DEFT-DISB-AMT   TO WS-TOTAL-OUTPUT-TRANS-AMT
        // WS-FILE-AMT-ACCUM
        // END-IF
        // WRITE TEN99-T04W-DEFT-OUT-RCD FROM WS-OUTPUT-TRANS-RCD
        // MOVE SPACES TO WS-OUTPUT-TRANS-RCD
        // INITIALIZE REJECT-INDICATORS
        // .
        // 182100***************************************************************** 30020001
        // 182200*8999-WRITE-SYSOUT.                                               30190001
        // 182300***************************************************************** 30200001
        // 182400*    ++INCLUDE C2INZ002                                         * 30210001
        // 182500*      COMMON PARAGRAPH TO WRITE A SYSOUT RECORD                * 30220001
        // 182600*  8999-WRITE-SYSOUT.                                           * 30230001
        // 182700***************************************************************** 30240001
        // COPY C2INZ002.
        // EJECT
        // ===== End COBOL =====
        
        try {
            state.wsFileRecCounter = state.wsFileRecCounter + 1;
            if ("\u0000".equals(state.wsOutputHdrValue) || "\uFFFF".equals(state.wsOutputTrlValue)) {
                // CONTINUE
            } else {
                state.wsTotalOutputTransCnt = state.wsTotalOutputTransCnt + 1;
                state.wsTotalOutputTransAmt = state.wsTotalOutputTransAmt.add(state.wsT04rDeftDisbAmt);
                state.wsFileAmtAccum = state.wsFileAmtAccum.add(state.wsT04rDeftDisbAmt);
            }
            writeDeftTransRcd(state);
            state.wsOutputTransRcd = " ";
            initializeRejectIndicators(state);
        } catch (Exception e) {
            throw new RuntimeException("Error in writeDeftTransRcd: " + e.getMessage(), e);
        }
    }


    private void terminationRoutine(ProgramState state) {
        // ===== Original COBOL: 9000-TERMINATION-ROUTINE =====
        // 183100***************************************************************** 30280001
        // 183200*      CHECK FOR PROCESSING ERROR FLAG SET TO "Y".              * 30290001
        // 183300*      PREPARE GRAND TOTALS.                                    * 30310001
        // 183400***************************************************************** 30320001
        // IF ERROR-FLAG = "Y"
        // PERFORM 9998-COREDUMP
        // ELSE
        // 183800***************************************************************** 30730001
        // 183900*  MOVE TOTALS FOR GRAND TOTAL DISPLAY                          * 30740001
        // 184000***************************************************************** 30750001
        // MOVE WS-TOTAL-OUTPUT-TRANS-CNT TO WS-OUTPUT-TRANS-CNT
        // MOVE WS-TOTAL-OUTPUT-TRANS-AMT TO WS-OUTPUT-TRANS-AMT
        // PERFORM 8999-WRITE-SYSOUT
        // PERFORM 8999-WRITE-SYSOUT
        // MOVE LIT-GRAND-TOTAL TO CC-E01W-DISPLAY-RCD
        // PERFORM 8999-WRITE-SYSOUT
        // MOVE LIT-TRANS-FILE TO MSG-OUTPUT-FILE-NAME
        // PERFORM 6600-OUTPUT-BALANCING-PROCESS
        // END-IF
        // PERFORM 9010-ENDING-SYSOUT-MESSAGES
        // PERFORM 9999-CLOSE-FILES
        // .
        // EJECT
        // ===== End COBOL =====
        
        try {
            if ("Y".equals(state.errorFlag)) {
                coredump(state);
            } else {
                state.wsOutputTransCnt = state.wsTotalOutputTransCnt;
                state.wsOutputTransAmt = state.wsTotalOutputTransAmt;
                includeSysoutDisplay(state);
                includeSysoutDisplay(state);
                state.ccE01wDisplayRcd = state.litGrandTotal;
                // TODO: unknown method writeSysout(state);
                state.msgOutputFileName = state.litTransFile;
                outputBalancingProcess(state);
            }
            endingSysoutMessages(state);
            // TODO: unknown method closeFiles(state);
        } catch (Exception e) {
            throw new RuntimeException("Error in terminationRoutine: " + e.getMessage(), e);
        }
    }


    private void endingSysoutMessages(ProgramState state) {
        // ===== Original COBOL: 9010-ENDING-SYSOUT-MESSAGES =====
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
        // CLOSE
        // TEN99-T01R-MISC-TRANS-FILE
        // TEN99-T03R-BCCW-FILE
        // TEN99-T04R-DEFT-FILE
        // TEN99-T05R-REJ-CYCLE-FILE
        // WS-T07R-1099ENTRY-CD-FILE
        // CC-R01R-CONTROL-CARD
        // TEN99-T01W-MISC-TRANS-OUT-FILE
        // TEN99-T03W-BCCW-OUT-FILE
        // TEN99-T04W-DEFT-OUT-FILE
        // TEN99-T05W-REJ-CYCLE-OUT-FILE
        // .
        // 189100***************************************************************** 31340001
        // 189200*   END PROGRAM CCAC6310                                        * 31350001
        // 189300***************************************************************** 31360001
        // 
        // ===== End COBOL =====
        
        try {
            includeSysoutDisplay(state);
        } catch (Exception e) {
            throw new RuntimeException("Error in endingSysoutMessages: " + e.getMessage(), e);
        }
    }

    // =========== STUB METHODS ===========
    
    private void coredump(ProgramState state) {
        // COBOL STOP RUN equivalent - for error handling
        throw new RuntimeException("COBOL STOP RUN triggered - check SYSOUT for errors");
    }
    
    private void initializeRejectIndicators(ProgramState state) {
        // Initialize reject indicators to default values
        state.ten99T05wRejCycleOutRcd = "";
    }

}