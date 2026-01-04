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
 * Complete Tasklet for COBOL program CCAC6320
 * Generated with full business logic translation
 */
public class CCAC6320Tasklet implements Tasklet {

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
        BufferedWriter ten99T01wTransOutputFileWriter;
        boolean ten99T01wTransOutputFileEof = false;
        String ten99T01wTransOutputFileLine;
        BufferedWriter ten99T02wRejCycleOutFileWriter;
        boolean ten99T02wRejCycleOutFileEof = false;
        String ten99T02wRejCycleOutFileLine;
        BufferedWriter ten99C01wRejRptOutputFileWriter;
        boolean ten99C01wRejRptOutputFileEof = false;
        String ten99C01wRejRptOutputFileLine;
        BufferedWriter ten99T03wForeignOutputFileWriter;
        boolean ten99T03wForeignOutputFileEof = false;
        String ten99T03wForeignOutputFileLine;
        BufferedWriter ten99T04wExcludesFileWriter;
        boolean ten99T04wExcludesFileEof = false;
        String ten99T04wExcludesFileLine;
        BufferedReader ccR01rControlCardReader;
        boolean ccR01rControlCardEof = false;
        String ccR01rControlCardLine;

        // Processing state
        boolean endOfInput = false;
        String returnCode = "0";
        String recordsRead = "0";
        String recordsWritten = "0";

        // WORKING-STORAGE variables
        String panValet = "024CCAC6320  05/19/05";
        String pups2000 = "";
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
        String wsT03rBccwRcd = "";
        String wsT03rBccwBusCode = "";
        String wsT03rBccwLabelCode = "";
        String wsT03rBccwBranchCode = "";
        String wsT03rBccwClocBrCode = "048";
        String wsT03rBccwCheckNum = "";
        String wsT03rBccwName = "";
        String wsT03rBccwAddress1 = "";
        String wsT03rBccwAddress2 = "";
        String wsT03rBccwCity = "";
        String wsT03rBccwState = "";
        String wsT03rBccwZipcode = "";
        String wsT03rBccwCompassCode = "";
        String wsT03rBccwDesc = "";
        String wsT03rBccwDisbAmt = "";
        String wsT03rBccwDisbSeq = "";
        String wsT03rBccwSsn = "";
        String wsT03rBccwBusUnit = "";
        String wsT03rBccwOpLoc = "";
        String wsT03rBccwTaxType = "";
        String wsT03rValidTaxType = "3 7";
        String wsT03rBccwTinInd = "";
        String wsT03rBccwAConstant = "A";
        String wsT03rBccwHdr = "";
        String wsT03rBccwHdrValue = "";
        String wsT03rBccwHdrId = "";
        String wsT03rBccwHdrDate = "";
        String wsT03rBccwHdrCc = "";
        String wsT03rBccwHdrYy = "";
        String wsT03rBccwHdrMm = "";
        String wsT03rBccwHdrDd = "";
        String wsT03rBccwTrl = "";
        String wsT03rBccwTrlValue = "";
        String wsT03rBccwTrlCnt = "";
        String wsT03rBccwTrlAmt = "";
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
        String wsT05rRejectKeyTaxType = "";
        String wsT05rRejectKeyTinInd = "";
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
        String wsT01wOutputTransRcd = "";
        String wsT01wRecKey = "";
        String wsT01wCheckNumKey = "";
        String wsT01wCheckPrefix = "";
        String wsT01wCheckMonth = "";
        String wsT01wCheckSeq = "";
        String wsT01wTaxType = "";
        String wsT01wTinInd = "";
        String wsT01wSourceCode = "";
        String wsT01wTransactionIndicator = "SPACES";
        String wsT01wTransIndicAdd = "A";
        String wsT01wTransIndicDel = "D";
        String wsT01wTransIndicChg = "C";
        String wsT01wOutputDetail = "";
        String wsT01wBranch = "";
        String wsT01wBranchCode = "";
        String wsT01wBranchPos1 = "";
        String wsT01wBranchPos2 = "";
        String wsT01wBranchPos3 = "";
        String wsT01wName = "";
        String wsT01wAddr1 = "";
        String wsT01wAddr2 = "";
        String wsT01wCity = "";
        String wsT01wState = "";
        String wsT01wPostcode = "";
        String wsT01wSsn = "";
        String wsT01wSsnUs = "";
        String allOnes = "ALL 1";
        String allTwos = "ALL 2";
        String allThrees = "ALL 3";
        String allFours = "ALL 4";
        String allFives = "ALL 5";
        String allSixes = "ALL 6";
        String allSevens = "ALL 7";
        String allEights = "ALL 8";
        String allNines = "ALL 9";
        String wsT01wSsnRemainder = "";
        String wsT01wOperLoc = "";
        String canadianOpLoc = "6572 3798";
        String wsT01wCheckNum = "";
        String wsT01wCheckDate = "";
        String wsT01wCheckDateCc = "";
        String wsT01wCheckDateYy = "";
        String wsT01wCheckDateMm = "";
        String wsT01wCheckDateDd = "";
        String wsT01wTotalAmt = "";
        String wsT01w1099Amt = "";
        String wsT01wComment = "";
        String wsT01wNbrDistRcd = "";
        String wsT01wDistRcd = "";
        String wsT01wDistCompassCode = "";
        String wsT01wDist1099Indic = "";
        String wsT01wDistAmt = "";
        String wsT01wLineNbr = "";
        String wsT01wOutputTransHdr = "";
        String wsT01wOutputHdrValue = "";
        String wsT01wOutputHdrId = "";
        String wsT01wOutputHdrDate = "";
        String wsT01wOutputHdrCc = "";
        String wsT01wOutputHdrYy = "";
        String wsT01wOutputHdrMm = "";
        String wsT01wOutputTransTrl = "";
        String wsT01wOutputTrlValue = "";
        String wsT01wOutputTrlCnt = "";
        String wsT01wOutputTrlAmt = "";
        String wsT02wRejectRecyclingRcd = "";
        String wsT02wRejectKey = "";
        String wsT02wRejectKeyOpLoc = "";
        String wsT02wRejectKeyBrDeptCd = "";
        String wsT02wRejectKeyCheckNum = "";
        String wsT02wRejectKeyCheckDt = "";
        String wsT02wRejectTaxType = "";
        String wsT02wRejectTinInd = "";
        String wsT02wRejectSourceCode = "";
        String wsT02wRejectDetail = "";
        String wsT02wRejectRecyclingHdr = "";
        String wsT02wRejectHdrValue = "";
        String wsT02wRejectHdrId = "";
        String wsT02wRejectHdrDate = "";
        String wsT02wRejectHdrCc = "";
        String wsT02wRejectHdrYy = "";
        String wsT02wRejectHdrMm = "";
        String wsT02wRejectRecyclingTrl = "";
        String wsT02wRejectTrlValue = "";
        String wsT02wRejectTrlCnt = "";
        String wsT02wRejectTrlAmt = "";
        String wsT03wForOutputRcd = "";
        String wsT03wForSourceCode = "";
        String wsT03wForeignCountry = "";
        String wsT03wForDetail = "";
        String wsT03wForOutputHdr = "";
        String wsT03wForHdrValue = "";
        String wsT03wForHdrId = "";
        String wsT03wForHdrDate = "";
        String wsT03wForHdrCc = "";
        String wsT03wForHdrYy = "";
        String wsT03wForHdrMm = "";
        String wsT03wForOutputTrl = "";
        String wsT03wForTrlValue = "";
        String wsT03wForTrlCnt = "";
        String wsT03wForTrlAmt = "";
        String wsT04wExcludesOutputRcd = "";
        String wsT04wExcludesHdr = "";
        String wsT04wExcludesHdrValue = "";
        String wsT04wExcludesHdrId = "";
        String wsT04wExcludesHdrDate = "";
        String wsT04wExcludesCc = "";
        String wsT04wExcludesYy = "";
        String wsT04wExcludesMm = "";
        String wsT04wExcludesTrl = "";
        String wsT04wExcludesValue = "";
        String wsT04wExcludesCnt = "";
        String wsT04wExcludesAmt = "";
        String wsC01wRejRptOutputRcd = "";
        String wsC01wRejectKey = "";
        String wsC01wRejRptKeyOpLoc = "";
        String wsC01wRejRptKeyBrDeptCd = "";
        String wsC01wRejRptKeyCheckNum = "";
        String wsC01wRejRptKeyCheckDt = "";
        String wsC01wRejRptTaxType = "";
        String wsC01wRejRptTinInd = "";
        String wsC01wRejRptSourceCode = "";
        String wsC01wRejRptRejectCodes = "";
        String wsC01wRejRptDetail = "";
        String wsC01wRejRptOutputHdr = "";
        String wsC01wRejRptHdrValue = "";
        String wsC01wRejRptHdrId = "";
        String wsC01wRejRptHdrDate = "";
        String wsC01wRejRptHdrCc = "";
        String wsC01wRejRptHdrYy = "";
        String wsC01wRejRptHdrMm = "";
        String wsC01wRejRptOutputTrl = "";
        String wsC01wRejRptTrlValue = "";
        String wsC01wRejRptTrlCnt = "";
        String wsC01wRejRptTrlAmt = "";
        String cntCounters = "";
        String wsMiscFormTransCounter = "0";
        String wsMiscFormSeqCnt = "0";
        String wsBccwTransCounter = "0";
        String wsBccwDistCounter = "0";
        String wsBccwTrlCounter = "0";
        String wsDeftTransCounter = "0";
        String wsDeftDistCounter = "0";
        String wsDeftTrlCounter = "0";
        String wsRejCycleTransCounter = "0";
        String wsOutputTransCnt = "0";
        String wsForTransCnt = "0";
        String wsRejTransCnt = "0";
        String wsExcludesTransCnt = "0";
        String wsTotalOutputTransCnt = "0";
        String wsTotalForTransCnt = "0";
        String wsTotalRejTransCnt = "0";
        String wsTotalRejRptTransCnt = "0";
        String wsTotalExcludesTransCnt = "0";
        String swSwitches = "";
        String swBccwBuildComplete = "N";
        String swBccwBuildCompleteYes = "Y";
        String swDeftBuildComplete = "N";
        String swDeftBuildCompleteYes = "Y";
        String wsIndicators = "";
        String rejectIndicators = "";
        String rejectFlag = "SPACE";
        String rejectName = "SPACE";
        String rejectNameErr = "Y";
        String rejectAddress = "SPACE";
        String rejectAddressErr = "Y";
        String rejectCity = "SPACE";
        String rejectCityErr = "Y";
        String rejectState = "SPACE";
        String rejectStateErr = "Y";
        String rejectPostcode = "SPACE";
        String rejectPostcodeErr = "Y";
        String rejectSsn = "SPACE";
        String rejectSsnErr = "Y";
        String rejectAmount = "SPACE";
        String foreignIndicator = "";
        String foreignIndicatorNo = "N";
        String foreignIndicatorCdn = "C";
        String foreignIndicatorYes = "Y";
        String foreignIndicatorPr = "P";
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
        String lit5 = "5";
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
        String litForeignFile = "";
        String litRejectFile = "";
        String litExcludesFile = "";
        String litTen99OutputTrans = "";
        String litTen99RejectTrans = "";
        String litTen99ForeignTrans = "";
        String litTen99ExcludesTrans = "";
        String litTen99RejReportTrans = "";
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
        String wsRecKeysArea = "";
        String wsBccwKey = "";
        String wsBccwCheckNum = "";
        String wsBccwTaxType = "";
        String wsBccwTinInd = "";
        String wsDeftKey = "";
        String wsDeftBatchNum = "";
        String wsDeftTaxType = "";
        String wsDeftTinInd = "";
        String accAccumulators = "";
        String wsMiscFormTransAmtAccum = "0";
        String wsBccwTransAmtAccum = "0";
        String wsBccwDistAmtAccum = "0";
        String wsBccwTrlAmtAccum = "0";
        String wsDeftTransAmtAccum = "0";
        String wsDeftDistAmtAccum = "0";
        String wsDeftTrlAmtAccum = "0";
        String wsTransAmtAccum = "0";
        String wsRejCycleTransAmtAccum = "0";
        String wsOutputTransAmt = "0";
        String wsForTransAmt = "0";
        String wsRejTransAmt = "0";
        String wsExcludesTransAmt = "0";
        String wsTotalOutputTransAmt = "0";
        String wsTotalForTransAmt = "0";
        String wsTotalRejTransAmt = "0";
        String wsTotalRejRptTransAmt = "0";
        String wsTotalExcludesTransAmt = "0";
        String stblStateTable = "";
        String stblStateConstants = "";
        String stblStateTableR = "";
        String stblStateCodeN = "";
        String stblStateCodeA = "";
        String stblStateTableX = "";
        String stblStateConstantsX = "";
        String stblStateTableRx = "";
        String stblStateCodeNx = "";
        String stblStateCodeAx = "";
        String stblProvTable = "";
        String stblProvConstants = "";
        String stblProvTableR = "";
        String stblProvCodeN = "";
        String stblProvCodeA = "";
        String stblProvTableX = "";
        String stblProvConstantsX = "";
        String stblProvTableRx = "";
        String stblProvCodeNx = "";
        String stblProvCodeAx = "";
    }

    @Override
    public RepeatStatus execute(StepContribution contribution, ChunkContext chunkContext) {
        ProgramState state = new ProgramState();
        try {
            mainline(state);
        } catch (Exception e) {
            throw new RuntimeException("Error executing CCAC6320", e);
        }
        return RepeatStatus.FINISHED;
    }

    private void openFiles(ProgramState state) {
        try {
            Path testDir = Paths.get("../work/mainframe_clean/testcases", "CCAC6320");
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
            Path ten99T01wTransOutputFilePath = testDir.resolve("output/ten99T01wTransOutputFile_out.txt");
            Files.createDirectories(ten99T01wTransOutputFilePath.getParent());
            state.ten99T01wTransOutputFileWriter = Files.newBufferedWriter(ten99T01wTransOutputFilePath);
            Path ten99T02wRejCycleOutFilePath = testDir.resolve("output/ten99T02wRejCycleOutFile_out.txt");
            Files.createDirectories(ten99T02wRejCycleOutFilePath.getParent());
            state.ten99T02wRejCycleOutFileWriter = Files.newBufferedWriter(ten99T02wRejCycleOutFilePath);
            Path ten99C01wRejRptOutputFilePath = testDir.resolve("output/ten99C01wRejRptOutputFile_out.txt");
            Files.createDirectories(ten99C01wRejRptOutputFilePath.getParent());
            state.ten99C01wRejRptOutputFileWriter = Files.newBufferedWriter(ten99C01wRejRptOutputFilePath);
            Path ten99T03wForeignOutputFilePath = testDir.resolve("output/ten99T03wForeignOutputFile_out.txt");
            Files.createDirectories(ten99T03wForeignOutputFilePath.getParent());
            state.ten99T03wForeignOutputFileWriter = Files.newBufferedWriter(ten99T03wForeignOutputFilePath);
            Path ten99T04wExcludesFilePath = testDir.resolve("output/ten99T04wExcludesFile_out.txt");
            Files.createDirectories(ten99T04wExcludesFilePath.getParent());
            state.ten99T04wExcludesFileWriter = Files.newBufferedWriter(ten99T04wExcludesFilePath);
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
            if (state.ten99T01wTransOutputFileWriter != null) state.ten99T01wTransOutputFileWriter.close();
            if (state.ten99T02wRejCycleOutFileWriter != null) state.ten99T02wRejCycleOutFileWriter.close();
            if (state.ten99C01wRejRptOutputFileWriter != null) state.ten99C01wRejRptOutputFileWriter.close();
            if (state.ten99T03wForeignOutputFileWriter != null) state.ten99T03wForeignOutputFileWriter.close();
            if (state.ten99T04wExcludesFileWriter != null) state.ten99T04wExcludesFileWriter.close();
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

    private void writeTen99T01wTransOutputFile(ProgramState state, String line) {
        try {
            if (state.ten99T01wTransOutputFileWriter != null && line != null) {
                state.ten99T01wTransOutputFileWriter.write(line);
                state.ten99T01wTransOutputFileWriter.newLine();
            }
        } catch (Exception e) { throw new RuntimeException("Write error", e); }
    }

    private void writeTen99T02wRejCycleOutFile(ProgramState state, String line) {
        try {
            if (state.ten99T02wRejCycleOutFileWriter != null && line != null) {
                state.ten99T02wRejCycleOutFileWriter.write(line);
                state.ten99T02wRejCycleOutFileWriter.newLine();
            }
        } catch (Exception e) { throw new RuntimeException("Write error", e); }
    }

    private void writeTen99C01wRejRptOutputFile(ProgramState state, String line) {
        try {
            if (state.ten99C01wRejRptOutputFileWriter != null && line != null) {
                state.ten99C01wRejRptOutputFileWriter.write(line);
                state.ten99C01wRejRptOutputFileWriter.newLine();
            }
        } catch (Exception e) { throw new RuntimeException("Write error", e); }
    }

    private void writeTen99T03wForeignOutputFile(ProgramState state, String line) {
        try {
            if (state.ten99T03wForeignOutputFileWriter != null && line != null) {
                state.ten99T03wForeignOutputFileWriter.write(line);
                state.ten99T03wForeignOutputFileWriter.newLine();
            }
        } catch (Exception e) { throw new RuntimeException("Write error", e); }
    }

    private void writeTen99T04wExcludesFile(ProgramState state, String line) {
        try {
            if (state.ten99T04wExcludesFileWriter != null && line != null) {
                state.ten99T04wExcludesFileWriter.write(line);
                state.ten99T04wExcludesFileWriter.newLine();
            }
        } catch (Exception e) { throw new RuntimeException("Write error", e); }
    }


    // ========================================
    // TRANSLATED BUSINESS LOGIC
    // ========================================

    private void mainline(ProgramState state) {
        // TODO: Translate COBOL paragraph '0000-MAINLINE'
        // Original COBOL:
        // 134300***************************************************************** 12630001
        //     PERFORM 1000-INITIALIZATION
        //     PERFORM 3000-MAIN-PROCESS
        //     PERFORM 9000-TERMINATION-ROUTINE
        //     GOBACK
        //     .
        // EJECT
        // 135000***************************************************************** 12700001
        initialization(state);
        mainProcess(state);
        terminationRoutine(state);
    }
    private void initialization(ProgramState state) {
        // TODO: Translate COBOL paragraph '1000-INITIALIZATION'
        // Original COBOL:
        // 135200***************************************************************** 12720001
        //     OPEN INPUT
        //        TEN99-T01R-MISC-TRANS-FILE
        //        TEN99-T03R-BCCW-FILE
        //        TEN99-T04R-DEFT-FILE
        //        TEN99-T05R-REJ-CYCLE-FILE
        //        WS-T07R-1099ENTRY-CD-FILE
        //        CC-R01R-CONTROL-CARD
        //     OUTPUT
        //        TEN99-T01W-TRANS-OUTPUT-FILE
        //        TEN99-T02W-REJ-CYCLE-OUT-FILE
        //        TEN99-C01W-REJ-RPT-OUTPUT-FILE
        //        TEN99-T03W-FOREIGN-OUTPUT-FILE
        // 136600***************************************************************** 12860001
        // 136700*    THE TEN99-T04W FILE IS EITHER DESIGNED FOR ACCUMULATING    * 12870001
        // 136800*    TRANSACTIONS THAT DO NOT HAVE A SOCIAL SECURITY NUMBER OR  * 12880001
        // 136900*    A 1099 REPORTABLE AMOUNT.  THESE TRANSACTIONS ARE NOT      * 12890001
        // 137000*    TO BE WRITTEN TO THE REJECT FILE.                          * 12900001
        // 137100***************************************************************** 12910001
        //        TEN99-T04W-EXCLUDES-FILE
        //        CC-E01W-DISPLAY-FILE
        //     PERFORM 1130-INCLUDE-SYSOUT-DISPLAY
        //     PERFORM 7900-READ-CONTROL-CARD-IN
        //     INITIALIZE             WS-T01W-OUTPUT-TRANS-RCD
        //                            WS-T02W-REJECT-RECYCLING-RCD
        //                            WS-T03W-FOR-OUTPUT-RCD
        //                            WS-T04W-EXCLUDES-OUTPUT-RCD
        //                            WS-C01W-REJ-RPT-OUTPUT-RCD
        //     MOVE LOW-VALUES     TO WS-T01W-OUTPUT-HDR-VALUE
        //                            WS-T02W-REJECT-HDR-VALUE
        //                            WS-T03W-FOR-HDR-VALUE
        //                            WS-T04W-EXCLUDES-HDR-VALUE
        //                            WS-C01W-REJ-RPT-HDR-VALUE
        //     MOVE CC-TAX-MM1     TO WS-T01W-OUTPUT-HDR-MM
        //                            WS-T02W-REJECT-HDR-MM
        //                            WS-T03W-FOR-HDR-MM
        //                            WS-T04W-EXCLUDES-MM
        //                            WS-C01W-REJ-RPT-HDR-MM
        //     MOVE CC-TAX-YY1     TO WS-T01W-OUTPUT-HDR-YY
        //                            WS-T02W-REJECT-HDR-YY
        //                            WS-T03W-FOR-HDR-YY
        //                            WS-T04W-EXCLUDES-YY
        //                            WS-C01W-REJ-RPT-HDR-YY
        //     MOVE CC-TAX-CC1     TO WS-T01W-OUTPUT-HDR-CC
        //                            WS-T02W-REJECT-HDR-CC
        //                            WS-T03W-FOR-HDR-CC
        //                            WS-T04W-EXCLUDES-CC
        //                            WS-C01W-REJ-RPT-HDR-CC
        //     MOVE LIT-TEN99-OUTPUT-TRANS     TO WS-T01W-OUTPUT-HDR-ID
        //     MOVE LIT-TEN99-REJECT-TRANS     TO WS-T02W-REJECT-HDR-ID
        //     MOVE LIT-TEN99-FOREIGN-TRANS    TO WS-T03W-FOR-HDR-ID
        //     MOVE LIT-TEN99-EXCLUDES-TRANS   TO WS-T04W-EXCLUDES-HDR-ID
        //     MOVE LIT-TEN99-REJ-REPORT-TRANS TO WS-C01W-REJ-RPT-HDR-ID
        //     PERFORM 8000-WRITE-HEADERS
        //     .
        // EJECT
        // 141100***************************************************************** 13310001
        includeSysoutDisplay(state);
        readControlCardIn(state);
        writeHeaders(state);
    }
    private void includeSysoutDisplay(ProgramState state) {
        // TODO: Translate COBOL paragraph '1130-INCLUDE-SYSOUT-DISPLAY'
        // Original COBOL:
        // 141300***************************************************************** 13330001
        // 141400*    ++INCLUDE C2INZ001                                         * 13340001
        // 141500*      COMMON INITIAL SYSOUT DISPLAYS                           * 13350001
        // 141600***************************************************************** 13360001
        // COPY C2INZ001.
        // EJECT
        // 142000***************************************************************** 13400001

    }
    private void mainProcess(ProgramState state) {
        // TODO: Translate COBOL paragraph '3000-MAIN-PROCESS'
        // Original COBOL:
        // 142200***************************************************************** 13420001
        // 142300*      MAIN PROCESSING ROUTINE                                  * 13430001
        // 142400***************************************************************** 13440001
        //     PERFORM 7910-READ-1099ENTRY-TABLE
        //        VARYING CODE-INDEX FROM 1 BY 1
        //        UNTIL CODE-INDEX GREATER THAN LIT-NUM-OF-1099-CODES
        // 142800***************************************************************** 13480001
        // 142900*      DETERMINE THE PROCESSING SCHEDULE BASED ON TAX MONTH     * 13490001
        // 143000*      VERIFY EACH INPUT HEADER RECORD TO ENSURE CORRECT FILES  * 13500001
        // 143100*      PROCESS THE DETAIL AND TRAILER FOR EACH FILE             * 13510001
        // 143200***************************************************************** 13520001
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
        // 144600***************************************************************** 13660001
        read1099entryTable(state);
        janMonthProcessing(state);
        febDecMonthProcessing(state);
        yearEndProcessing(state);
    }
    private void janMonthProcessing(ProgramState state) {
        // TODO: Translate COBOL paragraph '3002-JAN-MONTH-PROCESSING'
        // Original COBOL:
        // 144800***************************************************************** 13680001
        // 144900*   PROCESSING SCHEDULE FOR JANUARY                             * 13690001
        // 145000***************************************************************** 13700001
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
        // 146400***************************************************************** 13840001
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
        // 146600***************************************************************** 13860001
        // 146700*   PROCESSING SCHEDULE FOR FEBRUARY THROUGH DECEMBER           * 13870001
        // 146800***************************************************************** 13880001
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
        // 148500***************************************************************** 14050001
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
        // 148700***************************************************************** 14070001
        // 148800*   PROCESSING SCHEDULE FOR YEAR-END                            * 14080001
        // 148900***************************************************************** 14090001
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
        // 150100***************************************************************** 14210001
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
        // 150300***************************************************************** 14230001
        // 150400*   TEST MISCELLANEOUS FORM 20071 INPUT HEADER FOR BAD HEADER   * 14240001
        // 150500*   OR BAD CONTROL DATE.                                        * 14250001
        // 150600***************************************************************** 14260001
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
        //              CONTINUE
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
        // 153400***************************************************************** 14540001
        // 153500***************************************************************** 14550001
        miscFormReadRcd(state);
        writeSysout(state);
        writeSysout(state);
        writeSysout(state);
    }
    private void bccwVerifyProcess(ProgramState state) {
        // TODO: Translate COBOL paragraph '4200-BCCW-VERIFY-PROCESS'
        // Original COBOL:
        // 153700***************************************************************** 14570001
        // 153800*   TEST B&CCW FILE FOR NO HEADER OR BAD CONTROL DATE           * 14580001
        // 153900***************************************************************** 14590001
        //     PERFORM 7200-BCCW-READ-RCD
        //     IF TEN99-T03R-BCCW-EOF-YES
        //        MOVE LIT-BCCW    TO MSG-NO-DATA-FILE-NAME
        //        MOVE MSG-NO-DATA TO CC-E01W-DISPLAY-RCD
        //        MOVE "Y"         TO ERROR-FLAG
        //        PERFORM 8999-WRITE-SYSOUT
        //     ELSE
        //        PERFORM 4210-BCCW-VERIFY UNTIL
        //           END-OF-BCCW-HEADERS-YES OR
        //           END-OF-BCCW-DETAIL-YES
        //     END-IF
        //     .
        //     EJECT
        // 155300***************************************************************** 14730001
        bccwReadRcd(state);
        writeSysout(state);
        bccwVerify(state);
    }
    private void bccwVerify(ProgramState state) {
        // TODO: Translate COBOL paragraph '4210-BCCW-VERIFY'
        // Original COBOL:
        // 155500***************************************************************** 14750001
        // 155600*   TEST THE MULTIPLE HEADERS FOR NO DATA OR                    * 14760001
        // 155700*   BAD CONTROL DATE                                            * 14770001
        // 155800***************************************************************** 14780001
        //     IF WS-T03R-BCCW-HDR-ID        = LIT-BCCW-FILE
        //        IF WS-T03R-BCCW-HDR-YY = CC-TAX-YY1 AND
        //           WS-T03R-BCCW-HDR-CC = CC-TAX-CC1 AND
        //           WS-T03R-BCCW-HDR-MM = CC-TAX-MM1
        //           CONTINUE
        //        ELSE
        //           MOVE LIT-BCCW           TO MSG-FILE-NAME
        //           MOVE MSG-INCORRECT-DATE TO CC-E01W-DISPLAY-RCD
        //           MOVE "Y"                TO ERROR-FLAG
        //           PERFORM 8999-WRITE-SYSOUT
        //        END-IF
        //     ELSE
        //        MOVE LIT-BCCW     TO MSG-FILE-ID
        //        MOVE MSG-BAD-FILE TO CC-E01W-DISPLAY-RCD
        //        MOVE "Y"          TO ERROR-FLAG
        //        PERFORM 8999-WRITE-SYSOUT
        //     END-IF
        //     PERFORM 7200-BCCW-READ-RCD
        //     .
        //     EJECT
        // 157900***************************************************************** 14990001
        writeSysout(state);
        writeSysout(state);
        bccwReadRcd(state);
    }
    private void deftVerifyProcess(ProgramState state) {
        // TODO: Translate COBOL paragraph '4300-DEFT-VERIFY-PROCESS'
        // Original COBOL:
        // 158100***************************************************************** 15010001
        // 158200*   TEST DEFT  FILE FOR NO HEADER OR BAD CONTROL DATE           * 15020001
        // 158300***************************************************************** 15030001
        //     PERFORM 7300-DEFT-READ-RCD
        //     IF TEN99-T04R-DEFT-EOF-YES
        //        MOVE LIT-DEFT    TO MSG-NO-DATA-FILE-NAME
        //        MOVE MSG-NO-DATA TO CC-E01W-DISPLAY-RCD
        //        MOVE "Y"         TO ERROR-FLAG
        //        PERFORM 8999-WRITE-SYSOUT
        //     ELSE
        //        PERFORM 4310-DEFT-VERIFY
        //           UNTIL END-OF-DEFT-HEADERS-YES OR
        //                 END-OF-DEFT-DETAIL-YES
        //     END-IF
        //     .
        //     EJECT
        // 159700***************************************************************** 15170001
        deftReadRcd(state);
        writeSysout(state);
        deftVerify(state);
    }
    private void deftVerify(ProgramState state) {
        // TODO: Translate COBOL paragraph '4310-DEFT-VERIFY'
        // Original COBOL:
        // 159900***************************************************************** 15190001
        // 160000*   TEST THE HEADER FOR NO DATA OR BAD CONTROL DATE             * 15200001
        // 160100***************************************************************** 15210001
        //     IF WS-T04R-DEFT-HDR-ID    = LIT-DEFT-FILE
        //        IF WS-T04R-DEFT-HDR-YY = CC-TAX-YY1 AND
        //           WS-T04R-DEFT-HDR-CC = CC-TAX-CC1 AND
        //           WS-T04R-DEFT-HDR-MM = CC-TAX-MM1
        //           CONTINUE
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
        //     PERFORM 7300-DEFT-READ-RCD
        //     .
        //     EJECT
        // 162200***************************************************************** 15420001
        writeSysout(state);
        writeSysout(state);
        deftReadRcd(state);
    }
    private void rejCycleVerifyProcess(ProgramState state) {
        // TODO: Translate COBOL paragraph '4500-REJ-CYCLE-VERIFY-PROCESS'
        // Original COBOL:
        // 162400***************************************************************** 15440001
        // 162500*   TEST FOR EMPTY FILE OR BAD CONTROL DATE                     * 15450001
        // 162600***************************************************************** 15460001
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
        //              CONTINUE
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
        // 165500***************************************************************** 15750001
        rejCycleReadRcd(state);
        writeSysout(state);
        writeSysout(state);
        writeSysout(state);
    }
    private void miscFormProcess(ProgramState state) {
        // TODO: Translate COBOL paragraph '5000-MISC-FORM-PROCESS'
        // Original COBOL:
        // 165700***************************************************************** 15770001
        // 165800*      PRIMING READ AND EOF TEST FOR MISC FORM INPUT            * 15780001
        // 165900***************************************************************** 15790001
        //     INITIALIZE WS-MISC-FORM-SEQ-CNT
        //     PERFORM 6500-INITIAL-OUTPUT-COUNT
        //     PERFORM 7000-MISC-FORM-READ-RCD
        // 166300***************************************************************** 15830001
        // 166400*   TEST FOR NO DATA ON FILE                                    * 15840001
        // 166500***************************************************************** 15850001
        //     IF TEN99-T01R-MISC-FORM-EOF-YES OR
        //        WS-T01R-MISC-FORM-TRL-VALUE = HIGH-VALUES
        //       MOVE LIT-MISC-FORM TO MSG-NO-DATA-FILE-NAME
        //       MOVE MSG-NO-DATA TO CC-E01W-DISPLAY-RCD
        //       PERFORM 8999-WRITE-SYSOUT
        //       PERFORM 5040-MISC-FORM-RESULTS-PROCESS
        //     ELSE
        //       PERFORM 5010-MISC-FORM-PROCESS-RCD UNTIL
        //               TEN99-T01R-MISC-FORM-EOF-YES  OR
        //               WS-T01R-MISC-FORM-TRL-VALUE = HIGH-VALUES
        //       PERFORM 5040-MISC-FORM-RESULTS-PROCESS
        //     END-IF
        //     .
        //     EJECT
        // 168100***************************************************************** 16010001
        initialOutputCount(state);
        miscFormReadRcd(state);
        writeSysout(state);
        miscFormResultsProcess(state);
        miscFormProcessRcd(state);
        miscFormResultsProcess(state);
    }
    private void miscFormProcessRcd(ProgramState state) {
        // TODO: Translate COBOL paragraph '5010-MISC-FORM-PROCESS-RCD'
        // Original COBOL:
        // 168300***************************************************************** 16030001
        // 168400*   PROCESS THE DETAIL AND TRAILER FOR THE MISC FORM INPUT        16040001
        // 168500*   INCREMENT COUNTERS FOR TRAILER TESTING                        16050001
        // 168600***************************************************************** 16060001
        //     ADD 1 TO WS-MISC-FORM-TRANS-COUNTER
        //     ADD WS-T01R-MISC-FORM-AMT TO WS-MISC-FORM-TRANS-AMT-ACCUM
        //     PERFORM 5020-MISC-FORM-EDIT
        //     PERFORM 5030-MISC-FORM-WRITE-TRANS-RCD
        //     PERFORM 7000-MISC-FORM-READ-RCD
        //     .
        // EJECT
        // 169500***************************************************************** 16150001
        miscFormEdit(state);
        miscFormWriteTransRcd(state);
        miscFormReadRcd(state);
    }
    private void miscFormEdit(ProgramState state) {
        // TODO: Translate COBOL paragraph '5020-MISC-FORM-EDIT'
        // Original COBOL:
        // 169700***************************************************************** 16170001
        // 169800*      BUILD THE UNIVERSAL RECORD                               * 16180001
        // 169900***************************************************************** 16190001
        //     INITIALIZE REJECT-INDICATORS
        //     ADD LIT-1 TO WS-MISC-FORM-SEQ-CNT
        //     MOVE LIT-M                         TO WS-T01W-CHECK-PREFIX
        //     MOVE CC-TAX-MM1                    TO WS-T01W-CHECK-MONTH
        //     MOVE WS-MISC-FORM-SEQ-CNT          TO WS-T01W-CHECK-SEQ
        //     MOVE WS-T01W-CHECK-NUM-KEY         TO WS-T01W-CHECK-NUM
        //     MOVE WS-T01R-MISC-FORM-BRANCH      TO WS-T01W-BRANCH
        //     MOVE WS-T01R-MISC-FORM-NAME        TO WS-T01W-NAME
        //     MOVE WS-T01R-MISC-FORM-ADDR-1      TO WS-T01W-ADDR-1
        //     MOVE WS-T01R-MISC-FORM-ADDR-2      TO WS-T01W-ADDR-2
        //     MOVE WS-T01R-MISC-FORM-CITY        TO WS-T01W-CITY
        //     MOVE WS-T01R-MISC-FORM-STATE       TO WS-T01W-STATE
        //     MOVE WS-T01R-MISC-FORM-POSTCODE    TO WS-T01W-POSTCODE
        //     MOVE WS-T01R-MISC-FORM-SSN         TO WS-T01W-SSN-US
        //     MOVE WS-T01R-MISC-FORM-OPER-LOC    TO WS-T01W-OPER-LOC
        //     MOVE WS-T01R-MISC-FORM-DESCRIPTION TO WS-T01W-COMMENT
        //     MOVE WS-T01R-MISC-FORM-AMT         TO WS-T01W-TOTAL-AMT
        //     MOVE WS-T01R-MISC-FORM-AMT         TO WS-T01W-1099-AMT
        //     MOVE WS-T01R-MISC-FORM-TAX-TYPE    TO WS-T01W-TAX-TYPE
        //     MOVE WS-T01R-MISC-FORM-TIN-IND     TO WS-T01W-TIN-IND
        //     EVALUATE TRUE
        //     WHEN WS-T01R-MISC-FORM-AMT = LIT-ZERO
        //        MOVE "Y"        TO REJECT-AMOUNT
        //        MOVE "Y"        TO REJECT-FLAG
        //     WHEN WS-T01R-MISC-FORM-AMT NOT NUMERIC
        //        MOVE "Y"        TO REJECT-AMOUNT
        //        MOVE "Y"        TO REJECT-FLAG
        //     END-EVALUATE
        //     PERFORM 6000-GENERAL-EDIT-PROCESS
        //     .
        // EJECT
        // 173200***************************************************************** 16500001
        generalEditProcess(state);
    }
    private void miscFormWriteTransRcd(ProgramState state) {
        // TODO: Translate COBOL paragraph '5030-MISC-FORM-WRITE-TRANS-RCD'
        // Original COBOL:
        // 173400***************************************************************** 16520001
        // 173500*   SETUP AND WRITE OUTPUT RECORD                               * 16530001
        // 173600***************************************************************** 16540001
        //     EVALUATE TRUE
        //     WHEN FOREIGN-INDICATOR-NO
        //        MOVE LIT-MISC-FORM TO WS-T01W-SOURCE-CODE
        //        IF REJECT-FLAG = SPACES
        //           PERFORM 8010-WRITE-TRANS-RCD
        //        ELSE
        //           INITIALIZE WS-T02W-REJECT-RECYCLING-RCD
        //                      WS-C01W-REJ-RPT-OUTPUT-RCD
        //           MOVE LIT-MISC-FORM TO WS-T02W-REJECT-SOURCE-CODE
        //           MOVE LIT-MISC-FORM TO WS-C01W-REJ-RPT-SOURCE-CODE
        //           MOVE WS-T01W-OPER-LOC TO WS-T02W-REJECT-KEY-OP-LOC
        //           MOVE WS-T01W-OPER-LOC TO WS-C01W-REJ-RPT-KEY-OP-LOC
        //           MOVE WS-T01W-BRANCH TO WS-T02W-REJECT-KEY-BR-DEPT-CD
        //           MOVE WS-T01W-BRANCH TO WS-C01W-REJ-RPT-KEY-BR-DEPT-CD
        //           MOVE WS-T01W-CHECK-DATE TO
        //                WS-T02W-REJECT-KEY-CHECK-DT
        //           MOVE WS-T01W-CHECK-DATE TO
        //                WS-C01W-REJ-RPT-KEY-CHECK-DT
        //           MOVE WS-T01W-CHECK-NUM TO
        //                WS-T02W-REJECT-KEY-CHECK-NUM
        //           MOVE WS-T01W-CHECK-NUM TO
        //                WS-C01W-REJ-RPT-KEY-CHECK-NUM
        //           MOVE WS-T01R-MISC-FORM-RCD TO WS-T02W-REJECT-DETAIL
        //           MOVE WS-T01W-OUTPUT-DETAIL TO WS-C01W-REJ-RPT-DETAIL
        //           PERFORM 8020-WRITE-REJECT-TRANS-RCD
        //           PERFORM 8030-WRITE-REJECT-REPORT-RCD
        //        END-IF
        //     WHEN FOREIGN-INDICATOR-CDN
        //        INITIALIZE WS-T03W-FOR-OUTPUT-RCD
        //        MOVE LIT-MISC-FORM TO WS-T03W-FOR-SOURCE-CODE
        //        MOVE LIT-CANADA    TO WS-T03W-FOREIGN-COUNTRY
        //        MOVE WS-T01R-MISC-FORM-RCD TO WS-T03W-FOR-DETAIL
        //        PERFORM 8040-WRITE-FOR-TRANS-RCD
        //     WHEN FOREIGN-INDICATOR-PR
        //        INITIALIZE WS-T03W-FOR-OUTPUT-RCD
        //        MOVE LIT-MISC-FORM TO WS-T03W-FOR-SOURCE-CODE
        //        MOVE LIT-PUERTO-RICO TO WS-T03W-FOREIGN-COUNTRY
        //        MOVE WS-T01R-MISC-FORM-RCD TO WS-T03W-FOR-DETAIL
        //        PERFORM 8040-WRITE-FOR-TRANS-RCD
        //     WHEN OTHER
        //        INITIALIZE WS-T03W-FOR-OUTPUT-RCD
        //        MOVE LIT-MISC-FORM TO WS-T03W-FOR-SOURCE-CODE
        //        MOVE WS-T01R-MISC-FORM-RCD TO WS-T03W-FOR-DETAIL
        //        PERFORM 8040-WRITE-FOR-TRANS-RCD
        //     END-EVALUATE
        //     .
        // EJECT
        // 178400***************************************************************** 17020001
        writeTransRcd(state);
        writeRejectTransRcd(state);
        writeRejectReportRcd(state);
        writeForTransRcd(state);
        writeForTransRcd(state);
        writeForTransRcd(state);
    }
    private void miscFormResultsProcess(ProgramState state) {
        // TODO: Translate COBOL paragraph '5040-MISC-FORM-RESULTS-PROCESS'
        // Original COBOL:
        // 178600***************************************************************** 17040001
        // 178700*      DISPLAY TRAILER RESULTS                                  * 17050001
        // 178800***************************************************************** 17060001
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
        //         PERFORM 6600-OUTPUT-BALANCING-PROCESS
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
        //     MOVE "Y" TO TEN99-T01R-MISC-FORM-EOF
        //     .
        // EJECT
        // 184000***************************************************************** 17580001
        writeSysout(state);
        writeSysout(state);
        writeSysout(state);
        writeSysout(state);
        writeSysout(state);
        outputBalancingProcess(state);
        writeSysout(state);
        writeSysout(state);
        writeSysout(state);
        writeSysout(state);
        writeSysout(state);
        writeSysout(state);
    }
    private void bccwProcess(ProgramState state) {
        // TODO: Translate COBOL paragraph '5200-BCCW-PROCESS'
        // Original COBOL:
        // 184200***************************************************************** 17600001
        // 184300*      PRIMING READ AND EOF TEST FOR B&CCW INPUT                * 17610001
        // 184400***************************************************************** 17620001
        //     PERFORM 6500-INITIAL-OUTPUT-COUNT
        // 184600***************************************************************** 17640001
        // 184700*   TEST FOR NO DATA ON FILE                                    * 17650001
        // 184800***************************************************************** 17660001
        //     IF TEN99-T03R-BCCW-EOF-YES OR
        //        END-OF-BCCW-DETAIL-YES
        //        MOVE LIT-BCCW    TO MSG-NO-DATA-FILE-NAME
        //        MOVE MSG-NO-DATA TO CC-E01W-DISPLAY-RCD
        //        PERFORM 8999-WRITE-SYSOUT
        //        PERFORM 5240-BCCW-TRAILER-PROCESS
        //        MOVE "Y"         TO ERROR-FLAG
        //     ELSE
        //        MOVE WS-T03R-BCCW-CHECK-NUM      TO WS-BCCW-CHECK-NUM
        //        MOVE WS-T03R-BCCW-TAX-TYPE       TO WS-BCCW-TAX-TYPE
        //        MOVE WS-T03R-BCCW-TIN-IND        TO WS-BCCW-TIN-IND
        //        PERFORM 5220-BCCW-EDIT
        //        SET DISB-INDEX TO 1
        //        PERFORM 5210-BCCW-PROCESS-RCD UNTIL
        //           WS-T03R-BCCW-TRL-VALUE = HIGH-VALUES
        //        PERFORM 5230-BCCW-WRITE-TRANS-RCD
        //        PERFORM 5240-BCCW-TRAILER-PROCESS
        //     END-IF
        //     .
        // EJECT
        // 186900***************************************************************** 17850001
        initialOutputCount(state);
        writeSysout(state);
        bccwTrailerProcess(state);
        bccwEdit(state);
        bccwProcessRcd(state);
        bccwWriteTransRcd(state);
        bccwTrailerProcess(state);
    }
    private void bccwProcessRcd(ProgramState state) {
        // TODO: Translate COBOL paragraph '5210-BCCW-PROCESS-RCD'
        // Original COBOL:
        // 187100***************************************************************** 17870001
        // 187200*      PROCESS THE DETAIL AND TRAILER FOR THE B&CCW ISSUES      * 17880001
        // 187300*      FIRST TIME THROUGH CHECK FOR NO DATA                     * 17890001
        // 187400*      INCREMENT COUNTERS FOR PRIMING READ                      * 17900001
        // 187500*      PROCESS THE DETAIL AND TRAILER FOR THE B&CCW ISSUES      * 17910001
        // 187600*                                                               * 17920001
        // 187700*      PROCESS RECORDS UNTIL THE KEY CHANGES.                   * 17930001
        // 187800*      KEY => CHECK NUM, TAX TYPE, TIN                          * 17930001
        // 187900*      THERE MAY BE MULTIPLE RECORDS FOR EACH CHECK NUMBER.     * 17940001
        // 188000***************************************************************** 17950001
        //     IF WS-T03R-BCCW-CHECK-NUM = WS-BCCW-CHECK-NUM
        //        IF (WS-BCCW-TAX-TYPE = WS-T03R-BCCW-TAX-TYPE)
        //            OR (WS-BCCW-TAX-TYPE = SPACE AND
        //                   WS-T03R-BCCW-TAX-TYPE > SPACE)
        //           IF WS-T03R-BCCW-TIN-IND = WS-BCCW-TIN-IND
        //                PERFORM 5215-BCCW-DIST-CNTL
        //           ELSE
        //                PERFORM 5230-BCCW-WRITE-TRANS-RCD
        //                SET DISB-INDEX TO 1
        //                PERFORM 5220-BCCW-EDIT
        //                PERFORM 5215-BCCW-DIST-CNTL
        //           END-IF
        //        ELSE
        //             PERFORM 5230-BCCW-WRITE-TRANS-RCD
        //             SET DISB-INDEX TO 1
        //             PERFORM 5220-BCCW-EDIT
        //             PERFORM 5215-BCCW-DIST-CNTL
        //        END-IF
        //     ELSE
        //        IF WS-T03R-BCCW-TRL-VALUE = HIGH-VALUES
        //           PERFORM 5230-BCCW-WRITE-TRANS-RCD
        //           PERFORM 5240-BCCW-TRAILER-PROCESS
        //        ELSE
        //           PERFORM 5230-BCCW-WRITE-TRANS-RCD
        //           SET DISB-INDEX TO 1
        //           PERFORM 5220-BCCW-EDIT
        //           PERFORM 5215-BCCW-DIST-CNTL
        //        END-IF
        //     END-IF
        //     .
        // EJECT
        bccwDistCntl(state);
        bccwWriteTransRcd(state);
        bccwEdit(state);
        bccwDistCntl(state);
        bccwWriteTransRcd(state);
        bccwEdit(state);
        bccwDistCntl(state);
        bccwWriteTransRcd(state);
        bccwTrailerProcess(state);
        bccwWriteTransRcd(state);
        bccwEdit(state);
        bccwDistCntl(state);
    }
    private void bccwDistCntl(ProgramState state) {
        // TODO: Translate COBOL paragraph '5215-BCCW-DIST-CNTL'
        // Original COBOL:
        // 191400***************************************************************** 18130001
        // 191500*      CONTROL THE PROCESSING TO COMBINE AND WRITE              * 18140001
        // 191600*      TRANSACTION RECORDS.                                     * 18150001
        // 191700***************************************************************** 18160001
        // 191800***************************************************************** 18350001
        // 191900* IF THE COMPASS CODE IS EQUAL TO SPACES AND THE RECORD         * 18360001
        // 192000* IS FROM INSURANCE - DO NOT ADD TO THE 1099 AMOUNT. SHOULD BE  * 18370001
        // 192100* REPORTED ON THE EXCLUDE FILE. EFFECTIVE WITH THE 2010 TAX     * 18370001
        // 192200* YEAR - INSURANCE/RWS IS NOT 1099 ELIGIBLE.                    * 18370001
        // 192300*                                                               *
        // 192400* IF THE COMPASS CODE IS EQUAL TO SPACES AND THE RECORD         * 18360001
        // 192500* IS FROM SPECIAL CHECK PROCESSING - CLOC BRANCH CODE = 048     * 18370001
        // 192600* THE CHECK IS 1099 ELIGIBLE.                                   * 18370001
        // 192700*                                                               *
        // 192800* IF THERE IS A COMPASS CODE, THE PROGRAM SEARCHES THE          * 18370001
        // 192900* 1099 ENTRY CODE TABLE TO DETERMINE WHETHER THE RECORD         * 18380001
        // 193000* IS 1099 REPORTABLE                                            * 18390001
        // 193100***************************************************************** 18400001
        //            ADD WS-T03R-BCCW-DISB-AMT TO WS-T01W-TOTAL-AMT
        //            ADD WS-T03R-BCCW-DISB-AMT TO
        //                WS-BCCW-TRANS-AMT-ACCUM
        //            ADD LIT-1 TO WS-T01W-NBR-DIST-RCD
        //            ADD LIT-1 TO WS-BCCW-TRANS-COUNTER
        // 193700***************************************************************** 18220001
        // 193800*        IF THE NUMBER OF DISBURSEMENTS IS GREATER THAN 5       * 18230001
        // 193900*        ACCUMULATE THE DATA FOR THE REMAINING DISBURSEMENTS    * 18240001
        // 194000*        IN THE 5TH BUCKET.                                     * 18250001
        // 194100*                                                               * 18260001
        // 194200*   3/17/97 -                                                   * 18270001
        // 194300*        SET WS-T01W-NBR-DIST-RCD TO 5 - DO NOT CHANGE THIS -   * 18280001
        // 194400*        THIS CODE PREVENTS A SOC7 - WS-T01W-DIST-RCD AREA CAN  * 18290001
        // 194500*        ONLY HAVE 5 OCCURENCES - THE WS-T01W-NBR-DIST-RCD IS   * 18300001
        // 194600*        USED TO ACCESS THE WS-T01W-DIST-RCD AREA.              * 18310001
        // 194700***************************************************************** 18320001
        //     IF DISB-INDEX > 5
        //        MOVE  5     TO WS-T01W-NBR-DIST-RCD
        //        IF WS-T03R-BCCW-COMPASS-CODE = SPACES
        //           IF WS-T03R-BCCW-CLOC-BR-CODE
        //              ADD WS-T03R-BCCW-DISB-AMT TO
        //                  WS-T01W-DIST-AMT(LIT-5)
        //              ADD WS-T03R-BCCW-DISB-AMT TO
        //                  WS-T01W-1099-AMT
        //           ELSE
        //              CONTINUE
        //           END-IF
        //        ELSE
        //           SET CODE-INDEX TO 1
        //           SEARCH WS-T07R-1099ENTRY-CODES
        //             AT END
        //               CONTINUE
        //             WHEN WS-T03R-BCCW-COMPASS-CODE =
        //                WS-T07R-1099ENTRY-CODE(CODE-INDEX)
        //               IF WS-T01W-DIST-1099-INDIC(LIT-5) = "Y"
        //                  CONTINUE
        //               ELSE
        //                  MOVE "Y" TO WS-T01W-DIST-1099-INDIC(LIT-5)
        //                  MOVE WS-T03R-BCCW-COMPASS-CODE TO
        //                     WS-T01W-DIST-COMPASS-CODE(LIT-5)
        //               END-IF
        //             ADD WS-T03R-BCCW-DISB-AMT TO
        //                 WS-T01W-1099-AMT
        //           END-SEARCH
        //           ADD WS-T03R-BCCW-DISB-AMT TO
        //             WS-T01W-DIST-AMT(LIT-5)
        //        END-IF
        //     ELSE
        //        MOVE WS-T03R-BCCW-DISB-AMT TO
        //                WS-T01W-DIST-AMT(DISB-INDEX)
        //        MOVE WS-T03R-BCCW-COMPASS-CODE TO
        //                WS-T01W-DIST-COMPASS-CODE (DISB-INDEX)
        //        IF WS-T03R-BCCW-COMPASS-CODE = SPACES
        //           IF WS-T03R-BCCW-CLOC-BR-CODE
        //               ADD WS-T03R-BCCW-DISB-AMT TO
        //                   WS-T01W-1099-AMT
        //               MOVE "Y" TO WS-T01W-DIST-1099-INDIC(DISB-INDEX)
        //           ELSE
        //               CONTINUE
        //           END-IF
        //        ELSE
        //            SET CODE-INDEX TO 1
        //            SEARCH WS-T07R-1099ENTRY-CODES
        //             AT END
        //               MOVE SPACES TO WS-T01W-DIST-1099-INDIC(DISB-INDEX)
        //             WHEN WS-T01W-DIST-COMPASS-CODE(DISB-INDEX) =
        //               WS-T07R-1099ENTRY-CODE(CODE-INDEX)
        //             MOVE "Y" TO WS-T01W-DIST-1099-INDIC(DISB-INDEX)
        //             ADD WS-T01W-DIST-AMT(DISB-INDEX)
        //               TO WS-T01W-1099-AMT
        //            END-SEARCH
        //        END-IF
        //     END-IF
        //     SET DISB-INDEX UP BY 1
        //     MOVE WS-T03R-BCCW-CHECK-NUM TO WS-BCCW-CHECK-NUM
        //     MOVE WS-T03R-BCCW-TIN-IND   TO WS-BCCW-TIN-IND
        //     IF (WS-BCCW-TAX-TYPE = SPACE) AND (WS-T03R-VALID-TAX-TYPE)
        //        MOVE WS-T03R-BCCW-TAX-TYPE TO WS-T01W-TAX-TYPE
        //     END-IF
        //     MOVE WS-T03R-BCCW-TAX-TYPE  TO WS-BCCW-TAX-TYPE
        //     PERFORM 7200-BCCW-READ-RCD
        //     .
        // EJECT
        // 201600***************************************************************** 18940001
        bccwReadRcd(state);
    }
    private void bccwEdit(ProgramState state) {
        // TODO: Translate COBOL paragraph '5220-BCCW-EDIT'
        // Original COBOL:
        // 201800***************************************************************** 18960001
        // 201900*      BUILD THE UNIVERSAL RECORD                               * 18970001
        // 202000***************************************************************** 18980001
        //     INITIALIZE SW-BCCW-BUILD-COMPLETE
        //     MOVE CC-TAX-MM1                   TO   WS-T01W-CHECK-DATE-MM
        //     MOVE CC-TAX-DD1                   TO   WS-T01W-CHECK-DATE-DD
        //     MOVE CC-TAX-YY1                   TO   WS-T01W-CHECK-DATE-YY
        //     MOVE CC-TAX-CC1                   TO   WS-T01W-CHECK-DATE-CC
        //     MOVE WS-T03R-BCCW-BRANCH-CODE TO       WS-T01W-BRANCH
        //     MOVE WS-T03R-BCCW-NAME            TO   WS-T01W-NAME
        //     MOVE WS-T03R-BCCW-ADDRESS-1       TO   WS-T01W-ADDR-1
        //     MOVE WS-T03R-BCCW-ADDRESS-2       TO   WS-T01W-ADDR-2
        //     MOVE WS-T03R-BCCW-CITY            TO   WS-T01W-CITY
        //     MOVE WS-T03R-BCCW-STATE           TO   WS-T01W-STATE
        //     MOVE WS-T03R-BCCW-ZIPCODE         TO   WS-T01W-POSTCODE
        //     MOVE WS-T03R-BCCW-SSN             TO   WS-T01W-SSN-US
        //     MOVE WS-T03R-BCCW-OP-LOC          TO   WS-T01W-OPER-LOC
        //     MOVE WS-T03R-BCCW-DESC            TO   WS-T01W-COMMENT
        //     MOVE WS-T03R-BCCW-CHECK-NUM       TO   WS-T01W-CHECK-NUM
        //     MOVE WS-T03R-BCCW-CHECK-NUM       TO   WS-T01W-CHECK-NUM-KEY
        //                                            WS-BCCW-CHECK-NUM
        //     MOVE WS-T03R-BCCW-TIN-IND         TO   WS-T01W-TIN-IND
        //                                            WS-BCCW-TIN-IND
        //     MOVE WS-T03R-BCCW-TAX-TYPE        TO   WS-T01W-TAX-TYPE
        //                                            WS-BCCW-TAX-TYPE
        //     EVALUATE TRUE
        //       WHEN WS-T03R-BCCW-DISB-AMT = LIT-ZERO
        //         IF WS-T03R-VALID-TAX-TYPE
        //             CONTINUE
        //         ELSE
        //          MOVE "Y"        TO REJECT-AMOUNT
        //          MOVE "Y"        TO REJECT-FLAG
        //         END-IF
        //       WHEN WS-T03R-BCCW-DISB-AMT NOT NUMERIC
        //          MOVE "Y"        TO REJECT-AMOUNT
        //          MOVE "Y"        TO REJECT-FLAG
        //       WHEN OTHER
        //          CONTINUE
        //     END-EVALUATE
        //     PERFORM 6000-GENERAL-EDIT-PROCESS
        //     .
        // EJECT
        // 206200***************************************************************** 19310001
        generalEditProcess(state);
    }
    private void bccwWriteTransRcd(ProgramState state) {
        // TODO: Translate COBOL paragraph '5230-BCCW-WRITE-TRANS-RCD'
        // Original COBOL:
        // 206400***************************************************************** 19330001
        // 206500*   SETUP AND WRITE OUTPUT RECORD                               * 19340001
        // 206600***************************************************************** 19350001
        //     IF FOREIGN-INDICATOR-NO
        //        MOVE LIT-BCCW TO WS-T01W-SOURCE-CODE
        //        IF REJECT-FLAG = SPACES
        //           SET DISB-INDEX TO 1
        //           IF WS-T01W-1099-AMT = LIT-ZERO
        //              PERFORM 8050-WRITE-EXCLUDES-RCD
        //              INITIALIZE WS-T01W-NBR-DIST-RCD
        //           ELSE
        //              PERFORM 8010-WRITE-TRANS-RCD
        //              INITIALIZE WS-T01W-NBR-DIST-RCD
        //           END-IF
        //        ELSE
        //           INITIALIZE WS-T02W-REJECT-RECYCLING-RCD
        //                      WS-C01W-REJ-RPT-OUTPUT-RCD
        //           MOVE LIT-BCCW      TO WS-T02W-REJECT-SOURCE-CODE
        //           MOVE LIT-BCCW      TO WS-C01W-REJ-RPT-SOURCE-CODE
        //           MOVE WS-T01W-OPER-LOC TO WS-T02W-REJECT-KEY-OP-LOC
        //           MOVE WS-T01W-OPER-LOC TO WS-C01W-REJ-RPT-KEY-OP-LOC
        //           MOVE WS-T01W-BRANCH TO WS-T02W-REJECT-KEY-BR-DEPT-CD
        //           MOVE WS-T01W-BRANCH TO WS-C01W-REJ-RPT-KEY-BR-DEPT-CD
        //           MOVE WS-T01W-CHECK-DATE TO
        //                WS-T02W-REJECT-KEY-CHECK-DT
        //           MOVE WS-T01W-CHECK-DATE TO
        //                WS-C01W-REJ-RPT-KEY-CHECK-DT
        //           MOVE WS-T01W-CHECK-NUM TO
        //                WS-T02W-REJECT-KEY-CHECK-NUM
        //           MOVE WS-T01W-CHECK-NUM TO
        //                WS-C01W-REJ-RPT-KEY-CHECK-NUM
        //           MOVE WS-T01W-TAX-TYPE  TO
        //                WS-C01W-REJ-RPT-TAX-TYPE
        //                WS-T02W-REJECT-TAX-TYPE
        //           MOVE WS-T01W-TIN-IND   TO
        //                WS-C01W-REJ-RPT-TIN-IND
        //                WS-T02W-REJECT-TIN-IND
        //           MOVE WS-T01W-OUTPUT-DETAIL    TO
        //                WS-T02W-REJECT-DETAIL
        //           MOVE WS-T01W-OUTPUT-DETAIL TO WS-C01W-REJ-RPT-DETAIL
        //           SET DISB-INDEX TO 1
        //           PERFORM 8020-WRITE-REJECT-TRANS-RCD
        //           PERFORM 8030-WRITE-REJECT-REPORT-RCD
        //           INITIALIZE WS-T01W-NBR-DIST-RCD
        //        END-IF
        //     ELSE
        //        INITIALIZE WS-T03W-FOR-OUTPUT-RCD
        //        MOVE LIT-BCCW TO WS-T03W-FOR-SOURCE-CODE
        //        MOVE WS-T01R-MISC-FORM-RCD TO WS-T03W-FOR-DETAIL
        //        SET DISB-INDEX TO 1
        //        PERFORM 8040-WRITE-FOR-TRANS-RCD
        //        INITIALIZE WS-T01W-NBR-DIST-RCD
        //     END-IF
        //     .
        // EJECT
        // 211900***************************************************************** 19820001
        writeExcludesRcd(state);
        writeTransRcd(state);
        writeRejectTransRcd(state);
        writeRejectReportRcd(state);
        writeForTransRcd(state);
    }
    private void bccwTrailerProcess(ProgramState state) {
        // TODO: Translate COBOL paragraph '5240-BCCW-TRAILER-PROCESS'
        // Original COBOL:
        // 212100***************************************************************** 19840001
        // 212200*      TEST TRAILER TOTALS AND DISPLAY RESULTS                  * 19850001
        // 212300***************************************************************** 19860001
        //     PERFORM 8999-WRITE-SYSOUT
        //     PERFORM 5250-BCCW-TRAILER-ACCUM UNTIL
        //        TEN99-T03R-BCCW-EOF-YES
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
        //     PERFORM 6600-OUTPUT-BALANCING-PROCESS
        //     PERFORM 8999-WRITE-SYSOUT
        //     MOVE "Y" TO TEN99-T03R-BCCW-EOF
        //     .
        // EJECT
        // 218300***************************************************************** 20460001
        writeSysout(state);
        bccwTrailerAccum(state);
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
        // 218500***************************************************************** 20480001
        // 218600*      ACCUMULATE BCCW TRAILER TOTALS                           * 20490001
        // 218700*      NOTE: THE FILE HAS BEEN SORTED TO INCLUDE MULTIPLE       * 20500001
        // 218800*      HEADERS AND TRAILERS.  FOR THIS REASON, THE TRAILERS     * 20510001
        // 218900*      MUST BE ACCUMULATED TO BALANCE THE DATA.                 * 20520001
        // 219000***************************************************************** 20530001
        //     ADD WS-T03R-BCCW-TRL-CNT TO WS-BCCW-TRL-COUNTER
        //     ADD WS-T03R-BCCW-TRL-AMT TO WS-BCCW-TRL-AMT-ACCUM
        //     PERFORM 7200-BCCW-READ-RCD
        //     .
        // EJECT
        // 219600***************************************************************** 20590001
        // EJECT
        // 219900***************************************************************** 20620001
        bccwReadRcd(state);
    }
    private void deftProcess(ProgramState state) {
        // TODO: Translate COBOL paragraph '5400-DEFT-PROCESS'
        // Original COBOL:
        // 220100***************************************************************** 20640001
        // 220200*      PRIMING READ AND EOF TEST FOR DEFT  INPUT                * 20650001
        // 220300***************************************************************** 20660001
        //     PERFORM 6500-INITIAL-OUTPUT-COUNT
        // 220500***************************************************************** 20680001
        // 220600*   TEST FOR NO DATA ON FILE                                    * 20690001
        // 220700***************************************************************** 20700001
        //     IF TEN99-T04R-DEFT-EOF-YES OR
        //        END-OF-DEFT-DETAIL-YES
        //        MOVE LIT-DEFT               TO MSG-NO-DATA-FILE-NAME
        //        MOVE MSG-NO-DATA            TO CC-E01W-DISPLAY-RCD
        //        PERFORM 8999-WRITE-SYSOUT
        //        PERFORM 5440-DEFT-TRAILER-PROCESS
        //     ELSE
        //        MOVE WS-T04R-DEFT-BATCH-NUM TO WS-DEFT-BATCH-NUM
        //        MOVE WS-T04R-DEFT-TAX-TYPE  TO WS-DEFT-TAX-TYPE
        //        MOVE WS-T04R-DEFT-TIN-IND   TO WS-DEFT-TIN-IND
        //        PERFORM 5420-DEFT-EDIT
        //        SET DISB-INDEX              TO 1
        //        PERFORM 5410-DEFT-PROCESS-RCD
        //          UNTIL WS-T04R-DEFT-TRL-VALUE = HIGH-VALUES
        //        PERFORM 5430-DEFT-WRITE-TRANS-RCD
        //        PERFORM 5440-DEFT-TRAILER-PROCESS
        //     END-IF
        //     .
        // EJECT
        // 222700***************************************************************** 20880001
        initialOutputCount(state);
        writeSysout(state);
        deftTrailerProcess(state);
        deftEdit(state);
        deftProcessRcd(state);
        deftWriteTransRcd(state);
        deftTrailerProcess(state);
    }
    private void deftProcessRcd(ProgramState state) {
        // TODO: Translate COBOL paragraph '5410-DEFT-PROCESS-RCD'
        // Original COBOL:
        // 222900***************************************************************** 20900001
        // 223000*      PROCESS THE DETAIL AND TRAILER FOR THE DEFT  ISSUES      * 20910001
        // 223100*      INCREMENT COUNTERS FOR PRIMING READ                      * 20920001
        // 223200*      PROCESS THE DETAIL AND TRAILER FOR THE DEFT  ISSUES      * 20930001
        // 223300*                                                               * 20940001
        // 223400*      PROCESS RECORDS UNTIL THE BATCH NUMBER CHANGES.          * 20950001
        // 223500*      THERE MAY BE MULTIPLE RECORDS FOR EACH BATCH NUMBER.     * 20960001
        // 223600***************************************************************** 20970001
        //     IF (WS-T04R-DEFT-BATCH-NUM = WS-DEFT-BATCH-NUM)
        //         AND (WS-T04R-DEFT-TAX-TYPE = WS-DEFT-TAX-TYPE)
        //         AND (WS-T04R-DEFT-TIN-IND = WS-DEFT-TIN-IND)
        //           PERFORM 5415-DEFT-DIST-CNTL
        //     ELSE
        //        IF WS-T04R-DEFT-TRL-VALUE = HIGH-VALUES
        //           PERFORM 5430-DEFT-WRITE-TRANS-RCD
        //           PERFORM 5440-DEFT-TRAILER-PROCESS
        //        ELSE
        //           PERFORM 5430-DEFT-WRITE-TRANS-RCD
        //           SET DISB-INDEX       TO 1
        //           PERFORM 5420-DEFT-EDIT
        //           PERFORM 5415-DEFT-DIST-CNTL
        //        END-IF
        //     END-IF
        //     .
        // EJECT
        deftDistCntl(state);
        deftWriteTransRcd(state);
        deftTrailerProcess(state);
        deftWriteTransRcd(state);
        deftEdit(state);
        deftDistCntl(state);
    }
    private void deftDistCntl(ProgramState state) {
        // TODO: Translate COBOL paragraph '5415-DEFT-DIST-CNTL'
        // Original COBOL:
        // 225600***************************************************************** 21150001
        // 225700*      CONTROL THE PROCESSING TO COMBINE AND WRITE              * 21160001
        // 225800*      TRANSACTION RECORDS.                                     * 21170001
        // 225900***************************************************************** 21180001
        //        ADD WS-T04R-DEFT-DISB-AMT     TO WS-T01W-TOTAL-AMT
        //                                         WS-DEFT-TRANS-AMT-ACCUM
        //        ADD LIT-1                     TO WS-T01W-NBR-DIST-RCD
        //                                         WS-DEFT-TRANS-COUNTER
        // 226400***************************************************************** 21230001
        // 226500*        IF THE NUMBER OF DISBURSEMENTS IS GREATER THAN 5       * 21240001
        // 226600*        ACCUMULATE THE DATA FOR THE REMAINING DISBURSEMENTS    * 21250001
        // 226700*        IN THE 5TH BUCKET.                                     * 21260001
        // 226800*                                                               * 21270001
        // 226900*        SET WS-T01W-NBR-DIST-RCD TO 5 - DO NOT CHANGE THIS -   * 21280001
        // 227000*        THIS CODE PREVENTS A SOC7 - WS-T01W-DIST-RCD AREA CAN  * 21290001
        // 227100*        ONLY HAVE 5 OCCURENCES - THE WS-T01W-NBR-DIST-RCD IS   * 21300001
        // 227200*        USED TO ACCESS THE WS-T01W-DIST-RCD AREA.              * 21310001
        // 227300***************************************************************** 21320001
        //     IF DISB-INDEX > 5
        //        MOVE  5                            TO WS-T01W-NBR-DIST-RCD21340001
        //        SET CODE-INDEX TO 1
        //        SEARCH WS-T07R-1099ENTRY-CODES
        //            AT END
        //               CONTINUE
        //            WHEN WS-T04R-DEFT-COMPASS-CODE =
        //                 WS-T07R-1099ENTRY-CODE(CODE-INDEX)
        //                 IF WS-T01W-DIST-1099-INDIC(LIT-5) = "Y"
        //                    CONTINUE
        //                 ELSE
        //                    MOVE "Y"                       TO
        //                         WS-T01W-DIST-1099-INDIC(LIT-5)
        //                    MOVE WS-T04R-DEFT-COMPASS-CODE TO
        //                         WS-T01W-DIST-COMPASS-CODE(LIT-5)
        //                 END-IF
        //             ADD WS-T04R-DEFT-DISB-AMT             TO
        //                 WS-T01W-1099-AMT
        //        END-SEARCH
        //        ADD WS-T04R-DEFT-DISB-AMT                  TO
        //            WS-T01W-DIST-AMT(LIT-5)
        //     ELSE
        //        MOVE WS-T04R-DEFT-DISB-AMT         TO
        //             WS-T01W-DIST-AMT(DISB-INDEX)
        //        MOVE WS-T04R-DEFT-COMPASS-CODE     TO
        //             WS-T01W-DIST-COMPASS-CODE (DISB-INDEX)
        //        SET CODE-INDEX                     TO 1
        //        SEARCH WS-T07R-1099ENTRY-CODES
        //            AT END
        //               MOVE SPACES                 TO
        //                    WS-T01W-DIST-1099-INDIC(DISB-INDEX)
        //               WHEN WS-T01W-DIST-COMPASS-CODE(DISB-INDEX) =
        //                    WS-T07R-1099ENTRY-CODE(CODE-INDEX)
        //                    MOVE "Y"               TO
        //                         WS-T01W-DIST-1099-INDIC(DISB-INDEX)
        //                    ADD WS-T01W-DIST-AMT(DISB-INDEX)
        //                                           TO WS-T01W-1099-AMT
        //        END-SEARCH
        //     END-IF
        //     SET DISB-INDEX UP BY 1
        //     MOVE WS-T04R-DEFT-BATCH-NUM        TO WS-DEFT-BATCH-NUM
        //     MOVE WS-T04R-DEFT-TIN-IND          TO WS-DEFT-TIN-IND
        //     MOVE WS-T04R-DEFT-TAX-TYPE         TO WS-DEFT-TAX-TYPE
        //     PERFORM 7300-DEFT-READ-RCD
        //          .
        // EJECT
        // 232400***************************************************************** 21810001
        deftReadRcd(state);
    }
    private void deftEdit(ProgramState state) {
        // TODO: Translate COBOL paragraph '5420-DEFT-EDIT'
        // Original COBOL:
        // 232600***************************************************************** 21830001
        // 232700*      BUILD THE UNIVERSAL RECORD                               * 21840001
        // 232800***************************************************************** 21850001
        //     INITIALIZE SW-DEFT-BUILD-COMPLETE
        //     MOVE WS-T04R-DEFT-BNK-STLMT-MM    TO   WS-T01W-CHECK-DATE-MM
        //     MOVE WS-T04R-DEFT-BNK-STLMT-DD    TO   WS-T01W-CHECK-DATE-DD
        //     MOVE WS-T04R-DEFT-BNK-STLMT-CC    TO   WS-T01W-CHECK-DATE-CC
        //     MOVE WS-T04R-DEFT-BNK-STLMT-YY    TO   WS-T01W-CHECK-DATE-YY
        //     MOVE WS-T04R-DEFT-BRANCH-CODE     TO   WS-T01W-BRANCH
        //     MOVE WS-T04R-DEFT-NAME            TO   WS-T01W-NAME
        //     MOVE WS-T04R-DEFT-ADDRESS-1       TO   WS-T01W-ADDR-1
        //     MOVE WS-T04R-DEFT-ADDRESS-2       TO   WS-T01W-ADDR-2
        //     MOVE WS-T04R-DEFT-CITY            TO   WS-T01W-CITY
        //     MOVE WS-T04R-DEFT-STATE           TO   WS-T01W-STATE
        //     MOVE WS-T04R-DEFT-ZIPCODE         TO   WS-T01W-POSTCODE
        //     MOVE WS-T04R-DEFT-SSN             TO   WS-T01W-SSN-US
        //     MOVE WS-T04R-DEFT-OP-LOC          TO   WS-T01W-OPER-LOC
        //     MOVE WS-T04R-DEFT-DESC            TO   WS-T01W-COMMENT
        //     MOVE WS-T04R-DEFT-BATCH-NUM       TO   WS-T01W-CHECK-NUM
        //     MOVE WS-T04R-DEFT-BATCH-NUM       TO   WS-T01W-CHECK-NUM-KEY
        //                                            WS-DEFT-BATCH-NUM
        //     MOVE WS-T04R-DEFT-TAX-TYPE        TO   WS-T01W-TAX-TYPE
        //                                            WS-DEFT-TAX-TYPE
        //     MOVE WS-T04R-DEFT-TIN-IND         TO   WS-T01W-TIN-IND
        //                                            WS-DEFT-TIN-IND
        //     MOVE WS-T04R-DEFT-LINE-NBR        TO   WS-T01W-LINE-NBR
        //     EVALUATE TRUE
        //     WHEN WS-T04R-DEFT-DISB-AMT = LIT-ZERO
        //        MOVE "Y"                       TO   REJECT-AMOUNT
        //                                            REJECT-FLAG
        //     WHEN WS-T04R-DEFT-DISB-AMT NOT NUMERIC
        //        MOVE "Y"                       TO   REJECT-AMOUNT
        //                                            REJECT-FLAG
        //     WHEN OTHER
        //        CONTINUE
        //     END-EVALUATE
        //     PERFORM 6000-GENERAL-EDIT-PROCESS
        //     .
        // EJECT
        // 236800***************************************************************** 22200001
        generalEditProcess(state);
    }
    private void deftWriteTransRcd(ProgramState state) {
        // TODO: Translate COBOL paragraph '5430-DEFT-WRITE-TRANS-RCD'
        // Original COBOL:
        // 237000***************************************************************** 22220001
        // 237100*   SETUP AND WRITE OUTPUT RECORD                               * 22230001
        // 237200***************************************************************** 22240001
        // 237300********************************************************          22250001
        //     MOVE LIT-DEFT      TO WS-T01W-SOURCE-CODE
        //     EVALUATE TRUE
        //     WHEN FOREIGN-INDICATOR-NO
        //        IF REJECT-FLAG = SPACES
        //           SET DISB-INDEX TO 1
        //           IF WS-T01W-1099-AMT = LIT-ZERO
        //              PERFORM 8050-WRITE-EXCLUDES-RCD
        //              INITIALIZE WS-T01W-NBR-DIST-RCD
        //           ELSE
        //              PERFORM 8010-WRITE-TRANS-RCD
        //              INITIALIZE WS-T01W-NBR-DIST-RCD
        //           END-IF
        //        ELSE
        //           INITIALIZE WS-T02W-REJECT-RECYCLING-RCD
        //                      WS-C01W-REJ-RPT-OUTPUT-RCD
        //           MOVE LIT-DEFT         TO WS-T02W-REJECT-SOURCE-CODE
        //                                    WS-C01W-REJ-RPT-SOURCE-CODE
        //           MOVE WS-T01W-OPER-LOC TO WS-T02W-REJECT-KEY-OP-LOC
        //           MOVE WS-T01W-OPER-LOC TO WS-C01W-REJ-RPT-KEY-OP-LOC
        //           MOVE WS-T01W-BRANCH   TO WS-T02W-REJECT-KEY-BR-DEPT-CD
        //           MOVE WS-T01W-BRANCH   TO WS-C01W-REJ-RPT-KEY-BR-DEPT-CD22460001
        //           MOVE WS-T01W-CHECK-DATE TO
        //                WS-T02W-REJECT-KEY-CHECK-DT
        //           MOVE WS-T01W-CHECK-DATE TO
        //                WS-C01W-REJ-RPT-KEY-CHECK-DT
        //           MOVE WS-T01W-CHECK-NUM  TO
        //                WS-T02W-REJECT-KEY-CHECK-NUM
        //           MOVE WS-T01W-CHECK-NUM  TO
        //                WS-C01W-REJ-RPT-KEY-CHECK-NUM
        //           MOVE WS-T01W-TAX-TYPE  TO
        //                WS-C01W-REJ-RPT-TAX-TYPE
        //                WS-T02W-REJECT-TAX-TYPE
        //           MOVE WS-T01W-TIN-IND   TO
        //                WS-C01W-REJ-RPT-TIN-IND
        //                WS-T02W-REJECT-TIN-IND
        //           MOVE WS-T01W-OUTPUT-DETAIL    TO
        //                WS-T02W-REJECT-DETAIL
        //           MOVE WS-T01W-OUTPUT-DETAIL TO WS-C01W-REJ-RPT-DETAIL
        //           SET DISB-INDEX TO 1
        //           PERFORM 8020-WRITE-REJECT-TRANS-RCD
        //           PERFORM 8030-WRITE-REJECT-REPORT-RCD
        //        END-IF
        //     WHEN OTHER
        //        INITIALIZE WS-T03W-FOR-OUTPUT-RCD
        //        MOVE LIT-DEFT              TO WS-T03W-FOR-SOURCE-CODE
        //        MOVE WS-T01R-MISC-FORM-RCD TO WS-T03W-FOR-DETAIL
        //        SET DISB-INDEX TO 1
        //        PERFORM 8040-WRITE-FOR-TRANS-RCD
        //        INITIALIZE WS-T01W-NBR-DIST-RCD
        //     END-EVALUATE
        //     .
        // EJECT
        // 242600***************************************************************** 22720001
        writeExcludesRcd(state);
        writeTransRcd(state);
        writeRejectTransRcd(state);
        writeRejectReportRcd(state);
        writeForTransRcd(state);
    }
    private void deftTrailerProcess(ProgramState state) {
        // TODO: Translate COBOL paragraph '5440-DEFT-TRAILER-PROCESS'
        // Original COBOL:
        // 242800***************************************************************** 22740001
        // 242900*      TEST TRAILER TOTALS AND DISPLAY RESULTS                  * 22750001
        // 243000***************************************************************** 22760001
        //     PERFORM 8999-WRITE-SYSOUT
        //     PERFORM 5450-DEFT-TRAILER-ACCUM
        //        UNTIL TEN99-T04R-DEFT-EOF-YES
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
        //     PERFORM 6600-OUTPUT-BALANCING-PROCESS
        //     PERFORM 8999-WRITE-SYSOUT
        //     MOVE "Y"                              TO TEN99-T04R-DEFT-EOF
        //     .
        // EJECT
        // 248600***************************************************************** 23320001
        writeSysout(state);
        deftTrailerAccum(state);
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
        // 248800***************************************************************** 23340001
        // 248900*      ACCUMULATE DEFT TRAILER TOTALS                           * 23350001
        // 249000***************************************************************** 23360001
        //     ADD WS-T04R-DEFT-TRL-CNT TO WS-DEFT-TRL-COUNTER
        //     ADD WS-T04R-DEFT-TRL-AMT TO WS-DEFT-TRL-AMT-ACCUM
        //     PERFORM 7300-DEFT-READ-RCD
        //     .
        // EJECT
        // 249600***************************************************************** 23420001
        deftReadRcd(state);
    }
    private void rejCycleProcess(ProgramState state) {
        // TODO: Translate COBOL paragraph '5500-REJ-CYCLE-PROCESS'
        // Original COBOL:
        // 249800***************************************************************** 23440001
        // 249900*      PRIMING READ AND EOF TEST FOR REJECT RECYCLED INPUT      * 23450001
        // 250000***************************************************************** 23460001
        //     PERFORM 6500-INITIAL-OUTPUT-COUNT
        //     PERFORM 7500-REJ-CYCLE-READ-RCD
        //     IF TEN99-T05R-REJ-CYCLE-EOF-YES OR
        //        WS-T05R-REJ-CYCLE-TRL-VALUE = HIGH-VALUES
        // 250500***************************************************************** 23510001
        // 250600*   TEST FOR NO DATA ON FILE                                    * 23520001
        // 250700***************************************************************** 23530001
        //       MOVE LIT-REJECT-FILE TO MSG-NO-DATA-FILE-NAME
        //       MOVE MSG-NO-DATA TO CC-E01W-DISPLAY-RCD
        //       PERFORM 8999-WRITE-SYSOUT
        //       PERFORM 5540-REJ-CYCLE-TRAILER-PROCESS
        //     ELSE
        //       PERFORM 5510-REJ-CYCLE-PROCESS-RCD UNTIL
        //                TEN99-T05R-REJ-CYCLE-EOF-YES
        //     END-IF
        //     .
        //     EJECT
        // 251800***************************************************************** 23640001
        initialOutputCount(state);
        rejCycleReadRcd(state);
        writeSysout(state);
        rejCycleTrailerProcess(state);
        rejCycleProcessRcd(state);
    }
    private void rejCycleProcessRcd(ProgramState state) {
        // TODO: Translate COBOL paragraph '5510-REJ-CYCLE-PROCESS-RCD'
        // Original COBOL:
        // 252000***************************************************************** 23660001
        // 252100*      PROCESS THE DETAIL AND TRAILER FOR REJECT RECYCLED INPUT * 23670001
        // 252200***************************************************************** 23680001
        //     IF WS-T05R-REJ-CYCLE-TRL-VALUE = HIGH-VALUES OR
        //           TEN99-T05R-REJ-CYCLE-EOF-YES
        //        PERFORM 5540-REJ-CYCLE-TRAILER-PROCESS
        //     ELSE
        // 252700***************************************************************** 23730001
        // 252800*   INCREMENT COUNTERS FOR TRAILER TESTING                      * 23740001
        // 252900***************************************************************** 23750001
        //        ADD 1 TO WS-REJ-CYCLE-TRANS-COUNTER
        //        PERFORM 5520-REJ-CYCLE-EDIT
        //     END-IF
        //     .
        // EJECT
        // 253500***************************************************************** 23810001
        rejCycleTrailerProcess(state);
        rejCycleEdit(state);
    }
    private void rejCycleEdit(ProgramState state) {
        // TODO: Translate COBOL paragraph '5520-REJ-CYCLE-EDIT'
        // Original COBOL:
        // 253700***************************************************************** 23830001
        // 253800*      STRIP OFF THE REJECT KEY AND PROCESS ACCORDING TO        * 23840001
        // 253900*      THE SOURCE CODE.                                         * 23850001
        // 254000***************************************************************** 23860001
        //     INITIALIZE REJECT-INDICATORS
        //                WS-T01W-OUTPUT-TRANS-RCD
        //     MOVE WS-T05R-REJECT-SOURCE-CODE TO WS-T01W-SOURCE-CODE
        //     EVALUATE TRUE
        //     WHEN WS-T05R-REJECT-SOURCE-CODE = LIT-MISC-FORM
        //        MOVE WS-T05R-REJECT-KEY-CHECK-NUM  TO WS-T01W-CHECK-NUM
        //        MOVE WS-T05R-MISC-FORM-AMT         TO WS-T01W-1099-AMT
        //        MOVE WS-T05R-REJECT-KEY-CHECK-NUM  TO
        //             WS-T01W-CHECK-NUM-KEY
        //        MOVE WS-T05R-MISC-FORM-BRANCH      TO WS-T01W-BRANCH
        //        MOVE WS-T05R-MISC-FORM-NAME        TO WS-T01W-NAME
        //        MOVE WS-T05R-MISC-FORM-ADDR-1      TO WS-T01W-ADDR-1
        //        MOVE WS-T05R-MISC-FORM-ADDR-2      TO WS-T01W-ADDR-2
        //        MOVE WS-T05R-MISC-FORM-CITY        TO WS-T01W-CITY
        //        MOVE WS-T05R-MISC-FORM-STATE       TO WS-T01W-STATE
        //        MOVE WS-T05R-MISC-FORM-POSTCODE    TO WS-T01W-POSTCODE
        //        MOVE WS-T05R-MISC-FORM-SSN         TO WS-T01W-SSN-US
        //        MOVE WS-T05R-MISC-FORM-OPER-LOC    TO WS-T01W-OPER-LOC
        //        MOVE WS-T05R-MISC-FORM-DESCRIPTION TO WS-T01W-COMMENT
        //        MOVE WS-T05R-MISC-FORM-AMT         TO WS-T01W-TOTAL-AMT
        //        MOVE WS-T05R-MISC-FORM-AMT         TO WS-T01W-1099-AMT
        //        MOVE WS-T05R-REJECT-MISC-DETAIL    TO
        //               WS-T01R-MISC-FORM-RCD
        //        EVALUATE TRUE
        //        WHEN WS-T01W-TOTAL-AMT     = LIT-ZERO
        //           MOVE "Y"        TO REJECT-AMOUNT
        //           MOVE "Y"        TO REJECT-FLAG
        //        WHEN WS-T01W-TOTAL-AMT     NOT NUMERIC
        //           MOVE "Y"        TO REJECT-AMOUNT
        //           MOVE "Y"        TO REJECT-FLAG
        //        END-EVALUATE
        //        ADD  WS-T05R-MISC-FORM-AMT TO
        //             WS-REJ-CYCLE-TRANS-AMT-ACCUM
        //        PERFORM 6000-GENERAL-EDIT-PROCESS
        //        PERFORM 5030-MISC-FORM-WRITE-TRANS-RCD
        //        PERFORM 7500-REJ-CYCLE-READ-RCD
        //     WHEN WS-T05R-REJECT-SOURCE-CODE = LIT-BCCW
        //        MOVE WS-T05R-REJECT-KEY-CHECK-NUM
        //                                       TO WS-T01W-CHECK-NUM-KEY
        //        MOVE WS-T05R-REJECT-KEY-TAX-TYPE TO WS-T01W-TAX-TYPE
        //        MOVE WS-T05R-REJECT-KEY-TIN-IND  TO WS-T01W-TIN-IND
        //        MOVE WS-T05R-REJECT-DETAIL TO WS-T01W-OUTPUT-DETAIL
        //        EVALUATE TRUE
        //        WHEN WS-T01W-TOTAL-AMT = LIT-ZERO
        //           MOVE "Y"        TO REJECT-AMOUNT
        //           MOVE "Y"        TO REJECT-FLAG
        //        WHEN WS-T01W-TOTAL-AMT     NOT NUMERIC
        //           MOVE "Y"        TO REJECT-AMOUNT
        //           MOVE "Y"        TO REJECT-FLAG
        //        END-EVALUATE
        //        ADD WS-T01W-TOTAL-AMT TO WS-REJ-CYCLE-TRANS-AMT-ACCUM
        //        PERFORM 6000-GENERAL-EDIT-PROCESS
        //        PERFORM 6400-1099ENTRY-CODE-SEARCH
        //        PERFORM 5230-BCCW-WRITE-TRANS-RCD
        //        PERFORM 7500-REJ-CYCLE-READ-RCD
        //     WHEN WS-T05R-REJECT-SOURCE-CODE = LIT-DEFT
        //        MOVE WS-T05R-REJECT-DETAIL TO WS-T01W-OUTPUT-DETAIL
        //        MOVE WS-T05R-REJECT-KEY-CHECK-NUM TO
        //                                      WS-T01W-CHECK-NUM-KEY
        //        EVALUATE TRUE
        //        WHEN WS-T01W-TOTAL-AMT = LIT-ZERO
        //           MOVE "Y"        TO REJECT-AMOUNT
        //           MOVE "Y"        TO REJECT-FLAG
        //        WHEN WS-T01W-TOTAL-AMT     NOT NUMERIC
        //           MOVE "Y"        TO REJECT-AMOUNT
        //           MOVE "Y"        TO REJECT-FLAG
        //        END-EVALUATE
        //        ADD WS-T01W-TOTAL-AMT TO WS-REJ-CYCLE-TRANS-AMT-ACCUM
        //        PERFORM 6000-GENERAL-EDIT-PROCESS
        //        PERFORM 6400-1099ENTRY-CODE-SEARCH
        //        PERFORM 5430-DEFT-WRITE-TRANS-RCD
        //        PERFORM 7500-REJ-CYCLE-READ-RCD
        //     WHEN OTHER
        // 262600***************************************************************** 24710001
        // 262700*    PROCESS UNKNOWN SOURCE CODE TYPES                          * 24720001
        // 262800***************************************************************** 24730001
        //        MOVE LIT-UNKWN-SOURCE-CODE-TYPE  TO CC-E01W-DISPLAY-RCD
        //        PERFORM 8999-WRITE-SYSOUT
        //        MOVE WS-T01W-OUTPUT-TRANS-RCD    TO CC-E01W-DISPLAY-RCD
        //        PERFORM 8999-WRITE-SYSOUT
        //        MOVE "Y" TO ERROR-FLAG
        //        PERFORM 7500-REJ-CYCLE-READ-RCD
        //     END-EVALUATE
        //     .
        // EJECT
        // 263800***************************************************************** 24830001
        generalEditProcess(state);
        miscFormWriteTransRcd(state);
        rejCycleReadRcd(state);
        generalEditProcess(state);
        ten1099entryCodeSearch(state);
        bccwWriteTransRcd(state);
        rejCycleReadRcd(state);
        generalEditProcess(state);
        ten1099entryCodeSearch(state);
        deftWriteTransRcd(state);
        rejCycleReadRcd(state);
        writeSysout(state);
        writeSysout(state);
        rejCycleReadRcd(state);
    }
    private void rejCycleTrailerProcess(ProgramState state) {
        // TODO: Translate COBOL paragraph '5540-REJ-CYCLE-TRAILER-PROCESS'
        // Original COBOL:
        // 264000***************************************************************** 24850001
        // 264100*      TEST TRAILER TOTALS AND DISPLAY RESULTS                  * 24860001
        // 264200***************************************************************** 24870001
        //     PERFORM 8999-WRITE-SYSOUT
        //     PERFORM 8999-WRITE-SYSOUT
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
        //     PERFORM 6600-OUTPUT-BALANCING-PROCESS
        //     MOVE "Y" TO TEN99-T05R-REJ-CYCLE-EOF
        //     .
        // EJECT
        // 270200***************************************************************** 25470001
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
    private void generalEditProcess(ProgramState state) {
        // TODO: Translate COBOL paragraph '6000-GENERAL-EDIT-PROCESS'
        // Original COBOL:
        // 270400***************************************************************** 25490001
        // 270500*   CHECK FOR FOREIGN TRANSACTIONS                              * 25500001
        // 270600***************************************************************** 25510001
        //     MOVE "N" TO FOREIGN-INDICATOR
        // 270800***************************************************************** 25530001
        // 270900*   CANADIAN TEST                                               * 25540001
        // 271000***************************************************************** 25550001
        //     PERFORM 6100-VERIFY-STATE-CODE
        //     IF WS-US-IND-YES
        //        CONTINUE
        //     ELSE
        //        PERFORM 6200-VERIFY-PROV-CODE
        //        IF WS-CDN-IND-YES
        //           MOVE LIT-CDN TO FOREIGN-INDICATOR
        //        ELSE
        //           MOVE "Y" TO REJECT-STATE
        //           MOVE "Y" TO REJECT-FLAG
        //        END-IF
        //     END-IF
        // 272300***************************************************************** 25680001
        // 272400*   CANADIAN TEST                                               * 25690001
        // 272500***************************************************************** 25700001
        //     IF CANADIAN-OP-LOC
        //         MOVE LIT-CDN TO FOREIGN-INDICATOR
        //     END-IF
        // 272900***************************************************************** 25740001
        // 273000*   PUERTO RICO TEST                                            * 25750001
        // 273100***************************************************************** 25760001
        //     IF WS-T01W-BRANCH-CODE = LIT-TW OR
        //        WS-T01W-STATE       = LIT-PR-STATE-CODE
        //        MOVE LIT-PR TO FOREIGN-INDICATOR
        //     END-IF
        //     IF FOREIGN-INDICATOR-NO
        //        PERFORM 6010-DOMESTIC-EDIT-PROCESS
        //     END-IF
        //     .
        // EJECT
        // 274200***************************************************************** 25870001
        verifyStateCode(state);
        verifyProvCode(state);
        domesticEditProcess(state);
    }
    private void domesticEditProcess(ProgramState state) {
        // TODO: Translate COBOL paragraph '6010-DOMESTIC-EDIT-PROCESS'
        // Original COBOL:
        // 274400***************************************************************** 25890001
        // 274500*   EDITS COMMON TO ALL FILES                                   * 25900001
        // 274600*   POSTAL CODE TEST                                            * 25910001
        // 274700***************************************************************** 25920001
        //     IF WS-T01W-POSTCODE = SPACES
        //        MOVE "Y" TO REJECT-POSTCODE
        //        MOVE "Y" TO REJECT-FLAG
        //     END-IF
        // 275300***************************************************************** 25980001
        // 275400*   NAME TEST                                                   * 25990001
        // 275500***************************************************************** 26000001
        //     IF WS-T01W-NAME = SPACES
        //        MOVE "Y" TO REJECT-NAME
        //        MOVE "Y" TO REJECT-FLAG
        //     END-IF
        // 276100***************************************************************** 26060001
        // 276200*   SOCIAL SECURITY NUMBER TEST FOR ALPHABETIC CHARACTERS       * 26070001
        // 276300*   AND SPACES (WILL ALSO BE CAUGHT IN THIS TEST)               * 26080001
        // 276400***************************************************************** 26090001
        //     IF WS-T01W-SSN-US IS NUMERIC
        //        CONTINUE
        //     ELSE
        //        MOVE "Y" TO REJECT-SSN
        //        MOVE "Y" TO REJECT-FLAG
        //     END-IF
        // 277200***************************************************************** 26170001
        // 277300*   SOCIAL SECURITY NUMBER TEST FOR VALID NUMBER                * 26180001
        // 277400***************************************************************** 26190001
        //     IF ALL-ONES
        //        MOVE "Y" TO REJECT-SSN
        //        MOVE "Y" TO REJECT-FLAG
        //     ELSE
        //        CONTINUE
        //     END-IF
        //     IF ALL-TWOS
        //        MOVE "Y" TO REJECT-SSN
        //        MOVE "Y" TO REJECT-FLAG
        //     ELSE
        //        CONTINUE
        //     END-IF
        //     IF ALL-THREES
        //        MOVE "Y" TO REJECT-SSN
        //        MOVE "Y" TO REJECT-FLAG
        //     ELSE
        //        CONTINUE
        //     END-IF
        //     IF ALL-FOURS
        //        MOVE "Y" TO REJECT-SSN
        //        MOVE "Y" TO REJECT-FLAG
        //     ELSE
        //        CONTINUE
        //     END-IF
        //     IF ALL-FIVES
        //        MOVE "Y" TO REJECT-SSN
        //        MOVE "Y" TO REJECT-FLAG
        //     ELSE
        //        CONTINUE
        //     END-IF
        //     IF ALL-SIXES
        //        MOVE "Y" TO REJECT-SSN
        //        MOVE "Y" TO REJECT-FLAG
        //     ELSE
        //        CONTINUE
        //     END-IF
        //     IF ALL-SEVENS
        //        MOVE "Y" TO REJECT-SSN
        //        MOVE "Y" TO REJECT-FLAG
        //     ELSE
        //        CONTINUE
        //     END-IF
        //     IF ALL-EIGHTS
        //        MOVE "Y" TO REJECT-SSN
        //        MOVE "Y" TO REJECT-FLAG
        //     ELSE
        //        CONTINUE
        //     END-IF
        //     IF ALL-NINES
        //        MOVE "Y" TO REJECT-SSN
        //        MOVE "Y" TO REJECT-FLAG
        //     ELSE
        //        CONTINUE
        //     END-IF
        // 283800***************************************************************** 26830001
        // 283900*   SOCIAL SECURITY NUMBER TEST FOR ZERO SSN                    * 26840001
        // 284000***************************************************************** 26850001
        //     IF WS-T01W-SSN-US = LIT-ZERO-SSN
        //        MOVE "Y" TO REJECT-SSN
        //        MOVE "Y" TO REJECT-FLAG
        //     END-IF
        // 284600***************************************************************** 26910001
        // 284700*   ADDRESS TEST                                                * 26920001
        // 284800***************************************************************** 26930001
        //     IF WS-T01W-ADDR-1 = SPACES
        //        MOVE "Y" TO REJECT-ADDRESS
        //        MOVE "Y" TO REJECT-FLAG
        //        MOVE LIT-GEN-DEL TO WS-T01W-ADDR-1
        //     END-IF
        // 285500***************************************************************** 27000001
        // 285600*   CITY TEST                                                   * 27010001
        // 285700***************************************************************** 27020001
        //     IF WS-T01W-CITY = SPACES
        //        MOVE "Y" TO REJECT-CITY
        //        MOVE "Y" TO REJECT-FLAG
        //     END-IF
        //     .
        // EJECT
        // 286400***************************************************************** 27090001

    }
    private void verifyStateCode(ProgramState state) {
        // TODO: Translate COBOL paragraph '6100-VERIFY-STATE-CODE'
        // Original COBOL:
        // 286600***************************************************************** 27110001
        // 286700*       SEARCH STATE TABLE TO CONVERT STATE CODE IF NUMERIC AND * 27120001
        // 286800*          VERIFY THAT THE STATE CODE IS ON THE TABLE           * 27130001
        // 286900*       SEARCH STATE TABLE TO VERIFY STATE CODE IF ALPHA IS     * 27140001
        // 287000*          CONTAINED ON THE TABLE.                              * 27150001
        // 287100***************************************************************** 27160001
        //     IF WS-T01W-STATE IS NUMERIC
        //        SEARCH ALL STBL-STATE-TABLE-R
        //        AT END
        //           MOVE "N" TO WS-US-IND
        //        WHEN STBL-STATE-CODE-N(STATE-INDX) = WS-T01W-STATE
        //           MOVE STBL-STATE-CODE-A(STATE-INDX) TO WS-T01W-STATE
        //           MOVE "Y" TO WS-US-IND
        //        END-SEARCH
        //     ELSE
        //        SEARCH ALL STBL-STATE-TABLE-RX
        //        AT END
        //           MOVE "N" TO WS-US-IND
        //        WHEN STBL-STATE-CODE-AX(STATE-INDX-X) = WS-T01W-STATE
        //           MOVE "Y" TO WS-US-IND
        //        END-SEARCH
        //     END-IF
        //     .
        // EJECT
        // 289000***************************************************************** 27350001

    }
    private void verifyProvCode(ProgramState state) {
        // TODO: Translate COBOL paragraph '6200-VERIFY-PROV-CODE'
        // Original COBOL:
        // 289200***************************************************************** 27370001
        // 289300*       SEARCH PROV TABLE TO CONVERT PROV CODE IF NUMERIC AND   * 27380001
        // 289400*          VERIFY THAT THE PROV CODE IS ON THE TABLE            * 27390001
        // 289500*       SEARCH PROV TABLE TO VERIFY PROV CODE IF ALPHA IS       * 27400001
        // 289600*          CONTAINED ON THE TABLE.                              * 27410001
        // 289700***************************************************************** 27420001
        //     IF WS-T01W-STATE IS NUMERIC
        //        SEARCH ALL STBL-PROV-TABLE-R
        //        AT END
        //           MOVE "N" TO WS-CDN-IND
        //        WHEN STBL-PROV-CODE-N(PROV-INDX) = WS-T01W-STATE
        //           MOVE STBL-PROV-CODE-A(PROV-INDX) TO WS-T01W-STATE
        //           MOVE "Y" TO WS-CDN-IND
        //        END-SEARCH
        //     ELSE
        //        SEARCH ALL STBL-PROV-TABLE-RX
        //        AT END
        //           MOVE "N" TO WS-CDN-IND
        //        WHEN STBL-PROV-CODE-AX(PROV-INDX-X) = WS-T01W-STATE
        //           MOVE "Y" TO WS-CDN-IND
        //        END-SEARCH
        //     END-IF
        //     .
        // EJECT
        // 291600***************************************************************** 27610001

    }
    private void ten1099entryCodeSearch(ProgramState state) {
        // TODO: Translate COBOL paragraph '6400-1099ENTRY-CODE-SEARCH'
        // Original COBOL:
        // 291800***************************************************************** 27630001
        // 291900*    PERFORM CODE SEARCH                                        * 27640001
        // 292000***************************************************************** 27650001
        //     IF DISB-INDEX < 6
        //     SET CODE-INDEX TO 1
        //     SEARCH WS-T07R-1099ENTRY-CODES
        //     AT END
        //        MOVE SPACES TO WS-T01W-DIST-1099-INDIC(DISB-INDEX)
        //     WHEN WS-T01W-DIST-COMPASS-CODE(DISB-INDEX) =
        //           WS-T07R-1099ENTRY-CODE(CODE-INDEX)
        //        MOVE "Y" TO WS-T01W-DIST-1099-INDIC(DISB-INDEX)
        // 292900*****   ADD WS-T01W-DIST-AMT(DISB-INDEX) TO WS-T01W-1099-AMT      27740001
        //     END-SEARCH
        //     ADD WS-T01W-DIST-AMT(DISB-INDEX) TO WS-TRANS-AMT-ACCUM
        //     SET DISB-INDEX UP BY 1
        //     END-IF
        //     .
        // EJECT
        // 293600***************************************************************** 27810001
        code(state);
    }
    private void initialOutputCount(ProgramState state) {
        // TODO: Translate COBOL paragraph '6500-INITIAL-OUTPUT-COUNT'
        // Original COBOL:
        // 293800***************************************************************** 27830001
        // 293900*    INITIALIZE OUTPUT COUNTERS                                 * 27840001
        // 294000***************************************************************** 27850001
        //      INITIALIZE WS-OUTPUT-TRANS-CNT
        //                 WS-OUTPUT-TRANS-AMT
        //                 WS-FOR-TRANS-CNT
        //                 WS-FOR-TRANS-AMT
        //                 WS-REJ-TRANS-CNT
        //                 WS-REJ-TRANS-AMT
        //                 WS-EXCLUDES-TRANS-CNT
        //                 WS-EXCLUDES-TRANS-AMT
        //      .
        // EJECT
        // 295100***************************************************************** 27960001

    }
    private void outputBalancingProcess(ProgramState state) {
        // TODO: Translate COBOL paragraph '6600-OUTPUT-BALANCING-PROCESS'
        // Original COBOL:
        // 295300***************************************************************** 27980001
        // 295400*    DISPLAY CONTROL TOTALS FOR ALL OUTPUTS                     * 27990001
        // 295500***************************************************************** 28000001
        //      MOVE WS-OUTPUT-TRANS-CNT TO MSG-NUM-RCDS
        //      MOVE LIT-TRANS-FILE      TO MSG-OUTPUT-FILE-NAME
        //      MOVE WS-OUTPUT-TRANS-AMT TO MSG-AMOUNT
        //      MOVE MSG-CONTROL-TOTALS  TO CC-E01W-DISPLAY-RCD
        //      PERFORM 8999-WRITE-SYSOUT
        //      MOVE WS-FOR-TRANS-CNT    TO MSG-NUM-RCDS
        //      MOVE LIT-FOREIGN-FILE    TO MSG-OUTPUT-FILE-NAME
        //      MOVE WS-FOR-TRANS-AMT    TO MSG-AMOUNT
        //      MOVE MSG-CONTROL-TOTALS  TO CC-E01W-DISPLAY-RCD
        //      PERFORM 8999-WRITE-SYSOUT
        //      MOVE WS-REJ-TRANS-CNT    TO MSG-NUM-RCDS
        //      MOVE LIT-REJECT-FILE     TO MSG-OUTPUT-FILE-NAME
        //      MOVE WS-REJ-TRANS-AMT    TO MSG-AMOUNT
        //      MOVE MSG-CONTROL-TOTALS  TO CC-E01W-DISPLAY-RCD
        //      PERFORM 8999-WRITE-SYSOUT
        //      MOVE WS-EXCLUDES-TRANS-CNT TO MSG-NUM-RCDS
        //      MOVE LIT-EXCLUDES-FILE     TO MSG-OUTPUT-FILE-NAME
        //      MOVE WS-EXCLUDES-TRANS-AMT TO MSG-AMOUNT
        //      MOVE MSG-CONTROL-TOTALS    TO CC-E01W-DISPLAY-RCD
        //      PERFORM 8999-WRITE-SYSOUT
        //      .
        // EJECT
        // 298100***************************************************************** 28260001
        writeSysout(state);
        writeSysout(state);
        writeSysout(state);
        writeSysout(state);
    }
    private void miscFormReadRcd(ProgramState state) {
        // TODO: Translate COBOL paragraph '7000-MISC-FORM-READ-RCD'
        // Original COBOL:
        // 298300***************************************************************** 28280001
        // 298400*      READ ROUTINE FOR ALL MISC FORM FILE READS                * 28290001
        // 298500***************************************************************** 28300001
        //     INITIALIZE WS-T01R-MISC-FORM-RCD
        //     READ TEN99-T01R-MISC-TRANS-FILE INTO
        //          WS-T01R-MISC-FORM-RCD
        //        AT END
        //        MOVE "Y" TO TEN99-T01R-MISC-FORM-EOF
        //     END-READ
        //     .
        // EJECT
        // 299400***************************************************************** 28390001

    }
    private void bccwReadRcd(ProgramState state) {
        // TODO: Translate COBOL paragraph '7200-BCCW-READ-RCD'
        // Original COBOL:
        // 299600***************************************************************** 28410001
        // 299700*      READ ROUTINE FOR B&CCW ISSUES                            * 28420001
        // 299800***************************************************************** 28430001
        //     INITIALIZE WS-T03R-BCCW-RCD
        //     READ TEN99-T03R-BCCW-FILE         INTO WS-T03R-BCCW-RCD
        //        AT END
        //        MOVE "Y" TO TEN99-T03R-BCCW-EOF
        //     END-READ
        //     EVALUATE TRUE
        //     WHEN WS-T03R-BCCW-HDR-VALUE = LOW-VALUES
        //        CONTINUE
        //     WHEN WS-T03R-BCCW-HDR-VALUE = HIGH-VALUES
        //        MOVE "Y" TO END-OF-BCCW-DETAIL
        //     WHEN OTHER
        //        MOVE "Y" TO END-OF-BCCW-HEADERS
        //     END-EVALUATE
        //     .
        // EJECT
        // 301400***************************************************************** 28590001

    }
    private void deftReadRcd(ProgramState state) {
        // TODO: Translate COBOL paragraph '7300-DEFT-READ-RCD'
        // Original COBOL:
        // 301600***************************************************************** 28610001
        // 301700*      READ ROUTINE FOR DEFT  ISSUES                            * 28620001
        // 301800***************************************************************** 28630001
        //     INITIALIZE WS-T04R-DEFT-RCD
        //     READ TEN99-T04R-DEFT-FILE         INTO WS-T04R-DEFT-RCD
        //        AT END
        //        MOVE "Y" TO TEN99-T04R-DEFT-EOF
        //     END-READ
        //     EVALUATE TRUE
        //     WHEN WS-T04R-DEFT-HDR-VALUE = LOW-VALUES
        //        CONTINUE
        //     WHEN WS-T04R-DEFT-HDR-VALUE = HIGH-VALUES
        //        MOVE "Y" TO END-OF-DEFT-DETAIL
        //     WHEN OTHER
        //        MOVE "Y" TO END-OF-DEFT-HEADERS
        //     END-EVALUATE
        //     .
        // EJECT
        // 303600***************************************************************** 28810001

    }
    private void rejCycleReadRcd(ProgramState state) {
        // TODO: Translate COBOL paragraph '7500-REJ-CYCLE-READ-RCD'
        // Original COBOL:
        // 303800***************************************************************** 28830001
        // 303900*      READ ROUTINE FOR ALL MISC FORM FILE READS                * 28840001
        // 304000***************************************************************** 28850001
        //     INITIALIZE WS-T05R-REJECT-RECYCLING-RCD
        //     READ TEN99-T05R-REJ-CYCLE-FILE INTO
        //             WS-T05R-REJECT-RECYCLING-RCD
        //        AT END
        //        MOVE "Y" TO TEN99-T05R-REJ-CYCLE-EOF
        //     END-READ
        //     .
        // EJECT
        // 304900***************************************************************** 28940001

    }
    private void readControlCardIn(ProgramState state) {
        // TODO: Translate COBOL paragraph '7900-READ-CONTROL-CARD-IN'
        // Original COBOL:
        // 305100***************************************************************** 28960001
        // 305200*      READ ROUTINE FOR CONTROL CARD                            * 28970001
        // 305300***************************************************************** 28980001
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
        // 306500***************************************************************** 29100001
        writeSysout(state);
        coredump(state);
    }
    private void read1099entryTable(ProgramState state) {
        // TODO: Translate COBOL paragraph '7910-READ-1099ENTRY-TABLE'
        // Original COBOL:
        // 306700***************************************************************** 29120001
        // 306800*      READ ROUTINE FOR 1099 REPORTABLE ENTRY CODE TABLE        * 29130001
        // 306900***************************************************************** 29140001
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
        // 308500***************************************************************** 29270001
        writeSysout(state);
        coredump(state);
    }
    private void writeHeaders(ProgramState state) {
        // TODO: Translate COBOL paragraph '8000-WRITE-HEADERS'
        // Original COBOL:
        // 308700***************************************************************** 29290001
        // 308800*      WRITE HEADER RECORDS FOR ALL OUTPUT FILES.               * 29300001
        // 308900***************************************************************** 29310001
        //     WRITE TEN99-T01W-TRANS-OUTPUT-RCD
        //           FROM WS-T01W-OUTPUT-TRANS-HDR
        //     WRITE TEN99-T02W-REJ-CYCLE-OUT-RCD
        //           FROM WS-T02W-REJECT-RECYCLING-HDR
        //     WRITE TEN99-T03W-FOREIGN-OUTPUT-RCD
        //           FROM WS-T03W-FOR-OUTPUT-HDR
        //     WRITE TEN99-T04W-EXCLUDES-RCD
        //           FROM WS-T04W-EXCLUDES-HDR
        //     WRITE TEN99-C01W-REJ-RPT-OUTPUT-RCD
        //           FROM WS-C01W-REJ-RPT-OUTPUT-HDR
        //     .
        // EJECT
        // 310200***************************************************************** 29440001

    }
    private void writeTransRcd(ProgramState state) {
        // TODO: Translate COBOL paragraph '8010-WRITE-TRANS-RCD'
        // Original COBOL:
        // 310400***************************************************************** 29460001
        // 310500*      WRITE TRANSACTION FOR RECORD PASSING ALL EDITS           * 29470001
        // 310600***************************************************************** 29480001
        //     ADD LIT-1 TO WS-TOTAL-OUTPUT-TRANS-CNT
        //     ADD LIT-1 TO WS-OUTPUT-TRANS-CNT
        //     ADD WS-T01W-TOTAL-AMT TO WS-TOTAL-OUTPUT-TRANS-AMT
        //     ADD WS-T01W-TOTAL-AMT TO WS-OUTPUT-TRANS-AMT
        //     MOVE LIT-A TO WS-T01W-TRANSACTION-INDICATOR
        //     WRITE TEN99-T01W-TRANS-OUTPUT-RCD
        //           FROM WS-T01W-OUTPUT-TRANS-RCD
        //     INITIALIZE WS-T01W-OUTPUT-TRANS-RCD
        //     INITIALIZE REJECT-INDICATORS
        //     .
        // EJECT
        // 311800***************************************************************** 29600001

    }
    private void writeRejectTransRcd(ProgramState state) {
        // TODO: Translate COBOL paragraph '8020-WRITE-REJECT-TRANS-RCD'
        // Original COBOL:
        // 312000***************************************************************** 29620001
        // 312100*      WRITE REJECT FILE FOR RECORD NOT PASSING ALL EDITS       * 29630001
        // 312200***************************************************************** 29640001
        //     ADD LIT-1 TO WS-TOTAL-REJ-TRANS-CNT
        //     ADD LIT-1 TO WS-REJ-TRANS-CNT
        //     ADD LIT-1 TO WS-TOTAL-REJ-RPT-TRANS-CNT
        //     ADD WS-T01W-TOTAL-AMT TO WS-TOTAL-REJ-TRANS-AMT
        //     ADD WS-T01W-TOTAL-AMT TO WS-REJ-TRANS-AMT
        //     ADD WS-T01W-TOTAL-AMT TO WS-TOTAL-REJ-RPT-TRANS-AMT
        //     WRITE TEN99-T02W-REJ-CYCLE-OUT-RCD
        //        FROM WS-T02W-REJECT-RECYCLING-RCD
        //     .
        // EJECT
        // 313300***************************************************************** 29750001

    }
    private void writeRejectReportRcd(ProgramState state) {
        // TODO: Translate COBOL paragraph '8030-WRITE-REJECT-REPORT-RCD'
        // Original COBOL:
        // 313500***************************************************************** 29770001
        // 313600*      WRITE REJECT FILE TO BE USED AS INPUT FOR REPORTS        * 29780001
        // 313700***************************************************************** 29790001
        //     MOVE REJECT-INDICATORS TO WS-C01W-REJ-RPT-REJECT-CODES
        //     WRITE TEN99-C01W-REJ-RPT-OUTPUT-RCD
        //        FROM WS-C01W-REJ-RPT-OUTPUT-RCD
        //     INITIALIZE WS-T01W-OUTPUT-TRANS-RCD
        //     INITIALIZE REJECT-INDICATORS
        //     .
        // EJECT
        // 314500***************************************************************** 29870001

    }
    private void writeForTransRcd(ProgramState state) {
        // TODO: Translate COBOL paragraph '8040-WRITE-FOR-TRANS-RCD'
        // Original COBOL:
        // 314700***************************************************************** 29890001
        // 314800*      WRITE FOREIGN TRANSACTION RECORD                         * 29900001
        // 314900***************************************************************** 29910001
        //     ADD LIT-1 TO WS-TOTAL-FOR-TRANS-CNT
        //                  WS-FOR-TRANS-CNT
        //     ADD WS-T01W-TOTAL-AMT TO WS-TOTAL-FOR-TRANS-AMT
        //     ADD WS-T01W-TOTAL-AMT TO WS-FOR-TRANS-AMT
        //     WRITE TEN99-T03W-FOREIGN-OUTPUT-RCD
        //           FROM WS-T01W-OUTPUT-TRANS-RCD
        //     INITIALIZE WS-T01W-OUTPUT-TRANS-RCD
        //     INITIALIZE REJECT-INDICATORS
        //     .
        // EJECT
        // 316000***************************************************************** 30020001

    }
    private void writeExcludesRcd(ProgramState state) {
        // TODO: Translate COBOL paragraph '8050-WRITE-EXCLUDES-RCD'
        // Original COBOL:
        // 316200***************************************************************** 30040001
        // 316300*      WRITE EXCLUDED TRANSACTION RECORDS                       * 30050001
        // 316400*      SOCIAL SECURITY NUMBER OR TRANS WITH $0 1099 REPORTABLE  * 30060001
        // 316500*      AMOUNT.                                                  * 30070001
        // 316600***************************************************************** 30080001
        //     ADD LIT-1 TO WS-TOTAL-EXCLUDES-TRANS-CNT
        //     ADD LIT-1 TO WS-EXCLUDES-TRANS-CNT
        //     ADD WS-T01W-TOTAL-AMT TO WS-TOTAL-EXCLUDES-TRANS-AMT
        //     ADD WS-T01W-TOTAL-AMT TO WS-EXCLUDES-TRANS-AMT
        //     WRITE TEN99-T04W-EXCLUDES-RCD
        //           FROM WS-T01W-OUTPUT-TRANS-RCD
        //     INITIALIZE WS-T01W-OUTPUT-TRANS-RCD
        //     INITIALIZE REJECT-INDICATORS
        //     .
        // EJECT
        // 317700*8999-WRITE-SYSOUT.                                               30190001
        // 317800***************************************************************** 30200001
        // 317900*    ++INCLUDE C2INZ002                                         * 30210001
        // 318000*      COMMON PARAGRAPH TO WRITE A SYSOUT RECORD                * 30220001
        // 318100*  8999-WRITE-SYSOUT.                                           * 30230001
        // 318200***************************************************************** 30240001
        // COPY C2INZ002.
        // EJECT

    }
    private void terminationRoutine(ProgramState state) {
        // TODO: Translate COBOL paragraph '9000-TERMINATION-ROUTINE'
        // Original COBOL:
        // 318600***************************************************************** 30280001
        // 318700*      CHECK FOR PROCESSING ERROR FLAG SET TO "Y".              * 30290001
        // 318800*      PREPARE THE TRAILER REOCRD FOR EACH OUTPUT FILE.         * 30300001
        // 318900*      PREPARE GRAND TOTALS.                                    * 30310001
        // 319000***************************************************************** 30320001
        //     IF ERROR-FLAG = "Y"
        //        PERFORM 9998-COREDUMP
        //     ELSE
        //        INITIALIZE          WS-T01W-OUTPUT-TRANS-RCD
        //                            WS-T02W-REJECT-RECYCLING-RCD
        //                            WS-T03W-FOR-OUTPUT-RCD
        //                            WS-T04W-EXCLUDES-OUTPUT-RCD
        //                            WS-C01W-REJ-RPT-OUTPUT-RCD
        //        MOVE     HIGH-VALUES  TO WS-T01W-OUTPUT-TRL-VALUE
        //                                 WS-T02W-REJECT-TRL-VALUE
        //                                 WS-T03W-FOR-TRL-VALUE
        //                                 WS-T04W-EXCLUDES-VALUE
        //                                 WS-C01W-REJ-RPT-TRL-VALUE
        //        MOVE WS-TOTAL-OUTPUT-TRANS-CNT TO WS-T01W-OUTPUT-TRL-CNT
        //        MOVE WS-TOTAL-OUTPUT-TRANS-AMT TO WS-T01W-OUTPUT-TRL-AMT
        //        MOVE WS-TOTAL-REJ-TRANS-CNT    TO WS-T02W-REJECT-TRL-CNT
        //        MOVE WS-TOTAL-REJ-TRANS-AMT    TO WS-T02W-REJECT-TRL-AMT
        //        MOVE WS-TOTAL-REJ-RPT-TRANS-CNT TO
        //             WS-C01W-REJ-RPT-TRL-CNT
        //        MOVE WS-TOTAL-REJ-RPT-TRANS-AMT TO
        //             WS-C01W-REJ-RPT-TRL-AMT
        //        MOVE WS-TOTAL-FOR-TRANS-CNT    TO WS-T03W-FOR-TRL-CNT
        //        MOVE WS-TOTAL-FOR-TRANS-AMT    TO WS-T03W-FOR-TRL-AMT
        //        MOVE WS-TOTAL-EXCLUDES-TRANS-CNT TO
        //                   WS-T04W-EXCLUDES-CNT
        //        MOVE WS-TOTAL-EXCLUDES-TRANS-AMT TO
        //                   WS-T04W-EXCLUDES-AMT
        // 321800***************************************************************** 30600001
        // 321900*      WRITE TRAILER RECORDS FOR ALL OUTPUT FILES.              * 30610001
        // 322000***************************************************************** 30620001
        //     WRITE TEN99-T01W-TRANS-OUTPUT-RCD
        //           FROM WS-T01W-OUTPUT-TRANS-TRL
        //     WRITE TEN99-T02W-REJ-CYCLE-OUT-RCD
        //           FROM WS-T02W-REJECT-RECYCLING-TRL
        //     WRITE TEN99-T03W-FOREIGN-OUTPUT-RCD
        //           FROM WS-T03W-FOR-OUTPUT-TRL
        //     WRITE TEN99-T04W-EXCLUDES-RCD
        //           FROM WS-T04W-EXCLUDES-TRL
        //     WRITE TEN99-C01W-REJ-RPT-OUTPUT-RCD
        //           FROM WS-C01W-REJ-RPT-OUTPUT-TRL
        // 323100***************************************************************** 30730001
        // 323200*  MOVE TOTALS FOR GRAND TOTAL DISPLAY                          * 30740001
        // 323300***************************************************************** 30750001
        //        MOVE WS-TOTAL-OUTPUT-TRANS-CNT TO WS-OUTPUT-TRANS-CNT
        //        MOVE WS-TOTAL-OUTPUT-TRANS-AMT TO WS-OUTPUT-TRANS-AMT
        //        MOVE WS-TOTAL-REJ-TRANS-CNT    TO WS-REJ-TRANS-CNT
        //        MOVE WS-TOTAL-REJ-TRANS-AMT    TO WS-REJ-TRANS-AMT
        //        MOVE WS-TOTAL-FOR-TRANS-AMT    TO WS-FOR-TRANS-AMT
        //        MOVE WS-TOTAL-FOR-TRANS-CNT    TO WS-FOR-TRANS-CNT
        //        MOVE WS-TOTAL-EXCLUDES-TRANS-CNT TO
        //                   WS-EXCLUDES-TRANS-CNT
        //        MOVE WS-TOTAL-EXCLUDES-TRANS-AMT TO
        //                   WS-EXCLUDES-TRANS-AMT
        //        PERFORM 8999-WRITE-SYSOUT
        //        PERFORM 8999-WRITE-SYSOUT
        //        MOVE LIT-GRAND-TOTAL TO CC-E01W-DISPLAY-RCD
        //        PERFORM 8999-WRITE-SYSOUT
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
        // 325500***************************************************************** 30970001
        // 325600*    ++INCLUDE C2INZ003                                         * 30980001
        // 325700*      COMMON ENDING SYSOUT MESSAGES                            * 30990001
        // 325800***************************************************************** 31000001
        // COPY C2INZ003.
        // EJECT
        // 326300*9998-COREDUMP.                                                   31050001
        // 326400***************************************************************** 31060001
        // 326500*    ++INCLUDE C2INZ004                                         * 31070001
        // 326600*      COMMON ABEND INFORMATION PARAGRAPH                       * 31080001
        // 326700*      9998-COREDUMP                                            * 31090001
        // 326800***************************************************************** 31100001
        // COPY C2INZ004.
        // EJECT
        // 327200*9999-CLOSE-FILES.                                                31140001
        // 327300***************************************************************** 31150001
        // 327400*    ++INCLUDE C2INZ005                                         * 31160001
        // 327500*      COMMON CLOSE FILES PARAGRAPH                             * 31170001
        // 327600*      PERFORM 9999-CLOSE-FILES                                 * 31180001
        // 327700***************************************************************** 31190001
        // COPY C2INZ005.
        //     CLOSE
        //           TEN99-T01R-MISC-TRANS-FILE
        //           TEN99-T03R-BCCW-FILE
        //           TEN99-T04R-DEFT-FILE
        //           TEN99-T05R-REJ-CYCLE-FILE
        //           WS-T07R-1099ENTRY-CD-FILE
        //           CC-R01R-CONTROL-CARD
        //           TEN99-T01W-TRANS-OUTPUT-FILE
        //           TEN99-T02W-REJ-CYCLE-OUT-FILE
        //           TEN99-C01W-REJ-RPT-OUTPUT-FILE
        //           TEN99-T03W-FOREIGN-OUTPUT-FILE
        //           TEN99-T04W-EXCLUDES-FILE
        //           .
        // 329200***************************************************************** 31340001
        // 329300*   END PROGRAM CCAC6320                                        * 31350001
        // 329400***************************************************************** 31360001
        // 
        closeFiles(state);
    }

    private void coredump(ProgramState state) {
        // Stub for 9998-COREDUMP
    }


    private void code(ProgramState state) {
        // Stub for CODE
    }


    private void writeSysout(ProgramState state) {
        // Stub for 8999-WRITE-SYSOUT
    }

}
