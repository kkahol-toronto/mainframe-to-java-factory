package com.fordcredit.misc1099.domain.copybook;

import java.math.BigDecimal;

public class Clcww013 {

    /**
     * FILLER PIC X(14) VALUE "***1099 RCD***"
     */
    private String filler; // 14

    public String getFiller() {
        return filler;
    }

    public void setFiller(String filler) {
        this.filler = filler;
    }

    /**
     * 01 TEN99-RECORD-HDR.
     */
    private Ten99RecordHdr ten99RecordHdr;

    public Ten99RecordHdr getTen99RecordHdr() {
        return ten99RecordHdr;
    }

    public void setTen99RecordHdr(Ten99RecordHdr ten99RecordHdr) {
        this.ten99RecordHdr = ten99RecordHdr;
    }

    /**
     * 01 TEN99-RCD.
     */
    private Ten99Rcd ten99Rcd;

    public Ten99Rcd getTen99Rcd() {
        return ten99Rcd;
    }

    public void setTen99Rcd(Ten99Rcd ten99Rcd) {
        this.ten99Rcd = ten99Rcd;
    }

    /**
     * 01 TEN99-RECORD-TLR.
     */
    private Ten99RecordTlr ten99RecordTlr;

    public Ten99RecordTlr getTen99RecordTlr() {
        return ten99RecordTlr;
    }

    public void setTen99RecordTlr(Ten99RecordTlr ten99RecordTlr) {
        this.ten99RecordTlr = ten99RecordTlr;
    }

    /**
     * TEN99-RECORD-HDR
     */
    public static class Ten99RecordHdr {
        /**
         * TEN99-LOW-VALUE-HDR PIC X(10) VALUE LOW-VALUES.
         */
        private String ten99LowValueHdr; // 10

        /**
         * FILLER PIC X(02) VALUE SPACES.
         */
        private String filler1; // 2

        /**
         * TEN99-FILE-ID-HDR PIC X(17) VALUE "B&CCW 1099 FILE  ".
         */
        private String ten99FileIdHdr; // 17

        /**
         * FILLER PIC X(10) VALUE SPACES.
         */
        private String filler2; // 10

        /**
         * TEN99-PROCESSING-DATE-HDR
         */
        private Ten99ProcessingDateHdr ten99ProcessingDateHdr;

        public String getTen99LowValueHdr() {
            return ten99LowValueHdr;
        }

        public void setTen99LowValueHdr(String ten99LowValueHdr) {
            this.ten99LowValueHdr = ten99LowValueHdr;
        }

        public String getFiller1() {
            return filler1;
        }

        public void setFiller1(String filler1) {
            this.filler1 = filler1;
        }

        public String getTen99FileIdHdr() {
            return ten99FileIdHdr;
        }

        public void setTen99FileIdHdr(String ten99FileIdHdr) {
            this.ten99FileIdHdr = ten99FileIdHdr;
        }

        public String getFiller2() {
            return filler2;
        }

        public void setFiller2(String filler2) {
            this.filler2 = filler2;
        }

        public Ten99ProcessingDateHdr getTen99ProcessingDateHdr() {
            return ten99ProcessingDateHdr;
        }

        public void setTen99ProcessingDateHdr(Ten99ProcessingDateHdr ten99ProcessingDateHdr) {
            this.ten99ProcessingDateHdr = ten99ProcessingDateHdr;
        }

        /**
         * TEN99-PROCESSING-DATE-HDR
         */
        public static class Ten99ProcessingDateHdr {
            /**
             * TEN99-CC-HDR PIC X(02)
             */
            private String ten99CcHdr; // 2

            /**
             * TEN99-YY-HDR PIC X(02)
             */
            private String ten99YyHdr; // 2

            /**
             * TEN99-MM-HDR PIC X(02)
             */
            private String ten99MmHdr; // 2

            /**
             * TEN99-DD-HDR PIC X(02)
             */
            private String ten99DdHdr; // 2

            public String getTen99CcHdr() {
                return ten99CcHdr;
            }

            public void setTen99CcHdr(String ten99CcHdr) {
                this.ten99CcHdr = ten99CcHdr;
            }

            public String getTen99YyHdr() {
                return ten99YyHdr;
            }

            public void setTen99YyHdr(String ten99YyHdr) {
                this.ten99YyHdr = ten99YyHdr;
            }

            public String getTen99MmHdr() {
                return ten99MmHdr;
            }

            public void setTen99MmHdr(String ten99MmHdr) {
                this.ten99MmHdr = ten99MmHdr;
            }

            public String getTen99DdHdr() {
                return ten99DdHdr;
            }

            public void setTen99DdHdr(String ten99DdHdr) {
                this.ten99DdHdr = ten99DdHdr;
            }
        }
    }

    /**
     * TEN99-RCD
     */
    public static class Ten99Rcd {
        /**
         * TEN99-CO-BUS-CODE-DATA PIC X(04)
         */
        private String ten99CoBusCodeData; // 4

        /**
         * TEN99-FIN-LABEL-CODE-DATA PIC 9(04)
         */
        private BigDecimal ten99FinLabelCodeData; // 4, scale 0

        /**
         * TEN99-ISSUING-BR-CODE-DATA PIC X(03)
         */
        private String ten99IssuingBrCodeData; // 3

        /**
         * FILLER PIC X(01)
         */
        private String filler1; // 1

        /**
         * TEN99-ONSITE-CHK-NUM-DATA PIC X(08)
         */
        private String ten99OnsiteChkNumData; // 8

        /**
         * TEN99-NAME-DATA PIC X(40)
         */
        private String ten99NameData; // 40

        /**
         * TEN99-ADDRESS-LINE1-DATA PIC X(40)
         */
        private String ten99AddressLine1Data; // 40

        /**
         * TEN99-ADDRESS-LINE2-DATA PIC X(40)
         */
        private String ten99AddressLine2Data; // 40

        /**
         * TEN99-CITY-DATA PIC X(28)
         */
        private String ten99CityData; // 28

        /**
         * TEN99-STATE-DATA PIC X(05)
         */
        private String ten99StateData; // 5

        /**
         * TEN99-ZIP-DATA PIC X(11)
         */
        private String ten99ZipData; // 11

        /**
         * TEN99-COMPASS-ENT-CDE-DATA PIC X(03)
         */
        private String ten99CompassEntCdeData; // 3

        /**
         * TEN99-RSN-FOR-DISB-DATA PIC X(60)
         */
        private String ten99RsnForDisbData; // 60

        /**
         * TEN99-RPT-DISB-AMT-DATA PIC S9(10)V99
         */
        private BigDecimal ten99RptDisbAmtData; // 12, scale 2

        /**
         * TEN99-DISB-REQ-SEQ-NUM-DATA PIC 9(09)
         */
        private BigDecimal ten99DisbReqSeqNumData; // 9, scale 0

        /**
         * TEN99-SSN-TAX-ID-DATA PIC X(16)
         */
        private String ten99SsnTaxIdData; // 16

        /**
         * TEN99-PS-BUS-UNIT PIC X(05)
         */
        private String ten99PsBusUnit; // 5

        /**
         * TEN99-PS-OPERATION-LOC PIC X(04)
         */
        private String ten99PsOperationLoc; // 4

        /**
         * FILLER PIC X(24)
         */
        private String filler2; // 24

        /**
         * TEN99-TAX-TYPE PIC X(01)
         */
        private String ten99TaxType; // 1

        /**
         * TEN99-TIN-IND PIC X(01)
         */
        private String ten99TinInd; // 1

        /**
         * TEN99-A-CONSTANT-DATA PIC X(01) VALUE "A"
         */
        private String ten99AConstantData; // 1

        public String getTen99CoBusCodeData() {
            return ten99CoBusCodeData;
        }

        public void setTen99CoBusCodeData(String ten99CoBusCodeData) {
            this.ten99CoBusCodeData = ten99CoBusCodeData;
        }

        public BigDecimal getTen99FinLabelCodeData() {
            return ten99FinLabelCodeData;
        }

        public void setTen99FinLabelCodeData(BigDecimal ten99FinLabelCodeData) {
            this.ten99FinLabelCodeData = ten99FinLabelCodeData;
        }

        public String getTen99IssuingBrCodeData() {
            return ten99IssuingBrCodeData;
        }

        public void setTen99IssuingBrCodeData(String ten99IssuingBrCodeData) {
            this.ten99IssuingBrCodeData = ten99IssuingBrCodeData;
        }

        public String getFiller1() {
            return filler1;
        }

        public void setFiller1(String filler1) {
            this.filler1 = filler1;
        }

        public String getTen99OnsiteChkNumData() {
            return ten99OnsiteChkNumData;
        }

        public void setTen99OnsiteChkNumData(String ten99OnsiteChkNumData) {
            this.ten99OnsiteChkNumData = ten99OnsiteChkNumData;
        }

        public String getTen99NameData() {
            return ten99NameData;
        }

        public void setTen99NameData(String ten99NameData) {
            this.ten99NameData = ten99NameData;
        }

        public String getTen99AddressLine1Data() {
            return ten99AddressLine1Data;
        }

        public void setTen99AddressLine1Data(String ten99AddressLine1Data) {
            this.ten99AddressLine1Data = ten99AddressLine1Data;
        }

        public String getTen99AddressLine2Data() {
            return ten99AddressLine2Data;
        }

        public void setTen99AddressLine2Data(String ten99AddressLine2Data) {
            this.ten99AddressLine2Data = ten99AddressLine2Data;
        }

        public String getTen99CityData() {
            return ten99CityData;
        }

        public void setTen99CityData(String ten99CityData) {
            this.ten99CityData = ten99CityData;
        }

        public String getTen99StateData() {
            return ten99StateData;
        }

        public void setTen99StateData(String ten99StateData) {
            this.ten99StateData = ten99StateData;
        }

        public String getTen99ZipData() {
            return ten99ZipData;
        }

        public void setTen99ZipData(String ten99ZipData) {
            this.ten99ZipData = ten99ZipData;
        }

        public String getTen99CompassEntCdeData() {
            return ten99CompassEntCdeData;
        }

        public void setTen99CompassEntCdeData(String ten99CompassEntCdeData) {
            this.ten99CompassEntCdeData = ten99CompassEntCdeData;
        }

        public String getTen99RsnForDisbData() {
            return ten99RsnForDisbData;
        }

        public void setTen99RsnForDisbData(String ten99RsnForDisbData) {
            this.ten99RsnForDisbData = ten99RsnForDisbData;
        }

        public BigDecimal getTen99RptDisbAmtData() {
            return ten99RptDisbAmtData;
        }

        public void setTen99RptDisbAmtData(BigDecimal ten99RptDisbAmtData) {
            this.ten99RptDisbAmtData = ten99RptDisbAmtData;
        }

        public BigDecimal getTen99DisbReqSeqNumData() {
            return ten99DisbReqSeqNumData;
        }

        public void setTen99DisbReqSeqNumData(BigDecimal ten99DisbReqSeqNumData) {
            this.ten99DisbReqSeqNumData = ten99DisbReqSeqNumData;
        }

        public String getTen99SsnTaxIdData() {
            return ten99SsnTaxIdData;
        }

        public void setTen99SsnTaxIdData(String ten99SsnTaxIdData) {
            this.ten99SsnTaxIdData = ten99SsnTaxIdData;
        }

        public String getTen99PsBusUnit() {
            return ten99PsBusUnit;
        }

        public void setTen99PsBusUnit(String ten99PsBusUnit) {
            this.ten99PsBusUnit = ten99PsBusUnit;
        }

        public String getTen99PsOperationLoc() {
            return ten99PsOperationLoc;
        }

        public void setTen99PsOperationLoc(String ten99PsOperationLoc) {
            this.ten99PsOperationLoc = ten99PsOperationLoc;
        }

        public String getFiller2() {
            return filler2;
        }

        public void setFiller2(String filler2) {
            this.filler2 = filler2;
        }

        public String getTen99TaxType() {
            return ten99TaxType;
        }

        public void setTen99TaxType(String ten99TaxType) {
            this.ten99TaxType = ten99TaxType;
        }

        public String getTen99TinInd() {
            return ten99TinInd;
        }

        public void setTen99TinInd(String ten99TinInd) {
            this.ten99TinInd = ten99TinInd;
        }

        public String getTen99AConstantData() {
            return ten99AConstantData;
        }

        public void setTen99AConstantData(String ten99AConstantData) {
            this.ten99AConstantData = ten99AConstantData;
        }
    }

    /**
     * TEN99-RECORD-TLR
     */
    public static class Ten99RecordTlr {
        /**
         * TEN99-HIGH-VALUE-TLR PIC X(10) VALUE HIGH-VALUES.
         */
        private String ten99HighValueTlr; // 10

        /**
         * FILLER PIC X(02) VALUE SPACES.
         */
        private String filler1; // 2

        /**
         * TEN99-WRITTEN-CNT-TLR PIC X(08)
         */
        private String ten99WrittenCntTlr; // 8

        /**
         * FILLER PIC X(10) VALUE SPACES.
         */
        private String filler2; // 10

        /**
         * TEN99-TOTAL-DOLLAR-AMT-TLR PIC S9(13)V99
         */
        private BigDecimal ten99TotalDollarAmtTlr; // 15, scale 2

        /**
         * FILLER PIC X(180) VALUE SPACE.
         */
        private String filler3; // 180

        public String getTen99HighValueTlr() {
            return ten99HighValueTlr;
        }

        public void setTen99HighValueTlr(String ten99HighValueTlr) {
            this.ten99HighValueTlr = ten99HighValueTlr;
        }

        public String getFiller1() {
            return filler1;
        }

        public void setFiller1(String filler1) {
            this.filler1 = filler1;
        }

        public String getTen99WrittenCntTlr() {
            return ten99WrittenCntTlr;
        }

        public void setTen99WrittenCntTlr(String ten99WrittenCntTlr) {
            this.ten99WrittenCntTlr = ten99WrittenCntTlr;
        }

        public String getFiller2() {
            return filler2;
        }

        public void setFiller2(String filler2) {
            this.filler2 = filler2;
        }

        public BigDecimal getTen99TotalDollarAmtTlr() {
            return ten99TotalDollarAmtTlr;
        }

        public void setTen99TotalDollarAmtTlr(BigDecimal ten99TotalDollarAmtTlr) {
            this.ten99TotalDollarAmtTlr = ten99TotalDollarAmtTlr;
        }

        public String getFiller3() {
            return filler3;
        }

        public void setFiller3(String filler3) {
            this.filler3 = filler3;
        }
    }
}
