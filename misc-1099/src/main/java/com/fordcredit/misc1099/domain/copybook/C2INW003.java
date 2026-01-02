package com.fordcredit.misc1099.domain.copybook;

import java.math.BigDecimal;

public class C2INW003 {

    public static class SYSOUT_PANVALET_RCD {
        public String filler; // X(02)
        public String panv_literal; // X(19)
        public PANV_DATA panv_data;
        public String filler2; // X(91)

        public static class PANV_DATA {
            public BigDecimal panv_level; // 9(03)
            public String panv_program; // X(08)
            public String filler; // X(02)
            public String panv_date; // X(08)
        }
    }

    public static class SYSOUT_PROCESSING_RCD {
        public String filler; // X(02)
        public String spr_program; // X(08)
        public String filler2; // X(01)
        public String spr_start_end; // X(14)
        public SPR_DATE spr_date;
        public String filler3; // X(04)
        public SPR_TIME spr_time;
        public String filler4; // X(86)

        public static class SPR_DATE {
            public String spr_date_mo; // X(02)
            public String filler; // X(01)
            public String spr_date_da; // X(02)
            public String filler2; // X(01)
            public String spr_date_cen; // X(02)
            public String spr_date_yr; // X(02)
        }

        public static class SPR_TIME {
            public String spr_time_hh; // X(02)
            public String filler; // X(01)
            public String spr_time_mm; // X(02)
            public String filler2; // X(01)
            public String spr_time_ss; // X(02)
        }
    }

    public static class SYSOUT_ABEND_RCD {
        public String filler; // X(02)
        public String sar_paragraph_lit; // X(10)
        public String sar_paragraph; // X(31)
        public String filler2; // X(01)
        public String sar_return_code_lit; // X(13)
        public String sar_return_code; // X(05)
        public String filler3; // X(01)
        public String sar_db_name_lit; // X(10)
        public String sar_db_name; // X(11)
        public String filler4; // X(01)
        public String sar_table_name_lit; // X(07)
        public String sar_table_name; // X(31)
        public String filler5; // X(10)
    }

    public static class SYSOUT_COUNT_RCD {
        public String filler; // X(02)
        public String scr_literal; // X(30)
        public BigDecimal scr_count; // 9(13)
        public String filler2; // X(88)
    }

    public static class SYSOUT_WORK_AREAS {
        public SWA_CURRENT_DATE swa_current_date;
        public SWA_CURRENT_TIME swa_current_time;

        public static class SWA_CURRENT_DATE {
            public String swa_curr_yr; // X(02)
            public String swa_curr_mo; // X(02)
            public String swa_curr_da; // X(02)
        }

        public static class SWA_CURRENT_TIME {
            public String swa_curr_hh; // X(02)
            public String swa_curr_mm; // X(02)
            public String swa_curr_ss; // X(02)
        }
    }

    public static class SYSOUT_LITERALS {
        public SYSOUT_COUNT_LITERALS sysout_count_literals;
        public SYSOUT_ASTERISKS sysout_asterisks;
        public SYSOUT_ABEND_LIT sysout_abend_lit;
        public SYSOUT_PROCESSING_RCD_LITERALS sysout_processing_rcd_literals;

        public static class SYSOUT_COUNT_LITERALS {
            public String scl_read_lit; // X(13)
            public String scl_written_lit; // X(16)
            public String scl_page_lit; // X(14)
            public String scl_line_lit; // X(14)
        }

        public static class SYSOUT_ASTERISKS {
            public String filler; // X(02)
            public String filler2; // X(44)
            public String filler3; // X(44)
            public String filler4; // X(43)
        }

        public static class SYSOUT_ABEND_LIT {
            public String sal_paragraph_lit; // X(09)
            public String sal_return_code_lit; // X(12)
            public String sal_db_name_lit; // X(09)
            public String sal_table_name_lit; // X(06)
        }

        public static class SYSOUT_PROCESSING_RCD_LITERALS {
            public String sprl_start_lit; // X(11)
            public String sprl_end_lit; // X(09)
        }
    }

    public SYSOUT_PANVALET_RCD sysout_panvalet_rcd;
    public SYSOUT_PROCESSING_RCD sysout_processing_rcd;
    public SYSOUT_ABEND_RCD sysout_abend_rcd;
    public SYSOUT_COUNT_RCD sysout_count_rcd;
    public SYSOUT_WORK_AREAS sysout_work_areas;
    public SYSOUT_LITERALS sysout_literals;
}
