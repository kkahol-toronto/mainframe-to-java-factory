package com.fordcredit.misc1099.domain.copybook;

import java.math.BigDecimal;

public class C2INP001 {

    // 01 SWA-CURRENT-TIME.
    public static class SWA_CURRENT_TIME {
        // 05 SWA-CURR-HH     PIC 99.
        public BigDecimal swa_curr_hh;
        // 05 SWA-CURR-MM     PIC 99.
        public BigDecimal swa_curr_mm;
        // 05 SWA-CURR-SS     PIC 99.
        public BigDecimal swa_curr_ss;
    }

    // 01 SWA-CURRENT-DATE.
    public static class SWA_CURRENT_DATE {
        // 05 SWA-CURR-MO     PIC 99.
        public BigDecimal swa_curr_mo;
        // 05 SWA-CURR-DA     PIC 99.
        public BigDecimal swa_curr_da;
        // 05 SWA-CURR-YR     PIC 99.
        public BigDecimal swa_curr_yr;
    }

    // 01 SPR-TIME.
    public static class SPR_TIME {
        // 05 SPR-TIME-HH     PIC 99.
        public BigDecimal spr_time_hh;
        // 05 SPR-TIME-MM     PIC 99.
        public BigDecimal spr_time_mm;
        // 05 SPR-TIME-SS     PIC 99.
        public BigDecimal spr_time_ss;
    }

    // 01 SPR-DATE.
    public static class SPR_DATE {
        // 05 SPR-DATE-CEN    PIC 99.
        public BigDecimal spr_date_cen;
        // 05 SPR-DATE-YR     PIC 99.
        public BigDecimal spr_date_yr;
        // 05 SPR-DATE-MO     PIC 99.
        public BigDecimal spr_date_mo;
        // 05 SPR-DATE-DA     PIC 99.
        public BigDecimal spr_date_da;
    }

    // 01 PANV-DATA.
    public static class PANV_DATA {
        // 05 PAN-VALET       PIC X(20).
        public String pan_valet;
        // 05 PANV-PROGRAM    PIC X(8).
        public String panv_program;
    }

    // 01 SPR-PROGRAM       PIC X(8).
    public String spr_program;

    // 01 SPR-START-END     PIC X(10).
    public String spr_start_end;

    // 01 SYSOUT-ASTERISKS  PIC X(80).
    public String sysout_asterisks;

    // 01 SYSOUT-PANVALET-RCD PIC X(80).
    public String sysout_panvalet_rcd;

    // 01 SYSOUT-PROCESSING-RCD PIC X(80).
    public String sysout_processing_rcd;

    // 01 CC-E01W-DISPLAY-RCD PIC X(80).
    public String cc_e01w_display_rcd;

    // 01 SPACES            PIC X(80).
    public String spaces;

    // 01 SWA-CURRENT-TIME
    public SWA_CURRENT_TIME swa_current_time;

    // 01 SWA-CURRENT-DATE
    public SWA_CURRENT_DATE swa_current_date;

    // 01 SPR-TIME
    public SPR_TIME spr_time;

    // 01 SPR-DATE
    public SPR_DATE spr_date;

    // 01 PANV-DATA
    public PANV_DATA panv_data;

    public C2INP001() {
        // No initialization or defaults as per requirements
    }
}
