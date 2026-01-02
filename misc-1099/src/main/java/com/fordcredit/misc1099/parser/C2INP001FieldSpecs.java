package com.fordcredit.misc1099.parser;
import java.util.List;
import com.fordcredit.misc1099.util.fixedwidth.FieldSpec;

public class C2INP001FieldSpecs {

public static List<FieldSpec> fields() {
return List.of(
FieldSpec.string("SWA-CURRENT-TIME", 1, 8),
FieldSpec.string("SWA-CURR-HH", 9, 2),
FieldSpec.string("SWA-CURR-MM", 11, 2),
FieldSpec.string("SWA-CURR-SS", 13, 2),
FieldSpec.string("SWA-CURRENT-DATE", 15, 8),
FieldSpec.string("SWA-CURR-MO", 23, 2),
FieldSpec.string("SWA-CURR-DA", 25, 2),
FieldSpec.string("SWA-CURR-YR", 27, 2),
FieldSpec.string("SPR-DATE-CEN", 29, 2),
FieldSpec.string("SYSOUT-ASTERISKS", 31, 80),
FieldSpec.string("PAN-VALET", 111, 40),
FieldSpec.string("SYSOUT-PANVALET-RCD", 151, 80),
FieldSpec.string("PANV-PROGRAM", 231, 8),
FieldSpec.string("SPRL-START-LIT", 239, 8),
FieldSpec.string("SYSOUT-PROCESSING-RCD", 247, 80),
FieldSpec.string("CC-E01W-DISPLAY-RCD", 327, 80)
);
}
}
