package com.fordcredit.misc1099.parser;
import java.util.List;
import com.fordcredit.misc1099.util.fixedwidth.FieldSpec;

public class Ten99RcdFieldSpecs {

public static List<FieldSpec> fields() {
return List.of(
FieldSpec.string("TEN99_CO_BUS_CODE_DATA", 1, 4),
FieldSpec.decimal("TEN99_FIN_LABEL_CODE_DATA", 5, 4, 0),
FieldSpec.string("TEN99_ISSUING_BR_CODE_DATA", 9, 3),
FieldSpec.string("FILLER1", 12, 1),
FieldSpec.string("TEN99_ONSITE_CHK_NUM_DATA", 13, 8),
FieldSpec.string("TEN99_NAME_DATA", 21, 40),
FieldSpec.string("TEN99_ADDRESS_LINE1_DATA", 61, 40),
FieldSpec.string("TEN99_ADDRESS_LINE2_DATA", 101, 40),
FieldSpec.string("TEN99_CITY_DATA", 141, 28),
FieldSpec.string("TEN99_STATE_DATA", 169, 5),
FieldSpec.string("TEN99_ZIP_DATA", 174, 11),
FieldSpec.string("TEN99_COMPASS_ENT_CDE_DATA", 185, 3),
FieldSpec.string("TEN99_RSN_FOR_DISB_DATA", 188, 60),
FieldSpec.decimal("TEN99_RPT_DISB_AMT_DATA", 248, 12, 2),
FieldSpec.decimal("TEN99_DISB_REQ_SEQ_NUM_DATA", 260, 9, 0),
FieldSpec.string("TEN99_SSN_TAX_ID_DATA", 269, 16),
FieldSpec.string("TEN99_PS_BUS_UNIT", 285, 5),
FieldSpec.string("TEN99_PS_OPERATION_LOC", 290, 4),
FieldSpec.string("FILLER2", 294, 24),
FieldSpec.string("TEN99_TAX_TYPE", 318, 1),
FieldSpec.string("TEN99_TIN_IND", 319, 1),
FieldSpec.string("TEN99_A_CONSTANT_DATA", 320, 1)
);
}
}
