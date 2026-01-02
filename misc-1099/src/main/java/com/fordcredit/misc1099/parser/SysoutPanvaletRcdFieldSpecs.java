package com.fordcredit.misc1099.parser;
import java.util.List;
import com.fordcredit.misc1099.util.fixedwidth.FieldSpec;

public class SysoutPanvaletRcdFieldSpecs {

public static List<FieldSpec> fields() {
return List.of(
FieldSpec.string("FILLER", 1, 2),
FieldSpec.string("PANV-LITERAL", 3, 19),
FieldSpec.decimal("PANV-LEVEL", 22, 3, 0),
FieldSpec.string("PANV-PROGRAM", 25, 8),
FieldSpec.string("FILLER2", 33, 2),
FieldSpec.string("PANV-DATE", 35, 8),
FieldSpec.string("FILLER3", 43, 91)
);
}
}
