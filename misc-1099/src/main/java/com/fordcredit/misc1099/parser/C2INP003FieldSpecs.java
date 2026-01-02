package com.fordcredit.misc1099.parser;
import java.util.List;
import com.fordcredit.misc1099.util.fixedwidth.FieldSpec;

public class C2INP003FieldSpecs {

public static List<FieldSpec> fields() {
return List.of(
FieldSpec.string("CC-E01W-DISPLAY-RCD", 1, 80)
);
}
}
