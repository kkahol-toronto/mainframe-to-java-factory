package com.fordcredit.misc1099.parser;
import java.util.List;
import com.fordcredit.misc1099.util.fixedwidth.FieldSpec;

public class CcE01wDisplayRcdFieldSpecs {

public static List<FieldSpec> fields() {
return List.of(
FieldSpec.string("ccE01wDisplayRcd", 1, 133)
);
}
}
