package com.fordcredit.misc1099.parser;
import java.util.List;
import com.fordcredit.misc1099.util.fixedwidth.FieldSpec;

public class C2INP004FieldSpecs {

public static List<FieldSpec> fields() {
return List.of(
FieldSpec.string("ABEND-PARAGRAPH-NAME", 1, 8),
FieldSpec.decimal("RETURN-CODE", 9, 4, 0),
FieldSpec.string("DATABASE-NAME", 13, 8),
FieldSpec.string("TABLE-NAME", 21, 8)
);
}
}
