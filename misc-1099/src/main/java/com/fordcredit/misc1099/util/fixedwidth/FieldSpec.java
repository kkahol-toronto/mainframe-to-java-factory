package com.fordcredit.misc1099.util.fixedwidth;

public final class FieldSpec {

    public enum Kind {
        STRING,
        DECIMAL
    }

    private final String name;
    private final int start;
    private final int length;
    private final int scale;
    private final Kind kind;

    private FieldSpec(String name, int start, int length, int scale, Kind kind) {
        this.name = name;
        this.start = start;
        this.length = length;
        this.scale = scale;
        this.kind = kind;
    }

    public static FieldSpec string(String name, int start, int length) {
        return new FieldSpec(name, start, length, 0, Kind.STRING);
    }

    public static FieldSpec decimal(String name, int start, int length, int scale) {
        return new FieldSpec(name, start, length, scale, Kind.DECIMAL);
    }

    public String name() { return name; }
    public int start() { return start; }
    public int length() { return length; }
    public int scale() { return scale; }
    public Kind kind() { return kind; }
}
