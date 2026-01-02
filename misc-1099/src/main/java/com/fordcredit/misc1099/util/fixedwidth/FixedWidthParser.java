package com.fordcredit.misc1099.util.fixedwidth;

import java.lang.reflect.Method;
import java.math.BigDecimal;
import java.util.List;
import java.util.function.Supplier;

public final class FixedWidthParser<T> {

    private final Supplier<T> factory;
    private final List<FieldSpec> fields;

    public FixedWidthParser(Supplier<T> factory, List<FieldSpec> fields) {
        this.factory = factory;
        this.fields = fields;
    }

    public T parse(String line) {
        T instance = factory.get();

        for (FieldSpec f : fields) {
            try {
                String raw = line.substring(
                    f.start(),
                    f.start() + f.length()
                ).trim();

                String setterName =
                    "set" + Character.toUpperCase(f.name().charAt(0))
                    + f.name().substring(1);

                Method setter;

                switch (f.kind()) {
                    case STRING -> {
                        setter = instance.getClass()
                            .getMethod(setterName, String.class);
                        setter.invoke(instance, raw);
                    }
                    case DECIMAL -> {
                        setter = instance.getClass()
                            .getMethod(setterName, BigDecimal.class);
                        BigDecimal val = raw.isEmpty()
                            ? BigDecimal.ZERO
                            : new BigDecimal(raw).movePointLeft(f.scale());
                        setter.invoke(instance, val);
                    }
                }
            } catch (Exception e) {
                throw new RuntimeException(
                    "Failed to bind field " + f.name(), e
                );
            }
        }

        return instance;
    }
}
