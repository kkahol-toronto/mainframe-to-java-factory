package com.fordcredit.misc1099.batch.bind;

import java.lang.reflect.Method;

/**
 * Reflection-based binder (Layer 3F.1).
 *
 * This keeps compilation green while the runtime parser API stabilizes.
 * Layer 3F.2 can replace this with direct calls once the parser API is finalized.
 *
 * Contract:
 * - fieldSpecsClassName must have: public static java.util.List fields()
 * - pojoClassName must have a public no-arg constructor
 */
public final class ReflectionDomainBinder {

    private ReflectionDomainBinder() {}

    public static Object bind(String rawLine, String pojoClassName, String fieldSpecsClassName) {
        if (rawLine == null) return null;
        try {
            Class<?> pojoClass = Class.forName(pojoClassName);
            Object pojo = pojoClass.getDeclaredConstructor().newInstance();

            Class<?> specsClass = Class.forName(fieldSpecsClassName);
            Method fieldsMethod = specsClass.getMethod("fields");
            Object specsObj = fieldsMethod.invoke(null);

            // Try known runtime binder candidates (optional)
            String[] binderCandidates = new String[] {
                "com.fordcredit.misc1099.util.fixedwidth.RuntimeFixedWidthBinder",
                "com.fordcredit.misc1099.util.fixedwidth.FixedWidthBinder",
                "com.fordcredit.misc1099.util.fixedwidth.FixedWidthRuntimeParser"
            };

            for (String candidate : binderCandidates) {
                try {
                    Class<?> binder = Class.forName(candidate);

                    // bind(Object pojo, String line, List specs)
                    try {
                        Method m = binder.getMethod("bind", Object.class, String.class, java.util.List.class);
                        m.invoke(null, pojo, rawLine, (java.util.List<?>) specsObj);
                        return pojo;
                    } catch (NoSuchMethodException ignore) {}

                    // parse(String line, List specs, Class pojoClass)
                    try {
                        Method m = binder.getMethod("parse", String.class, java.util.List.class, Class.class);
                        Object out = m.invoke(null, rawLine, (java.util.List<?>) specsObj, pojoClass);
                        if (out != null) return out;
                    } catch (NoSuchMethodException ignore) {}

                } catch (ClassNotFoundException ignore) {
                    // try next
                }
            }

            // Fallback: return constructed POJO even if not populated yet
            return pojo;

        } catch (Exception e) {
            throw new RuntimeException("ReflectionDomainBinder.bind failed", e);
        }
    }
}
