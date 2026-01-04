package com.fordcredit.misc1099.batch.runner;

/**
 * Standalone runner for CCAC6250Tasklet.
 * Used for golden-master testing outside Spring context.
 */
public class CCAC6250Runner {

    public static void main(String[] args) {
        System.out.println("▶ Running CCAC6250Tasklet...");
        
        try {
            com.fordcredit.misc1099.batch.program.CCAC6250Tasklet tasklet = new com.fordcredit.misc1099.batch.program.CCAC6250Tasklet();
            tasklet.execute(null, null);
            System.out.println("✔ CCAC6250Tasklet completed successfully");
        } catch (Exception e) {
            System.err.println("✖ CCAC6250Tasklet failed: " + e.getMessage());
            e.printStackTrace();
            System.exit(1);
        }
    }
}
