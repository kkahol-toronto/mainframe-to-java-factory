package com.fordcredit.misc1099.batch.runner;

/**
 * Standalone runner for CCAC6340Tasklet.
 * Used for golden-master testing outside Spring context.
 */
public class CCAC6340Runner {

    public static void main(String[] args) {
        System.out.println("▶ Running CCAC6340Tasklet...");
        
        try {
            com.fordcredit.misc1099.batch.program.CCAC6340Tasklet tasklet = new com.fordcredit.misc1099.batch.program.CCAC6340Tasklet();
            tasklet.execute(null, null);
            System.out.println("✔ CCAC6340Tasklet completed successfully");
        } catch (Exception e) {
            System.err.println("✖ CCAC6340Tasklet failed: " + e.getMessage());
            e.printStackTrace();
            System.exit(1);
        }
    }
}
