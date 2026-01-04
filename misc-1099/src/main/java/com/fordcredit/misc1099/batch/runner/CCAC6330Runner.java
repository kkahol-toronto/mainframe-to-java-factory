package com.fordcredit.misc1099.batch.runner;

/**
 * Standalone runner for CCAC6330Tasklet.
 * Used for golden-master testing outside Spring context.
 */
public class CCAC6330Runner {

    public static void main(String[] args) {
        System.out.println("▶ Running CCAC6330Tasklet...");
        
        try {
            com.fordcredit.misc1099.batch.program.CCAC6330Tasklet tasklet = new com.fordcredit.misc1099.batch.program.CCAC6330Tasklet();
            tasklet.execute(null, null);
            System.out.println("✔ CCAC6330Tasklet completed successfully");
        } catch (Exception e) {
            System.err.println("✖ CCAC6330Tasklet failed: " + e.getMessage());
            e.printStackTrace();
            System.exit(1);
        }
    }
}
