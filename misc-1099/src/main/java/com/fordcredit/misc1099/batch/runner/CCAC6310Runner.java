package com.fordcredit.misc1099.batch.runner;

/**
 * Standalone runner for CCAC6310Tasklet.
 * Used for golden-master testing outside Spring context.
 */
public class CCAC6310Runner {

    public static void main(String[] args) {
        System.out.println("▶ Running CCAC6310Tasklet...");
        
        try {
            com.fordcredit.misc1099.batch.program.CCAC6310Tasklet tasklet = new com.fordcredit.misc1099.batch.program.CCAC6310Tasklet();
            tasklet.execute(null, null);
            System.out.println("✔ CCAC6310Tasklet completed successfully");
        } catch (Exception e) {
            System.err.println("✖ CCAC6310Tasklet failed: " + e.getMessage());
            e.printStackTrace();
            System.exit(1);
        }
    }
}
