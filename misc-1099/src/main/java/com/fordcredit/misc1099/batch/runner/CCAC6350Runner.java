package com.fordcredit.misc1099.batch.runner;

/**
 * Standalone runner for CCAC6350Tasklet.
 * Used for golden-master testing outside Spring context.
 */
public class CCAC6350Runner {

    public static void main(String[] args) {
        System.out.println("▶ Running CCAC6350Tasklet...");
        
        try {
            com.fordcredit.misc1099.batch.program.CCAC6350Tasklet tasklet = new com.fordcredit.misc1099.batch.program.CCAC6350Tasklet();
            tasklet.execute(null, null);
            System.out.println("✔ CCAC6350Tasklet completed successfully");
        } catch (Exception e) {
            System.err.println("✖ CCAC6350Tasklet failed: " + e.getMessage());
            e.printStackTrace();
            System.exit(1);
        }
    }
}
