package com.fordcredit.misc1099.batch.runner;

/**
 * Test runner for CCAC6340
 */
public class CCAC6340Runner {
    public static void main(String[] args) {
        System.out.println("▶ Running CCAC6340Tasklet...");
        try {
            var tasklet = new com.fordcredit.misc1099.batch.program.CCAC6340Tasklet();
            tasklet.execute(null, null);
            System.out.println("✔ CCAC6340Tasklet completed successfully");
        } catch (Exception e) {
            System.err.println("✘ CCAC6340Tasklet failed: " + e.getMessage());
            e.printStackTrace();
            System.exit(1);
        }
    }
}
