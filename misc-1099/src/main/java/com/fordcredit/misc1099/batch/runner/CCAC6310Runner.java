package com.fordcredit.misc1099.batch.runner;

public class CCAC6310Runner {
    public static void main(String[] args) {
        System.out.println("▶ Running CCAC6310Tasklet...");
        try {
            var tasklet = new com.fordcredit.misc1099.batch.program.CCAC6310Tasklet();
            tasklet.execute(null, null);
            System.out.println("✔ CCAC6310Tasklet completed successfully");
        } catch (Exception e) {
            System.err.println("✘ CCAC6310Tasklet failed: " + e.getMessage());
            e.printStackTrace();
            System.exit(1);
        }
    }
}
