package com.fordcredit.misc1099.batch.runner;

public class CCAC6350Runner {
    public static void main(String[] args) {
        System.out.println("▶ Running CCAC6350Tasklet...");
        try {
            var tasklet = new com.fordcredit.misc1099.batch.program.CCAC6350Tasklet();
            tasklet.execute(null, null);
            System.out.println("✔ CCAC6350Tasklet completed successfully");
        } catch (Exception e) {
            System.err.println("✘ CCAC6350Tasklet failed: " + e.getMessage());
            e.printStackTrace();
            System.exit(1);
        }
    }
}
