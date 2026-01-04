package com.fordcredit.misc1099.batch.runner;

public class CCAC6320Runner {
    public static void main(String[] args) {
        System.out.println("▶ Running CCAC6320Tasklet...");
        try {
            var tasklet = new com.fordcredit.misc1099.batch.program.CCAC6320Tasklet();
            tasklet.execute(null, null);
            System.out.println("✔ CCAC6320Tasklet completed successfully");
        } catch (Exception e) {
            System.err.println("✘ CCAC6320Tasklet failed: " + e.getMessage());
            e.printStackTrace();
            System.exit(1);
        }
    }
}
