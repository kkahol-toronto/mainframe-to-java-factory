package com.fordcredit.misc1099.batch.runner;

import com.fordcredit.misc1099.batch.program.CCAC6330Tasklet;

public class CCAC6330Runner {
    public static void main(String[] args) {
        String basePath = args.length > 0 ? args[0] : "work/mainframe_clean/testcases/CCAC6330";
        CCAC6330Tasklet tasklet = new CCAC6330Tasklet(basePath);
        try {
            tasklet.execute(null, null);
            System.out.println("CCAC6330 completed successfully");
        } catch (Exception e) {
            System.err.println("CCAC6330 failed: " + e.getMessage());
            e.printStackTrace();
            System.exit(1);
        }
    }
}
