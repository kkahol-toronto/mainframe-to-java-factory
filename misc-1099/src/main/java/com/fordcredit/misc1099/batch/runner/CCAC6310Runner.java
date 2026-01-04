package com.fordcredit.misc1099.batch.runner;

import com.fordcredit.misc1099.batch.program.CCAC6310Tasklet;

public class CCAC6310Runner {
    public static void main(String[] args) {
        String basePath = args.length > 0 ? args[0] : "work/mainframe_clean/testcases/CCAC6310";
        CCAC6310Tasklet tasklet = new CCAC6310Tasklet(basePath);
        try {
            tasklet.execute(null, null);
            System.out.println("CCAC6310 completed successfully");
        } catch (Exception e) {
            System.err.println("CCAC6310 failed: " + e.getMessage());
            e.printStackTrace();
            System.exit(1);
        }
    }
}
