package com.fordcredit.misc1099.batch.runner;

import com.fordcredit.misc1099.batch.program.CCAC6340Tasklet;

public class CCAC6340Runner {
    public static void main(String[] args) {
        String basePath = args.length > 0 ? args[0] : "work/mainframe_clean/testcases/CCAC6340";
        CCAC6340Tasklet tasklet = new CCAC6340Tasklet(basePath);
        try {
            tasklet.execute(null, null);
            System.out.println("CCAC6340 completed successfully");
        } catch (Exception e) {
            System.err.println("CCAC6340 failed: " + e.getMessage());
            e.printStackTrace();
            System.exit(1);
        }
    }
}
