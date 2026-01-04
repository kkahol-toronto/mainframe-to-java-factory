package com.fordcredit.misc1099.batch.runner;

import com.fordcredit.misc1099.batch.program.CCAC6350Tasklet;

public class CCAC6350Runner {
    public static void main(String[] args) {
        String basePath = args.length > 0 ? args[0] : "work/mainframe_clean/testcases/CCAC6350";
        CCAC6350Tasklet tasklet = new CCAC6350Tasklet(basePath);
        try {
            tasklet.execute(null, null);
            System.out.println("CCAC6350 completed successfully");
        } catch (Exception e) {
            System.err.println("CCAC6350 failed: " + e.getMessage());
            e.printStackTrace();
            System.exit(1);
        }
    }
}
