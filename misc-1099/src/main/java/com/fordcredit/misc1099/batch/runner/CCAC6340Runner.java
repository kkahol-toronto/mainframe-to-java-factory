package com.fordcredit.misc1099.batch.runner;

import com.fordcredit.misc1099.batch.program.CCAC6340Tasklet;

public class CCAC6340Runner {
    public static void main(String[] args) {
        CCAC6340Tasklet tasklet = new CCAC6340Tasklet();
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
