package com.fordcredit.misc1099.batch.config;

import org.springframework.batch.core.Job;
import org.springframework.batch.core.Step;
import org.springframework.batch.core.job.builder.JobBuilder;
import org.springframework.batch.core.repository.JobRepository;
import org.springframework.batch.core.step.builder.StepBuilder;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.transaction.PlatformTransactionManager;

/**
 * Spring Batch Job configuration for JCL: CCAC@630
 * //*=====================================================================00090000
 */
@Configuration
public class CCAC630JobConfig {

    private final JobRepository jobRepository;
    private final PlatformTransactionManager transactionManager;

    public CCAC630JobConfig(JobRepository jobRepository, 
                       PlatformTransactionManager transactionManager) {
        this.jobRepository = jobRepository;
        this.transactionManager = transactionManager;
    }

    @Bean
    public Job ccac630Job() {
        return new JobBuilder("CCAC@630", jobRepository)
                .start(step0())
                .build();
    }


    @Bean
    public Step step0() {
        return new StepBuilder("CCAC6301", jobRepository)
                .tasklet(new com.fordcredit.misc1099.batch.program.CCAC6310Tasklet("work/mainframe_clean/testcases/CCAC6310"), transactionManager)
                .build();
    }


    @Bean
    public Step step1() {
        return new StepBuilder("CCAC6302", jobRepository)
                .tasklet(new com.fordcredit.misc1099.batch.tasklet.SortTasklet("CCAC6302"), transactionManager)
                .build();
    }


    @Bean
    public Step step2() {
        return new StepBuilder("CCAC6303", jobRepository)
                .tasklet(new com.fordcredit.misc1099.batch.tasklet.SortTasklet("CCAC6303"), transactionManager)
                .build();
    }


    @Bean
    public Step step3() {
        return new StepBuilder("CCAC6304", jobRepository)
                .tasklet(new com.fordcredit.misc1099.batch.tasklet.IebgenerTasklet("CCAC6304"), transactionManager)
                .build();
    }


    @Bean
    public Step step4() {
        return new StepBuilder("CCAC6305", jobRepository)
                .tasklet(new com.fordcredit.misc1099.batch.tasklet.IebgenerTasklet("CCAC6305"), transactionManager)
                .build();
    }


    @Bean
    public Step step5() {
        return new StepBuilder("CCAC6306", jobRepository)
                .tasklet(new com.fordcredit.misc1099.batch.tasklet.SortTasklet("CCAC6306"), transactionManager)
                .build();
    }


    @Bean
    public Step step6() {
        return new StepBuilder("CCAC6307", jobRepository)
                .tasklet(new com.fordcredit.misc1099.batch.program.CCAC6320Tasklet("work/mainframe_clean/testcases/CCAC6320"), transactionManager)
                .build();
    }


    @Bean
    public Step step7() {
        return new StepBuilder("CCAC6308", jobRepository)
                .tasklet(new com.fordcredit.misc1099.batch.tasklet.SortTasklet("CCAC6308"), transactionManager)
                .build();
    }


    @Bean
    public Step step8() {
        return new StepBuilder("CCAC6309", jobRepository)
                .tasklet(new com.fordcredit.misc1099.batch.program.CCAC6330Tasklet("work/mainframe_clean/testcases/CCAC6330"), transactionManager)
                .build();
    }


    @Bean
    public Step step9() {
        return new StepBuilder("CCAC6310", jobRepository)
                .tasklet(new com.fordcredit.misc1099.batch.tasklet.SortTasklet("CCAC6310"), transactionManager)
                .build();
    }


    @Bean
    public Step step10() {
        return new StepBuilder("CCAC630A", jobRepository)
                .tasklet(new com.fordcredit.misc1099.batch.program.CCAC6340Tasklet(), transactionManager)
                .build();
    }


    @Bean
    public Step step11() {
        return new StepBuilder("CCAC630B", jobRepository)
                .tasklet(new com.fordcredit.misc1099.batch.tasklet.SortTasklet("CCAC630B"), transactionManager)
                .build();
    }


    @Bean
    public Step step12() {
        return new StepBuilder("CCAC630C", jobRepository)
                .tasklet(new com.fordcredit.misc1099.batch.tasklet.SortTasklet("CCAC630C"), transactionManager)
                .build();
    }


    @Bean
    public Step step13() {
        return new StepBuilder("CCAC630D", jobRepository)
                .tasklet(new com.fordcredit.misc1099.batch.tasklet.SortTasklet("CCAC630D"), transactionManager)
                .build();
    }


    @Bean
    public Step step14() {
        return new StepBuilder("CCAC630E", jobRepository)
                .tasklet(new com.fordcredit.misc1099.batch.tasklet.SortTasklet("CCAC630E"), transactionManager)
                .build();
    }


    @Bean
    public Step step22() {
        return new StepBuilder("CCAC630M", jobRepository)
                .tasklet(new com.fordcredit.misc1099.batch.tasklet.IebgenerTasklet("CCAC630M"), transactionManager)
                .build();
    }


    @Bean
    public Step step23() {
        return new StepBuilder("CCAC630N", jobRepository)
                .tasklet(new com.fordcredit.misc1099.batch.tasklet.SortTasklet("CCAC630N"), transactionManager)
                .build();
    }


    @Bean
    public Step step24() {
        return new StepBuilder("CCAC630P", jobRepository)
                .tasklet(new com.fordcredit.misc1099.batch.tasklet.SortTasklet("CCAC630P"), transactionManager)
                .build();
    }


    @Bean
    public Step step25() {
        return new StepBuilder("CCAC630Q", jobRepository)
                .tasklet(new com.fordcredit.misc1099.batch.tasklet.SortTasklet("CCAC630Q"), transactionManager)
                .build();
    }


    @Bean
    public Step step27() {
        return new StepBuilder("CCAC9999", jobRepository)
                .tasklet(new com.fordcredit.misc1099.batch.tasklet.IebgenerTasklet("CCAC9999"), transactionManager)
                .build();
    }

}
