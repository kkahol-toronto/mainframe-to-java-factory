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
 * Spring Batch Job configuration for JCL: CCAC@635
 * //*=====================================================================00090000
 */
@Configuration
public class CCAC635JobConfig {

    private final JobRepository jobRepository;
    private final PlatformTransactionManager transactionManager;

    public CCAC635JobConfig(JobRepository jobRepository, 
                       PlatformTransactionManager transactionManager) {
        this.jobRepository = jobRepository;
        this.transactionManager = transactionManager;
    }

    @Bean
    public Job ccac635Job() {
        return new JobBuilder("CCAC@635", jobRepository)
                .start(step0())
                .build();
    }


    @Bean
    public Step step0() {
        return new StepBuilder("CCAC6351", jobRepository)
                .tasklet(new com.fordcredit.misc1099.batch.tasklet.IebgenerTasklet("CCAC6351"), transactionManager)
                .build();
    }


    @Bean
    public Step step1() {
        return new StepBuilder("CCAC6352", jobRepository)
                .tasklet(new com.fordcredit.misc1099.batch.tasklet.SortTasklet("CCAC6352"), transactionManager)
                .build();
    }


    @Bean
    public Step step2() {
        return new StepBuilder("CCAC6353", jobRepository)
                .tasklet(new com.fordcredit.misc1099.batch.program.CCAC6350Tasklet(), transactionManager)
                .build();
    }


    @Bean
    public Step step4() {
        return new StepBuilder("CCAC6355", jobRepository)
                .tasklet(new com.fordcredit.misc1099.batch.tasklet.IebgenerTasklet("CCAC6355"), transactionManager)
                .build();
    }


    @Bean
    public Step step5() {
        return new StepBuilder("CCAC6356", jobRepository)
                .tasklet(new com.fordcredit.misc1099.batch.tasklet.IebgenerTasklet("CCAC6356"), transactionManager)
                .build();
    }


    @Bean
    public Step step6() {
        return new StepBuilder("CCAC6357", jobRepository)
                .tasklet(new com.fordcredit.misc1099.batch.tasklet.IebgenerTasklet("CCAC6357"), transactionManager)
                .build();
    }


    @Bean
    public Step step7() {
        return new StepBuilder("CCAC6358", jobRepository)
                .tasklet(new com.fordcredit.misc1099.batch.tasklet.IebgenerTasklet("CCAC6358"), transactionManager)
                .build();
    }


    @Bean
    public Step step14() {
        return new StepBuilder("CCAC635R", jobRepository)
                .tasklet(new com.fordcredit.misc1099.batch.tasklet.IebgenerTasklet("CCAC635R"), transactionManager)
                .build();
    }


    @Bean
    public Step step15() {
        return new StepBuilder("CCAC635S", jobRepository)
                .tasklet(new com.fordcredit.misc1099.batch.tasklet.IebgenerTasklet("CCAC635S"), transactionManager)
                .build();
    }


    @Bean
    public Step step16() {
        return new StepBuilder("CCAC9999", jobRepository)
                .tasklet(new com.fordcredit.misc1099.batch.tasklet.IebgenerTasklet("CCAC9999"), transactionManager)
                .build();
    }

}
