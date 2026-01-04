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
 * Spring Batch Job configuration for JCL: CCAC@640
 * NARRATIVE
 */
@Configuration
public class CCAC640JobConfig {

    private final JobRepository jobRepository;
    private final PlatformTransactionManager transactionManager;

    public CCAC640JobConfig(JobRepository jobRepository, 
                       PlatformTransactionManager transactionManager) {
        this.jobRepository = jobRepository;
        this.transactionManager = transactionManager;
    }

    @Bean
    public Job ccac640Job() {
        return new JobBuilder("CCAC@640", jobRepository)
                .start(step0())
                .build();
    }


    @Bean
    public Step step0() {
        return new StepBuilder("CCAC6401", jobRepository)
                .tasklet(new com.fordcredit.misc1099.batch.tasklet.SortTasklet("CCAC6401"), transactionManager)
                .build();
    }


    @Bean
    public Step step1() {
        return new StepBuilder("CCAC6402", jobRepository)
                .tasklet(new com.fordcredit.misc1099.batch.tasklet.IebgenerTasklet("CCAC6402"), transactionManager)
                .build();
    }


    @Bean
    public Step step3() {
        return new StepBuilder("CCAC6404", jobRepository)
                .tasklet(new com.fordcredit.misc1099.batch.tasklet.IebgenerTasklet("CCAC6404"), transactionManager)
                .build();
    }

}
