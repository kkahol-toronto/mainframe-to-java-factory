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
 * Spring Batch Job configuration for JCL: CCAC@641
 * - SFPT SPUT ANNUAL FILE CC.AC.T01AC640
 */
@Configuration
public class CCAC641JobConfig {

    private final JobRepository jobRepository;
    private final PlatformTransactionManager transactionManager;

    public CCAC641JobConfig(JobRepository jobRepository, 
                       PlatformTransactionManager transactionManager) {
        this.jobRepository = jobRepository;
        this.transactionManager = transactionManager;
    }

    @Bean
    public Job ccac641Job() {
        return new JobBuilder("CCAC@641", jobRepository)
                .start(step2())
                .next(step3())
                .next(step4())
                .build();
    }


    @Bean
    public Step step2() {
        return new StepBuilder("CCAC6413", jobRepository)
                .tasklet(new com.fordcredit.misc1099.batch.tasklet.IebgenerTasklet("CCAC6413"), transactionManager)
                .build();
    }


    @Bean
    public Step step3() {
        return new StepBuilder("CCAC6414", jobRepository)
                .tasklet(new com.fordcredit.misc1099.batch.tasklet.IebgenerTasklet("CCAC6414"), transactionManager)
                .build();
    }


    @Bean
    public Step step4() {
        return new StepBuilder("CCAC6415", jobRepository)
                .tasklet(new com.fordcredit.misc1099.batch.tasklet.IebgenerTasklet("CCAC6415"), transactionManager)
                .build();
    }

}
