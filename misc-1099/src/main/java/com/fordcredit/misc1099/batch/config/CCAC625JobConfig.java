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
 * Spring Batch Job configuration for JCL: CCAC@625
 * //*=====================================================================00110000
 */
@Configuration
public class CCAC625JobConfig {

    private final JobRepository jobRepository;
    private final PlatformTransactionManager transactionManager;

    public CCAC625JobConfig(JobRepository jobRepository, 
                       PlatformTransactionManager transactionManager) {
        this.jobRepository = jobRepository;
        this.transactionManager = transactionManager;
    }

    @Bean
    public Job ccac625Job() {
        return new JobBuilder("CCAC@625", jobRepository)
                .start(step0())
                .build();
    }


    @Bean
    public Step step0() {
        return new StepBuilder("CCAC6251", jobRepository)
                .tasklet(new com.fordcredit.misc1099.batch.tasklet.SortTasklet("CCAC6251"), transactionManager)
                .build();
    }


    @Bean
    public Step step1() {
        return new StepBuilder("CCAC6252", jobRepository)
                .tasklet(new com.fordcredit.misc1099.batch.tasklet.SortTasklet("CCAC6252"), transactionManager)
                .build();
    }


    @Bean
    public Step step2() {
        return new StepBuilder("CCAC6253", jobRepository)
                .tasklet(new com.fordcredit.misc1099.batch.program.CCAC6250Tasklet(), transactionManager)
                .build();
    }


    @Bean
    public Step step5() {
        return new StepBuilder("CCAC9999", jobRepository)
                .tasklet(new com.fordcredit.misc1099.batch.tasklet.IebgenerTasklet("CCAC9999"), transactionManager)
                .build();
    }

}
