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
 * Spring Batch Job configuration for JCL: CCAC@626
 * PR,DEST=LAS02.FMCCRJE,FORMS=PXCC1,DDNAME=
 */
@Configuration
public class CCAC626JobConfig {

    private final JobRepository jobRepository;
    private final PlatformTransactionManager transactionManager;

    public CCAC626JobConfig(JobRepository jobRepository, 
                       PlatformTransactionManager transactionManager) {
        this.jobRepository = jobRepository;
        this.transactionManager = transactionManager;
    }

    @Bean
    public Job ccac626Job() {
        return new JobBuilder("CCAC@626", jobRepository)
                .start(step0())
                .build();
    }


    @Bean
    public Step step0() {
        return new StepBuilder("CCAC6261", jobRepository)
                .tasklet(new com.fordcredit.misc1099.batch.tasklet.IebgenerTasklet("CCAC6261"), transactionManager)
                .build();
    }


    @Bean
    public Step step1() {
        return new StepBuilder("CCAC6262", jobRepository)
                .tasklet(new com.fordcredit.misc1099.batch.tasklet.IebgenerTasklet("CCAC6262"), transactionManager)
                .build();
    }


    @Bean
    public Step step2() {
        return new StepBuilder("CCAC6263", jobRepository)
                .tasklet(new com.fordcredit.misc1099.batch.tasklet.SortTasklet("CCAC6263"), transactionManager)
                .build();
    }


    @Bean
    public Step step3() {
        return new StepBuilder("CCAC6264", jobRepository)
                .tasklet(new com.fordcredit.misc1099.batch.tasklet.SortTasklet("CCAC6264"), transactionManager)
                .build();
    }


    @Bean
    public Step step4() {
        return new StepBuilder("CCAC6265", jobRepository)
                .tasklet(new com.fordcredit.misc1099.batch.tasklet.SortTasklet("CCAC6265"), transactionManager)
                .build();
    }


    @Bean
    public Step step5() {
        return new StepBuilder("CCAC6266", jobRepository)
                .tasklet(new com.fordcredit.misc1099.batch.program.CCAC6250Tasklet(), transactionManager)
                .build();
    }


    @Bean
    public Step step6() {
        return new StepBuilder("CCAC6267", jobRepository)
                .tasklet(new com.fordcredit.misc1099.batch.tasklet.IebgenerTasklet("CCAC6267"), transactionManager)
                .build();
    }


    @Bean
    public Step step7() {
        return new StepBuilder("CCAC9999", jobRepository)
                .tasklet(new com.fordcredit.misc1099.batch.tasklet.IebgenerTasklet("CCAC9999"), transactionManager)
                .build();
    }

}
