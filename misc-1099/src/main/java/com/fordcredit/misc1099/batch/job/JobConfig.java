package com.fordcredit.misc1099.batch.job;

import org.springframework.batch.core.Job;
import org.springframework.batch.core.Step;
import org.springframework.batch.core.job.builder.JobBuilder;
import org.springframework.batch.core.repository.JobRepository;
import org.springframework.batch.core.step.builder.StepBuilder;
import org.springframework.batch.repeat.RepeatStatus;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.transaction.PlatformTransactionManager;

@Configuration
public class JobConfig {

    @Bean
    public Job ccac626Job(JobRepository jobRepository, PlatformTransactionManager transactionManager) {
        return new JobBuilder("ccac626Job", jobRepository)
                .start(stepCCAC6261(jobRepository, transactionManager))
                .next(stepCCAC6262(jobRepository, transactionManager))
                .next(stepCCAC6263(jobRepository, transactionManager))
                .next(stepCCAC6264(jobRepository, transactionManager))
                .next(stepCCAC6265(jobRepository, transactionManager))
                .next(stepCCAC6266(jobRepository, transactionManager))
                .next(stepCCAC6267(jobRepository, transactionManager))
                .build();
    }

    @Bean
    public Step stepCCAC6261(JobRepository jobRepository, PlatformTransactionManager transactionManager) {
        return new StepBuilder("stepCCAC6261", jobRepository)
                .tasklet((contribution, chunkContext) -> {
                    // GENER COPY OF REJECT FILE
                    return RepeatStatus.FINISHED;
                }, transactionManager)
                .build();
    }

    @Bean
    public Step stepCCAC6262(JobRepository jobRepository, PlatformTransactionManager transactionManager) {
        return new StepBuilder("stepCCAC6262", jobRepository)
                .tasklet((contribution, chunkContext) -> {
                    // GENER COPY OF FORM 20071-MANUAL ISSUES FILE
                    return RepeatStatus.FINISHED;
                }, transactionManager)
                .build();
    }

    @Bean
    public Step stepCCAC6263(JobRepository jobRepository, PlatformTransactionManager transactionManager) {
        return new StepBuilder("stepCCAC6263", jobRepository)
                .tasklet((contribution, chunkContext) -> {
                    // COPY AND REFORMAT USER ALTERED/APPROVED 1099 FILE (B in column 205)
                    return RepeatStatus.FINISHED;
                }, transactionManager)
                .build();
    }

    @Bean
    public Step stepCCAC6264(JobRepository jobRepository, PlatformTransactionManager transactionManager) {
        return new StepBuilder("stepCCAC6264", jobRepository)
                .tasklet((contribution, chunkContext) -> {
                    // SORT CORPORATE MAINTENANCE FILE WITH MASTER CORPORATE FILE, ELIMINATE DUPLICATES
                    return RepeatStatus.FINISHED;
                }, transactionManager)
                .build();
    }

    @Bean
    public Step stepCCAC6265(JobRepository jobRepository, PlatformTransactionManager transactionManager) {
        return new StepBuilder("stepCCAC6265", jobRepository)
                .tasklet((contribution, chunkContext) -> {
                    // EXTRACT (B)BUSINESS, (C)CHANGE, (D)DELETE, (M)MECHANICAL MASTER RECS
                    return RepeatStatus.FINISHED;
                }, transactionManager)
                .build();
    }

    @Bean
    public Step stepCCAC6266(JobRepository jobRepository, PlatformTransactionManager transactionManager) {
        return new StepBuilder("stepCCAC6266", jobRepository)
                .tasklet((contribution, chunkContext) -> {
                    // ADD HEADER/TRAILER TO MASTER UPDATES FILE
                    return RepeatStatus.FINISHED;
                }, transactionManager)
                .build();
    }

    @Bean
    public Step stepCCAC6267(JobRepository jobRepository, PlatformTransactionManager transactionManager) {
        return new StepBuilder("stepCCAC6267", jobRepository)
                .tasklet((contribution, chunkContext) -> {
                    // GENERATE EMPTY DATASET TO MANUAL ISSUES FILE
                    return RepeatStatus.FINISHED;
                }, transactionManager)
                .build();
    }
}
