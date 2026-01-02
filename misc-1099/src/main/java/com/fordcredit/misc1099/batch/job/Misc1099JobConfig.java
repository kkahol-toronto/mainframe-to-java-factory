package com.fordcredit.misc1099.batch.job;

import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Bean;
import org.springframework.batch.core.Job;
import org.springframework.batch.core.Step;
import org.springframework.batch.core.job.builder.JobBuilder;
import org.springframework.batch.core.step.builder.StepBuilder;
import org.springframework.batch.core.repository.JobRepository;
import org.springframework.transaction.PlatformTransactionManager;
import org.springframework.batch.repeat.RepeatStatus;

@Configuration
public class Misc1099JobConfig {

    @Bean
    public Job misc1099Job(JobRepository jobRepository, Step misc1099Step) {
        return new JobBuilder("misc1099Job", jobRepository)
                .start(misc1099Step)
                .build();
    }

    @Bean
    public Step misc1099Step(JobRepository jobRepository, PlatformTransactionManager transactionManager) {
        return new StepBuilder("misc1099Step", jobRepository)
                .tasklet((contribution, chunkContext) -> {
                    // TODO: Implement MISC 1099 processing logic here
                    return RepeatStatus.FINISHED;
                }, transactionManager)
                .build();
    }
}
