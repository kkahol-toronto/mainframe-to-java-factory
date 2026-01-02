package com.fordcredit.misc1099.batch.tasklet;

import org.springframework.batch.core.ExitStatus;
import org.springframework.batch.core.StepContribution;
import org.springframework.batch.core.scope.context.ChunkContext;
import org.springframework.batch.core.step.tasklet.Tasklet;
import org.springframework.batch.repeat.RepeatStatus;

public class ConditionalOnlyTasklet implements Tasklet {

    private final String description;

    public ConditionalOnlyTasklet(String description) {
        this.description = description;
    }

    @Override
    public RepeatStatus execute(
            StepContribution contribution,
            ChunkContext chunkContext
    ) {
        // TODO: Implement failure-only logic
        // JCL equivalent: COND=ONLY
        contribution.setExitStatus(ExitStatus.FAILED);
        return RepeatStatus.FINISHED;
    }
}
