package com.fordcredit.misc1099.batch.config;

import org.springframework.context.annotation.Configuration;

/**
 * Layer 3E wiring placeholder.
 *
 * This class exists so reader/writer beans can be added incrementally without touching job/step config.
 * In Layer 3E.1 we will add real FlatFileItemReader/Writer beans per program and per DD mapping.
 */
@Configuration
public class BatchIoConfig {
    // TODO: Add ItemReader/ItemWriter beans (Layer 3E.1)
}
