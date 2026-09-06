-- Add index for fast duplicate checks by model and timestamp
CREATE INDEX IF NOT EXISTS idx_summaries_model_time
    ON summaries (model, summary_timestamp_start);
