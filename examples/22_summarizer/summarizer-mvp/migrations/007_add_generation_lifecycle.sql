-- Additive rollout: legacy completed rows remain publishable; unfinished rows queue.
ALTER TABLE summaries ADD COLUMN generation_status TEXT NOT NULL DEFAULT 'queued';
ALTER TABLE summaries ADD COLUMN generation_attempt INTEGER NOT NULL DEFAULT 0;
ALTER TABLE summaries ADD COLUMN generation_epoch INTEGER NOT NULL DEFAULT 0;
ALTER TABLE summaries ADD COLUMN generation_started_at TEXT NOT NULL DEFAULT '';
ALTER TABLE summaries ADD COLUMN generation_updated_at TEXT NOT NULL DEFAULT '';
ALTER TABLE summaries ADD COLUMN next_retry_at TEXT NOT NULL DEFAULT '';
ALTER TABLE summaries ADD COLUMN generation_error_code TEXT NOT NULL DEFAULT '';
ALTER TABLE summaries ADD COLUMN generation_error_message TEXT NOT NULL DEFAULT '';
ALTER TABLE summaries ADD COLUMN provider_interaction_id TEXT NOT NULL DEFAULT '';
UPDATE summaries SET generation_status = CASE WHEN summary_done = 1 THEN 'succeeded' ELSE 'queued' END;
CREATE INDEX IF NOT EXISTS idx_summaries_generation_recovery ON summaries (generation_status, next_retry_at, generation_updated_at);
