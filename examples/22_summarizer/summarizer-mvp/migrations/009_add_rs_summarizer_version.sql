-- Historical rows predate generator provenance. Keep that fact explicit.
ALTER TABLE summaries
    ADD COLUMN rs_summarizer_version TEXT NOT NULL DEFAULT '';
