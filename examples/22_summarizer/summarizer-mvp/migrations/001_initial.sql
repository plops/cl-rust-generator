PRAGMA journal_mode=WAL;

CREATE TABLE IF NOT EXISTS summaries (
    identifier INTEGER PRIMARY KEY AUTOINCREMENT,
    model TEXT NOT NULL DEFAULT '',
    transcript TEXT NOT NULL DEFAULT '',
    host TEXT NOT NULL DEFAULT '',
    original_source_link TEXT NOT NULL DEFAULT '',
    include_comments BOOLEAN NOT NULL DEFAULT 0,
    include_timestamps BOOLEAN NOT NULL DEFAULT 0,
    include_glossary BOOLEAN NOT NULL DEFAULT 0,
    output_language TEXT NOT NULL DEFAULT '',
    summary TEXT NOT NULL DEFAULT '',
    summary_done BOOLEAN NOT NULL DEFAULT 0,
    summary_input_tokens INTEGER NOT NULL DEFAULT 0,
    summary_output_tokens INTEGER NOT NULL DEFAULT 0,
    summary_timestamp_start TEXT NOT NULL DEFAULT '',
    summary_timestamp_end TEXT NOT NULL DEFAULT '',
    timestamps TEXT NOT NULL DEFAULT '',
    timestamps_done BOOLEAN NOT NULL DEFAULT 0,
    timestamps_input_tokens INTEGER NOT NULL DEFAULT 0,
    timestamps_output_tokens INTEGER NOT NULL DEFAULT 0,
    timestamps_timestamp_start TEXT NOT NULL DEFAULT '',
    timestamps_timestamp_end TEXT NOT NULL DEFAULT '',
    timestamped_summary_in_youtube_format TEXT NOT NULL DEFAULT '',
    cost REAL NOT NULL DEFAULT 0.0,
    embedding BLOB,
    embedding_model TEXT NOT NULL DEFAULT '',
    full_embedding BLOB
);

CREATE INDEX IF NOT EXISTS idx_dedup_lookup
    ON summaries (original_source_link, model, summary_timestamp_start);
