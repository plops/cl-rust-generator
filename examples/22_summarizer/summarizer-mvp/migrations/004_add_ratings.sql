-- Migration: Add summary_ratings table for dual 1-5 star ratings per summary and client IP
CREATE TABLE IF NOT EXISTS summary_ratings (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    summary_id INTEGER NOT NULL,
    client_ip TEXT NOT NULL,
    summary_rating INTEGER CHECK(summary_rating IS NULL OR (summary_rating >= 1 AND summary_rating <= 5)),
    content_rating INTEGER CHECK(content_rating IS NULL OR (content_rating >= 1 AND content_rating <= 5)),
    created_at TEXT NOT NULL DEFAULT (datetime('now')),
    updated_at TEXT NOT NULL DEFAULT (datetime('now')),
    FOREIGN KEY(summary_id) REFERENCES summaries(identifier) ON DELETE CASCADE
);

CREATE UNIQUE INDEX IF NOT EXISTS idx_summary_ratings_summary_ip
    ON summary_ratings(summary_id, client_ip);
