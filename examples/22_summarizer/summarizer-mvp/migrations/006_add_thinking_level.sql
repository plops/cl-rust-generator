-- Persist per-summary Gemini 3 thinking effort. Existing rows retain the
-- historical behavior, which always requested high thinking effort.
ALTER TABLE summaries ADD COLUMN thinking_level TEXT NOT NULL DEFAULT 'high'
    CHECK (thinking_level IN ('auto', 'minimal', 'low', 'medium', 'high'));
