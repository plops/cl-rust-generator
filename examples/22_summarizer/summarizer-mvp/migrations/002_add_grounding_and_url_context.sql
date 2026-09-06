-- Migration: Add grounding, url context, thinking, and thinking tokens columns
ALTER TABLE summaries ADD COLUMN google_search_grounding BOOLEAN NOT NULL DEFAULT 0;
ALTER TABLE summaries ADD COLUMN url_context BOOLEAN NOT NULL DEFAULT 0;
ALTER TABLE summaries ADD COLUMN thinking TEXT NOT NULL DEFAULT '';
ALTER TABLE summaries ADD COLUMN thinking_tokens INTEGER NOT NULL DEFAULT 0;
