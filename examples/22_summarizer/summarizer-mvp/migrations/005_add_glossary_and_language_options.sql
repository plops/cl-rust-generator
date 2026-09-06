CREATE INDEX IF NOT EXISTS idx_summaries_lang_glossary
    ON summaries (output_language, include_glossary);
