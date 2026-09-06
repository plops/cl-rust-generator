-- 007 had no reliable ownership/heartbeat information for legacy unfinished
-- rows. Do not replay an unbounded historical backlog on application startup.
-- Preserve any partial draft, stop polling, and let the user explicitly retry.
UPDATE summaries
SET generation_status = 'failed',
    summary_done = 1,
    generation_error_code = 'internal',
    generation_error_message = 'This legacy generation was not resumed automatically. Please retry it if still needed.',
    generation_updated_at = datetime('now')
WHERE generation_status = 'queued'
  AND generation_attempt = 0
  AND generation_started_at = ''
  AND generation_updated_at = '';
