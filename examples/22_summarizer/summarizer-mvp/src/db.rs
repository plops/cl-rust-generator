use crate::models::{GenerationStatus, SummaryRow};
use sqlx::SqlitePool;
use sqlx::sqlite::{SqliteConnectOptions, SqliteJournalMode, SqlitePoolOptions};
use std::str::FromStr;
pub async fn init_db(database_url: &str) -> Result<SqlitePool, sqlx::Error> {
    if let Some(parent) = std::path::Path::new(database_url.trim_start_matches("sqlite:")).parent()
    {
        std::fs::create_dir_all(parent).map_err(sqlx::Error::Io)?;
    }
    {
        let options = SqliteConnectOptions::from_str(database_url)?
            .create_if_missing(true)
            .journal_mode(SqliteJournalMode::Wal);
        let pool = SqlitePoolOptions::new()
            .max_connections(5)
            .connect_with(options)
            .await?;
        sqlx::migrate!("./migrations").run(&pool).await?;
        Ok(pool)
    }
}
pub async fn insert_submit(
    db: &SqlitePool,
    model: &str,
    transcript: &str,
) -> Result<i64, sqlx::Error> {
    {
        let status = GenerationStatus::Queued.as_str();
        let result = sqlx::query("INSERT INTO summaries (model, transcript, original_source_link, generation_status) VALUES (?, ?, '', ?)").bind(model).bind(transcript).bind(status).execute(db).await?;
        Ok(result.last_insert_rowid())
    }
}
pub async fn fetch_row(
    db: &SqlitePool,
    identifier: i64,
) -> Result<Option<SummaryRow>, sqlx::Error> {
    (sqlx::query_as::<_, SummaryRow>("SELECT identifier, model, transcript, summary, summary_done, generation_status, generation_error_message FROM summaries WHERE identifier = ?")).bind(identifier).fetch_optional(db).await
}
pub async fn claim_running(
    db: &SqlitePool,
    identifier: i64,
    now: &str,
) -> Result<bool, sqlx::Error> {
    {
        let new_status = GenerationStatus::Running.as_str();
        let old_status = GenerationStatus::Queued.as_str();
        let result = sqlx::query("UPDATE summaries SET generation_status = ?, generation_started_at = ?, generation_updated_at = ? WHERE identifier = ? AND generation_status = ?").bind(new_status).bind(now).bind(now).bind(identifier).bind(old_status).execute(db).await?;
        Ok(0 != result.rows_affected())
    }
}
pub async fn finish_success(
    db: &SqlitePool,
    identifier: i64,
    summary: String,
    in_tokens: i64,
    out_tokens: i64,
    now: &str,
) -> Result<(), sqlx::Error> {
    {
        let status = GenerationStatus::Succeeded.as_str();
        let _result = sqlx::query("UPDATE summaries SET summary = ?, summary_done = 1, generation_status = ?, summary_input_tokens = ?, summary_output_tokens = ?, summary_timestamp_end = ? WHERE identifier = ?").bind(summary).bind(status).bind(in_tokens).bind(out_tokens).bind(now).bind(identifier).execute(db).await?;
    }
    Ok(())
}
pub async fn finish_failed(
    db: &SqlitePool,
    identifier: i64,
    message: String,
    now: &str,
) -> Result<(), sqlx::Error> {
    {
        let status = GenerationStatus::Failed.as_str();
        let _result = sqlx::query("UPDATE summaries SET generation_status = ?, generation_error_code = 'mvp_error', generation_error_message = ?, generation_updated_at = ? WHERE identifier = ?").bind(status).bind(message).bind(now).bind(identifier).execute(db).await?;
    }
    Ok(())
}
pub async fn store_embedding(
    db: &SqlitePool,
    identifier: i64,
    bytes: Vec<u8>,
) -> Result<(), sqlx::Error> {
    sqlx::query("UPDATE summaries SET embedding = ?, embedding_model = 'gemini-embedding-001' WHERE identifier = ?").bind(bytes).bind(identifier).execute(db).await?;
    Ok(())
}
pub async fn fetch_all_embeddings(db: &SqlitePool) -> Result<Vec<(i64, Vec<u8>)>, sqlx::Error> {
    {
        let rows = (sqlx::query_as::<_, (i64, Vec<u8>)>(
            "SELECT identifier, embedding FROM summaries WHERE embedding IS NOT NULL",
        ))
        .fetch_all(db)
        .await?;
        Ok(rows)
    }
}
#[cfg(test)]
mod tests {
    use super::*;
    #[tokio::test]
    async fn lifecycle_success() -> Result<(), sqlx::Error> {
        {
            let pool = init_db("sqlite::memory:").await?;
            let id = insert_submit(&pool, "m", "t").await?;
            {
                let queued = fetch_row(&pool, id).await?.expect("row present");
                assert!(queued.generation_status == "queued");
            }
            {
                let claimed = claim_running(&pool, id, "t0").await?;
                assert!(claimed);
                {
                    let again = claim_running(&pool, id, "t1").await?;
                    assert!(!again);
                }
            }
            finish_success(&pool, id, "the summary".to_string(), 10, 20, "t2").await?;
            {
                let done = fetch_row(&pool, id).await?.expect("row present");
                assert!(done.generation_status == "succeeded");
                assert!(done.summary == "the summary");
                assert!(done.summary_done);
            }
            Ok(())
        }
    }
    #[tokio::test]
    async fn lifecycle_failed() -> Result<(), sqlx::Error> {
        {
            let pool = init_db("sqlite::memory:").await?;
            let id = insert_submit(&pool, "m", "t").await?;
            claim_running(&pool, id, "t0").await?;
            finish_failed(&pool, id, "boom".to_string(), "t1").await?;
            {
                let row = fetch_row(&pool, id).await?.expect("row present");
                assert!(row.generation_status == "failed");
                assert!(row.generation_error_message == "boom");
            }
            Ok(())
        }
    }
    #[tokio::test]
    async fn embedding_roundtrip() -> Result<(), sqlx::Error> {
        {
            let pool = init_db("sqlite::memory:").await?;
            let id = insert_submit(&pool, "m", "t").await?;
            let bytes = vec![0, 1, 2, 3];
            store_embedding(&pool, id, bytes.clone()).await?;
            {
                let rows = fetch_all_embeddings(&pool).await?;
                assert!(rows.len() == 1);
                assert!(rows[0].0 == id);
                assert!(rows[0].1 == bytes);
            }
            Ok(())
        }
    }
}
