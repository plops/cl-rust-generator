use crate::AppState;
use crate::models::{SearchForm, SubmitForm};
use crate::{ai, db};
use axum::Router;
use axum::extract::{Form, Path, State};
use axum::response::{Html, IntoResponse};
use axum::routing::{get, post};
use sqlx::SqlitePool;
const INDEX_HTML: &str = "<!doctype html><html><body><h1>summarizer-mvp</h1><form method='post' action='/submit'><input name='model' value='gemini-3.5-flash-lite' size='40'/><br/><textarea name='transcript' rows='20' cols='80'></textarea><br/><button type='submit'>summarize</button></form><h2>search</h2><form method='post' action='/search'><input name='query' size='60'/><button type='submit'>search</button></form></body></html>";
async fn index(State(_app): State<AppState>) -> impl IntoResponse {
    Html(INDEX_HTML)
}
async fn submit(State(app): State<AppState>, Form(input): Form<SubmitForm>) -> impl IntoResponse {
    {
        let transcript = input.transcript.trim();
        if transcript.is_empty() {
            return Html("<p>empty transcript</p>".to_string());
        }
        {
            let identifier = match db::insert_submit(&app.db, &input.model, transcript).await {
                Ok(id) => id,
                Err(e) => return Html(format!("<p>db error: {}", e)),
            };
            let db_clone = app.db.clone();
            let key_clone = app.api_key.clone();
            let base_clone = app.ai_base_url.clone();
            let model_clone = input.model.clone();
            let text_owned = transcript.to_string();
            tokio::spawn(async move {
                run_generation(
                    db_clone,
                    identifier,
                    key_clone,
                    base_clone,
                    model_clone,
                    text_owned,
                )
                .await
            });
            Html(format!(
                "<p>queued #{}</p><p><a href='/status/{}'>poll status</a></p>",
                identifier, identifier
            ))
        }
    }
}
async fn status(State(app): State<AppState>, Path(identifier): Path<i64>) -> impl IntoResponse {
    match db::fetch_row(&app.db, identifier).await {
        Ok(Some(row)) => Html(format!(
            "<h1>summary #{}</h1><p>status: {}</p><pre>{}</pre>",
            row.identifier, row.generation_status, row.summary
        )),
        Ok(None) => Html("<p>not found</p>".to_string()),
        Err(e) => Html(format!("<p>db error: {}</p>", e)),
    }
}
async fn search(State(app): State<AppState>, Form(input): Form<SearchForm>) -> impl IntoResponse {
    {
        let client = reqwest::Client::new();
        let query_vec =
            match ai::embed_text(&client, &app.ai_base_url, &app.api_key, &input.query).await {
                Ok(v) => v,
                Err(e) => return Html(format!("<p>embed error: {}", e)),
            };
        let rows = match db::fetch_all_embeddings(&app.db).await {
            Ok(r) => r,
            Err(e) => return Html(format!("<p>db error: {}", e)),
        };
        let mut scored: Vec<(i64, f32)> = vec![];
        for (id, bytes) in rows {
            {
                let vec = ai::bytes_to_embedding(&bytes);
                scored.push((id, ai::cosine_similarity(&query_vec, &vec)));
            }
        }
        scored.sort_by(|a, b| b.1.partial_cmp(&a.1).unwrap_or(std::cmp::Ordering::Equal));
        scored.truncate(5);
        {
            let items: Vec<String> = scored
                .iter()
                .map(|(id, score)| format!("<li>summary #{}: {:.3}</li>", id, score))
                .collect();
            Html(format!(
                "<h1>similar summaries</h1><ul>{}</ul>",
                items.join(" ")
            ))
        }
    }
}
async fn run_generation(
    db: SqlitePool,
    identifier: i64,
    api_key: String,
    base_url: String,
    model: String,
    transcript: String,
) {
    {
        let now = chrono::Utc::now().to_rfc3339();
        if let Ok(true) = db::claim_running(&db, identifier, &now).await {
            {
                let client = reqwest::Client::new();
                match ai::generate_summary(&client, &base_url, &api_key, &model, &transcript).await
                {
                    Err(message) => {
                        drop(db::finish_failed(&db, identifier, message, &now).await);
                    }
                    Ok(generation) => {
                        let end = chrono::Utc::now().to_rfc3339();
                        drop(
                            db::finish_success(
                                &db,
                                identifier,
                                generation.summary.clone(),
                                generation.in_tokens,
                                generation.out_tokens,
                                &end,
                            )
                            .await,
                        );
                        if let Ok(vec) =
                            ai::embed_text(&client, &base_url, &api_key, &generation.summary).await
                        {
                            drop(
                                db::store_embedding(&db, identifier, ai::embedding_to_bytes(&vec))
                                    .await,
                            );
                        }
                    }
                }
            }
        }
    }
}
pub fn build_router(state: AppState) -> Router {
    Router::new()
        .route("/", get(index))
        .route("/submit", post(submit))
        .route("/status/{identifier}", get(status))
        .route("/search", post(search))
        .with_state(state)
}
#[cfg(test)]
mod tests {
    const MOCK_SUMMARY_JSON: &str = "{\"candidates\":[{\"content\":{\"parts\":[{\"text\":\"mock summary\"}]}}],\"usageMetadata\":{\"promptTokenCount\":7,\"candidatesTokenCount\":13}}";
    const MOCK_EMBED_JSON: &str = "{\"embedding\":{\"values\":[0.5,0.25]}}";
    async fn mock_generate() -> String {
        MOCK_SUMMARY_JSON.to_string()
    }
    async fn mock_embed() -> String {
        MOCK_EMBED_JSON.to_string()
    }
    #[tokio::test]
    async fn background_generation_against_mock() -> Result<(), sqlx::Error> {
        {
            let mock = axum::Router::new()
                .route(
                    "/v1beta/models/m:generateContent",
                    axum::routing::post(mock_generate),
                )
                .route(
                    "/v1beta/models/gemini-embedding-001:embedContent",
                    axum::routing::post(mock_embed),
                );
            let listener = tokio::net::TcpListener::bind("127.0.0.1:0").await.unwrap();
            let base = format!("http://{}", listener.local_addr().unwrap());
            let pool = crate::db::init_db("sqlite::memory:").await?;
            let id = crate::db::insert_submit(&pool, "m", "t").await?;
            tokio::spawn(async move {
                axum::serve(listener, mock.into_make_service())
                    .await
                    .unwrap();
            });
            super::run_generation(
                pool.clone(),
                id,
                "k".to_string(),
                base,
                "m".to_string(),
                "t".to_string(),
            )
            .await;
            {
                let row = crate::db::fetch_row(&pool, id).await?.expect("row present");
                assert!(row.generation_status == "succeeded");
                assert!(row.summary == "mock summary");
            }
            {
                let rows = crate::db::fetch_all_embeddings(&pool).await?;
                assert!(rows.len() == 1);
            }
            Ok(())
        }
    }
}
