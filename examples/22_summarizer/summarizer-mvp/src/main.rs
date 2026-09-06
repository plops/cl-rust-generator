mod ai;
mod db;
mod models;
mod routes;
use sqlx::SqlitePool;
#[derive(Debug, Clone)]
pub struct AppState {
    pub db: SqlitePool,
    pub api_key: String,
    pub ai_base_url: String,
}
#[tokio::main]
async fn main() {
    {
        let api_key = std::env::var("GEMINI_API_KEY").unwrap_or_else(|_| String::new());
        let ai_base_url = std::env::var("GEMINI_BASE_URL")
            .unwrap_or_else(|_| "https://generativelanguage.googleapis.com".to_string());
        let db = db::init_db("sqlite:data/summarizer-mvp.db")
            .await
            .expect("db init failed");
        let state = AppState {
            db: db.clone(),
            api_key,
            ai_base_url,
        };
        let app = routes::build_router(state);
        let listener = tokio::net::TcpListener::bind("127.0.0.1:5001")
            .await
            .expect("bind failed");
        axum::serve(listener, app.into_make_service())
            .await
            .unwrap();
    }
}
