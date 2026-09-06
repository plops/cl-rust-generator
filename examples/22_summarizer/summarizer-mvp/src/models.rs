#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum GenerationStatus {
    Queued,
    Running,
    Succeeded,
    Failed,
}
impl GenerationStatus {
    pub fn as_str(&self) -> String {
        match *self {
            GenerationStatus::Queued => "queued".to_string(),
            GenerationStatus::Running => "running".to_string(),
            GenerationStatus::Succeeded => "succeeded".to_string(),
            GenerationStatus::Failed => "failed".to_string(),
        }
    }
}
#[derive(Debug, Clone, serde::Serialize, sqlx::FromRow)]
pub struct SummaryRow {
    pub identifier: i64,
    pub model: String,
    pub transcript: String,
    pub summary: String,
    pub summary_done: bool,
    pub generation_status: String,
    pub generation_error_message: String,
}
#[derive(Debug, serde::Deserialize)]
pub struct SubmitForm {
    pub model: String,
    pub transcript: String,
}
#[derive(Debug, serde::Deserialize)]
pub struct SearchForm {
    pub query: String,
}
