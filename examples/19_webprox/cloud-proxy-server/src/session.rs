use chromiumoxide::Page;
use std::collections::HashMap;
use std::sync::Arc;
use std::time::{Duration, Instant};
use tokio::sync::Mutex;

use crate::extractor::LinkMap;

const IDLE_TIMEOUT: Duration = Duration::from_secs(300); // 5 minutes
const REAP_INTERVAL: Duration = Duration::from_secs(60);

pub type SessionId = String;

pub struct Session {
    pub page: Page,
    pub last_active: Instant,
    pub current_url: Option<String>,
    pub link_map: LinkMap,
}

impl Session {
    pub fn new(page: Page) -> Self {
        Self {
            page,
            last_active: Instant::now(),
            current_url: None,
            link_map: LinkMap::new(),
        }
    }

    pub fn touch(&mut self) {
        self.last_active = Instant::now();
    }

    pub fn is_idle(&self) -> bool {
        self.last_active.elapsed() > IDLE_TIMEOUT
    }
}

#[derive(Clone)]
pub struct SessionManager {
    sessions: Arc<Mutex<HashMap<SessionId, Session>>>,
}

impl SessionManager {
    pub fn new() -> Self {
        Self {
            sessions: Arc::new(Mutex::new(HashMap::new())),
        }
    }

    pub async fn create_session(&self, id: SessionId, page: Page) {
        let mut sessions = self.sessions.lock().await;
        sessions.insert(id, Session::new(page));
    }

    /// Access the session's page for async operations
    pub async fn get_page(&self, id: &str) -> Option<Page> {
        let mut sessions = self.sessions.lock().await;
        sessions.get_mut(id).map(|session| {
            session.touch();
            session.page.clone()
        })
    }

    pub async fn set_current_url(&self, id: &str, url: String) {
        let mut sessions = self.sessions.lock().await;
        if let Some(session) = sessions.get_mut(id) {
            session.current_url = Some(url);
        }
    }

    /// Store the link_map for a session (used for click resolution)
    pub async fn set_link_map(&self, id: &str, link_map: LinkMap) {
        let mut sessions = self.sessions.lock().await;
        if let Some(session) = sessions.get_mut(id) {
            session.link_map = link_map;
        }
    }

    /// Resolve a link_id to a URL for a given session
    pub async fn resolve_link(&self, id: &str, link_id: u32) -> Option<String> {
        let sessions = self.sessions.lock().await;
        sessions
            .get(id)
            .and_then(|s| s.link_map.get(&link_id).cloned())
    }

    pub async fn remove_session(&self, id: &str) {
        let mut sessions = self.sessions.lock().await;
        sessions.remove(id);
    }

    /// Background loop that reaps idle sessions every 60 seconds
    pub async fn reap_idle_sessions_loop(&self) {
        let mut interval = tokio::time::interval(REAP_INTERVAL);
        loop {
            interval.tick().await;
            let mut sessions = self.sessions.lock().await;
            let idle_ids: Vec<SessionId> = sessions
                .iter()
                .filter(|(_, s)| s.is_idle())
                .map(|(id, _)| id.clone())
                .collect();

            for id in &idle_ids {
                tracing::info!("Reaping idle session: {}", id);
                sessions.remove(id);
            }
        }
    }
}
