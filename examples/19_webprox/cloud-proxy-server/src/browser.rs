use chromiumoxide::browser::{Browser, BrowserConfig};
use chromiumoxide::cdp::browser_protocol::fetch::{
    ContinueRequestParams, EventRequestPaused, FailRequestParams, RequestPattern,
};
use chromiumoxide::cdp::browser_protocol::network::ErrorReason;
use chromiumoxide::Page;
use futures_util::StreamExt;
use std::sync::Arc;

/// File extensions to block for bandwidth savings
const BLOCKED_EXTENSIONS: &[&str] = &[
    ".png", ".jpg", ".jpeg", ".gif", ".svg", ".webp", ".ico", ".bmp",
    ".mp4", ".webm", ".ogg", ".mp3", ".wav", ".flac",
    ".css", ".woff", ".woff2", ".ttf", ".eot", ".otf",
];

pub struct BrowserPool {
    browser: Arc<Browser>,
}

impl BrowserPool {
    pub async fn new() -> Result<Self, Box<dyn std::error::Error + Send + Sync>> {
        let config = BrowserConfig::builder()
            .no_sandbox()
            .arg("--headless=new")
            .arg("--disable-gpu")
            .arg("--disable-images")
            .arg("--blink-settings=imagesEnabled=false")
            .arg("--disable-remote-fonts")
            .enable_request_intercept()
            .build()
            .map_err(|e| format!("Browser config error: {}", e))?;

        let (browser, mut handler) = Browser::launch(config).await?;

        // Spawn the CDP handler loop
        tokio::spawn(async move {
            while let Some(_event) = handler.next().await {}
        });

        Ok(Self {
            browser: Arc::new(browser),
        })
    }

    /// Create a new page (tab) for a session, with network interception
    pub async fn new_page(&self) -> Result<Page, Box<dyn std::error::Error + Send + Sync>> {
        let page = self.browser.new_page("about:blank").await?;

        // Spawn a task to intercept and block heavy resources on this page
        Self::spawn_request_interceptor(&page).await?;

        Ok(page)
    }

    /// Listen for fetch events and block images/media/css/fonts
    async fn spawn_request_interceptor(
        page: &Page,
    ) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
        let mut events = page.event_listener::<EventRequestPaused>().await?;
        let page_clone = page.clone();

        tokio::spawn(async move {
            while let Some(event) = events.next().await {
                let url = event.request.url.to_lowercase();
                let should_block = BLOCKED_EXTENSIONS
                    .iter()
                    .any(|ext| url.contains(ext));

                if should_block {
                    // Abort the request
                    let fail = FailRequestParams::new(
                        event.request_id.clone(),
                        ErrorReason::BlockedByClient,
                    );
                    let _ = page_clone.execute(fail).await;
                } else {
                    // Continue the request
                    let cont = ContinueRequestParams::new(event.request_id.clone());
                    let _ = page_clone.execute(cont).await;
                }
            }
        });

        Ok(())
    }

    /// Navigate a page to a URL and wait for load
    pub async fn navigate(
        page: &Page,
        url: &str,
    ) -> Result<String, Box<dyn std::error::Error + Send + Sync>> {
        page.goto(url).await?.wait_for_navigation().await?;

        // Extract the full HTML content
        let html = page.content().await?;

        Ok(html)
    }
}
