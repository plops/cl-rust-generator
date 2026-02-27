use chromiumoxide::browser::{Browser, BrowserConfig};
use chromiumoxide::cdp::browser_protocol::dom::EventDocumentUpdated;
use chromiumoxide::cdp::browser_protocol::fetch::{
    ContinueRequestParams, EventRequestPaused, FailRequestParams,
};
use chromiumoxide::cdp::browser_protocol::network::ErrorReason;
use chromiumoxide::Page;
use futures_util::StreamExt;
use std::sync::Arc;
use tokio::sync::mpsc;
use crate::service::ResourceFlags;

/// File extensions to block for bandwidth savings
const IMAGE_EXTENSIONS: &[&str] = &[".png", ".jpg", ".jpeg", ".gif", ".svg", ".webp", ".ico", ".bmp"];
const MEDIA_EXTENSIONS: &[&str] = &[".mp4", ".webm", ".ogg", ".mp3", ".wav", ".flac"];
const CSS_EXTENSIONS: &[&str] = &[".css"];
const FONT_EXTENSIONS: &[&str] = &[".woff", ".woff2", ".ttf", ".eot", ".otf"];

pub struct BrowserPool {
    browser: Arc<Browser>,
    flags: ResourceFlags,
}

impl BrowserPool {
    pub async fn new(
        headless: bool,
        flags: ResourceFlags,
    ) -> Result<Self, Box<dyn std::error::Error + Send + Sync>> {
        tracing::info!("Initializing browser with headless={}, flags={:?}", headless, flags);
        
        let mut builder = BrowserConfig::builder()
            .no_sandbox()
            .user_data_dir(format!("/tmp/chromiumoxide-{}", fastrand::u64(..)))
            .enable_request_intercept();

        if !flags.load_images && !flags.load_all {
            builder = builder
                .arg("--disable-images")
                .arg("--blink-settings=imagesEnabled=false");
        }

        if !flags.load_fonts && !flags.load_all {
            builder = builder.arg("--disable-remote-fonts");
        }

        // Configure headless mode
        if headless {
            tracing::info!("Adding --headless=new flag");
            builder = builder.arg("--headless=new");
        } else {
            // Use with_head() to run in visible mode
            tracing::info!("Using with_head() for visible mode");
            builder = builder.with_head();
        }

        let config = builder
            .build()
            .map_err(|e| format!("Browser config error: {}", e))?;

        let (browser, mut handler) = Browser::launch(config).await?;

        // Spawn the CDP handler loop
        tokio::spawn(async move {
            while let Some(_event) = handler.next().await {}
        });

        Ok(Self {
            browser: Arc::new(browser),
            flags,
        })
    }

    /// Create a new page (tab) for a session, with network interception
    #[allow(dead_code)]
    pub async fn new_page(&self) -> Result<Page, Box<dyn std::error::Error + Send + Sync>> {
        let page = self.browser.new_page("about:blank").await?;

        // Spawn a task to intercept and block heavy resources on this page
        Self::spawn_request_interceptor(&page, self.flags.clone()).await?;

        Ok(page)
    }

    /// Create a new page with a DOM mutation notification channel
    pub async fn new_page_with_dom_watcher(
        &self,
        dom_tx: mpsc::Sender<()>,
    ) -> Result<Page, Box<dyn std::error::Error + Send + Sync>> {
        let page = self.browser.new_page("about:blank").await?;

        Self::spawn_request_interceptor(&page, self.flags.clone()).await?;
        Self::spawn_dom_watcher(&page, dom_tx).await?;

        Ok(page)
    }

    /// Listen for DOM.documentUpdated events and notify via channel
    async fn spawn_dom_watcher(
        page: &Page,
        dom_tx: mpsc::Sender<()>,
    ) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
        let mut events = page.event_listener::<EventDocumentUpdated>().await?;

        tokio::spawn(async move {
            while let Some(_event) = events.next().await {
                // Notify that the DOM changed — debouncer will coalesce
                let _ = dom_tx.try_send(());
            }
        });

        Ok(())
    }

    /// Listen for fetch events and block images/media/css/fonts
    async fn spawn_request_interceptor(
        page: &Page,
        flags: ResourceFlags,
    ) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
        let mut events = page.event_listener::<EventRequestPaused>().await?;
        let page_clone = page.clone();

        tokio::spawn(async move {
            while let Some(event) = events.next().await {
                let url = event.request.url.to_lowercase();
                
                let mut should_block = false;

                if !flags.load_all {
                    if !flags.load_images && IMAGE_EXTENSIONS.iter().any(|ext| url.contains(ext)) {
                        should_block = true;
                    } else if !flags.load_media && MEDIA_EXTENSIONS.iter().any(|ext| url.contains(ext)) {
                        should_block = true;
                    } else if !flags.load_css && CSS_EXTENSIONS.iter().any(|ext| url.contains(ext)) {
                        should_block = true;
                    } else if !flags.load_fonts && FONT_EXTENSIONS.iter().any(|ext| url.contains(ext)) {
                        should_block = true;
                    }
                }

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

    /// Navigate a page to a URL and extract content.
    /// Uses a timeout to avoid hanging when blocked resources prevent the load event.
    pub async fn navigate(
        page: &Page,
        url: &str,
    ) -> Result<String, Box<dyn std::error::Error + Send + Sync>> {
        // Start navigation but don't wait forever for the load event.
        // Blocked resources (CSS, fonts, images) can prevent the load event
        // from firing, so we use a timeout and grab content regardless.
        let nav_result = tokio::time::timeout(
            std::time::Duration::from_secs(10),
            async {
                page.goto(url).await?.wait_for_navigation().await
            },
        )
        .await;

        match nav_result {
            Ok(Ok(_)) => {
                tracing::debug!("Navigation completed normally for {}", url);
            }
            Ok(Err(e)) => {
                tracing::warn!("Navigation error for {}: {}, extracting anyway", url, e);
            }
            Err(_) => {
                tracing::warn!("Navigation timed out for {}, extracting available content", url);
            }
        }

        // Small delay to let any remaining JS execute
        tokio::time::sleep(std::time::Duration::from_millis(500)).await;

        // Extract whatever HTML content is available
        let html = tokio::time::timeout(
            std::time::Duration::from_secs(5),
            page.content()
        ).await
        .map_err(|_| "Timeout getting page content")??;
        Ok(html)
    }
}
