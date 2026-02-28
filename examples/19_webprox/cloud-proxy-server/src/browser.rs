use chromiumoxide::browser::{Browser, BrowserConfig};
use chromiumoxide::cdp::browser_protocol::dom::EventDocumentUpdated;
use chromiumoxide::cdp::browser_protocol::fetch::{
    ContinueRequestParams, EventRequestPaused, FailRequestParams,
};
use chromiumoxide::cdp::browser_protocol::network::ErrorReason;
use chromiumoxide::Page;
use futures_util::StreamExt;
use std::path::Path;
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
        chrome_binary: Option<String>,
        extra_chrome_args: Vec<String>,
    ) -> Result<Self, Box<dyn std::error::Error + Send + Sync>> {
        tracing::info!("Initializing browser with headless={}, flags={:?}", headless, flags);

        let selected_chrome_binary = chrome_binary.or_else(detect_chrome_binary);
        let mut launch_args: Vec<String> = Vec::new();

        let mut builder = BrowserConfig::builder()
            .no_sandbox()
            .user_data_dir(format!("/tmp/chromiumoxide-{}", fastrand::u64(..)))
            .enable_request_intercept();

        if let Some(path) = selected_chrome_binary.clone() {
            tracing::info!("Using browser executable: {}", path);
            builder = builder.chrome_executable(path);
        } else {
            tracing::info!("Using chromiumoxide auto-detected browser executable");
        }

        if !flags.load_images && !flags.load_all {
            launch_args.push("--disable-images".to_string());
            launch_args.push("--blink-settings=imagesEnabled=false".to_string());
            builder = builder
                .arg("--disable-images")
                .arg("--blink-settings=imagesEnabled=false");
        }

        if !flags.load_fonts && !flags.load_all {
            launch_args.push("--disable-remote-fonts".to_string());
            builder = builder.arg("--disable-remote-fonts");
        }

        // Configure headless mode
        if headless {
            tracing::info!("Using chromium new headless mode");
            builder = builder.new_headless_mode();
        } else {
            // Use with_head() to run in visible mode
            tracing::info!("Using with_head() for visible mode");
            builder = builder.with_head();
        }

        if !extra_chrome_args.is_empty() {
            tracing::info!("Adding {} extra Chrome arg(s) from config/CLI", extra_chrome_args.len());
            launch_args.extend(extra_chrome_args.iter().cloned());
            builder = builder.args(extra_chrome_args.iter().cloned());
        }

        tracing::debug!(
            executable = selected_chrome_binary.as_deref().unwrap_or("<auto-detect>"),
            ?launch_args,
            "Browser launch settings (chromiumoxide also appends built-in default args)"
        );

        let config = builder
            .build()
            .map_err(|e| format!("Browser config error: {}", e))?;

        let (browser, mut handler) = Browser::launch(config).await.map_err(|e| {
            let message = e.to_string();
            if message.contains("unknown flag `disable-background-networking`") {
                std::io::Error::other(
                    "Browser wrapper rejected Chromium flags. Set a real binary path with \
                     --chrome-binary or config `chrome_binary`, and retry.",
                )
            } else {
                std::io::Error::other(format!("Browser launch failed: {}", e))
            }
        })?;

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

fn detect_chrome_binary() -> Option<String> {
    const ABSOLUTE_CANDIDATES: &[&str] = &[
        "/usr/lib/chromium-browser/chrome",
        "/usr/lib/chromium/chromium",
        "/snap/chromium/current/usr/lib/chromium-browser/chrome",
        "/usr/bin/google-chrome-stable",
        "/usr/bin/google-chrome",
        "/usr/bin/chromium",
    ];
    const PATH_CANDIDATES: &[&str] = &[
        "google-chrome-stable",
        "google-chrome",
        "chromium",
        "chrome",
        "chrome-browser",
        "chromium-browser",
    ];

    for candidate in ABSOLUTE_CANDIDATES {
        if Path::new(candidate).is_file() {
            return Some((*candidate).to_string());
        }
    }

    let path_env = std::env::var_os("PATH")?;
    for dir in std::env::split_paths(&path_env) {
        for bin in PATH_CANDIDATES {
            let candidate = dir.join(bin);
            if candidate.is_file() {
                return Some(candidate.to_string_lossy().to_string());
            }
        }
    }

    None
}
