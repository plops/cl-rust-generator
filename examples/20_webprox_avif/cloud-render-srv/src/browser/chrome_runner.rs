use chromiumoxide::browser::{Browser, BrowserConfig, HeadlessMode};
use chromiumoxide::Page;
use anyhow::Result;
use tokio_stream::StreamExt;

pub struct ChromeRunner {
    browser: Browser,
}

impl ChromeRunner {
    pub async fn new(headless: bool) -> Result<Self> {
        let config = BrowserConfig::builder()
            .no_sandbox()
            .headless_mode(if headless { HeadlessMode::True } else { HeadlessMode::False })
            .build()
            .map_err(|e| anyhow::anyhow!("Failed to build browser config: {}", e))?;
        
        let (browser, mut handler) = Browser::launch(config).await?;
        
        // Start the handler in the background
        tokio::spawn(async move {
            while let Some(_) = handler.next().await {
                // Handle browser events
            }
        });
        
        Ok(ChromeRunner { browser })
    }
    
    pub async fn new_page(&self) -> Result<Page> {
        let page = self.browser.new_page("about:blank").await?;
        Ok(page)
    }
    
    pub async fn close(&mut self) -> Result<()> {
        self.browser.close().await?;
        Ok(())
    }
}
