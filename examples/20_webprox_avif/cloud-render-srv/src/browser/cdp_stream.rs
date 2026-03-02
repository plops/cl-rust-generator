use chromiumoxide::Page;
use chromiumoxide::page::ScreenshotParams;
use image::RgbaImage;
use anyhow::Result;

pub struct CdpStream;

impl CdpStream {
    pub async fn capture_screenshot(page: &Page, full_page: bool) -> Result<RgbaImage> {
        // Build screenshot parameters with optional full page
        let params = if full_page {
            ScreenshotParams::builder().full_page(true).build()
        } else {
            ScreenshotParams::default()
        };
        
        // Capture screenshot as PNG bytes
        let png_bytes: Vec<u8> = page
            .screenshot(params)
            .await?;
        
        // Load PNG as image
        let img = image::load_from_memory(&png_bytes)?;
        let rgba_img = img.to_rgba8();
        
        Ok(rgba_img)
    }
    
    pub async fn navigate_to(page: &Page, url: &str) -> Result<()> {
        page.goto(url).await?;
        
        // Wait for page to load
        page.wait_for_navigation().await?;
        
        // Additional wait to ensure page is fully rendered
        tokio::time::sleep(std::time::Duration::from_millis(1000)).await;
        
        Ok(())
    }
}
