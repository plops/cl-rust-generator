use chromiumoxide::browser::{Browser, BrowserConfig};
use futures_util::StreamExt;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("Testing Chrome launch and control...\n");

    // Test 1: Headless mode
    println!("1. Testing headless mode...");
    let config = BrowserConfig::builder()
        .no_sandbox()
        .arg("--headless=new")
        .build()?;
    
    let (mut browser, mut handler) = Browser::launch(config).await?;
    
    tokio::spawn(async move {
        while handler.next().await.is_some() {}
    });
    
    let page = browser.new_page("about:blank").await?;
    page.goto("https://example.com").await?;
    let title = page.get_title().await?;
    println!("   ✓ Headless: Loaded page with title: {:?}", title);
    
    browser.close().await?;
    println!("   ✓ Headless: Browser closed successfully\n");

    // Test 2: Visible mode
    println!("2. Testing visible mode (with_head)...");
    let config = BrowserConfig::builder()
        .no_sandbox()
        .with_head()
        .build()?;
    
    let (mut browser, mut handler) = Browser::launch(config).await?;
    
    tokio::spawn(async move {
        while handler.next().await.is_some() {}
    });
    
    let page = browser.new_page("about:blank").await?;
    page.goto("https://example.com").await?;
    let title = page.get_title().await?;
    println!("   ✓ Visible: Loaded page with title: {:?}", title);
    println!("   (Chrome window should be visible for 3 seconds)");
    
    tokio::time::sleep(tokio::time::Duration::from_secs(3)).await;
    
    browser.close().await?;
    println!("   ✓ Visible: Browser closed successfully\n");

    println!("All tests passed! Chrome can be launched and controlled.");
    
    Ok(())
}
