use chromiumoxide::cdp::browser_protocol::input::{
    DispatchKeyEventParams, DispatchKeyEventType,
};
use chromiumoxide::Page;

/// Dispatches input events to the browser via CDP
pub struct InputDispatcher;

impl InputDispatcher {
    /// Type a string into the currently focused element by dispatching key events
    pub async fn type_text(
        page: &Page,
        text: &str,
    ) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
        for ch in text.chars() {
            let key_down = DispatchKeyEventParams::builder()
                .r#type(DispatchKeyEventType::KeyDown)
                .text(ch.to_string())
                .build()
                .unwrap();
            page.execute(key_down).await?;

            let key_up = DispatchKeyEventParams::builder()
                .r#type(DispatchKeyEventType::KeyUp)
                .build()
                .unwrap();
            page.execute(key_up).await?;
        }
        Ok(())
    }

    /// Submit the current form (press Enter)
    pub async fn submit(
        page: &Page,
    ) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
        let key_down = DispatchKeyEventParams::builder()
            .r#type(DispatchKeyEventType::KeyDown)
            .key("Enter")
            .code("Enter")
            .text("\r")
            .build()
            .unwrap();
        page.execute(key_down).await?;

        let key_up = DispatchKeyEventParams::builder()
            .r#type(DispatchKeyEventType::KeyUp)
            .key("Enter")
            .code("Enter")
            .build()
            .unwrap();
        page.execute(key_up).await?;
        Ok(())
    }

    /// Click on a link by navigating to its URL via JS evaluation
    #[allow(dead_code)]
    pub async fn click_link(
        page: &Page,
        url: &str,
    ) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
        let js = format!(
            r#"
            (function() {{
                var links = document.querySelectorAll('a');
                for (var i = 0; i < links.length; i++) {{
                    if (links[i].href === '{}') {{
                        links[i].click();
                        return true;
                    }}
                }}
                window.location.href = '{}';
                return true;
            }})()
            "#,
            url.replace('\'', "\\'").replace('"', "\\\""),
            url.replace('\'', "\\'").replace('"', "\\\""),
        );
        page.evaluate(js).await?;
        Ok(())
    }
}
