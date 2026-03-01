use chromiumoxide::Page;
use proto_def::graphical_proxy::{SpatialMetadata, LinkBox};
use serde_json::Value;
use anyhow::Result;

pub async fn extract_spatial_metadata(page: &Page) -> Result<SpatialMetadata> {
    let js_script = r#"
        (() => {
            const links = Array.from(document.querySelectorAll('a')).map((a, i) => {
                const rect = a.getBoundingClientRect();
                return {
                    id: i,
                    url: a.href,
                    label: a.innerText.substring(0, 50),
                    x: Math.round(rect.x + window.scrollX),
                    y: Math.round(rect.y + window.scrollY),
                    width: Math.round(rect.width),
                    height: Math.round(rect.height)
                };
            });
            return {
                title: document.title,
                doc_width: document.documentElement.scrollWidth,
                doc_height: document.documentElement.scrollHeight,
                links: links
            };
        })();
    "#;

    let result: Value = page.evaluate(js_script).await?.into_value()?;
    
    let title = result["title"].as_str().unwrap_or("").to_string();
    let doc_width = result["doc_width"].as_u64().unwrap_or(0) as u32;
    let doc_height = result["doc_height"].as_u64().unwrap_or(0) as u32;
    
    let mut links = Vec::new();
    if let Some(links_array) = result["links"].as_array() {
        for link_data in links_array {
            let link_box = LinkBox {
                id: link_data["id"].as_u64().unwrap_or(0) as u32,
                url: link_data["url"].as_str().unwrap_or("").to_string(),
                label: link_data["label"].as_str().unwrap_or("").to_string(),
                x: link_data["x"].as_i64().unwrap_or(0) as i32,
                y: link_data["y"].as_i64().unwrap_or(0) as i32,
                width: link_data["width"].as_i64().unwrap_or(0) as i32,
                height: link_data["height"].as_i64().unwrap_or(0) as i32,
            };
            links.push(link_box);
        }
    }
    
    Ok(SpatialMetadata {
        title,
        document_width: doc_width,
        document_height: doc_height,
        links,
    })
}
