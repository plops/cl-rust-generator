use cloud_proxy_server::extractor::ExtractionPipeline;

fn main() {
    let html = std::fs::read_to_string("/tmp/hn.html").expect("save HN html to /tmp/hn.html first");
    let result = ExtractionPipeline::process(&html, "https://news.ycombinator.com");
    println!("=== TITLE ===");
    println!("{}", result.title);
    println!("=== MARKDOWN ({} lines, {} chars) ===", result.markdown.lines().count(), result.markdown.len());
    println!("{}", &result.markdown[..result.markdown.len().min(3000)]);
    println!("=== LINK MAP ({} entries) ===", result.link_map.len());
    for (id, url) in result.link_map.iter().take(10) {
        println!("  {} -> {}", id, url);
    }
}
