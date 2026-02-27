use cloud_proxy_server::extractor::ExtractionPipeline;

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let path = args.get(1).map(|s| s.as_str()).unwrap_or("hn.html");
    let html = std::fs::read_to_string(path).expect("failed to read HTML file");
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
