use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use html_to_markdown_rs::convert_with_visitor;
use html_to_markdown_rs::visitor::{HtmlVisitor, NodeContext, VisitResult};
use readability_rust::Readability;

/// Maps numeric link IDs to full URLs
pub type LinkMap = HashMap<u32, String>;

/// Result of the extraction pipeline
pub struct ExtractionResult {
    pub markdown: String,
    pub link_map: LinkMap,
    pub title: String,
}

/// Visitor that rewrites links to numeric IDs, collecting the URL mapping
#[derive(Debug)]
struct LinkRewriter {
    counter: u32,
    link_map: HashMap<u32, String>,
}

impl LinkRewriter {
    fn new() -> Self {
        Self {
            counter: 1,
            link_map: HashMap::new(),
        }
    }
}

impl HtmlVisitor for LinkRewriter {
    fn visit_link(
        &mut self,
        _ctx: &NodeContext,
        href: &str,
        text: &str,
        _title: Option<&str>,
    ) -> VisitResult {
        if href.starts_with("http://") || href.starts_with("https://") {
            let id = self.counter;
            self.counter += 1;
            self.link_map.insert(id, href.to_string());
            VisitResult::Custom(format!("[{}]({})", text, id))
        } else {
            VisitResult::Continue
        }
    }
}

/// The full extraction pipeline: raw HTML → readability → markdown → link_map
pub struct ExtractionPipeline;

impl ExtractionPipeline {
    /// Run the full pipeline on raw HTML
    pub fn process(html: &str, _url: &str) -> ExtractionResult {
        // Stage 1: Readability — extract main content
        let (clean_html, title) = Self::extract_readable(html);

        // Stage 2: Convert to Markdown with link rewriting via visitor
        let (markdown, link_map) = Self::to_markdown_with_links(&clean_html);

        ExtractionResult {
            markdown,
            link_map,
            title,
        }
    }

    /// Use readability-rust to strip non-semantic content
    fn extract_readable(html: &str) -> (String, String) {
        match Readability::new(html, None) {
            Ok(mut parser) => match parser.parse() {
                Some(article) => {
                    let title = article.title.unwrap_or_else(|| "Untitled".to_string());
                    let content = article.content.unwrap_or_else(|| html.to_string());
                    (content, title)
                }
                None => {
                    tracing::warn!("Readability returned no article, using raw HTML");
                    (html.to_string(), Self::extract_title_fallback(html))
                }
            },
            Err(e) => {
                tracing::warn!("Readability init failed: {}, using raw HTML", e);
                (html.to_string(), Self::extract_title_fallback(html))
            }
        }
    }

    /// Convert HTML to Markdown, replacing URLs with numeric IDs via visitor
    fn to_markdown_with_links(html: &str) -> (String, LinkMap) {
        let rewriter = Rc::new(RefCell::new(LinkRewriter::new()));
        let visitor_handle = Some(rewriter.clone() as Rc<RefCell<dyn HtmlVisitor>>);

        let markdown = match convert_with_visitor(html, None, visitor_handle) {
            Ok(md) => md,
            Err(e) => {
                tracing::warn!("Markdown conversion failed: {:?}, stripping tags", e);
                Self::strip_tags(html)
            }
        };

        let link_map = rewriter.borrow().link_map.clone();
        (markdown, link_map)
    }

    /// Fallback tag stripper
    fn strip_tags(html: &str) -> String {
        let mut result = String::new();
        let mut in_tag = false;
        for ch in html.chars() {
            match ch {
                '<' => in_tag = true,
                '>' => in_tag = false,
                _ if !in_tag => result.push(ch),
                _ => {}
            }
        }
        result
    }

    /// Fallback title extraction from <title> tag
    fn extract_title_fallback(html: &str) -> String {
        if let Some(start) = html.find("<title>") {
            let start = start + 7;
            if let Some(end) = html[start..].find("</title>") {
                return html[start..start + end].trim().to_string();
            }
        }
        "Untitled".to_string()
    }
}
