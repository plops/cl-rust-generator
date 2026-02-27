use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use html_to_markdown_rs::convert_with_visitor;
use html_to_markdown_rs::visitor::{HtmlVisitor, NodeContext, VisitResult};
use readability_rust::Readability;
use url::Url;

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
    base_url: Option<Url>,
}

impl LinkRewriter {
    fn new(base_url: Option<Url>) -> Self {
        Self {
            counter: 1,
            link_map: HashMap::new(),
            base_url,
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
        let absolute_url = if let Some(base) = &self.base_url {
            base.join(href).map(|u| u.to_string()).unwrap_or_else(|_| href.to_string())
        } else {
            href.to_string()
        };

        let id = self.counter;
        self.counter += 1;
        self.link_map.insert(id, absolute_url);
        VisitResult::Custom(format!("[{}]({})", text, id))
    }
}

/// The full extraction pipeline: raw HTML -> readability -> markdown -> link_map
pub struct ExtractionPipeline;

impl ExtractionPipeline {
    /// Run the full pipeline on raw HTML
    pub fn process(html: &str, url: &str) -> ExtractionResult {
        // Stage 0: Strip data URIs that cause panics in html-to-markdown-rs
        let sanitized = Self::strip_data_uris(html);

        // Stage 1: Readability
        let (clean_html, title) = Self::extract_readable(&sanitized);

        // Stage 2: Markdown with link rewriting
        let (markdown, link_map) = Self::to_markdown_with_links(&clean_html, url);

        // Stage 3: Fallback if Readability stripped too much content
        // Count non-whitespace lines in the markdown
        let content_lines = markdown.lines()
            .filter(|line| !line.trim().is_empty())
            .count();
        
        if content_lines < 5 {
            tracing::warn!("Readability produced minimal content ({} lines), retrying with raw HTML", content_lines);
            let (raw_markdown, raw_link_map) = Self::to_markdown_with_links(&sanitized, url);
            ExtractionResult { 
                markdown: raw_markdown, 
                link_map: raw_link_map, 
                title 
            }
        } else {
            ExtractionResult { markdown, link_map, title }
        }
    }

    /// Strip data: URIs from src/href attributes to prevent panics in the
    /// markdown converter (it chokes on base64 blobs with bad byte indexing)
    fn strip_data_uris(html: &str) -> String {
        let mut result = String::with_capacity(html.len());
        let mut rest = html;
        while let Some(pos) = rest.find("data:") {
            let before = &rest[..pos];
            let in_attr = before.ends_with("=\"") || before.ends_with("='");
            if in_attr {
                result.push_str(before);
                result.push('#');
                let after = &rest[pos..];
                let end = after.find('"')
                    .or_else(|| after.find('\''))
                    .unwrap_or(after.len());
                rest = &rest[pos + end..];
            } else {
                result.push_str(&rest[..pos + 5]);
                rest = &rest[pos + 5..];
            }
        }
        result.push_str(rest);
        result
    }

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

    /// Convert HTML to Markdown with catch_unwind as safety net against library panics
    fn to_markdown_with_links(html: &str, base_url: &str) -> (String, LinkMap) {
        let base = Url::parse(base_url).ok();
        let rewriter = Rc::new(RefCell::new(LinkRewriter::new(base)));
        let rewriter_clone = rewriter.clone();
        let html_owned = html.to_string();

        let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(move || {
            let visitor = Some(rewriter_clone as Rc<RefCell<dyn HtmlVisitor>>);
            convert_with_visitor(&html_owned, None, visitor)
        }));

        let markdown = match result {
            Ok(Ok(md)) => md,
            Ok(Err(e)) => {
                tracing::warn!("Markdown conversion failed: {:?}, stripping tags", e);
                Self::strip_tags(html)
            }
            Err(_) => {
                tracing::warn!("Markdown converter panicked, falling back to tag stripping");
                Self::strip_tags(html)
            }
        };

        let link_map = rewriter.borrow().link_map.clone();
        (markdown, link_map)
    }

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
