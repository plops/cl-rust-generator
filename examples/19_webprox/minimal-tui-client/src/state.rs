use std::collections::HashMap;

/// Resolved link map: numeric ID → full URL
pub type LinkMap = HashMap<u32, String>;

/// The local page state maintained by the client
pub struct PageState {
    pub markdown: String,
    pub link_map: LinkMap,
    pub title: String,
    pub url: String,
}

impl PageState {
    pub fn new() -> Self {
        Self {
            markdown: String::new(),
            link_map: LinkMap::new(),
            title: String::new(),
            url: String::new(),
        }
    }

    /// Apply a full page update
    pub fn apply_full_page(
        &mut self,
        markdown: String,
        link_map: HashMap<u32, String>,
        title: String,
        url: String,
    ) {
        self.markdown = markdown;
        self.link_map = link_map;
        self.title = title;
        self.url = url;
    }

    /// Resolve a numeric link ID to its full URL
    pub fn resolve_link(&self, id: u32) -> Option<&str> {
        self.link_map.get(&id).map(|s| s.as_str())
    }
}
