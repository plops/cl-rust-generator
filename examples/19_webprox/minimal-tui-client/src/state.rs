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

struct TextPatch {
    line_index: usize,
    new_text: String,
}

impl PageState {
    /// Apply a compressed DOM delta patch to the current markdown
    /// Returns true if the patch was applied successfully
    pub fn apply_patch(&mut self, compressed: &[u8]) -> bool {
        // Decompress with zstd
        let decompressed = match zstd::bulk::decompress(compressed, 256 * 1024) {
            Ok(d) => d,
            Err(_) => return false,
        };

        let json_str = match std::str::from_utf8(&decompressed) {
            Ok(s) => s,
            Err(_) => return false,
        };

        // Parse the minimal JSON patch array and apply text replacements
        let mut lines: Vec<String> = self.markdown.lines().map(|l| l.to_string()).collect();
        let mut modified = false;

        for entry in json_str.split("},{") {
            if let Some(tp) = Self::parse_text_patch(entry) {
                if tp.line_index < lines.len() {
                    lines[tp.line_index] = tp.new_text;
                    modified = true;
                }
            }
        }

        if modified {
            self.markdown = lines.join("\n");
        }
        modified
    }

    fn parse_text_patch(entry: &str) -> Option<TextPatch> {
        if !entry.contains("\"type\":\"text\"") {
            return None;
        }
        let path_start = entry.find("\"path\":[")?;
        let path_content = &entry[path_start + 8..];
        let path_end = path_content.find(']')?;
        let path_str = &path_content[..path_end];
        let nums: Vec<&str> = path_str.split(',').collect();
        let line_index = if nums.len() >= 2 {
            nums[1].trim().parse::<usize>().ok()?
        } else {
            nums[0].trim().parse::<usize>().ok()?
        };
        let text_start = entry.find("\"text\":\"")?;
        let text_content = &entry[text_start + 8..];
        let text_end = text_content.find('"')?;
        let new_text = text_content[..text_end].replace("\\\"", "\"");
        Some(TextPatch { line_index, new_text })
    }
}
