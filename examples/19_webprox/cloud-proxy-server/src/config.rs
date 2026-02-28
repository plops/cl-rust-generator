use serde::Deserialize;
use std::fs;
use std::path::Path;

#[derive(Debug, Clone, Default, Deserialize)]
pub struct ServerConfigFile {
    pub listen_addr: Option<String>,
    pub no_headless: Option<bool>,
    pub load_all: Option<bool>,
    pub load_images: Option<bool>,
    pub load_media: Option<bool>,
    pub load_css: Option<bool>,
    pub load_fonts: Option<bool>,
    pub chrome_binary: Option<String>,
    pub extra_chrome_args: Option<Vec<String>>,
    pub log_level: Option<String>,
}

impl ServerConfigFile {
    pub fn load(path: &Path) -> Result<Self, Box<dyn std::error::Error + Send + Sync>> {
        let raw = fs::read_to_string(path)?;
        let config = toml::from_str::<Self>(&raw)?;
        Ok(config)
    }
}
