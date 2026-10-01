//! GitHub-Trending → DeepWiki-Algo-Analyse.
//!
//! Die Module sind in Datenflussreihenfolge nummeriert; `main.rs` verdrahtet sie nur.

#[path = "01_types.rs"]
pub mod types;

#[path = "02_config.rs"]
pub mod config;

#[path = "03_parser.rs"]
pub mod parser;

#[path = "04_mcp_protocol.rs"]
pub mod mcp_protocol;

#[path = "05_tool_eval.rs"]
pub mod tool_eval;

#[path = "06_mcp_client.rs"]
pub mod mcp_client;

#[path = "07_pipeline.rs"]
pub mod pipeline;

#[path = "08_output.rs"]
pub mod output;
