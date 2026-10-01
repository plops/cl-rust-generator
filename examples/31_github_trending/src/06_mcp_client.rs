//! MCP-Transport: blockierender Streamable-HTTP-Client mit Session-Verwaltung.

use std::io::Write;
use std::time::Duration;

use serde_json::{Value, json};

use crate::config::{Config, REQUEST_TIMEOUT_SECS};
use crate::mcp_protocol::{
    McpError, notification_payload, parse_jsonrpc_response, request_payload,
};
use crate::tool_eval::{interpret_tool_result, is_unknown_tool};
use crate::types::Repo;

/// Blockierender MCP-Client: `initialize` → Session-ID → `tools/call` (mit Fallback-Tool).
pub struct McpClient {
    agent: ureq::Agent,
    endpoint: String,
    protocol_version: String,
    primary_tool: String,
    fallback_tool: String,
    session_id: Option<String>,
    next_id: u64,
}

impl McpClient {
    pub fn new(config: &Config) -> Self {
        let timeout = Duration::from_secs(REQUEST_TIMEOUT_SECS);
        let agent_config = ureq::config::Config::builder()
            .timeout_global(Some(timeout))
            .build();
        Self {
            agent: ureq::Agent::new_with_config(agent_config),
            endpoint: config.endpoint.clone(),
            protocol_version: config.protocol_version.clone(),
            primary_tool: config.primary_tool.clone(),
            fallback_tool: config.fallback_tool.clone(),
            session_id: None,
            next_id: 1,
        }
    }

    pub fn session_id(&self) -> Option<&str> {
        self.session_id.as_deref()
    }

    /// `initialize` → Session-ID sichern → `notifications/initialized`.
    pub fn initialize(&mut self, log: &mut dyn Write, verbose: bool) -> Result<(), McpError> {
        let params = json!({
            "protocolVersion": self.protocol_version,
            "capabilities": {},
            "clientInfo": {"name": "github-trending-algos", "version": env!("CARGO_PKG_VERSION")},
        });
        self.request("initialize", Some(params), log, verbose)?;
        if verbose {
            match self.session_id() {
                Some(session) => vlog(
                    log,
                    verbose,
                    &format!("  MCP: initialize OK (session-id={session})"),
                ),
                None => vlog(log, verbose, "  MCP: initialize OK (keine session-id)"),
            }
        }
        self.notify("notifications/initialized", Some(json!({})), log, verbose)?;
        vlog(log, verbose, "  MCP: notifications/initialized gesendet");
        Ok(())
    }

    /// Stellt die Analyse-Frage; bei unbekanntem Tool einmal mit Alternativnamen.
    pub fn ask(
        &mut self,
        repo: &Repo,
        question: &str,
        log: &mut dyn Write,
        verbose: bool,
    ) -> Result<String, McpError> {
        let primary = self.primary_tool.clone();
        let fallback = self.fallback_tool.clone();
        let full_name = repo.full_name();
        let params_for = |tool: &str| json!({"name": tool, "arguments": {"repoName": full_name, "question": question}});
        vlog(
            log,
            verbose,
            &format!("  → DeepWiki-Request für {full_name} gesendet (tool={primary})"),
        );
        match self.request("tools/call", Some(params_for(&primary)), log, verbose) {
            Ok(result) => interpret_tool_result(&result),
            Err(error) if is_unknown_tool(&error) => {
                vlog(
                    log,
                    verbose,
                    &format!("  MCP: Tool {primary} unbekannt ({error}); Fallback {fallback}"),
                );
                let result =
                    self.request("tools/call", Some(params_for(&fallback)), log, verbose)?;
                interpret_tool_result(&result)
            }
            Err(error) => Err(error),
        }
    }

    fn take_id(&mut self) -> u64 {
        let id = self.next_id;
        self.next_id = self.next_id.wrapping_add(1).max(1);
        id
    }

    fn request(
        &mut self,
        method: &str,
        params: Option<Value>,
        log: &mut dyn Write,
        verbose: bool,
    ) -> Result<Value, McpError> {
        let id = self.take_id();
        let payload = request_payload(method, params, id)?;
        vlog(
            log,
            verbose,
            &format!("  MCP: POST {method} (id={id}) an {}", self.endpoint),
        );
        let raw = self.http_post(&payload)?;
        if let Some(session) = raw.session {
            self.session_id = Some(session);
        }
        vlog(
            log,
            verbose,
            &format!("  MCP: Antwort auf id={id} ({} Bytes)", raw.body.len()),
        );
        parse_jsonrpc_response(&raw.body, Some(id))
    }

    fn notify(
        &mut self,
        method: &str,
        params: Option<Value>,
        log: &mut dyn Write,
        verbose: bool,
    ) -> Result<(), McpError> {
        let payload = notification_payload(method, params)?;
        vlog(
            log,
            verbose,
            &format!("  MCP: POST {method} (notification) an {}", self.endpoint),
        );
        let raw = self.http_post(&payload)?;
        if let Some(session) = raw.session {
            self.session_id = Some(session);
        }
        Ok(())
    }

    fn http_post(&self, payload: &Value) -> Result<RawResponse, McpError> {
        let mut call = self
            .agent
            .post(self.endpoint.as_str())
            .header("Content-Type", "application/json")
            .header("Accept", "application/json, text/event-stream");
        if let Some(session) = &self.session_id {
            call = call.header("Mcp-Session-Id", session.as_str());
        }
        let mut response = call.send_json(payload).map_err(map_ureq_error)?;
        let session = response
            .headers()
            .get("mcp-session-id")
            .and_then(|value| value.to_str().ok())
            .map(str::to_owned);
        let body = response
            .body_mut()
            .read_to_string()
            .map_err(|error| McpError::Transport(error.to_string()))?;
        Ok(RawResponse { session, body })
    }
}

struct RawResponse {
    session: Option<String>,
    body: String,
}

fn map_ureq_error(error: ureq::Error) -> McpError {
    match error {
        ureq::Error::StatusCode(code) => McpError::HttpStatus(code),
        other => McpError::Transport(other.to_string()),
    }
}

/// Schreibt eine Verbose-Zeile; still, wenn `verbose` aus ist.
fn vlog(log: &mut dyn Write, verbose: bool, message: &str) {
    if verbose {
        let _ = writeln!(log, "{message}");
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn fresh_client_has_no_session() {
        let client = McpClient::new(&Config::default());
        assert_eq!(client.session_id(), None);
    }

    #[test]
    fn request_ids_increase() {
        let mut client = McpClient::new(&Config::default());
        assert_eq!(client.take_id(), 1);
        assert_eq!(client.take_id(), 2);
    }

    #[test]
    fn unknown_tool_detection() {
        let rpc = McpError::Rpc {
            code: -32601,
            message: "Method not found".to_owned(),
        };
        assert!(is_unknown_tool(&rpc));
        assert!(!is_unknown_tool(&McpError::HttpStatus(429)));
    }
}
