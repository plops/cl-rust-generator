//! MCP-Protokoll: JSON-RPC-Hüllen, SSE-Parsing, Antwort-Auswahl.

use std::fmt;

use serde::Serialize;
use serde_json::Value;

/// JSON-RPC-2.0-Hülle; `id: None` markiert eine Notification (ohne Antwort).
#[derive(Debug, Serialize)]
struct JsonRpcRequest {
    jsonrpc: &'static str,
    method: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    params: Option<Value>,
    #[serde(skip_serializing_if = "Option::is_none")]
    id: Option<u64>,
}

/// Alle Fehler der MCP-Schicht; kein Pfad darf panicken.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum McpError {
    Transport(String),
    HttpStatus(u16),
    Protocol(String),
    Rpc { code: i32, message: String },
    Tool(String),
}

impl fmt::Display for McpError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Transport(detail) => write!(f, "Transportfehler: {detail}"),
            Self::HttpStatus(code) => write!(f, "HTTP-Status {code}"),
            Self::Protocol(detail) => write!(f, "Protokollfehler: {detail}"),
            Self::Rpc { code, message } => write!(f, "RPC-Fehler {code}: {message}"),
            Self::Tool(detail) => write!(f, "Tool-Fehler: {detail}"),
        }
    }
}

impl std::error::Error for McpError {}

/// Baut einen JSON-RPC-Request-Payload (`id` Pflicht).
pub fn request_payload(method: &str, params: Option<Value>, id: u64) -> Result<Value, McpError> {
    serde_json::to_value(JsonRpcRequest {
        jsonrpc: "2.0",
        method: method.to_owned(),
        params,
        id: Some(id),
    })
    .map_err(|error| McpError::Protocol(format!("Request-Serialisierung: {error}")))
}

/// Baut einen Notification-Payload (ohne `id`, keine Antwort erwartet).
pub fn notification_payload(method: &str, params: Option<Value>) -> Result<Value, McpError> {
    serde_json::to_value(JsonRpcRequest {
        jsonrpc: "2.0",
        method: method.to_owned(),
        params,
        id: None,
    })
    .map_err(|error| McpError::Protocol(format!("Notify-Serialisierung: {error}")))
}

/// Extrahiert JSON-Payloads aus SSE (`data: {...}`) oder reinem JSON-Body.
pub fn extract_json_payloads(body: &str) -> Vec<Value> {
    let trimmed = body.trim();
    if trimmed.is_empty() {
        return Vec::new();
    }
    if trimmed.starts_with('{')
        && let Ok(value) = serde_json::from_str::<Value>(trimmed)
    {
        return vec![value];
    }
    let mut out = Vec::new();
    for line in body.lines() {
        let Some(data) = line.trim().strip_prefix("data:") else {
            continue;
        };
        let data = data.trim();
        if data.is_empty() || data == "[DONE]" {
            continue;
        }
        if let Ok(value) = serde_json::from_str(data) {
            out.push(value);
        }
    }
    out
}

/// Wählt die JSON-RPC-Antwort (bevorzugt mit passender `id`) und liefert `result`.
pub fn parse_jsonrpc_response(body: &str, expected_id: Option<u64>) -> Result<Value, McpError> {
    let mut fallback = None;
    for payload in extract_json_payloads(body) {
        if !payload.is_object() {
            continue;
        }
        let id_matches = payload.get("id").and_then(Value::as_u64) == expected_id;
        if expected_id.is_some() && !id_matches {
            if fallback.is_none() {
                fallback = Some(payload);
            }
            continue;
        }
        return interpret_response(payload);
    }
    if let Some(payload) = fallback {
        return interpret_response(payload);
    }
    Err(McpError::Protocol(
        "Antwort enthält kein JSON-RPC-Objekt".to_owned(),
    ))
}

fn interpret_response(payload: Value) -> Result<Value, McpError> {
    if let Some(error) = payload.get("error")
        && !error.is_null()
    {
        let code = error
            .get("code")
            .and_then(Value::as_i64)
            .and_then(|code| i32::try_from(code).ok())
            .unwrap_or(0);
        let message = error
            .get("message")
            .and_then(Value::as_str)
            .unwrap_or("unbekannter RPC-Fehler")
            .to_owned();
        return Err(McpError::Rpc { code, message });
    }
    match payload.get("result") {
        Some(result) if !result.is_null() => Ok(result.clone()),
        _ => Err(McpError::Protocol(
            "Antwort enthält weder result noch error".to_owned(),
        )),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const SUCCESS_SSE: &str = "event: message\ndata: {\"jsonrpc\":\"2.0\",\"id\":3,\"result\":{\"content\":[{\"type\":\"text\",\"text\":\"Hallo\"}],\"isError\":false}}\n\n";
    const PING_SSE: &str = ": ping - 2026-10-01\n\nevent: message\ndata: {\"jsonrpc\":\"2.0\",\"id\":1,\"result\":{\"ok\":true}}\n\n";

    #[test]
    fn payload_builders_set_id_correctly() {
        let request = request_payload("tools/call", None, 9).expect("request");
        assert_eq!(request.get("id").and_then(Value::as_u64), Some(9));
        assert_eq!(request.get("jsonrpc").and_then(Value::as_str), Some("2.0"));
        let notify = notification_payload("notifications/initialized", None).expect("notify");
        assert!(notify.get("id").is_none());
    }

    #[test]
    fn extracts_sse_data_and_ignores_pings() {
        let payloads = extract_json_payloads(PING_SSE);
        assert_eq!(payloads.len(), 1);
        assert_eq!(payloads[0].get("id").and_then(Value::as_u64), Some(1));
    }

    #[test]
    fn accepts_plain_json_body() {
        let payloads = extract_json_payloads(r#"{"jsonrpc":"2.0","id":7,"result":{}}"#);
        assert_eq!(payloads.len(), 1);
    }

    #[test]
    fn skips_done_markers_and_garbage() {
        let payloads = extract_json_payloads("data: [DONE]\ndata: kein-json\n\nevent: x\n");
        assert!(payloads.is_empty());
    }

    #[test]
    fn parses_response_with_matching_id() {
        let result = parse_jsonrpc_response(SUCCESS_SSE, Some(3)).expect("result");
        assert_eq!(result.get("isError").and_then(Value::as_bool), Some(false));
    }

    #[test]
    fn parses_rpc_error_object() {
        let body = "data: {\"jsonrpc\":\"2.0\",\"id\":2,\"error\":{\"code\":-32601,\"message\":\"Method not found\"}}";
        let error = parse_jsonrpc_response(body, Some(2)).expect_err("rpc error");
        assert_eq!(
            error,
            McpError::Rpc {
                code: -32601,
                message: "Method not found".to_owned()
            }
        );
    }
}
