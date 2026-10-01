//! Tool-Auswertung: `tools/call`-Texte, Not-indexed-Matcher, Outcome-Klassifizierung.

use serde_json::Value;

use crate::mcp_protocol::McpError;
use crate::types::{AnalysisOutcome, Repo};

/// Wertet `tools/call`-Resultate aus: `isError:true` → `McpError::Tool`, sonst Text.
pub fn interpret_tool_result(result: &Value) -> Result<String, McpError> {
    let mut texts = Vec::new();
    if let Some(content) = result.get("content").and_then(Value::as_array) {
        for item in content {
            if let Some(text) = item.get("text").and_then(Value::as_str) {
                texts.push(text.to_owned());
            }
        }
    }
    if let Some(structured) = result
        .get("structuredContent")
        .and_then(|value| value.get("result"))
        .and_then(Value::as_str)
        && !texts.iter().any(|text| text == structured)
    {
        texts.push(structured.to_owned());
    }
    let combined = texts.join("\n");
    if result.get("isError").and_then(Value::as_bool) == Some(true) {
        let detail = if combined.is_empty() {
            "DeepWiki meldete einen Fehler ohne Details".to_owned()
        } else {
            combined
        };
        return Err(McpError::Tool(detail));
    }
    if combined.trim().is_empty() {
        return Err(McpError::Protocol(
            "tools/call lieferte kein Text-Content".to_owned(),
        ));
    }
    Ok(combined)
}

/// Erkennt unbekannte Tools/Methoden (Fallback-Trigger, niemals „Missing“).
pub fn is_unknown_tool(error: &McpError) -> bool {
    let text = error.to_string().to_lowercase();
    text.contains("method not found") || text.contains("unknown tool")
}

/// Erkennt „nicht indiziert“ anhand der Server-Fehlermeldungen (case-insensitiv).
pub fn is_not_indexed_message(message: &str) -> bool {
    let lower = message.to_lowercase();
    [
        "not indexed",
        "isn't indexed",
        "isnt indexed",
        "not yet indexed",
        "not found",
        "to index",
        "index it",
    ]
    .iter()
    .any(|needle| lower.contains(needle))
}

/// Klassifiziert ein Abfrage-Ergebnis; unbekannte Tools und HTTP 404/500 beachten.
pub fn classify(repo: &Repo, result: Result<String, McpError>) -> AnalysisOutcome {
    match result {
        Ok(markdown) => AnalysisOutcome::Success {
            repo: repo.clone(),
            markdown,
        },
        Err(McpError::HttpStatus(404 | 500)) => AnalysisOutcome::Missing { repo: repo.clone() },
        Err(error) if is_unknown_tool(&error) => AnalysisOutcome::Failed {
            repo: repo.clone(),
            reason: error.to_string(),
        },
        Err(error) if is_not_indexed_message(&error.to_string()) => {
            AnalysisOutcome::Missing { repo: repo.clone() }
        }
        Err(error) => AnalysisOutcome::Failed {
            repo: repo.clone(),
            reason: error.to_string(),
        },
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[test]
    fn tool_result_ok_and_error_paths() {
        let ok = json!({"content": [{"type": "text", "text": "Analyse"}], "isError": false});
        assert_eq!(interpret_tool_result(&ok).expect("ok"), "Analyse");
        let failed = json!({"content": [{"type": "text", "text": "Repository not found. Visit https://deepwiki.com to index it."}], "isError": true});
        assert!(matches!(
            interpret_tool_result(&failed),
            Err(McpError::Tool(_))
        ));
        let empty = json!({"content": [], "isError": false});
        assert!(matches!(
            interpret_tool_result(&empty),
            Err(McpError::Protocol(_))
        ));
    }

    #[test]
    fn not_indexed_matcher_matrix() {
        for message in [
            "Repository is not indexed yet",
            "Repo NOT INDEXED",
            "Project isn't indexed",
            "Project isnt indexed",
            "Repository not found. Visit https://deepwiki.com to index it.",
            "Please wait, to index this repo visit deepwiki",
        ] {
            assert!(is_not_indexed_message(message), "sollte matchen: {message}");
        }
        assert!(!is_not_indexed_message("HTTP 429 too many requests"));
        assert!(!is_not_indexed_message("connection reset by peer"));
    }

    #[test]
    fn classify_routes_all_variants() {
        let repo = Repo::new("o", "r").expect("valid");
        assert!(matches!(
            classify(&repo, Ok("md".to_owned())),
            AnalysisOutcome::Success { .. }
        ));
        assert!(matches!(
            classify(&repo, Err(McpError::HttpStatus(404))),
            AnalysisOutcome::Missing { .. }
        ));
        assert!(matches!(
            classify(&repo, Err(McpError::HttpStatus(500))),
            AnalysisOutcome::Missing { .. }
        ));
        assert!(matches!(
            classify(&repo, Err(McpError::Tool("x is not indexed".to_owned()))),
            AnalysisOutcome::Missing { .. }
        ));
        // „Method not found“ trotz „not found“-Teilstring: kein Missing!
        assert!(matches!(
            classify(
                &repo,
                Err(McpError::Rpc {
                    code: -32601,
                    message: "Method not found".to_owned()
                })
            ),
            AnalysisOutcome::Failed { .. }
        ));
        assert!(matches!(
            classify(&repo, Err(McpError::HttpStatus(429))),
            AnalysisOutcome::Failed { .. }
        ));
    }
}
