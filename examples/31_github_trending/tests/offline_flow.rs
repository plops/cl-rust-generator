//! Offline-Integration: Feed → SSE-Fixtures → Klassifizierung → Pipeline → Dateien.
//! Kein Test in dieser Datei braucht Netzwerk.

use std::time::Duration;

use github_trending_algos::mcp_protocol::{McpError, parse_jsonrpc_response};
use github_trending_algos::output;
use github_trending_algos::parser;
use github_trending_algos::pipeline::{Analyzer, Sleeper, run};
use github_trending_algos::tool_eval::{classify, interpret_tool_result};
use github_trending_algos::types::{AnalysisOutcome, Repo, RunReport};

const FEED: &str = include_str!("../plan/20260110_01_ask_deepwiki/example_input.txt");

// Gekürzte, aber format-echte Fixtures (SSE-Hülle wie vom Live-Server beobachtet).
const SUCCESS_SSE: &str = ": ping - 2026-10-01\n\nevent: message\ndata: {\"jsonrpc\":\"2.0\",\"id\":5,\"result\":{\"content\":[{\"type\":\"text\",\"text\":\"# rust-lang/rust\\n\\nAnalyse\"}],\"structuredContent\":{\"result\":\"# rust-lang/rust\\n\\nAnalyse\"},\"isError\":false}}\n\n";
const NOT_INDEXED_SSE: &str = "event: message\ndata: {\"jsonrpc\":\"2.0\",\"id\":6,\"result\":{\"content\":[{\"type\":\"text\",\"text\":\"Error processing question: Repository not found. Visit https://deepwiki.com to index it.\"}],\"isError\":true}}\n\n";

#[test]
fn feed_yields_22_repos_in_order() {
    let repos = parser::parse_repos(FEED);
    let names: Vec<String> = repos.iter().map(Repo::full_name).collect();
    assert_eq!(repos.len(), 22);
    assert_eq!(names.first().expect("first"), "max-sixty/worktrunk");
    assert_eq!(names.last().expect("last"), "rust-lang/rust");
    // Stichprobe: gemischte Schreibweisen und Sonderzeichen bleiben erhalten.
    assert!(names.contains(&"HakanSeven12/OpenCADStudio".to_owned()));
    assert!(names.contains(&"mesamirh/MovieBox-Tui".to_owned()));
}

#[test]
fn sse_success_chain_yields_markdown() {
    let result = parse_jsonrpc_response(SUCCESS_SSE, Some(5)).expect("result");
    let markdown = interpret_tool_result(&result).expect("markdown");
    let repo = Repo::new("rust-lang", "rust").expect("valid");
    let outcome = classify(&repo, Ok(markdown));
    assert!(matches!(outcome, AnalysisOutcome::Success { .. }));
    assert_eq!(outcome.status_word(), "OK");
}

#[test]
fn sse_not_indexed_chain_yields_missing() {
    let result = parse_jsonrpc_response(NOT_INDEXED_SSE, Some(6)).expect("result");
    let error = interpret_tool_result(&result).expect_err("tool error");
    assert!(matches!(error, McpError::Tool(_)));
    let repo = Repo::new("some", "fresh-repo").expect("valid");
    let outcome = classify(&repo, Err(error));
    assert!(matches!(outcome, AnalysisOutcome::Missing { .. }));
}

struct Scripted {
    script: Vec<AnalysisOutcome>,
}

impl Analyzer for Scripted {
    fn analyze(
        &mut self,
        _repo: &Repo,
        _log: &mut dyn std::io::Write,
        _verbose: bool,
    ) -> AnalysisOutcome {
        self.script.remove(0)
    }
}

struct Noop;

impl Sleeper for Noop {
    fn sleep(&self, _duration: Duration) {}
}

#[test]
fn pipeline_to_files_end_to_end() {
    let repos = parser::parse_repos("rust-lang / rust\nsome / fresh-repo\n");
    let mut analyzer = Scripted {
        script: vec![
            AnalysisOutcome::Success {
                repo: repos[0].clone(),
                markdown: "# rust-lang/rust\n\nAnalyse".to_owned(),
            },
            AnalysisOutcome::Missing {
                repo: repos[1].clone(),
            },
        ],
    };
    let mut progress = Vec::new();
    let report = run(
        &repos,
        &mut analyzer,
        &Noop,
        Duration::from_millis(1750),
        &mut progress,
        false,
    );
    assert_eq!(report.total(), 2);
    let log = String::from_utf8(progress).expect("utf8 log");
    assert!(
        log.contains("[1/2] Analysiere rust-lang/rust ... OK"),
        "{log}"
    );
    assert!(
        log.contains("[2/2] Analysiere some/fresh-repo ... MISSING"),
        "{log}"
    );

    let dir = std::env::temp_dir().join(format!("trending-e2e-{}", std::process::id()));
    std::fs::create_dir_all(&dir).expect("temp dir");
    let stamp = "2026-01-10_01-02-03";
    let (algos, missing) = output::write_reports_with_stamp(&report, stamp, &dir).expect("write");
    let algos_text = std::fs::read_to_string(&algos).expect("read algos");
    assert!(algos_text.contains("# rust-lang/rust"), "{algos_text}");
    let missing_text = std::fs::read_to_string(&missing).expect("read missing");
    assert!(
        missing_text.contains("- [ ] [some/fresh-repo](https://github.com/some/fresh-repo)"),
        "{missing_text}"
    );
    std::fs::remove_dir_all(&dir).expect("cleanup");
}

#[test]
fn empty_report_renders_notes() {
    let report = RunReport::default();
    assert!(output::render_algos(&report).contains("Keine erfolgreichen Analysen"));
    assert!(output::render_missing(&report, "stamp").contains("Alle abgefragten"));
    // Fortschritts-Log bleibt bei leerer Eingabe still.
    let mut analyzer = Scripted { script: vec![] };
    let mut progress = Vec::new();
    let report = run(
        &[],
        &mut analyzer,
        &Noop,
        Duration::ZERO,
        &mut progress,
        false,
    );
    assert!(report.is_empty());
    assert!(progress.is_empty());
}

#[test]
fn several_projects_are_each_processed_exactly_once() {
    let input = "alpha / one\nbeta / two\ngamma / three\ndelta / four\nepsilon / five\n";
    let repos = parser::parse_repos(input);
    assert_eq!(repos.len(), 5, "alle Projekte erkannt");
    let mut analyzer = Scripted {
        script: repos
            .iter()
            .map(|repo| AnalysisOutcome::Success {
                repo: repo.clone(),
                markdown: format!("# {}", repo.full_name()),
            })
            .collect(),
    };
    let mut progress = Vec::new();
    let report = run(
        &repos,
        &mut analyzer,
        &Noop,
        Duration::ZERO,
        &mut progress,
        false,
    );
    assert_eq!(report.total(), 5);
    assert_eq!(report.successes().len(), 5);
    let log = String::from_utf8(progress).expect("utf8 log");
    for (index, repo) in repos.iter().enumerate() {
        assert!(
            log.contains(&format!(
                "[{}/5] Analysiere {} ... OK",
                index + 1,
                repo.full_name()
            )),
            "{log}"
        );
    }
}

#[test]
fn verbose_logs_each_project_step() {
    let repos = parser::parse_repos("alpha / one\nbeta / two\n");
    let mut analyzer = Scripted {
        script: vec![
            AnalysisOutcome::Success {
                repo: repos[0].clone(),
                markdown: String::from("# alpha/one"),
            },
            AnalysisOutcome::Missing {
                repo: repos[1].clone(),
            },
        ],
    };
    let mut progress = Vec::new();
    let report = run(
        &repos,
        &mut analyzer,
        &Noop,
        Duration::from_millis(1750),
        &mut progress,
        true,
    );
    assert_eq!(report.total(), 2);
    let log = String::from_utf8(progress).expect("utf8 log");
    assert!(log.contains("→ [1/2] Starte alpha/one"), "{log}");
    assert!(log.contains("← [1/2] alpha/one: OK"), "{log}");
    assert!(log.contains("→ [2/2] Starte beta/two"), "{log}");
    assert!(log.contains("← [2/2] beta/two: MISSING"), "{log}");
    assert!(log.contains("warte 1750 ms (Rate-Limit)"), "{log}");
}
