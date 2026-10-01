//! Pipeline: serieller Analyse-Loop mit Rate-Limit-Delay und Fortschritts-Log.

use std::io::Write;
use std::time::Duration;

use crate::config::build_question;
use crate::mcp_client::McpClient;
use crate::tool_eval::classify;
use crate::types::{AnalysisOutcome, Repo, RunReport};

/// Abstraktion des Analyse-Schritts (echter MCP-Client oder Test-Skript).
pub trait Analyzer {
    fn analyze(&mut self, repo: &Repo) -> AnalysisOutcome;
}

impl Analyzer for McpClient {
    fn analyze(&mut self, repo: &Repo) -> AnalysisOutcome {
        let question = build_question(repo);
        classify(repo, self.ask(repo, &question))
    }
}

/// Abstraktion des Delays (echter Sleep oder No-op in Tests).
pub trait Sleeper {
    fn sleep(&self, duration: Duration);
}

/// Echter Delay via `std::thread::sleep`.
pub struct ThreadSleeper;

impl Sleeper for ThreadSleeper {
    fn sleep(&self, duration: Duration) {
        std::thread::sleep(duration);
    }
}

/// Führt den Loop aus; einzelne Repo-Fehler brechen den Lauf nie ab.
pub fn run<A, S, W>(
    repos: &[Repo],
    analyzer: &mut A,
    sleeper: &S,
    delay: Duration,
    progress: &mut W,
) -> RunReport
where
    A: Analyzer,
    S: Sleeper,
    W: Write,
{
    let total = repos.len();
    let mut report = RunReport::default();
    for (index, repo) in repos.iter().enumerate() {
        let outcome = analyzer.analyze(repo);
        log_progress(progress, index + 1, total, &outcome);
        report.push(outcome);
        if index + 1 < total {
            sleeper.sleep(delay);
        }
    }
    report
}

fn log_progress<W: Write>(out: &mut W, current: usize, total: usize, outcome: &AnalysisOutcome) {
    let detail = match outcome {
        AnalysisOutcome::Success { .. } => String::new(),
        AnalysisOutcome::Missing { .. } => String::from(" (nicht indiziert)"),
        AnalysisOutcome::Failed { reason, .. } => format!(": {reason}"),
    };
    let line = format!(
        "[{current}/{total}] Analysiere {} ... {}{detail}\n",
        outcome.repo().full_name(),
        outcome.status_word()
    );
    let _ = out.write_all(line.as_bytes());
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::cell::Cell;

    struct Scripted {
        outcomes: Vec<AnalysisOutcome>,
    }

    impl Analyzer for Scripted {
        fn analyze(&mut self, _repo: &Repo) -> AnalysisOutcome {
            self.outcomes.remove(0)
        }
    }

    struct RecordingSleeper {
        calls: Cell<usize>,
    }

    impl Sleeper for RecordingSleeper {
        fn sleep(&self, _duration: Duration) {
            self.calls.set(self.calls.get() + 1);
        }
    }

    fn repo(name: &str) -> Repo {
        Repo::new("owner", name).expect("valid")
    }

    #[test]
    fn logs_progress_and_sleeps_between_requests() {
        let repos = vec![repo("a"), repo("b"), repo("c")];
        let mut analyzer = Scripted {
            outcomes: vec![
                AnalysisOutcome::Success {
                    repo: repo("a"),
                    markdown: String::new(),
                },
                AnalysisOutcome::Missing { repo: repo("b") },
                AnalysisOutcome::Failed {
                    repo: repo("c"),
                    reason: "timeout".to_owned(),
                },
            ],
        };
        let sleeper = RecordingSleeper {
            calls: Cell::new(0),
        };
        let mut progress = Vec::new();
        let report = run(
            &repos,
            &mut analyzer,
            &sleeper,
            Duration::from_millis(10),
            &mut progress,
        );

        assert_eq!(report.total(), 3);
        assert_eq!(sleeper.calls.get(), 2, "Delay nur zwischen Anfragen");
        let log = String::from_utf8(progress).expect("utf8");
        assert!(log.contains("[1/3] Analysiere owner/a ... OK"), "{log}");
        assert!(
            log.contains("[2/3] Analysiere owner/b ... MISSING"),
            "{log}"
        );
        assert!(
            log.contains("[3/3] Analysiere owner/c ... ERROR: timeout"),
            "{log}"
        );
    }

    #[test]
    fn empty_input_produces_empty_report() {
        let mut analyzer = Scripted { outcomes: vec![] };
        let sleeper = RecordingSleeper {
            calls: Cell::new(0),
        };
        let mut progress = Vec::new();
        let report = run(
            &[],
            &mut analyzer,
            &sleeper,
            Duration::from_millis(10),
            &mut progress,
        );
        assert!(report.is_empty());
        assert_eq!(sleeper.calls.get(), 0);
        assert!(progress.is_empty());
    }
}
