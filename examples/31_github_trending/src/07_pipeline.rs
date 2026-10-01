//! Pipeline: serieller Analyse-Loop mit Rate-Limit-Delay und Fortschritts-Log.

use std::io::Write;
use std::time::Duration;

use crate::config::build_question;
use crate::mcp_client::McpClient;
use crate::tool_eval::classify;
use crate::types::{AnalysisOutcome, Repo, RunReport};

/// Abstraktion des Analyse-Schritts (echter MCP-Client oder Test-Skript).
pub trait Analyzer {
    fn analyze(&mut self, repo: &Repo, log: &mut dyn Write, verbose: bool) -> AnalysisOutcome;
}

impl Analyzer for McpClient {
    fn analyze(&mut self, repo: &Repo, log: &mut dyn Write, verbose: bool) -> AnalysisOutcome {
        let question = build_question(repo);
        vlog(
            log,
            verbose,
            &format!(
                "  → Frage für {} gebaut ({} Zeichen)",
                repo.full_name(),
                question.chars().count()
            ),
        );
        classify(repo, self.ask(repo, &question, log, verbose))
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
/// Jedes übergebene Repo wird genau einmal analysiert, in Eingabereihenfolge.
pub fn run<A, S, W>(
    repos: &[Repo],
    analyzer: &mut A,
    sleeper: &S,
    delay: Duration,
    progress: &mut W,
    verbose: bool,
) -> RunReport
where
    A: Analyzer,
    S: Sleeper,
    W: Write,
{
    let total = repos.len();
    let mut report = RunReport::default();
    for (index, repo) in repos.iter().enumerate() {
        let current = index + 1;
        vlog(
            &mut *progress,
            verbose,
            &format!(
                "  → [{current}/{total}] Starte {}: DeepWiki-Anfrage wird vorbereitet",
                repo.full_name()
            ),
        );
        let outcome = analyzer.analyze(repo, &mut *progress, verbose);
        log_progress(progress, current, total, &outcome);
        vlog(
            &mut *progress,
            verbose,
            &format!(
                "  ← [{current}/{total}] {}: {}",
                repo.full_name(),
                verbose_detail(&outcome)
            ),
        );
        report.push(outcome);
        if current < total {
            vlog(
                &mut *progress,
                verbose,
                &format!("  → warte {} ms (Rate-Limit)", delay.as_millis()),
            );
            sleeper.sleep(delay);
        } else {
            vlog(
                &mut *progress,
                verbose,
                "  → letztes Repo erreicht, kein Delay",
            );
        }
    }
    report
}

/// Detailtext für die Verbose-Ergebniszeile eines Repos.
fn verbose_detail(outcome: &AnalysisOutcome) -> String {
    match outcome {
        AnalysisOutcome::Success { markdown, .. } => {
            format!("OK ({} Zeichen Markdown)", markdown.chars().count())
        }
        AnalysisOutcome::Missing { .. } => String::from("MISSING (nicht indiziert)"),
        AnalysisOutcome::Failed { reason, .. } => format!("ERROR ({reason})"),
    }
}

/// Schreibt eine Verbose-Zeile; still, wenn `verbose` aus ist.
fn vlog(log: &mut dyn Write, verbose: bool, message: &str) {
    if verbose {
        let _ = writeln!(log, "{message}");
    }
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
        fn analyze(
            &mut self,
            _repo: &Repo,
            _log: &mut dyn Write,
            _verbose: bool,
        ) -> AnalysisOutcome {
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
            false,
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
        assert!(
            !log.contains("→") && !log.contains("←"),
            "still ohne verbose: {log}"
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
            true,
        );
        assert!(report.is_empty());
        assert_eq!(sleeper.calls.get(), 0);
        assert!(progress.is_empty());
    }

    #[test]
    fn verbose_logs_every_step_for_each_repo() {
        let repos = vec![repo("a"), repo("b"), repo("c")];
        let mut analyzer = Scripted {
            outcomes: vec![
                AnalysisOutcome::Success {
                    repo: repo("a"),
                    markdown: String::from("# Analyse"),
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
            true,
        );

        assert_eq!(report.total(), 3, "jedes Repo wird verarbeitet");
        let log = String::from_utf8(progress).expect("utf8");
        for (index, name) in ["a", "b", "c"].iter().enumerate() {
            let current = index + 1;
            assert!(
                log.contains(&format!("→ [{current}/3] Starte owner/{name}")),
                "{log}"
            );
            assert!(
                log.contains(&format!("← [{current}/3] owner/{name}:")),
                "{log}"
            );
        }
        assert!(log.contains("OK (9 Zeichen Markdown)"), "{log}");
        assert!(log.contains("MISSING (nicht indiziert)"), "{log}");
        assert!(log.contains("ERROR (timeout)"), "{log}");
        assert_eq!(log.matches("warte 10 ms (Rate-Limit)").count(), 2, "{log}");
        assert!(log.contains("letztes Repo erreicht, kein Delay"), "{log}");
    }
}
