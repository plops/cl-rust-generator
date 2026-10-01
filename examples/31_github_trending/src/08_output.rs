//! Output: Zeitstempel-Dateien `algos_<ts>.md` und `not-index-yet_<ts>.md`.

use std::fs;
use std::io;
use std::path::{Path, PathBuf};

use chrono::Local;

use crate::types::RunReport;

/// Aktueller lokaler Zeitstempel im Format `YYYY-MM-DD_HH-mm-ss`.
pub fn current_timestamp() -> String {
    Local::now().format("%Y-%m-%d_%H-%M-%S").to_string()
}

pub fn algos_filename(stamp: &str) -> String {
    format!("algos_{stamp}.md")
}

pub fn missing_filename(stamp: &str) -> String {
    format!("not-index-yet_{stamp}.md")
}

/// Erfolgreiche Analysen, getrennt durch `---`.
pub fn render_algos(report: &RunReport) -> String {
    if report.successes().is_empty() {
        return String::from("_Keine erfolgreichen Analysen in diesem Lauf._\n");
    }
    let mut out = report
        .successes()
        .iter()
        .map(|(_, markdown)| markdown.trim())
        .collect::<Vec<_>>()
        .join("\n\n---\n\n");
    out.push('\n');
    out
}

/// Checkliste der nicht indizierten Repos mit GitHub- und DeepWiki-Links.
pub fn render_missing(report: &RunReport, stamp: &str) -> String {
    let mut out = format!("# Nicht indizierte Repositories ({stamp})\n");
    if report.missing().is_empty() {
        out.push_str("_Alle abgefragten Repositories sind indiziert._\n");
        return out;
    }
    for repo in report.missing() {
        out.push_str(&format!(
            "- [ ] [{}]({}) – DeepWiki: {}\n",
            repo.full_name(),
            repo.github_url(),
            repo.deepwiki_url()
        ));
    }
    out
}

/// Schreibt beide Report-Dateien mit frischem Zeitstempel nach `dir`.
pub fn write_reports(report: &RunReport, dir: &Path) -> io::Result<(PathBuf, PathBuf)> {
    write_reports_with_stamp(report, &current_timestamp(), dir)
}

/// Variante mit festem Stempel (deterministisch testbar).
pub fn write_reports_with_stamp(
    report: &RunReport,
    stamp: &str,
    dir: &Path,
) -> io::Result<(PathBuf, PathBuf)> {
    let algos_path = dir.join(algos_filename(stamp));
    let missing_path = dir.join(missing_filename(stamp));
    fs::write(&algos_path, render_algos(report))?;
    fs::write(&missing_path, render_missing(report, stamp))?;
    Ok((algos_path, missing_path))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::{AnalysisOutcome, Repo};

    fn sample_report() -> RunReport {
        let mut report = RunReport::default();
        report.push(AnalysisOutcome::Success {
            repo: Repo::new("a", "one").expect("valid"),
            markdown: String::from("# a/one\nText"),
        });
        report.push(AnalysisOutcome::Success {
            repo: Repo::new("b", "two").expect("valid"),
            markdown: String::from("# b/two\nText"),
        });
        report.push(AnalysisOutcome::Missing {
            repo: Repo::new("c", "three").expect("valid"),
        });
        report
    }

    #[test]
    fn timestamp_format_is_sortable() {
        let stamp = current_timestamp();
        assert_eq!(stamp.len(), "2026-01-10_01-02-03".len(), "{stamp}");
        assert_eq!(&stamp[4..5], "-");
        assert_eq!(&stamp[10..11], "_");
        assert!(
            stamp
                .chars()
                .all(|c| c.is_ascii_digit() || c == '-' || c == '_'),
            "{stamp}"
        );
    }

    #[test]
    fn algos_joined_with_separator() {
        let rendered = render_algos(&sample_report());
        assert!(
            rendered.contains("# a/one\nText\n\n---\n\n# b/two\nText\n"),
            "{rendered}"
        );
    }

    #[test]
    fn missing_checklist_matches_template() {
        let rendered = render_missing(&sample_report(), "2026-01-10_01-02-03");
        assert!(
            rendered.contains("# Nicht indizierte Repositories (2026-01-10_01-02-03)\n"),
            "{rendered}"
        );
        assert!(
            rendered.contains(
                "- [ ] [c/three](https://github.com/c/three) – DeepWiki: https://deepwiki.com/c/three\n"
            ),
            "{rendered}"
        );
    }

    #[test]
    fn empty_sets_render_notes() {
        let report = RunReport::default();
        assert!(render_algos(&report).contains("Keine erfolgreichen Analysen"));
        assert!(render_missing(&report, "stamp").contains("Alle abgefragten"));
    }

    #[test]
    fn writes_both_files_to_dir() {
        let dir = std::env::temp_dir().join(format!("trending-test-{}", std::process::id()));
        fs::create_dir_all(&dir).expect("temp dir");
        let (algos, missing) =
            write_reports_with_stamp(&sample_report(), "2026-01-10_01-02-03", &dir).expect("write");
        assert_eq!(
            algos.file_name().expect("name").to_str(),
            Some("algos_2026-01-10_01-02-03.md")
        );
        assert_eq!(
            missing.file_name().expect("name").to_str(),
            Some("not-index-yet_2026-01-10_01-02-03.md")
        );
        assert!(
            fs::read_to_string(&algos)
                .expect("read")
                .contains("# a/one")
        );
        fs::remove_dir_all(&dir).expect("cleanup");
    }
}
