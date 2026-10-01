//! Kern-Typen: Repository-Adresse, Analyse-Ergebnisse, Lauf-Report.

use std::fmt;

/// Ein GitHub-Repository als `owner/name`-Paar (Original-Schreibweise bleibt erhalten).
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Repo {
    owner: String,
    name: String,
}

impl Repo {
    /// Erzeugt ein Repo; leere Teile werden abgelehnt.
    pub fn new(owner: &str, name: &str) -> Option<Self> {
        let owner = owner.trim();
        let name = name.trim();
        if owner.is_empty() || name.is_empty() {
            return None;
        }
        Some(Self {
            owner: owner.to_owned(),
            name: name.to_owned(),
        })
    }

    pub fn owner(&self) -> &str {
        &self.owner
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    /// `owner/name` für Logs, Prompts und API-Parameter.
    pub fn full_name(&self) -> String {
        format!("{self}")
    }

    /// Schlüssel für die Duplikat-Erkennung (GitHub-Namen sind case-insensitiv eindeutig).
    pub fn dedup_key(&self) -> String {
        format!("{self}").to_lowercase()
    }

    pub fn github_url(&self) -> String {
        format!("https://github.com/{self}")
    }

    pub fn deepwiki_url(&self) -> String {
        format!("https://deepwiki.com/{self}")
    }
}

impl fmt::Display for Repo {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}/{}", self.owner, self.name)
    }
}

/// Ergebnis der Analyse eines einzelnen Repositories.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AnalysisOutcome {
    /// DeepWiki lieferte eine Analyse (Markdown).
    Success { repo: Repo, markdown: String },
    /// Repository ist (noch) nicht bei DeepWiki indiziert.
    Missing { repo: Repo },
    /// Technischer Fehler (Netzwerk, Rate-Limit, Protokoll …); Lauf geht weiter.
    Failed { repo: Repo, reason: String },
}

impl AnalysisOutcome {
    pub fn repo(&self) -> &Repo {
        match self {
            Self::Success { repo, .. } | Self::Missing { repo } | Self::Failed { repo, .. } => repo,
        }
    }

    /// Kurzwort für das Fortschritts-Log: `OK`, `MISSING` oder `ERROR`.
    pub fn status_word(&self) -> &'static str {
        match self {
            Self::Success { .. } => "OK",
            Self::Missing { .. } => "MISSING",
            Self::Failed { .. } => "ERROR",
        }
    }
}

/// Sammelergebnis eines Laufs über alle Repositories.
#[derive(Debug, Default)]
pub struct RunReport {
    successes: Vec<(Repo, String)>,
    missing: Vec<Repo>,
    failed: Vec<(Repo, String)>,
}

impl RunReport {
    pub fn push(&mut self, outcome: AnalysisOutcome) {
        match outcome {
            AnalysisOutcome::Success { repo, markdown } => {
                self.successes.push((repo, markdown));
            }
            AnalysisOutcome::Missing { repo } => self.missing.push(repo),
            AnalysisOutcome::Failed { repo, reason } => self.failed.push((repo, reason)),
        }
    }

    pub fn successes(&self) -> &[(Repo, String)] {
        &self.successes
    }

    pub fn missing(&self) -> &[Repo] {
        &self.missing
    }

    pub fn failed(&self) -> &[(Repo, String)] {
        &self.failed
    }

    pub fn total(&self) -> usize {
        self.successes.len() + self.missing.len() + self.failed.len()
    }

    pub fn is_empty(&self) -> bool {
        self.total() == 0
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn repo_new_rejects_empty_parts() {
        assert!(Repo::new("", "repo").is_none());
        assert!(Repo::new("owner", "  ").is_none());
        assert!(Repo::new("  ", "  ").is_none());
    }

    #[test]
    fn repo_trims_and_formats() {
        let repo = Repo::new("  max-sixty ", " worktrunk ").expect("valid repo");
        assert_eq!(repo.owner(), "max-sixty");
        assert_eq!(repo.name(), "worktrunk");
        assert_eq!(repo.full_name(), "max-sixty/worktrunk");
        assert_eq!(repo.github_url(), "https://github.com/max-sixty/worktrunk");
        assert_eq!(
            repo.deepwiki_url(),
            "https://deepwiki.com/max-sixty/worktrunk"
        );
    }

    #[test]
    fn dedup_key_is_case_insensitive() {
        let a = Repo::new("NVIDIA", "OpenShell").expect("valid");
        let b = Repo::new("nvidia", "openshell").expect("valid");
        assert_eq!(a.dedup_key(), b.dedup_key());
    }

    #[test]
    fn outcome_status_words() {
        let repo = Repo::new("a", "b").expect("valid");
        assert_eq!(
            AnalysisOutcome::Success {
                repo: repo.clone(),
                markdown: String::new()
            }
            .status_word(),
            "OK"
        );
        assert_eq!(
            AnalysisOutcome::Missing { repo: repo.clone() }.status_word(),
            "MISSING"
        );
        assert_eq!(
            AnalysisOutcome::Failed {
                repo,
                reason: String::new()
            }
            .status_word(),
            "ERROR"
        );
    }

    #[test]
    fn report_counts_by_variant() {
        let mut report = RunReport::default();
        assert!(report.is_empty());
        let repo = Repo::new("a", "b").expect("valid");
        report.push(AnalysisOutcome::Success {
            repo: repo.clone(),
            markdown: "# x".to_owned(),
        });
        report.push(AnalysisOutcome::Missing { repo: repo.clone() });
        report.push(AnalysisOutcome::Failed {
            repo,
            reason: "boom".to_owned(),
        });
        assert_eq!(report.total(), 3);
        assert_eq!(report.successes().len(), 1);
        assert_eq!(report.missing().len(), 1);
        assert_eq!(report.failed().len(), 1);
    }
}
