//! Parser: extrahiert `owner/repo`-Paare aus unstrukturiertem Trending-Text.

use std::collections::HashSet;

use crate::types::Repo;

/// Extrahiert alle Repositories in Feed-Reihenfolge; Duplikate entfallen.
///
/// Erkannt werden Vollzeilen `owner / repo` (Spaces um `/` optional) sowie
/// `github.com/owner/repo`-Links in längeren Zeilen. Alles andere wird ignoriert.
pub fn parse_repos(text: &str) -> Vec<Repo> {
    let mut seen: HashSet<String> = HashSet::new();
    let mut repos = Vec::new();

    for line in text.lines() {
        let found = parse_full_line(line).or_else(|| parse_github_url(line));
        if let Some(repo) = found {
            let key = repo.dedup_key();
            if seen.insert(key) {
                repos.push(repo);
            }
        }
    }
    repos
}

/// Matcht, wenn die gesamte (getrimmte) Zeile exakt `owner/repo` ist.
fn parse_full_line(line: &str) -> Option<Repo> {
    let trimmed = line.trim();
    if trimmed.is_empty() {
        return None;
    }
    let (left, right) = trimmed.split_once('/')?;
    if right.contains('/') {
        return None;
    }
    let owner = left.trim();
    let name = right.trim();
    if is_valid_name(owner) && is_valid_name(name) {
        Repo::new(owner, name)
    } else {
        None
    }
}

/// Fallback: extrahiert `owner/repo` aus `github.com/owner/repo…`-Links.
fn parse_github_url(line: &str) -> Option<Repo> {
    let lower = line.to_lowercase();
    let marker = "github.com/";
    let start = lower.find(marker)? + marker.len();
    let tail = line[start..].trim_start();
    let mut parts = tail.split('/');
    let owner = parts.next()?.trim();
    let raw_name = parts.next()?.trim();
    // Repo-Anteil endet an Pfad-, Query-, Fragment- oder Satzzeichen-Grenzen.
    let name: String = raw_name
        .chars()
        .take_while(|c| {
            !matches!(
                c,
                '?' | '#' | ' ' | '\t' | ')' | ']' | '"' | '\'' | ',' | ';'
            )
        })
        .collect();
    let name = name.trim();
    if is_valid_name(owner) && is_valid_name(name) {
        Repo::new(owner, name)
    } else {
        None
    }
}

/// Pragmatische GitHub-Namensregel: 1–100 Zeichen aus `[A-Za-z0-9._-]`,
/// beginnt/endet alphanumerisch, enthält mindestens einen Buchstaben/Ziffer.
fn is_valid_name(value: &str) -> bool {
    if value.len() > 100 {
        return false;
    }
    let mut chars = value.chars();
    let (Some(first), Some(last)) = (chars.next(), value.chars().next_back()) else {
        return false;
    };
    if !first.is_alphanumeric() || !last.is_alphanumeric() {
        return false;
    }
    value
        .chars()
        .all(|c| c.is_alphanumeric() || matches!(c, '-' | '_' | '.'))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn names(repos: &[Repo]) -> Vec<String> {
        repos.iter().map(Repo::full_name).collect()
    }

    #[test]
    fn parses_spaced_and_compact_forms() {
        let repos = parse_repos("max-sixty / worktrunk\nlongbridge/gpui-kit\n");
        assert_eq!(
            names(&repos),
            vec!["max-sixty/worktrunk", "longbridge/gpui-kit"]
        );
    }

    #[test]
    fn skips_noise_lines() {
        let text = "Trending\n\nRust 8,589 306 Built by @max-sixty\nTerms\n  \nFooter navigation\n";
        assert!(parse_repos(text).is_empty());
    }

    #[test]
    fn rejects_slash_prose_and_paths() {
        // Mehr als ein Slash oder Leerzeichen im Namen-Teil: kein Repo.
        assert!(parse_repos("apples/oranges are tasty\n").is_empty());
        assert!(parse_repos("a/b/c\n").is_empty());
        assert!(parse_repos("/leading\ntrailing/\n").is_empty());
        assert!(parse_repos("-bad / repo\nowner / bad-\n").is_empty());
    }

    #[test]
    fn dedups_case_insensitively_preserving_order() {
        let text = "NVIDIA / OpenShell\nnvidia/openshell\nrustfs / rustfs\nNVIDIA/OpenShell\n";
        assert_eq!(
            names(&parse_repos(text)),
            vec!["NVIDIA/OpenShell", "rustfs/rustfs"]
        );
    }

    #[test]
    fn accepts_dots_underscores_and_mixed_case() {
        let repos = parse_repos(
            "HakanSeven12 / OpenCADStudio\nmesamirh / MovieBox-Tui\nfoo.bar_baz / qux-1.2\n",
        );
        assert_eq!(
            names(&repos),
            vec![
                "HakanSeven12/OpenCADStudio",
                "mesamirh/MovieBox-Tui",
                "foo.bar_baz/qux-1.2"
            ]
        );
    }

    #[test]
    fn extracts_github_urls() {
        let text = "siehe https://github.com/rust-lang/rust für Details\n[x](https://github.com/serde-rs/serde?tab=x)\n";
        assert_eq!(
            names(&parse_repos(text)),
            vec!["rust-lang/rust", "serde-rs/serde"]
        );
    }

    #[test]
    fn parses_full_example_feed() {
        let text = include_str!("../plan/20260110_01_ask_deepwiki/example_input.txt");
        let repos = parse_repos(text);
        assert_eq!(repos.len(), 22, "Feed enthält 22 Repos");
        assert_eq!(
            repos.first().expect("first").full_name(),
            "max-sixty/worktrunk"
        );
        assert_eq!(repos.last().expect("last").full_name(), "rust-lang/rust");
    }
}
