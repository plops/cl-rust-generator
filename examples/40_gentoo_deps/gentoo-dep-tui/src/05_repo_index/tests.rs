use super::*;
use std::io::Write;

fn test_profile() -> Profile {
    Profile::test_default()
}

fn write_cache(repo: &Path, cat: &str, pf: &str, body: &str) {
    // Echtes Layout: `<kat>/<paket-version>` (zweistufig, kein pkg-Dir).
    let dir = repo.join("metadata").join("md5-cache").join(cat);
    std::fs::create_dir_all(&dir).unwrap();
    let mut f = std::fs::File::create(dir.join(pf)).unwrap();
    f.write_all(body.as_bytes()).unwrap();
}

const ENTRY: &str = "\
EAPI=8\n\
DESCRIPTION=test\n\
SLOT=0\n\
KEYWORDS=amd64\n\
IUSE=ssl\n\
RDEPEND=dev-libs/openssl\n";

#[test]
fn selects_highest_stable_and_skips_live() {
    let d = tempfile::tempdir().unwrap();
    let repo = d.path();
    write_cache(repo, "app-misc", "jq-1.7.1", ENTRY);
    write_cache(repo, "app-misc", "jq-1.8.0", ENTRY);
    write_cache(repo, "app-misc", "jq-9999", ENTRY);
    let idx = RepoIndex::load(repo, None, &test_profile()).unwrap();
    assert_eq!(idx.cache_files, 3);
    let sel = idx.select("app-misc/jq").unwrap();
    assert_eq!(sel.version_str, "1.8.0");
}

#[test]
fn mask_and_keywords_filter() {
    let d = tempfile::tempdir().unwrap();
    let repo = d.path();
    write_cache(repo, "app-misc", "jq-1.8.0", ENTRY);
    let unstable = ENTRY.replace("KEYWORDS=amd64", "KEYWORDS=~amd64");
    write_cache(repo, "app-misc", "jq-1.9.0", &unstable);
    let mut prof = test_profile();
    prof.package_mask = vec!["=app-misc/jq-1.8.0".into()];
    let idx = RepoIndex::load(repo, None, &prof).unwrap();
    // 1.9.0 Testing ohne ~amd64 raus, 1.8.0 maskiert → nichts waehlbar.
    assert!(idx.select("app-misc/jq").is_none());
    prof.accept_keywords.insert("~amd64".into());
    prof.package_mask.clear();
    let idx = RepoIndex::load(repo, None, &prof).unwrap();
    assert_eq!(idx.select("app-misc/jq").unwrap().version_str, "1.9.0");
}

#[test]
fn dash_unmask_reverts_earlier_mask() {
    // base maskiert, spaeteres `-atom` (Linux-Profil) entmaskiert wieder.
    let d = tempfile::tempdir().unwrap();
    let repo = d.path();
    write_cache(
        repo,
        "sys-libs",
        "libseccomp-2.6.1",
        &ENTRY.replace("IUSE=ssl", "IUSE="),
    );
    let mut prof = test_profile();
    prof.package_mask = vec!["sys-libs/libseccomp".into(), "-sys-libs/libseccomp".into()];
    let idx = RepoIndex::load(repo, None, &prof).unwrap();
    assert!(idx.select("sys-libs/libseccomp").is_some());
    prof.package_mask = vec!["sys-libs/libseccomp".into()];
    let idx = RepoIndex::load(repo, None, &prof).unwrap();
    assert!(idx.select("sys-libs/libseccomp").is_none());
}

#[test]
fn slot_subslot_matches_exactly() {
    // `:0/1` maskiert nur Sub-Slot 1, nicht `0/2`.
    let d = tempfile::tempdir().unwrap();
    let repo = d.path();
    let slot2 = ENTRY.replace("SLOT=0\n", "SLOT=0/2\n");
    let slot1 = ENTRY.replace("SLOT=0\n", "SLOT=0/1\n");
    write_cache(repo, "virtual", "libcrypt-2", &slot2);
    write_cache(repo, "virtual", "libcrypt-1-r2", &slot1);
    let mut prof = test_profile();
    prof.package_mask = vec!["virtual/libcrypt:0/1".into()];
    let idx = RepoIndex::load(repo, None, &prof).unwrap();
    let sel = idx.select("virtual/libcrypt").unwrap();
    assert_eq!(sel.version_str, "2");
}

#[test]
fn version_operators_match() {
    use crate::mod_02_profile::version_matches;
    use portage_atom::Operator;
    let v = |s: &str| portage_atom::Version::parse(s).unwrap();
    let want = v("1.8.0");
    assert!(version_matches(
        Operator::GreaterOrEqual,
        false,
        &want,
        &v("1.9.0"),
        "1.9.0"
    ));
    assert!(!version_matches(
        Operator::Greater,
        false,
        &want,
        &v("1.8.0"),
        "1.8.0"
    ));
    assert!(version_matches(
        Operator::Equal,
        true,
        &v("1.8"),
        &v("1.8.3"),
        "1.8.3"
    ));
    assert!(version_matches(
        Operator::Approximate,
        false,
        &v("1.8.0"),
        &v("1.8.0-r1"),
        "1.8.0-r1"
    ));
}

#[test]
fn empty_keywords_are_filtered() {
    // `KEYWORDS=""` (unkeyworded) darf nie still akzeptiert werden.
    let d = tempfile::tempdir().unwrap();
    let repo = d.path();
    let no_kw = ENTRY.replace("KEYWORDS=amd64", "KEYWORDS=");
    write_cache(repo, "app-misc", "jq-1.8.0", &no_kw);
    let idx = RepoIndex::load(repo, None, &test_profile()).unwrap();
    assert!(idx.select("app-misc/jq").is_none());
}

#[test]
fn select_cpv_picks_exact_version() {
    let d = tempfile::tempdir().unwrap();
    let repo = d.path();
    write_cache(repo, "app-misc", "jq-1.7.1", ENTRY);
    write_cache(repo, "app-misc", "jq-1.8.0", ENTRY);
    let idx = RepoIndex::load(repo, None, &test_profile()).unwrap();
    let exact = idx.select_cpv("app-misc/jq", "app-misc/jq-1.7.1").unwrap();
    assert_eq!(exact.version_str, "1.7.1");
    // Leeres CPV = neueste Version (Fallback nur hier).
    assert_eq!(
        idx.select_cpv("app-misc/jq", "").unwrap().version_str,
        "1.8.0"
    );
}
