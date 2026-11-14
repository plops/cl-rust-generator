use super::*;
use std::io::Write;

fn tmpdir() -> tempfile::TempDir {
    tempfile::tempdir().unwrap()
}

#[test]
fn world_skips_comments_and_blanks() {
    let d = tmpdir();
    let p = d.path().join("world");
    let mut f = std::fs::File::create(&p).unwrap();
    writeln!(f, "# kommentar\n\napp-misc/jq\n\n  dev-libs/openssl  ").unwrap();
    let w = read_world(&p).unwrap();
    assert_eq!(w, vec!["app-misc/jq", "dev-libs/openssl"]);
}

#[test]
fn key_value_parses_quoted_and_comment() {
    let kv = parse_key_value("USE=\"ssl -doc\" # kommentar\nARCH=amd64\n");
    assert_eq!(kv.get("USE").unwrap(), "ssl -doc");
    assert_eq!(kv.get("ARCH").unwrap(), "amd64");
}

#[test]
fn use_expand_seeds_effective_flags() {
    let d = tmpdir();
    let profiles = d.path().join("profiles");
    let base = profiles.join("base");
    std::fs::create_dir_all(&base).unwrap();
    std::fs::write(
        base.join("make.defaults"),
        "ARCH=amd64\nELIBC=glibc\nKERNEL=linux\nUSE_EXPAND=\"PYTHON_TARGETS ABI_X86\"\nPYTHON_TARGETS=\"python3_14\"\nABI_X86=\"64\"\n",
    )
    .unwrap();
    let prof = load_profile_stack(&base, &profiles).unwrap();
    let mut iuse = HashMap::new();
    for f in [
        "python_targets_python3_13",
        "python_targets_python3_14",
        "abi_x86_64",
        "ssl",
    ] {
        iuse.insert(f.to_string(), false);
    }
    let active = prof.apply_use_defaults(&iuse, "dev-python/jaraco-context");
    // Expand-Defaults greifen (nur fuer IUSE-Flags), Plattform immer.
    assert!(active.contains("python_targets_python3_14"));
    assert!(!active.contains("python_targets_python3_13"));
    assert!(active.contains("abi_x86_64"));
    assert!(active.contains("amd64"));
    assert!(active.contains("elibc_glibc"));
    assert!(active.contains("kernel_linux"));
    assert!(!active.contains("ssl"));
}

#[test]
fn arch_variable_expands_in_accept_keywords() {
    let d = tmpdir();
    let profiles = d.path().join("profiles");
    let base = profiles.join("base");
    std::fs::create_dir_all(&base).unwrap();
    std::fs::write(
        base.join("make.defaults"),
        "ARCH=amd64\nACCEPT_KEYWORDS=\"${ARCH}\"\n",
    )
    .unwrap();
    let prof = load_profile_stack(&base, &profiles).unwrap();
    assert_eq!(prof.arch, "amd64");
    assert!(prof.accept_keywords.contains("amd64"));
    assert!(!prof.accept_keywords.contains("${ARCH}"));
}

#[test]
fn profile_stack_inherits_parent() {
    let d = tmpdir();
    let profiles = d.path().join("profiles");
    let base = profiles.join("base");
    let child = profiles.join("default/linux/amd64/23.0");
    std::fs::create_dir_all(&child).unwrap();
    std::fs::create_dir_all(&base).unwrap();
    std::fs::write(base.join("use.mask"), "wayland\n").unwrap();
    std::fs::write(base.join("make.defaults"), "ARCH=amd64\n").unwrap();
    std::fs::write(child.join("parent"), "../../../../../base\n").unwrap();
    std::fs::write(child.join("use.mask"), "systemd\n").unwrap();
    let mut prof = load_profile_stack(&child, &profiles).unwrap();
    prof.use_mask = read_flag_list(&base.join("use.mask")).unwrap();
    let child_mask = read_flag_list(&child.join("use.mask")).unwrap();
    prof.use_mask.extend(child_mask);
    assert!(prof.use_mask.contains("wayland"));
    assert!(prof.use_mask.contains("systemd"));
}

#[test]
fn apply_use_defaults_order() {
    let mut p = Profile::test_default();
    p.global_use = ["ssl".into(), "-doc".into()].into_iter().collect();
    p.use_force = ["gui".into()].into_iter().collect();
    p.use_mask = ["ssl".into()].into_iter().collect();
    let mut iuse = HashMap::new();
    iuse.insert("doc".into(), true);
    iuse.insert("ssl".into(), false);
    iuse.insert("gui".into(), false);
    let active = p.apply_use_defaults(&iuse, "app-misc/jq");
    // Maske gewinnt ueber global USE.
    assert!(!active.contains("ssl"));
    assert!(active.contains("gui"));
    assert!(!active.contains("doc"));
    // Force ohne IUSE-Eintrag bleibt draussen (kein Phantom-Flag, review3).
    iuse.remove("gui");
    let active = p.apply_use_defaults(&iuse, "app-misc/jq");
    assert!(!active.contains("gui"));
}

#[test]
fn global_and_package_use_are_iuse_gated() {
    // `openmp`/`cet` global bzw. pro Paket, aber nicht in IUSE des Pakets:
    // duerfen nicht in den effektiven Flags landen (review3 Task 1.1).
    let mut p = Profile::test_default();
    p.global_use = ["openmp".into(), "ssl".into()].into_iter().collect();
    p.package_use
        .insert("app-misc/jq".into(), vec!["cet".into(), "-ssl".into()]);
    p.use_force = ["phantom".into()].into_iter().collect();
    p.sync_pattern_caches();
    let mut iuse = HashMap::new();
    iuse.insert("ssl".into(), false);
    let active = p.apply_use_defaults(&iuse, "app-misc/jq");
    assert!(!active.contains("openmp"));
    assert!(!active.contains("cet"));
    assert!(!active.contains("phantom"));
    // `package.use` schaltet deklarierte Flags weiterhin (`-ssl` greift).
    assert!(!active.contains("ssl"));
    // Plattform-Flag bleibt aktiv.
    assert!(active.contains("amd64"));
}

#[test]
fn use_expand_hidden_loads_from_make_defaults() {
    let d = tmpdir();
    let profiles = d.path().join("profiles");
    let base = profiles.join("base");
    std::fs::create_dir_all(&base).unwrap();
    std::fs::write(
        base.join("make.defaults"),
        "ARCH=amd64\nUSE_EXPAND_HIDDEN=\"ABI_X86 ELIBC\"\n",
    )
    .unwrap();
    let prof = load_profile_stack(&base, &profiles).unwrap();
    assert!(prof.use_expand_hidden.contains("ABI_X86"));
    assert!(prof.use_expand_hidden.contains("ELIBC"));
}

#[test]
fn use_mask_negation_unmasks_parent() {
    let d = tmpdir();
    let profiles = d.path().join("profiles");
    let base = profiles.join("base");
    let child = profiles.join("child");
    std::fs::create_dir_all(&base).unwrap();
    std::fs::create_dir_all(&child).unwrap();
    std::fs::write(base.join("use.mask"), "wayland\nsystemd\n").unwrap();
    std::fs::write(base.join("make.defaults"), "ARCH=amd64\n").unwrap();
    std::fs::write(child.join("parent"), "../base\n").unwrap();
    std::fs::write(child.join("use.mask"), "-wayland\n").unwrap();
    let prof = load_profile_stack(&child, &profiles).unwrap();
    // `-wayland` im Kind hebt die Eltern-Maske auf, `systemd` bleibt.
    assert!(!prof.use_mask.contains("wayland"));
    assert!(prof.use_mask.contains("systemd"));
}

#[test]
fn make_defaults_use_accumulates_with_negation() {
    let d = tmpdir();
    let profiles = d.path().join("profiles");
    let base = profiles.join("base");
    let child = profiles.join("child");
    std::fs::create_dir_all(&base).unwrap();
    std::fs::create_dir_all(&child).unwrap();
    std::fs::write(base.join("make.defaults"), "ARCH=amd64\nUSE=\"ssl doc\"\n").unwrap();
    std::fs::write(child.join("parent"), "../base\n").unwrap();
    std::fs::write(child.join("make.defaults"), "USE=\"${USE} -ssl ipv6\"\n").unwrap();
    let prof = load_profile_stack(&child, &profiles).unwrap();
    assert!(!prof.global_use.contains("ssl"));
    assert!(prof.global_use.contains("doc"));
    assert!(prof.global_use.contains("ipv6"));
}

#[test]
fn package_use_matches_version_and_slot() {
    let mut p = Profile::test_default();
    p.package_use.insert(
        ">=dev-libs/openssl-3.0".into(),
        vec!["tls-compression".into()],
    );
    p.package_use
        .insert("dev-libs/openssl:0".into(), vec!["ssl".into()]);
    p.sync_pattern_caches();
    // Version 3.0/Slot 0: beide Atome treffen.
    let tokens = p.package_use_for("dev-libs/openssl", "3.0", "0/0");
    assert!(tokens.contains(&"tls-compression".to_string()));
    assert!(tokens.contains(&"ssl".to_string()));
    // Version 1.1: nur das Slot-Atom trifft.
    let tokens = p.package_use_for("dev-libs/openssl", "1.1", "0/0");
    assert!(!tokens.contains(&"tls-compression".to_string()));
    assert!(tokens.contains(&"ssl".to_string()));
}

#[test]
fn glob_equal_is_pure_string_prefix() {
    // PMS 8.3.1 `=*`: reiner String-Prefix, `=1*` matcht auch `10`.
    let want = portage_atom::Version::parse("1.8").unwrap();
    let have = portage_atom::Version::parse("1.8.3").unwrap();
    assert!(version_matches(
        portage_atom::Operator::Equal,
        true,
        &want,
        &have,
        "1.8.3"
    ));
    let have_r1 = portage_atom::Version::parse("1.8.0-r1").unwrap();
    let want2 = portage_atom::Version::parse("1.8").unwrap();
    assert!(version_matches(
        portage_atom::Operator::Equal,
        true,
        &want2,
        &have_r1,
        "1.8.0-r1"
    ));
    // Ohne Glob kein Match (exakter Vergleich).
    assert!(!version_matches(
        portage_atom::Operator::Equal,
        false,
        &want,
        &have,
        "1.8.3"
    ));
}

#[test]
fn key_value_supports_multiline_and_continuation() {
    let kv = parse_key_value("USE=\"\n ssl\n -doc\n\"\nARCH=amd64\n");
    assert_eq!(kv.get("USE").unwrap(), "\n ssl\n -doc\n");
    let kv = parse_key_value("USE=\"ssl \\\n doc\" # kommentar\n");
    assert_eq!(kv.get("USE").unwrap(), "ssl   doc");
    let kv = parse_key_value("USE='a\nb'\n");
    assert_eq!(kv.get("USE").unwrap(), "a\nb");
}

#[test]
fn apply_use_tokens_is_incremental_and_deterministic() {
    let mut p = Profile::test_default();
    p.apply_use_tokens("ssl doc");
    // make.conf ohne ${USE}: Profil-USE bleibt, `-doc` loescht gezielt.
    p.apply_use_tokens("-doc ipv6");
    assert!(p.global_use.contains("ssl"));
    assert!(p.global_use.contains("ipv6"));
    assert!(!p.global_use.contains("doc"));
    assert!(!p.global_use.iter().any(|t| t.starts_with('-')));
    // `-*` leert alles vorherige.
    p.apply_use_tokens("-* qt5");
    assert_eq!(p.global_use.len(), 1);
    assert!(p.global_use.contains("qt5"));
}

#[test]
fn package_use_never_beats_mask_or_force() {
    let mut p = Profile::test_default();
    p.package_use.insert(
        "dev-libs/openssl".into(),
        vec!["masked_flag".into(), "-forced_flag".into()],
    );
    p.use_mask.insert("masked_flag".into());
    p.use_force.insert("forced_flag".into());
    p.sync_pattern_caches();
    let mut iuse = HashMap::new();
    iuse.insert("masked_flag".into(), false);
    iuse.insert("forced_flag".into(), false);
    let active = p.effective_defaults(&iuse, "dev-libs/openssl", "3.0", "0");
    assert!(!active.contains("masked_flag"));
    assert!(active.contains("forced_flag"));
}

#[test]
fn use_expand_known_without_defaults() {
    let d = tmpdir();
    let profiles = d.path().join("profiles");
    let base = profiles.join("base");
    std::fs::create_dir_all(&base).unwrap();
    std::fs::write(
        base.join("make.defaults"),
        "ARCH=amd64\nUSE_EXPAND=\"VIDEO_CARDS\"\n",
    )
    .unwrap();
    let prof = load_profile_stack(&base, &profiles).unwrap();
    assert!(prof.use_expand.contains("VIDEO_CARDS"));
}

#[test]
fn package_use_dir_reads_all_files() {
    let d = tmpdir();
    let dir = d.path().join("package.use");
    std::fs::create_dir(&dir).unwrap();
    std::fs::write(dir.join("00-base"), "dev-libs/openssl -ssl\n").unwrap();
    std::fs::write(dir.join("10-extra"), "# c\napp-misc/jq abbrev\n").unwrap();
    let pu = read_package_use(&dir).unwrap();
    assert_eq!(
        pu.get("dev-libs/openssl").unwrap(),
        &vec!["-ssl".to_string()]
    );
    assert_eq!(pu.get("app-misc/jq").unwrap(), &vec!["abbrev".to_string()]);
}

#[test]
fn package_unmask_line_negates_mask() {
    use super::stack::read_atom_lines;
    let d = tmpdir();
    std::fs::write(d.path().join("package.mask"), ">=dev-libs/openssl-99\n").unwrap();
    std::fs::write(d.path().join("package.unmask"), ">=dev-libs/openssl-99\n").unwrap();
    let mut extra: Vec<String> = read_atom_lines(&d.path().join("package.mask"));
    for line in read_atom_lines(&d.path().join("package.unmask")) {
        let atom = line.split_whitespace().next().unwrap_or("").to_string();
        if atom.starts_with('-') {
            extra.push(atom);
        } else {
            extra.push(format!("-{atom}"));
        }
    }
    assert_eq!(extra.last().unwrap().as_str(), "->=dev-libs/openssl-99");
    let mut p = Profile::test_default();
    p.package_mask = extra;
    p.sync_pattern_caches();
    let last = p.package_mask_index.last().unwrap();
    assert!(last.unmask);
    assert_eq!(last.cpn, "dev-libs/openssl");
}

#[test]
fn package_accept_keywords_parsed_with_default_testing() {
    let mut p = Profile::test_default();
    p.package_accept_keywords_raw = vec![
        "dev-libs/openssl ~amd64".to_string(),
        "app-misc/jq".to_string(),
    ];
    p.sync_pattern_caches();
    let rules = p.package_accept_keywords.get("dev-libs/openssl").unwrap();
    assert!(rules[0].1.contains("~amd64"));
    // Ohne Keywords auf der Zeile gilt ~arch.
    let jq = p.package_accept_keywords.get("app-misc/jq").unwrap();
    assert!(jq[0].1.contains("~amd64"));
}
