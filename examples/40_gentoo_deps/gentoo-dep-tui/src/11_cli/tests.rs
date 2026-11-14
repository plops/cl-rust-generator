use super::context::resolve_make_profile_from;

#[test]
fn make_profile_relative_symlink_resolves() {
    // `../../repo/profiles/...` enthaelt `..`-Segmente (Task 1.4).
    let d = tempfile::tempdir().unwrap();
    let profiles = d.path().join("repo").join("profiles");
    let target_dir = profiles.join("default/linux/amd64/23.0");
    std::fs::create_dir_all(&target_dir).unwrap();
    let link_dir = d.path().join("etc").join("portage");
    std::fs::create_dir_all(&link_dir).unwrap();
    std::os::unix::fs::symlink(
        "../../repo/profiles/default/linux/amd64/23.0",
        link_dir.join("make.profile"),
    )
    .unwrap();
    let got = resolve_make_profile_from(&link_dir.join("make.profile"), &profiles).unwrap();
    assert_eq!(got, "default/linux/amd64/23.0");
}
