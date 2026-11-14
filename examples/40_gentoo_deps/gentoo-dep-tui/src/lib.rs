//! gentoo-dep-tui — `ncdu` fuer Gentoo-USE-Flags.
//!
//! Nur Modul-Deklarationen, kein Verhalten.
//! Die Dateien folgen der Datenfluss-Reihenfolge (Clock-vor-Peripherie-Prinzip
//! aus `prompt.txt`, sinngemaess): Typen → Profil → Manifest → Binhost →
//! Repo-Index → Graph → Kaskade → Delta → CLI → TUI.

#[path = "00_perf.rs"]
pub mod mod_00_perf;
#[path = "01_types.rs"]
pub mod mod_01_types;
#[path = "02_profile.rs"]
pub mod mod_02_profile;
#[path = "03_manifest.rs"]
pub mod mod_03_manifest;
#[path = "04_binhost.rs"]
pub mod mod_04_binhost;
#[path = "05_repo_index.rs"]
pub mod mod_05_repo_index;
#[path = "06_graph.rs"]
pub mod mod_06_graph;
#[path = "07_cascade.rs"]
pub mod mod_07_cascade;
#[path = "08_delta.rs"]
pub mod mod_08_delta;
#[path = "11_cli.rs"]
pub mod mod_11_cli;
#[path = "12_tui_state.rs"]
pub mod mod_12_tui_state;
#[path = "13_tui_view.rs"]
pub mod mod_13_tui_view;
#[path = "14_tui_runner.rs"]
pub mod mod_14_tui_runner;
#[cfg(test)]
#[path = "test_helpers.rs"]
pub mod test_helpers;
