//! live-relay Bibliothek: Module fuer Tests und `main.rs`.
//! Nur Deklarationen; Verhalten in `01_config` … `04_serve`.

#[path = "01_config.rs"]
pub mod config;
#[path = "03_relay.rs"]
pub mod relay;
#[path = "04_serve.rs"]
pub mod serve;
#[path = "02_state.rs"]
pub mod state;
