//! 07_cascade — `REQUIRED_USE`- und `pkg[flag]`-Kaskaden mit Konflikterkennung.
//!
//! Genau eine Zustaendigkeit: erzwungene USE-Flags per Fixpunkt fortpflanzen,
//! Widersprueche als `ConflictInfo` mit Quick-Switch-Kandidaten melden.
//! Kaskaden aendern die Aufloesung → Rueckkopplung in `resolve` (max. Runden).

use std::collections::HashSet;

use crate::mod_01_types::{ConflictInfo, EngineResult};
use crate::mod_06_graph::{Engine, ResolvedGraph, UseOverrides};

use step::CascadeStep;

/// Maximalrunden des Kaskaden-Fixpunkts (danach: Konflikt statt Endlosschleife).
const MAX_ROUNDS: u8 = 8;

/// Ergebnis einer Kaskaden-Aufloesung.
#[derive(Debug, Clone, Default)]
pub struct CascadeOutcome {
    /// Finaler Graph nach Fixpunkt.
    pub graph: ResolvedGraph,
    /// Irreduzible Konflikte der Finalrunde (leere Liste = konsistent).
    pub conflicts: Vec<ConflictInfo>,
    /// Benoetigte Fixpunkt-Runden.
    pub rounds: u8,
    /// Effektive Overrides: User-Intents + Kaskaden-Forces. Der uebergebene
    /// `engine.overrides` bleibt unveraendert (nur User-Intents); der finale
    /// aktive USE-Stand steht zusaetzlich in `graph.nodes[*].use_flags`.
    pub effective_overrides: UseOverrides,
}

/// Welt + Kaskaden vollstaendig aufloesen (Fixpunkt aus Resolve ↔ Enforce).
pub fn resolve_with_cascade(
    engine: &mut Engine<'_>,
    world: &[String],
) -> EngineResult<CascadeOutcome> {
    resolve_with_cascade_opts(engine, world, true, true)
}

/// Wie `resolve_with_cascade`, aber `include_build` und Blame-Sammlung
/// steuerbar (Simulationen: `false, false`, review3 Task 3.3).
pub fn resolve_with_cascade_opts(
    engine: &mut Engine<'_>,
    world: &[String],
    include_build: bool,
    collect_blame: bool,
) -> EngineResult<CascadeOutcome> {
    crate::mod_00_perf::perf_count_cascade();
    let _timer = crate::mod_00_perf::ScopeTimer::cascade();
    // User-Intents klonen und auf einer Arbeitskopie (`sim`) kaskadieren:
    // `engine.overrides` bleibt unveraendert (nur explizite User-Intents),
    // Kaskaden-Forces landen in `sim.overrides` (`effective_overrides`).
    let user = engine.overrides.clone();
    let mut sim = engine.fork();
    sim.overrides = user.clone();
    let mut forced: HashSet<(String, String)> = HashSet::new();
    let mut conflicts: Vec<ConflictInfo>;
    let mut rounds = 0u8;
    loop {
        let graph =
            crate::mod_06_graph::resolve_with_options(&sim, world, include_build, collect_blame)?;
        // Schritt-Scope: `step` leiht `sim` mutabel, danach ist es frei.
        // `forced` ueberlebt Runden (Ping-Pong-Schutz bei harten Widerspruechen).
        let (changed, step_conflicts) = {
            let mut step = CascadeStep::new(&mut sim, &user, &mut forced);
            step.enforce_graph(&graph);
            (step.changed, step.conflicts)
        };
        rounds += 1;
        crate::mod_00_perf::perf_count_cascade_round();
        // Nur ungeloeste Konflikte der Finalrunde berichten (transiente
        // Zwischenstaende konvergieren weg und sind keine User-Befunde).
        conflicts = step_conflicts;
        if conflicts.is_empty() && !changed {
            return Ok(CascadeOutcome {
                graph,
                conflicts,
                rounds,
                effective_overrides: sim.overrides.clone(),
            });
        }
        if !changed {
            // Overrides unveraendert: der eben aufgeloeste Graph ist final —
            // kein redundanter Extra-`resolve` (review3 Task 3.3).
            return Ok(CascadeOutcome {
                graph,
                conflicts,
                rounds,
                effective_overrides: sim.overrides.clone(),
            });
        }
        if rounds >= MAX_ROUNDS {
            let graph = crate::mod_06_graph::resolve_with_options(
                &sim,
                world,
                include_build,
                collect_blame,
            )?;
            let mut out_conflicts = conflicts;
            out_conflicts.push(ConflictInfo {
                package: "(kaskade)".into(),
                message: format!(
                    "Fixpunkt konvergiert nicht nach {rounds} Runden (oszillierende Constraints)"
                ),
                caused_by: Vec::new(),
                options: vec![],
            });
            return Ok(CascadeOutcome {
                graph,
                conflicts: out_conflicts,
                rounds,
                effective_overrides: sim.overrides.clone(),
            });
        }
    }
}

#[path = "07_cascade/enforce.rs"]
mod enforce;
#[path = "07_cascade/required_use.rs"]
mod required_use;
#[path = "07_cascade/step.rs"]
mod step;
#[cfg(test)]
#[path = "07_cascade/tests.rs"]
mod tests;
