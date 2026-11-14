//! 06_graph/blame — Blame-Ketten von `world` bis Ziel.
//!
//! Genau eine Zustaendigkeit: Tiefensuche ueber Rueckwaertskanten,
//! Welt-Pakete als gueltige Wurzeln (auch bei Zyklen).

use std::collections::HashSet;

use portage_atom::Dep;

use crate::mod_01_types::BlameLink;

use super::ResolvedGraph;

impl ResolvedGraph {
    /// Alle Blame-Ketten von `world` bis `target` (Tiefensuche).
    ///
    /// Pfad-Konvention: `path[0]` ist das Ziel, das Ende ein Welt-Paket.
    /// Glied `i` bedeutet: `path[i]` wird von `path[i+1]` gebraucht.
    /// Welt-Pakete sind gueltige Wurzeln (auch bei Zyklen zwischen ihnen,
    /// Task 2.2) — statt `[]` liefert ein Welt-Zyklus echte Ketten.
    pub fn blame(&self, target: &str) -> Vec<Vec<BlameLink>> {
        let world: HashSet<String> = self.world.iter().map(|w| world_cpn(w)).collect();
        let mut chains = Vec::new();
        let mut path = vec![target.to_string()];
        self.blame_dfs(&world, target, &mut path, &mut chains, 0);
        chains
    }

    fn blame_dfs(
        &self,
        world: &HashSet<String>,
        node: &str,
        path: &mut Vec<String>,
        chains: &mut Vec<Vec<BlameLink>>,
        depth: usize,
    ) {
        if depth > 32 || chains.len() >= 16 {
            return;
        }
        match self.reverse.get(node) {
            None => {
                // Wurzel erreicht (keine Eltern): Kette schliessen.
                Self::close_chain(self, path, chains);
            }
            Some(parents) => {
                // Welt-Paket mit geschlossenem Pfad: gueltige Wurzel, nicht
                // weitersteigen (bricht Welt-Zyklen statt `[]` zu liefern).
                if world.contains(node) && path.len() > 1 {
                    Self::close_chain(self, path, chains);
                    return;
                }
                for (parent, _) in parents {
                    if path.contains(parent) {
                        continue; // Zyklenfest.
                    }
                    path.push(parent.clone());
                    self.blame_dfs(world, parent, path, chains, depth + 1);
                    path.pop();
                }
            }
        }
    }

    /// Pfad in `BlameLink`-Kette ueberfuehren.
    fn close_chain(graph: &ResolvedGraph, path: &[String], chains: &mut Vec<Vec<BlameLink>>) {
        let mut links = Vec::new();
        for w in path.windows(2) {
            let via = graph
                .reverse
                .get(&w[0])
                .and_then(|ps| ps.iter().find(|(p, _)| *p == w[1]))
                .and_then(|(_, v)| v.clone());
            links.push(BlameLink {
                package: w[0].clone(),
                required_by: w[1].clone(),
                via_flag: via.filter(|v| !v.is_empty()),
            });
        }
        chains.push(links);
    }
}

/// Welt-Atom auf `cat/pkg` kuerzen (fuer Blame-Wurzeln, Task 2.2).
fn world_cpn(atom: &str) -> String {
    if let Ok(dep) = Dep::parse(atom) {
        return dep.cpn.to_string();
    }
    atom.split(':').next().unwrap_or(atom).to_string()
}
