//! 07_cascade/required_use — REQUIRED_USE-Auswerter und Optionsbauer.
//!
//! Genau eine Zustaendigkeit: Ausdrucksbaum gegen aktive Flags
//! auswerten, Aktionen und konkrete Quick-Switch-Changes liefern.

use std::collections::HashSet;

use portage_metadata::RequiredUseExpr;

use crate::mod_01_types::QuickSwitch;

use super::step::CascadeStep;

/// Erzwungene Einzelaktion aus `REQUIRED_USE`.
pub(super) enum ForceAction {
    /// Flag ein-/ausschalten.
    Set(String, bool),
    /// Unaufloesbar: Nachricht + Quick-Switch-Optionen.
    Conflict(String, Vec<QuickSwitch>),
}

impl<'e, 'a, 'u, 'f> CascadeStep<'e, 'a, 'u, 'f> {
    /// `REQUIRED_USE`-Baum gegen aktive Flags auswerten (sammelt Aktionen).
    ///
    /// `cond`: naechstes umgebendes `flag? (...)` — bei Widerspruch gegen ein
    /// verbotenes Flag zusaetzliche Option, die Bedingung abzuschalten.
    /// Maskierte oder user-gesperrte Flags werden nie automatisch gewaehlt.
    pub(super) fn enforce_expr(
        &self,
        expr: &RequiredUseExpr,
        active: &HashSet<String>,
        out: &mut Vec<ForceAction>,
        cpn: &str,
        cond: Option<&str>,
    ) {
        match expr {
            RequiredUseExpr::Flag { name, negated } => {
                let has = active.contains(name);
                if *negated {
                    if has {
                        let mut options = vec![QuickSwitch {
                            label: format!("{name} abschalten"),
                            changes: vec![(cpn.to_string(), name.clone(), false)],
                        }];
                        if let Some(c) = cond
                            && c != name
                        {
                            options.push(QuickSwitch {
                                label: format!("Bedingung {c} abschalten"),
                                changes: vec![(cpn.to_string(), c.to_string(), false)],
                            });
                        }
                        out.push(ForceAction::Conflict(
                            format!("REQUIRED_USE verbietet {name}, ist aber an"),
                            options,
                        ));
                    }
                } else if !has {
                    out.push(ForceAction::Set(name.clone(), true));
                }
            }
            RequiredUseExpr::UseConditional {
                flag,
                negated,
                entries,
            } => {
                if active.contains(flag) != *negated {
                    for child in entries {
                        self.enforce_expr(child, active, out, cpn, Some(flag));
                    }
                }
            }
            RequiredUseExpr::All(entries) => {
                for child in entries {
                    self.enforce_expr(child, active, out, cpn, cond);
                }
            }
            RequiredUseExpr::AnyOf(children) => {
                if !children.iter().any(|c| satisfied(c, active)) {
                    if let Some((flag, want)) = children
                        .iter()
                        .filter_map(|c| self.viable_pick(cpn, c, active))
                        .next()
                    {
                        out.push(ForceAction::Set(flag, want));
                    } else {
                        out.push(ForceAction::Conflict(
                            "REQUIRED_USE-|| unerfuellt, keine Option einschaltbar".into(),
                            children
                                .iter()
                                .map(|c| QuickSwitch {
                                    label: format!("erfuellen: {}", describe(c)),
                                    changes: satisfy_changes(c, active, cpn),
                                })
                                .collect(),
                        ));
                    }
                }
            }
            RequiredUseExpr::ExactlyOne(children) => {
                let n = children.iter().filter(|c| satisfied(c, active)).count();
                if n == 0 {
                    self.enforce_expr(
                        &RequiredUseExpr::AnyOf(children.clone()),
                        active,
                        out,
                        cpn,
                        cond,
                    );
                } else if n > 1 {
                    out.push(ForceAction::Conflict(
                        "REQUIRED_USE-^^ mehrfach erfuellt (genau eines erlaubt)".into(),
                        self.keep_only_options(children, active, cpn),
                    ));
                }
            }
            RequiredUseExpr::AtMostOne(children) => {
                let n = children.iter().filter(|c| satisfied(c, active)).count();
                if n > 1 {
                    out.push(ForceAction::Conflict(
                        "REQUIRED_USE-?? mehrfach erfuellt (hoechstens eines erlaubt)".into(),
                        self.keep_only_options(children, active, cpn),
                    ));
                }
            }
        }
    }

    /// Erster realisierbarer `||`-Kandidat: nicht profil-maskiert/erzwungen,
    /// nicht gegen User-Intent; negierte Zweige via Abschalten.
    fn viable_pick(
        &self,
        cpn: &str,
        expr: &RequiredUseExpr,
        active: &HashSet<String>,
    ) -> Option<(String, bool)> {
        match expr {
            RequiredUseExpr::Flag { name, negated } => {
                if *negated {
                    if active.contains(name.as_str())
                        && self.user_value(cpn, name) != Some(true)
                        && !self.engine.profile.is_forced(name)
                    {
                        Some((name.clone(), false))
                    } else {
                        None
                    }
                } else if !active.contains(name.as_str())
                    && !self.engine.profile.is_masked(name)
                    && self.user_value(cpn, name) != Some(false)
                {
                    Some((name.clone(), true))
                } else {
                    None
                }
            }
            RequiredUseExpr::UseConditional {
                flag,
                negated,
                entries,
            } => {
                if active.contains(flag) != *negated {
                    entries
                        .iter()
                        .filter_map(|c| self.viable_pick(cpn, c, active))
                        .next()
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    /// `^^`/`??`-Optionen mit echten Aenderungen: je aktuell anstehendem
    /// einfachem Flag eine Option, die alle anderen abschaltet (Task 2.4).
    fn keep_only_options(
        &self,
        children: &[RequiredUseExpr],
        active: &HashSet<String>,
        cpn: &str,
    ) -> Vec<QuickSwitch> {
        let mut on_flags: Vec<String> = Vec::new();
        for c in children {
            collect_on_simple(c, active, &mut on_flags);
        }
        on_flags.sort();
        on_flags.dedup();
        let mut options: Vec<QuickSwitch> = on_flags
            .iter()
            .map(|keep| QuickSwitch {
                label: format!("nur behalten: {keep}"),
                changes: on_flags
                    .iter()
                    .filter(|f| *f != keep)
                    .map(|f| (cpn.to_string(), f.clone(), false))
                    .collect(),
            })
            .filter(|o: &QuickSwitch| !o.changes.is_empty())
            .collect();
        if options.is_empty() {
            // Degeneriert (nur komplexe Zweige): erfuellte Zweige gezielt
            // brechen statt leere Changes zu emittieren.
            options = children
                .iter()
                .filter(|c| satisfied(c, active))
                .map(|c| QuickSwitch {
                    label: format!("nur behalten (komplex): {}", describe(c)),
                    changes: break_changes(c, active, cpn),
                })
                .filter(|o: &QuickSwitch| !o.changes.is_empty())
                .collect();
        }
        options
    }
}

/// Konkrete Aenderungen, die `expr` UNerfuellt machen (fuer `^^`/`??`-Fallback).
fn break_changes(
    expr: &RequiredUseExpr,
    active: &HashSet<String>,
    cpn: &str,
) -> Vec<(String, String, bool)> {
    match expr {
        RequiredUseExpr::Flag { name, negated } => {
            if active.contains(name.as_str()) == !negated {
                vec![(cpn.to_string(), name.clone(), *negated)]
            } else {
                Vec::new()
            }
        }
        RequiredUseExpr::UseConditional {
            flag,
            negated,
            entries,
        } => {
            if active.contains(flag) != *negated {
                entries
                    .iter()
                    .map(|c| break_changes(c, active, cpn))
                    .find(|v| !v.is_empty())
                    .unwrap_or_default()
            } else {
                Vec::new()
            }
        }
        RequiredUseExpr::All(entries) => entries
            .iter()
            .map(|c| break_changes(c, active, cpn))
            .find(|v| !v.is_empty())
            .unwrap_or_default(),
        RequiredUseExpr::AnyOf(children)
        | RequiredUseExpr::ExactlyOne(children)
        | RequiredUseExpr::AtMostOne(children) => children
            .iter()
            .filter(|c| satisfied(c, active))
            .flat_map(|c| break_changes(c, active, cpn))
            .collect(),
    }
}

/// Einfache aktuell anstehende Flags einsammeln (fuer `^^`/`??`-Optionen).
fn collect_on_simple(expr: &RequiredUseExpr, active: &HashSet<String>, out: &mut Vec<String>) {
    match expr {
        RequiredUseExpr::Flag { name, negated } => {
            if !negated && active.contains(name.as_str()) {
                out.push(name.clone());
            }
        }
        RequiredUseExpr::UseConditional {
            flag,
            negated,
            entries,
        } => {
            if active.contains(flag) != *negated {
                for c in entries {
                    collect_on_simple(c, active, out);
                }
            }
        }
        RequiredUseExpr::All(entries) => {
            for c in entries {
                collect_on_simple(c, active, out);
            }
        }
        RequiredUseExpr::AnyOf(children)
        | RequiredUseExpr::ExactlyOne(children)
        | RequiredUseExpr::AtMostOne(children) => {
            for c in children {
                if satisfied(c, active) {
                    collect_on_simple(c, active, out);
                }
            }
        }
    }
}

/// Konkrete Aenderungen, die `expr` erfuellen (leer = bereits erfuellt).
fn satisfy_changes(
    expr: &RequiredUseExpr,
    active: &HashSet<String>,
    cpn: &str,
) -> Vec<(String, String, bool)> {
    match expr {
        RequiredUseExpr::Flag { name, negated } => {
            if active.contains(name.as_str()) == !negated {
                Vec::new()
            } else {
                vec![(cpn.to_string(), name.clone(), !negated)]
            }
        }
        RequiredUseExpr::UseConditional {
            flag,
            negated,
            entries,
        } => {
            if active.contains(flag) != *negated {
                entries
                    .iter()
                    .flat_map(|c| satisfy_changes(c, active, cpn))
                    .collect()
            } else {
                Vec::new()
            }
        }
        RequiredUseExpr::All(entries) => entries
            .iter()
            .flat_map(|c| satisfy_changes(c, active, cpn))
            .collect(),
        RequiredUseExpr::AnyOf(children)
        | RequiredUseExpr::ExactlyOne(children)
        | RequiredUseExpr::AtMostOne(children) => children
            .iter()
            .map(|c| satisfy_changes(c, active, cpn))
            .find(|v| !v.is_empty())
            .unwrap_or_default(),
    }
}

/// Ob ein `REQUIRED_USE`-Teilbaum aktuell erfuellt ist.
fn satisfied(expr: &RequiredUseExpr, active: &HashSet<String>) -> bool {
    match expr {
        RequiredUseExpr::Flag { name, negated } => active.contains(name) != *negated,
        RequiredUseExpr::UseConditional {
            flag,
            negated,
            entries,
        } => {
            if active.contains(flag) != *negated {
                entries.iter().all(|c| satisfied(c, active))
            } else {
                true
            }
        }
        RequiredUseExpr::All(entries) => entries.iter().all(|c| satisfied(c, active)),
        RequiredUseExpr::AnyOf(children) => children.iter().any(|c| satisfied(c, active)),
        RequiredUseExpr::ExactlyOne(children) => {
            children.iter().filter(|c| satisfied(c, active)).count() == 1
        }
        RequiredUseExpr::AtMostOne(children) => {
            children.iter().filter(|c| satisfied(c, active)).count() <= 1
        }
    }
}

/// Kurzbeschreibung eines Ausdrucks (fuer Optionen).
fn describe(expr: &RequiredUseExpr) -> String {
    match expr {
        RequiredUseExpr::Flag { name, negated } => {
            format!("{}{}", if *negated { "!" } else { "" }, name)
        }
        RequiredUseExpr::AnyOf(_) => "|| (...)".into(),
        RequiredUseExpr::ExactlyOne(_) => "^^ (...)".into(),
        RequiredUseExpr::AtMostOne(_) => "?? (...)".into(),
        RequiredUseExpr::UseConditional { flag, negated, .. } => {
            format!("{}{}? (...)", if *negated { "!" } else { "" }, flag)
        }
        RequiredUseExpr::All(_) => "(...)".into(),
    }
}
