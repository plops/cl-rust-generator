//! Simulationskonfiguration, Defaults, CLI und Dam-Break-Initialisierung.

use crate::types::{GridMeta, Particle, SphParams};

/// Vorgabe-Konfiguration (Feature-Set 1 aus der Aufgabenstellung).
#[derive(Clone, Debug)]
pub struct SimConfig {
    /// Partikelanzahl, geklemmt auf 2_048–262_144.
    pub particles: usize,
    /// Ruhedichte ρ₀ in kg/m³.
    pub rest_density: f32,
    /// Glättungslänge h in m.
    pub h: f32,
    /// Gas-Steifigkeit k.
    pub stiffness: f32,
    /// Viskosität μ in Pa·s.
    pub viscosity: f32,
    /// Gravitationsbetrag in m/s².
    pub gravity: f32,
    /// Zeitschritt dt in s.
    pub dt: f32,
    /// Physikschritte pro Render-Frame (GUI).
    pub substeps: u32,
    /// Domänenbreite in m.
    pub domain_w: f32,
    /// Domänenhöhe in m.
    pub domain_h: f32,
    /// Wand-Reflexionsdämpfung.
    pub wall_damping: f32,
}

impl Default for SimConfig {
    fn default() -> Self {
        Self {
            particles: 16_384,
            rest_density: 1000.0,
            h: 0.04,
            stiffness: 2000.0,
            viscosity: 0.1,
            gravity: 9.81,
            dt: 0.0008,
            substeps: 3,
            domain_w: 1.6,
            domain_h: 1.0,
            wall_damping: 0.5,
        }
    }
}

impl SimConfig {
    /// Engt die Partikelanzahl auf den unterstützten Bereich ein.
    pub fn clamp_particles(&mut self) {
        self.particles = self.particles.clamp(2_048, 262_144);
    }

    /// Anfangsabstand der Dam-Break-Platzierung in m.
    ///
    /// Der Damm füllt links 45 % der Breite und 85 % der Höhe; der Abstand
    /// folgt aus Fläche/N, die Masse aus ρ₀ × Abstand² (2D).
    pub fn initial_spacing(&self) -> f32 {
        let area = 0.45 * self.domain_w * 0.85 * self.domain_h;
        (area / self.particles as f32).sqrt()
    }

    /// Partikelmasse in kg aus Ruhedichte und Anfangsabstand.
    pub fn mass(&self) -> f32 {
        let s = self.initial_spacing();
        self.rest_density * s * s
    }

    /// Zugehöriges Uniform Grid (Zellgröße = h).
    pub fn grid_meta(&self) -> GridMeta {
        GridMeta::new(self.domain_w, self.domain_h, self.h)
    }

    /// Flache Kernel-Parameter für GPU- und CPU-Backend.
    pub fn sph_params(&self) -> SphParams {
        let grid = self.grid_meta();
        SphParams {
            mass: self.mass(),
            h: self.h,
            rest_density: self.rest_density,
            stiffness: self.stiffness,
            viscosity: self.viscosity,
            dt: self.dt,
            gravity: self.gravity,
            wall_damping: self.wall_damping,
            num_particles: self.particles as u32,
            grid_w: grid.w,
            grid_h: grid.h,
            domain_w: self.domain_w,
            domain_h: self.domain_h,
        }
    }

    /// Dam-Break-Anfangszustand: ruhender Partikelblock links, stürzt ein.
    ///
    /// Zeilenweises Gitter mit `initial_spacing`, Start an der linken Wand
    /// mit einer halben Zelle Abstand; exakt `particles` Einträge.
    pub fn dam_break(&self) -> Vec<Particle> {
        let s = self.initial_spacing();
        let x0 = 0.5 * self.h;
        let y0 = 0.5 * self.h;
        let block_w = 0.45 * self.domain_w;
        let cols = ((block_w - x0) / s).floor() as usize + 1;
        let cols = cols.max(1);
        let mut out = Vec::with_capacity(self.particles);
        let mut row = 0;
        while out.len() < self.particles {
            let y = y0 + row as f32 * s;
            debug_assert!(y < self.domain_h, "Damm höher als Domäne");
            for col in 0..cols {
                if out.len() >= self.particles {
                    break;
                }
                // Halbversatz pro Zeile gegen perfekte Gitter-Artefakte.
                let stagger = if row % 2 == 1 { 0.5 * s } else { 0.0 };
                let x = (x0 + col as f32 * s + stagger).min(block_w);
                out.push(Particle::at_rest([x, y], self.rest_density));
            }
            row += 1;
        }
        out
    }
}

/// Kommandozeile: `sph [--headless --steps N --particles N --substeps N
/// --bench --cpu]`.
#[derive(Clone, Debug)]
pub struct Cli {
    /// Ohne Fenster rechnen (Validierung/Benchmark).
    pub headless: bool,
    /// Physikschritte im Headless-Modus (Default 500).
    pub steps: usize,
    /// Partikelanzahl (Default 16_384).
    pub particles: usize,
    /// Sub-Steps pro Frame (nur GUI, Default 3).
    pub substeps: u32,
    /// Durchsatz-Tabelle statt Kurzbericht (nur headless).
    pub bench: bool,
    /// CPU-Backend erzwingen (Debug/Fallback).
    pub cpu: bool,
    /// Override Glättungslänge h in m (Default 0.04, = Zellgröße).
    pub h: Option<f32>,
    /// Override Gas-Steifigkeit k (Default 2000).
    pub stiffness: Option<f32>,
    /// Override Zeitschritt dt in s (Default 0.0008).
    pub dt: Option<f32>,
    /// GUI nach N Frames beenden (Smoke-Test, Default: unbegrenzt).
    pub frames: Option<u64>,
}

impl Default for Cli {
    fn default() -> Self {
        Self {
            headless: false,
            steps: 500,
            particles: 16_384,
            substeps: 3,
            bench: false,
            cpu: false,
            h: None,
            stiffness: None,
            dt: None,
            frames: None,
        }
    }
}

impl Cli {
    /// Parst `std::env::args`, druckt Hilfe/Fehler und terminiert ggf.
    pub fn parse_args() -> Self {
        match Self::parse_from(std::env::args().skip(1)) {
            Ok(cli) => cli,
            Err(msg) => {
                eprintln!("{msg}");
                std::process::exit(2);
            }
        }
    }

    /// Testbare Variante: parst beliebige Argument-Iterables.
    pub fn parse_from<I, S>(args: I) -> Result<Self, String>
    where
        I: IntoIterator<Item = S>,
        S: Into<std::ffi::OsString>,
    {
        use lexopt::ValueExt as _;
        // lexopt deutet das erste Element als Programmnamen: Dummy voranstellen.
        let mut full = vec![std::ffi::OsString::from("sph")];
        full.extend(args.into_iter().map(Into::into));
        let mut cli = Cli::default();
        let mut parser = lexopt::Parser::from_iter(full);
        while let Some(arg) = parser.next().map_err(|e| e.to_string())? {
            use lexopt::Arg::*;
            match arg {
                Long("headless") => cli.headless = true,
                Long("bench") => cli.bench = true,
                Long("cpu") => cli.cpu = true,
                Long("steps") => {
                    cli.steps = parser
                        .value()
                        .map_err(|e| e.to_string())?
                        .parse()
                        .map_err(|_| "steps muss eine Zahl sein".to_string())?;
                }
                Long("particles") => {
                    cli.particles = parser
                        .value()
                        .map_err(|e| e.to_string())?
                        .parse()
                        .map_err(|_| "particles muss eine Zahl sein".to_string())?;
                }
                Long("substeps") => {
                    cli.substeps = parser
                        .value()
                        .map_err(|e| e.to_string())?
                        .parse()
                        .map_err(|_| "substeps muss eine Zahl sein".to_string())?;
                }
                Long("h") => {
                    cli.h = Some(
                        parser
                            .value()
                            .map_err(|e| e.to_string())?
                            .parse()
                            .map_err(|_| "h muss eine Zahl sein".to_string())?,
                    );
                }
                Long("stiffness") => {
                    cli.stiffness = Some(
                        parser
                            .value()
                            .map_err(|e| e.to_string())?
                            .parse()
                            .map_err(|_| "stiffness muss eine Zahl sein".to_string())?,
                    );
                }
                Long("dt") => {
                    cli.dt = Some(
                        parser
                            .value()
                            .map_err(|e| e.to_string())?
                            .parse()
                            .map_err(|_| "dt muss eine Zahl sein".to_string())?,
                    );
                }
                Long("frames") => {
                    cli.frames = Some(
                        parser
                            .value()
                            .map_err(|e| e.to_string())?
                            .parse()
                            .map_err(|_| "frames muss eine Zahl sein".to_string())?,
                    );
                }
                Short('h') | Long("help") => {
                    return Err(Self::help());
                }
                _ => return Err(format!("unbekanntes Argument: {arg:?}\n{}", Self::help())),
            }
        }
        if cli.steps == 0 {
            return Err("steps muss >= 1 sein".to_string());
        }
        if cli.substeps == 0 {
            return Err("substeps muss >= 1 sein".to_string());
        }
        if cli.h.is_some_and(|h| h <= 0.0 || !h.is_finite()) {
            return Err("h muss > 0 sein".to_string());
        }
        Ok(cli)
    }

    fn help() -> String {
        "sph – 2D-SPH-Fluidsimulation (GPU/cuda-oxide + macroquad)\n\n\
         Aufruf: sph [OPTIONEN]\n\
         \t--headless       ohne Fenster rechnen (Validierung/Benchmark)\n\
         \t--steps N        Physikschritte headless (Default 500)\n\
         \t--particles N    Partikelanzahl 2048–262144 (Default 16384)\n\
         \t--substeps N     Physikschritte pro Frame, nur GUI (Default 3)\n\
         \t--bench          Durchsatz-Tabelle (nur headless)\n\
         \t--cpu            CPU-Backend statt GPU\n\
         \t--h H            Glättungslänge/Zellgröße in m (Default 0.04)\n\
         \t--stiffness K    Gas-Steifigkeit (Default 2000)\n\
         \t--dt DT          Zeitschritt in s (Default 0.0008)\n\
         \t--frames N       GUI nach N Frames beenden (Smoke-Test)\n\
         \t-h, --help       diese Hilfe\n"
            .to_string()
    }

    /// Überträgt CLI-Werte in eine `SimConfig` (mit Klemmung).
    pub fn sim_config(&self) -> SimConfig {
        let mut cfg = SimConfig {
            particles: self.particles,
            substeps: self.substeps,
            h: self.h.unwrap_or(0.04),
            stiffness: self.stiffness.unwrap_or(2000.0),
            dt: self.dt.unwrap_or(0.0008),
            ..SimConfig::default()
        };
        cfg.clamp_particles();
        cfg
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn defaults_entsprechen_feature_set_1() {
        let cfg = SimConfig::default();
        assert_eq!(cfg.particles, 16_384);
        assert_eq!(cfg.rest_density, 1000.0);
        assert_eq!(cfg.h, 0.04);
        assert_eq!(cfg.stiffness, 2000.0);
        assert_eq!(cfg.viscosity, 0.1);
        assert_eq!(cfg.gravity, 9.81);
        assert_eq!(cfg.dt, 0.0008);
        assert_eq!(cfg.wall_damping, 0.5);
    }

    #[test]
    fn dam_break_liefert_exakt_n_partikel_im_block() {
        for n in [2_048, 16_384] {
            let cfg = SimConfig {
                particles: n,
                ..SimConfig::default()
            };
            let parts = cfg.dam_break();
            assert_eq!(parts.len(), n);
            for p in &parts {
                assert!(p.pos[0] >= 0.0 && p.pos[0] <= 0.45 * cfg.domain_w + 1e-6);
                assert!(p.pos[1] >= 0.0 && p.pos[1] < cfg.domain_h);
                assert_eq!(p.vel, [0.0, 0.0]);
            }
        }
    }

    #[test]
    fn cli_parst_headless_run() {
        let cli = Cli::parse_from(["--headless", "--steps", "500", "--particles", "4096"])
            .expect("parse");
        assert!(cli.headless && !cli.bench && !cli.cpu);
        assert_eq!((cli.steps, cli.particles), (500, 4096));
        let cfg = cli.sim_config();
        assert_eq!(cfg.particles, 4096);
    }

    #[test]
    fn cli_parst_h_override() {
        let cli = Cli::parse_from(["--h", "0.02"]).expect("parse");
        assert_eq!(cli.sim_config().h, 0.02);
        assert!(Cli::parse_from(["--h", "0.0"]).is_err());
        assert!(Cli::parse_from(["--h", "-0.01"]).is_err());
        assert!(Cli::parse_from(["--h", "viel"]).is_err());
        // Ohne Flag bleibt der alte Default.
        assert_eq!(Cli::default().sim_config().h, 0.04);
    }

    #[test]
    fn cli_klemmt_partikel_und_lehnt_leere_steps_ab() {
        let cli = Cli::parse_from(["--particles", "999999999"]).expect("parse");
        assert_eq!(cli.sim_config().particles, 262_144);
        assert!(Cli::parse_from(["--steps", "0"]).is_err());
        assert!(Cli::parse_from(["--nope"]).is_err());
    }
}
