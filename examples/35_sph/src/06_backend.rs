//! Rechen-Backends hinter dem `Backend`-Trait: GPU (cuda-oxide) + CPU.
//!
//! Beide Backends führen exakt dieselbe Physik aus (Dichte → Kraft →
//! Integration über ein Uniform Grid); die GUI/Headless-Schicht sieht nur
//! den Trait und Host-Spiegel der Positionen/Geschwindigkeiten/Dichten.

use rayon::prelude::*;

use crate::params::SimConfig;
use crate::spatial_grid::{CpuGrid, cell_coords};
use crate::sph_math::{poly6, pressure, spiky_grad_factor, visc_laplacian};
use crate::types::{InteractParams, Particle, SphParams};

/// Ein Physik-Zeitschritt + Host-Zugriff, backend-unabhängig.
pub trait Backend {
    /// Führt genau einen Zeitschritt dt aus (N Partikel).
    ///
    /// Auf der GPU ist das rein asynchron (kein Host-Sync); erst
    /// `sync_host` bzw. [`Backend::run_steps`] synchronisieren.
    fn step(&mut self);
    /// Führt `steps` Zeitschritte aus und meldet die reine GPU-Kernelzeit
    /// in Millisekunden, wenn das Backend sie per CUDA-Events misst
    /// (sonst `None`, z. B. CPU-Backend mit Wallclock).
    fn run_steps(&mut self, steps: usize) -> Option<f32> {
        for _ in 0..steps {
            self.step();
        }
        None
    }
    /// Holt Geräte- in Host-Spiegel (CPU: kopiert aus SoA-Vektoren).
    fn sync_host(&mut self);
    /// Positionen nach `sync_host` (Weltkoordinaten, m).
    fn positions(&self) -> &[[f32; 2]];
    /// Geschwindigkeiten nach `sync_host` (m/s).
    fn velocities(&self) -> &[[f32; 2]];
    /// Dichten nach `sync_host` (kg/m³).
    fn densities(&self) -> &[f32];
    /// Setzt Maus-/Hindernis-/Gravitationszustand für Folgeschritte.
    fn set_interact(&mut self, inter: InteractParams);
    /// Lädt Anfangszustand (Länge muss zu N passen).
    fn reset(&mut self, particles: &[Particle]);
    /// Partikelanzahl N.
    fn particle_count(&self) -> usize;
}

/// CPU-Referenz: gleiche Mathematik wie die GPU-Kernel, parallel per Rayon.
///
/// SoA-Layout wie auf der GPU: Jede Phase liest unveränderliche Vektoren
/// und schreibt genau einen Vektor — disjunkte Borrows, daher ohne `unsafe`
/// über alle Kerne parallelisierbar. Die Summationsreihenfolge innerhalb
/// eines Partikels ist unverändert (bitidentisch zur sequenziellen Form).
pub struct CpuBackend {
    pos: Vec<[f32; 2]>,
    vel: Vec<[f32; 2]>,
    force: Vec<[f32; 2]>,
    dens: Vec<f32>,
    pres: Vec<f32>,
    grid: CpuGrid,
    params: SphParams,
    inter: InteractParams,
    host_pos: Vec<[f32; 2]>,
    host_vel: Vec<[f32; 2]>,
    host_dens: Vec<f32>,
}

impl CpuBackend {
    /// Leeres Backend für `cfg` (füllen via `reset`).
    pub fn new(cfg: &SimConfig) -> Self {
        let n = cfg.particles;
        let grid = CpuGrid::new(cfg.grid_meta().num_cells(), n);
        Self {
            pos: vec![[0.0, 0.0]; n],
            vel: vec![[0.0, 0.0]; n],
            force: vec![[0.0, 0.0]; n],
            dens: vec![0.0; n],
            pres: vec![0.0; n],
            grid,
            params: cfg.sph_params(),
            inter: InteractParams::neutral(cfg.domain_w, cfg.domain_h),
            host_pos: vec![[0.0, 0.0]; n],
            host_vel: vec![[0.0, 0.0]; n],
            host_dens: vec![0.0; n],
        }
    }
}

impl Backend for CpuBackend {
    fn step(&mut self) {
        let params = self.params;
        let grid_meta = crate::types::GridMeta {
            w: params.grid_w,
            h: params.grid_h,
            cell: params.h,
            inv_cell: 1.0 / params.h,
        };
        self.grid.rebuild(&self.pos, &grid_meta);
        // Dichte + Druck (inkl. Selbstwechselwirkung).
        let (pos, grid) = (&self.pos, &self.grid);
        self.dens
            .par_iter_mut()
            .zip(self.pres.par_iter_mut())
            .enumerate()
            .for_each(|(i, (rho, pres))| {
                let pi = pos[i];
                let (cx, cy) = cell_coords(pi[0], pi[1], &grid_meta);
                let mut acc = 0.0f32;
                grid.for_each_neighbor(cx, cy, &grid_meta, |j| {
                    let r = crate::sph_math::dist(pi, pos[j]);
                    acc += params.mass * poly6(r, params.h);
                });
                *rho = acc;
                *pres = pressure(acc, params.rest_density, params.stiffness);
            });
        // Druck- + Viskositätskräfte.
        let (pos, vel, dens, pres, grid) =
            (&self.pos, &self.vel, &self.dens, &self.pres, &self.grid);
        self.force.par_iter_mut().enumerate().for_each(|(i, f)| {
            let pi = pos[i];
            let vi = vel[i];
            let rhoi = dens[i];
            let ppi = pres[i];
            let (cx, cy) = cell_coords(pi[0], pi[1], &grid_meta);
            let mut fx = 0.0f32;
            let mut fy = 0.0f32;
            grid.for_each_neighbor(cx, cy, &grid_meta, |j| {
                if j == i {
                    return;
                }
                let pj = pos[j];
                let qx = pi[0] - pj[0];
                let qy = pi[1] - pj[1];
                let r2 = qx * qx + qy * qy;
                if r2 < params.h * params.h && r2 > 0.0 {
                    let r = r2.sqrt();
                    let rhoj = dens[j];
                    let pterm = ppi / (rhoi * rhoi) + pres[j] / (rhoj * rhoj);
                    // ρᵢ-Faktor wie im Spec-Pseudocode (kürzt sich bei a=F/ρᵢ).
                    let f = -rhoi * params.mass * pterm * spiky_grad_factor(r, params.h);
                    fx += f * qx;
                    fy += f * qy;
                    let w = params.viscosity * params.mass * visc_laplacian(r, params.h) / rhoj;
                    let vj = vel[j];
                    fx += w * (vj[0] - vi[0]);
                    fy += w * (vj[1] - vi[1]);
                }
            });
            *f = [fx, fy];
        });
        // Integration (Symplectic Euler, Wände, Hindernis, Maus, Strahl).
        let (force, dens) = (&self.force, &self.dens);
        let inter = self.inter;
        self.pos
            .par_iter_mut()
            .zip(self.vel.par_iter_mut())
            .enumerate()
            .for_each(|(i, (p_slot, v_slot))| {
                let iu = i as u32;
                if inter.mouse_mode == 2
                    && iu >= inter.jet_start
                    && iu < inter.jet_start + inter.jet_count
                {
                    let k = (iu - inter.jet_start) as f32;
                    let jx = k * 0.618_034;
                    let jy = k * 0.381_966;
                    *p_slot = [
                        inter.mouse[0] + (jx - jx.floor() - 0.5) * 0.016,
                        inter.mouse[1] + (jy - jy.floor() - 0.5) * 0.016,
                    ];
                    *v_slot = inter.jet_vel;
                    return;
                }
                let mut p = *p_slot;
                let mut v = *v_slot;
                let inv_rho = 1.0 / dens[i].max(1e-6);
                let mut ax = force[i][0] * inv_rho;
                let mut ay = force[i][1] * inv_rho - params.gravity * inter.gravity_on;
                if inter.mouse_mode == 1 {
                    let dx = p[0] - inter.mouse[0];
                    let dy = p[1] - inter.mouse[1];
                    let r2 = dx * dx + dy * dy;
                    let rv = 0.2;
                    if r2 < rv * rv {
                        let r = r2.sqrt().max(1e-4);
                        let strength = 60.0 * (1.0 - r / rv) / r;
                        ax += -dy * strength;
                        ay += dx * strength;
                    }
                }
                v[0] += ax * params.dt;
                v[1] += ay * params.dt;
                let vmax = 12.0;
                let s2 = v[0] * v[0] + v[1] * v[1];
                if s2 > vmax * vmax {
                    let s = vmax / s2.sqrt();
                    v[0] *= s;
                    v[1] *= s;
                }
                p[0] += v[0] * params.dt;
                p[1] += v[1] * params.dt;
                let damp = params.wall_damping;
                if p[0] < 0.0 {
                    p[0] = 0.0;
                    v[0] = -v[0] * damp;
                }
                if p[0] > params.domain_w {
                    p[0] = params.domain_w;
                    v[0] = -v[0] * damp;
                }
                if p[1] < 0.0 {
                    p[1] = 0.0;
                    v[1] = -v[1] * damp;
                }
                if p[1] > params.domain_h {
                    p[1] = params.domain_h;
                    v[1] = -v[1] * damp;
                }
                let ox = p[0] - inter.obstacle[0];
                let oy = p[1] - inter.obstacle[1];
                let orad = inter.obstacle_r;
                if ox * ox + oy * oy < orad * orad {
                    let od = (ox * ox + oy * oy).sqrt().max(1e-6);
                    let (nx, ny) = (ox / od, oy / od);
                    p[0] = inter.obstacle[0] + nx * orad;
                    p[1] = inter.obstacle[1] + ny * orad;
                    let vn = v[0] * nx + v[1] * ny;
                    if vn < 0.0 {
                        v[0] -= (1.0 + damp) * vn * nx;
                        v[1] -= (1.0 + damp) * vn * ny;
                    }
                }
                *p_slot = p;
                *v_slot = v;
            });
    }

    fn sync_host(&mut self) {
        self.host_pos.copy_from_slice(&self.pos);
        self.host_vel.copy_from_slice(&self.vel);
        self.host_dens.copy_from_slice(&self.dens);
    }

    fn positions(&self) -> &[[f32; 2]] {
        &self.host_pos
    }

    fn velocities(&self) -> &[[f32; 2]] {
        &self.host_vel
    }

    fn densities(&self) -> &[f32] {
        &self.host_dens
    }

    fn set_interact(&mut self, inter: InteractParams) {
        self.inter = inter;
    }

    fn reset(&mut self, particles: &[Particle]) {
        assert_eq!(particles.len(), self.pos.len());
        for (i, p) in particles.iter().enumerate() {
            self.pos[i] = p.pos;
            self.vel[i] = p.vel;
            self.force[i] = p.force;
            self.dens[i] = p.density;
            self.pres[i] = p.pressure;
        }
        self.sync_host();
    }

    fn particle_count(&self) -> usize {
        self.pos.len()
    }
}

/// GPU-Backend (Ping-Pong-Sortierung, nur mit `gpu`-Feature, da der
/// Geräte-Anker plain nicht linkt). Implementierung in `06a_gpu_backend.rs`.
#[cfg(feature = "gpu")]
pub use crate::gpu_backend::GpuBackend;
