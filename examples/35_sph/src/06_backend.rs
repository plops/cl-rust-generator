//! Rechen-Backends hinter dem `Backend`-Trait: GPU (cuda-oxide) + CPU.
//!
//! Beide Backends führen exakt dieselbe Physik aus (Dichte → Kraft →
//! Integration über ein Uniform Grid); die GUI/Headless-Schicht sieht nur
//! den Trait und Host-Spiegel der Positionen/Geschwindigkeiten/Dichten.

use crate::params::SimConfig;
use crate::spatial_grid::{CpuGrid, cell_coords};
use crate::sph_math::{poly6, pressure, spiky_grad_factor, visc_laplacian};
use crate::types::{InteractParams, Particle, SphParams};

/// Ein Physik-Zeitschritt + Host-Zugriff, backend-unabhängig.
pub trait Backend {
    /// Führt genau einen Zeitschritt dt aus (N Partikel).
    fn step(&mut self);
    /// Holt Geräte- in Host-Spiegel (CPU: kopiert aus Partikeln).
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

/// CPU-Referenz: gleiche Mathematik wie die GPU-Kernel, ein Thread.
pub struct CpuBackend {
    particles: Vec<Particle>,
    grid: CpuGrid,
    params: SphParams,
    inter: InteractParams,
    scratch_pos: Vec<[f32; 2]>,
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
            particles: vec![Particle::at_rest([0.0, 0.0], 0.0); n],
            grid,
            params: cfg.sph_params(),
            inter: InteractParams::neutral(cfg.domain_w, cfg.domain_h),
            scratch_pos: vec![[0.0, 0.0]; n],
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
        for (i, p) in self.particles.iter().enumerate() {
            self.scratch_pos[i] = p.pos;
        }
        self.grid.rebuild(&self.scratch_pos, &grid_meta);
        let n = self.particles.len();
        // Dichte + Druck (inkl. Selbstwechselwirkung).
        for i in 0..n {
            let pi = self.particles[i].pos;
            let (cx, cy) = cell_coords(pi[0], pi[1], &grid_meta);
            let mut rho = 0.0f32;
            self.grid.for_each_neighbor(cx, cy, &grid_meta, |j| {
                let r = crate::sph_math::dist(pi, self.particles[j].pos);
                rho += params.mass * poly6(r, params.h);
            });
            self.particles[i].density = rho;
            self.particles[i].pressure = pressure(rho, params.rest_density, params.stiffness);
        }
        // Druck- + Viskositätskräfte.
        for i in 0..n {
            let pi = self.particles[i].pos;
            let vi = self.particles[i].vel;
            let rhoi = self.particles[i].density;
            let ppi = self.particles[i].pressure;
            let (cx, cy) = cell_coords(pi[0], pi[1], &grid_meta);
            let mut fx = 0.0f32;
            let mut fy = 0.0f32;
            self.grid.for_each_neighbor(cx, cy, &grid_meta, |j| {
                if j == i {
                    return;
                }
                let pj = self.particles[j].pos;
                let qx = pi[0] - pj[0];
                let qy = pi[1] - pj[1];
                let r2 = qx * qx + qy * qy;
                if r2 < params.h * params.h && r2 > 0.0 {
                    let r = r2.sqrt();
                    let rhoj = self.particles[j].density;
                    let pterm = ppi / (rhoi * rhoi) + self.particles[j].pressure / (rhoj * rhoj);
                    // ρᵢ-Faktor wie im Spec-Pseudocode (kürzt sich bei a=F/ρᵢ).
                    let f = -rhoi * params.mass * pterm * spiky_grad_factor(r, params.h);
                    fx += f * qx;
                    fy += f * qy;
                    let w = params.viscosity * params.mass * visc_laplacian(r, params.h) / rhoj;
                    let vj = self.particles[j].vel;
                    fx += w * (vj[0] - vi[0]);
                    fy += w * (vj[1] - vi[1]);
                }
            });
            self.particles[i].force = [fx, fy];
        }
        // Integration (Symplectic Euler, Wände, Hindernis, Maus, Strahl).
        let inter = self.inter;
        for i in 0..n {
            let iu = i as u32;
            if inter.mouse_mode == 2 && iu >= inter.jet_start && iu < inter.jet_start + inter.jet_count
            {
                let k = (iu - inter.jet_start) as f32;
                let ax = k * 0.6180339887;
                let ay = k * 0.3819660113;
                self.particles[i].pos = [
                    inter.mouse[0] + (ax - ax.floor() - 0.5) * 0.016,
                    inter.mouse[1] + (ay - ay.floor() - 0.5) * 0.016,
                ];
                self.particles[i].vel = inter.jet_vel;
                continue;
            }
            let mut p = self.particles[i].pos;
            let mut v = self.particles[i].vel;
            let inv_rho = 1.0 / self.particles[i].density.max(1e-6);
            let mut ax = self.particles[i].force[0] * inv_rho;
            let mut ay = self.particles[i].force[1] * inv_rho - params.gravity * inter.gravity_on;
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
            self.particles[i].pos = p;
            self.particles[i].vel = v;
        }
    }

    fn sync_host(&mut self) {
        for (i, p) in self.particles.iter().enumerate() {
            self.host_pos[i] = p.pos;
            self.host_vel[i] = p.vel;
            self.host_dens[i] = p.density;
        }
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
        assert_eq!(particles.len(), self.particles.len());
        self.particles.copy_from_slice(particles);
        self.sync_host();
    }

    fn particle_count(&self) -> usize {
        self.particles.len()
    }
}

/// GPU-Backend: SoA-Device-Buffer + Launch-Sequenz.
/// Hinweis: Unter plain `cargo test` wird `device::load` nie aufgerufen und
/// vom Linker gestrippt (verifiziert); GPU-Läufe brauchen `cargo oxide run`.
pub use gpu::GpuBackend;

mod gpu {
    use cuda_core::{CudaContext, DeviceBuffer, LaunchConfig1D};

    use super::*;
    use crate::gpu_kernels::device;

    /// Blockgröße aller Kernel (Ampere: mehr Register/Thread als bei 512).
    const BLOCK: u32 = 256;

    pub struct GpuBackend {
        stream: std::sync::Arc<cuda_core::CudaStream>,
        module: device::LoadedModule,
        pos: DeviceBuffer<[f32; 2]>,
        vel: DeviceBuffer<[f32; 2]>,
        acc: DeviceBuffer<[f32; 2]>,
        dens: DeviceBuffer<f32>,
        pres: DeviceBuffer<f32>,
        hash: DeviceBuffer<u32>,
        order: DeviceBuffer<u32>,
        cell_start: DeviceBuffer<u32>,
        counts: DeviceBuffer<u32>,
        params: SphParams,
        inter: InteractParams,
        blocks: u32,
        ncell: u32,
        host_pos: Vec<[f32; 2]>,
        host_vel: Vec<[f32; 2]>,
        host_dens: Vec<f32>,
    }

    /// Exklusiver Gerätezeiger (Aufrufer sichert Alleinzugriff zu).
    fn dev_ptr<T>(buf: &mut DeviceBuffer<T>) -> *mut T {
        buf.cu_deviceptr() as *mut T
    }

    impl GpuBackend {
        /// Baut Kontext, lädt das PTX-Modul, allokiert SoA-Buffer.
        pub fn new(cfg: &SimConfig) -> Result<Self, String> {
            let n = cfg.particles;
            let params = cfg.sph_params();
            let ncell = params.grid_w * params.grid_h;
            let ctx = CudaContext::new(0).map_err(|e| format!("CUDA-Kontext: {e:?}"))?;
            let stream = ctx.default_stream();
            // SAFETY: Dieses Paket besitzt das eingebettete Device-Bundle
            // des obigen `device`-Moduls.
            let module =
                unsafe { device::load(&ctx) }.map_err(|e| format!("Modul-Load: {e:?}"))?;
            let dev = |len: usize| {
                DeviceBuffer::<u32>::zeroed(&stream, len).map_err(|e| format!("{e:?}"))
            };
            let pos = DeviceBuffer::zeroed(&stream, n).map_err(|e| format!("{e:?}"))?;
            let vel = DeviceBuffer::zeroed(&stream, n).map_err(|e| format!("{e:?}"))?;
            let acc = DeviceBuffer::zeroed(&stream, n).map_err(|e| format!("{e:?}"))?;
            let dens = DeviceBuffer::zeroed(&stream, n).map_err(|e| format!("{e:?}"))?;
            let pres = DeviceBuffer::zeroed(&stream, n).map_err(|e| format!("{e:?}"))?;
            let hash = dev(n)?;
            let order = dev(n)?;
            let cell_start = dev(ncell as usize + 1)?;
            let counts = dev(ncell as usize)?;
            Ok(Self {
                stream,
                module,
                pos,
                vel,
                acc,
                dens,
                pres,
                hash,
                order,
                cell_start,
                counts,
                params,
                inter: InteractParams::neutral(cfg.domain_w, cfg.domain_h),
                blocks: n.div_ceil(BLOCK as usize) as u32,
                ncell,
                host_pos: vec![[0.0, 0.0]; n],
                host_vel: vec![[0.0, 0.0]; n],
                host_dens: vec![0.0; n],
            })
        }

        fn grid_cfg(&self) -> LaunchConfig1D {
            LaunchConfig1D::new(self.blocks, BLOCK, 0)
        }

        fn scan_cfg(&self) -> LaunchConfig1D {
            LaunchConfig1D::new(1, BLOCK, 0)
        }
    }

    impl Backend for GpuBackend {
        fn step(&mut self) {
            self.counts
                .zero_async(&self.stream)
                .expect("counts zurücksetzen");
            let grid = self.grid_cfg();
            let scan = self.scan_cfg();
            let params = self.params;
            let inter = self.inter;
            let n = params.num_particles;
            // Prepared-Launch validiert Shape/Kontrakt; die Rohzeiger sind
            // exklusiv (dev_ptr nimmt &mut) und decken alle Zugriffe ab.
            let p_hash = self.module.prepare_k_hash(grid).expect("prep k_hash");
            let p_scan = self.module.prepare_k_scan(scan).expect("prep k_scan");
            let p_reorder = self.module.prepare_k_reorder(grid).expect("prep k_reorder");
            let p_dens = self.module.prepare_k_density(grid).expect("prep k_density");
            let p_force = self.module.prepare_k_force(grid).expect("prep k_force");
            let p_int = self
                .module
                .prepare_k_integrate(grid)
                .expect("prep k_integrate");
            self.module
                .k_hash(
                    &self.stream,
                    &p_hash,
                    &self.pos,
                    params,
                    dev_ptr(&mut self.counts),
                    &mut self.hash,
                )
                .expect("k_hash");
            self.module
                .k_scan(
                    &self.stream,
                    &p_scan,
                    dev_ptr(&mut self.counts),
                    self.ncell,
                    dev_ptr(&mut self.cell_start),
                )
                .expect("k_scan");
            self.module
                .k_reorder(
                    &self.stream,
                    &p_reorder,
                    &self.hash,
                    n,
                    dev_ptr(&mut self.counts),
                    dev_ptr(&mut self.order),
                )
                .expect("k_reorder");
            self.module
                .k_density(
                    &self.stream,
                    &p_dens,
                    &self.pos,
                    &self.order,
                    &self.cell_start,
                    params,
                    &mut self.dens,
                    &mut self.pres,
                )
                .expect("k_density");
            self.module
                .k_force(
                    &self.stream,
                    &p_force,
                    &self.pos,
                    &self.vel,
                    &self.dens,
                    &self.pres,
                    &self.order,
                    &self.cell_start,
                    params,
                    &mut self.acc,
                )
                .expect("k_force");
            self.module
                .k_integrate(
                    &self.stream,
                    &p_int,
                    &self.acc,
                    params,
                    inter,
                    &mut self.pos,
                    &mut self.vel,
                )
                .expect("k_integrate");
            self.stream.synchronize().expect("Stream-Sync");
        }

        fn sync_host(&mut self) {
            self.pos
                .copy_to_host(&self.stream, &mut self.host_pos)
                .expect("pos-Download");
            self.vel
                .copy_to_host(&self.stream, &mut self.host_vel)
                .expect("vel-Download");
            self.dens
                .copy_to_host(&self.stream, &mut self.host_dens)
                .expect("dens-Download");
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
            assert_eq!(particles.len(), self.host_pos.len());
            let pos: Vec<[f32; 2]> = particles.iter().map(|p| p.pos).collect();
            let vel: Vec<[f32; 2]> = particles.iter().map(|p| p.vel).collect();
            self.pos
                .copy_from_host(&self.stream, &pos)
                .expect("pos-Upload");
            self.vel
                .copy_from_host(&self.stream, &vel)
                .expect("vel-Upload");
            self.stream.synchronize().expect("Upload-Sync");
            self.sync_host();
        }

        fn particle_count(&self) -> usize {
            self.host_pos.len()
        }
    }
}
