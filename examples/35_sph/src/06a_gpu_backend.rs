//! GPU-Backend mit sortierten Ping-Pong-Puffern (nur mit `gpu`-Feature).
//!
//! SoA-Device-Buffer + Launch-Sequenz: Hash → Scan → Reorder → Permute →
//! Dichte → Kraft → Integration. `pos`/`vel` halten die aktuelle
//! Zellordnung, `pos_alt`/`vel_alt` nehmen die nächste auf; nach jedem
//! Schritt tauscht der Host die Handles (kein Copy). Alle Steps sind
//! asynchron — Sync nur beim Download und an Messpunkten.

use cuda_core::{CudaContext, DeviceBuffer, LaunchConfig1D};

use crate::backend::Backend;
use crate::gpu_kernels::physics_device;
use crate::params::SimConfig;
use crate::sort_kernels::sort_device;
use crate::types::{InteractParams, Particle, SphParams};

/// Blockgröße aller Kernel (Ampere: mehr Register/Thread als bei 512).
const BLOCK: u32 = 256;

pub struct GpuBackend {
    stream: std::sync::Arc<cuda_core::CudaStream>,
    sort: sort_device::LoadedModule,
    physics: physics_device::LoadedModule,
    pos: DeviceBuffer<[f32; 2]>,
    pos_alt: DeviceBuffer<[f32; 2]>,
    vel: DeviceBuffer<[f32; 2]>,
    vel_alt: DeviceBuffer<[f32; 2]>,
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
    /// Baut Kontext, lädt beide PTX-Module, allokiert SoA-Buffer.
    pub fn new(cfg: &SimConfig) -> Result<Self, String> {
        let n = cfg.particles;
        let params = cfg.sph_params();
        let ncell = params.grid_w * params.grid_h;
        let ctx = CudaContext::new(0).map_err(|e| format!("CUDA-Kontext: {e:?}"))?;
        let stream = ctx.default_stream();
        // SAFETY: Dieses Paket besitzt die eingebetteten Device-Bundles
        // beider Module (`sort_device`, `physics_device`).
        let sort = unsafe { sort_device::load(&ctx) }.map_err(|e| format!("Sort-Modul: {e:?}"))?;
        let physics =
            unsafe { physics_device::load(&ctx) }.map_err(|e| format!("Physik-Modul: {e:?}"))?;
        let dev =
            |len: usize| DeviceBuffer::<u32>::zeroed(&stream, len).map_err(|e| format!("{e:?}"));
        let pos = DeviceBuffer::zeroed(&stream, n).map_err(|e| format!("{e:?}"))?;
        let pos_alt = DeviceBuffer::zeroed(&stream, n).map_err(|e| format!("{e:?}"))?;
        let vel = DeviceBuffer::zeroed(&stream, n).map_err(|e| format!("{e:?}"))?;
        let vel_alt = DeviceBuffer::zeroed(&stream, n).map_err(|e| format!("{e:?}"))?;
        let acc = DeviceBuffer::zeroed(&stream, n).map_err(|e| format!("{e:?}"))?;
        let dens = DeviceBuffer::zeroed(&stream, n).map_err(|e| format!("{e:?}"))?;
        let pres = DeviceBuffer::zeroed(&stream, n).map_err(|e| format!("{e:?}"))?;
        let hash = dev(n)?;
        let order = dev(n)?;
        let cell_start = dev(ncell as usize + 1)?;
        let counts = dev(ncell as usize)?;
        Ok(Self {
            stream,
            sort,
            physics,
            pos,
            pos_alt,
            vel,
            vel_alt,
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
        let p_hash = self.sort.prepare_k_hash(grid).expect("prep k_hash");
        let p_scan = self.sort.prepare_k_scan(scan).expect("prep k_scan");
        let p_reorder = self.sort.prepare_k_reorder(grid).expect("prep k_reorder");
        let p_permute = self.sort.prepare_k_permute(grid).expect("prep k_permute");
        let p_dens = self
            .physics
            .prepare_k_density(grid)
            .expect("prep k_density");
        let p_force = self.physics.prepare_k_force(grid).expect("prep k_force");
        let p_int = self
            .physics
            .prepare_k_integrate(grid)
            .expect("prep k_integrate");
        self.sort
            .k_hash(
                &self.stream,
                &p_hash,
                &self.pos,
                params,
                dev_ptr(&mut self.counts),
                &mut self.hash,
            )
            .expect("k_hash");
        self.sort
            .k_scan(
                &self.stream,
                &p_scan,
                dev_ptr(&mut self.counts),
                self.ncell,
                dev_ptr(&mut self.cell_start),
            )
            .expect("k_scan");
        self.sort
            .k_reorder(
                &self.stream,
                &p_reorder,
                &self.hash,
                n,
                dev_ptr(&mut self.counts),
                dev_ptr(&mut self.order),
            )
            .expect("k_reorder");
        self.sort
            .k_permute(
                &self.stream,
                &p_permute,
                &self.order,
                &self.pos,
                &self.vel,
                n,
                &mut self.pos_alt,
                &mut self.vel_alt,
            )
            .expect("k_permute");
        self.physics
            .k_density(
                &self.stream,
                &p_dens,
                &self.pos_alt,
                &self.cell_start,
                params,
                &mut self.dens,
                &mut self.pres,
            )
            .expect("k_density");
        self.physics
            .k_force(
                &self.stream,
                &p_force,
                &self.pos_alt,
                &self.vel_alt,
                &self.dens,
                &self.pres,
                &self.cell_start,
                params,
                &mut self.acc,
            )
            .expect("k_force");
        self.physics
            .k_integrate(
                &self.stream,
                &p_int,
                &self.acc,
                params,
                inter,
                &mut self.pos_alt,
                &mut self.vel_alt,
            )
            .expect("k_integrate");
        // Ping-Pong: sortierte nächste Ordnung wird aktuell (Handle-Tausch).
        std::mem::swap(&mut self.pos, &mut self.pos_alt);
        std::mem::swap(&mut self.vel, &mut self.vel_alt);
        // Absichtlich kein synchronize: FIFO-Stream, Sync nur in sync_host.
    }

    fn run_steps(&mut self, steps: usize) -> Option<f32> {
        let flags = Some(cuda_core::sys::CUevent_flags_enum_CU_EVENT_DEFAULT);
        let start = self.stream.record_event(flags).expect("Event-Start");
        for _ in 0..steps {
            self.step();
        }
        let stop = self.stream.record_event(flags).expect("Event-Stopp");
        let ms = start.elapsed_ms(&stop).expect("Event-Zeit");
        Some(ms)
    }

    fn sync_host(&mut self) {
        // Einziger Sync-Punkt im Normalpfad: jedes copy_to_host wartet
        // auf alle zuvor eingereihten Kernel (ein Sync pro Copy).
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
