//! GPU-Physikkerne als cuda-oxide-Single-Source (`#[kernel]` → PTX).
//!
//! Sechs Kernel pro Zeitschritt: `k_hash` (Zell-Hash + atomares Zählen),
//! `k_scan` (exklusiver Präfix-Sum + Cursor-Init, ein Thread), `k_reorder`
//! (atomarer Scatter in Morton-ähnliche Zellordnung), `k_density`,
//! `k_force`, `k_integrate`. Alle Nachbarschleifen nutzen `while` (primitiv,
//! warp-freundlich), alle Launches Blockgröße 256.

use cuda_device::atomic::{AtomicOrdering, DeviceAtomicU32};
use cuda_device::{DisjointSlice, kernel, launch_bounds, launch_contract, thread};
use cuda_host::cuda_module;

use crate::spatial_grid::hash_cell;
use crate::sph_math::{poly6_coef, pressure, spiky_coef, visc_coef};
use crate::types::{InteractParams, SphParams};

/// Enthält alle Device-Kernel; `cuda_host` generiert daraus `LoadedModule`
/// mit typisierten Launch-Funktionen (`module.k_hash(...)`).
///
/// Die Rohzeiger-Parameter sind exklusiv und größen-geprüft (Host garantiert
/// die Buffer, Kernel prüfen Indexschranken); `unsafe fn` ist für `#[kernel]`
/// nicht vorgesehen, daher ist der Deref-Lint hier erlaubt.
#[cuda_module]
#[allow(clippy::not_unsafe_ptr_arg_deref, clippy::too_many_arguments)]
pub mod device {
    use super::*;

    /// Schritt 1a: Zell-Hash pro Partikel + atomare Zellzählung.
    #[kernel]
    #[launch_bounds(256)]
    #[launch_contract(domain = 1, block = (256, 1, 1))]
    pub fn k_hash(
        pos: &[[f32; 2]],
        params: SphParams,
        counts: *mut u32,
        mut hash: DisjointSlice<u32>,
    ) {
        let idx = thread::index_1d();
        let i = idx.get();
        if i >= params.num_particles as usize {
            return;
        }
        let p = pos[i];
        let h = hash_cell(p[0], p[1], 1.0 / params.h, params.grid_w, params.grid_h);
        let Some(slot) = hash.get_mut(idx) else {
            return;
        };
        *slot = h;
        let atomic = unsafe { DeviceAtomicU32::from_ptr(counts.add(h as usize)) };
        atomic.fetch_add(1, AtomicOrdering::Relaxed);
    }

    /// Schritt 1b: exklusiver Präfix-Sum über Zellzählungen (nur Thread 0).
    ///
    /// Schreibt `cell_start` (ncell+1) und verwandelt `counts` in-place in
    /// den Scatter-Cursor für `k_reorder`.
    #[kernel]
    #[launch_bounds(256)]
    #[launch_contract(domain = 1, block = (256, 1, 1))]
    pub fn k_scan(counts: *mut u32, ncell: u32, cell_start: *mut u32) {
        if thread::index_1d().get() != 0 {
            return;
        }
        let mut sum = 0u32;
        let mut c = 0u32;
        while c < ncell {
            unsafe {
                let slot = DeviceAtomicU32::from_ptr(counts.add(c as usize));
                let cnt = slot.load(AtomicOrdering::Relaxed);
                cell_start.add(c as usize).write(sum);
                slot.store(sum, AtomicOrdering::Relaxed);
                sum += cnt;
            }
            c += 1;
        }
        unsafe {
            cell_start.add(ncell as usize).write(sum);
        }
    }

    /// Schritt 1c: scattert Partikelindizes zellweise nach `order`.
    #[kernel]
    #[launch_bounds(256)]
    #[launch_contract(domain = 1, block = (256, 1, 1))]
    pub fn k_reorder(hash: &[u32], n: u32, cursor: *mut u32, order: *mut u32) {
        let i = thread::index_1d().get();
        if i >= n as usize {
            return;
        }
        let h = hash[i];
        let slot = unsafe { DeviceAtomicU32::from_ptr(cursor.add(h as usize)) }
            .fetch_add(1, AtomicOrdering::Relaxed);
        // Eindeutig: jedes fetch_add liefert einen eigenen Slot.
        unsafe {
            order.add(slot as usize).write(i as u32);
        }
    }

    /// Schritt 2: Dichte (Poly6 über 3×3 Zellen) + Druck (EOS).
    ///
    /// Die Selbstwechselwirkung (r = 0) ist enthalten: Das Partikel liegt
    /// garantiert in seiner Zentralzelle, daher ist ρᵢ immer > 0.
    #[kernel]
    #[launch_bounds(256)]
    #[launch_contract(domain = 1, block = (256, 1, 1))]
    pub fn k_density(
        pos: &[[f32; 2]],
        order: &[u32],
        cell_start: &[u32],
        params: SphParams,
        mut dens: DisjointSlice<f32>,
        mut pres: DisjointSlice<f32>,
    ) {
        let i = thread::index_1d().get();
        if i >= params.num_particles as usize {
            return;
        }
        let pi = pos[i];
        let h = params.h;
        let h2 = h * h;
        let coef = poly6_coef(h) * params.mass;
        let inv = 1.0 / h;
        let gw = params.grid_w as i32;
        let gh = params.grid_h as i32;
        let cx = ((pi[0] * inv).floor() as i32).clamp(0, gw - 1);
        let cy = ((pi[1] * inv).floor() as i32).clamp(0, gh - 1);
        let mut rho = 0.0f32;
        let mut dy = -1i32;
        while dy <= 1 {
            let mut dx = -1i32;
            while dx <= 1 {
                let nx = cx + dx;
                let ny = cy + dy;
                if nx >= 0 && nx < gw && ny >= 0 && ny < gh {
                    let c = (ny * gw + nx) as usize;
                    let mut k = cell_start[c] as usize;
                    let end = cell_start[c + 1] as usize;
                    while k < end {
                        let j = order[k] as usize;
                        let qx = pi[0] - pos[j][0];
                        let qy = pi[1] - pos[j][1];
                        let r2 = qx * qx + qy * qy;
                        if r2 < h2 {
                            let d = h2 - r2;
                            rho += d * d * d;
                        }
                        k += 1;
                    }
                }
                dx += 1;
            }
            dy += 1;
        }
        rho *= coef;
        let p = pressure(rho, params.rest_density, params.stiffness);
        let Some(d) = dens.get_mut(thread::index_1d()) else {
            return;
        };
        *d = rho;
        let Some(pp) = pres.get_mut(thread::index_1d()) else {
            return;
        };
        *pp = p;
    }

    /// Schritt 3: Druck- (Spiky) + Viskositätskräfte als Beschleunigung.
    ///
    /// aᵢ = (Fᵢ^druck + Fᵢ^visk) / ρᵢ; Gravitation folgt in `k_integrate`.
    #[kernel]
    #[launch_bounds(256)]
    #[launch_contract(domain = 1, block = (256, 1, 1))]
    pub fn k_force(
        pos: &[[f32; 2]],
        vel: &[[f32; 2]],
        dens: &[f32],
        pres: &[f32],
        order: &[u32],
        cell_start: &[u32],
        params: SphParams,
        mut acc: DisjointSlice<[f32; 2]>,
    ) {
        let i = thread::index_1d().get();
        if i >= params.num_particles as usize {
            return;
        }
        let pi = pos[i];
        let vi = vel[i];
        let rhoi = dens[i];
        let ppi = pres[i];
        let h = params.h;
        let h2 = h * h;
        let spiky = spiky_coef(h);
        let visc = visc_coef(h) * params.viscosity * params.mass;
        let inv = 1.0 / h;
        let gw = params.grid_w as i32;
        let gh = params.grid_h as i32;
        let cx = ((pi[0] * inv).floor() as i32).clamp(0, gw - 1);
        let cy = ((pi[1] * inv).floor() as i32).clamp(0, gh - 1);
        let mut fx = 0.0f32;
        let mut fy = 0.0f32;
        let mut dy = -1i32;
        while dy <= 1 {
            let mut dx = -1i32;
            while dx <= 1 {
                let nx = cx + dx;
                let ny = cy + dy;
                if nx >= 0 && nx < gw && ny >= 0 && ny < gh {
                    let c = (ny * gw + nx) as usize;
                    let mut k = cell_start[c] as usize;
                    let end = cell_start[c + 1] as usize;
                    while k < end {
                        let j = order[k] as usize;
                        if j != i {
                            let qx = pi[0] - pos[j][0];
                            let qy = pi[1] - pos[j][1];
                            let r2 = qx * qx + qy * qy;
                            if r2 < h2 && r2 > 0.0 {
                                let r = r2.sqrt();
                                let rhoj = dens[j];
                                // Druck: −ρᵢ·m·(Pᵢ/ρᵢ² + Pⱼ/ρⱼ²)·∇W
                                // (ρᵢ kürzt sich erst bei a = F/ρᵢ).
                                let pterm = ppi / (rhoi * rhoi) + pres[j] / (rhoj * rhoj);
                                let t = h - r;
                                let s = -spiky * t * t / r;
                                let f = -rhoi * params.mass * pterm * s;
                                fx += f * qx;
                                fy += f * qy;
                                // Viskosität: μ·m·(vⱼ−vᵢ)/ρⱼ·∇²W.
                                let w = visc * (h - r) / rhoj;
                                let vj = vel[j];
                                fx += w * (vj[0] - vi[0]);
                                fy += w * (vj[1] - vi[1]);
                            }
                        }
                        k += 1;
                    }
                }
                dx += 1;
            }
            dy += 1;
        }
        let inv_rho = 1.0 / rhoi.max(1e-6);
        let Some(a) = acc.get_mut(thread::index_1d()) else {
            return;
        };
        *a = [fx * inv_rho, fy * inv_rho];
    }

    /// Schritt 4: Symplectic Euler + Wände + Hindernis + Maus + Strahl.
    #[kernel]
    #[launch_bounds(256)]
    #[launch_contract(domain = 1, block = (256, 1, 1))]
    pub fn k_integrate(
        acc: &[[f32; 2]],
        params: SphParams,
        inter: InteractParams,
        mut pos: DisjointSlice<[f32; 2]>,
        mut vel: DisjointSlice<[f32; 2]>,
    ) {
        let i = thread::index_1d().get();
        if i >= params.num_particles as usize {
            return;
        }
        // Strahl: recycelte Indizes deterministisch um die Maus streuen.
        // (i < N ≤ 262144 passt immer in u32.)
        let iu = i as u32;
        if inter.mouse_mode == 2 && iu >= inter.jet_start && iu < inter.jet_start + inter.jet_count
        {
            let k = (i as u32 - inter.jet_start) as f32;
            let ax = k * 0.618_034;
            let ay = k * 0.381_966;
            let fx = (ax - ax.floor() - 0.5) * 0.016;
            let fy = (ay - ay.floor() - 0.5) * 0.016;
            let Some(p_slot) = pos.get_mut(thread::index_1d()) else {
                return;
            };
            *p_slot = [inter.mouse[0] + fx, inter.mouse[1] + fy];
            let Some(v_slot) = vel.get_mut(thread::index_1d()) else {
                return;
            };
            *v_slot = inter.jet_vel;
            return;
        }
        let Some(p_slot) = pos.get_mut(thread::index_1d()) else {
            return;
        };
        let mut p = *p_slot;
        let Some(v_slot) = vel.get_mut(thread::index_1d()) else {
            return;
        };
        let mut v = *v_slot;
        let a = acc[i];
        let mut ax = a[0];
        let mut ay = a[1] - params.gravity * inter.gravity_on;
        // Wirbel: tangentiale Beschleunigung um die Maus (Radius 0.2 m).
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
        // Geschwindigkeitsbegrenzung gegen Explosionen.
        let vmax = 12.0;
        let s2 = v[0] * v[0] + v[1] * v[1];
        if s2 > vmax * vmax {
            let s = vmax / s2.sqrt();
            v[0] *= s;
            v[1] *= s;
        }
        p[0] += v[0] * params.dt;
        p[1] += v[1] * params.dt;
        // Rechteckwände mit Dämpfung.
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
        // Kreishindernis: herausschieben + radiale Reflexion.
        let ox = p[0] - inter.obstacle[0];
        let oy = p[1] - inter.obstacle[1];
        let orad = inter.obstacle_r;
        if ox * ox + oy * oy < orad * orad {
            let od = (ox * ox + oy * oy).sqrt().max(1e-6);
            let nx = ox / od;
            let ny = oy / od;
            p[0] = inter.obstacle[0] + nx * orad;
            p[1] = inter.obstacle[1] + ny * orad;
            let vn = v[0] * nx + v[1] * ny;
            if vn < 0.0 {
                v[0] -= (1.0 + damp) * vn * nx;
                v[1] -= (1.0 + damp) * vn * ny;
            }
        }
        let Some(p_slot) = pos.get_mut(thread::index_1d()) else {
            return;
        };
        *p_slot = p;
        let Some(v_slot) = vel.get_mut(thread::index_1d()) else {
            return;
        };
        *v_slot = v;
    }
}
