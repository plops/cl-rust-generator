//! GPU-Sortierkerne: Hash, Scan, Reorder, Permute (cuda-oxide-Single-Source).
//!
//! Baut pro Zeitschritt die Zellordnung auf: `k_hash` (Zell-Hash + atomares
//! Zählen), `k_scan` (exklusiver Präfix-Sum + Cursor-Init), `k_reorder`
//! (atomarer Scatter in Zellordnung), `k_permute` (physikalisches Umsortieren
//! von `pos`/`vel` in zellkontinuierliche Puffer). Alle Launches Blockgröße
//! 256, alle Schleifen `while` (primitiv, warp-freundlich).

use cuda_device::atomic::{AtomicOrdering, DeviceAtomicU32};
use cuda_device::{DisjointSlice, kernel, launch_bounds, launch_contract, thread};
use cuda_host::cuda_module;

use crate::spatial_grid::hash_cell;
use crate::types::SphParams;

/// Enthält alle Sortier-Device-Kernel; `cuda_host` generiert daraus
/// `LoadedModule` mit typisierten Launch-Funktionen.
#[cuda_module]
#[allow(clippy::not_unsafe_ptr_arg_deref)]
pub mod sort_device {
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

    /// Schritt 1d: permutiert `pos`/`vel` in Zellordnung.
    ///
    /// `pos_out[s] = pos[order[s]]`: ein Gather pro Schritt mit
    /// koaleszierenden Writes; danach arbeiten alle Physik-Kernel direkt auf
    /// sortierten Puffern ohne jede `order`-Indirektion.
    #[kernel]
    #[launch_bounds(256)]
    #[launch_contract(domain = 1, block = (256, 1, 1))]
    pub fn k_permute(
        order: &[u32],
        pos: &[[f32; 2]],
        vel: &[[f32; 2]],
        n: u32,
        mut pos_out: DisjointSlice<[f32; 2]>,
        mut vel_out: DisjointSlice<[f32; 2]>,
    ) {
        let i = thread::index_1d().get();
        if i >= n as usize {
            return;
        }
        let src = order[i] as usize;
        let p = pos[src];
        let v = vel[src];
        let Some(p_slot) = pos_out.get_mut(thread::index_1d()) else {
            return;
        };
        *p_slot = p;
        let Some(v_slot) = vel_out.get_mut(thread::index_1d()) else {
            return;
        };
        *v_slot = v;
    }
}
