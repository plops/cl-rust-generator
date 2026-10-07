//! GPU-Sortierkerne: Hash, Scan, Reorder, Permute (cuda-oxide-Single-Source).
//!
//! Baut pro Zeitschritt die Zellordnung auf: `k_hash` (Zell-Hash + atomares
//! Zählen), `k_scan` (exklusiver Präfix-Sum + Cursor-Init), `k_reorder`
//! (atomarer Scatter in Zellordnung), `k_permute` (physikalisches Umsortieren
//! von `pos`/`vel` in zellkontinuierliche Puffer). Alle Launches Blockgröße
//! 256, alle Schleifen `while` (primitiv, warp-freundlich).

use cuda_device::atomic::{AtomicOrdering, DeviceAtomicU32};
use cuda_device::{DisjointSlice, SharedArray, kernel, launch_bounds, launch_contract, thread};
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

    /// Schritt 1b: paralleler exklusiver Präfix-Sum über Zellzählungen.
    ///
    /// Kooperativer Ein-Block-Chunk-Scan mit allen 256 Threads: Jeder Thread
    /// summiert sein Zell-Chunk in Shared Memory, Thread 0 bildet den Präfix
    /// über die Chunk-Summen, danach schreibt jeder Thread den exklusiven
    /// Scan seines Chunks nach `cell_start` und initialisiert den
    /// `counts`-Scatter-Cursor für `k_reorder`. Skaliert auf beliebiges
    /// `ncell` (auch feine Grids und 3D).
    #[kernel]
    #[launch_bounds(256)]
    #[launch_contract(domain = 1, block = (256, 1, 1))]
    pub fn k_scan(counts: *mut u32, ncell: u32, cell_start: *mut u32) {
        static mut CHUNK: SharedArray<u32, 256> = SharedArray::UNINIT;
        static mut TOTAL: SharedArray<u32, 1> = SharedArray::UNINIT;
        // Raw-Pointer statt Indexing: kein `static_mut_refs`-Lint.
        let chunk_ptr = unsafe { SharedArray::as_raw_mut_ptr(&raw mut CHUNK) };
        let total_ptr = unsafe { SharedArray::as_raw_mut_ptr(&raw mut TOTAL) };
        let tid = thread::threadIdx_x() as usize;
        let n = ncell as usize;
        let span = n.div_ceil(256);
        let begin = tid * span;
        let mut end = begin + span;
        if end > n {
            end = n;
        }
        // Phase 1: lokale Chunk-Summe (atomare Loads wie bisher).
        let mut sum = 0u32;
        let mut c = begin;
        while c < end {
            let slot = unsafe { DeviceAtomicU32::from_ptr(counts.add(c)) };
            sum += slot.load(AtomicOrdering::Relaxed);
            c += 1;
        }
        unsafe {
            chunk_ptr.add(tid).write(sum);
        }
        thread::sync_threads();
        // Phase 2: exklusiver Präfix über die 256 Chunk-Summen (Thread 0).
        if tid == 0 {
            let mut run = 0u32;
            let mut t = 0usize;
            while t < 256 {
                let v = unsafe { chunk_ptr.add(t).read() };
                unsafe {
                    chunk_ptr.add(t).write(run);
                }
                run += v;
                t += 1;
            }
            unsafe {
                total_ptr.write(run);
            }
        }
        thread::sync_threads();
        // Phase 3: exklusiver Scan im eigenen Chunk + Cursor-Init.
        let mut run = unsafe { chunk_ptr.add(tid).read() };
        let mut c = begin;
        while c < end {
            let slot = unsafe { DeviceAtomicU32::from_ptr(counts.add(c)) };
            let cnt = slot.load(AtomicOrdering::Relaxed);
            unsafe {
                cell_start.add(c).write(run);
            }
            slot.store(run, AtomicOrdering::Relaxed);
            run += cnt;
            c += 1;
        }
        if tid == 0 {
            let total = unsafe { total_ptr.read() };
            unsafe {
                cell_start.add(n).write(total);
            }
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
