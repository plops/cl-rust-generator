//! Gemeinsame Datentypen für Host und Device (AoS + Kernel-Parameter).
//!
//! `Particle` ist das hostseitige Partikel (Init, CPU-Backend, Renderer).
//! Die GPU arbeitet mit SoA-Buffern (`pos`, `vel`, …), deren Elementtypen
//! ebenfalls hier liegen. Alle Typen sind `Pod` für typsichere Transfers.

use bytemuck::{Pod, Zeroable};

/// Ein SPH-Partikel (hostseitig, Array-of-Structs, 32 Byte).
#[repr(C)]
#[derive(Clone, Copy, Debug, PartialEq, Pod, Zeroable)]
pub struct Particle {
    /// Position in Metern, Domäne `[0, W] × [0, H]`, y zeigt nach oben.
    pub pos: [f32; 2],
    /// Geschwindigkeit in m/s.
    pub vel: [f32; 2],
    /// Kraft in N (Druck + Viskosität, ohne Gravitation).
    pub force: [f32; 2],
    /// Dichte in kg/m³.
    pub density: f32,
    /// Druck in Pa (leicht negativ bis −Tension-Cap, siehe `sph_math::pressure`).
    pub pressure: f32,
}

impl Particle {
    /// Ruhendes Partikel an `pos` mit Ruhedichte (Druck folgt aus EOS).
    pub fn at_rest(pos: [f32; 2], rest_density: f32) -> Self {
        Self {
            pos,
            vel: [0.0, 0.0],
            force: [0.0, 0.0],
            density: rest_density,
            pressure: 0.0,
        }
    }
}

/// Physikalische + numerische SPH-Parameter, per Wert an GPU-Kernel übergeben.
#[repr(C)]
#[derive(Clone, Copy, Debug, Pod, Zeroable)]
pub struct SphParams {
    /// Partikelmasse in kg (aus Ruhe-Dichte × Anfangsfläche).
    pub mass: f32,
    /// Glättungslänge h in m (= Zellgröße des Uniform Grid).
    pub h: f32,
    /// Ruhedichte ρ₀ in kg/m³.
    pub rest_density: f32,
    /// Gas-Steifigkeit k der Zustandsgleichung.
    pub stiffness: f32,
    /// Dynamische Viskosität μ in Pa·s.
    pub viscosity: f32,
    /// Zeitschritt dt in s.
    pub dt: f32,
    /// Gravitationsbetrag in m/s² (Richtung −y, 0 = abgeschaltet).
    pub gravity: f32,
    /// Reflexionsdämpfung an Wänden/Hindernis (0.2 = weiches Zusammenlaufen).
    pub wall_damping: f32,
    /// Partikelanzahl N (Kernel-Abbruchschranke).
    pub num_particles: u32,
    /// Grid-Breite in Zellen.
    pub grid_w: u32,
    /// Grid-Höhe in Zellen.
    pub grid_h: u32,
    /// Domänenbreite in m.
    pub domain_w: f32,
    /// Domänenhöhe in m.
    pub domain_h: f32,
}

/// Interaktion pro Frame, per Wert an den Integrations-Kernel übergeben.
#[repr(C)]
#[derive(Clone, Copy, Debug, Pod, Zeroable)]
pub struct InteractParams {
    /// Mausposition in Weltkoordinaten (m).
    pub mouse: [f32; 2],
    /// 0 = keine, 1 = Wirbel an Maus, 2 = Strahl an Maus.
    pub mouse_mode: u32,
    /// Hindernis-Mittelpunkt in Weltkoordinaten (m).
    pub obstacle: [f32; 2],
    /// Hindernis-Radius in m.
    pub obstacle_r: f32,
    /// Gravitation an (1.0) / aus (0.0), multiplikativ.
    pub gravity_on: f32,
    /// Strahl: erster recycelter Partikelindex.
    pub jet_start: u32,
    /// Strahl: Anzahl recycelter Partikel (0 = inaktiv).
    pub jet_count: u32,
    /// Strahl: Injektionsgeschwindigkeit in m/s.
    pub jet_vel: [f32; 2],
}

impl InteractParams {
    /// Neutral: kein Mauseffekt, Hindernis mittig-rechts, Gravitation an.
    pub fn neutral(domain_w: f32, domain_h: f32) -> Self {
        Self {
            mouse: [0.5 * domain_w, 0.5 * domain_h],
            mouse_mode: 0,
            obstacle: [0.7 * domain_w, 0.45 * domain_h],
            obstacle_r: 0.08,
            gravity_on: 1.0,
            jet_start: 0,
            jet_count: 0,
            jet_vel: [2.5, 0.5],
        }
    }
}

/// Geometrie des Uniform Grid (Zellgröße = Glättungslänge h).
#[derive(Clone, Copy, Debug)]
pub struct GridMeta {
    /// Zellen in x.
    pub w: u32,
    /// Zellen in y.
    pub h: u32,
    /// Zellgröße in m.
    pub cell: f32,
    /// Kehrwert der Zellgröße (spart Divisionen im Kernel).
    pub inv_cell: f32,
}

impl GridMeta {
    /// Baut das Grid für die Domäne; panikt bei entarteten Maßen.
    pub fn new(domain_w: f32, domain_h: f32, h: f32) -> Self {
        assert!(domain_w > 0.0 && domain_h > 0.0 && h > 0.0);
        let w = (domain_w / h).ceil() as u32;
        let h_cells = (domain_h / h).ceil() as u32;
        assert!(w > 0 && h_cells > 0);
        Self {
            w,
            h: h_cells,
            cell: h,
            inv_cell: 1.0 / h,
        }
    }

    /// Gesamtzellenzahl.
    pub fn num_cells(&self) -> usize {
        self.w as usize * self.h as usize
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn particle_layout_ist_32_byte_pod() {
        assert_eq!(size_of::<Particle>(), 32);
        assert_eq!(align_of::<Particle>(), 4);
        let p = Particle::at_rest([0.1, 0.2], 1000.0);
        let bytes = bytemuck::bytes_of(&p);
        let back: &Particle = bytemuck::from_bytes(bytes);
        assert_eq!(*back, p);
    }

    #[test]
    fn grid_meta_rundet_zellen_auf() {
        let g = GridMeta::new(1.6, 1.0, 0.04);
        assert_eq!((g.w, g.h), (40, 25));
        assert_eq!(g.num_cells(), 1000);
    }
}
