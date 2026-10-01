//! winit-Anwendung: Fenster, Scan-Thread, Hover-Picking, Render-Steuerung.
//!
//! Der Dateisystem-Scan läuft in einem Hintergrund-Thread; die GUI fragt das
//! Ergebnis in `about_to_wait` ab. Gerendert wird nur bei Bedarf (On Demand):
//! Resize, Hover-Wechsel oder frische Scan-Daten fordern einen Redraw an.

use std::path::PathBuf;
use std::sync::{Arc, mpsc};
use std::thread;

use winit::application::ApplicationHandler;
use winit::dpi::LogicalSize;
use winit::event::{ElementState, WindowEvent};
use winit::event_loop::{ActiveEventLoop, ControlFlow, EventLoopProxy};
use winit::keyboard::{KeyCode, PhysicalKey};
use winit::window::{Window, WindowAttributes, WindowId};

use crate::layout::{pick, squarify};
use crate::render::{FLAG_FLAT, FLAG_HI, RectInstance, WgpuState};
use crate::scan::scan_tree;
use crate::text::layout_line;
use crate::types::{Node, Rect, format_bytes};

/// Höhe der Header-Leiste in Pixeln.
pub const HEADER_H: f32 = 36.0;
/// Header-Hintergrund (sRGB 0..1).
const HEADER_BG: (f32, f32, f32) = (0.05, 0.06, 0.08);
/// Text-Skalierung (8×8-Glyphen → 16 px) und Abstände.
const TEXT_SCALE: f32 = 2.0;
const TEXT_PAD_X: f32 = 14.0;
/// Rechtecke unter 1 px werden nicht gezeichnet (aber noch gepickt).
const MIN_DRAW_PX: f32 = 1.0;

/// Fenster-Anwendung (wird an `EventLoop::run_app` übergeben).
pub struct App {
    target: PathBuf,
    proxy: EventLoopProxy<()>,
    tx: Option<mpsc::Sender<Node>>,
    rx: mpsc::Receiver<Node>,
    scan_started: bool,
    window: Option<Arc<Window>>,
    gpu: Option<WgpuState>,
    root: Option<Node>,
    cursor: Option<(f32, f32)>,
    hover_key: Option<(PathBuf, u64)>,
    size: (u32, u32),
    fatal: Option<String>,
}

impl App {
    /// Bereitet den Scan vor; der Thread startet erst in `resumed` und
    /// weckt die Loop per `proxy` bei Fertigstellung auf.
    pub fn new(target: PathBuf, proxy: EventLoopProxy<()>) -> Self {
        let (tx, rx) = mpsc::channel();
        Self {
            target,
            proxy,
            tx: Some(tx),
            rx,
            scan_started: false,
            window: None,
            gpu: None,
            root: None,
            cursor: None,
            hover_key: None,
            size: (1280, 800),
            fatal: None,
        }
    }

    /// Entnimmt einen fatalen Initialisierungsfehler (für den Exit-Code).
    pub fn take_error(&mut self) -> Option<String> {
        self.fatal.take()
    }

    /// Layout bei (w,h): Canvas unter dem Header, Upload zu GPU.
    fn relayout(&mut self) {
        let (w, h) = self.size;
        let canvas_h = (h as f32 - HEADER_H).max(1.0);
        let canvas = Rect::new(0.0, HEADER_H, w as f32, canvas_h);
        let mut rects = vec![
            RectInstance {
                x: 0.0,
                y: 0.0,
                w: w as f32,
                h: HEADER_H,
                r: HEADER_BG.0,
                g: HEADER_BG.1,
                b: HEADER_BG.2,
                flags: FLAG_FLAT,
            },
            // Canvas-Hintergrund: Regionen ohne gezeichnete Rechtecke
            // (Subpixel-Skip, MAX_RECTS-Kappe) zeigen Verzeichnisfarbe
            // statt der Clear-Farbe.
            RectInstance {
                x: canvas.x,
                y: canvas.y,
                w: canvas.w,
                h: canvas_h,
                r: crate::color::DIR_COLOR.r,
                g: crate::color::DIR_COLOR.g,
                b: crate::color::DIR_COLOR.b,
                flags: FLAG_FLAT,
            },
        ];
        if let Some(root) = &mut self.root {
            root.rect = canvas;
            squarify(&mut root.children, canvas);
            collect_rects(&root.children, &mut rects);
        }
        if let Some(gpu) = &mut self.gpu {
            gpu.upload_rects(&rects);
        }
        // Hover nach Layout neu auflösen (Knoten können gewandert sein).
        self.hover_key = None;
        self.refresh_hover();
    }

    /// Hover-Text für die Headerzeile (Hover > Zusammenfassung > Scan-Status).
    fn header_text(&self) -> String {
        if let Some((x, y)) = self.cursor
            && let Some(root) = &self.root
            && let Some(hit) = pick(&root.children, x, y)
        {
            return format!("{} ({})", hit.path.display(), format_bytes(hit.size));
        }
        match &self.root {
            Some(root) => format!(
                "{}: {} ({} Einträge)",
                self.target.display(),
                format_bytes(root.size),
                root.children.len()
            ),
            None => format!("Scanning {} ...", self.target.display()),
        }
    }

    /// Aktualisiert Highlight, Header-Glyphen und Fenstertitel bei Hover-Wechsel.
    fn refresh_hover(&mut self) {
        let hit = self.cursor.and_then(|(x, y)| {
            self.root
                .as_ref()
                .and_then(|root| pick(&root.children, x, y))
        });
        let key = hit.map(|n| (n.path.clone(), n.size));
        if key == self.hover_key {
            return;
        }
        self.hover_key = key;
        let hl = hit.map(|n| RectInstance {
            x: n.rect.x,
            y: n.rect.y,
            w: n.rect.w,
            h: n.rect.h,
            r: n.color.r,
            g: n.color.g,
            b: n.color.b,
            flags: FLAG_HI,
        });
        let text = self.header_text();
        if let Some(gpu) = &mut self.gpu {
            gpu.set_highlight(hl);
            gpu.upload_glyphs(&layout_line(
                &text,
                TEXT_PAD_X,
                (HEADER_H - 16.0) / 2.0,
                TEXT_SCALE,
                self.size.0 as f32 - 2.0 * TEXT_PAD_X,
            ));
        }
        if let Some(window) = &self.window {
            window.set_title(&truncate(&text, 256));
            window.request_redraw();
        }
    }
}

/// Sammelt Zeichen-Rechtecke in Breitensuche (Eltern zuerst, Kinder darüber).
///
/// Breitensuche statt Tiefensuche: Bei riesigen Bäumen kappt `MAX_RECTS`
/// sonst ganze hintere Teilbäume — deren Top-Level-Rechtecke (sortiert
/// klein = unten rechts) bleiben grau, obwohl Hover sie findet. In
/// Breitensuche stehen alle Top-Level-Rechtecke vorne; die Kappe trifft
/// nur noch tiefes Detail, unter dem die Elternfarbe sichtbar bleibt.
fn collect_rects(nodes: &[Node], out: &mut Vec<RectInstance>) {
    use std::collections::VecDeque;
    let mut queue: VecDeque<&Node> = nodes.iter().collect();
    while let Some(node) = queue.pop_front() {
        if out.len() >= crate::render::MAX_RECTS {
            return;
        }
        if node.rect.w >= MIN_DRAW_PX && node.rect.h >= MIN_DRAW_PX {
            out.push(RectInstance {
                x: node.rect.x,
                y: node.rect.y,
                w: node.rect.w,
                h: node.rect.h,
                r: node.color.r,
                g: node.color.g,
                b: node.color.b,
                flags: 0,
            });
        }
        queue.extend(node.children.iter());
    }
}

/// Kürzt auf maximal `max` Zeichen (Fenster-Titel).
fn truncate(text: &str, max: usize) -> String {
    if text.chars().count() <= max {
        text.to_string()
    } else {
        text.chars().take(max).collect()
    }
}

impl ApplicationHandler for App {
    fn resumed(&mut self, event_loop: &ActiveEventLoop) {
        if self.window.is_some() {
            return;
        }
        let window = match event_loop.create_window(
            WindowAttributes::default()
                .with_title("Treemap")
                .with_inner_size(LogicalSize::new(1280, 800)),
        ) {
            Ok(window) => Arc::new(window),
            Err(err) => {
                self.fatal = Some(format!("create_window: {err:?}"));
                event_loop.exit();
                return;
            }
        };
        let size = window.inner_size();
        self.size = (size.width.max(1), size.height.max(1));
        match WgpuState::new(window.clone(), self.size.0, self.size.1) {
            Ok(mut gpu) => {
                gpu.resize(self.size.0, self.size.1);
                self.gpu = Some(gpu);
                self.window = Some(window);
                self.relayout();
                // Scan-Thread erst hier starten: `send_event` weckt die
                // Loop (`ControlFlow::Wait`) bei Fertigstellung auf, damit
                // `about_to_wait` den Baum ohne Mausbewegung abholt.
                if !self.scan_started {
                    self.scan_started = true;
                    if let Some(tx) = self.tx.take() {
                        let proxy = self.proxy.clone();
                        let target = self.target.clone();
                        thread::spawn(move || {
                            let root = scan_tree(&target);
                            let _ = tx.send(root);
                            let _ = proxy.send_event(());
                        });
                    }
                }
            }
            Err(err) => {
                self.fatal = Some(err);
                event_loop.exit();
            }
        }
    }

    fn window_event(
        &mut self,
        event_loop: &ActiveEventLoop,
        _window_id: WindowId,
        event: WindowEvent,
    ) {
        match event {
            WindowEvent::CloseRequested => event_loop.exit(),
            WindowEvent::Resized(size) => {
                self.size = (size.width.max(1), size.height.max(1));
                if let Some(gpu) = &mut self.gpu {
                    gpu.resize(self.size.0, self.size.1);
                }
                self.relayout();
                if let Some(window) = &self.window {
                    window.request_redraw();
                }
            }
            WindowEvent::CursorMoved { position, .. } => {
                self.cursor = Some((position.x as f32, position.y as f32));
                self.refresh_hover();
            }
            WindowEvent::CursorLeft { .. } => {
                self.cursor = None;
                self.refresh_hover();
            }
            WindowEvent::RedrawRequested => {
                if let Some(gpu) = &self.gpu
                    && let Err(err) = gpu.render()
                {
                    eprintln!("render: {err}");
                }
            }
            WindowEvent::KeyboardInput { event, .. }
                if event.physical_key == PhysicalKey::Code(KeyCode::Escape)
                    && event.state == ElementState::Pressed =>
            {
                event_loop.exit();
            }
            _ => {}
        }
    }

    fn about_to_wait(&mut self, event_loop: &ActiveEventLoop) {
        let mut dirty = false;
        while let Ok(root) = self.rx.try_recv() {
            self.root = Some(root);
            dirty = true;
        }
        if dirty {
            self.relayout();
            if let Some(window) = &self.window {
                window.request_redraw();
            }
        }
        event_loop.set_control_flow(ControlFlow::Wait);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn truncate_short_and_long() {
        assert_eq!(truncate("abc", 10), "abc");
        assert_eq!(truncate("abcdef", 4), "abcd");
    }

    fn flat_node(name: &str, w: f32, children: Vec<Node>) -> Node {
        use crate::types::{Rect, Rgb};
        Node {
            path: PathBuf::from(name),
            size: 10,
            is_dir: false,
            children,
            rect: Rect::new(0.0, 0.0, w, 10.0),
            color: Rgb::new(1.0, 0.0, 0.0),
        }
    }

    #[test]
    fn collect_is_breadth_first() {
        // Regressionstest: Bei Tiefensuche kappt MAX_RECTS ganze hintere
        // Regionen (unten rechts grau), obwohl Hover dort trifft. In
        // Breitensuche stehen alle Top-Level-Rechtecke vorne.
        let nodes = vec![
            flat_node("A", 30.0, vec![flat_node("a1", 10.0, vec![])]),
            flat_node("B", 20.0, vec![flat_node("b1", 11.0, vec![])]),
            flat_node("C", 15.0, vec![]),
        ];
        let mut out = Vec::new();
        collect_rects(&nodes, &mut out);
        let widths: Vec<f32> = out.iter().map(|r| r.w).collect();
        assert_eq!(widths, vec![30.0, 20.0, 15.0, 10.0, 11.0]);
    }

    #[test]
    fn collect_respects_cap() {
        use crate::render::MAX_RECTS;
        let nodes: Vec<Node> = (0..MAX_RECTS + 1000)
            .map(|i| flat_node(&format!("f{i}"), 5.0, vec![]))
            .collect();
        let mut out = Vec::new();
        collect_rects(&nodes, &mut out);
        assert_eq!(out.len(), MAX_RECTS);
    }

    #[test]
    fn collect_skips_subpixel() {
        use crate::types::{Rect, Rgb};
        let nodes = vec![
            Node {
                path: PathBuf::from("big"),
                size: 100,
                is_dir: false,
                children: Vec::new(),
                rect: Rect::new(0.0, 0.0, 10.0, 10.0),
                color: Rgb::new(1.0, 0.0, 0.0),
            },
            Node {
                path: PathBuf::from("tiny"),
                size: 1,
                is_dir: false,
                children: Vec::new(),
                rect: Rect::new(0.0, 0.0, 0.5, 0.5),
                color: Rgb::new(0.0, 1.0, 0.0),
            },
        ];
        let mut out = Vec::new();
        collect_rects(&nodes, &mut out);
        assert_eq!(out.len(), 1);
        assert_eq!((out[0].w, out[0].h), (10.0, 10.0));
    }
}
