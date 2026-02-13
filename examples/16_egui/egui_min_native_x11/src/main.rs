use eframe::egui;

fn main() -> eframe::Result<()> {
    let native_options = eframe::NativeOptions {
        viewport: egui::ViewportBuilder::default().with_inner_size([320.0, 240.0]),
        ..Default::default()
    };
    
    eframe::run_native(
        "Gentoo X11 Minimal",
        native_options,
        Box::new(|_cc| Ok(Box::new(MinimalApp::default()))),
    )
}

struct MinimalApp {
    name: String,
}

impl Default for MinimalApp {
    fn default() -> Self {
        Self { name: "Gentoo User".to_owned() }
    }
}

impl eframe::App for MinimalApp {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        egui::CentralPanel::default().show(ctx, |ui| {
            ui.heading("Minimal egui on X11");
            ui.horizontal(|ui| {
                ui.label("Your name: ");
                ui.text_edit_singleline(&mut self.name);
            });
            if ui.button("Click Me").clicked() {
                println!("Hello, {}!", self.name);
            }
        });
    }
}
