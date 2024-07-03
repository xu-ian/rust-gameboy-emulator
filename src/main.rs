use std::sync::Arc;
use std::sync::Mutex;
use std::fs::read;

use egui::Vec2;
use eframe::egui;
use egui::Shape;
use egui::Color32;
use egui::Stroke;
use egui::Pos2;
use egui::Sense;
use egui::Rect;
use egui::Rounding;

use gameboy::cpu;



fn main() {
    let state = cpu::RunningState::new();
    let memclone = state.get_memory_copy();

    //Loads the rom from file system to a vector
    let bytes = read("./Tetris.gb").unwrap();
    //Loads the contents of the rom into the memory
    memclone.lock().unwrap()[..bytes.len()].clone_from_slice(bytes.as_slice());

    //println!("Copied Memory: {:#04x}", memclone.lock().unwrap()[0x7eb0]);
    //println!("Read Rom: {:#04x}", bytes[0x7eb0]);
    //println!("From state: {:#04x}", state.get_memory_copy().lock().unwrap()[0x7eb0]);
    gameboy::run(state);

    
    let native_options = eframe::NativeOptions::default();
    eframe::run_native("My egui App", native_options, 
        Box::new(|_cc| Box::new(MyEguiApp::new(memclone)))).expect("Something happened");
}

//#[derive(Default)]
struct MyEguiApp {
    _memory: Arc<Mutex<Box<[u8; 65536]>>>
}


impl MyEguiApp {
    fn new(memory: Arc<Mutex<Box<[u8; 65536]>>>) -> Self {
        // Customize egui here with cc.egui_ctx.set_fonts and cc.egui_ctx.set_visuals.
        // Restore app state using cc.storage (requires the "persistence" feature).
        // Use the cc.gl (a glow::Context) to create graphics shaders and buffers that you can use
        // for e.g. egui::PaintCallback.
        Self {
            _memory: memory
        }
    }
}



impl eframe::App for MyEguiApp {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        egui::CentralPanel::default().show(ctx, |ui| {
            let (_response, painter) = ui.allocate_painter(
                Vec2::new(ui.available_width(), 300.0),
                Sense::hover()
            );
            
            let pixel = Rect {
                min: Pos2 {x:4.0, y: 4.0},
                max: Pos2 {x: 8.0, y: 8.0},
            };
            let no_rounding = Rounding { nw: 0.0, ne: 0.0, sw: 0.0, se: 0.0 }; 
            let c = [Color32::from_rgb(155,188,15),
                                    Color32::from_rgb(139,172,15),
                                    Color32::from_rgb(48,98,48),
                                    Color32::from_rgb(15,56,15)];
            println!("Frame: {}", ctx.frame_nr());

            for x in 1..160 {
                for y in 1..144 {
                    painter.add(Shape::Rect(
                        egui::epaint::RectShape {
                            rect: pixel.translate(Vec2 {x: (x*4) as f32, y: (y*4) as f32}),
                            rounding: no_rounding,
                            fill: c[(x+y+((ctx.frame_nr()as usize) % 4))%4],
                            stroke: Stroke {
                                width: 0.0,
                                color: c[(x+y+((ctx.frame_nr()as usize) % 4))%4]
                            },
                            fill_texture_id: egui::TextureId::Managed(0),
                            uv: Rect::ZERO,
                        }
                    ));
        
                }
            }
        });
    }
}