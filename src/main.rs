extern crate crossterm;

use std::fs::read as read_file;
use std::num::Wrapping;
use std::sync::Arc;
use std::sync::Mutex;
use std::thread;
//use std::time::Duration;
//use std::time::Instant;
use std::sync::mpsc::{Sender, Receiver};
use std::sync::mpsc;


use eframe::egui;
use egui::Color32;
use egui::Pos2;
use egui::Rect;
use egui::Rounding;
use egui::Sense;
use egui::Shape;
use egui::Stroke;
use egui::Vec2;

use gameboy::cpu;

fn main() {
    let state = cpu::RunningState::new();
    let memclone = state.get_memory_copy();

    //Loads the rom from file system to a vector
    let bytes = read_file("./Tetris2.gb").unwrap();
    //Loads the contents of the rom into the memory
    memclone.lock().unwrap()[..bytes.len()].clone_from_slice(bytes.as_slice());

    let (tx, rx): (Sender<u8>, Receiver<u8>) = mpsc::channel();

    thread::spawn(move || {
        gameboy::run(state, rx);
    });

    let native_options = eframe::NativeOptions::default();
    eframe::run_native(
        "My egui App",
        native_options,
        Box::new(|_cc| Box::new(MyEguiApp::new(memclone, tx))),
    )
    .expect("Something happened");

}

//#[derive(Default)]
struct MyEguiApp {
    memory: Arc<Mutex<Box<[u8; 65536]>>>,
    scanline : [[u8; 160]; 144],
    sender: Sender<u8>,

}

//type Sprite = [u8; 16];

impl MyEguiApp {
    fn new(memory: Arc<Mutex<Box<[u8; 65536]>>>, sender: Sender<u8>) -> Self {
        // Customize egui here with cc.egui_ctx.set_fonts and cc.egui_ctx.set_visuals.
        // Restore app state using cc.storage (requires the "persistence" feature).
        // Use the cc.gl (a glow::Context) to create graphics shaders and buffers that you can use
        // for e.g. egui::PaintCallback.
        Self { memory: memory, scanline: [[0; 160]; 144], sender }
    }

    fn read_memory(&self, pos: u16) -> u8 {
        (*self.memory.lock().unwrap())[usize::from(pos)]
    }

    fn write_memory(&mut self, pos: u16, data: u8) {
        (*self.memory.lock().unwrap())[usize::from(pos)] = data;
    }

    fn read_ly(&self) -> u8 {
        self.read_memory(0xFF44)
    }

    fn update_ly(&mut self, data: u8) {
        if data > 153 {
            self.write_memory(0xFF44, 0);
        } else {
            self.write_memory(0xFF44, data);
        }
    }

    fn read_lyc(&self) -> u8 {
        self.read_memory(0xFF45)
    }

    fn set_ly_equal(&mut self, lyc: u8, ly: u8) {
        let dat = self.read_memory(0xFF41);
        if lyc == ly {
            self.write_memory(0xFF41, dat | 0b0000_0010);
            if (dat & 0b0100_000) > 0 {//If LYC int select, request STAT Interrupt
                let int_flag = self.read_memory(0xFF0F);
                self.write_memory(0xFF0F, int_flag | 0b0000_0010);
            }
        } else {
            self.write_memory(0xFF41, dat & 0b1111_1101);   
            if lyc + 1 == ly {//Request VBlank Interrupt
                let int_flag = self.read_memory(0xFF0F);
                self.write_memory(0xFF0F, int_flag | 0b0000_0001);
            }
        }
    }

    //Read Background X value
    fn read_scx(&self) -> u8 {
        self.read_memory(0xFF43)
    }

    //Read Background Y value
    fn read_scy(&self) -> u8 {
        self.read_memory(0xFF42)
    }

    //Read Window X value
    fn read_wx(&self) -> u8 {
        self.read_memory(0xFF4B)
    }

    //Read Window Y value
    fn read_wy(&self) -> u8 {
        self.read_memory(0xFF4A)
    }

    /*fn get_sprite(&mut self, tile_number: u16) -> Sprite {
        let y = 0;
        let mut x = 0x8000;
        let mut sprite = [0u8; 16];
        if y == 1 {
            x +=0x800;
        }
        for i in 0..16 {
            sprite[i] = self.read_memory(x + (tile_number*16) + (i as u16));
        }
        sprite
    }*/

    fn tile_data_area(&self) -> u16 {
        let lcdc = self.read_memory(0xFF40);
        //0x8000
        if (lcdc & 0b0001_0000) > 0 {
            0x8000
        } else {
            0x8800
        }
    }

    //Gets the sprite line of a given sprite
    fn get_sprite_line(&mut self, tile_number: u8, tile_line: u8) -> [u8; 2] {
        let root_area = self.tile_data_area();
        let mut sprite = [0u8; 2];
        for i in 0..2 {
            sprite[i] = self.read_memory(
                root_area + ((tile_number as u16) * 16) + ((tile_line as u16) * 2) + (i as u16),
            );
        }
        let mut msb = 0;
        let mut lsb = 0;

        for x in 0..8 {
            if ((sprite[0] >> x) & 0x1) > 0 {
                msb += 2_u32.pow(7-x);
            }
            if ((sprite[1] >> x) & 0x1) > 0 {
                lsb += 2_u32.pow(7-x);   
            }
        }
        sprite[0] = msb as u8;
        sprite[1] = lsb as u8;
        //println!("Sprite:{:08b}, {:08b}", sprite[0], sprite[1]);
        sprite
    }

    //Returns the byte pair position for a given sprite line
    fn get_sprite_byte(&mut self, byte_pair: [u8; 2], pos: u8) -> u8 {
        (((byte_pair[1] >> pos) & 0x1) << 1) + ((byte_pair[0] >> pos) & 0x1)
    }

    fn window_tile_map_area(&self) -> u16 {
        let lcdc = self.read_memory(0xFF40);
        if (lcdc & 0b0100_0000) > 0 {
            0x9C00
        } else {
            0x9800
        }
    }

    fn bg_tile_map_area(&self) -> u16 {
        let lcdc = self.read_memory(0xFF40);
        if (lcdc & 0b0000_1000) > 0 {
            0x9C00
        } else {
            0x9800
        }
    }

    //Returns the sprite offset read in the 32x32 grid
    fn read_background(&mut self, x: u8, y: u8) -> u8 {
        let bg_root = self.bg_tile_map_area();
        self.read_memory(bg_root + (y as u16) * 32 + (x as u16))
    }

    //Returns the sprite offset read in the 32 x 32 grid
    fn read_window(&mut self, x: u8, y: u8) -> u8 {
        let win_root = self.window_tile_map_area();
        self.read_memory(win_root + (y as u16) * 32 + (x as u16))
    }

    fn read_oam(&mut self, object_number: u16) -> [u8; 4] {
        let mut object = [0u8; 4];
        let oam_root = 0xFE00;
        for i in 0..4 {
            object[i] = self.read_memory(oam_root + (object_number * 4) + (i as u16));
        }
        object
    }

    fn window_enable(&self) -> bool {
        let lcdc = self.read_memory(0xFF40);
        (lcdc & 0b0010_0000) > 0
    }

    fn obj_enable(&self) -> bool {
        let lcdc = self.read_memory(0xFF40);
        (lcdc & 0b0000_0010) > 0
    }

    fn ppu_enable(&self) -> bool {
        let lcdc = self.read_memory(0xFF40);
        (lcdc & 0b1000_0000) > 0
    }
}

impl eframe::App for MyEguiApp {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        ctx.input(|input| {     
            if input.key_pressed(egui::Key::ArrowUp) {
                //println!("Up");
                self.sender.send(2).expect("Could not send the data");
            } else if input.key_pressed(egui::Key::ArrowDown) {
                //println!("Down");
                self.sender.send(3).expect("Could not send the data");
            } else if input.key_pressed(egui::Key::ArrowLeft) {
                //println!("Left");
                self.sender.send(1).expect("Could not send the data");
            } else if input.key_pressed(egui::Key::ArrowRight) {
                //println!("Right");
                self.sender.send(0).expect("Could not send the data");
            } else if input.key_pressed(egui::Key::Z) {
                //println!("A");
                self.sender.send(4).expect("Could not send the data");
            } else if input.key_pressed(egui::Key::X) {
                //println!("B");
                self.sender.send(5).expect("Could not send the data");
            } else if input.key_pressed(egui::Key::Enter) {
                //println!("Start");
                self.sender.send(7).expect("Could not send the data");
            } else if input.key_pressed(egui::Key::Backspace) {
                //println!("Select");
                self.sender.send(6).expect("Could not send the data");
            } else if input.key_released(egui::Key::ArrowUp) {
                self.sender.send(10).expect("Could not send the data");
            } else if input.key_released(egui::Key::ArrowDown) {
                self.sender.send(11).expect("Could not send the data");
            } else if input.key_released(egui::Key::ArrowLeft) { 
                self.sender.send(9).expect("Could not send the data");
            } else if input.key_released(egui::Key::ArrowRight) { 
                self.sender.send(8).expect("Could not send the data");
            } else if input.key_released(egui::Key::Z) { 
                self.sender.send(12).expect("Could not send the data");
            } else if input.key_released(egui::Key::X) { 
                self.sender.send(13).expect("Could not send the data");
            } else if input.key_released(egui::Key::Backspace) { 
                self.sender.send(15).expect("Could not send the data");
            } else if input.key_released(egui::Key::Enter) {
                self.sender.send(14).expect("Could not send the data");
            }
        });

        if self.ppu_enable() {
            for _ in 0..154 {
                let ly = Wrapping(self.read_ly());
                let lyc = Wrapping(self.read_lyc());
                
                let scx = Wrapping(self.read_scx());
                let scy = Wrapping(self.read_scy());
    
                if ly.0 < 144 {
                    for i in 0..160 {
                        let safe_i = Wrapping(i);
                        //Retrieves the tile number, from the 32x32 tile map
                        let tile_number = self.read_background((scx + safe_i).0 / 8, (scy + ly).0 / 8);
                        
                        //Gets the line of the sprite from the sprite pointed to by the tile number
                        let sprite_line = self.get_sprite_line(tile_number, (scy + ly).0 % 8);
        
                        //Gets the color byte on the line of the sprite
                        self.scanline[usize::from(ly.0)][usize::from(i)] = self.get_sprite_byte(sprite_line, i % 8);

                        if self.window_enable() {
                            println!("Window enabled");
                            let seven = Wrapping(7);
                            let tile_number = self.read_window((scx + safe_i - seven).0 / 8, (scy + ly).0 / 8);
                            let _sprite_line = self.get_sprite_line(tile_number, (scy + ly).0 % 8);
                        }
                        
                    }
                }

                if self.obj_enable() {
                    for n in 0..40 {
                        let object = self.read_oam(n);
                        if object[0] > 15 && object[0] < 160 && object[1] > 7 && object[1] < 160 {
                            for x in 0..8 {
                                for y in 0..8 {
                                    let tile_number = object[2];
                                    let sprite_line = self.get_sprite_line(tile_number, y % 8);
                                    self.scanline[usize::from(object[0] + y - 16)][usize::from(object[1] + x - 8)] = self.get_sprite_byte(sprite_line, x % 8);
                                }
                            }
                        }
                    }           
                }

                self.update_ly((ly + Wrapping(1)).0);
                self.set_ly_equal(lyc.0, ly.0);
    
            } 
        }
        egui::CentralPanel::default().show(ctx, |ui| {
            let (_response, painter) =
                ui.allocate_painter(Vec2::new(ui.available_width(), 800.0), Sense::hover());
            let pixel = Rect { min: Pos2 { x: 7.0, y: 7.0 }, max: Pos2 { x: 10.0, y: 10.0 } };
            let no_rounding = Rounding { nw: 0.0, ne: 0.0, sw: 0.0, se: 0.0 };
            
            let c = [
                Color32::from_rgb(155, 188, 15),
                Color32::from_rgb(139, 172, 15),
                Color32::from_rgb(48, 98, 48),
                Color32::from_rgb(15, 56, 15),
            ];

            for y in 0..144 {
                for x in 0..160 {
                    painter.add(Shape::Rect(egui::epaint::RectShape {
                        rect: pixel.translate(Vec2 {
                            x: (x * 2) as f32,
                            y: (y * 2) as f32,
                        }),
                        rounding: no_rounding,
                        fill: c[usize::from(self.scanline[y][x])],
                        stroke: Stroke {
                            width: 0.0,
                            color: c[usize::from(self.scanline[y][x])],
                        },
                        fill_texture_id: egui::TextureId::Managed(0),
                        uv: Rect::ZERO,
                    }));
                }
            }
        });
        //This gives around 60 frames a second
        //let x = Instant::now() - start;
        //println!("Time Spent: {}", x.as_millis());
        ctx.request_repaint();
        //ctx.request_repaint_after(Duration::from_millis(16));
    }
}
