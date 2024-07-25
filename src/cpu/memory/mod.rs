use super::registers::Registers;
use std::num::Wrapping;
use std::sync::Arc;
use std::sync::Mutex;

//TODO:DMA OAM Transfer
pub struct Memory {
  pub data: Arc<Mutex<Box<[u8; 0x10000]>>>,
}

fn increment(program_counter: &mut u16) {
  let value = Wrapping(*program_counter);
  let one = Wrapping(1u16);
  *program_counter = (value + one).0;
}


impl Memory {

  pub fn new() -> Memory {
    let mut mem = Memory {
      data: Arc::new(Mutex::new(Box::new([0; 0x10000]))),
    };
    mem.write_memory(0xff00, 0xCF);
    mem.write_memory(0xff01, 0x00);
    mem.write_memory(0xff02, 0x7E);
    mem.write_memory(0xff04, 0xAB);
    mem.write_memory(0xff05, 0x00);
    mem.write_memory(0xff06, 0x00);
    mem.write_memory(0xff07, 0xF8);
    mem.write_memory(0xff0f, 0xE1);
    mem.write_memory(0xff10, 0x80);
    mem.write_memory(0xff11, 0xBF);
    mem.write_memory(0xff12, 0xF3);
    mem.write_memory(0xff13, 0xFF);
    mem.write_memory(0xff14, 0xBF);
    mem.write_memory(0xff16, 0x3F);
    mem.write_memory(0xff17, 0x00);
    mem.write_memory(0xff18, 0xFF);
    mem.write_memory(0xff19, 0xBF);
    mem.write_memory(0xff1A, 0x7F);
    mem.write_memory(0xff1B, 0xFF);
    mem.write_memory(0xff1C, 0x9F);
    mem.write_memory(0xff1D, 0xFF);
    mem.write_memory(0xff1E, 0xBF);
    mem.write_memory(0xff20, 0xFF);
    mem.write_memory(0xff21, 0x00);
    mem.write_memory(0xff22, 0x00);
    mem.write_memory(0xff23, 0xBF);
    mem.write_memory(0xff24, 0x77);
    mem.write_memory(0xff25, 0xF3);
    mem.write_memory(0xff26, 0xF1);
    mem.write_memory(0xff40, 0x91);
    mem.write_memory(0xff41, 0x85);
    mem.write_memory(0xff42, 0x00);
    mem.write_memory(0xff43, 0x00);
    mem.write_memory(0xff45, 0x00);
    mem.write_memory(0xff46, 0xFF);
    mem.write_memory(0xff47, 0xFC);
    mem.write_memory(0xff48, 0xFF);
    mem.write_memory(0xff49, 0xFF);
    mem.write_memory(0xff4A, 0x00);
    mem.write_memory(0xff4B, 0x00);
    mem.write_memory(0xffFF, 0x00);
    mem
  }

  pub fn read_memory_from_pc(&self, registers: &mut Registers) -> u8 {
    let position = registers.pc;

    increment(&mut registers.pc);
    self.read_memory(position)
  }

  pub fn read_memory(&self, position: u16) -> u8 {
    (*self.data.lock().unwrap())[usize::from(position)]
  }

  pub fn perform_data_transfer(&mut self) {
    let msb = self.read_memory(0xFF46);
    for i in 0..0x100 {
      let source = self.read_memory(((msb as u16) << 8) + i);
      self.write_memory(usize::from(0xFE00 + i), source);
    }
  }

  pub fn update_joystick(&mut self, data: u8) {
    (*self.data.lock().unwrap())[0xff00] = data;
  }

  //TODO: Some memory should not be writeable
  pub fn write_memory(&mut self, position: usize, data: u8) {
    
    if position > 0x7fff {
      (*self.data.lock().unwrap())[position] = data;
    }
    
    if position == 0xFF46 {
      self.perform_data_transfer();
    }
  }
}

