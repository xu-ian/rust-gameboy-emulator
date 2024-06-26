use super::registers::Registers;
use std::num::Wrapping;
use std::sync::Arc;
use std::sync::Mutex;
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
    mem.write_memory(0xff05, 0x00);
    mem.write_memory(0xff06, 0x00);
    mem.write_memory(0xff07, 0x00);
    mem.write_memory(0xff10, 0x80);
    mem.write_memory(0xff11, 0xBF);
    mem.write_memory(0xff12, 0xF3);
    mem.write_memory(0xff14, 0xBF);
    mem.write_memory(0xff16, 0x3F);
    mem.write_memory(0xff17, 0x00);
    mem.write_memory(0xff19, 0xBF);
    mem.write_memory(0xff1A, 0x7F);
    mem.write_memory(0xff1B, 0xFF);
    mem.write_memory(0xff1C, 0x9F);
    mem.write_memory(0xff1E, 0xBF);
    mem.write_memory(0xff20, 0xFF);
    mem.write_memory(0xff21, 0x00);
    mem.write_memory(0xff22, 0x00);
    mem.write_memory(0xff23, 0xBF);
    mem.write_memory(0xff24, 0x77);
    mem.write_memory(0xff25, 0xF3);
    mem.write_memory(0xff26, 0xF1);
    mem.write_memory(0xff40, 0x91);
    mem.write_memory(0xff42, 0x00);
    mem.write_memory(0xff43, 0x00);
    mem.write_memory(0xff45, 0x00);
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

  //TODO: Some memory should not be writeable
  pub fn write_memory(&mut self, position: usize, data: u8) {
    
    (*self.data.lock().unwrap())[position] = data;
  }
}

