use super::registers::Registers;
use std::num::Wrapping;
pub struct Memory {
  pub data: Box<[u8; 0x10000]>,
}

fn increment(program_counter: &mut u16) {
  let value = Wrapping(*program_counter);
  let one = Wrapping(1u16);
  *program_counter = (value + one).0;
}


impl Memory {
  pub fn read_memory_from_pc(&self, registers: &mut Registers) -> u8 {
    let position = registers.pc;

    increment(&mut registers.pc);
    self.read_memory(position)
  }

  pub fn read_memory(&self, position: u16) -> u8 {
    (*self.data)[usize::from(position)]
  }

  //TODO: Some memory should not be writeable
  pub fn write_memory(&mut self, position: usize, data: u8) {
    (*self.data)[position] = data;
  }
}

