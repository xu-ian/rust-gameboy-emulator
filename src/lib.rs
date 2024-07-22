use cpu::RunningState;
use cpu::memory::Memory;
use std::time::Duration;
use std::time::Instant;
use std::sync::mpsc::Receiver;

use crossterm::event::read;

pub mod cpu;

pub fn run(mut state: RunningState, rx: Receiver<i32>) {
  
  let mut counter:u128 = 0;
  //let start = Instant::now();
  let mut scount = 0;
  let mut input_countdown = 0;
  let mut input_type = 0x3F;

  let mut breakpoint = false;

  loop {
  //state.logging[0] = true;
  //state.logging[1] = true;
      let s = Instant::now();
      state.next();
      
      
      counter+=1;

      //if breakpoint {
      //  println!("Position: {:04x}", state.registers.pc);
      //  println!("FF00: {:04x}", state.memory.read_memory(0xff00));
      //  println!("FF80: {:04x}", state.memory.read_memory(0xff80));
      //  read().unwrap();
      //}

      //if state.registers.pc == 0x29a6 {
      //  breakpoint = true;
      //  state.interrupts = false;
      //}

      if state.registers.pc == 0x41e {
        //state.dump_oam();
        //state.dump_tilemap();
      }

      if counter % 3_000_000 == 0 {
        //state.dump_registers();
        //let x = Instant::now() - start;
        //println!("Time Spent: {} ms", x.as_micros());
        //println!("Total action time: {} micros, Average action time: {} micros",scount, scount / 3_000_000);
        scount = 0;
        //state.dump_tilemap();
      }

      let x = Instant::now() - s;
      scount += x.as_micros();

      match rx.try_recv() {
        Ok(x) => {
          input_countdown = 75000;
          input_type = x;
        },
        Err(_) => (),
      }

      if input_countdown > 0 {
        state.memory.write_memory(0xFF00, input_type as u8);
        input_countdown -= 1;
      } else {
        state.memory.write_memory(0xFF00, 0xCF);
      }

      if state.interrupts {
        let interrupts = state.read_interrupt_enable() & state.read_interrupt_flags();
        state.handle_interrupt(interrupts);
      }
  }
  
  //for i in 0..250 {
    //print!("Sprite({i}): ");
    //for j in 0..16 {
        //print!("{:04x}, ", state.memory.read_memory(0x8000 + i*16 + j));
    //}
    //println!("");
  //}
  //state.dump_registers();
  //state.dump_tilemap();
  
}