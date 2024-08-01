use cpu::RunningState;
use std::num::Wrapping;
use std::time::Instant;
use std::sync::mpsc::Receiver;

//use crossterm::event::read;

pub mod cpu;

pub fn run(mut state: RunningState, rx: Receiver<u8>) {
  
  let mut start = Instant::now();

  //let mut breakpoint = false;

  loop {

    state.next();

  
      /*if breakpoint {
        println!("Position: {:04x}", state.registers.pc);
        read().unwrap();
      }*/

    if state.registers.pc == 0x2cd {
      //state.dump_oam();
      //state.dump_tilemap();
    }

    let end = Instant::now();
    let passed = (end - start).as_micros() / 61;
    if passed > 0 {
      let safex = Wrapping(state.memory.read_memory(0xff04));
      let safepass = Wrapping(passed as u8);
      state.memory.write_memory(0xff04, (safex + safepass).0);
      start = end;
    }

    //Reads inputs from user
    match rx.try_recv() {
        Ok(x) => {
          //Joypad Array As follows
          /* 
            [
              [Right, Left, Up, Down],
              [A, B, Select, Start]
            ]
          */
          state.joypad[usize::from((x % 8) / 4)][usize::from((x % 8) % 4)] = x / 8;
        },
        Err(_) => (),
      }

    //Handles interrupts
    if state.interrupts {
      if state.read_active_interrupt() > 0 {
        state.handle_interrupt();
      }
    }
  }
}