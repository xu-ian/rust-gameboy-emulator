use cpu::RunningState;
use std::num::Wrapping;
use std::time::Instant;
use std::sync::mpsc::Receiver;

//use crossterm::event::read;

pub mod cpu;

pub fn run(mut state: RunningState, rx: Receiver<u8>) {
  
  let mut start = Instant::now();
  let mut s = Instant::now();
  //let mut breakpoint = false;
  let mut counter = 0;
  loop {
    counter +=1;
    if counter % 3_000_000 == 0 {
      println!("Total time for a million actions: {}", (Instant::now() - s).as_micros());
      s = Instant::now();
    }
    state.next();

  
      /*if breakpoint {
        println!("Position: {:04x}", state.registers.pc);
        read().unwrap();
      }*/

    if state.registers.pc == 0x01db {
      //state.logging[0] = true;
      //state.logging[1] = true;
      //println!("Main loop");
      //state.logging[0] = true;
      //state.logging[1] = true;
      //state.dump_oam();
      //state.dump_tilemap();
      //state.banks.dump_rom_bank(1);
      //state.banks.dump_ram_bank(0);
    }

    let end = Instant::now();
    let passed = (end - start).as_micros() / 61;
    if passed > 0 {
      let safex = Wrapping(state.memory.read_memory(0xff04));
      let safepass = Wrapping(passed as u8);
      state.memory.write_memory(0xff04, (safex + safepass).0);
      start = end;
    }

    let tac = state.memory.read_memory(0xff07);
    if tac & 0x04 > 0 { //If timer is enabled
      if tac & 0x03 == 0 {
        if state.m_cycles % 256 < state.prev_cycles % 256 {
          match state.tima.checked_add(1) {
            Some(_) => (),
            None => {
              state.tima = state.memory.read_memory(0xff06);
              let interrupts = state.memory.read_memory(0xff0f);
              state.memory.write_memory(0xff0f, interrupts | 0b0000_0100);
            },
          }
        }
      } else if tac & 0x03 == 1 {
        if (state.m_cycles % 4 < state.prev_cycles % 4) || state.m_cycles - state.prev_cycles >= 4 {
          match state.tima.checked_add(1) {
            Some(_) => (),
            None => {
              state.tima = state.memory.read_memory(0xff06);
              let interrupts = state.memory.read_memory(0xff0f);
              state.memory.write_memory(0xff0f, interrupts | 0b0000_0100);
            },
          }
        }
      } else if tac & 0x03 == 2 {
        if state.m_cycles % 16 < state.prev_cycles % 16 {
          match state.tima.checked_add(1) {
            Some(_) => (),
            None => {
              state.tima = state.memory.read_memory(0xff06);
              let interrupts = state.memory.read_memory(0xff0f);
              state.memory.write_memory(0xff0f, interrupts | 0b0000_0100);
            },
          }
        }
      } else {
        if state.m_cycles % 64 < state.prev_cycles % 64 {
          match state.tima.checked_add(1) {
            Some(_) => (),
            None => {
              state.tima = state.memory.read_memory(0xff06);
              let interrupts = state.memory.read_memory(0xff0f);
              state.memory.write_memory(0xff0f, interrupts | 0b0000_0100);
            },
          }
        }
      }
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