use cpu::RunningState;
use std::time::Instant;
use std::sync::mpsc::Receiver;

use crossterm::event::read;

pub mod cpu;

fn to_byte(bits: [u8; 4]) -> u8 {
  bits[0] + (bits[1] << 1) + (bits[2] << 2) + (bits[3] << 3)
}

pub fn run(mut state: RunningState, rx: Receiver<u8>) {
  
  let mut counter:u128 = 0;
  let mut scount = 0;
  let mut input_type = 0x3F;

  let mut breakpoint = false;

  loop {
      let s = Instant::now();
      state.next();
      
      counter+=1;

      if breakpoint {
        println!("Position: {:04x}", state.registers.pc);
        state.registers.dump_registers();
        read().unwrap();
      }

      //if state.registers.pc == 0x2004 {
        //breakpoint = true;
        //state.logging[3] = true;
        //state.interrupts = false;
      //}

      if state.registers.pc == 0x29de {
        //print!("FF80: {}, ", state.memory.read_memory(0xff80));
        //println!("FF81: {}", state.memory.read_memory(0xff81));
        //state.dump_oam();
        //state.dump_tilemap();
      }

      //Logs gameboy speed
      if counter % 3_000_000 == 0 {
        println!("Program counter:{:04x}", state.registers.pc);
        println!("Total action time: {} micros, Average action time: {} micros",scount, scount / 3_000_000);
        scount = 0;
      }
      let x = Instant::now() - s;
      scount += x.as_micros();

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
          if x / 8 > 0 {
            print!("Release button ");
            match x % 8 {
              0 => println!("Right"),
              1 => println!("Left"),
              2 => println!("Up"),
              3 => println!("Down"),
              4 => println!("A"),
              5 => println!("B"),
              6 => println!("Select"),
              7 => println!("Start"),
              _ => ()
            }
          } else {
            print!("Press button ");
            match x % 8 {
              0 => println!("Right"),
              1 => println!("Left"),
              2 => println!("Up"),
              3 => println!("Down"),
              4 => println!("A"),
              5 => println!("B"),
              6 => println!("Select"),
              7 => println!("Start"),
              _ => ()
            }
          }
          state.joypad[usize::from((x % 8) / 4)][usize::from((x % 8) % 4)] = x / 8;

        },
        Err(_) => (),
      }
      let input_type = state.memory.read_memory(0xff00);
      //println!("input type: {:04x}", !input_type & 0xf0);
      if (!input_type & 0x30) == 0x20 {
        state.memory.update_joystick(to_byte(state.joypad[1]));
      } else if (!input_type & 0x30) == 0x10 {
        state.memory.update_joystick(to_byte(state.joypad[0]));
      }

      //Handles interrupts
      if state.interrupts {
        let interrupts = state.read_interrupt_enable() & state.read_interrupt_flags();
        state.handle_interrupt(interrupts);
      }
  }
}