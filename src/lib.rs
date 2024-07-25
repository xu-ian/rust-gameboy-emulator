use cpu::RunningState;
use std::num::Wrapping;
use std::thread;
use std::time::Instant;
use std::sync::mpsc::Receiver;

use crossterm::event::read;

pub mod cpu;

pub fn run(mut state: RunningState, rx: Receiver<u8>) {
  
  //let mut counter:u128 = 0;
  /*let mut scount0 = 0;
  let mut scount1 = 0;
  let mut scount2 = 0;
  let mut scount3 = 0;
  let mut scount4 = 0;
  let mut inttime = 0;*/

  let mut start = Instant::now();

  //let mut breakpoint = false;

  //let mut s = Instant::now();

  loop {
      //counter+=1;

      /*if counter % 1000 == 0 {
        s = Instant::now();
      }*/

      state.next();

      /*if counter % 1000 == 0 {
        scount0 += (Instant::now() - s).as_micros();
      }*/

      /*if breakpoint {
        println!("Position: {:04x}", state.registers.pc);
        read().unwrap();
      }*/

      //if state.registers.pc == 0x2004 {
        //breakpoint = true;
        //state.logging[3] = true;
        //state.interrupts = false;
      //}

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

      /*if counter % 1000 == 0 {
        scount4 += (Instant::now() - s).as_micros();
      }*/

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

      /*if counter % 1000 == 0 {
        scount2 += (Instant::now() - s).as_micros();
      }*/

      //Handles interrupts
      if state.interrupts {
        if state.read_active_interrupt() > 0 {
          //inttime += 1;
          state.handle_interrupt();
        }
      }
      /*if counter % 1000 == 0 {
        scount1 += (Instant::now() - s).as_micros();
      }*/

            //Logs gameboy speed
            /*if counter % 3_000_000 == 0 {
              println!("Program counter:{:04x}", state.registers.pc);
              println!("Total action time(Instruction+counter): {} micros", scount0*1000);
              println!("Total action time(Timer): {} micros", (scount4 - scount0)*1000);
              println!("Total action time(Input Receiver): {} micros", (scount2-scount4)*1000);
              //println!("Total action time(Interrupts): {} micros", scount3-scount2);
              println!("Total interrupts time: {}, number of interrupts: {}", (scount1 - scount2)*1000, inttime);
              scount0 = 0; 
              scount1 = 0; 
              scount2 = 0; 
              scount3 = 0;
              scount4 = 0;
              inttime = 0;
            }*/

  }
}