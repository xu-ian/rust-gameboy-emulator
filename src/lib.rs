extern crate crossterm;

use cpu::RunningState;
use cpu::memory::Memory;

use std::{thread::{self, sleep}, time::Duration};


use crossterm::event::{read, Event, KeyCode, KeyEvent};

pub mod cpu;

pub fn run(mut state: RunningState) {

  println!("Start");
  let arccopymem = state.get_memory_copy();

  //Creates the key input thread
  thread::spawn(move || {
      let mut memcpy = Memory {
          data: arccopymem,
      };
      let mut data = 0;
      loop {
          match read().unwrap() {
              Event::Key(KeyEvent {//UP
                  code: KeyCode::Up,
                  ..
              }) => data = 0b0001_0100,
              Event::Key(KeyEvent {//Down
                  code: KeyCode::Down,
                  ..
              }) => data = 0b0001_1000,
              Event::Key(KeyEvent {//Left
                  code: KeyCode::Left,
                  ..
              }) => data = 0b0001_0010,
              Event::Key(KeyEvent {//Right
                  code: KeyCode::Right,
                  ..
              }) => data = 0b0001_0001,
              Event::Key(KeyEvent {//A
                  code: KeyCode::Char('z'),
                  ..
              }) => data = 0b0010_0010,
              Event::Key(KeyEvent {//B
                  code: KeyCode::Char('x'),
                  ..
              }) => data = 0b0010_0001,
              Event::Key(KeyEvent {//Start
                  code: KeyCode::Enter,
                  ..
              }) => data = 0b0010_1000,
              Event::Key(KeyEvent {//Select
                  code: KeyCode::Backspace,
                  ..
              }) => data = 0b0010_0100,
              Event::Key(KeyEvent {//Quit
                  code: KeyCode::Esc,
                  ..
              }) => break,
              _ => (),
          }    
          if data != 0 {
            memcpy.write_memory(0xff00, data);
          }
      }
  });

  let mut counter:u128 = 0;

  state.registers.pc = 0x2c4;
  //state.registers.d = 0x4a;
  //state.registers.e = 0x07;
  sleep(Duration::from_millis(2000));
  loop {
  //state.logging[0] = true;
  //state.logging[1] = true;
    
      state.next();
      counter+=1;

      if state.registers.pc == 0x2ca {
        println!("Input: {}", state.memory.read_memory(0xFF80));
      }

      if state.registers.pc == 0x2d3 {
        break;
      }
      if state.registers.pc >= 0x29a6 && state.registers.pc < 0x29d3 {
        println!("Input: {} ", state.memory.read_memory(0xff00));
      }

      if state.registers.pc == 0x29b0 {
        println!("29b0");
        println!("Input: {}", state.memory.read_memory(0xff00));
        //state.dump_registers();
      }

      if state.registers.pc == 0x29d4 {
        println!("29d4");
        println!("Input: {}", state.memory.read_memory(0xFF00));
        //state.dump_registers();
      }

      if counter % 10000000 == 0 {
        state.dump_registers();
        //state.dump_tilemap();
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
  state.dump_registers();
  //state.dump_tilemap();
  
}