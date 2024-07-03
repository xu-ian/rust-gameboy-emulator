extern crate crossterm;

use cpu::RunningState;
use cpu::memory::Memory;

use std::thread;


use crossterm::event::{read, Event, KeyCode, KeyEvent};

pub mod cpu;

pub fn run(mut state: RunningState) {

  let arccopymem = state.get_memory_copy();

  //Creates the key input thread
  thread::spawn(move || {
      let mut memcpy = Memory {
          data: arccopymem
      };

      loop {
          match read().unwrap() {
              Event::Key(KeyEvent {
                  code: KeyCode::Up,
                  ..
              }) => memcpy.write_memory(0xff00, 0b0001_0100),
              Event::Key(KeyEvent {
                  code: KeyCode::Down,
                  ..
              }) => memcpy.write_memory(0xff00, 0b0001_1000),
              Event::Key(KeyEvent {
                  code: KeyCode::Left,
                  ..
              }) => memcpy.write_memory(0xff00, 0b0001_0010),
              Event::Key(KeyEvent {
                  code: KeyCode::Right,
                  ..
              }) => memcpy.write_memory(0xff00, 0b0001_0001),
              Event::Key(KeyEvent {
                  code: KeyCode::Char('z'),
                  ..
              }) => memcpy.write_memory(0xff00, 0b0010_0010),
              Event::Key(KeyEvent {
                  code: KeyCode::Char('x'),
                  ..
              }) => memcpy.write_memory(0xff00, 0b0010_0001),
              Event::Key(KeyEvent {
                  code: KeyCode::Enter,
                  ..
              }) => memcpy.write_memory(0xff00, 0b0010_1000),
              Event::Key(KeyEvent {
                  code: KeyCode::Backspace,
                  ..
              }) => memcpy.write_memory(0xff00, 0b0010_0100),
              Event::Key(KeyEvent {
                  code: KeyCode::Esc,
                  ..
              }) => break,
              _ => (),
          }    
      }
  });

  //loop {
  //state.logging[0] = true;
  //state.logging[1] = true;
  //12328 marks end of first loop
  //Starts infinite loop from line 0x233 
  for _ in 1..50000 {
      state.next();
      //let inter = state.interrupts;
      //println!("IME Allow Interrupt status {inter}, Interrupts: {:04x}", state.read_interrupt_enable() & state.read_interrupt_flags());
  }
  state.dump_registers();
  //state.dump_tilemap();

  //println!("{:?}", arr);
}