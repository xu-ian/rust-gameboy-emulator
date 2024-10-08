  pub type Register = u8;

  pub enum FlagActions {
    Set,
    Reset,
    Flip
  }

  fn bits(byte:u8, pos: i32, bits: u8) -> u8 {
    (byte >> pos) & bits
  }


  pub struct Registers {
    pub pc: u16,
    pub sp: u16,
    pub a: Register,
    pub f: Register,
    pub b: Register,
    pub c: Register,
    pub d: Register,
    pub e: Register,
    pub h: Register,
    pub l: Register,
  }

  impl Registers {

    pub fn new() -> Registers {
      Registers {
        a:0x01,f:0xb0,
        b:0,c:0x13,
        d:0,e:0xd8,
        h:0x01,l:0x4d,
        pc:0x0100,
        sp:0xfffe,
      }
    }

    pub fn write_register(&mut self, byte:u8, value: u8) {
      if byte == 0x0 {
        self.b = value;
      } else if byte == 0x1 {
        self.c = value;
      } else if byte == 0x2 {
        self.d = value;
      } else if byte == 0x3 {
        self.e = value;
      } else if byte == 0x4 {
        self.h = value;
      } else if byte == 0x5 {
        self.l = value;
      } else if byte == 0x7 {
        self.a = value;
      } else {
        panic!("Attempting to write to unknown register");
      }
    }

    pub fn read_register(&self, byte:u8) -> u8 {
      if byte == 0x0 {
        self.b
      } else if byte == 0x1 {
        self.c
      } else if byte == 0x2 {
        self.d
      } else if byte == 0x3 {
        self.e
      } else if byte == 0x4 {
        self.h
      } else if byte == 0x5 {
        self.l
      } else if byte == 0x7 {
        self.a
      } else {
        panic!("Requested register {byte} does not exist");
      }
    }

    pub fn modify_zero_flag(&mut self, flag: FlagActions) {      
      match flag {
        FlagActions::Set => self.f |= 0b1000_0000,
        FlagActions::Reset => self.f &= 0b0111_1111,
        FlagActions::Flip => self.f ^= 0b1000_0000
      }
    }

    pub fn set_zero_flag(&mut self, set: bool) {
      if set {
        self.modify_zero_flag(FlagActions::Set);
      } else {
        self.modify_zero_flag(FlagActions::Reset);
      }
    }

    pub fn get_zero_flag(&mut self) -> u8 {
      u8::from(self.f & 0b1000_0000 > 0)
    }

    pub fn modify_sub_flag(&mut self, flag: FlagActions) {
      match flag {
        FlagActions::Set => self.f |= 0b0100_0000,
        FlagActions::Reset => self.f &= 0b1011_1111,
        FlagActions::Flip => self.f ^= 0b0100_0000
      }
    }

    pub fn set_sub_flag(&mut self, set: bool) {
      if set {
        self.modify_sub_flag(FlagActions::Set);
      } else {
        self.modify_sub_flag(FlagActions::Reset);
      }
    }

    pub fn get_sub_flag(&mut self) -> u8 {
      u8::from(self.f & 0b0100_0000 > 0)
    }

    pub fn modify_half_carry_flag(&mut self, flag: FlagActions) {
      match flag {
        FlagActions::Set => self.f |= 0b0010_0000,
        FlagActions::Reset => self.f &= 0b1101_1111,
        FlagActions::Flip => self.f ^= 0b0010_0000
      }
    }

    pub fn set_half_carry_flag(&mut self, set: bool) {
      if set {
        self.modify_half_carry_flag(FlagActions::Set);
      } else {
        self.modify_half_carry_flag(FlagActions::Reset);
      }
    }

    pub fn get_half_carry_flag(&mut self) -> u8 {
      u8::from(self.f & 0b0010_0000 > 0)
    }

    pub fn modify_carry_flag(&mut self, flag: FlagActions) {
      match flag {
        FlagActions::Set => self.f |= 0b0001_0000,
        FlagActions::Reset => self.f &= 0b1110_1111,
        FlagActions::Flip => self.f ^= 0b0001_0000
      }
    }

    pub fn set_carry_flag(&mut self, set: bool) {
      if set {
        self.modify_carry_flag(FlagActions::Set);
      } else {
        self.modify_carry_flag(FlagActions::Reset);
      }
    }

    pub fn get_carry_flag(&mut self) -> u8 {
      u8::from(self.f & 0b0001_0000 > 0)
    }

    pub fn dump_registers(&mut self) {
      println!("Program Counter: {:04x}", self.pc);
      println!("Stack Pointer: {:04x}", self.sp);
      println!("Accumulator: {}, Flags: Zero:{} Sub:{} Half:{} Carry:{}", self.a, 
        bits(self.f, 7, 1), bits(self.f, 6, 1), bits(self.f, 5, 1), bits(self.f, 4, 1));
      println!("Registers:");
      println!("B: {:02x}, C: {:02x}", self.b, self.c);
      println!("D: {:02x}, E: {:02x}", self.d, self.e);
      println!("H: {:02x}, L: {:02x}", self.h, self.l);
    }
    
    pub fn get_hl_value(&self) -> u16 {
      Self::join_u8(self.h, self.l)
    }

    pub fn get_bc_value(&self) -> u16 {
      Self::join_u8(self.b, self.c)
    }

    pub fn get_de_value(&self) -> u16 {
      Self::join_u8(self.d, self.e)
    }
    
    pub fn join_u8(int1: u8, int2: u8) -> u16 {
      (u16::from(int1) << 8) + u16::from(int2)
    }
  
  }

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn verify_join_u8() {
    assert_eq!(Registers::join_u8(0x01, 0x10), 0x0110, "joining 0x01 and 0x10 equals {:#04x} instead of 0x0110", Registers::join_u8(0x01, 0x10));
  }

}