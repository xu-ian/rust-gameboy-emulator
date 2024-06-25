pub mod cpu {
    use std::num::Wrapping;

  
  type Register = u8;

  pub enum FlagActions {
    Set,
    Reset,
    Flip
  }

  pub enum RegisterPairs {
    BC,
    DE,
    HL
  }

  pub struct Registers {
    pc: u16,
    sp: u16,
    a: Register,
    f: Register,
    b: Register,
    c: Register,
    d: Register,
    e: Register,
    h: Register,
    l: Register,
  }

  pub struct RunningState {
    registers: Registers,
    memory: Memory
  }

  impl Registers {
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

    pub fn set_zero_flag(&mut self, flag: FlagActions) {      
      match flag {
        FlagActions::Set => self.f |= 0b1000_0000,
        FlagActions::Reset => self.f &= 0b0111_1111,
        FlagActions::Flip => self.f ^= 0b1000_0000
      }
    }

    pub fn get_zero_flag(&mut self) -> u8 {
      u8::from(self.f & 0b1000_0000 > 0)
    }

    pub fn set_sub_flag(&mut self, flag: FlagActions) {
      match flag {
        FlagActions::Set => self.f |= 0b0100_0000,
        FlagActions::Reset => self.f &= 0b1011_1111,
        FlagActions::Flip => self.f ^= 0b0100_0000
      }
    }

    pub fn get_sub_flag(&mut self) -> u8 {
      u8::from(self.f & 0b0100_0000 > 0)
    }

    pub fn set_half_carry_flag(&mut self, flag: FlagActions) {
      match flag {
        FlagActions::Set => self.f |= 0b0010_0000,
        FlagActions::Reset => self.f &= 0b1101_1111,
        FlagActions::Flip => self.f ^= 0b0010_0000
      }
    }

    pub fn get_half_carry_flag(&mut self) -> u8 {
      u8::from(self.f & 0b0010_0000 > 0)
    }

    pub fn set_carry_flag(&mut self, flag: FlagActions) {
      match flag {
        FlagActions::Set => self.f |= 0b0001_0000,
        FlagActions::Reset => self.f &= 0b1110_1111,
        FlagActions::Flip => self.f ^= 0b0001_0000
      }
    }

    pub fn get_carry_flag(&mut self) -> u8 {
      u8::from(self.f & 0b0001_0000 > 0)
    }

    pub fn dump_registers(&mut self) {
      println!("Program Counter: {}", self.pc);
      println!("Stack Pointer: {}", self.sp);
      println!("Accumulator: {}, Flags: {} {} {} {}", self.a, 
        bits(self.f, 7, 1), bits(self.f, 6, 1), bits(self.f, 5, 1), bits(self.f, 4, 1));
      println!("Registers:");
      println!("B: {}, C: {}", self.b, self.c);
      println!("D: {}, E: {}", self.d, self.e);
      println!("H: {}, L: {}", self.h, self.l);
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
      (u16::from(int1) << 4) + u16::from(int2)
    }
  
  }

  pub enum MemAction {
    Save,
    Load
  }

  pub enum Dest {
    Reg(Register),
    RegPair(RegisterPairs),
    HL,
    Immediate,
  }

  pub enum Instruction {
    //Loads the specified register with the data from the destination
    Load(Register, Dest),
    //Saves the data from the Destination to the memory specified by HL
    SaveHL(Dest),
    //Saves/Loads the data to/from register A from/to the memory address of BC
    AfBC(MemAction),
    //Saves/Loads the data to/from register A from/to the memory address of DE
    AfDE(MemAction),
    //Saves/Loads A to/from the location specified by the next two memory spaces
    Ann(MemAction),
    //Saves/Loads A to/from the location specified by 0xFF00 + C register
    AXC(MemAction),
    //Saves/Loads A to/from the location specified by 0xFF00 + next byte memory
    AXn(MemAction),
    //Saves/Loads A to/from the memory address of HL, decrement HL afterwards
    AfHLdec(MemAction),
    //Saves/Loads A to/from the memory address of HL, increment HL afterwards
    AfHLinc(MemAction),
    //Loads to the register pair, the next two bytes of data
    Loadnn(Dest),
    //Saves the contents of the stack pointer to the memory location pointed to by the next two bytes of data
    SaveSPnn,
    //Copies HL into SP 
    LoadSPHL,
    //Pushes contents of register pair to stack, stack is incremented
    PushStack(Dest),
    //Pops contents of stack to register pair, stack is decremented
    PopStack(Dest),
    //Loads content from adjusted stack pointer, adjusted by next byte of data(signed)
    LoadStackAdj,
    //Adds the destination to accumulator
    Add(Dest),
    //Adds the destination and the carry bit to accumulator
    AddCarry(Dest),
    //Sub the destination from accumulator
    Sub(Dest),
    //Sub the destination and carry flag from accumulator
    SubCarry(Dest),
    //Updates flags based on comparison between destination and accumulator
    Compare(Dest),
    //Increments register or HL Mem Location 
    Inc(Dest),
    //Decrements register or HL Mem Location
    Dec(Dest),
    //Bitwise AND Acc and Destination
    AND(Dest),
    //Bitwise OR Acc and Destination
    OR(Dest),
    //Bitwise XOR Acc and Desination
    XOR(Dest),
    //Flips carry flag, clears N and H flag
    CCFlag,
    //Sets carry flag, clears N and H flag
    SCFlag,
    //Performs a BCD conversion of the previous operation based on the flags
    DAA,
    //Flips Acc and sets N and H
    CmpA,
    //Increments the register pair
    Inc16(Dest),
    //Decrements the register pair
    Dec16(Dest),
    //Adds Register pair to HL and stores in HL
    Add16(Dest),
    //Adds immediate byte to stack pointer
    SPAddn,
    //Rotates the destination bits left, carry included
    RotateLC(Dest),
    //Rotate the destination bits right, carry included
    RotateRC(Dest),
    //Rotates the destination bits left, carry is set based on bit pushed out but is not included in rotate
    RotateL(Dest),
    //Rotates the destination bits right, carry is set based on bit pushed out but is not included in rotate
    RotateR(Dest),
    //Shifts destination left by a specified number of bits 
    ShiftL(Dest),
    //Shifts destination right by a bit new bit is set
    ShiftRArith(Dest),
    //Shifts destination right by a bit new bit are empty
    ShiftRLog(Dest),
    //Swaps the left and right 4 bits of destination
    SwapNibble(Dest),
    //Tests the bit in the destination byte, set zero flag if 0 bit
    TestBit(u8, Dest),
    //Sets the bit in the destination byte to zero
    ResetBit(u8, Dest),
    //Sets the bit in the destination byte to one
    SetBit(u8, Dest),
    //Represents the CB Prefix
    Prefix,
    //Sets program counter(JUMPS) to the value of the destination memory
    Jump(Dest),
    //Sets Program counter(JUMPS) to the value of the destination memory if condition is fulfilled
    JumpCond(bool, Dest),
    //Increases/Decreases program counter by value of next byte
    JumpRel,
    //Increases/Decreases program counter by value of next byte if condition is fulfilled
    JumpRelCond(bool),
    //Function Call. Stack pointer advances, program counter jumps to function position. Jump dest is next 2 bytes
    Call(Dest),
    //Conditional Function Call. See above. 
    CondCall(u8, Dest),
    //Return from function load and roll back stack to program counter
    Ret,
    //Conditional return from function load. See above
    CondRet(u8),
    //Return from interrupt handler
    RetI,
    //Save current position in stack pointer and jump to specified position
    Restart(u8),
    //CPU stops until interrupt is done if IME, and does other pending actions if not IME
    HALT,
    //I don't know
    STOP,
    //Disable interrupt
    DI,
    //Enable interrupt
    EI,
    //Does nothing
    NOP,
  }

  fn check(instruction:u8, b7:u8, b6:u8, b5:u8, b4:u8, b3:u8, b2:u8, b1:u8, b0:u8) -> bool {
    if (b0 == 2 || bits(instruction.clone(), 0, 1) == b0) &&
       (b1 == 2 || bits(instruction.clone(), 1, 1) == b1) &&
       (b1 == 2 || bits(instruction.clone(), 2, 1) == b2) &&
       (b1 == 2 || bits(instruction.clone(), 3, 1) == b3) &&
       (b1 == 2 || bits(instruction.clone(), 4, 1) == b4) &&
       (b1 == 2 || bits(instruction.clone(), 5, 1) == b5) &&
       (b1 == 2 || bits(instruction.clone(), 6, 1) == b6) &&
       (b1 == 2 || bits(instruction.clone(), 7, 1) == b7) {
      true
    } else {
      false
    }
  }

  fn get_register_pair(pair:u8) -> RegisterPairs{
    if pair == 0 {
      RegisterPairs::BC
    } else if pair == 1 {
      RegisterPairs::DE
    } else {
      RegisterPairs::HL
    }
  }

  //Split into CB and non-CB prefixed instructions
  impl Instruction {
    pub fn translate(instruction: u8) -> Instruction {
      if check(instruction,0,1,2,2,2,2,2,2) {
        Instruction::Load(
          bits(instruction, 3, 3),
          Dest::Reg(bits(instruction, 0, 3))
        )
      } else if check(instruction, 0,0,2,2,2,1,1,0) {
        Instruction::Load(
          bits(instruction, 3, 3),
          Dest::Immediate
        )
      } else if check(instruction, 0,0,0,0,1,0,1,0) {
        Instruction::AfBC(
          MemAction::Load
        )
      } else if check(instruction, 0,0,0,1,1,0,1,0) {
        Instruction::AfDE(
          MemAction::Load
        )
      } else if check(instruction, 0,0,0,0,0,0,1,0) {
        Instruction::AfBC(
          MemAction::Save
        )        
      } else if check(instruction, 0,0,0,1,0,0,1,0) {
        Instruction::AfDE(
          MemAction::Save
        )
      } else if check(instruction, 1,1,1,1,1,0,1,0) {
        Instruction::Ann(
          MemAction::Load
        )
      } else if check(instruction, 1,1,1,0,1,0,1,0) {
        Instruction::Ann(
          MemAction::Save
        )
      } else if check(instruction, 1,1,1,1,0,0,1,0) {
        Instruction::AXC(
          MemAction::Load
        )
      } else if check(instruction, 1,1,1,0,0,0,1,0) {
        Instruction::AXC(
          MemAction::Save
        )        
      } else if check(instruction, 1,1,1,1,0,0,0,0) {
        Instruction::AXn(
          MemAction::Load
        )
      } else if check(instruction, 1,1,1,0,0,0,0,0) {
        Instruction::AXn(
          MemAction::Save
        )
      } else if check(instruction, 0,0,1,1,1,0,1,0) {
        Instruction::AfHLdec(
          MemAction::Load
        )
      } else if check(instruction, 0,0,1,1,0,0,1,0) {
        Instruction::AfHLdec(
          MemAction::Save
        )
      } else if check(instruction, 0,0,1,0,1,0,1,0) {
        Instruction::AfHLinc(
          MemAction::Load
        )
      } else if check(instruction, 0,0,1,0,0,0,1,0) {
        Instruction::AfHLinc(
          MemAction::Save
        )
      } else if check(instruction, 0,0,2,2,0,0,0,1) {
        let pair = get_register_pair(bits(instruction, 4, 2));

        Instruction::Loadnn(
          Dest::RegPair(pair)
        )
      } else if check(instruction, 0,0,0,0,1,0,0,0) {
        Instruction::SaveSPnn
      } else if check(instruction, 1,1,1,1,1,0,0,1) {
        Instruction::LoadSPHL
      } else if check(instruction, 1,1,2,2,0,1,0,1) {
        let pair = get_register_pair(bits(instruction, 4, 2));
        
        Instruction::PushStack(
          Dest::RegPair(pair)
        )
      } else if check(instruction, 1,1,2,2,0,0,0,1) {
        let pair = get_register_pair(bits(instruction, 4, 2));

        Instruction::PopStack(
          Dest::RegPair(pair)
        )
      } else if check(instruction, 1,1,1,1,1,0,0,0) {
        Instruction::LoadStackAdj
      } else if check(instruction, 1,0,0,0,0,2,2,2) {
        Instruction::Add(
          Dest::Reg(bits(instruction, 0, 3))
        )
      } else if check(instruction, 1,1,0,0,0,1,1,0) {
        Instruction::Add(
          Dest::Immediate
        )
      } else if check(instruction, 1,0,0,0,1,2,2,2) {
        Instruction::AddCarry(
          Dest::Reg(bits(instruction, 0, 3))
        )
      } else if check(instruction, 1,1,0,0,1,1,1,0) {
        Instruction::AddCarry(
          Dest::Immediate
        )
      } else if check(instruction, 1,0,0,1,0,2,2,2) {
        Instruction::Sub(
          Dest::Reg(bits(instruction, 0, 3))
        )
      } else if check(instruction, 1,1,0,1,0,1,1,0) {
        Instruction::Sub(
          Dest::Immediate
        )        
      } else if check(instruction, 1,0,0,1,1,2,2,2) {
        Instruction::SubCarry(
          Dest::Reg(bits(instruction, 0, 3))
        )
      } else if check(instruction, 1,1,0,1,1,1,1,0) {
        Instruction::SubCarry(
          Dest::Immediate
        )        
      } else if check(instruction, 1,0,1,1,1,2,2,2) {
        Instruction::Compare(
          Dest::Reg(bits(instruction, 0, 3))
        )
      } else if check(instruction, 1,1,1,1,1,1,1,0) {
        Instruction::Compare(
          Dest::Immediate
        )
      } else if check(instruction, 0,0,2,2,2,1,0,0) {
        Instruction::Inc(
          Dest::Reg(bits(instruction, 3, 3))
        )
      } else if check(instruction, 0,0,2,2,2,1,0,1) {
        Instruction::Dec(
          Dest::Reg(bits(instruction, 3, 3))
        )
      } else if check(instruction, 1,0,1,0,0,2,2,2) {
        Instruction::AND(
          Dest::Reg(bits(instruction,0,3))
        )
      } else if check(instruction, 1,1,1,0,0,1,1,0) {
        Instruction::AND(
          Dest::Immediate
        )        
      } else if check(instruction, 1,0,1,1,0,2,2,2) {
        Instruction::OR(
          Dest::Reg(bits(instruction, 0, 3))
        )
      } else if check(instruction, 1,1,1,1,0,1,1,0) {
        Instruction::OR(
          Dest::Immediate
        )        
      } else if check(instruction, 1,0,1,0,1,2,2,2) {
        Instruction::XOR(
          Dest::Reg(bits(instruction,0,3))
        )
      } else if check(instruction, 1,1,1,0,1,1,1,0) {
        Instruction::XOR(
          Dest::Immediate
        )        
      } else if check(instruction, 0,0,1,1,1,1,1,1) {
        Instruction::CCFlag
      } else if check(instruction, 0,0,1,1,0,1,1,1) {
        Instruction::SCFlag
      } else if check(instruction, 0,0,1,0,0,1,1,1) {
        Instruction::DAA
      } else if check(instruction, 0,0,1,0,1,1,1,1) {
        Instruction::CmpA
      } else if check(instruction, 0,0,2,2,0,0,1,1) {
        let pair = get_register_pair(bits(instruction, 4, 2));
        Instruction::Inc16(
          Dest::RegPair(pair)
        )
      } else if check(instruction, 0,0,2,2,1,0,1,1) {
        let pair = get_register_pair(bits(instruction, 4, 2));
        Instruction::Dec16(
          Dest::RegPair(pair)
        )
      } else if check(instruction, 0,0,2,2,1,0,0,1) {
        let pair = get_register_pair(bits(instruction, 4, 2));
        Instruction::Add16(
          Dest::RegPair(pair)
        )
      } else if check(instruction, 1,1,1,0,1,0,0,0) {
        Instruction::SPAddn
      } else if check(instruction, 1,1,0,0,1,0,1,1) {
        Instruction::Prefix
      } else if check(instruction, 1,1,0,0,0,0,1,1) {
        Instruction::Jump(
          Dest::Immediate
        )
      } else if check(instruction, 1,1,1,0,1,0,0,1) {
        Instruction::Jump(
          Dest::HL
        )
      } else if check(instruction, 1,1,0,2,2,0,1,0) {
        Instruction::JumpCond(
          bits(instruction, 3, 2) == 0,
          Dest::Immediate
        )
      } else if check(instruction, 0,0,0,1,1,0,0,0) {
        Instruction::JumpRel
      } else if check(instruction, 0,0,1,2,2,0,0,0) {
        Instruction::JumpRelCond(
          bits(instruction, 3, 2) == 0
        )
      } else if check(instruction, 1,1,0,0,1,1,0,1) {
        Instruction::Call(
          Dest::Immediate
        )
      } else if check(instruction, 1,1,0,2,2,1,0,0) {
        Instruction::CondCall(
          bits(instruction, 3, 2),
          Dest::Immediate
        )
      } else if check(instruction, 1,1,0,0,1,0,0,1) {
        Instruction::Ret
      } else if check(instruction, 1,1,0,2,2,0,0,0) {
        Instruction::CondRet(
          bits(instruction, 3, 2)
        )
      } else if check(instruction, 1,1,0,1,1,0,0,1) {
        Instruction::RetI
      } else if check(instruction, 1,1,2,2,2,1,1,1) {
        Instruction::Restart(
          bits(instruction, 3, 3)
        )
      } else if check(instruction, 0,0,0,1,0,0,0,0) {
        Instruction::STOP
      } else if check(instruction, 0,1,1,1,0,1,1,0) {
        Instruction::HALT
      } else if check(instruction, 1,1,1,1,0,0,1,1) {
        Instruction::DI
      } else if check(instruction, 1,1,1,0,1,0,1,1) {
        Instruction::EI
      } else {
        Instruction::NOP
      }
    }
    
    pub fn translatecb(instruction: u8) -> Instruction {
      if check(instruction, 0,0,0,0,0,2,2,2) {
        Instruction::RotateLC(
          Dest::Reg(bits(instruction,0,3))
        )
      } else if check(instruction, 0,0,0,0,1,2,2,2) {
        Instruction::RotateRC(
          Dest::Reg(bits(instruction,0,3))
        )
      } else if check(instruction, 0,0,0,1,0,2,2,2) {
        Instruction::RotateL(
          Dest::Reg(bits(instruction,0,3))
        )
      } else if check(instruction, 0,0,0,1,1,2,2,2) {
        Instruction::RotateR(
          Dest::Reg(bits(instruction,0,3))
        )
      } else if check(instruction, 0,0,1,0,0,2,2,2) {
        Instruction::ShiftL(
          Dest::Reg(bits(instruction, 0, 3))
        )
      } else if check(instruction, 0,0,1,0,1,2,2,2) {
        Instruction::ShiftRArith(
          Dest::Reg(bits(instruction, 0, 3))
        )
      } else if check(instruction, 0,0,1,1,0,2,2,2) {
        Instruction::SwapNibble(
          Dest::Reg(bits(instruction, 0, 3))
        )
      } else if check(instruction, 0,0,1,1,1,2,2,2) {
        Instruction::ShiftRLog(
          Dest::Reg(bits(instruction, 0, 3))
        )
      } else if check(instruction, 0,1,2,2,2,2,2,2) {
        Instruction::TestBit(
          bits(instruction, 3, 3),
          Dest::Reg(bits(instruction, 0, 3))
        )
      } else if check(instruction, 1,0,2,2,2,2,2,2) {
        Instruction::ResetBit(
          bits(instruction, 3, 3),
          Dest::Reg(bits(instruction, 0, 3))
        )
      } else if check(instruction, 1,1,2,2,2,2,2,2) {
        Instruction::SetBit(
          bits(instruction, 3, 3),
          Dest::Reg(bits(instruction, 0, 3))
        )
      } else { //This should never be called, if it is called something is wrong
        Instruction::NOP
      }
    }
  }

  pub struct Memory {
    data: Box<[u8; 0x10000]>,
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

  //Rust errors if an integer overflow occurs, so a check needs to be done
  pub fn increment(program_counter: &mut u16) {
    if *program_counter == 0xFFFF {
      *program_counter = 0;
    } else {
      *program_counter += 1;
    }
  }

  pub fn decrement(int: &mut u16) {
    if *int == 0x0000 {
      *int = 0xFFFF;
    } else {
      *int -= 1;
    }
  }

  /*Gets a specified number of bits from a byte starting from a position
    Ex: byte:01101001, pos:3, bits: 2
    Returns 0b01, the 4th bit(1) and 5th bit(0) from the right from the byte */
  fn bits(byte:u8, pos: i32, bits: u8) -> u8 {
    (byte >> pos) & bits
  }

  fn check_half_carry_add(byte1: u8, byte2: u8, carry: u8) -> bool {
    ((byte1 & 0x0F) + (byte2 & 0x0F) + carry) & 0x10 == 0x10
  }

  fn check_half_carry_sub(byte1: u8, byte2: u8, carry: u8) -> bool {
    match (byte1 & 0x0F).checked_sub(byte2 * 0x0F) {
      Some(_) => false,
      None => {
        match ((byte1 & 0x0F) + carry).checked_sub(byte2 * 0x0F) {
          Some(_) => false,
          None => true
        }
      }
    }
  }

  impl RunningState {

  fn read_register(&mut self, byte: u8) -> u8 {
    self.registers.read_register(byte)
  }

  fn write_register(&mut self, byte: u8, value: u8) {
    self.registers.write_register(byte, value)
  }

  fn read_memory(&mut self, position: u16) -> u8 {
    self.memory.read_memory(position)
  }

  fn read_memory_from_pc(&mut self) -> u8 {
    self.memory.read_memory_from_pc(&mut self.registers)
  }

  fn write_memory(&mut self, position: usize, data: u8) {
    self.memory.write_memory(position, data)
  }

  pub fn perform_action(&mut self, instruction: Instruction) {
    match instruction {
      //Saves Immediate into location specified by HL
      Instruction::Load(6, Dest::Immediate) => self.load_immediate_to_hl(),
      //Loads Immediate into register
      Instruction::Load(register, Dest::Immediate) => self.load_immediate_to_register(register),
      //Saves register value to HL memory location
      Instruction::Load(6, Dest::Reg(register)) => self.save_register_to_hl(register),
      //Loads data from HL memory location to register
      Instruction::Load(register, Dest::Reg(6)) => self.load_h1_to_register(register),
      //Copies data from one register to another
      Instruction::Load(reg1, Dest::Reg(reg2)) => self.register_to_register_copy(reg1, reg2),
      //Loads A with memory at location BC
      Instruction::AfBC(MemAction::Load) => self.load_bc_address_to_a(),
      //Saves A at memory location BC
      Instruction::AfBC(MemAction::Save) => self.save_a_to_bc_address(),
      //Loads A with memory at location DE
      Instruction::AfDE(MemAction::Load) => self.load_de_address_to_a(),
      //Saves A at memory location DE
      Instruction::AfDE(MemAction::Save) => self.save_a_to_de_address(),
      //Loads A with memory at location from immediate
      Instruction::Ann(MemAction::Load) => self.load_immediate_address_to_a(),
      //Saves A at memory location from immediate
      Instruction::Ann(MemAction::Save) => self.save_a_to_immediate_address(),
      //Loads A with memory at location 0xFF00 + C
      Instruction::AXC(MemAction::Load) => self.load_ff_c_to_a(),
      //Saves A at memory location 0xFF00 + C
      Instruction::AXC(MemAction::Save) => self.save_a_to_ff_c(),
      //Loads A with memory at location 0xFF00 + immediate
      Instruction::AXn(MemAction::Load) => self.load_ff_n_to_a(),
      //Saves A at memory location 0xFF00 + immediate
      Instruction::AXn(MemAction::Save) => self.save_a_to_ff_n(),
      //Loads A with memory at location HL, then decrements HL
      Instruction::AfHLdec(MemAction::Load) => self.load_hl_address_to_a_dec(),
      //Saves A at memory location HL, then decrements HL
      Instruction::AfHLdec(MemAction::Save) => self.save_a_to_hl_address_dec(),
      //Loads A with memory at location HL, then increments HL
      Instruction::AfHLinc(MemAction::Load) => self.load_hl_address_to_a_inc(),
      //Saves A at memory location HL, then increments HL
      Instruction::AfHLinc(MemAction::Save) => self.save_a_to_hl_address_inc(),
      //Loads immediate to a register pair
      Instruction::Loadnn(Dest::RegPair(pair)) => self.load_immediate_to_register_pair(pair),
      //Saves Data from stack pointer to memory address at immediate
      Instruction::SaveSPnn => self.save_sp_to_immediate_address(),
      //Sets stack pointer to HL
      Instruction::LoadSPHL => self.load_hl_to_sp(),
      //Pushes data from register pair BC to stack
      Instruction::PushStack(Dest::RegPair(RegisterPairs::BC)) => self.push_bc_to_stack(),
      //Pushes data from register pair DE to stack
      Instruction::PushStack(Dest::RegPair(RegisterPairs::DE)) => self.push_de_to_stack(),
      //Pushes data from register pair HL to stack
      Instruction::PushStack(Dest::RegPair(RegisterPairs::HL)) => self.push_hl_to_stack(),
      //Pops stack to register pair BC
      Instruction::PopStack(Dest::RegPair(RegisterPairs::BC)) => self.pop_stack_to_bc(),
      //Pops stack to register pair DE
      Instruction::PopStack(Dest::RegPair(RegisterPairs::DE)) => self.pop_stack_to_de(),      
      //Pops stack to register pair HL
      Instruction::PopStack(Dest::RegPair(RegisterPairs::HL)) => self.pop_stack_to_hl(),
      //Loads adjusted stack position to HL register pair
      Instruction::LoadStackAdj => self.load_adjusted_stack_to_hl(),
      //Adds data from HL location to accumulator
      Instruction::Add(Dest::Reg(6)) => self.add_hl_data_to_a(),
      //Adds a register to accumulator
      Instruction::Add(Dest::Reg(register)) => self.add_register_to_a(register),
      //Adds immediate to accumulator
      Instruction::Add(Dest::Immediate) => self.add_immediate_to_a(),
      //Adds data from HL location and carry to accumulator
      Instruction::AddCarry(Dest::Reg(6)) => self.add_hl_data_to_a_carry(),
      //Adds a register and carry to accumulator
      Instruction::AddCarry(Dest::Reg(register)) => self.add_register_to_a_carry(register),
      //Adds immediate and carry to accumulator
      Instruction::AddCarry(Dest::Immediate) => self.add_immediate_to_a_carry(),
      //Subs data from HL location from accumulator
      Instruction::Sub(Dest::Reg(6)) => self.sub_hl_data_to_a(),
      //Subs a register from accumulator
      Instruction::Sub(Dest::Reg(register)) => self.sub_register_to_a(register),
      //Subs immediate from accumulator
      Instruction::Sub(Dest::Immediate) => self.sub_immediate_to_a(),
      //Subs data from HL location from accumulator with carry
      Instruction::SubCarry(Dest::Reg(6)) => self.sub_hl_data_to_a_carry(),
      //Subs a register from accumulator with carry
      Instruction::SubCarry(Dest::Reg(register)) => self.sub_register_to_a_carry(register),
      //Subs immediate from accumulator with carry
      Instruction::SubCarry(Dest::Immediate) => self.sub_immediate_to_a_carry(),
      //Compares data from HL location with accumulator
      Instruction::Compare(Dest::Reg(6)) => self.cmp_hl_data_to_a(),
      //Compares data from register with accumulator
      Instruction::Compare(Dest::Reg(register)) => self.cmp_register_to_a(register),
      //Compares data from immediate with accumulator
      Instruction::Compare(Dest::Immediate) => self.cmp_immediate_to_a(),
      //Increments data at address specified by hl
      Instruction::Inc(Dest::Reg(6)) => self.inc_hl_data(),
      //Increments specified register
      Instruction::Inc(Dest::Reg(register)) => self.inc_register(register),
      //Decrements data at address specified by hl
      Instruction::Dec(Dest::Reg(6)) => self.dec_hl_data(),
      //Decrements specified register
      Instruction::Dec(Dest::Reg(register)) => self.dec_register(register),
      //ANDs data from HL location with accumulator
      Instruction::AND(Dest::Reg(6)) => self.and_hl_data_to_a(),
      //ANDs data from register with accumulator
      Instruction::AND(Dest::Reg(register)) => self.and_register_to_a(register),
      //ANDs data from immediate with accumulator
      Instruction::AND(Dest::Immediate) => self.and_immediate_to_a(),
      //ORs data from HL location with accumulator 
      Instruction::OR(Dest::Reg(6)) => self.or_hl_data_to_a(),
      //ORs data from register with accumulator
      Instruction::OR(Dest::Reg(register)) => self.or_register_to_a(register),
      //ORs data from immediate with accumulator
      Instruction::OR(Dest::Immediate) => self.or_immediate_to_a(),
      //XORs data from HL location with accumulator 
      Instruction::XOR(Dest::Reg(6)) => self.xor_hl_data_to_a(),
      //XORs data from register with accumulator
      Instruction::XOR(Dest::Reg(register)) => self.xor_register_to_a(register),
      //XORs data from immediate with accumulator
      Instruction::XOR(Dest::Immediate) => self.xor_immediate_to_a(),
      //Flips carry flag, resets subtraction and halfcarry flags
      Instruction::CCFlag => self.ccflag(),
      //Sets carry flag, resets subtraction and halfcarry flags
      Instruction::SCFlag => self.scflag(),

      _ => return
    }
  }

  fn load_immediate_to_hl(&mut self) {
    let data = self.read_memory_from_pc();
    self.write_memory(usize::from(self.registers.get_hl_value()), data);
  }

  fn load_immediate_to_register(&mut self, register: Register) {
    let data = self.read_memory_from_pc();
    self.write_register(register, data);
  }

  fn save_register_to_hl(&mut self, register: Register) {
    let data = self.read_register(register);
    self.write_memory(usize::from(self.registers.get_hl_value()), data);
  }

  fn load_h1_to_register(&mut self, register: Register) {
    let data = self.read_memory(self.registers.get_hl_value());
    self.write_register(register, data);
  }

  fn register_to_register_copy(&mut self, dst: Register, src: Register) {
    let data = self.read_register(src);
    self.write_register(dst, data);
  }

  fn load_bc_address_to_a(&mut self) {
    self.registers.a = self.read_memory(self.registers.get_bc_value());
  } 

  fn save_a_to_bc_address(&mut self) {
    self.write_memory(usize::from(self.registers.get_bc_value()), self.registers.a);
  }

  fn load_de_address_to_a(&mut self) {
    self.registers.a = self.read_memory(self.registers.get_de_value());
  } 

  fn save_a_to_de_address(&mut self) {
    self.write_memory(usize::from(self.registers.get_de_value()), self.registers.a);
  }

  fn load_immediate_address_to_a(&mut self) {
    let leastbyte = self.read_memory_from_pc();
    let mostbyte = self.read_memory_from_pc();
    let address = Registers::join_u8(mostbyte, leastbyte);

    self.registers.a = self.read_memory(address);
  }

  fn save_a_to_immediate_address(&mut self) {
    let leastbyte = self.read_memory_from_pc();
    let mostbyte = self.read_memory_from_pc();
    let address = Registers::join_u8(mostbyte, leastbyte);
    self.write_memory(usize::from(address), self.registers.a);
  }

  fn load_ff_c_to_a(&mut self) {
    let location = Registers::join_u8(0xFF, self.registers.c);
    self.registers.a = self.read_memory(location);
  }

  fn save_a_to_ff_c(&mut self) {
    let location = Registers::join_u8(0xFF, self.registers.c);
    self.write_memory(usize::from(location), self.registers.a);
  }

  fn load_ff_n_to_a(&mut self) {
    let next_mem = self.read_memory_from_pc();
    let location = Registers::join_u8(0xFF, next_mem);
    self.registers.a = self.read_memory(location);
  }

  fn save_a_to_ff_n(&mut self) {
    let next_mem = self.read_memory_from_pc();
    let location = Registers::join_u8(0xFF, next_mem);
    self.write_memory(usize::from(location), self.registers.a);
  }

  fn load_hl_address_to_a_dec(&mut self) {
    let mut address = self.registers.get_hl_value();
    self.registers.a = self.read_memory(address);
    decrement(&mut address);
    self.registers.l = (address % 16) as u8;
    self.registers.h = (address >> 4) as u8;
  }

  fn save_a_to_hl_address_dec(&mut self) {
    let mut address = self.registers.get_hl_value();
    self.write_memory(usize::from(address), self.registers.a);
    decrement(&mut address);
    self.registers.l = (address % 16) as u8;
    self.registers.h = (address >> 4) as u8;
  }

  fn load_hl_address_to_a_inc(&mut self) {
    let mut address = self.registers.get_hl_value();
    self.registers.a = self.read_memory(address);
    increment(&mut address);
    self.registers.l = (address % 16) as u8;
    self.registers.h = (address >> 4) as u8;
  }

  fn save_a_to_hl_address_inc(&mut self) {
    let mut address = self.registers.get_hl_value();
    self.write_memory(usize::from(address), self.registers.a);
    increment(&mut address);
    self.registers.l = (address % 16) as u8;
    self.registers.h = (address >> 4) as u8;
  }

  fn load_immediate_to_register_pair(&mut self, pair: RegisterPairs) {
    let lesser = self.read_memory_from_pc();
    let greater = self.read_memory_from_pc();
    match pair {
      RegisterPairs::BC => {self.registers.b = greater;self.registers.c = lesser},
      RegisterPairs::DE => {self.registers.d = greater;self.registers.e = lesser},
      RegisterPairs::HL => {self.registers.h = greater;self.registers.l = lesser},
    };

  }

  fn save_sp_to_immediate_address(&mut self) {
    let lesser = self.read_memory_from_pc();
    let greater = self.read_memory_from_pc();
    let address = Registers::join_u8(greater, lesser);
    let lsb = (self.registers.sp % 16) as u8;
    let msb = (self.registers.sp >> 4) as u8;
    self.write_memory(usize::from(address), lsb);
    self.write_memory(usize::from(address + 1), msb);
  }
  
  fn load_hl_to_sp(&mut self) {
    self.registers.sp = self.registers.get_hl_value();
  }

  fn push_bc_to_stack(&mut self) {
    self.registers.sp -=1;
    self.write_memory(usize::from(self.registers.sp), self.registers.b);
    self.registers.sp -=1;
    self.write_memory(usize::from(self.registers.sp), self.registers.c);
  }

  fn push_de_to_stack(&mut self) {
    self.registers.sp -=1;
    self.write_memory(usize::from(self.registers.sp), self.registers.d);
    self.registers.sp -=1;
    self.write_memory(usize::from(self.registers.sp), self.registers.e);
  }

  fn push_hl_to_stack(&mut self) {
    self.registers.sp -=1;
    self.write_memory(usize::from(self.registers.sp), self.registers.h);
    self.registers.sp -=1;
    self.write_memory(usize::from(self.registers.sp), self.registers.l);
  }

  fn pop_stack_to_bc(&mut self) {
    self.registers.c = self.read_memory(self.registers.sp);
    self.registers.sp +=1;
    self.registers.b = self.read_memory(self.registers.sp);
    self.registers.sp +=1;
  }

  fn pop_stack_to_de(&mut self) {
    self.registers.e = self.read_memory(self.registers.sp);
    self.registers.sp +=1;
    self.registers.d = self.read_memory(self.registers.sp);
    self.registers.sp +=1;
  }

  fn pop_stack_to_hl(&mut self) {
    self.registers.l = self.read_memory(self.registers.sp);
    self.registers.sp +=1;
    self.registers.h = self.read_memory(self.registers.sp);
    self.registers.sp +=1;
  }

  fn load_adjusted_stack_to_hl(&mut self) {
    let adjustment = self.read_memory_from_pc() as i8;
    let mut stack_pointer = self.registers.sp;
    if adjustment < 0 {
      stack_pointer -= u16::from((-adjustment) as u8);
    } else {
      stack_pointer += u16::from(adjustment as u8);
    }
    self.registers.l = self.read_memory(stack_pointer);
    self.registers.h = self.read_memory(stack_pointer+1);
  }

  fn add_register_to_a(&mut self, register: Register) {
    let value = self.read_register(register);
    self.add_x_to_a(value, 0);
  }

  fn add_hl_data_to_a(&mut self) {
    let value = self.read_memory(self.registers.get_hl_value());
    self.add_x_to_a(value, 0);

  }

  fn add_immediate_to_a(&mut self) {
    let value = self.read_memory_from_pc();
    self.add_x_to_a(value, 0);
  }

  fn add_register_to_a_carry(&mut self, register: Register) {
    let value = self.read_register(register);
    self.add_x_to_a(value, 1);
  }

  fn add_hl_data_to_a_carry(&mut self) {
    let value = self.read_memory(self.registers.get_hl_value());
    self.add_x_to_a(value, 1);

  }

  fn add_immediate_to_a_carry(&mut self) {
    let value = self.read_memory_from_pc();
    self.add_x_to_a(value, 1);
  }

  fn add_x_to_a(&mut self, add_value:u8, carry:u8) {
    if check_half_carry_add(self.registers.a, add_value, carry) {
      self.registers.set_half_carry_flag(FlagActions::Set);
    } else {
      self.registers.set_half_carry_flag(FlagActions::Reset);
    }
  
    match self.registers.a.checked_add(add_value) {
      Some(x) => {
        match x.checked_add(carry) {
          Some(y) => {
            self.registers.a = y;
            self.registers.set_carry_flag(FlagActions::Reset);
          },
          None => {
            self.registers.a = u8::MIN;
            self.registers.set_carry_flag(FlagActions::Set);
          }
        } 
      },
      None => {
        let a = Wrapping(self.registers.a);
        let b = Wrapping(add_value);
        let c = Wrapping(carry);
        self.registers.a = (a + b + c).0;

        self.registers.set_carry_flag(FlagActions::Set);
      }
    }

    if self.registers.a == 0 {
      self.registers.set_zero_flag(FlagActions::Set);
    } else {
      self.registers.set_zero_flag(FlagActions::Reset);
    }

    self.registers.set_sub_flag(FlagActions::Reset);
  }

  fn sub_register_to_a(&mut self, register: Register) {
    let value = self.read_register(register);
    self.sub_x_to_a(value, 0);
  }

  fn sub_immediate_to_a(&mut self) {
    let value = self.read_memory_from_pc();
    self.sub_x_to_a(value, 0);
  }

  fn sub_hl_data_to_a(&mut self) {
    let value = self.read_memory(self.registers.get_hl_value());
    self.sub_x_to_a(value, 0);
  }

  fn sub_register_to_a_carry(&mut self, register: Register) {
    let value = self.read_register(register);
    self.sub_x_to_a(value, 1);
  }

  fn sub_immediate_to_a_carry(&mut self) {
    let value = self.read_memory_from_pc();
    self.sub_x_to_a(value, 1);
  }

  fn sub_hl_data_to_a_carry(&mut self) {
    let value = self.read_memory(self.registers.get_hl_value());
    self.sub_x_to_a(value, 1);
  }

  fn sub_x_to_a(&mut self, sub_value:u8, carry:u8) {

    if check_half_carry_sub(self.registers.a, sub_value, carry) {
      self.registers.set_half_carry_flag(FlagActions::Set);
    } else {
      self.registers.set_half_carry_flag(FlagActions::Reset);
    }

    match self.registers.a.checked_sub(sub_value) {
      Some(x) => {
        self.registers.a = x;

        self.registers.set_carry_flag(FlagActions::Reset);
      },
      None => {
        match (self.registers.a + carry).checked_sub(sub_value) {
          Some(_) => {
            self.registers.a = u8::MIN;
          },
          None => {
            let a = Wrapping(self.registers.a);
            let b = Wrapping(sub_value);
            let c = Wrapping(carry);
            self.registers.a = (a - b + c).0;
    
            self.registers.set_carry_flag(FlagActions::Set);
          }
        }

      }
    }

    if self.registers.a == 0 {
      self.registers.set_zero_flag(FlagActions::Set);
    } else {
      self.registers.set_zero_flag(FlagActions::Reset);
    }

    self.registers.set_sub_flag(FlagActions::Set);
  }

  fn cmp_register_to_a(&mut self, register: Register) {
    let value = self.read_register(register);
    self.cmp_x_to_a(value);
  }

  fn cmp_hl_data_to_a(&mut self) {
    let value = self.read_memory(self.registers.get_hl_value());
    self.cmp_x_to_a(value);
  }

  fn cmp_immediate_to_a(&mut self) {
    let value = self.read_memory_from_pc();
    self.cmp_x_to_a(value);
  }

  fn cmp_x_to_a(&mut self, cmp_value:u8) {
    if check_half_carry_sub(self.registers.a, cmp_value, 0) {
      self.registers.set_half_carry_flag(FlagActions::Set);
    } else {
      self.registers.set_half_carry_flag(FlagActions::Reset);
    }

    match self.registers.a.checked_sub(cmp_value) {
      Some(x) => {
        self.registers.set_carry_flag(FlagActions::Reset);
        if x == 0 {
          self.registers.set_zero_flag(FlagActions::Set);
        } else {
          self.registers.set_zero_flag(FlagActions::Reset);
        }
      },
      None => {
        self.registers.set_carry_flag(FlagActions::Set);
        self.registers.set_zero_flag(FlagActions::Reset);
      }
    }

    self.registers.set_sub_flag(FlagActions::Set);
  }

  fn inc_hl_data(&mut self) {
    let value = Wrapping(self.read_memory(self.registers.get_hl_value()));
    let one = Wrapping(1u8);
    
    if check_half_carry_add(value.0, 1, 0) {
      self.registers.set_half_carry_flag(FlagActions::Set);
    } else {
      self.registers.set_half_carry_flag(FlagActions::Reset);      
    }

    self.write_memory(usize::from(self.registers.get_hl_value()),(value + one).0);
    
    self.registers.set_sub_flag(FlagActions::Reset);
    
    if self.read_memory(self.registers.get_hl_value()) == 0 {
      self.registers.set_zero_flag(FlagActions::Set)
    } else {
      self.registers.set_zero_flag(FlagActions::Reset);
    }
  }

  fn inc_register(&mut self, register: Register) {
    let value = Wrapping(self.read_register(register));
    let one = Wrapping(1u8);

    if check_half_carry_add(value.0, 1, 0) {
      self.registers.set_half_carry_flag(FlagActions::Set);
    } else {
      self.registers.set_half_carry_flag(FlagActions::Reset);      
    }

    self.write_register(register, (value + one).0);

    self.registers.set_sub_flag(FlagActions::Reset);

    if self.read_register(register) == 0 {
      self.registers.set_zero_flag(FlagActions::Set)
    } else {
      self.registers.set_zero_flag(FlagActions::Reset);
    }
  }

  fn dec_hl_data(&mut self) {
    let value = Wrapping(self.read_memory(self.registers.get_hl_value()));
    let one = Wrapping(1u8);

    if check_half_carry_sub(value.0, 1, 0) {
      self.registers.set_half_carry_flag(FlagActions::Set);
    } else {
      self.registers.set_half_carry_flag(FlagActions::Reset);      
    }

    self.write_memory(usize::from(self.registers.get_hl_value()),(value - one).0);

    self.registers.set_sub_flag(FlagActions::Reset);

    if self.read_memory(self.registers.get_hl_value()) == 0 {
      self.registers.set_zero_flag(FlagActions::Set)
    } else {
      self.registers.set_zero_flag(FlagActions::Reset);
    }
  }

  fn dec_register(&mut self, register: Register) {
    let value = Wrapping(self.read_register(register));
    let one = Wrapping(1u8);

    if check_half_carry_sub(value.0, 1, 0) {
      self.registers.set_half_carry_flag(FlagActions::Set);
    } else {
      self.registers.set_half_carry_flag(FlagActions::Reset);      
    }

    self.write_register(register, (value - one).0);

    self.registers.set_sub_flag(FlagActions::Reset);

    if self.read_register(register) == 0 {
      self.registers.set_zero_flag(FlagActions::Set)
    } else {
      self.registers.set_zero_flag(FlagActions::Reset);
    }
  }

  fn and_hl_data_to_a(&mut self) {
    self.registers.set_carry_flag(FlagActions::Reset);
    self.registers.set_half_carry_flag(FlagActions::Set);
    self.registers.set_sub_flag(FlagActions::Reset);

    self.registers.a &= self.read_memory(self.registers.get_hl_value());
    if self.registers.a == 0 {
      self.registers.set_zero_flag(FlagActions::Set);
    } else {
      self.registers.set_zero_flag(FlagActions::Reset);
    }
  }

  fn and_register_to_a(&mut self, register: Register) {
    self.registers.set_carry_flag(FlagActions::Reset);
    self.registers.set_half_carry_flag(FlagActions::Set);
    self.registers.set_sub_flag(FlagActions::Reset);

    self.registers.a &= self.read_register(register);
    if self.registers.a == 0 {
      self.registers.set_zero_flag(FlagActions::Set);
    } else {
      self.registers.set_zero_flag(FlagActions::Reset);
    }
  }

  fn and_immediate_to_a(&mut self) {
    self.registers.set_carry_flag(FlagActions::Reset);
    self.registers.set_half_carry_flag(FlagActions::Set);
    self.registers.set_sub_flag(FlagActions::Reset);

    self.registers.a &= self.read_memory_from_pc();
    if self.registers.a == 0 {
      self.registers.set_zero_flag(FlagActions::Set);
    } else {
      self.registers.set_zero_flag(FlagActions::Reset);
    }
  }

  fn or_hl_data_to_a(&mut self) {
    self.registers.set_carry_flag(FlagActions::Reset);
    self.registers.set_half_carry_flag(FlagActions::Reset);
    self.registers.set_sub_flag(FlagActions::Reset);

    self.registers.a |= self.read_memory(self.registers.get_hl_value());
    if self.registers.a == 0 {
      self.registers.set_zero_flag(FlagActions::Set);
    } else {
      self.registers.set_zero_flag(FlagActions::Reset);
    }
  }

  fn or_register_to_a(&mut self, register: Register) {
    self.registers.set_carry_flag(FlagActions::Reset);
    self.registers.set_half_carry_flag(FlagActions::Reset);
    self.registers.set_sub_flag(FlagActions::Reset);

    self.registers.a |= self.read_register(register);
    if self.registers.a == 0 {
      self.registers.set_zero_flag(FlagActions::Set);
    } else {
      self.registers.set_zero_flag(FlagActions::Reset);
    }
  }

  fn or_immediate_to_a(&mut self) {
    self.registers.set_carry_flag(FlagActions::Reset);
    self.registers.set_half_carry_flag(FlagActions::Reset);
    self.registers.set_sub_flag(FlagActions::Reset);

    self.registers.a |= self.read_memory_from_pc();
    if self.registers.a == 0 {
      self.registers.set_zero_flag(FlagActions::Set);
    } else {
      self.registers.set_zero_flag(FlagActions::Reset);
    }
  }

  fn xor_hl_data_to_a(&mut self) {
    self.registers.set_carry_flag(FlagActions::Reset);
    self.registers.set_half_carry_flag(FlagActions::Reset);
    self.registers.set_sub_flag(FlagActions::Reset);

    self.registers.a ^= self.read_memory(self.registers.get_hl_value());
    if self.registers.a == 0 {
      self.registers.set_zero_flag(FlagActions::Set);
    } else {
      self.registers.set_zero_flag(FlagActions::Reset);
    }
  }

  fn xor_register_to_a(&mut self, register: Register) {
    self.registers.set_carry_flag(FlagActions::Reset);
    self.registers.set_half_carry_flag(FlagActions::Reset);
    self.registers.set_sub_flag(FlagActions::Reset);

    self.registers.a ^= self.read_register(register);
    if self.registers.a == 0 {
      self.registers.set_zero_flag(FlagActions::Set);
    } else {
      self.registers.set_zero_flag(FlagActions::Reset);
    }
  }

  fn xor_immediate_to_a(&mut self) {
    self.registers.set_carry_flag(FlagActions::Reset);
    self.registers.set_half_carry_flag(FlagActions::Reset);
    self.registers.set_sub_flag(FlagActions::Reset);

    self.registers.a ^= self.read_memory_from_pc();
    if self.registers.a == 0 {
      self.registers.set_zero_flag(FlagActions::Set);
    } else {
      self.registers.set_zero_flag(FlagActions::Reset);
    }
  }

  fn ccflag(&mut self) {
    self.registers.set_carry_flag(FlagActions::Flip);
    self.registers.set_half_carry_flag(FlagActions::Reset);
    self.registers.set_sub_flag(FlagActions::Reset);
  }

  fn scflag(&mut self) {
    self.registers.set_carry_flag(FlagActions::Set);
    self.registers.set_half_carry_flag(FlagActions::Reset);
    self.registers.set_sub_flag(FlagActions::Reset);
  }

}

  pub fn run() {
    let mut registers = Registers {
      pc: 0b1111_1111_1111_1111,
      sp: 0b1010_0101_1001_0110,
      a: 0b0000_1111, f: 0b10100101,
      b: 0b1000_1111, c: 0b0001_1111,
      d: 0b0100_1111, e: 0b0010_1111,
      h: 0b0010_1111, l: 0b1000_1111,
    };
    let mut memory = Memory {
      data: Box::new([0; 0x10000]),
    };

    registers.dump_registers();
    memory.write_memory(usize::from(registers.pc), 0b0000_0100);
    let data = memory.read_memory_from_pc(&mut registers);
    let mut state = RunningState {
      registers,
      memory,
    };
    state.perform_action(Instruction::translate(data));
    state.registers.set_zero_flag(FlagActions::Flip);
    state.registers.set_sub_flag(FlagActions::Set);
    state.registers.set_half_carry_flag(FlagActions::Flip);
    state.registers.set_carry_flag(FlagActions::Reset);
    state.registers.dump_registers();
    println!("{data}");
  }



}

 