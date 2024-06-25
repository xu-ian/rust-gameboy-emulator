pub mod cpu {
  
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

    pub fn set_subtraction_flag(&mut self, flag: FlagActions) {
      match flag {
        FlagActions::Set => self.f |= 0b0100_0000,
        FlagActions::Reset => self.f &= 0b1011_1111,
        FlagActions::Flip => self.f ^= 0b0100_0000
      }
    }

    pub fn set_half_carry_flag(&mut self, flag: FlagActions) {
      match flag {
        FlagActions::Set => self.f |= 0b0010_0000,
        FlagActions::Reset => self.f &= 0b1101_1111,
        FlagActions::Flip => self.f ^= 0b0010_0000
      }
    }

    pub fn set_carry_flag(&mut self, flag: FlagActions) {
      match flag {
        FlagActions::Set => self.f |= 0b0001_0000,
        FlagActions::Reset => self.f &= 0b1110_1111,
        FlagActions::Flip => self.f ^= 0b0001_0000
      }
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
      } else if check(instruction, 1,0,0,1,1,1,1,0) {
        Instruction::Compare(
          Dest::HL
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

  fn perform_action(instruction: Instruction, registers: &mut Registers, memory: &mut Memory) {
    match instruction {
      //Saves Immediate into location specified by HL
      Instruction::Load(6, Dest::Immediate) => load_immediate_to_hl(registers, memory),
      //Loads Immediate into register
      Instruction::Load(register, Dest::Immediate) => load_immediate_to_register(registers, memory, register),
      //Saves register value to HL memory location
      Instruction::Load(6, Dest::Reg(register)) => save_register_to_hl(registers, memory, register),
      //Loads data from HL memory location to register
      Instruction::Load(register, Dest::Reg(6)) => load_h1_to_register(registers, memory, register),
      //Copies data from one register to another
      Instruction::Load(reg1, Dest::Reg(reg2)) => register_to_register_copy(registers, reg1, reg2),
      //Loads A with memory at location BC
      Instruction::AfBC(MemAction::Load) => load_bc_address_to_a(registers, memory),
      //Saves A at memory location BC
      Instruction::AfBC(MemAction::Save) => save_a_to_bc_address(registers, memory),
      //Loads A with memory at location DE
      Instruction::AfDE(MemAction::Load) => load_de_address_to_a(registers, memory),
      //Saves A at memory location DE
      Instruction::AfDE(MemAction::Save) => save_a_to_de_address(registers, memory),
      //Loads A with memory at location from immediate
      Instruction::Ann(MemAction::Load) => load_immediate_address_to_a(registers, memory),
      //Saves A at memory location from immediate
      Instruction::Ann(MemAction::Save) => save_a_to_immediate_address(registers, memory),
      //Loads A with memory at location 0xFF00 + C
      Instruction::AXC(MemAction::Load) => load_ff_c_to_a(registers, memory),
      //Saves A at memory location 0xFF00 + C
      Instruction::AXC(MemAction::Save) => save_a_to_ff_c(registers, memory),
      //Loads A with memory at location 0xFF00 + immediate
      Instruction::AXn(MemAction::Load) => load_ff_n_to_a(registers, memory),
      //Saves A at memory location 0xFF00 + immediate
      Instruction::AXn(MemAction::Save) => save_a_to_ff_n(registers, memory),
      //Loads A with memory at location HL, then decrements HL
      Instruction::AfHLdec(MemAction::Load) => load_hl_address_to_a_dec(registers, memory),
      //Saves A at memory location HL, then decrements HL
      Instruction::AfHLdec(MemAction::Save) => save_a_to_hl_address_dec(registers, memory),
      //Loads A with memory at location HL, then increments HL
      Instruction::AfHLinc(MemAction::Load) => load_hl_address_to_a_inc(registers, memory),
      //Saves A at memory location HL, then increments HL
      Instruction::AfHLinc(MemAction::Save) => save_a_to_hl_address_inc(registers, memory),
      //Loads immediate to a register pair
      Instruction::Loadnn(Dest::RegPair(pair)) => load_immediate_to_register_pair(registers, memory, pair),
      //Saves Data from stack pointer to memory address at immediate
      Instruction::SaveSPnn => save_sp_to_immediate_address(registers, memory),
      Instruction::LoadSPHL => load_hl_to_sp(registers),

      _ => return
    }
  }

  fn load_immediate_to_hl(registers: &mut Registers, memory: &mut Memory) {
    let data = memory.read_memory_from_pc(registers);
    memory.write_memory(usize::from(registers.get_hl_value()), data);
  }

  fn load_immediate_to_register(registers: &mut Registers, memory: &Memory, register: Register) {
    let data = memory.read_memory_from_pc(registers);
    registers.write_register(register, data);
  }

  fn save_register_to_hl(registers: &mut Registers, memory: &mut Memory, register: Register) {
    let data = registers.read_register(register);
    memory.write_memory(usize::from(registers.get_hl_value()), data);
  }

  fn load_h1_to_register(registers: &mut Registers, memory: &mut Memory, register: Register) {
    let data = memory.read_memory(registers.get_hl_value());
    registers.write_register(register, data);
  }

  fn register_to_register_copy(registers: &mut Registers, dst: Register, src: Register) {
    let data = registers.read_register(src);
    registers.write_register(dst, data);
  }

  fn load_bc_address_to_a(registers: &mut Registers, memory: &Memory) {
    registers.a = memory.read_memory(registers.get_bc_value());
  } 

  fn save_a_to_bc_address(registers: &Registers, memory: &mut Memory) {
    memory.write_memory(usize::from(registers.get_bc_value()), registers.a);
  }

  fn load_de_address_to_a(registers: &mut Registers, memory: &Memory) {
    registers.a = memory.read_memory(registers.get_de_value());
  } 

  fn save_a_to_de_address(registers: &Registers, memory: &mut Memory) {
    memory.write_memory(usize::from(registers.get_de_value()), registers.a);
  }

  fn load_immediate_address_to_a(registers: &mut Registers, memory: &mut Memory) {
    let leastbyte = memory.read_memory_from_pc(registers);
    let mostbyte = memory.read_memory_from_pc(registers);
    let address = Registers::join_u8(mostbyte, leastbyte);

    registers.a = memory.read_memory(address);
  }

  fn save_a_to_immediate_address(registers: &mut Registers, memory: &mut Memory) {
    let leastbyte = memory.read_memory_from_pc(registers);
    let mostbyte = memory.read_memory_from_pc(registers);
    let address = Registers::join_u8(mostbyte, leastbyte);
    memory.write_memory(usize::from(address), registers.a);
  }

  fn load_ff_c_to_a(registers: &mut Registers, memory: &mut Memory) {
    let location = Registers::join_u8(0xFF, registers.c);
    registers.a = memory.read_memory(location);
  }

  fn save_a_to_ff_c(registers: &mut Registers, memory: &mut Memory) {
    let location = Registers::join_u8(0xFF, registers.c);
    memory.write_memory(usize::from(location), registers.a);
  }

  fn load_ff_n_to_a(registers: &mut Registers, memory: &mut Memory) {
    let next_mem = memory.read_memory_from_pc(registers);
    let location = Registers::join_u8(0xFF, next_mem);
    registers.a = memory.read_memory(location);
  }

  fn save_a_to_ff_n(registers: &mut Registers, memory: &mut Memory) {
    let next_mem = memory.read_memory_from_pc(registers);
    let location = Registers::join_u8(0xFF, next_mem);
    memory.write_memory(usize::from(location), registers.a);
  }

  fn load_hl_address_to_a_dec(registers: &mut Registers, memory: &mut Memory) {
    let mut address = registers.get_hl_value();
    registers.a = memory.read_memory(address);
    decrement(&mut address);
    registers.l = (address % 16) as u8;
    registers.h = (address >> 4) as u8;
  }

  fn save_a_to_hl_address_dec(registers: &mut Registers, memory: &mut Memory) {
    let mut address = registers.get_hl_value();
    memory.write_memory(usize::from(address), registers.a);
    decrement(&mut address);
    registers.l = (address % 16) as u8;
    registers.h = (address >> 4) as u8;
  }

  fn load_hl_address_to_a_inc(registers: &mut Registers, memory: &mut Memory) {
    let mut address = registers.get_hl_value();
    registers.a = memory.read_memory(address);
    increment(&mut address);
    registers.l = (address % 16) as u8;
    registers.h = (address >> 4) as u8;
  }

  fn save_a_to_hl_address_inc(registers: &mut Registers, memory: &mut Memory) {
    let mut address = registers.get_hl_value();
    memory.write_memory(usize::from(address), registers.a);
    increment(&mut address);
    registers.l = (address % 16) as u8;
    registers.h = (address >> 4) as u8;
  }

  fn load_immediate_to_register_pair(registers: &mut Registers, memory: &mut Memory, pair: RegisterPairs) {
    let lesser = memory.read_memory_from_pc(registers);
    let greater = memory.read_memory_from_pc(registers);
    match pair {
      RegisterPairs::BC => {registers.b = greater;registers.c = lesser},
      RegisterPairs::DE => {registers.d = greater;registers.e = lesser},
      RegisterPairs::HL => {registers.h = greater;registers.l = lesser},
    };

  }

  fn save_sp_to_immediate_address(registers: &mut Registers, memory: &mut Memory) {
    let lesser = memory.read_memory_from_pc(registers);
    let greater = memory.read_memory_from_pc(registers);
    let address = Registers::join_u8(greater, lesser);
    let lsb = (registers.sp % 16) as u8;
    let msb = (registers.sp >> 4) as u8;
    memory.write_memory(usize::from(address), lsb);
    memory.write_memory(usize::from(address + 1), msb);
  }
  
  fn load_hl_to_sp(registers: &mut Registers) {
    registers.sp = registers.get_hl_value();
  }

  fn push_register_pair_to_stack(registers: &mut Registers, memory: &mut Memory) {
    
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
    perform_action(Instruction::translate(data), &mut registers, &mut memory);
    registers.set_zero_flag(FlagActions::Flip);
    registers.set_subtraction_flag(FlagActions::Set);
    registers.set_half_carry_flag(FlagActions::Flip);
    registers.set_carry_flag(FlagActions::Reset);
    registers.dump_registers();
    println!("{data}");
  }



}

 