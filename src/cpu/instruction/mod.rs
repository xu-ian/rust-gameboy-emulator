
use super::registers::Register;
use std::fmt;
use std::fmt::*;

#[derive(Debug, PartialEq)]
pub enum RegisterPairs {
  BC,
  DE,
  HL,
  AF,
}

impl fmt::Display for RegisterPairs {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    match self {
      RegisterPairs::BC => write!(f, "BC"),
      RegisterPairs::DE => write!(f, "DE"),
      RegisterPairs::HL => write!(f, "HL"),
      RegisterPairs::AF => write!(f, "AF"),
    }
  }
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum Cond {
  NZ, //00
  Z,  //01
  NC, //10
  C   //11
}

impl fmt::Display for Cond {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    match self {
      Cond::C => write!(f, "C"),
      Cond::NC => write!(f, "NC"),
      Cond::Z => write!(f, "Z"),
      Cond::NZ => write!(f, "NZ"),
    }
  }
}

#[derive(Debug, PartialEq)]
pub enum MemAction {
  Save,
  Load
}

impl fmt::Display for MemAction {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    match self {
      MemAction::Save => write!(f, "Save"),
      MemAction::Load => write!(f, "Load"),
    }
  }
}

#[derive(Debug, PartialEq)]
pub enum Dest {
  Reg(Register),
  RegPair(RegisterPairs),
  HL,
  Immediate,
}

impl fmt::Display for Dest {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    match self {
      Dest::Reg(x) => write!(f, "Reg({})", x),
      Dest::RegPair(x) => write!(f, "RegPair({})", x),
      Dest::HL => write!(f, "HL"),
      Dest::Immediate => write!(f, "Immediate"),
    }
  }
}



#[derive(Debug, PartialEq)]
pub enum Instruction {
  //Loads the specified register with the data from the destination
  Load(Register, Dest),
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
  JumpCond(Cond),
  //Increases/Decreases program counter by value of next byte
  JumpRel,
  //Increases/Decreases program counter by value of next byte if condition is fulfilled
  JumpRelCond(Cond),
  //Function Call. Stack pointer advances, program counter jumps to function position. Jump dest is next 2 bytes
  Call,
  //Conditional Function Call. See above. 
  CondCall(Cond),
  //Return from function load and roll back stack to program counter
  Ret,
  //Conditional return from function load. See above
  CondRet(Cond),
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

impl fmt::Display for Instruction {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    match self {
      Instruction::Load(reg, dst) => write!(f, "Load({}, {})", reg, dst),
      Instruction::AfBC(m_act) => write!(f, "AfBC({})", m_act),
      Instruction::AfDE(m_act) => write!(f, "AfDE({})", m_act),
      Instruction::Ann(m_act) => write!(f, "Ann({})", m_act),
      Instruction::AXC(m_act) => write!(f, "AXC({})", m_act),
      Instruction::AXn(m_act) => write!(f, "AXn({})", m_act),
      Instruction::AfHLdec(m_act) => write!(f, "AfHLdec({})", m_act),
      Instruction::AfHLinc(m_act) => write!(f, "AfHLinc({})", m_act),
      Instruction::Loadnn(dst) => write!(f, "Loadnn({})", dst),
      Instruction::SaveSPnn => write!(f, "SaveSPnn"),
      Instruction::LoadSPHL => write!(f, "LoadSPHL"),
      Instruction::PushStack(dst) => write!(f, "PushStack({})", dst),
      Instruction::PopStack(dst) => write!(f, "PopStack({})", dst),
      Instruction::LoadStackAdj => write!(f, "LoadStackAdj"),
      Instruction::Add(dst) => write!(f, "Add({})", dst),
      Instruction::AddCarry(dst) => write!(f, "AddCarry({})", dst),
      Instruction::Sub(dst) => write!(f, "Sub({})", dst),
      Instruction::SubCarry(dst) => write!(f, "SubCarry({})", dst),
      Instruction::Compare(dst) => write!(f, "Compare({})", dst),
      Instruction::Inc(dst) => write!(f, "Inc({})", dst),
      Instruction::Dec(dst) => write!(f, "Dec({})", dst),
      Instruction::AND(dst) => write!(f, "AND({})", dst),
      Instruction::OR(dst) => write!(f, "OR({})", dst),
      Instruction::XOR(dst) => write!(f, "XOR({})", dst),
      Instruction::CCFlag => write!(f, "CCFlag"),
      Instruction::SCFlag => write!(f, "SCFlag"),
      Instruction::DAA => write!(f, "DAA"),
      Instruction::CmpA => write!(f, "CmpA"),
      Instruction::Inc16(dst) => write!(f, "Inc16({})", dst),
      Instruction::Dec16(dst) => write!(f, "Dec16({})", dst),
      Instruction::Add16(dst) => write!(f, "Add16({})", dst),
      Instruction::SPAddn => write!(f, "SPAddn"),
      Instruction::RotateLC(dst) => write!(f, "RotateLC({})", dst),
      Instruction::RotateRC(dst) => write!(f, "RotateRC({})", dst),
      Instruction::RotateL(dst) => write!(f, "RotateL({})", dst),
      Instruction::RotateR(dst) => write!(f, "RotateR({})", dst),
      Instruction::ShiftL(dst) => write!(f, "ShiftL({})", dst),
      Instruction::ShiftRArith(dst) => write!(f, "ShiftRArith({})", dst),
      Instruction::ShiftRLog(dst) => write!(f, "ShiftRLog({})", dst),
      Instruction::SwapNibble(dst) => write!(f, "SwapNibble({})", dst),
      Instruction::TestBit(u8, dst) => write!(f, "TestBIt({}, {})", u8, dst),
      Instruction::ResetBit(u8, dst) => write!(f, "ResetBit({}, {})", u8, dst),
      Instruction::SetBit(u8, dst) => write!(f, "SetBit({}, {})", u8, dst),
      Instruction::Prefix => write!(f, "Prefix"),
      Instruction::Jump(dst) => write!(f, "Jump({})", dst),
      Instruction::JumpCond(cond) => write!(f, "JumpCond({})", cond),
      Instruction::JumpRel => write!(f, "JumpRel"),
      Instruction::JumpRelCond(cond) => write!(f, "JumpRelCond({})", cond),
      Instruction::Call => write!(f, "Call"),
      Instruction::CondCall(cond) => write!(f, "CondCall({})", cond),
      Instruction::Ret => write!(f, "Ret"),
      Instruction::CondRet(cond) => write!(f, "CondRet({})", cond),
      Instruction::RetI => write!(f, "RetI"),
      Instruction::Restart(u8) => write!(f, "Restart({})", u8),
      Instruction::HALT => write!(f, "HALT"),
      Instruction::STOP => write!(f, "STOP"),
      Instruction::DI => write!(f, "DI"),
      Instruction::EI => write!(f, "EI"),
      Instruction::NOP => write!(f, "NOP"),
    }
  }
}

fn check(instruction:u8, b7:u8, b6:u8, b5:u8, b4:u8, b3:u8, b2:u8, b1:u8, b0:u8) -> bool {
  if (b0 == 2 || bits(instruction.clone(), 0, 1) == b0) &&
     (b1 == 2 || bits(instruction.clone(), 1, 1) == b1) &&
     (b2 == 2 || bits(instruction.clone(), 2, 1) == b2) &&
     (b3 == 2 || bits(instruction.clone(), 3, 1) == b3) &&
     (b4 == 2 || bits(instruction.clone(), 4, 1) == b4) &&
     (b5 == 2 || bits(instruction.clone(), 5, 1) == b5) &&
     (b6 == 2 || bits(instruction.clone(), 6, 1) == b6) &&
     (b7 == 2 || bits(instruction.clone(), 7, 1) == b7) {
    true
  } else {
    false
  }
}

fn compute_condition(bits: u8) -> Cond {
  if bits == 0 {
    Cond::NZ
  } else if bits == 1 {
    Cond::Z
  } else if bits == 2 {
    Cond::NC
  } else if bits == 3 {
    Cond::C
  } else {
    panic!("Should not reach here as 2 bits should only have 4 options");
  }
}

fn get_register_pair(pair:u8) -> RegisterPairs{
  if pair == 0 {
    RegisterPairs::BC
  } else if pair == 1 {
    RegisterPairs::DE
  } else if pair == 2 {
    RegisterPairs::HL
  } else if pair == 3 {
    RegisterPairs::AF
  } else {
    panic!("Should not be any number other than 0-3");
  }
}

//Split into CB and non-CB prefixed instructions
impl Instruction {
  pub fn translate(instruction: u8) -> Instruction {
    if check(instruction, 0,0,0,1,0,0,0,0) {
      Instruction::STOP
    } else if check(instruction, 0,1,1,1,0,1,1,0) {
      Instruction::HALT
    } else if check(instruction,0,1,2,2,2,2,2,2) {
      Instruction::Load(
        bits(instruction, 3, 0b111),
        Dest::Reg(bits(instruction, 0, 0b111))
      )
    } else if check(instruction, 0,0,2,2,2,1,1,0) {
      Instruction::Load(
        bits(instruction, 3, 0b111),
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
      let pair: RegisterPairs = get_register_pair(bits(instruction, 4, 0b11));

      Instruction::Loadnn(
        Dest::RegPair(pair)
      )
    } else if check(instruction, 0,0,0,0,1,0,0,0) {
      Instruction::SaveSPnn
    } else if check(instruction, 1,1,1,1,1,0,0,1) {
      Instruction::LoadSPHL
    } else if check(instruction, 1,1,2,2,0,1,0,1) {
      let pair = get_register_pair(bits(instruction, 4, 0b11));
      
      Instruction::PushStack(
        Dest::RegPair(pair)
      )
    } else if check(instruction, 1,1,2,2,0,0,0,1) {
      let pair = get_register_pair(bits(instruction, 4, 0b11));

      Instruction::PopStack(
        Dest::RegPair(pair)
      )
    } else if check(instruction, 1,1,1,1,1,0,0,0) {
      Instruction::LoadStackAdj
    } else if check(instruction, 1,0,0,0,0,2,2,2) {
      Instruction::Add(
        Dest::Reg(bits(instruction, 0, 0b111))
      )
    } else if check(instruction, 1,1,0,0,0,1,1,0) {
      Instruction::Add(
        Dest::Immediate
      )
    } else if check(instruction, 1,0,0,0,1,2,2,2) {
      Instruction::AddCarry(
        Dest::Reg(bits(instruction, 0, 0b111))
      )
    } else if check(instruction, 1,1,0,0,1,1,1,0) {
      Instruction::AddCarry(
        Dest::Immediate
      )
    } else if check(instruction, 1,0,0,1,0,2,2,2) {
      Instruction::Sub(
        Dest::Reg(bits(instruction, 0, 0b111))
      )
    } else if check(instruction, 1,1,0,1,0,1,1,0) {
      Instruction::Sub(
        Dest::Immediate
      )        
    } else if check(instruction, 1,0,0,1,1,2,2,2) {
      Instruction::SubCarry(
        Dest::Reg(bits(instruction, 0, 0b111))
      )
    } else if check(instruction, 1,1,0,1,1,1,1,0) {
      Instruction::SubCarry(
        Dest::Immediate
      )        
    } else if check(instruction, 1,0,1,1,1,2,2,2) {
      Instruction::Compare(
        Dest::Reg(bits(instruction, 0, 0b111))
      )
    } else if check(instruction, 1,1,1,1,1,1,1,0) {
      Instruction::Compare(
        Dest::Immediate
      )
    } else if check(instruction, 0,0,2,2,2,1,0,0) {
      Instruction::Inc(
        Dest::Reg(bits(instruction, 3, 0b111))
      )
    } else if check(instruction, 0,0,2,2,2,1,0,1) {
      Instruction::Dec(
        Dest::Reg(bits(instruction, 3, 0b111))
      )
    } else if check(instruction, 1,0,1,0,0,2,2,2) {
      Instruction::AND(
        Dest::Reg(bits(instruction,0,0b111))
      )
    } else if check(instruction, 1,1,1,0,0,1,1,0) {
      Instruction::AND(
        Dest::Immediate
      )        
    } else if check(instruction, 1,0,1,1,0,2,2,2) {
      Instruction::OR(
        Dest::Reg(bits(instruction, 0, 0b111))
      )
    } else if check(instruction, 1,1,1,1,0,1,1,0) {
      Instruction::OR(
        Dest::Immediate
      )        
    } else if check(instruction, 1,0,1,0,1,2,2,2) {
      Instruction::XOR(
        Dest::Reg(bits(instruction,0,0b111))
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
      let pair = get_register_pair(bits(instruction, 4, 0b11));
      Instruction::Inc16(
        Dest::RegPair(pair)
      )
    } else if check(instruction, 0,0,2,2,1,0,1,1) {
      let pair = get_register_pair(bits(instruction, 4, 0b11));
      Instruction::Dec16(
        Dest::RegPair(pair)
      )
    } else if check(instruction, 0,0,2,2,1,0,0,1) {
      let pair = get_register_pair(bits(instruction, 4, 0b11));
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
        compute_condition(bits(instruction, 3, 0b11))
      )
    } else if check(instruction, 0,0,0,1,1,0,0,0) {
      Instruction::JumpRel
    } else if check(instruction, 0,0,1,2,2,0,0,0) {
      Instruction::JumpRelCond(
        compute_condition(bits(instruction, 3, 0b11))
      )
    } else if check(instruction, 1,1,0,0,1,1,0,1) {
      Instruction::Call
    } else if check(instruction, 1,1,0,2,2,1,0,0) {
      Instruction::CondCall(
        compute_condition(bits(instruction, 3, 0b11))
      )
    } else if check(instruction, 1,1,0,0,1,0,0,1) {
      Instruction::Ret
    } else if check(instruction, 1,1,0,2,2,0,0,0) {
      Instruction::CondRet(
        compute_condition(bits(instruction, 3, 0b11))
      )
    } else if check(instruction, 1,1,0,1,1,0,0,1) {
      Instruction::RetI
    } else if check(instruction, 1,1,2,2,2,1,1,1) {
      Instruction::Restart(
        bits(instruction, 3, 0b111)
      )
    } else if check(instruction, 1,1,1,1,0,0,1,1) {
      Instruction::DI
    } else if check(instruction, 1,1,1,1,1,0,1,1) {
      Instruction::EI
    } else if instruction == 0x07 {
      Instruction::RotateLC(Dest::Reg(7))
    } else if instruction == 0x0f {
      Instruction::RotateRC(Dest::Reg(7))
    } else if instruction == 0x17 {
      Instruction::RotateL(Dest::Reg(7))
    } else if instruction == 0x1f {
      Instruction::RotateR(Dest::Reg(7))
    } else {
      Instruction::NOP
    }
  }
  
  pub fn translatecb(instruction: u8) -> Instruction {
    if check(instruction, 0,0,0,0,0,2,2,2) {
      Instruction::RotateLC(
        Dest::Reg(bits(instruction,0,0b111))
      )
    } else if check(instruction, 0,0,0,0,1,2,2,2) {
      Instruction::RotateRC(
        Dest::Reg(bits(instruction,0,0b111))
      )
    } else if check(instruction, 0,0,0,1,0,2,2,2) {
      Instruction::RotateL(
        Dest::Reg(bits(instruction,0,0b111))
      )
    } else if check(instruction, 0,0,0,1,1,2,2,2) {
      Instruction::RotateR(
        Dest::Reg(bits(instruction,0,0b111))
      )
    } else if check(instruction, 0,0,1,0,0,2,2,2) {
      Instruction::ShiftL(
        Dest::Reg(bits(instruction, 0, 0b111))
      )
    } else if check(instruction, 0,0,1,0,1,2,2,2) {
      Instruction::ShiftRArith(
        Dest::Reg(bits(instruction, 0, 0b111))
      )
    } else if check(instruction, 0,0,1,1,0,2,2,2) {
      Instruction::SwapNibble(
        Dest::Reg(bits(instruction, 0, 0b111))
      )
    } else if check(instruction, 0,0,1,1,1,2,2,2) {
      Instruction::ShiftRLog(
        Dest::Reg(bits(instruction, 0, 0b111))
      )
    } else if check(instruction, 0,1,2,2,2,2,2,2) {
      Instruction::TestBit(
        bits(instruction, 3, 0b111),
        Dest::Reg(bits(instruction, 0, 0b111))
      )
    } else if check(instruction, 1,0,2,2,2,2,2,2) {
      Instruction::ResetBit(
        bits(instruction, 3, 0b111),
        Dest::Reg(bits(instruction, 0, 0b111))
      )
    } else if check(instruction, 1,1,2,2,2,2,2,2) {
      Instruction::SetBit(
        bits(instruction, 3, 0b111),
        Dest::Reg(bits(instruction, 0, 0b111))
      )
    } else { //This should never be called, if it is called something is wrong
      println!("Something has gone wrong: {instruction}");
      Instruction::NOP
    }
  }
}


/*Gets a specified number of bits from a byte starting from a position
  Ex: byte:01101001, pos:3, bits: 2
  Returns 0b01, the 4th bit(1) and 5th bit(0) from the right from the byte */
fn bits(byte:u8, pos: u8, bits: u8) -> u8 {
  //println!("Number:{:b}, Position:{}, bits read:{:b}", byte, pos, bits);
  (byte >> pos) & bits
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn correct_bitcheck() {
    let bitarray = [0b0000_0001, 0b0000_0010, 0b0000_0100, 0b0000_1000, 
                              0b0001_0000, 0b0010_0000, 0b0100_0000, 0b1000_0000];

    for i in 0u8..7u8 {
      for j in 0u8..7u8 {
        if i == j {
          assert_eq!(bits(bitarray[usize::from(i)], j, 1), 1, "Checking whether bit {j} of {:b} is 1", bitarray[usize::from(i)]);
        } else {
          assert_eq!(bits(bitarray[usize::from(i)], j, 1), 0, "Checking whether bit {j} of {:b} is 0", bitarray[usize::from(i)]);
        }
      }
    }

  }

  #[test]
  fn correct_instruction_single_bitcheck() {
    let bitarray = [0b0000_0001, 0b0000_0010, 0b0000_0100, 0b0000_1000, 
                              0b0001_0000, 0b0010_0000, 0b0100_0000, 0b1000_0000];
      assert_eq!(check(bitarray[0],0,0,0,0,0,0,0,1), true, "Hard comparison does not work for {:b}", bitarray[0]);
      assert_eq!(check(bitarray[1],0,0,0,0,0,0,1,0), true, "Hard comparison does not work for {:b}", bitarray[1]);
      assert_eq!(check(bitarray[2],0,0,0,0,0,1,0,0), true, "Hard comparison does not work for {:b}", bitarray[2]);
      assert_eq!(check(bitarray[3],0,0,0,0,1,0,0,0), true, "Hard comparison does not work for {:b}", bitarray[3]);
      assert_eq!(check(bitarray[4],0,0,0,1,0,0,0,0), true, "Hard comparison does not work for {:b}", bitarray[4]);
      assert_eq!(check(bitarray[5],0,0,1,0,0,0,0,0), true, "Hard comparison does not work for {:b}", bitarray[5]);
      assert_eq!(check(bitarray[6],0,1,0,0,0,0,0,0), true, "Hard comparison does not work for {:b}", bitarray[6]);
      assert_eq!(check(bitarray[7],1,0,0,0,0,0,0,0), true, "Hard comparison does not work for {:b}", bitarray[7]);
  }

  #[test]
  fn correct_instruction_dynamic_bitcheck() {
    let bitarray = [0b0000_0001, 0b0000_0010, 0b0000_0100, 0b0000_1000, 
                              0b0001_0000, 0b0010_0000, 0b0100_0000, 0b1000_0000];
    assert_eq!(check(bitarray[0],2,0,0,0,0,0,0,1), true, "Hard comparison does not work for {:b}", bitarray[0]);
    assert_eq!(check(bitarray[1],0,2,0,0,0,0,1,0), true, "Hard comparison does not work for {:b}", bitarray[1]);
    assert_eq!(check(bitarray[2],0,0,2,0,0,1,0,0), true, "Hard comparison does not work for {:b}", bitarray[2]);
    assert_eq!(check(bitarray[3],0,0,0,2,1,0,0,0), true, "Hard comparison does not work for {:b}", bitarray[3]);
    assert_eq!(check(bitarray[4],0,0,0,1,2,0,0,0), true, "Hard comparison does not work for {:b}", bitarray[4]);
    assert_eq!(check(bitarray[5],0,0,1,0,0,2,0,0), true, "Hard comparison does not work for {:b}", bitarray[5]);
    assert_eq!(check(bitarray[6],0,1,0,0,0,0,2,0), true, "Hard comparison does not work for {:b}", bitarray[6]);
    assert_eq!(check(bitarray[7],1,0,0,0,0,0,0,2), true, "Hard comparison does not work for {:b}", bitarray[7]);
    assert_eq!(check(bitarray[0],2,0,0,0,0,0,0,0), false, "Hard comparison does not work for {:b}", bitarray[0]);
    assert_eq!(check(bitarray[1],0,2,0,0,0,0,0,0), false, "Hard comparison does not work for {:b}", bitarray[1]);
    assert_eq!(check(bitarray[2],0,0,2,0,0,0,0,0), false, "Hard comparison does not work for {:b}", bitarray[2]);
    assert_eq!(check(bitarray[3],0,0,0,2,0,0,0,0), false, "Hard comparison does not work for {:b}", bitarray[3]);
    assert_eq!(check(bitarray[4],0,0,0,0,2,0,0,0), false, "Hard comparison does not work for {:b}", bitarray[4]);
    assert_eq!(check(bitarray[5],0,0,0,0,0,2,0,0), false, "Hard comparison does not work for {:b}", bitarray[5]);
    assert_eq!(check(bitarray[6],0,0,0,0,0,0,2,0), false, "Hard comparison does not work for {:b}", bitarray[6]);
    assert_eq!(check(bitarray[7],0,0,0,0,0,0,0,2), false, "Hard comparison does not work for {:b}", bitarray[7]);
  }

  #[test]
  fn nop() {
    let inst = Instruction::translate(0b0000_0000);
    assert_eq!(inst, Instruction::NOP, "Does not read NOP correctly");
  }

  #[test]
  fn load() {
    for item in 0x40..0x7f {
      match Instruction::translate(item) {
        Instruction::HALT => (),
        Instruction::Load(a, Dest::Reg(b)) => assert!(0x40 + (a << 3) + b == item, "Opcode {:02x} does not translate properly, instead {item:02x}", 0x40 + (a << 3) + b),
        _ => assert!(false, "Opcode {item:02x} does not translate to load")
      }
    }
  }

  #[test]
  fn load_save_a() {
    let arr = [0xe0, 0xe2, 0xea, 0xf0, 0xf2, 0xfa];

    for item in arr {
      match Instruction::translate(item) {
        Instruction::AXn(MemAction::Save) => assert!(item == 0xe0, "Opcode 0xe0 does not translate properly, instead {item:02x}"),
        Instruction::AXC(MemAction::Save) => assert!(item == 0xe2, "Opcode 0xe2 does not translate properly, instead {item:02x}"),
        Instruction::Ann(MemAction::Save) => assert!(item == 0xea, "Opcode 0xea does not translate properly, instead {item:02x}"),
        Instruction::AXn(MemAction::Load) => assert!(item == 0xf0, "Opcode 0xf0 does not translate properly, instead {item:02x}"),
        Instruction::AXC(MemAction::Load) => assert!(item == 0xf2, "Opcode 0xf2 does not translate properly, instead {item:02x}"),
        Instruction::Ann(MemAction::Load) => assert!(item == 0xfa, "Opcode 0xfa does not translate properly, instead {item:02x}"),
        _ => assert!(false, "Opcode {item:02x} does not translate to special load/save accumulator")
      }
    }
  }

  #[test]
  fn save_load_a_from_reg_pair() {
    let arr = [0x02, 0x0a, 0x12, 0x1a, 0x22, 0x2a, 0x32, 0x3a];

    for item in arr {
      match Instruction::translate(item) {
        Instruction::AfBC(MemAction::Save) => assert!(item == 0x02, "Opcode 0x02 does not translate properly, instead {item:02x}"),
        Instruction::AfBC(MemAction::Load) => assert!(item == 0x0a, "Opcode 0x0a does not translate properly, instead {item:02x}"),
        Instruction::AfDE(MemAction::Save) => assert!(item == 0x12, "Opcode 0x12 does not translate properly, instead {item:02x}"),
        Instruction::AfDE(MemAction::Load) => assert!(item == 0x1a, "Opcode 0x1a does not translate properly, instead {item:02x}"),
        Instruction::AfHLinc(MemAction::Save) => assert!(item == 0x22, "Opcode 0x22 does not translate properly, instead {item:02x}"),
        Instruction::AfHLinc(MemAction::Load) => assert!(item == 0x2a, "Opcode 0x2a does not translate properly, instead {item:02x}"),
        Instruction::AfHLdec(MemAction::Save) => assert!(item == 0x32, "Opcode 0x32 does not translate properly, instead {item:02x}"),
        Instruction::AfHLdec(MemAction::Load) => assert!(item == 0x3a, "Opcode 0x3a does not translate properly, instead {item:02x}"),

        _ => assert!(false, "Opcode {item:02x} does not translate to load/save register pair")
      }
    }
  }

  #[test]
  fn load_16_pair() {
    let arr = [0x01, 0x11, 0x21, 0x31];

    for item in arr {
      match Instruction::translate(item) {
        Instruction::Loadnn(Dest::RegPair(RegisterPairs::BC)) => assert!(item == 0x01, "Opcode 0x01 does not translate properly, instead {item:02x}"),
        Instruction::Loadnn(Dest::RegPair(RegisterPairs::DE)) => assert!(item == 0x11, "Opcode 0x11 does not translate properly, instead {item:02x}"),
        Instruction::Loadnn(Dest::RegPair(RegisterPairs::HL)) => assert!(item == 0x21, "Opcode 0x21 does not translate properly, instead {item:02x}"),
        Instruction::Loadnn(Dest::RegPair(RegisterPairs::AF)) => assert!(item == 0x31, "Opcode 0x31 does not translate properly, instead {item:02x}"),
        _ => assert!(false, "Opcode {item:02x} does not translate to 16 bit load to register pair")
      }
    }
  }

  #[test]
  fn reset() {
    let arr = [0xc7, 0xcf, 0xd7, 0xdf, 0xe7, 0xef, 0xf7, 0xff];

    for item in arr {
      match Instruction::translate(item) {
        Instruction::Restart(0) => assert!(item == 0xc7, "Opcode 0xc7 does not translate properly, instead {item:02x}"),
        Instruction::Restart(1) => assert!(item == 0xcf, "Opcode 0xcf does not translate properly, instead {item:02x}"),
        Instruction::Restart(2) => assert!(item == 0xd7, "Opcode 0xd7 does not translate properly, instead {item:02x}"),
        Instruction::Restart(3) => assert!(item == 0xdf, "Opcode 0xdf does not translate properly, instead {item:02x}"),
        Instruction::Restart(4) => assert!(item == 0xe7, "Opcode 0xe7 does not translate properly, instead {item:02x}"),
        Instruction::Restart(5) => assert!(item == 0xef, "Opcode 0xef does not translate properly, instead {item:02x}"),
        Instruction::Restart(6) => assert!(item == 0xf7, "Opcode 0xf7 does not translate properly, instead {item:02x}"),
        Instruction::Restart(7) => assert!(item == 0xff, "Opcode 0xff does not translate properly, instead {item:02x}"),
        _ => assert!(false, "Opcode {item:02x} does not translate to reset")
      }
    }
  }

  #[test]
  fn inc_dec() {
    let arr = [0x04, 0x0c, 0x14, 0x1c, 0x24, 0x2c, 0x34, 0x3c, 0x05, 0x0d, 0x15, 0x1d, 0x25, 0x2d, 0x35, 0x3d];

    for item in arr {
      match Instruction::translate(item) {
        Instruction::Inc(Dest::Reg(0)) => assert!(item == 0x04, "Opcode 0x04 does not translate properly, instead {item:02x}"),
        Instruction::Inc(Dest::Reg(1)) => assert!(item == 0x0c, "Opcode 0x0c does not translate properly, instead {item:02x}"),
        Instruction::Inc(Dest::Reg(2)) => assert!(item == 0x14, "Opcode 0x14 does not translate properly, instead {item:02x}"),
        Instruction::Inc(Dest::Reg(3)) => assert!(item == 0x1c, "Opcode 0x1c does not translate properly, instead {item:02x}"),
        Instruction::Inc(Dest::Reg(4)) => assert!(item == 0x24, "Opcode 0x24 does not translate properly, instead {item:02x}"),
        Instruction::Inc(Dest::Reg(5)) => assert!(item == 0x2c, "Opcode 0x2c does not translate properly, instead {item:02x}"),
        Instruction::Inc(Dest::Reg(6)) => assert!(item == 0x34, "Opcode 0x34 does not translate properly, instead {item:02x}"),
        Instruction::Inc(Dest::Reg(7)) => assert!(item == 0x3c, "Opcode 0x3c does not translate properly, instead {item:02x}"),
        Instruction::Dec(Dest::Reg(0)) => assert!(item == 0x05, "Opcode 0x05 does not translate properly, instead {item:02x}"),
        Instruction::Dec(Dest::Reg(1)) => assert!(item == 0x0d, "Opcode 0x0d does not translate properly, instead {item:02x}"),
        Instruction::Dec(Dest::Reg(2)) => assert!(item == 0x15, "Opcode 0x15 does not translate properly, instead {item:02x}"),
        Instruction::Dec(Dest::Reg(3)) => assert!(item == 0x1d, "Opcode 0x1d does not translate properly, instead {item:02x}"),
        Instruction::Dec(Dest::Reg(4)) => assert!(item == 0x25, "Opcode 0x25 does not translate properly, instead {item:02x}"),
        Instruction::Dec(Dest::Reg(5)) => assert!(item == 0x2d, "Opcode 0x2d does not translate properly, instead {item:02x}"),
        Instruction::Dec(Dest::Reg(6)) => assert!(item == 0x35, "Opcode 0x35 does not translate properly, instead {item:02x}"),
        Instruction::Dec(Dest::Reg(7)) => assert!(item == 0x3d, "Opcode 0x3d does not translate properly, instead {item:02x}"),

        _ => assert!(false, "Opcode {item:02x} does not translate to increase or decrease")
      }
    }
  }

  #[test]
  fn inc_dec_16() {
    let arr = [0x03, 0x0b, 0x13, 0x1b, 0x23, 0x2b, 0x33, 0x3b];

    for item in arr {
      match Instruction::translate(item) {
        Instruction::Inc16(Dest::RegPair(RegisterPairs::BC)) => assert!(item == 0x03, "Opcode 0x03 does not translate properly, instead {item:02x}"),
        Instruction::Inc16(Dest::RegPair(RegisterPairs::DE)) => assert!(item == 0x13, "Opcode 0x13 does not translate properly, instead {item:02x}"),
        Instruction::Inc16(Dest::RegPair(RegisterPairs::HL)) => assert!(item == 0x23, "Opcode 0x23 does not translate properly, instead {item:02x}"),
        Instruction::Inc16(Dest::RegPair(RegisterPairs::AF)) => assert!(item == 0x33, "Opcode 0x33 does not translate properly, instead {item:02x}"),
        Instruction::Dec16(Dest::RegPair(RegisterPairs::BC)) => assert!(item == 0x0b, "Opcode 0x0b does not translate properly, instead {item:02x}"),
        Instruction::Dec16(Dest::RegPair(RegisterPairs::DE)) => assert!(item == 0x1b, "Opcode 0x1b does not translate properly, instead {item:02x}"),
        Instruction::Dec16(Dest::RegPair(RegisterPairs::HL)) => assert!(item == 0x2b, "Opcode 0x2b does not translate properly, instead {item:02x}"),
        Instruction::Dec16(Dest::RegPair(RegisterPairs::AF)) => assert!(item == 0x3b, "Opcode 0x3b does not translate properly, instead {item:02x}"),
        _ => assert!(false, "Opcode {item:02x} does not translate to increase or decrease 16 bit")
      }
    }
  }

  #[test]
  fn add_reg_pair_to_hl() {
    let arr = [0x09, 0x19, 0x29, 0x39];

    for item in arr {
      match Instruction::translate(item) {
        Instruction::Add16(Dest::RegPair(RegisterPairs::BC)) => assert!(item == 0x09, "Opcode 0x09 does not translate properly, instead {item:02x}"),
        Instruction::Add16(Dest::RegPair(RegisterPairs::DE)) => assert!(item == 0x19, "Opcode 0x19 does not translate properly, instead {item:02x}"),
        Instruction::Add16(Dest::RegPair(RegisterPairs::HL)) => assert!(item == 0x29, "Opcode 0x29 does not translate properly, instead {item:02x}"),
        Instruction::Add16(Dest::RegPair(RegisterPairs::AF)) => assert!(item == 0x39 , "Opcode 0x39 does not translate properly, instead {item:02x}"),
        _ => assert!(false, "Opcode {item:02x} does not translate to push or pop")
      }
    }
  }

  #[test]
  fn push_pop() {
    let arr = [0xc1, 0xc5, 0xd1, 0xd5, 0xe1, 0xe5, 0xf1, 0xf5];
    for item in arr {
      match Instruction::translate(item) {
        Instruction::PopStack(Dest::RegPair(RegisterPairs::BC)) => assert!(item == 0xc1, "Opcode 0xc1 does not translate properly, instead {item:02x}"),
        Instruction::PopStack(Dest::RegPair(RegisterPairs::DE)) => assert!(item == 0xd1, "Opcode 0xc5 does not translate properly, instead {item:02x}"),
        Instruction::PopStack(Dest::RegPair(RegisterPairs::HL)) => assert!(item == 0xe1, "Opcode 0xd1 does not translate properly, instead {item:02x}"),
        Instruction::PopStack(Dest::RegPair(RegisterPairs::AF)) => assert!(item == 0xf1, "Opcode 0xd5 does not translate properly, instead {item:02x}"),
        Instruction::PushStack(Dest::RegPair(RegisterPairs::BC)) => assert!(item == 0xc5, "Opcode 0xe1 does not translate properly, instead {item:02x}"),
        Instruction::PushStack(Dest::RegPair(RegisterPairs::DE)) => assert!(item == 0xd5, "Opcode 0xe5 does not translate properly, instead {item:02x}"),
        Instruction::PushStack(Dest::RegPair(RegisterPairs::HL)) => assert!(item == 0xe5, "Opcode 0xf1 does not translate properly, instead {item:02x}"),
        Instruction::PushStack(Dest::RegPair(RegisterPairs::AF)) => assert!(item == 0xf5 , "Opcode 0xf5 does not translate properly, instead {item:02x}"),
        _ => assert!(false, "Opcode {item:02x} does not translate to push or pop")
      }
    }
  }

  #[test]
  fn save_sp_to_nn() {
    match Instruction::translate(0x08) {
      Instruction::SaveSPnn => (),
        _ => assert!(false, "Opcode 0x08 does not translate to save stack pointer to immediate location")
    }
  }

  #[test]
  fn add_immediate_sp() {
    match Instruction::translate(0xe8) {
      Instruction::SPAddn => (),
      x => assert!(false, "Opcode 0xf8 does not translate to add immediate_byte_to_stack_pointer, {:?}", x)
    }
  }

  #[test]
  fn load_adjusted_sp_to_hl() {
    match Instruction::translate(0xf8) {
      Instruction::LoadStackAdj => (),
      x => assert!(false, "Opcode 0xf8 does not translate to add load stack adjacent memory, {:?}", x)
    }
  }

  #[test]
  fn load_hl_to_sp() {
    match Instruction::translate(0xf9) {
      Instruction::LoadSPHL => (),
      x => assert!(false, "Opcode 0xf8 does not translate to set stack pointer to hl, {:?}", x)
    }
  }



  #[test]
  fn xor() {
    let arr = [0xa8u8, 0xa9, 0xaa, 0xab, 0xac, 0xad, 0xae, 0xaf, 0xee];
    for item in arr {
      match Instruction::translate(item) {
        Instruction::XOR(Dest::Reg(0)) => assert!(item == 0xa8, "Opcode 0xa8 does not translate properly, instead {item:02x}"),
        Instruction::XOR(Dest::Reg(1)) => assert!(item == 0xa9, "Opcode 0xa9 does not translate properly, instead {item:02x}"),
        Instruction::XOR(Dest::Reg(2)) => assert!(item == 0xaa, "Opcode 0xaa does not translate properly, instead {item:02x}"),
        Instruction::XOR(Dest::Reg(3)) => assert!(item == 0xab, "Opcode 0xab does not translate properly, instead {item:02x}"),
        Instruction::XOR(Dest::Reg(4)) => assert!(item == 0xac, "Opcode 0xac does not translate properly, instead {item:02x}"),
        Instruction::XOR(Dest::Reg(5)) => assert!(item == 0xad, "Opcode 0xad does not translate properly, instead {item:02x}"),
        Instruction::XOR(Dest::Reg(6)) => assert!(item == 0xae, "Opcode 0xae does not translate properly, instead {item:02x}"),
        Instruction::XOR(Dest::Reg(7)) => assert!(item == 0xaf, "Opcode 0xaf does not translate properly, instead {item:02x}"),
        Instruction::XOR(Dest::Immediate) => assert!(item == 0xee, "Opcode 0xee does not translate properly, instead {item:02x}"),
        _ => assert!(false, "Opcode {item:02x} does not translate to xor")
      }
    }
  }

  #[test]
  fn and() {
    let arr = [0xa0u8, 0xa1, 0xa2, 0xa3, 0xa4, 0xa5, 0xa6, 0xa7, 0xe6];
    for item in arr {
      match Instruction::translate(item) {
        Instruction::AND(Dest::Reg(0)) => assert!(item == 0xa0, "Opcode 0xa0 does not translate properly, instead {item:02x}"),
        Instruction::AND(Dest::Reg(1)) => assert!(item == 0xa1, "Opcode 0xa1 does not translate properly, instead {item:02x}"),
        Instruction::AND(Dest::Reg(2)) => assert!(item == 0xa2, "Opcode 0xa2 does not translate properly, instead {item:02x}"),
        Instruction::AND(Dest::Reg(3)) => assert!(item == 0xa3, "Opcode 0xa3 does not translate properly, instead {item:02x}"),
        Instruction::AND(Dest::Reg(4)) => assert!(item == 0xa4, "Opcode 0xa4 does not translate properly, instead {item:02x}"),
        Instruction::AND(Dest::Reg(5)) => assert!(item == 0xa5, "Opcode 0xa5 does not translate properly, instead {item:02x}"),
        Instruction::AND(Dest::Reg(6)) => assert!(item == 0xa6, "Opcode 0xa6 does not translate properly, instead {item:02x}"),
        Instruction::AND(Dest::Reg(7)) => assert!(item == 0xa7, "Opcode 0xa7 does not translate properly, instead {item:02x}"),
        Instruction::AND(Dest::Immediate) => assert!(item == 0xe6, "Opcode 0xe6 does not translate properly, instead {item:02x}"),
        _ => assert!(false, "Opcode {} does not translate to and", item)
      }
    }
  }

  #[test]
  fn or() {
    let arr = [0xb0u8, 0xb1, 0xb2, 0xb3, 0xb4, 0xb5, 0xb6, 0xb7, 0xf6];
    for item in arr {
      match Instruction::translate(item) {
        Instruction::OR(Dest::Reg(0)) => assert!(item == 0xb0, "Opcode 0xb0 does not translate properly, instead {item:02x}"),
        Instruction::OR(Dest::Reg(1)) => assert!(item == 0xb1, "Opcode 0xb1 does not translate properly, instead {item:02x}"),
        Instruction::OR(Dest::Reg(2)) => assert!(item == 0xb2, "Opcode 0xb2 does not translate properly, instead {item:02x}"),
        Instruction::OR(Dest::Reg(3)) => assert!(item == 0xb3, "Opcode 0xb3 does not translate properly, instead {item:02x}"),
        Instruction::OR(Dest::Reg(4)) => assert!(item == 0xb4, "Opcode 0xb4 does not translate properly, instead {item:02x}"),
        Instruction::OR(Dest::Reg(5)) => assert!(item == 0xb5, "Opcode 0xb5 does not translate properly, instead {item:02x}"),
        Instruction::OR(Dest::Reg(6)) => assert!(item == 0xb6, "Opcode 0xb6 does not translate properly, instead {item:02x}"),
        Instruction::OR(Dest::Reg(7)) => assert!(item == 0xb7, "Opcode 0xb7 does not translate properly, instead {item:02x}"),
        Instruction::OR(Dest::Immediate) => assert!(item == 0xf6, "Opcode 0xf6 does not translate properly, instead {item:02x}"),
        _ => assert!(false, "Opcode {item:02x} does not translate to or")
      }
    }
  }

  #[test]
  fn add() {
    let arr = [0x80u8, 0x81, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87, 0xc6];
    for item in arr {
      match Instruction::translate(item) {
        Instruction::Add(Dest::Reg(0)) => assert!(item == 0x80, "Opcode 0x80 does not translate properly, instead {item:02x}"),
        Instruction::Add(Dest::Reg(1)) => assert!(item == 0x81, "Opcode 0x81 does not translate properly, instead {item:02x}"),
        Instruction::Add(Dest::Reg(2)) => assert!(item == 0x82, "Opcode 0x82 does not translate properly, instead {item:02x}"),
        Instruction::Add(Dest::Reg(3)) => assert!(item == 0x83, "Opcode 0x83 does not translate properly, instead {item:02x}"),
        Instruction::Add(Dest::Reg(4)) => assert!(item == 0x84, "Opcode 0x84 does not translate properly, instead {item:02x}"),
        Instruction::Add(Dest::Reg(5)) => assert!(item == 0x85, "Opcode 0x85 does not translate properly, instead {item:02x}"),
        Instruction::Add(Dest::Reg(6)) => assert!(item == 0x86, "Opcode 0x86 does not translate properly, instead {item:02x}"),
        Instruction::Add(Dest::Reg(7)) => assert!(item == 0x87, "Opcode 0x87 does not translate properly, instead {item:02x}"),
        Instruction::Add(Dest::Immediate) => assert!(item == 0xc6, "Opcode 0xc6 does not translate properly, instead {item:02x}"),
        _ => assert!(false, "Opcode {} does not translate to add", item)
      }
    }
  }

  #[test]
  fn sub() {
    let arr = [0x90u8, 0x91, 0x92, 0x93, 0x94, 0x95, 0x96, 0x97, 0xd6];
    for item in arr {
      match Instruction::translate(item) {
        Instruction::Sub(Dest::Reg(0)) => assert!(item == 0x90, "Opcode 0x90 does not translate properly, instead {item:02x}"),
        Instruction::Sub(Dest::Reg(1)) => assert!(item == 0x91, "Opcode 0x91 does not translate properly, instead {item:02x}"),
        Instruction::Sub(Dest::Reg(2)) => assert!(item == 0x92, "Opcode 0x92 does not translate properly, instead {item:02x}"),
        Instruction::Sub(Dest::Reg(3)) => assert!(item == 0x93, "Opcode 0x93 does not translate properly, instead {item:02x}"),
        Instruction::Sub(Dest::Reg(4)) => assert!(item == 0x94, "Opcode 0x94 does not translate properly, instead {item:02x}"),
        Instruction::Sub(Dest::Reg(5)) => assert!(item == 0x95, "Opcode 0x95 does not translate properly, instead {item:02x}"),
        Instruction::Sub(Dest::Reg(6)) => assert!(item == 0x96, "Opcode 0x96 does not translate properly, instead {item:02x}"),
        Instruction::Sub(Dest::Reg(7)) => assert!(item == 0x97, "Opcode 0x97 does not translate properly, instead {item:02x}"),
        Instruction::Sub(Dest::Immediate) => assert!(item == 0xd6, "Opcode 0xd6 does not translate properly, instead {item:02x}"),
        _ => assert!(false, "Opcode {} does not translate to sub", item)
      }
    }
  }

  #[test]
  fn addcarry() {
    let arr = [0x88u8, 0x89, 0x8a, 0x8b, 0x8c, 0x8d, 0x8e, 0x8f, 0xce];
    for item in arr {
      match Instruction::translate(item) {
        Instruction::AddCarry(Dest::Reg(0)) => assert!(item == 0x88, "Opcode 0x88 does not translate properly, instead {item:02x}"),
        Instruction::AddCarry(Dest::Reg(1)) => assert!(item == 0x89, "Opcode 0x89 does not translate properly, instead {item:02x}"),
        Instruction::AddCarry(Dest::Reg(2)) => assert!(item == 0x8a, "Opcode 0x8a does not translate properly, instead {item:02x}"),
        Instruction::AddCarry(Dest::Reg(3)) => assert!(item == 0x8b, "Opcode 0x8b does not translate properly, instead {item:02x}"),
        Instruction::AddCarry(Dest::Reg(4)) => assert!(item == 0x8c, "Opcode 0x8c does not translate properly, instead {item:02x}"),
        Instruction::AddCarry(Dest::Reg(5)) => assert!(item == 0x8d, "Opcode 0x8d does not translate properly, instead {item:02x}"),
        Instruction::AddCarry(Dest::Reg(6)) => assert!(item == 0x8e, "Opcode 0x8e does not translate properly, instead {item:02x}"),
        Instruction::AddCarry(Dest::Reg(7)) => assert!(item == 0x8f, "Opcode 0x8f does not translate properly, instead {item:02x}"),
        Instruction::AddCarry(Dest::Immediate) => assert!(item == 0xce, "Opcode 0xce does not translate properly, instead {item:02x}"),
        _ => assert!(false, "Opcode {} does not translate to add with carry", item)
      }
    }
  }

  #[test]
  fn subcarry() {
    let arr = [0x98u8, 0x99, 0x9a, 0x9b, 0x9c, 0x9d, 0x9e, 0x9f, 0xde];
    for item in arr {
      match Instruction::translate(item) {
        Instruction::SubCarry(Dest::Reg(0)) => assert!(item == 0x98, "Opcode 0x98 does not translate properly, instead {item:02x}"),
        Instruction::SubCarry(Dest::Reg(1)) => assert!(item == 0x99, "Opcode 0x99 does not translate properly, instead {item:02x}"),
        Instruction::SubCarry(Dest::Reg(2)) => assert!(item == 0x9a, "Opcode 0x9a does not translate properly, instead {item:02x}"),
        Instruction::SubCarry(Dest::Reg(3)) => assert!(item == 0x9b, "Opcode 0x9b does not translate properly, instead {item:02x}"),
        Instruction::SubCarry(Dest::Reg(4)) => assert!(item == 0x9c, "Opcode 0x9c does not translate properly, instead {item:02x}"),
        Instruction::SubCarry(Dest::Reg(5)) => assert!(item == 0x9d, "Opcode 0x9d does not translate properly, instead {item:02x}"),
        Instruction::SubCarry(Dest::Reg(6)) => assert!(item == 0x9e, "Opcode 0x9e does not translate properly, instead {item:02x}"),
        Instruction::SubCarry(Dest::Reg(7)) => assert!(item == 0x9f, "Opcode 0x9f does not translate properly, instead {item:02x}"),
        Instruction::SubCarry(Dest::Immediate) => assert!(item == 0xde, "Opcode 0xde does not translate properly, instead {item:02x}"),
        _ => assert!(false, "Opcode {} does not translate to sub with carry", item)
      }
    }
  }

  #[test]
  fn cp() {
    let arr = [0xb8u8, 0xb9, 0xba, 0xbb, 0xbc, 0xbd, 0xbe, 0xbf, 0xfe];
    for item in arr {
      match Instruction::translate(item) {
        Instruction::Compare(Dest::Reg(0)) => assert!(item == 0xb8, "Opcode 0xb8 does not translate properly, instead {item:02x}"),
        Instruction::Compare(Dest::Reg(1)) => assert!(item == 0xb9, "Opcode 0xb9 does not translate properly, instead {item:02x}"),
        Instruction::Compare(Dest::Reg(2)) => assert!(item == 0xba, "Opcode 0xba does not translate properly, instead {item:02x}"),
        Instruction::Compare(Dest::Reg(3)) => assert!(item == 0xbb, "Opcode 0xbb does not translate properly, instead {item:02x}"),
        Instruction::Compare(Dest::Reg(4)) => assert!(item == 0xbc, "Opcode 0xbc does not translate properly, instead {item:02x}"),
        Instruction::Compare(Dest::Reg(5)) => assert!(item == 0xbd, "Opcode 0xbd does not translate properly, instead {item:02x}"),
        Instruction::Compare(Dest::Reg(6)) => assert!(item == 0xbe, "Opcode 0xbe does not translate properly, instead {item:02x}"),
        Instruction::Compare(Dest::Reg(7)) => assert!(item == 0xbf, "Opcode 0xbf does not translate properly, instead {item:02x}"),
        Instruction::Compare(Dest::Immediate) => assert!(item == 0xfe, "Opcode 0xfe does not translate properly, instead {item:02x}"),
        _ => assert!(false, "Opcode {} does not translate to compare", item)
      }
    }
  }


  #[test]
  fn acc_manipulation() {
    let arr = [0x27, 0x2f, 0x37, 0x3f];

    for item in arr {
      match Instruction::translate(item) {
        Instruction::DAA => assert!(item == 0x27, "Opcode 0x27 does not get translated properly, instead {item:02x}"),
        Instruction::CmpA => assert!(item == 0x2f, "Opcode 0x2f does not get translated properly, instead {item:02x}"),
        Instruction::SCFlag => assert!(item == 0x37, "Opcode 0x37 does not get translated properly, instead {item:02x}"),
        Instruction::CCFlag => assert!(item == 0x3f, "Opcode 0xdf does not get translated properly, instead {item:02x}"),
        _ => assert!(false, "Opcode {:02x} does not translate into an accumulator manipulation operation", item)
      }
    }
  }

  #[test]
  fn base_rotate() {
    let arr = [0x07, 0x0f, 0x17, 0x1f];

    for item in arr {
      match Instruction::translate(item) {
        Instruction::RotateLC(Dest::Reg(7)) => assert!(item == 0x07, "Opcode 0x07 does not get translated properly, instead {item:02x}"),
        Instruction::RotateRC(Dest::Reg(7)) => assert!(item == 0x0f, "Opcode 0x0f does not get translated properly, instead {item:02x}"),
        Instruction::RotateL(Dest::Reg(7)) => assert!(item == 0x17, "Opcode 0x17 does not get translated properly, instead {item:02x}"),
        Instruction::RotateR(Dest::Reg(7)) => assert!(item == 0x1f, "Opcode 0x1f does not get translated properly, instead {item:02x}"),
        _ => assert!(false, "Opcode {:02x} does not translate into rotate", item)

      }
    }
  }



  #[test]
  fn cb_rotate() {
    for i in 0x00..0x1f {
      match Instruction::translatecb(i) {
        Instruction::RotateLC(Dest::Reg(x)) => assert!(i == x, "Opcode {i:02x} does not get translated properly, instead {x:02x}"),
        Instruction::RotateRC(Dest::Reg(x)) => assert!(i == 0x08+x, "Opcode {i:02x} does not get translated properly, instead {:02x}", 0x08+x),
        Instruction::RotateL(Dest::Reg(x)) => assert!(i == 0x10+x, "Opcode {i:02x} does not get translated properly, instead {:02x}", 0x10+x),
        Instruction::RotateR(Dest::Reg(x)) => assert!(i == 0x18+x, "Opcode {i:02x} does not get translated properly, instead {:02x}", 0x18+x),
        _ => assert!(false, "Opcode {:02x} does not translate into cb rotate", i)
      }
    }
  }

  #[test]
  fn cb_shift_swap() {
    for i in 0x20..0x3f {
      match Instruction::translatecb(i) {
        Instruction::ShiftL(Dest::Reg(x)) => assert!(i == 0x20+x, "Opcode {i:02x} does not get translated properly, instead {:02x}", 0x20+x),
        Instruction::ShiftRArith(Dest::Reg(x)) => assert!(i == 0x28+x, "Opcode {i:02x} does not get translated properly, instead {:02x}", 0x28+x),
        Instruction::SwapNibble(Dest::Reg(x)) => assert!(i == 0x30+x, "Opcode {i:02x} does not get translated properly, instead {:02x}", 0x30+x),
        Instruction::ShiftRLog(Dest::Reg(x)) => assert!(i == 0x38+x, "Opcode {i:02x} does not get translated properly, instead {:02x}", 0x38+x),
        _ => assert!(false, "Opcode {:02x} does not translate into cb shift/swap", i)
      }
    }
  }

  #[test]
  fn cb_test_bit() {
    for i in 0x40..0x7f {
      match Instruction::translatecb(i) {
        Instruction::TestBit(x, Dest::Reg(y)) => assert!(i == 0x40+(x<<3)+y, "Opcode {i:02x} does not get translated properly, instead {:02x}", 0x40+(x<<3)+y),
        _ => assert!(false, "Opcode {:02x} does not translate into cb test bit", i)
      }
    }
  }

  #[test]
  fn cb_reset_bit() {
    for i in 0x80..0xbf {
      match Instruction::translatecb(i) {
        Instruction::ResetBit(x, Dest::Reg(y)) => assert!(i == 0x80+(x<<3)+y, "Opcode {i:02x} does not get translated properly, instead {:02x}", 0x80+(x<<3)+y),
        _ => assert!(false, "Opcode {:02x} does not translate into cb reset bit", i)
      }
    }
  }

  #[test]
  fn cb_set_bit() {
    for i in 0xc0..0xff {
      match Instruction::translatecb(i) {
        Instruction::SetBit(x, Dest::Reg(y)) => assert!(i == 0xc0+(x<<3)+y, "Opcode {i:02x} does not get translated properly, instead {:02x}", 0xc0+(x<<3)+y),
        _ => assert!(false, "Opcode {:02x} does not translate into cb set bit", i)
      }
    }
  }


  #[test]
  fn jump() {
    let arr = [0xc2, 0xca, 0xd2, 0xda, 0xc3, 0xe9];

    for item in arr {
      match Instruction::translate(item) {
        Instruction::Jump(Dest::Immediate) => assert!(item == 0xc3, "Opcode 0xc3 does not get translated properly, instead {item:02x}"),
        Instruction::Jump(Dest::HL) => assert!(item == 0xe9, "Opcode 0xe9 does not get translated properly, instead {item:02x}"),
        Instruction::JumpCond(Cond::NZ) => assert!(item == 0xc2, "Opcode 0xc2 does not get translated properly, instead {item:02x}"),
        Instruction::JumpCond(Cond::Z) => assert!(item == 0xca, "Opcode 0xca does not get translated properly, instead {item:02x}"),
        Instruction::JumpCond(Cond::NC) => assert!(item == 0xd2, "Opcode 0xd2 does not get translated properly, instead {item:02x}"),
        Instruction::JumpCond(Cond::C) => assert!(item == 0xda, "Opcode 0xda does not get translated properly, instead {item:02x}"),
        _ => assert!(false, "Opcode {:02x} does not translate into jump", item)
      }
    }

  }
  
  #[test]
  fn jump_rel() {
    let arr = [0x18, 0x20, 0x28, 0x30, 0x38];

    for item in arr {
      match Instruction::translate(item) {
        Instruction::JumpRel => assert!(item == 0x18, "Opcode 0x18 does not get translated properly, instead {item:02x}"),
        Instruction::JumpRelCond(Cond::NZ) => assert!(item == 0x20, "Opcode 0x20 does not get translated properly, instead {item:02x}"),
        Instruction::JumpRelCond(Cond::Z) => assert!(item == 0x28, "Opcode 0x28 does not get translated properly, instead {item:02x}"),
        Instruction::JumpRelCond(Cond::NC) => assert!(item == 0x30, "Opcode 0x30 does not get translated properly, instead {item:02x}"),
        Instruction::JumpRelCond(Cond::C) => assert!(item == 0x38, "Opcode 0x38 does not get translated properly, instead {item:02x}"),
        _ => assert!(false, "Opcode {:02x} does not translate into relative jump", item)

      }
    }
  }

  #[test]
  fn ret() {
    let arr = [0xc0, 0xc8, 0xd0, 0xd8, 0xc9, 0xd9];

    for item in arr {
      match Instruction::translate(item) {
        Instruction::Ret => assert!(item == 0xc9, "Opcode 0xc9 does not get translated properly, instead {item:02x}"),
        Instruction::RetI => assert!(item == 0xd9, "Opcode 0xd9 does not get translated properly, instead {item:02x}"),
        Instruction::CondRet(Cond::NZ) => assert!(item == 0xc0, "Opcode 0xc0 does not get translated properly, instead {item:02x}"),
        Instruction::CondRet(Cond::Z) => assert!(item == 0xc8, "Opcode 0xc8 does not get translated properly, instead {item:02x}"),
        Instruction::CondRet(Cond::NC) => assert!(item == 0xd0, "Opcode 0xd0 does not get translated properly, instead {item:02x}"),
        Instruction::CondRet(Cond::C) => assert!(item == 0xd8, "Opcode 0xd8 does not get translated properly, instead {item:02x}"),
        _ => assert!(false, "Opcode {:02x} does not translate into return", item)
      }
    }
  }

  #[test]
  fn call() {
    let arr = [0xc4, 0xcc, 0xd4, 0xdc, 0xcd];

    for item in arr {
      match Instruction::translate(item) {
        Instruction::Call => assert!(item == 0xcd, "Opcode 0xcd does not get translated properly, instead {item:02x}"),
        Instruction::CondCall(Cond::NZ) => assert!(item == 0xc4, "Opcode 0xc4 does not get translated properly, instead {item:02x}"),
        Instruction::CondCall(Cond::Z) => assert!(item == 0xcc, "Opcode 0xcc does not get translated properly, instead {item:02x}"),
        Instruction::CondCall(Cond::NC) => assert!(item == 0xd4, "Opcode 0xd4 does not get translated properly, instead {item:02x}"),
        Instruction::CondCall(Cond::C) => assert!(item == 0xdc, "Opcode 0xdc does not get translated properly, instead {item:02x}"),
        _ => assert!(false, "Opcode {:02x} does not translate into return", item)
      }
    }
  }

  #[test]
  fn cb_alternate() {
    match Instruction::translate(0xcb) {
      Instruction::Prefix => assert!(true),
      _ => assert!(false, "Opcode 0xcb does not translate to cb prefix")
    }
  }

  #[test]
  fn pauses() {
    match Instruction::translate(0x76) {
      Instruction::HALT => (),
      inst => assert!(false, "Halt does not map to 0x76, maps to {:?}", inst)
    }

    match Instruction::translate(0x10) {
      Instruction::STOP => (),
      inst => assert!(false, "Stop does not map to 0x10, maps to {:?}", inst)
    }
  }

  #[test]
  fn interrupts() {
    let ei = Instruction::translate(0xfb);
    let di = Instruction::translate(0xf3);
    assert_eq!(ei, Instruction::EI, "Does not read EI correctly");
    assert_eq!(di, Instruction::DI, "Does not read DI correctly");

  }

}