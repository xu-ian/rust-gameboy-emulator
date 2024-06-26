
use super::registers::Register;

#[derive(Debug, PartialEq)]
pub enum RegisterPairs {
  BC,
  DE,
  HL
}

#[derive(Debug, PartialEq)]
pub enum MemAction {
  Save,
  Load
}

#[derive(Debug, PartialEq)]
pub enum Dest {
  Reg(Register),
  RegPair(RegisterPairs),
  HL,
  Immediate,
}
#[derive(Debug, PartialEq)]
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
  CondCall(bool, Dest),
  //Return from function load and roll back stack to program counter
  Ret,
  //Conditional return from function load. See above
  CondRet(bool),
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
        bits(instruction, 3, 0b0000_0111),
        Dest::Reg(bits(instruction, 0, 0b0000_0111))
      )
    } else if check(instruction, 0,0,2,2,2,1,1,0) {
      Instruction::Load(
        bits(instruction, 3, 0b0000_0111),
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
      let pair = get_register_pair(bits(instruction, 4, 0b0000_0011));

      Instruction::Loadnn(
        Dest::RegPair(pair)
      )
    } else if check(instruction, 0,0,0,0,1,0,0,0) {
      Instruction::SaveSPnn
    } else if check(instruction, 1,1,1,1,1,0,0,1) {
      Instruction::LoadSPHL
    } else if check(instruction, 1,1,2,2,0,1,0,1) {
      let pair = get_register_pair(bits(instruction, 4, 0b0000_0011));
      
      Instruction::PushStack(
        Dest::RegPair(pair)
      )
    } else if check(instruction, 1,1,2,2,0,0,0,1) {
      let pair = get_register_pair(bits(instruction, 4, 0b0000_0011));

      Instruction::PopStack(
        Dest::RegPair(pair)
      )
    } else if check(instruction, 1,1,1,1,1,0,0,0) {
      Instruction::LoadStackAdj
    } else if check(instruction, 1,0,0,0,0,2,2,2) {
      Instruction::Add(
        Dest::Reg(bits(instruction, 0, 0b0000_0111))
      )
    } else if check(instruction, 1,1,0,0,0,1,1,0) {
      Instruction::Add(
        Dest::Immediate
      )
    } else if check(instruction, 1,0,0,0,1,2,2,2) {
      Instruction::AddCarry(
        Dest::Reg(bits(instruction, 0, 0b0000_0111))
      )
    } else if check(instruction, 1,1,0,0,1,1,1,0) {
      Instruction::AddCarry(
        Dest::Immediate
      )
    } else if check(instruction, 1,0,0,1,0,2,2,2) {
      Instruction::Sub(
        Dest::Reg(bits(instruction, 0, 0b0000_0111))
      )
    } else if check(instruction, 1,1,0,1,0,1,1,0) {
      Instruction::Sub(
        Dest::Immediate
      )        
    } else if check(instruction, 1,0,0,1,1,2,2,2) {
      Instruction::SubCarry(
        Dest::Reg(bits(instruction, 0, 0b0000_0111))
      )
    } else if check(instruction, 1,1,0,1,1,1,1,0) {
      Instruction::SubCarry(
        Dest::Immediate
      )        
    } else if check(instruction, 1,0,1,1,1,2,2,2) {
      Instruction::Compare(
        Dest::Reg(bits(instruction, 0, 0b0000_0111))
      )
    } else if check(instruction, 1,1,1,1,1,1,1,0) {
      Instruction::Compare(
        Dest::Immediate
      )
    } else if check(instruction, 0,0,2,2,2,1,0,0) {
      Instruction::Inc(
        Dest::Reg(bits(instruction, 3, 0b0000_0111))
      )
    } else if check(instruction, 0,0,2,2,2,1,0,1) {
      Instruction::Dec(
        Dest::Reg(bits(instruction, 3, 0b0000_0111))
      )
    } else if check(instruction, 1,0,1,0,0,2,2,2) {
      Instruction::AND(
        Dest::Reg(bits(instruction,0,0b0000_0111))
      )
    } else if check(instruction, 1,1,1,0,0,1,1,0) {
      Instruction::AND(
        Dest::Immediate
      )        
    } else if check(instruction, 1,0,1,1,0,2,2,2) {
      Instruction::OR(
        Dest::Reg(bits(instruction, 0, 0b0000_0111))
      )
    } else if check(instruction, 1,1,1,1,0,1,1,0) {
      Instruction::OR(
        Dest::Immediate
      )        
    } else if check(instruction, 1,0,1,0,1,2,2,2) {
      Instruction::XOR(
        Dest::Reg(bits(instruction,0,0b0000_0111))
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
      let pair = get_register_pair(bits(instruction, 4, 0b0000_0011));
      Instruction::Inc16(
        Dest::RegPair(pair)
      )
    } else if check(instruction, 0,0,2,2,1,0,1,1) {
      let pair = get_register_pair(bits(instruction, 4, 0b0000_0011));
      Instruction::Dec16(
        Dest::RegPair(pair)
      )
    } else if check(instruction, 0,0,2,2,1,0,0,1) {
      let pair = get_register_pair(bits(instruction, 4, 0b0000_0011));
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
        bits(instruction, 3, 2) == 0,
        Dest::Immediate
      )
    } else if check(instruction, 1,1,0,0,1,0,0,1) {
      Instruction::Ret
    } else if check(instruction, 1,1,0,2,2,0,0,0) {
      Instruction::CondRet(
        bits(instruction, 3, 2) == 0
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
    } else if check(instruction, 1,1,1,1,1,0,1,1) {
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
  fn interrupts() {
    let ei = Instruction::translate(0xfb);
    let di = Instruction::translate(0xf3);
    assert_eq!(ei, Instruction::EI, "Does not read EI correctly");
    assert_eq!(di, Instruction::DI, "Does not read DI correctly");

  }

}