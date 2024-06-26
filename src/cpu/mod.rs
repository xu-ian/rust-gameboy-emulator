pub mod instruction;
pub mod memory;
pub mod registers;

use instruction::Dest;
use instruction::Instruction;
use instruction::MemAction;
use instruction::RegisterPairs;
use memory::Memory;
use registers::FlagActions;
use registers::Register;
use registers::Registers;
use std::num::Wrapping;
use std::sync::Arc;
use std::sync::Mutex;
use std::thread;

use crossterm::event::{read, Event, KeyCode, KeyEvent, KeyModifiers};

fn check_half_carry_add(byte1: u8, byte2: u8, carry: u8) -> bool {
    ((byte1 & 0x0F) + (byte2 & 0x0F) + carry) & 0x10 == 0x10
}

fn check_half_carry_add_16(byte1: u16, byte2: u16) -> bool {
    (((byte1 & 0x0FFF) + (byte2 & 0x0FFF)) & 0x1000) == 0x1000
}

fn check_half_carry_sub(byte1: u8, byte2: u8, carry: u8) -> bool {
    match (byte1 & 0x0F).checked_sub(byte2 & 0x0F) {
        Some(_) => false,
        None => match ((byte1 & 0x0F) + carry).checked_sub(byte2 & 0x0F) {
            Some(_) => false,
            None => true,
        },
    }
}

fn check_half_carry_sub_16(byte1: u16, byte2: u16) -> bool {
    match (byte1 & 0x0FFF).checked_sub(byte2 & 0x0FFF) {
        Some(_) => false,
        None => true,
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

pub struct RunningState {
    registers: Registers,
    memory: Memory,
    interrupts: bool,
}

impl RunningState {

    pub fn new() -> RunningState {
        RunningState {
            registers: Registers::new(),
            memory: Memory::new(),
            interrupts: false    
        }
    }

    pub fn set_memory(&mut self, mem: Arc<Mutex<Box<[u8; 0x10000]>>>) {
        self.memory.data = mem;
    }

    pub fn get_memory_copy(&self) -> Arc<Mutex<Box<[u8; 0x10000]>>> {
        Arc::clone(&self.memory.data)
    }

    fn read_register(&mut self, byte: u8) -> u8 {
        self.registers.read_register(byte)
    }

    fn write_register(&mut self, byte: u8, value: u8) {
        self.registers.write_register(byte, value)
    }

    fn read_memory(&mut self, position: u16) -> u8 {
        self.memory.read_memory(position)
    }

    fn read_interrupt_enable(&mut self) -> u8 {
        self.read_memory(0xFFFF)
    }

    fn read_interrupt_flags(&mut self) -> u8 {
        self.read_memory(0xFF0F)
    }

    fn read_memory_from_pc(&mut self) -> u8 {
        let data = self.memory.read_memory_from_pc(&mut self.registers);
        //println!("{:04x}", self.registers.pc);
        //println!("{:#04x}", data);
        data
    }

    fn write_memory(&mut self, position: usize, data: u8) {
        self.memory.write_memory(position, data)
    }

    pub fn perform_action(&mut self, instruction: Instruction) {
        //println!("{:?}", instruction);
        match instruction {
            //Saves Immediate into location specified by HL
            Instruction::Load(6, Dest::Immediate) => self.load_immediate_to_hl(),
            //Loads Immediate into register
            Instruction::Load(register, Dest::Immediate) => {
                self.load_immediate_to_register(register)
            }
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
            //Sets the accumulator to decimal adjust mode
            Instruction::DAA => self.decimal_adjust_accumulator(),
            //Flips bits in accumulator then sets sub and half carry flags
            Instruction::CmpA => self.complement_accumulator(),
            //Increments a 16 bit register pair
            Instruction::Inc16(Dest::RegPair(pair)) => self.increment_register_pair(pair),
            //Decrements a 16 bit register pair
            Instruction::Dec16(Dest::RegPair(pair)) => self.decrement_register_pair(pair),
            //Adds a 16 bit register pair to hl
            Instruction::Add16(Dest::RegPair(pair)) => self.add_register_pair_to_hl(pair),
            //Increases the stack pointer by an 8 bit immediate
            Instruction::SPAddn => self.increase_stack_pointer(),
            //Rotates hl data left circular
            Instruction::RotateLC(Dest::Reg(6)) => self.rotate_hl_data_left_circular(),
            //Rotates Register left circular
            Instruction::RotateLC(Dest::Reg(register)) => {
                self.rotate_register_left_circular(register)
            }
            //Rotates hl data right circular
            Instruction::RotateRC(Dest::Reg(6)) => self.rotate_hl_data_right_circular(),
            //Rotates Register right circular
            Instruction::RotateRC(Dest::Reg(register)) => {
                self.rotate_register_right_circular(register)
            }
            //Rotates hl data left
            Instruction::RotateL(Dest::Reg(6)) => self.rotate_hl_data_left(),
            //Rotates Register left
            Instruction::RotateL(Dest::Reg(register)) => self.rotate_register_left(register),
            //Rotates hl data right
            Instruction::RotateR(Dest::Reg(6)) => self.rotate_hl_data_right(),
            //Rotates Register right
            Instruction::RotateR(Dest::Reg(register)) => self.rotate_register_right(register),
            //Shifts hl data Left Arithmetic
            Instruction::ShiftL(Dest::Reg(6)) => self.shift_hl_data_left(),
            //Shifts register Left Arithmetic
            Instruction::ShiftL(Dest::Reg(register)) => self.shift_register_left(register),
            //Shifts hl data Right Arithmetic
            Instruction::ShiftRArith(Dest::Reg(6)) => self.shift_hl_data_right_ari(),
            //Shifts register Right Arithmetic
            Instruction::ShiftRArith(Dest::Reg(register)) => {
                self.shift_register_right_ari(register)
            }
            //Shifts hl data Right Logical
            Instruction::ShiftRLog(Dest::Reg(6)) => self.shift_hl_data_right_log(),
            //Shifts register Right Logical
            Instruction::ShiftRLog(Dest::Reg(register)) => self.shift_register_right_log(register),
            //Swaps hl data nibbles
            Instruction::SwapNibble(Dest::Reg(6)) => self.swap_hl_data_nibble(),
            //Swaps register nibbles
            Instruction::SwapNibble(Dest::Reg(register)) => self.swap_register_nibble(register),
            //Tests the bit in hl data and sets zero flag
            Instruction::TestBit(bit, Dest::Reg(6)) => self.test_hl_data_bit(bit),
            //Tests the bit in register and sets zero flag
            Instruction::TestBit(bit, Dest::Reg(register)) => self.test_register_bit(bit, register),
            //Resets the bit in hl data
            Instruction::ResetBit(bit, Dest::Reg(6)) => self.reset_hl_data_bit(bit),
            //Resets the bit in register
            Instruction::ResetBit(bit, Dest::Reg(register)) => {
                self.reset_register_bit(bit, register)
            }
            //Sets the bit in hl data
            Instruction::SetBit(bit, Dest::Reg(6)) => self.set_hl_data_bit(bit),
            //Sets the bit in register
            Instruction::SetBit(bit, Dest::Reg(register)) => self.set_register_bit(bit, register),
            //Sets the state to read from cb instructions next
            Instruction::Prefix => self.perform_cb_action(),
            //Jumps the program counter to 16 bit immediate
            Instruction::Jump(Dest::Immediate) => self.jump_to_immediate(),
            //Jumps the program counter to hl data
            Instruction::Jump(Dest::HL) => self.jump_to_hl_data(),
            //Jumps program counter to 16 bit immediate if the iszero check matches the zero flag
            Instruction::JumpCond(iszero, Dest::Immediate) => self.jump_cond_immediate(iszero),
            //Jumps program counter to relative immediate position
            Instruction::JumpRel => self.jump_rel_immediate(),
            //Jumps program counter to relative immediate position if iszero matches the zero flag
            Instruction::JumpRelCond(iszero) => self.jump_rel_cond_immediate(iszero),
            //Unconditional Function call to immediate
            Instruction::Call(Dest::Immediate) => self.call_immediate(),
            //Function call if iszero matches the zero flag
            Instruction::CondCall(iszero, Dest::Immediate) => self.call_cond_immediate(iszero),
            //Returns from function call
            Instruction::Ret => self.ret(),
            //Conditional returns from function call if iszero matches zero flag
            Instruction::CondRet(iszero) => self.ret_cond(iszero),
            //Unconditional return from function call and enables interrupts
            Instruction::RetI => self.ret_interrupt_handler(),
            //Function call to specific area specified by location
            Instruction::Restart(location) => self.restart(location),
            //State waits until an interrupt is given
            Instruction::HALT => self.halt(),
            //State waits until a button input is given
            Instruction::STOP => self.stop(),
            //Disables interrupts
            Instruction::DI => self.di(),
            //Enables interrupts
            Instruction::EI => self.ei(),
            //NOP operation
            _ => return,
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
            RegisterPairs::BC => {
                self.registers.b = greater;
                self.registers.c = lesser
            }
            RegisterPairs::DE => {
                self.registers.d = greater;
                self.registers.e = lesser
            }
            RegisterPairs::HL => {
                self.registers.h = greater;
                self.registers.l = lesser
            }
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
        self.registers.sp -= 1;
        self.write_memory(usize::from(self.registers.sp), self.registers.b);
        self.registers.sp -= 1;
        self.write_memory(usize::from(self.registers.sp), self.registers.c);
    }

    fn push_de_to_stack(&mut self) {
        self.registers.sp -= 1;
        self.write_memory(usize::from(self.registers.sp), self.registers.d);
        self.registers.sp -= 1;
        self.write_memory(usize::from(self.registers.sp), self.registers.e);
    }

    fn push_hl_to_stack(&mut self) {
        self.registers.sp -= 1;
        self.write_memory(usize::from(self.registers.sp), self.registers.h);
        self.registers.sp -= 1;
        self.write_memory(usize::from(self.registers.sp), self.registers.l);
    }

    fn pop_stack_to_bc(&mut self) {
        self.registers.c = self.read_memory(self.registers.sp);
        self.registers.sp += 1;
        self.registers.b = self.read_memory(self.registers.sp);
        self.registers.sp += 1;
    }

    fn pop_stack_to_de(&mut self) {
        self.registers.e = self.read_memory(self.registers.sp);
        self.registers.sp += 1;
        self.registers.d = self.read_memory(self.registers.sp);
        self.registers.sp += 1;
    }

    fn pop_stack_to_hl(&mut self) {
        self.registers.l = self.read_memory(self.registers.sp);
        self.registers.sp += 1;
        self.registers.h = self.read_memory(self.registers.sp);
        self.registers.sp += 1;
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
        self.registers.h = self.read_memory(stack_pointer + 1);
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

    fn add_x_to_a(&mut self, add_value: u8, carry: u8) {
        if check_half_carry_add(self.registers.a, add_value, carry) {
            self.registers.modify_half_carry_flag(FlagActions::Set);
        } else {
            self.registers.modify_half_carry_flag(FlagActions::Reset);
        }

        match self.registers.a.checked_add(add_value) {
            Some(x) => match x.checked_add(carry) {
                Some(y) => {
                    self.registers.a = y;
                    self.registers.modify_carry_flag(FlagActions::Reset);
                }
                None => {
                    self.registers.a = u8::MIN;
                    self.registers.modify_carry_flag(FlagActions::Set);
                }
            },
            None => {
                let a = Wrapping(self.registers.a);
                let b = Wrapping(add_value);
                let c = Wrapping(carry);
                self.registers.a = (a + b + c).0;

                self.registers.modify_carry_flag(FlagActions::Set);
            }
        }

        self.registers.set_zero_flag(self.registers.a == 0);

        self.registers.modify_sub_flag(FlagActions::Reset);
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

    fn sub_x_to_a(&mut self, sub_value: u8, carry: u8) {
        self.registers.set_half_carry_flag(check_half_carry_sub(
            self.registers.a,
            sub_value,
            carry,
        ));

        match self.registers.a.checked_sub(sub_value) {
            Some(x) => {
                self.registers.a = x;

                self.registers.set_carry_flag(false);
            }
            None => match (self.registers.a + carry).checked_sub(sub_value) {
                Some(_) => {
                    self.registers.a = u8::MIN;
                }
                None => {
                    let a = Wrapping(self.registers.a);
                    let b = Wrapping(sub_value);
                    let c = Wrapping(carry);
                    self.registers.a = (a - b + c).0;

                    self.registers.set_carry_flag(true);
                }
            },
        }

        self.registers.set_zero_flag(self.registers.a == 0);

        self.registers.set_sub_flag(true);
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

    fn cmp_x_to_a(&mut self, cmp_value: u8) {
        self.registers
            .set_half_carry_flag(check_half_carry_sub(self.registers.a, cmp_value, 0));

        match self.registers.a.checked_sub(cmp_value) {
            Some(x) => {
                self.registers.set_carry_flag(false);
                self.registers.set_zero_flag(x == 0);
            }
            None => {
                self.registers.set_carry_flag(true);
                self.registers.set_zero_flag(false);
            }
        }

        self.registers.modify_sub_flag(FlagActions::Set);
    }

    fn inc_hl_data(&mut self) {
        let mem = self.read_memory(self.registers.get_hl_value());
        let value = Wrapping(mem);
        let one = Wrapping(1u8);

        self.registers
            .set_half_carry_flag(check_half_carry_add(value.0, 1, 0));

        self.write_memory(usize::from(self.registers.get_hl_value()), (value + one).0);

        self.registers.set_sub_flag(false);

        self.registers.set_zero_flag(mem == 0);
    }

    fn inc_register(&mut self, register: Register) {
        let regvalue = self.read_register(register);
        let value = Wrapping(regvalue);
        let one = Wrapping(1u8);

        self.registers
            .set_half_carry_flag(check_half_carry_add(value.0, 1, 0));

        self.write_register(register, (value + one).0);

        self.registers.set_sub_flag(false);

        self.registers.set_zero_flag(regvalue == 0);
    }

    fn dec_hl_data(&mut self) {
        let mem = self.read_memory(self.registers.get_hl_value());
        let value = Wrapping(mem);
        let one = Wrapping(1u8);

        self.registers
            .set_half_carry_flag(check_half_carry_sub(value.0, 1, 0));

        self.write_memory(usize::from(self.registers.get_hl_value()), (value - one).0);

        self.registers.set_sub_flag(false);

        self.registers.set_zero_flag(mem == 0);
    }

    fn dec_register(&mut self, register: Register) {
        let regvalue = self.read_register(register);
        let value = Wrapping(regvalue);
        let one = Wrapping(1u8);

        self.registers
            .set_half_carry_flag(check_half_carry_sub(value.0, 1, 0));

        self.write_register(register, (value - one).0);

        self.registers.set_sub_flag(false);

        self.registers.set_zero_flag(regvalue == 0);
    }

    fn and_hl_data_to_a(&mut self) {
        self.registers.set_carry_flag(false);
        self.registers.set_half_carry_flag(true);
        self.registers.set_sub_flag(false);

        self.registers.a &= self.read_memory(self.registers.get_hl_value());
        self.registers.set_zero_flag(self.registers.a == 0);
    }

    fn and_register_to_a(&mut self, register: Register) {
        self.registers.set_carry_flag(false);
        self.registers.set_half_carry_flag(true);
        self.registers.set_sub_flag(false);

        self.registers.a &= self.read_register(register);
        self.registers.set_zero_flag(self.registers.a == 0);
    }

    fn and_immediate_to_a(&mut self) {
        self.registers.set_carry_flag(false);
        self.registers.set_half_carry_flag(true);
        self.registers.set_sub_flag(false);

        self.registers.a &= self.read_memory_from_pc();
        self.registers.set_zero_flag(self.registers.a == 0);
    }

    fn or_hl_data_to_a(&mut self) {
        self.registers.set_carry_flag(false);
        self.registers.set_half_carry_flag(false);
        self.registers.set_sub_flag(false);

        self.registers.a |= self.read_memory(self.registers.get_hl_value());
        self.registers.set_zero_flag(self.registers.a == 0);
    }

    fn or_register_to_a(&mut self, register: Register) {
        self.registers.set_carry_flag(false);
        self.registers.set_half_carry_flag(false);
        self.registers.set_sub_flag(false);

        self.registers.a |= self.read_register(register);
        self.registers.set_zero_flag(self.registers.a == 0);
    }

    fn or_immediate_to_a(&mut self) {
        self.registers.set_carry_flag(false);
        self.registers.set_half_carry_flag(false);
        self.registers.set_sub_flag(false);

        self.registers.a |= self.read_memory_from_pc();
        self.registers.set_zero_flag(self.registers.a == 0);
    }

    fn xor_hl_data_to_a(&mut self) {
        self.registers.set_carry_flag(false);
        self.registers.set_half_carry_flag(false);
        self.registers.set_sub_flag(false);

        self.registers.a ^= self.read_memory(self.registers.get_hl_value());
        self.registers.set_zero_flag(self.registers.a == 0);
    }

    fn xor_register_to_a(&mut self, register: Register) {
        self.registers.set_carry_flag(false);
        self.registers.set_half_carry_flag(false);
        self.registers.set_sub_flag(false);

        self.registers.a ^= self.read_register(register);
        self.registers.set_zero_flag(self.registers.a == 0);
    }

    fn xor_immediate_to_a(&mut self) {
        self.registers.set_carry_flag(false);
        self.registers.set_half_carry_flag(false);
        self.registers.set_sub_flag(false);

        self.registers.a ^= self.read_memory_from_pc();
        self.registers.set_zero_flag(self.registers.a == 0);
    }

    fn ccflag(&mut self) {
        self.registers.modify_carry_flag(FlagActions::Flip);
        self.registers.set_half_carry_flag(false);
        self.registers.set_sub_flag(false);
    }

    fn scflag(&mut self) {
        self.registers.set_carry_flag(true);
        self.registers.set_half_carry_flag(false);
        self.registers.set_sub_flag(false);
    }

    fn decimal_adjust_accumulator(&mut self) {
        let mut t = 0u8;

        if self.registers.get_half_carry_flag() == 1 || ((self.registers.a & 0xF) > 9) {
            t += 1;
        }

        if self.registers.get_carry_flag() == 1 || (self.registers.a > 0x99) {
            t += 2;
            self.registers.set_carry_flag(true);
        }

        // builds final H flag
        if self.registers.get_sub_flag() == 1 && self.registers.get_half_carry_flag() == 0 {
            self.registers.set_half_carry_flag(false);
        } else {
            if self.registers.get_sub_flag() == 1 && self.registers.get_half_carry_flag() == 1 {
                self.registers
                    .set_half_carry_flag((self.registers.a & 0x0F) < 6);
            } else {
                self.registers
                    .set_half_carry_flag((self.registers.a & 0x0F) >= 0x0A);
            }
        }

        if t == 1 {
            if self.registers.get_sub_flag() == 1 {
                self.registers.a -= 6;
            } else {
                self.registers.a += 6;
            }
        } else if t == 2 {
            if self.registers.get_sub_flag() == 1 {
                self.registers.a -= 0x60;
            } else {
                self.registers.a += 0x60;
            }
        } else if t == 3 {
            if self.registers.get_sub_flag() == 1 {
                self.registers.a -= 0x66;
            } else {
                self.registers.a += 0x66;
            }
        }
        self.registers.set_zero_flag(self.registers.a == 0);
    }

    fn complement_accumulator(&mut self) {
        self.registers.a ^= 0xFF;
        self.registers.set_half_carry_flag(true);
        self.registers.set_sub_flag(true);
    }

    fn modify_bc(&mut self, value: i8) {
        if value == 1 {
            let value = self.registers.get_bc_value() + 1;
            self.registers.c = (value % 16) as u8;
            self.registers.b = (value >> 4) as u8;
        } else {
            let value = self.registers.get_bc_value() - 1;
            self.registers.c = (value % 16) as u8;
            self.registers.b = (value >> 4) as u8;
        }
    }

    fn modify_de(&mut self, value: i8) {
        if value == 1 {
            let value = self.registers.get_de_value() + 1;
            self.registers.e = (value % 16) as u8;
            self.registers.d = (value >> 4) as u8;
        } else {
            let value = self.registers.get_de_value() - 1;
            self.registers.e = (value % 16) as u8;
            self.registers.d = (value >> 4) as u8;
        }
    }

    fn modify_hl(&mut self, value: i8) {
        if value == 1 {
            let value = self.registers.get_hl_value() + 1;
            self.registers.l = (value % 16) as u8;
            self.registers.h = (value >> 4) as u8;
        } else {
            let value = self.registers.get_hl_value() - 1;
            self.registers.l = (value % 16) as u8;
            self.registers.h = (value >> 4) as u8;
        }
    }

    fn increment_register_pair(&mut self, pair: RegisterPairs) {
        match pair {
            RegisterPairs::BC => self.modify_bc(1),
            RegisterPairs::DE => self.modify_de(1),
            RegisterPairs::HL => self.modify_hl(1),
        }
    }

    fn decrement_register_pair(&mut self, pair: RegisterPairs) {
        match pair {
            RegisterPairs::BC => self.modify_bc(-1),
            RegisterPairs::DE => self.modify_de(-1),
            RegisterPairs::HL => self.modify_hl(-1),
        }
    }

    fn add_register_pair_to_hl(&mut self, pair: RegisterPairs) {
        match pair {
            RegisterPairs::BC => self.add_value_to_hl(self.registers.get_bc_value()),
            RegisterPairs::DE => self.add_value_to_hl(self.registers.get_de_value()),
            RegisterPairs::HL => self.add_value_to_hl(self.registers.get_hl_value()),
        }
    }

    fn add_value_to_hl(&mut self, value: u16) {
        self.registers.set_half_carry_flag(check_half_carry_add_16(
            self.registers.get_hl_value(),
            value,
        ));

        let left = Wrapping(self.registers.get_hl_value());
        let right = Wrapping(value);
        match self.registers.get_hl_value().checked_add(value) {
            Some(_) => self.registers.set_carry_flag(false),
            None => self.registers.set_carry_flag(true),
        }

        let value = (left + right).0;
        self.registers.l = (value % 16) as u8;
        self.registers.h = (value > 4) as u8;
    }

    fn increase_stack_pointer(&mut self) {
        let value = self.read_memory_from_pc() as i8;
        let stack_pointer = self.registers.sp;

        self.registers.set_zero_flag(false);
        self.registers.set_sub_flag(false);

        let safestack = Wrapping(stack_pointer);
        if value > 0 {
            let safeval = Wrapping(value as u16);

            match stack_pointer.checked_add(value as u16) {
                Some(_) => self.registers.set_carry_flag(false),
                None => self.registers.set_carry_flag(true),
            }

            self.registers
                .set_half_carry_flag(check_half_carry_add_16(stack_pointer, value as u16));

            self.registers.sp = (safestack + safeval).0;
        } else {
            let safeval = Wrapping((-value) as u16);

            match stack_pointer.checked_sub((-value) as u16) {
                Some(_) => self.registers.set_carry_flag(false),
                None => self.registers.set_carry_flag(true),
            }

            self.registers
                .set_half_carry_flag(check_half_carry_sub_16(stack_pointer, (-value) as u16));

            self.registers.sp = (safestack - safeval).0;
        }
    }

    fn rotate_register_left_circular(&mut self, register: Register) {
        let regval = self.read_register(register);
        let leftmost = (regval & 0b1000_0000 > 7) as u8;
        self.write_register(register, (regval << 1) | leftmost);
        self.registers.set_carry_flag(leftmost == 1);
    }

    fn rotate_register_right_circular(&mut self, register: Register) {
        let regval = self.read_register(register);
        let rightmost = (regval & 0b0000_0001) as u8;
        self.write_register(register, (regval >> 1) | (rightmost << 7));
        self.registers.set_carry_flag(rightmost == 1);
    }

    fn rotate_register_left(&mut self, register: Register) {
        let regval = self.read_register(register);
        let leftmost = (regval & 0b1000_0000 > 7) as u8;
        let carry = self.registers.get_carry_flag();
        self.write_register(register, (regval << 1) | carry);
        self.registers.set_carry_flag(leftmost == 1);
    }

    fn rotate_register_right(&mut self, register: Register) {
        let regval = self.read_register(register);
        let rightmost = (regval & 0b0000_0001) as u8;
        let carry = self.registers.get_carry_flag();
        self.write_register(register, (regval >> 1) | (carry << 7));
        self.registers.set_carry_flag(rightmost == 1);
    }

    fn rotate_hl_data_left_circular(&mut self) {
        let regval = self.read_memory(self.registers.get_hl_value());
        let leftmost = (regval & 0b1000_0000 > 7) as u8;
        self.write_memory(
            usize::from(self.registers.get_hl_value()),
            (regval << 1) | leftmost,
        );
        self.registers.set_carry_flag(leftmost == 1);
    }

    fn rotate_hl_data_right_circular(&mut self) {
        let regval = self.read_memory(self.registers.get_hl_value());
        let rightmost = (regval & 0b0000_0001) as u8;
        self.write_memory(
            usize::from(self.registers.get_hl_value()),
            (regval >> 1) | (rightmost << 7),
        );
        self.registers.set_carry_flag(rightmost == 1);
    }

    fn rotate_hl_data_left(&mut self) {
        let regval = self.read_memory(self.registers.get_hl_value());
        let leftmost = (regval & 0b1000_0000 > 7) as u8;
        let carry = self.registers.get_carry_flag();
        self.write_memory(
            usize::from(self.registers.get_hl_value()),
            (regval << 1) | carry,
        );
        self.registers.set_carry_flag(leftmost == 1);
    }

    fn rotate_hl_data_right(&mut self) {
        let regval = self.read_memory(self.registers.get_hl_value());
        let rightmost = (regval & 0b0000_0001) as u8;
        let carry = self.registers.get_carry_flag();
        self.write_memory(
            usize::from(self.registers.get_hl_value()),
            (regval >> 1) | (carry << 7),
        );
        self.registers.set_carry_flag(rightmost == 1);
    }

    fn shift_register_left(&mut self, register: Register) {
        let regval = self.read_register(register);
        let leftmost = regval & 0b1000_0000 >> 7;
        self.write_register(register, regval << 1);
        self.registers.set_carry_flag(leftmost == 1);
        self.registers.set_zero_flag(regval << 1 == 0);
    }

    fn shift_register_right_ari(&mut self, register: Register) {
        let regval = self.read_register(register);
        let leftmost = regval & 0b1000_0000;
        let rightmost = regval & 0b0000_0001;
        self.write_register(register, (regval >> 1) | leftmost);
        self.registers.set_carry_flag(rightmost == 1);
        self.registers.set_zero_flag(regval << 1 == 0);
    }

    fn shift_register_right_log(&mut self, register: Register) {
        let regval = self.read_register(register);
        let rightmost = regval & 0b0000_0001;
        self.write_register(register, regval >> 1);
        self.registers.set_carry_flag(rightmost == 1);
        self.registers.set_zero_flag(regval << 1 == 0);
    }

    fn shift_hl_data_left(&mut self) {
        let regval = self.read_memory(self.registers.get_hl_value());
        let leftmost = regval & 0b1000_0000 >> 7;
        self.write_memory(usize::from(self.registers.get_hl_value()), regval << 1);
        self.registers.set_carry_flag(leftmost == 1);
        self.registers.set_zero_flag(regval << 1 == 0);
    }

    fn shift_hl_data_right_ari(&mut self) {
        let regval = self.read_memory(self.registers.get_hl_value());
        let leftmost = regval & 0b1000_0000;
        let rightmost = regval & 0b0000_0001;
        self.write_memory(
            usize::from(self.registers.get_hl_value()),
            (regval >> 1) | leftmost,
        );
        self.registers.set_carry_flag(rightmost == 1);
        self.registers.set_zero_flag(regval << 1 == 0);
    }

    fn shift_hl_data_right_log(&mut self) {
        let regval = self.read_memory(self.registers.get_hl_value());
        let rightmost = regval & 0b0000_0001;
        self.write_memory(usize::from(self.registers.get_hl_value()), regval >> 1);
        self.registers.set_carry_flag(rightmost == 1);
        self.registers.set_zero_flag(regval << 1 == 0);
    }

    fn swap_register_nibble(&mut self, register: Register) {
        self.registers.set_carry_flag(false);
        self.registers.set_half_carry_flag(false);
        self.registers.set_sub_flag(false);

        let regval = self.read_register(register);
        self.write_register(register, ((regval % 16) << 4) | (regval >> 4));
        self.registers.set_zero_flag(regval == 0);
    }

    fn swap_hl_data_nibble(&mut self) {
        self.registers.set_carry_flag(false);
        self.registers.set_half_carry_flag(false);
        self.registers.set_sub_flag(false);

        let regval = self.read_memory(self.registers.get_hl_value());
        self.write_memory(
            usize::from(self.registers.get_hl_value()),
            ((regval % 16) << 4) | (regval >> 4),
        );
        self.registers.set_zero_flag(regval == 0);
    }

    fn test_register_bit(&mut self, bit: u8, register: Register) {
        let regval = self.read_register(register);
        let bitval = (regval & (0b0000_0001 << bit)) >> bit;
        self.registers.set_half_carry_flag(true);
        self.registers.set_sub_flag(true);
        self.registers.set_zero_flag(bitval == 0);
    }

    fn test_hl_data_bit(&mut self, bit: u8) {
        let regval = self.read_memory(self.registers.get_hl_value());
        let bitval = (regval & (0b0000_0001 << bit)) >> bit;
        self.registers.set_half_carry_flag(true);
        self.registers.set_sub_flag(true);
        self.registers.set_zero_flag(bitval == 0);
    }

    fn set_register_bit(&mut self, bit: u8, register: Register) {
        let regval = self.read_register(register);
        let algo = 0b0000_0001 << bit;
        self.write_register(register, regval | algo);
    }

    fn set_hl_data_bit(&mut self, bit: u8) {
        let regval = self.read_memory(self.registers.get_hl_value());
        let algo = 0b0000_0001 << bit;
        self.write_memory(usize::from(self.registers.get_hl_value()), regval | algo);
    }

    fn reset_register_bit(&mut self, bit: u8, register: Register) {
        let regval = self.read_register(register);
        let algo = !((0b0000_0001 << bit) as u8);
        self.write_register(register, regval & algo);
    }

    fn reset_hl_data_bit(&mut self, bit: u8) {
        let regval = self.read_memory(self.registers.get_hl_value());
        let algo = !((0b0000_0001 << bit) as u8);
        self.write_memory(usize::from(self.registers.get_hl_value()), regval & algo);
    }

    fn perform_cb_action(&mut self) {
        //TODO: Set the next instruction read to read cb actions instead of normal actions
    }

    fn jump_to_hl_data(&mut self) {
        self.registers.pc = self.registers.get_hl_value();
    }

    fn jump_to_immediate(&mut self) {
        let lsb = self.read_memory_from_pc();
        let msb = self.read_memory_from_pc();
        self.registers.pc = Registers::join_u8(msb, lsb);
    }

    fn jump_cond_immediate(&mut self, iszero: bool) {
        if self.registers.get_zero_flag() ^ u8::from(iszero) == 0 {
            let lsb = self.read_memory_from_pc();
            let msb = self.read_memory_from_pc();
            self.registers.pc = Registers::join_u8(msb, lsb);
        }
    }

    fn jump_rel_immediate(&mut self) {
        let rel = self.read_memory_from_pc() as i8;
        if rel > 0 {
            self.registers.pc += rel as u16;
        } else {
            self.registers.pc -= (-rel) as u16;
        }
    }

    fn jump_rel_cond_immediate(&mut self, iszero: bool) {
        if self.registers.get_zero_flag() ^ u8::from(iszero) == 0 {
            let rel = self.read_memory_from_pc() as i8;
            if rel > 0 {
                self.registers.pc += rel as u16;
            } else {
                self.registers.pc -= (-rel) as u16;
            }
        }
    }

    fn call_immediate(&mut self) {
        let lsb = self.read_memory_from_pc();
        let msb = self.read_memory_from_pc();
        self.registers.sp -= 1;
        self.write_memory(
            usize::from(self.registers.sp),
            (self.registers.pc >> 4) as u8,
        );
        self.registers.sp -= 1;
        self.write_memory(
            usize::from(self.registers.sp),
            (self.registers.pc % 16) as u8,
        );
        self.registers.pc = Registers::join_u8(msb, lsb);
    }

    fn call_cond_immediate(&mut self, iszero: bool) {
        if self.registers.get_zero_flag() ^ u8::from(iszero) == 0 {
            let lsb = self.read_memory_from_pc();
            let msb = self.read_memory_from_pc();
            self.registers.sp -= 1;
            self.write_memory(
                usize::from(self.registers.sp),
                (self.registers.pc >> 4) as u8,
            );
            self.registers.sp -= 1;
            self.write_memory(
                usize::from(self.registers.sp),
                (self.registers.pc % 16) as u8,
            );
            self.registers.pc = Registers::join_u8(msb, lsb);
        }
    }

    fn ret(&mut self) {
        let lsb = self.read_memory(self.registers.sp);
        self.registers.sp += 1;
        let msb = self.read_memory(self.registers.sp);
        self.registers.sp += 1;
        self.registers.pc = Registers::join_u8(msb, lsb);
    }

    fn ret_cond(&mut self, iszero: bool) {
        if self.registers.get_zero_flag() ^ u8::from(iszero) == 0 {
            let lsb = self.read_memory(self.registers.sp);
            self.registers.sp += 1;
            let msb = self.read_memory(self.registers.sp);
            self.registers.sp += 1;
            self.registers.pc = Registers::join_u8(msb, lsb);
        }
    }

    fn ret_interrupt_handler(&mut self) {
        let lsb = self.read_memory(self.registers.sp);
        self.registers.sp += 1;
        let msb = self.read_memory(self.registers.sp);
        self.registers.sp += 1;
        self.registers.pc = Registers::join_u8(msb, lsb);
        self.interrupts = true;
    }

    fn restart(&mut self, location: u8) {
        let first = location & 0b0000_0110 >> 1;
        let second = location % 2;
        let mut position = 0x00 as u16;
        if first == 1 {
            position += 0x10;
        } else if first == 2 {
            position += 0x20;
        } else if first == 3 {
            position += 0x30;
        } else {
            panic!("This should not happen");
        }

        if second == 1 {
            position += 0x08;
        }

        self.registers.sp -= 1;
        self.write_memory(
            usize::from(self.registers.sp),
            (self.registers.pc >> 4) as u8,
        );
        self.registers.sp -= 1;
        self.write_memory(
            usize::from(self.registers.sp),
            (self.registers.pc % 16) as u8,
        );
        self.registers.pc = position;
    }

    fn halt(&mut self) {
        //TODO: if IME is set, wait until an interrupt
        // If IME is not set and none are pending, resume after an interrupt
        // If IME is set and there is a pending handle it? and resume immediately
    }

    fn stop(&mut self) {
        //TODO: Stops operations until a user input apparently
    }

    fn di(&mut self) {
        //println!("Interrupts disabled");
        self.interrupts = false;
    }

    fn ei(&mut self) {
        println!("Interrupts enabled");
        self.interrupts = true;
    }
}

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

    loop {
    //for _ in 1..500 {
        let data = state.read_memory_from_pc();
        state.perform_action(Instruction::translate(data));
        //let inter = state.interrupts;
        //println!("IME Allow Interrupt status {inter}, Interrupts: {:04x}", state.read_interrupt_enable() & state.read_interrupt_flags());
    }
    //state.registers.dump_registers();
}

