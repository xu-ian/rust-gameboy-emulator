pub mod instruction;
pub mod memory;
pub mod registers;
pub mod audio;
pub mod banks;

use banks::MBC;

use instruction::Dest;
use instruction::Instruction;
use instruction::MemAction;
use instruction::RegisterPairs;
use instruction::Cond;
use memory::Memory;
use registers::FlagActions;
use registers::Register;
use registers::Registers;
use std::num::Wrapping;
use std::sync::Arc;
use std::sync::Mutex;
use std::fs::OpenOptions;
use std::io::prelude::*;

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
    pub registers: Registers,
    pub memory: Memory,
    pub interrupts: bool,
    //User Input Variables
    pub joypad: [[u8; 4]; 2],
    pub input_type: u8,
    //Testing Variables
    pub logging: [bool; 4],
    //MBC variables
    pub banks: banks::Banks,
}

impl RunningState {

    pub fn new_plus(cart_type:u8, rom:u8, ram:u8) -> RunningState {
        RunningState {
            registers: Registers::new(),
            memory: Memory::new(),
            interrupts: false,
            joypad: [[1,1,1,1],[1,1,1,1]],
            input_type: 0x30u8,
            logging: [false, false, false, false],
            banks: banks::Banks::new(cart_type, rom, ram),
        }
    }

    pub fn new() -> RunningState {
        RunningState {
            registers: Registers::new(),
            memory: Memory::new(),
            interrupts: false,
            joypad: [[1,1,1,1],[1,1,1,1]],
            input_type: 0x30u8,
            logging: [false, false, false, false],
            banks: banks::Banks::new(0, 0, 0),
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

    fn join_bits(&self, typ: usize) -> u8 {
        self.joypad[typ][0] + (self.joypad[typ][1] << 1) + (self.joypad[typ][2] << 2) + (self.joypad[typ][3] << 3)
    }

    fn read_memory(&mut self, position: u16) -> u8 {
        if position == 0xff00 {
            if (!self.input_type & 0x30) == 0x20 {
                self.join_bits(1)
            } else if (!self.input_type & 0x30) == 0x10 {
                self.join_bits(0)
            } else {
                0x3f
            }
        } else if position < 0x4000 {
            self.banks.read_rom_0(usize::from(position))  
        } else if position >= 0x4000 && position < 0x8000 {
            self.banks.read_rom(usize::from(position))
        } else if position >= 0xA000 && position < 0xC000 {
            self.banks.read_ram(usize::from(position))
        }  else {
            self.memory.read_memory(position)
        }
    }

    pub fn read_active_interrupt(&mut self) -> u8 {
        self.read_memory(0xFFFF) & self.read_memory(0xFF0F)
    }

    //Only performs the interrrupt like a normal call
    pub fn handle_interrupt(&mut self) {
        let interrupts = self.read_active_interrupt();
        self.interrupts = false;
        if (interrupts & 0x1) > 0 {
            self.call(0, 0x40);
            self.write_memory(0xFF0F, interrupts & 0b1111_1110);
        } else if (interrupts & 0x2) > 0 {
            self.call(0, 0x48);
            self.write_memory(0xFF0F, interrupts & 0b1111_1101);
        } else if (interrupts & 0x4) > 0 {
            self.call(0, 0x50);
            self.write_memory(0xFF0F, interrupts & 0b1111_1011);
        } else if (interrupts & 0x8) > 0 {
            self.call(0, 0x58);
            self.write_memory(0xFF0F, interrupts & 0b1111_0111);
        } else if (interrupts & 0x10) > 0 {
            self.call(0, 0x60);
            self.write_memory(0xFF0F, interrupts & 0b1110_1111);
        } else {
            self.interrupts = true;
        }
    }

    fn call(&mut self, msb: u8, lsb: u8) {
        self.registers.sp -= 1;
        self.write_memory(
            usize::from(self.registers.sp),
            (self.registers.pc >> 8) as u8,
        );
        self.registers.sp -= 1;
        self.write_memory(
            usize::from(self.registers.sp),
            (self.registers.pc % 256) as u8,
        );
        self.registers.pc = Registers::join_u8(msb, lsb);
    }

    fn inc_it(&mut self) {
        let value = Wrapping(self.registers.pc);
        let one = Wrapping(1u16);
        self.registers.pc = (value + one).0;
    }

    fn read_memory_from_pc(&mut self) -> u8 {
        if self.logging[1] {
            let mut file = OpenOptions::new()
                .write(true)
                .append(true)
                .open("./src/log.txt")
                .unwrap();

            write!(file, "PC: {:04x}, ", self.registers.pc).expect("Could not open file");
        }
        if self.registers.pc < 0x8000 || (self.registers.pc >= 0xA000 && self.registers.pc <= 0xC000){
            let position = self.registers.pc;
            self.inc_it();
            self.read_memory(position)
        } else {
            self.memory.read_memory_from_pc(&mut self.registers)
        }
        //println!("{:#04x}", data);
    }

    fn write_memory(&mut self, position: usize, data: u8) {
        if position == 0xff00 {
            self.input_type = data;
        } else if position == 0xff14 && data & 0x80 > 0 {
            let mem = self.memory.read_memory(0xff26);
            self.memory.write_memory(0xff26, mem | 0x01);
        } else if position == 0xff19 && data & 0x80 > 0 {
            let mem = self.memory.read_memory(0xff26);
            self.memory.write_memory(0xff26, mem | 0x02);
        } else if position == 0xff1e && data & 0x80 > 0 {
            let mem = self.memory.read_memory(0xff26);
            self.memory.write_memory(0xff26, mem | 0x04);
        } else if position == 0xff23 && data & 0x80 > 0 {
            let mem = self.memory.read_memory(0xff26);
            self.memory.write_memory(0xff26, mem | 0x08);
        } else if position < 0x8000 { // Do not write to switchable ROM
            //println!("Bank:{:?}", self.banks.mbc);
            match self.banks.mbc {
                MBC::NoMBC => {
                    println!("Position: {:04x}, data: {:02x}", position, data);
                    if position >= 0x2000 && position < 0x4000 {
                        if data & 0x1f == 0 {
                            self.banks.set_rom_bank_lower(1);
                        } else {
                            self.banks.set_rom_bank_lower(data & 0x1f);
                        }
                    }
                },
                MBC::MBC1 => {
                    //println!("Position: {:04x}, data: {:02x}", position, data);
                    if position < 0x2000 && data == 0xA {
                        self.banks.enable_ram();
                    } else if position >= 0x2000 && position < 0x4000 {
                        if data & 0x1f == 0 {
                            self.banks.set_rom_bank_lower(1);
                        } else {
                            self.banks.set_rom_bank_lower(data & 0x1f);
                        }
                    } else if position >= 0x4000 && position < 0x6000 {
                        self.banks.set_rom_bank_upper_or_ram(data & 0x03);
                    } else if position >= 0x6000 && position < 0x8000 {
                        self.banks.set_banking_mode(data & 0x1);
                    }
                },
                _ => ()
            }
        } else if position >= 0xA000 && position < 0xC000 && self.banks.ram_enable { // Writing to switchable RAM
            self.banks.write_ram(position, data);
        } else {
            self.memory.write_memory(position, data);
        }
    }

    pub fn dump_registers(&mut self) {
        self.registers.dump_registers();
    }

    pub fn dump_tilemap(&mut self) {
        println!("Area 0");
        for i in 0x9800u16..0x9c00u16 {
            let x = self.read_memory(i);
            if i % 32 == 31 {
                println!("{} ", x);
            } else {
                print!("{} ", x);
            }
        }

        println!("Area 1");
        for i in 0x9c00u16..0xa000u16 {
            let x = self.read_memory(i);
            if i % 32 == 31 {
                println!("{} ", x);
            } else {
                print!("{} ", x);
            }
        }
    }

    pub fn dump_oam(&mut self) {
        for i in 0xFE00..0xFE9F {
            let x = self.read_memory(i);
            if i % 10 == 9 {
                println!("{:08b} ", x);
            } else {
                print!("{:08b} ", x);
            }
        }
    }

    pub fn next(&mut self) {
        let data = self.read_memory_from_pc();
        self.perform_action(Instruction::translate(data));
    }

    fn perform_action(&mut self, instruction: Instruction) {
        if self.logging[0] {
            let mut file = OpenOptions::new()
                .write(true)
                .append(true)
                .open("./src/log.txt")
                .unwrap();

            writeln!(file, "{}", instruction.to_string()).expect("Could not open file");
        }
        if self.logging[3] {
            println!("Instruction: {}", instruction.to_string());
        }
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
            Instruction::Load(register, Dest::Reg(6)) => self.load_hl_to_register(register),
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
            //Pushes data from register pair AF to stack
            Instruction::PushStack(Dest::RegPair(RegisterPairs::AF)) => self.push_af_to_stack(),
            //Pops stack to register pair BC
            Instruction::PopStack(Dest::RegPair(RegisterPairs::BC)) => self.pop_stack_to_bc(),
            //Pops stack to register pair DE
            Instruction::PopStack(Dest::RegPair(RegisterPairs::DE)) => self.pop_stack_to_de(),
            //Pops stack to register pair HL
            Instruction::PopStack(Dest::RegPair(RegisterPairs::HL)) => self.pop_stack_to_hl(),
            //Pops stack to register pair AF
            Instruction::PopStack(Dest::RegPair(RegisterPairs::AF)) => self.pop_stack_to_af(),
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
            Instruction::JumpCond(cond) => self.jump_cond_immediate(cond),
            //Jumps program counter to relative immediate position
            Instruction::JumpRel => self.jump_rel_immediate(),
            //Jumps program counter to relative immediate position if iszero matches the zero flag
            Instruction::JumpRelCond(cond) => self.jump_rel_cond_immediate(cond),
            //Unconditional Function call to immediate
            Instruction::Call => self.call_immediate(),
            //Function call if iszero matches the zero flag
            Instruction::CondCall(cond) => self.call_cond_immediate(cond),
            //Returns from function call
            Instruction::Ret => self.ret(),
            //Conditional returns from function call if iszero matches zero flag
            Instruction::CondRet(cond) => self.ret_cond(cond),
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

    fn load_hl_to_register(&mut self, register: Register) {
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
        self.registers.l = (address % 256) as u8;
        self.registers.h = (address >> 8) as u8;
    }

    fn save_a_to_hl_address_dec(&mut self) {
        let mut address = self.registers.get_hl_value();
        self.write_memory(usize::from(address), self.registers.a);
        decrement(&mut address);
        self.registers.l = (address % 256) as u8;
        self.registers.h = (address >> 8) as u8;
    }

    fn load_hl_address_to_a_inc(&mut self) {
        let mut address = self.registers.get_hl_value();
        self.registers.a = self.read_memory(address);

        increment(&mut address);
        self.registers.l = (address % 256) as u8;
        self.registers.h = (address >> 8) as u8;
    }

    fn save_a_to_hl_address_inc(&mut self) {
        let mut address = self.registers.get_hl_value();
        self.write_memory(usize::from(address), self.registers.a);
        increment(&mut address);
        self.registers.l = (address % 256) as u8;
        self.registers.h = (address >> 8) as u8;
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
            },
            RegisterPairs::AF => {
                self.registers.sp = Registers::join_u8(greater, lesser);
            }
        };
    }

    fn save_sp_to_immediate_address(&mut self) {
        let lesser = self.read_memory_from_pc();
        let greater = self.read_memory_from_pc();
        let address = Registers::join_u8(greater, lesser);
        let lsb = (self.registers.sp % 256) as u8;
        let msb = (self.registers.sp >> 8) as u8;
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

    fn push_af_to_stack(&mut self) {
        self.registers.sp -= 1;
        self.write_memory(usize::from(self.registers.sp), self.registers.a);
        self.registers.sp -= 1;
        self.write_memory(usize::from(self.registers.sp), self.registers.f);
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

    fn pop_stack_to_af(&mut self) {
        self.registers.f = self.read_memory(self.registers.sp);
        self.registers.sp += 1;
        self.registers.a = self.read_memory(self.registers.sp);
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
        let carry = self.registers.get_carry_flag();
        self.add_x_to_a(value, carry);
    }

    fn add_hl_data_to_a_carry(&mut self) {
        let value = self.read_memory(self.registers.get_hl_value());
        let carry = self.registers.get_carry_flag();
        self.add_x_to_a(value, carry);
    }

    fn add_immediate_to_a_carry(&mut self) {
        let value = self.read_memory_from_pc();
        let carry = self.registers.get_carry_flag();
        self.add_x_to_a(value, carry);
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
        let carry = self.registers.get_carry_flag();
        self.sub_x_to_a(value, carry);
    }

    fn sub_immediate_to_a_carry(&mut self) {
        let value = self.read_memory_from_pc();
        let carry = self.registers.get_carry_flag();
        self.sub_x_to_a(value, carry);
    }

    fn sub_hl_data_to_a_carry(&mut self) {
        let value = self.read_memory(self.registers.get_hl_value());
        let carry = self.registers.get_carry_flag();
        self.sub_x_to_a(value, carry);
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
                    self.registers.set_carry_flag(false);
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

        self.registers.set_zero_flag((value + one).0 == 0);
    }

    fn inc_register(&mut self, register: Register) {
        let regvalue = self.read_register(register);
        let value = Wrapping(regvalue);
        let one = Wrapping(1u8);

        self.registers
            .set_half_carry_flag(check_half_carry_add(value.0, 1, 0));

        self.write_register(register, (value + one).0);

        self.registers.set_sub_flag(false);

        self.registers.set_zero_flag((value + one).0 == 0);
    }

    fn dec_hl_data(&mut self) {
        let mem = self.read_memory(self.registers.get_hl_value());
        let value = Wrapping(mem);
        let one = Wrapping(1u8);

        self.registers
            .set_half_carry_flag(check_half_carry_sub(value.0, 1, 0));

        self.write_memory(usize::from(self.registers.get_hl_value()), (value - one).0);

        self.registers.set_sub_flag(true);

        self.registers.set_zero_flag((value - one).0 == 0);
    }

    fn dec_register(&mut self, register: Register) {
        let regvalue = self.read_register(register);
        let value = Wrapping(regvalue);
        let one = Wrapping(1u8);

        self.registers
            .set_half_carry_flag(check_half_carry_sub(value.0, 1, 0));

        self.write_register(register, (value - one).0);

        self.registers.set_sub_flag(true);

        self.registers.set_zero_flag((value - one).0 == 0);
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
        //println!("This triggers");
        let mut t = 0u8;
        //self.dump_registers();
        if self.registers.get_half_carry_flag() == 1 || ((self.registers.a & 0x0F) > 0x09) {
            t += 1;
        }

        if self.registers.get_carry_flag() == 1 || (self.registers.a > 0x99) {
            t += 2;
            self.registers.set_carry_flag(true);
        }

        // clears H flag
        self.registers.set_half_carry_flag(false);

        let alpha = Wrapping(self.registers.a);
        let six = Wrapping(0x06);
        let sixty = Wrapping(0x60);
        if t == 1 {
            //println!("Halfcarry");
            if self.registers.get_sub_flag() == 1 {
                //println!("Sub");
                self.registers.a = (alpha - six).0;
            } else {
                //println!("Add");
                self.registers.a = (alpha + six).0;
            }
        } else if t == 2 {
            //println!("Carry");
            if self.registers.get_sub_flag() == 1 {
                //println!("Sub");
                self.registers.a = (alpha - sixty).0;
            } else {
                //println!("Add");
                self.registers.a = (alpha + sixty).0;
            }
        } else if t == 3 {
            //println!("Carry + Halfcarry");
            if self.registers.get_sub_flag() == 1 {
                //println!("Sub");
                self.registers.a = (alpha - sixty - six).0;
            } else {
                //println!("Add");
                self.registers.a = (alpha + sixty + six).0;
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
        let bc = Wrapping(self.registers.get_bc_value());
        let one = Wrapping(1);
        if value == 1 {
            let value = (bc + one).0;
            self.registers.c = (value % 256) as u8;
            self.registers.b = (value >> 8) as u8;
        } else {
            let value = (bc - one).0;
            self.registers.c = (value % 256) as u8;
            self.registers.b = (value >> 8) as u8;
        }
    }

    fn modify_de(&mut self, value: i8) {
        let de = Wrapping(self.registers.get_de_value());
        let one = Wrapping(1);
        if value == 1 {
            let value = (de + one).0;
            self.registers.e = (value % 256) as u8;
            self.registers.d = (value >> 8) as u8;
        } else {
            let value = (de - one).0;
            self.registers.e = (value % 256) as u8;
            self.registers.d = (value >> 8) as u8;
        }
    }

    fn modify_hl(&mut self, value: i8) {
        let hl = Wrapping(self.registers.get_hl_value());
        let one = Wrapping(1);
        if value == 1 {
            let value = (hl + one).0;
            self.registers.l = (value % 256) as u8;
            self.registers.h = (value >> 8) as u8;
            
        } else {
            let value = (hl - one).0;
            self.registers.l = (value % 256) as u8;
            self.registers.h = (value >> 8) as u8;
        }
    }

    fn increment_register_pair(&mut self, pair: RegisterPairs) {
        match pair {
            RegisterPairs::BC => self.modify_bc(1),
            RegisterPairs::DE => self.modify_de(1),
            RegisterPairs::HL => self.modify_hl(1),
            RegisterPairs::AF => {
                let sp = Wrapping(self.registers.sp);
                let one = Wrapping(1);
                self.registers.sp= (sp + one).0;
            },
        }
    }

    fn decrement_register_pair(&mut self, pair: RegisterPairs) {
        match pair {
            RegisterPairs::BC => self.modify_bc(-1),
            RegisterPairs::DE => self.modify_de(-1),
            RegisterPairs::HL => self.modify_hl(-1),
            RegisterPairs::AF => {
                let sp = Wrapping(self.registers.sp);
                let one = Wrapping(1);
                self.registers.sp= (sp - one).0;
            },
        }
    }

    fn add_register_pair_to_hl(&mut self, pair: RegisterPairs) {
        match pair {
            RegisterPairs::BC => self.add_value_to_hl(self.registers.get_bc_value()),
            RegisterPairs::DE => self.add_value_to_hl(self.registers.get_de_value()),
            RegisterPairs::HL => self.add_value_to_hl(self.registers.get_hl_value()),
            RegisterPairs::AF => self.add_value_to_hl(self.registers.sp),
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
        self.registers.l = (value % 256) as u8;
        self.registers.h = (value >> 8) as u8;
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
        self.registers.set_half_carry_flag(false);
        self.registers.set_zero_flag(((regval << 1) | leftmost) == 0 && register != 7);
        self.registers.set_sub_flag(false);
    }

    fn rotate_register_right_circular(&mut self, register: Register) {
        let regval = self.read_register(register);
        let rightmost = (regval & 0b0000_0001) as u8;
        self.write_register(register, (regval >> 1) | (rightmost << 7));
        self.registers.set_carry_flag(rightmost == 1);
        self.registers.set_half_carry_flag(false);
        self.registers.set_zero_flag(((regval >> 1) | (rightmost << 7)) == 0 && register != 7);
        self.registers.set_sub_flag(false);
    }

    fn rotate_register_left(&mut self, register: Register) {
        let regval = self.read_register(register);
        let leftmost = (regval & 0b1000_0000 > 7) as u8;
        let carry = self.registers.get_carry_flag();
        self.write_register(register, (regval << 1) | carry);
        self.registers.set_carry_flag(leftmost == 1);
        self.registers.set_half_carry_flag(false);
        self.registers.set_zero_flag(((regval << 1) | carry) == 0 && register != 7);
        self.registers.set_sub_flag(false);
    }

    fn rotate_register_right(&mut self, register: Register) {
        let regval = self.read_register(register);
        let rightmost = (regval & 0b0000_0001) as u8;
        let carry = self.registers.get_carry_flag();
        self.write_register(register, (regval >> 1) | (carry << 7));
        self.registers.set_carry_flag(rightmost == 1);
        self.registers.set_half_carry_flag(false);
        self.registers.set_zero_flag(((regval >> 1) | (carry << 7)) == 0 && register != 7);
        self.registers.set_sub_flag(false);
    }

    fn rotate_hl_data_left_circular(&mut self) {
        let regval = self.read_memory(self.registers.get_hl_value());
        let leftmost = (regval & 0b1000_0000 > 7) as u8;
        self.write_memory(
            usize::from(self.registers.get_hl_value()),
            (regval << 1) | leftmost,
        );
        self.registers.set_carry_flag(leftmost == 1);
        self.registers.set_half_carry_flag(false);
        self.registers.set_zero_flag(((regval << 1) | leftmost) == 0);
        self.registers.set_sub_flag(false);
    }

    fn rotate_hl_data_right_circular(&mut self) {
        let regval = self.read_memory(self.registers.get_hl_value());
        let rightmost = (regval & 0b0000_0001) as u8;
        self.write_memory(
            usize::from(self.registers.get_hl_value()),
            (regval >> 1) | (rightmost << 7),
        );
        self.registers.set_carry_flag(rightmost == 1);
        self.registers.set_half_carry_flag(false);
        self.registers.set_zero_flag(((regval >> 1) | (rightmost << 7)) == 0);
        self.registers.set_sub_flag(false);
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
        self.registers.set_half_carry_flag(false);
        self.registers.set_zero_flag(((regval << 1) | carry) == 0);
        self.registers.set_sub_flag(false);
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
        self.registers.set_half_carry_flag(false);
        self.registers.set_zero_flag(((regval >> 1) | (carry << 7)) == 0);
        self.registers.set_sub_flag(false);
    }

    fn shift_register_left(&mut self, register: Register) {
        let regval = self.read_register(register);
        let leftmost = (regval & 0b1000_0000) >> 7;
        self.write_register(register, regval << 1);
        self.registers.set_carry_flag(leftmost == 1);
        self.registers.set_zero_flag(regval << 1 == 0);
        self.registers.set_sub_flag(false);
        self.registers.set_half_carry_flag(false);
    }

    fn shift_register_right_ari(&mut self, register: Register) {
        let regval = self.read_register(register);
        let leftmost = regval & 0b1000_0000;
        let rightmost = regval & 0b0000_0001;
        self.write_register(register, (regval >> 1) | leftmost);
        self.registers.set_carry_flag(rightmost == 1);
        self.registers.set_zero_flag((regval >> 1) | leftmost == 0);
        self.registers.set_sub_flag(false);
        self.registers.set_half_carry_flag(false);
    }

    fn shift_register_right_log(&mut self, register: Register) {
        let regval = self.read_register(register);
        let rightmost = regval & 0b0000_0001;
        self.write_register(register, regval >> 1);
        self.registers.set_carry_flag(rightmost == 1);
        self.registers.set_zero_flag(regval >> 1 == 0);
        self.registers.set_sub_flag(false);
        self.registers.set_half_carry_flag(false);
    }

    fn shift_hl_data_left(&mut self) {
        let regval = self.read_memory(self.registers.get_hl_value());
        let leftmost = (regval & 0b1000_0000) >> 7;
        self.write_memory(usize::from(self.registers.get_hl_value()), regval << 1);
        self.registers.set_carry_flag(leftmost == 1);
        self.registers.set_zero_flag(regval << 1 == 0);
        self.registers.set_sub_flag(false);
        self.registers.set_half_carry_flag(false);
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
        self.registers.set_zero_flag((regval >> 1) | leftmost == 0);
        self.registers.set_sub_flag(false);
        self.registers.set_half_carry_flag(false);
    }

    fn shift_hl_data_right_log(&mut self) {
        let regval = self.read_memory(self.registers.get_hl_value());
        let rightmost = regval & 0b0000_0001;
        self.write_memory(usize::from(self.registers.get_hl_value()), regval >> 1);
        self.registers.set_carry_flag(rightmost == 1);
        self.registers.set_zero_flag(regval >> 1 == 0);
        self.registers.set_sub_flag(false);
        self.registers.set_half_carry_flag(false);
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
        self.registers.set_sub_flag(false);
        self.registers.set_zero_flag(bitval == 0);
    }

    fn test_hl_data_bit(&mut self, bit: u8) {
        let regval = self.read_memory(self.registers.get_hl_value());
        let bitval = (regval & (0b0000_0001 << bit)) >> bit;
        self.registers.set_half_carry_flag(true);
        self.registers.set_sub_flag(false);
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
        let data = self.read_memory_from_pc();
        self.perform_action(Instruction::translatecb(data));
    }

    fn jump_to_hl_data(&mut self) {
        self.registers.pc = self.registers.get_hl_value();
        //println!("Jumped to: {:04x}", self.registers.pc);
    }

    fn jump_to_immediate(&mut self) {
        let lsb = self.read_memory_from_pc();
        let msb = self.read_memory_from_pc();
        self.registers.pc = Registers::join_u8(msb, lsb);
    }

    fn jump_cond_immediate(&mut self, cond: Cond) {
        let zero_flag = self.registers.get_zero_flag();
        let carry_flag = self.registers.get_carry_flag();
        let perform_action;
        match cond {
            Cond::NZ => perform_action = zero_flag == 0,
            Cond::Z => perform_action = zero_flag != 0,
            Cond::NC => perform_action = carry_flag == 0,
            Cond::C => perform_action = carry_flag != 0            
        }
        let lsb = self.read_memory_from_pc();
        let msb = self.read_memory_from_pc();
        if perform_action {
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

    fn jump_rel_cond_immediate(&mut self, cond: Cond) {
        if self.logging[3] {
            println!("Relative jump");
        }
        let zero_flag = self.registers.get_zero_flag();
        let carry_flag = self.registers.get_carry_flag();
        let perform_action;
        match cond {
            Cond::NZ => perform_action = zero_flag == 0,
            Cond::Z => perform_action = zero_flag != 0,
            Cond::NC => perform_action = carry_flag == 0,
            Cond::C => perform_action = carry_flag != 0            
        }
        let rel = self.read_memory_from_pc() as i8;
        if self.logging[3] {
            println!("Perform action: {}", perform_action);
        }
        if perform_action {
            if rel > 0 {
                self.registers.pc += rel as u16;
            } else {
                self.registers.pc -= (-rel) as u16;
            }
        }
        if self.logging[3] {
            println!("Result: {:04x}", self.registers.pc);
        }
    }

    fn call_immediate(&mut self) {
        let lsb = self.read_memory_from_pc();
        let msb = self.read_memory_from_pc();
        self.registers.sp -= 1;
        self.write_memory(
            usize::from(self.registers.sp),
            (self.registers.pc >> 8) as u8,
        );
        self.registers.sp -= 1;
        self.write_memory(
            usize::from(self.registers.sp),
            (self.registers.pc % 256) as u8,
        );
        self.registers.pc = Registers::join_u8(msb, lsb);
    }

    fn call_cond_immediate(&mut self, cond: Cond) {
        let zero_flag = self.registers.get_zero_flag();
        let carry_flag = self.registers.get_carry_flag();
        let perform_action;
        match cond {
            Cond::NZ => perform_action = zero_flag == 0,
            Cond::Z => perform_action = zero_flag != 0,
            Cond::NC => perform_action = carry_flag == 0,
            Cond::C => perform_action = carry_flag != 0            
        }
        let lsb = self.read_memory_from_pc();
        let msb = self.read_memory_from_pc();
        if perform_action {
            self.registers.sp -= 1;
            self.write_memory(
                usize::from(self.registers.sp),
                (self.registers.pc >> 8) as u8,
            );
            self.registers.sp -= 1;
            self.write_memory(
                usize::from(self.registers.sp),
                (self.registers.pc % 256) as u8,
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

    fn ret_cond(&mut self, cond: Cond) {
        let zero_flag = self.registers.get_zero_flag();
        let carry_flag = self.registers.get_carry_flag();
        let perform_action;
        match cond {
            Cond::NZ => perform_action = zero_flag == 0,
            Cond::Z => perform_action = zero_flag != 0,
            Cond::NC => perform_action = carry_flag == 0,
            Cond::C => perform_action = carry_flag != 0            
        }
        if perform_action {
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
        let first = (location & 0b0000_0110) >> 1;
        let second = location % 2;
        let mut position = 0x00 as u16;
        if first == 1 {
            position += 0x10;
        } else if first == 2 {
            position += 0x20;
        } else if first == 3 {
            position += 0x30;
        } else {}

        if second == 1 {
            position += 0x08;
        }

        //println!("Reset Position: {position:04x}");

        self.registers.sp -= 1;
        self.write_memory(
            usize::from(self.registers.sp),
            (self.registers.pc >> 8) as u8,
        );
        self.registers.sp -= 1;
        self.write_memory(
            usize::from(self.registers.sp),
            (self.registers.pc % 256) as u8,
        );
        self.registers.pc = position;
    }

    fn halt(&mut self) {
        println!("Called halt");
        //TODO: if IME is set, wait until an interrupt
        // If IME is not set and none are pending, resume after an interrupt
        // If IME is set and there is a pending handle it? and resume immediately
    }

    fn stop(&mut self) {
        println!("Called stop!");
        //TODO: Stops operations until a user input apparently
    }

    fn di(&mut self) {
        self.interrupts = false;
    }

    fn ei(&mut self) {
        self.interrupts = true;
    }
}

#[cfg(test)]
mod tests {

    use super::*;
    use std::time::Instant;

    //Load and Save do not change the flags

    #[test]
    fn check_load_immediate_to_hl() {
        let mut state = RunningState::new();
        state.write_memory(usize::from(state.registers.get_hl_value()), 0xFF);
        state.write_memory(usize::from(state.registers.pc), 0x00);
        state.load_immediate_to_hl();
        assert_eq!(0x00, state.read_memory(state.registers.get_hl_value()), 
                    "Memory from immediate was not saved in space pointed by hl");
    }

    #[test]
    fn performance_load_immediate_to_hl() {
        let start = Instant::now();
        let mut state = RunningState::new();
        for _ in 0..1_000_000 {
            state.load_immediate_to_hl();
            if state.registers.pc > 0xFF00 {
                state.registers.pc = 0;
            }
        }
        let x = Instant::now() - start;
        println!("Total {} micros", x.as_micros());
    }
    #[test]
    fn check_load_immediate_register() {
        let mut state = RunningState::new();
        for i in 0..8 {
            if i != 6 {
                state.write_register(i, 0x00);
                state.write_memory(usize::from(state.registers.pc), 0xFF);
                state.load_immediate_to_register(i);
                assert_eq!(0xFF, state.read_register(i), 
                            "Memory from immediate was not saved in register {i}");
            }
        }
    }

    #[test]
    fn check_save_register_to_hl() {
        let mut state = RunningState::new();
        for i in 0..8 {
            if i != 6 {
                state.write_register(i, 0xFF);
                state.write_memory(usize::from(state.registers.get_hl_value()), 0);
                state.save_register_to_hl(i);
                assert_eq!(0xFF, state.read_memory(state.registers.get_hl_value()),
                            "Memory from register {i} was not saved to hl memory location");
            }
        }
    }

    #[test]
    fn check_load_hl_to_register() {
        let mut state = RunningState::new();
        for i in 0..8 {
            if i != 6 {
                state.write_register(i, 0x00);
                state.write_memory(usize::from(state.registers.get_hl_value()), 0xFF); 
                state.load_hl_to_register(i);
                assert_eq!(0xFF, state.read_register(i), 
                            "Memory loaded from register {i} does not match memory from hl memory location");
            }
        }
    }

    #[test]
    fn check_register_to_register_copy() {
        let mut state = RunningState::new();
        for i in 0..8 {
            for j in 0..8 {
                if i != 6 && j != 6 {
                    state.write_register(i, 32-i);
                    state.write_register(j, 64-j);
                    state.register_to_register_copy(i, j);
                    assert_eq!(state.read_register(i), 64-j, "Register {i} did not copy data from register {j}");
                }
            }
        }
    }

    #[test]
    fn check_load_bc_address_to_a() {
        let mut state = RunningState::new();
        state.write_memory(usize::from(state.registers.get_bc_value()), 0xFF);
        state.registers.a = 0x00;
        state.load_bc_address_to_a();
        assert_eq!(state.registers.a, 0xFF, "Memory from BC address did not get loaded to register A");
    }

    #[test]
    fn check_save_a_to_bc_address() {
        let mut state = RunningState::new();
        state.write_memory(usize::from(state.registers.get_bc_value()), 0xFF);
        state.registers.a = 0x00;
        state.save_a_to_bc_address();
        assert_eq!(state.read_memory(state.registers.get_bc_value()), 0x00, "Memory from Register A did not get loaded to BC address");
    }

    #[test]
    fn check_load_de_address_to_a() {
        let mut state = RunningState::new();
        state.write_memory(usize::from(state.registers.get_de_value()), 0xFF);
        state.registers.a = 0x00;
        state.load_de_address_to_a();
        assert_eq!(state.registers.a, 0xFF, "Memory from DE address did not get loaded to register A");
    }

    #[test]
    fn check_save_a_to_de_address() {
        let mut state = RunningState::new();
        state.write_memory(usize::from(state.registers.get_de_value()), 0xFF);
        state.registers.a = 0x00;
        state.save_a_to_de_address();
        assert_eq!(state.read_memory(state.registers.get_de_value()), 0x00, "Memory from Register A did not get loaded to DE address");
    }

    #[test]
    fn check_load_immediate_address_to_a() {
        let mut state = RunningState::new();
        state.write_memory(usize::from(state.registers.pc), 0xAA);
        state.write_memory(usize::from(state.registers.pc+1), 0xAA);
        state.write_memory(0xAAAA, 0xFF);
        state.registers.a = 0x00;
        state.load_immediate_address_to_a();
        assert_eq!(state.registers.a, 0xFF, "Memory from immediate address did not get loaded to Register A")
    }

    #[test]
    fn check_save_a_to_immediate_address() {
        let mut state = RunningState::new();
        state.write_memory(usize::from(state.registers.pc), 0xAA);
        state.write_memory(usize::from(state.registers.pc+1), 0xAA);
        state.write_memory(0xAAAA, 0xFF);

        state.registers.a = 0x00;
        state.save_a_to_immediate_address();
        assert_eq!(state.read_memory(0xAAAA), 0x00, "Memory from Register A did not get loaded to the immediate address");
    }

    #[test]
    fn check_load_ff_c_to_a() {
        let mut state = RunningState::new();
        state.registers.c = 0xAA;
        state.registers.a = 0x00;
        state.write_memory(0xFFAA, 0xFF);
        state.load_ff_c_to_a();
        assert_eq!(state.registers.a, 0xFF, "Did not load data at position 0xFF00 + C to register A");
    }
    
    #[test]
    fn check_save_a_to_ff_c() {
        let mut state = RunningState::new();
        state.registers.c = 0xAA;
        state.registers.a = 0x00;
        state.write_memory(0xFFAA, 0xFF);
        state.save_a_to_ff_c();
        assert_eq!(0x00, state.read_memory(0xFFAA), "Did not save data from register A to at position 0xFF00 + C");
    }

    #[test]
    fn check_load_ff_n_to_a() {
        let mut state = RunningState::new();
        state.write_memory(usize::from(state.registers.pc), 0xAA);
        state.registers.a = 0x00;
        state.write_memory(0xFFAA, 0xFF);
        state.load_ff_n_to_a();
        assert_eq!(state.registers.a, 0xFF, "Did not load data at position 0xFF00 + immediate to register A");
    }

    #[test]
    fn check_save_a_to_ff_n() {
        let mut state = RunningState::new();
        state.write_memory(usize::from(state.registers.pc), 0xAA);
        state.registers.a = 0x00;
        state.write_memory(0xFFAA, 0xFF);
        state.save_a_to_ff_n();
        assert_eq!(0x00, state.read_memory(0xFFAA), "Did not save data from register A to at position 0xFF00 + immediate");
    }

    #[test]
    fn check_load_hl_address_to_a_dec() {
        //Normal case
        let mut state = RunningState::new();
        let current_hl_value = state.registers.get_hl_value();
        state.write_memory(usize::from(current_hl_value), 0xFF);
        state.registers.a = 0x00;
        state.load_hl_address_to_a_dec();
        assert_eq!(state.registers.a, 0xFF, "Memory from HL address did not get loaded to register A");
        assert_eq!(state.registers.get_hl_value(), current_hl_value - 1, "HL address did not decrease");

        //Underflow case(Should allow underflow)
        state.registers.h = 0;
        state.registers.l = 0;
        state.load_hl_address_to_a_dec();
    }
    
    #[test]
    fn check_save_a_to_hl_address_dec() {
        //Normal case
        let mut state = RunningState::new();
        let current_hl_value = state.registers.get_hl_value();
        state.write_memory(usize::from(current_hl_value), 0xFF);
        state.registers.a = 0x00;
        state.save_a_to_hl_address_dec();
        assert_eq!(0x00, state.read_memory(current_hl_value), "Memory from register A did not get loaded to HL address");
        assert_eq!(state.registers.get_hl_value(), current_hl_value - 1, "HL address did not decrease");

        //Underflow case(Should allow underflow)
        state.registers.h = 0;
        state.registers.l = 0;
        state.save_a_to_hl_address_dec();
    }

    #[test]
    fn check_load_hl_address_to_a_inc() {
        //Normal case
        let mut state = RunningState::new();
        let current_hl_value = state.registers.get_hl_value();
        state.write_memory(usize::from(current_hl_value), 0xFF);
        state.registers.a = 0x00;
        state.load_hl_address_to_a_inc();
        assert_eq!(state.registers.a, 0xFF, "Memory from HL address did not get loaded to register A");
        assert_eq!(state.registers.get_hl_value(), current_hl_value + 1, "HL address did not increase");

        //Overflow case(Should allow overflow)
        state.registers.h = 0xFF;
        state.registers.l = 0xFF;
        state.load_hl_address_to_a_inc();
    }

    #[test]
    fn check_save_a_to_hl_address_inc() {
        //Normal case
        let mut state = RunningState::new();
        let current_hl_value = state.registers.get_hl_value();
        state.write_memory(usize::from(current_hl_value), 0xFF);
        state.registers.a = 0x00;
        state.save_a_to_hl_address_inc();
        assert_eq!(0x00, state.read_memory(current_hl_value), "Memory from register A did not get loaded to HL address");
        assert_eq!(state.registers.get_hl_value(), current_hl_value + 1, "HL address did not increase");

        //Overflow case(Should allow overflow)
        state.registers.h = 0xFF;
        state.registers.l = 0xFF;
        state.save_a_to_hl_address_inc();
    }

    #[test]
    fn check_load_immediate_to_register_pair() {
        let mut state = RunningState::new();
        let program_counter = Clone::clone(&state.registers.pc);
        state.write_memory(usize::from(state.registers.pc), 0xFF);
        state.write_memory(usize::from(state.registers.pc + 1), 0xFE);
        state.registers.b = 0;
        state.registers.c = 0;
        state.registers.d = 0;
        state.registers.e = 0;
        state.registers.h = 0;
        state.registers.l = 0;
        state.registers.sp = 0;
        state.load_immediate_to_register_pair(RegisterPairs::BC);
        assert_eq!(0xFEFF, state.registers.get_bc_value(), "Did not load immediate to register pair BC");
        state.registers.pc = program_counter;
        state.load_immediate_to_register_pair(RegisterPairs::DE);
        assert_eq!(0xFEFF, state.registers.get_de_value(), "Did not load immediate to register pair DE");
        state.registers.pc = program_counter;
        state.load_immediate_to_register_pair(RegisterPairs::HL);
        assert_eq!(0xFEFF, state.registers.get_hl_value(), "Did not load immediate to register pair HL");
        state.registers.pc = program_counter;
        state.load_immediate_to_register_pair(RegisterPairs::AF);
        assert_eq!(0xFEFF, state.registers.sp, "Did not load immediate to stack pointer");        
        
    }

    #[test]
    fn check_save_sp_to_immediate_address() {
        let mut state = RunningState::new();
        state.registers.sp = 0xFFFE;
        state.write_memory(0xAAAA, 0);
        state.write_memory(0xAAAB, 0);
        state.write_memory(usize::from(state.registers.pc), 0xAA);
        state.write_memory(usize::from(state.registers.pc + 1), 0xAA);
        state.save_sp_to_immediate_address();
        assert_eq!(state.read_memory(0xAAAA), 0xFE, "Stack pointer lsb was not written to immediate address correctly");
        assert_eq!(state.read_memory(0xAAAB), 0xFF, "Stack pointer msb was not written to immediate address correctly");
    }

    #[test]
    fn check_load_hl_to_sp() {
        let mut state = RunningState::new();
        state.registers.h = 0xFF;
        state.registers.l = 0xFE;
        state.registers.sp = 0x0000;
        state.load_hl_to_sp();
        assert_eq!(state.registers.sp, 0xFFFE, "HL address was not loaded to stack pointer correctly");
    }

    #[test]
    fn check_push_bc_to_stack() {
        let mut state = RunningState::new();
        state.registers.b = 0xFF;
        state.registers.c = 0xFE;
        state.write_memory(0xFFFD, 0x00);
        state.write_memory(0xFFFC, 0x00);
        state.push_bc_to_stack();
        assert_eq!(state.read_memory(0xFFFD), 0xFF, "Did not write the B into the correct memory position");
        assert_eq!(state.read_memory(0xFFFC), 0xFE, "Did not write the C into the correct memory position");
    }

    #[test]
    fn check_push_de_to_stack() {
        let mut state = RunningState::new();
        state.registers.d = 0xFF;
        state.registers.e = 0xFE;
        state.write_memory(0xFFFD, 0x00);
        state.write_memory(0xFFFC, 0x00);
        state.push_de_to_stack();
        assert_eq!(state.read_memory(0xFFFD), 0xFF, "Did not write the D into the correct memory position");
        assert_eq!(state.read_memory(0xFFFC), 0xFE, "Did not write the E into the correct memory position");
    }

    #[test]
    fn check_push_hl_to_stack() {
        let mut state = RunningState::new();
        state.registers.h = 0xFF;
        state.registers.l = 0xFE;
        state.write_memory(0xFFFD, 0x00);
        state.write_memory(0xFFFC, 0x00);
        state.push_hl_to_stack();
        assert_eq!(state.read_memory(0xFFFD), 0xFF, "Did not write the H into the correct memory position");
        assert_eq!(state.read_memory(0xFFFC), 0xFE, "Did not write the L into the correct memory position");
    }

    #[test]
    fn check_push_af_to_stack() {
        let mut state = RunningState::new();
        state.registers.a = 0xFF;
        state.registers.f = 0xFE;
        state.write_memory(0xFFFD, 0x00);
        state.write_memory(0xFFFC, 0x00);
        state.push_af_to_stack();
        assert_eq!(state.read_memory(0xFFFD), 0xFF, "Did not write the A into the correct memory position");
        assert_eq!(state.read_memory(0xFFFC), 0xFE, "Did not write the F into the correct memory position");
    }

    #[test]
    fn check_pop_stack_to_bc() {
        let mut state = RunningState::new();
        state.registers.b = 0x00;
        state.registers.c = 0x00;
        state.registers.sp = 0xFFFC;
        state.write_memory(0xFFFD, 0xFF);
        state.write_memory(0xFFFC, 0xFE);
        state.pop_stack_to_bc();
        assert_eq!(state.registers.b, 0xFF, "Did not read the B from the correct memory position");
        assert_eq!(state.registers.c, 0xFE, "Did not read the C from the correct memory position");
    }
    
    #[test]
    fn check_pop_stack_to_de() {
        let mut state = RunningState::new();
        state.registers.d = 0x00;
        state.registers.e = 0x00;
        state.registers.sp = 0xFFFC;
        state.write_memory(0xFFFD, 0xFF);
        state.write_memory(0xFFFC, 0xFE);
        state.pop_stack_to_de();
        assert_eq!(state.registers.d, 0xFF, "Did not read the D from the correct memory position");
        assert_eq!(state.registers.e, 0xFE, "Did not read the E from the correct memory position");
    }

    #[test]
    fn check_pop_stack_to_hl() {
        let mut state = RunningState::new();
        state.registers.h = 0x00;
        state.registers.l = 0x00;
        state.registers.sp = 0xFFFC;
        state.write_memory(0xFFFD, 0xFF);
        state.write_memory(0xFFFC, 0xFE);
        state.pop_stack_to_hl();
        assert_eq!(state.registers.h, 0xFF, "Did not read the H from the correct memory position");
        assert_eq!(state.registers.l, 0xFE, "Did not read the L from the correct memory position");
    }

    #[test]
    fn check_pop_stack_to_af() {
        let mut state = RunningState::new();
        state.registers.a = 0x00;
        state.registers.f = 0x00;
        state.registers.sp = 0xFFFC;
        state.write_memory(0xFFFD, 0xFF);
        state.write_memory(0xFFFC, 0xFE);
        state.pop_stack_to_af();
        assert_eq!(state.registers.a, 0xFF, "Did not read the A from the correct memory position");
        assert_eq!(state.registers.f, 0xFE, "Did not read the F from the correct memory position");
    }

    #[test]
    fn check_load_adjusted_stack_to_hl() {
        let mut state = RunningState::new();
        state.registers.sp = 0xAAAA;
        state.registers.pc = 0x0100;
        state.registers.h = 0;
        state.registers.l = 0;
        state.write_memory(0xAAAF, 0x3);
        state.write_memory(0xAAB0, 0x4);
        state.write_memory(0x100, 5);
        state.load_adjusted_stack_to_hl();
        assert_eq!(state.registers.get_hl_value(), 0x0403, 
                    "Adjusted stack pointer did not load data from positive adjustment correctly");

        state.registers.sp = 0xAAAA;
        state.registers.pc = 0x0100;
        state.registers.h = 0;
        state.registers.l = 0;
        state.write_memory(0xAAA5, 0x1);
        state.write_memory(0xAAA6, 0x2);
        state.write_memory(0x100, 0b1111_1011);
        state.load_adjusted_stack_to_hl();
        assert_eq!(state.registers.get_hl_value(), 0x0201, 
                    "Adjusted stack pointer did not load data from negative adjustment correctly");
        
        //Overflowing stack pointer should fail, do not bother testing that

    }

    #[test]
    fn check_add_to_accumulator() {
        let mut state = RunningState::new();
        state.registers.h = 0;
        state.registers.l = 0;
        state.registers.b = 0b1111;
        state.registers.c = 0b1111;
        state.registers.d = 0b1111;
        state.registers.e = 0b1111;
        state.registers.pc = 0b0100;
        state.registers.a = 0b1100;
        state.write_memory(0, 0b1100);
        state.write_memory(1, 0b1110_1000);
        state.write_memory(usize::from(state.registers.pc), 0b1100);
        state.write_memory(usize::from(state.registers.pc+1), 0b1110_1000);
        state.write_memory(usize::from(state.registers.pc+2), 0b1000);
        
        state.add_hl_data_to_a();
        assert_eq!(state.registers.a, 0b1_1000, "Does not add the right number");
        assert_eq!(state.registers.get_half_carry_flag(), 1, "Does not set the half carry flag correctly");
        assert_eq!(state.registers.get_sub_flag(), 0, "Sets the subtraction flag erroneously");
        assert_eq!(state.registers.get_carry_flag(), 0, "Sets the carry flag erroneously");
        assert_eq!(state.registers.get_zero_flag(), 0, "Sets the zero flag erroneously");
        
        state.registers.l = 1;
        
        state.add_hl_data_to_a();
        assert_eq!(state.registers.a, 0, "Does not add the right number");
        assert_eq!(state.registers.get_carry_flag(), 1, "Does not set the carry flag");
        assert_eq!(state.registers.get_zero_flag(), 1, "Does not set the zero flag");

        state.registers.l = 0;
        state.add_hl_data_to_a();
        assert_eq!(state.registers.get_half_carry_flag(), 0, "Erroneously sets the half-carry flag");

        state.registers.a = 0b1100;

        state.add_immediate_to_a();
        assert_eq!(state.registers.a, 0b1_1000, "Does not add the right number");
        assert_eq!(state.registers.get_half_carry_flag(), 1, "Does not set the half carry flag correctly");
        assert_eq!(state.registers.get_sub_flag(), 0, "Sets the subtraction flag erroneously");
        assert_eq!(state.registers.get_carry_flag(), 0, "Sets the carry flag erroneously");
        assert_eq!(state.registers.get_zero_flag(), 0, "Sets the zero flag erroneously");

        state.add_immediate_to_a();
        assert_eq!(state.registers.a, 0, "Does not add the right number");
        assert_eq!(state.registers.get_carry_flag(), 1, "Does not set the carry flag");
        assert_eq!(state.registers.get_zero_flag(), 1, "Does not set the zero flag");

        state.registers.l = 0;
        state.add_hl_data_to_a();
        assert_eq!(state.registers.get_half_carry_flag(), 0, "Erroneously sets the half-carry flag");

        state.registers.h = 0b1111;
        state.registers.l = 0b1111;
        state.registers.a = 0b1111;
        for i in 0..8 {
            if i !=6 {
                state.add_register_to_a(i);
                if i != 7 {
                    assert_eq!(state.registers.a, 30 + i*15, "Does not add the correct numbers");
                } else {
                    assert_eq!(state.registers.a, 210, "Does not add the correct numbers");
                }
                assert_eq!(state.registers.get_half_carry_flag(), 1, "Does not set the half carry flag correctly");
                assert_eq!(state.registers.get_sub_flag(), 0, "Sets the subtraction flag erroneously");
                assert_eq!(state.registers.get_carry_flag(), 0, "Sets the carry flag erroneously");
                assert_eq!(state.registers.get_zero_flag(), 0, "Sets the zero flag erroneously");
            }
        }
        state.registers.a = 0b1111_1111;
        state.registers.b = 1;
        state.add_register_to_a(0);
        assert_eq!(state.registers.get_zero_flag(), 1, "Does not set the zero flag");
        assert_eq!(state.registers.get_carry_flag(), 1, "Does not set the carry flag");


    }

    #[test]
    fn check_add_immediate_to_a() {
        let mut state = RunningState::new();
        state.registers.pc = 0b0100;
        state.registers.a = 0b1100;
        state.write_memory(usize::from(state.registers.pc), 0b1100);
        state.write_memory(usize::from(state.registers.pc+1), 0b1110_1000);
        
        state.add_immediate_to_a();
        assert_eq!(state.registers.a, 0b1_1000, "Does not add the right number");
        assert_eq!(state.registers.get_half_carry_flag(), 1, "Does not set the half carry flag correctly");
        assert_eq!(state.registers.get_sub_flag(), 0, "Sets the subtraction flag erroneously");
        assert_eq!(state.registers.get_carry_flag(), 0, "Sets the carry flag erroneously");
        assert_eq!(state.registers.get_zero_flag(), 0, "Sets the zero flag erroneously");

        state.add_immediate_to_a();
        assert_eq!(state.registers.a, 0, "Does not add the right number");
        assert_eq!(state.registers.get_carry_flag(), 1, "Does not set the carry flag");
        assert_eq!(state.registers.get_zero_flag(), 1, "Does not set the zero flag");
    }

    #[test]
    fn check_add_hl_data_to_a() {
        let mut state = RunningState::new();
        state.registers.h = 0;
        state.registers.l = 0;
        state.registers.a = 0b1100;
        state.write_memory(0, 0b1100);
        state.write_memory(1, 0b1110_1000);
        
        state.add_hl_data_to_a();
        assert_eq!(state.registers.a, 0b1_1000, "Does not add the right number");
        assert_eq!(state.registers.get_half_carry_flag(), 1, "Does not set the half carry flag correctly");
        assert_eq!(state.registers.get_sub_flag(), 0, "Sets the subtraction flag erroneously");
        assert_eq!(state.registers.get_carry_flag(), 0, "Sets the carry flag erroneously");
        assert_eq!(state.registers.get_zero_flag(), 0, "Sets the zero flag erroneously");
        
        state.registers.l = 1;
        
        state.add_hl_data_to_a();
        assert_eq!(state.registers.a, 0, "Does not add the right number");
        assert_eq!(state.registers.get_carry_flag(), 1, "Does not set the carry flag");
        assert_eq!(state.registers.get_zero_flag(), 1, "Does not set the zero flag");

        state.registers.l = 0;
        state.add_hl_data_to_a();
        assert_eq!(state.registers.get_half_carry_flag(), 0, "Erroneously sets the half-carry flag");
    }

    #[test]
    fn check_add_register_to_a() {
        let mut state = RunningState::new();
        state.registers.b = 0b1111;
        state.registers.c = 0b1111;
        state.registers.d = 0b1111;
        state.registers.e = 0b1111;
        state.registers.h = 0b1111;
        state.registers.l = 0b1111;
        state.registers.a = 0b1111;
        for i in 0..8 {
            if i !=6 {
                state.add_register_to_a(i);
                if i != 7 {
                    assert_eq!(state.registers.a, 30 + i*15, "Does not add the correct numbers");
                } else {
                    assert_eq!(state.registers.a, 210, "Does not add the correct numbers");
                }
                assert_eq!(state.registers.get_half_carry_flag(), 1, "Does not set the half carry flag correctly");
                assert_eq!(state.registers.get_sub_flag(), 0, "Sets the subtraction flag erroneously");
                assert_eq!(state.registers.get_carry_flag(), 0, "Sets the carry flag erroneously");
                assert_eq!(state.registers.get_zero_flag(), 0, "Sets the zero flag erroneously");
            }
        }
        state.registers.a = 0b1111_1111;
        state.registers.b = 1;
        state.add_register_to_a(0);
        assert_eq!(state.registers.get_zero_flag(), 1, "Does not set the zero flag");
        assert_eq!(state.registers.get_carry_flag(), 1, "Does not set the carry flag");

        state.registers.a = 1;
        state.add_register_to_a(0);
        assert_eq!(state.registers.get_half_carry_flag(), 0, "Erroneously sets the half carry flag");

    }

    #[test]
    fn check_add_immediate_to_a_carry() {
        let mut state = RunningState::new();
        state.registers.pc = 0b0100;
        state.registers.a = 0b1011;
        state.registers.set_carry_flag(true);
        state.write_memory(usize::from(state.registers.pc), 0b1100);
        state.write_memory(usize::from(state.registers.pc+1), 0b1110_0111);
        state.write_memory(usize::from(state.registers.pc + 2), 1);

        state.add_immediate_to_a_carry();
        assert_eq!(state.registers.a, 0b1_1000, "Does not add the right number");
        assert_eq!(state.registers.get_half_carry_flag(), 1, "Does not set the half carry flag correctly");
        assert_eq!(state.registers.get_sub_flag(), 0, "Sets the subtraction flag erroneously");
        assert_eq!(state.registers.get_carry_flag(), 0, "Sets the carry flag erroneously");
        assert_eq!(state.registers.get_zero_flag(), 0, "Sets the zero flag erroneously");

        state.registers.set_carry_flag(true);
        state.add_immediate_to_a_carry();
        assert_eq!(state.registers.a, 0, "Does not add the right number");
        assert_eq!(state.registers.get_carry_flag(), 1, "Does not set the carry flag");
        assert_eq!(state.registers.get_zero_flag(), 1, "Does not set the zero flag");

        state.registers.set_carry_flag(true);
        state.add_immediate_to_a_carry();
        assert_eq!(state.registers.a, 2, "Does not add the right number");
        assert_eq!(state.registers.get_half_carry_flag(), 0, "Erroneously sets the half carry flag");

    }

    #[test]
    fn check_add_hl_data_to_a_carry() {
        let mut state = RunningState::new();
        state.registers.h = 0;
        state.registers.l = 0;
        state.registers.a = 0b1100;
        state.write_memory(0, 0b1011);
        state.write_memory(1, 0b1110_0111);
        state.registers.set_carry_flag(true);
        
        state.add_hl_data_to_a_carry();
        assert_eq!(state.registers.a, 0b1_1000, "Does not add the right number");
        assert_eq!(state.registers.get_half_carry_flag(), 1, "Does not set the half carry flag correctly");
        assert_eq!(state.registers.get_sub_flag(), 0, "Sets the subtraction flag erroneously");
        assert_eq!(state.registers.get_carry_flag(), 0, "Sets the carry flag erroneously");
        assert_eq!(state.registers.get_zero_flag(), 0, "Sets the zero flag erroneously");
        
        state.registers.l = 1;        
        state.registers.set_carry_flag(true);

        state.add_hl_data_to_a_carry();
        assert_eq!(state.registers.a, 0, "Does not add the right number");
        assert_eq!(state.registers.get_carry_flag(), 1, "Does not set the carry flag");
        assert_eq!(state.registers.get_zero_flag(), 1, "Does not set the zero flag");

        state.registers.l = 0;
        state.add_hl_data_to_a_carry();
        assert_eq!(state.registers.get_half_carry_flag(), 0, "Erroneously sets the half-carry flag");
    }

    #[test]
    fn check_add_register_to_a_carry() {
        let mut state = RunningState::new();
        state.registers.b = 0b1110;
        state.registers.c = 0b1110;
        state.registers.d = 0b1110;
        state.registers.e = 0b1110;
        state.registers.h = 0b1110;
        state.registers.l = 0b1110;
        state.registers.a = 0b1111;
        for i in 0..8 {
            if i !=6 {
                state.registers.set_carry_flag(true);
                state.add_register_to_a_carry(i);
                if i != 7 {
                    assert_eq!(state.registers.a, 30 + i*15, "Does not add the correct numbers");
                } else {
                    assert_eq!(state.registers.a, 211, "Does not add the correct numbers");
                }
                assert_eq!(state.registers.get_half_carry_flag(), 1, "Does not set the half carry flag correctly");
                assert_eq!(state.registers.get_sub_flag(), 0, "Sets the subtraction flag erroneously");
                assert_eq!(state.registers.get_carry_flag(), 0, "Sets the carry flag erroneously");
                assert_eq!(state.registers.get_zero_flag(), 0, "Sets the zero flag erroneously");
            }
        }
        state.registers.a = 0b1111_1110;
        state.registers.b = 1;
        state.registers.set_carry_flag(true);
        state.add_register_to_a_carry(0);
        assert_eq!(state.registers.get_zero_flag(), 1, "Does not set the zero flag");
        assert_eq!(state.registers.get_carry_flag(), 1, "Does not set the carry flag");

        state.registers.a = 1;
        state.add_register_to_a(0);
        assert_eq!(state.registers.get_half_carry_flag(), 0, "Erroneously sets the half carry flag");

    }

    #[test]
    fn check_sub_immediate_from_a() {
        let mut state = RunningState::new();
        state.registers.a = 1;
        state.write_memory(usize::from(state.registers.pc), 2);
        state.sub_immediate_to_a();
        assert_eq!(state.registers.a, 0b1111_1111, "Did not subtract properly");
        assert_eq!(state.registers.get_half_carry_flag(), 1, "Did not set half carry flag");
        assert_eq!(state.registers.get_carry_flag(), 1, "Did not set carry flag");
        assert_eq!(state.registers.get_zero_flag(), 0, "Erroneously set half zero flag");
        assert_eq!(state.registers.get_sub_flag(), 1, "Did not set sub flag");
        state.registers.a = 1;
        state.write_memory(usize::from(state.registers.pc), 1);
        state.sub_immediate_to_a();
        assert_eq!(state.registers.a, 0, "Did not subtract properly");
        assert_eq!(state.registers.get_half_carry_flag(), 0, "Erroneously set half carry flag");
        assert_eq!(state.registers.get_carry_flag(), 0, "Erroneously set carry flag");
        assert_eq!(state.registers.get_zero_flag(), 1, "Did not set half zero flag");
        assert_eq!(state.registers.get_sub_flag(), 1, "Did not set sub flag");
    }

    #[test]
    fn check_sub_hl_data_from_a() {
        let mut state = RunningState::new();
        state.registers.a = 1;
        state.registers.h = 0;
        state.registers.l = 0;
        state.write_memory(0, 2);
        state.sub_hl_data_to_a();
        assert_eq!(state.registers.a, 0b1111_1111, "Did not subtract properly");
        assert_eq!(state.registers.get_half_carry_flag(), 1, "Did not set half carry flag");
        assert_eq!(state.registers.get_carry_flag(), 1, "Did not set carry flag");
        assert_eq!(state.registers.get_zero_flag(), 0, "Erroneously set half zero flag");
        assert_eq!(state.registers.get_sub_flag(), 1, "Did not set sub flag");
        state.registers.a = 1;
        state.write_memory(0, 1);
        state.sub_hl_data_to_a();
        assert_eq!(state.registers.a, 0, "Did not subtract properly");
        assert_eq!(state.registers.get_half_carry_flag(), 0, "Erroneously set half carry flag");
        assert_eq!(state.registers.get_carry_flag(), 0, "Erroneously set carry flag");
        assert_eq!(state.registers.get_zero_flag(), 1, "Did not set half zero flag");
        assert_eq!(state.registers.get_sub_flag(), 1, "Did not set sub flag");
    }

    #[test]
    fn check_sub_register_from_a() {
        let mut state = RunningState::new();
        for i in 0..6 {
            state.registers.a = 1;
            state.write_register(i, 2);
            state.sub_register_to_a(i);
            assert_eq!(state.registers.a, 0b1111_1111, "Did not subtract properly");
            assert_eq!(state.registers.get_half_carry_flag(), 1, "Did not set half carry flag");
            assert_eq!(state.registers.get_carry_flag(), 1, "Did not set carry flag");
            assert_eq!(state.registers.get_zero_flag(), 0, "Erroneously set half zero flag");
            assert_eq!(state.registers.get_sub_flag(), 1, "Did not set sub flag");
            state.registers.a = 1;
            state.write_register(i, 1);
            state.sub_register_to_a(i);
            assert_eq!(state.registers.a, 0, "Did not subtract properly");
            assert_eq!(state.registers.get_half_carry_flag(), 0, "Erroneously set half carry flag");
            assert_eq!(state.registers.get_carry_flag(), 0, "Erroneously set carry flag");
            assert_eq!(state.registers.get_zero_flag(), 1, "Did not set half zero flag");
            assert_eq!(state.registers.get_sub_flag(), 1, "Did not set sub flag");
        }
        state.sub_register_to_a(7);
        assert_eq!(state.registers.a, 0, "Did not subtract properly");
        assert_eq!(state.registers.get_half_carry_flag(), 0, "Erroneously set half carry flag");
        assert_eq!(state.registers.get_carry_flag(), 0, "Erroneously set carry flag");
        assert_eq!(state.registers.get_zero_flag(), 1, "Did not set half zero flag");
        assert_eq!(state.registers.get_sub_flag(), 1, "Did not set sub flag");
    }

    #[test]
    fn check_sub_immediate_from_a_carry() {
        let mut state = RunningState::new();
        state.registers.a = 1;
        state.write_memory(usize::from(state.registers.pc), 3);
        state.registers.set_carry_flag(true);
        state.sub_immediate_to_a_carry();
        assert_eq!(state.registers.a, 0b1111_1111, "Did not subtract properly");
        assert_eq!(state.registers.get_half_carry_flag(), 1, "Did not set half carry flag");
        assert_eq!(state.registers.get_carry_flag(), 1, "Did not set carry flag");
        assert_eq!(state.registers.get_zero_flag(), 0, "Erroneously set half zero flag");
        assert_eq!(state.registers.get_sub_flag(), 1, "Did not set sub flag");
        state.registers.a = 1;
        state.write_memory(usize::from(state.registers.pc), 2);
        state.sub_immediate_to_a_carry();
        assert_eq!(state.registers.a, 0, "Did not subtract properly");
        assert_eq!(state.registers.get_half_carry_flag(), 0, "Erroneously set half carry flag");
        assert_eq!(state.registers.get_carry_flag(), 0, "Erroneously set carry flag");
        assert_eq!(state.registers.get_zero_flag(), 1, "Did not set half zero flag");
        assert_eq!(state.registers.get_sub_flag(), 1, "Did not set sub flag");
    }

    #[test]
    fn check_sub_hl_data_from_a_carry() {
        let mut state = RunningState::new();
        state.registers.a = 1;
        state.registers.h = 0;
        state.registers.l = 0;
        state.registers.set_carry_flag(true);
        state.write_memory(0, 3);
        state.sub_hl_data_to_a_carry();
        assert_eq!(state.registers.a, 0b1111_1111, "Did not subtract properly");
        assert_eq!(state.registers.get_half_carry_flag(), 1, "Did not set half carry flag");
        assert_eq!(state.registers.get_carry_flag(), 1, "Did not set carry flag");
        assert_eq!(state.registers.get_zero_flag(), 0, "Erroneously set half zero flag");
        assert_eq!(state.registers.get_sub_flag(), 1, "Did not set sub flag");
        state.registers.a = 1;
        state.write_memory(0, 2);
        state.sub_hl_data_to_a_carry();
        assert_eq!(state.registers.a, 0, "Did not subtract properly");
        assert_eq!(state.registers.get_half_carry_flag(), 0, "Erroneously set half carry flag");
        assert_eq!(state.registers.get_carry_flag(), 0, "Erroneously set carry flag");
        assert_eq!(state.registers.get_zero_flag(), 1, "Did not set half zero flag");
        assert_eq!(state.registers.get_sub_flag(), 1, "Did not set sub flag");
    }

    #[test]
    fn check_sub_register_from_a_carry() {
        let mut state = RunningState::new();
        for i in 0..6 {
            state.registers.a = 1;
            state.write_register(i, 3);
            state.registers.set_carry_flag(true);
            state.sub_register_to_a_carry(i);
            assert_eq!(state.registers.a, 0b1111_1111, "Did not subtract properly");
            assert_eq!(state.registers.get_half_carry_flag(), 1, "Did not set half carry flag");
            assert_eq!(state.registers.get_carry_flag(), 1, "Did not set carry flag");
            assert_eq!(state.registers.get_zero_flag(), 0, "Erroneously set half zero flag");
            assert_eq!(state.registers.get_sub_flag(), 1, "Did not set sub flag");
            state.registers.a = 1;
            state.write_register(i, 2);
            state.sub_register_to_a_carry(i);
            assert_eq!(state.registers.a, 0, "Did not subtract properly");
            assert_eq!(state.registers.get_half_carry_flag(), 0, "Erroneously set half carry flag");
            assert_eq!(state.registers.get_carry_flag(), 0, "Erroneously set carry flag");
            assert_eq!(state.registers.get_zero_flag(), 1, "Did not set half zero flag");
            assert_eq!(state.registers.get_sub_flag(), 1, "Did not set sub flag");
        }
        state.sub_register_to_a_carry(7);
        assert_eq!(state.registers.a, 0, "Did not subtract properly");
        assert_eq!(state.registers.get_half_carry_flag(), 0, "Erroneously set half carry flag");
        assert_eq!(state.registers.get_carry_flag(), 0, "Erroneously set carry flag");
        assert_eq!(state.registers.get_zero_flag(), 1, "Did not set half zero flag");
        assert_eq!(state.registers.get_sub_flag(), 1, "Did not set sub flag");
    }

    #[test]
    fn check_cmp_immediate_from_a() {
        let mut state = RunningState::new();
        state.registers.a = 1;
        state.write_memory(usize::from(state.registers.pc), 2);
        state.cmp_immediate_to_a();
        assert_eq!(state.registers.get_half_carry_flag(), 1, "Did not set half carry flag");
        assert_eq!(state.registers.get_carry_flag(), 1, "Did not set carry flag");
        assert_eq!(state.registers.get_zero_flag(), 0, "Erroneously set half zero flag");
        assert_eq!(state.registers.get_sub_flag(), 1, "Did not set sub flag");
        state.registers.a = 1;
        state.write_memory(usize::from(state.registers.pc), 1);
        state.cmp_immediate_to_a();
        assert_eq!(state.registers.get_half_carry_flag(), 0, "Erroneously set half carry flag");
        assert_eq!(state.registers.get_carry_flag(), 0, "Erroneously set carry flag");
        assert_eq!(state.registers.get_zero_flag(), 1, "Did not set half zero flag");
        assert_eq!(state.registers.get_sub_flag(), 1, "Did not set sub flag");
    }

    #[test]
    fn check_cmp_hl_data_from_a() {
        let mut state = RunningState::new();
        state.registers.a = 1;
        state.registers.h = 0;
        state.registers.l = 0;
        state.write_memory(0, 2);
        state.cmp_hl_data_to_a();
        assert_eq!(state.registers.get_half_carry_flag(), 1, "Did not set half carry flag");
        assert_eq!(state.registers.get_carry_flag(), 1, "Did not set carry flag");
        assert_eq!(state.registers.get_zero_flag(), 0, "Erroneously set half zero flag");
        assert_eq!(state.registers.get_sub_flag(), 1, "Did not set sub flag");
        state.registers.a = 1;
        state.write_memory(0, 1);
        state.cmp_hl_data_to_a();
        assert_eq!(state.registers.get_half_carry_flag(), 0, "Erroneously set half carry flag");
        assert_eq!(state.registers.get_carry_flag(), 0, "Erroneously set carry flag");
        assert_eq!(state.registers.get_zero_flag(), 1, "Did not set half zero flag");
        assert_eq!(state.registers.get_sub_flag(), 1, "Did not set sub flag");
    }

    #[test]
    fn check_cmp_register_from_a() {
        let mut state = RunningState::new();
        for i in 0..6 {
            state.registers.a = 1;
            state.write_register(i, 2);
            state.cmp_register_to_a(i);
            assert_eq!(state.registers.a, 1, "Changed the accumulator register");
            assert_eq!(state.registers.get_half_carry_flag(), 1, "Did not set half carry flag");
            assert_eq!(state.registers.get_carry_flag(), 1, "Did not set carry flag");
            assert_eq!(state.registers.get_zero_flag(), 0, "Erroneously set half zero flag");
            assert_eq!(state.registers.get_sub_flag(), 1, "Did not set sub flag");
            state.registers.a = 1;
            state.write_register(i, 1);
            state.cmp_register_to_a(i);
            assert_eq!(state.registers.a, 1, "Changed the accumulator register");
            assert_eq!(state.registers.get_half_carry_flag(), 0, "Erroneously set half carry flag");
            assert_eq!(state.registers.get_carry_flag(), 0, "Erroneously set carry flag");
            assert_eq!(state.registers.get_zero_flag(), 1, "Did not set half zero flag");
            assert_eq!(state.registers.get_sub_flag(), 1, "Did not set sub flag");
        }
        state.cmp_register_to_a(7);
        assert_eq!(state.registers.a, 1, "Changed the accumulator register");
        assert_eq!(state.registers.get_half_carry_flag(), 0, "Erroneously set half carry flag");
        assert_eq!(state.registers.get_carry_flag(), 0, "Erroneously set carry flag");
        assert_eq!(state.registers.get_zero_flag(), 1, "Did not set half zero flag");
        assert_eq!(state.registers.get_sub_flag(), 1, "Did not set sub flag");
    }

    #[test]
    fn check_inc_hl_data() {
        let mut state = RunningState::new();
        [state.registers.h, state.registers.l] = [0, 0];
        state.inc_hl_data();
        assert_eq!(state.read_memory(0), 1, "Does not increment correctly");
        assert_eq!(state.registers.get_sub_flag(), 0, "Does not set sub flag correctly");
        assert_eq!(state.registers.get_zero_flag(), 0, "Does not set zero flag correctly");
        assert_eq!(state.registers.get_half_carry_flag(), 0, "Does not set half carry flag correctly");
        state.write_memory(0, 0xFF);
        state.inc_hl_data();
        assert_eq!(state.read_memory(0), 0, "Does not increment correctly");
        assert_eq!(state.registers.get_sub_flag(), 0, "Does not set sub flag correctly");
        assert_eq!(state.registers.get_zero_flag(), 1, "Does not set zero flag correctly");
        assert_eq!(state.registers.get_half_carry_flag(), 1, "Does not set half carry flag correctly");        
    }

    #[test]
    fn check_inc_register() {
        let mut state = RunningState::new();
        for i in 0..8 {
            if i != 6 {
                state.write_register(i, 0);
                state.inc_register(i);
                assert_eq!(state.read_register(i), 1, "Does not increment correctly");
                assert_eq!(state.registers.get_sub_flag(), 0, "Does not set sub flag correctly");
                assert_eq!(state.registers.get_zero_flag(), 0, "Does not set zero flag correctly");
                assert_eq!(state.registers.get_half_carry_flag(), 0, "Does not set half carry flag correctly");
                state.write_register(i, 0xFF);
                state.inc_register(i);
                assert_eq!(state.read_register(i), 0, "Does not increment correctly");
                assert_eq!(state.registers.get_sub_flag(), 0, "Does not set sub flag correctly");
                assert_eq!(state.registers.get_zero_flag(), 1, "Does not set zero flag correctly");
                assert_eq!(state.registers.get_half_carry_flag(), 1, "Does not set half carry flag correctly");                     
            }
        }
    }
    
    #[test]
    fn check_dec_hl_data() {
        let mut state = RunningState::new();
        [state.registers.h, state.registers.l] = [0, 0];
        state.write_memory(0, 1);
        state.dec_hl_data();
        assert_eq!(state.read_memory(0), 0, "Does not increment correctly");
        assert_eq!(state.registers.get_sub_flag(), 1, "Does not set sub flag correctly");
        assert_eq!(state.registers.get_zero_flag(), 1, "Does not set zero flag correctly");
        assert_eq!(state.registers.get_half_carry_flag(), 0, "Does not set half carry flag correctly");
        state.dec_hl_data();
        assert_eq!(state.read_memory(0), 0xFF, "Does not increment correctly");
        assert_eq!(state.registers.get_sub_flag(), 1, "Does not set sub flag correctly");
        assert_eq!(state.registers.get_zero_flag(), 0, "Does not set zero flag correctly");
        assert_eq!(state.registers.get_half_carry_flag(), 1, "Does not set half carry flag correctly");        
    }

    #[test]
    fn check_dec_register() {
        let mut state = RunningState::new();
        for i in 0..8 {
            if i != 6 {
                state.write_register(i, 1);
                state.dec_register(i);
                assert_eq!(state.read_register(i), 0, "Does not decrement correctly");
                assert_eq!(state.registers.get_sub_flag(), 1, "Does not set sub flag correctly");
                assert_eq!(state.registers.get_zero_flag(), 1, "Does not set zero flag correctly");
                assert_eq!(state.registers.get_half_carry_flag(), 0, "Does not set half carry flag correctly");
                state.dec_register(i);
                assert_eq!(state.read_register(i), 0xFF, "Does not decrement correctly");
                assert_eq!(state.registers.get_sub_flag(), 1, "Does not set sub flag correctly");
                assert_eq!(state.registers.get_zero_flag(), 0, "Does not set zero flag correctly");
                assert_eq!(state.registers.get_half_carry_flag(), 1, "Does not set half carry flag correctly");                     
            }
        }
    }
    
    #[test]
    fn check_and_hl_data_to_a() {
        let mut state = RunningState::new();
        state.registers.a = 0b1001;
        state.registers.h = 0;
        state.registers.l = 0;
        state.write_memory(0, 0b1000);
        state.and_hl_data_to_a();
        assert_eq!(state.registers.a, 0b1000, 
            "Did not perform and operation correctly {}, 0b1000", state.registers.a);
        assert_eq!(state.registers.get_carry_flag(), 0, "Incorrect carry flag");
        assert_eq!(state.registers.get_half_carry_flag(), 1, "Incorrect half carry flag");
        assert_eq!(state.registers.get_zero_flag(), 0, "Incorrect zero flag");
        assert_eq!(state.registers.get_sub_flag(), 0, "Incorrect sub flag");
        state.write_memory(0, 0b0110);
        state.and_hl_data_to_a();
        assert_eq!(state.registers.a, 0, 
            "Did not perform and operation correctly {}, 0", state.registers.a);
        assert_eq!(state.registers.get_carry_flag(), 0, "Incorrect carry flag");
        assert_eq!(state.registers.get_half_carry_flag(), 1, "Incorrect half carry flag");
        assert_eq!(state.registers.get_zero_flag(), 1, "Incorrect zero flag");
        assert_eq!(state.registers.get_sub_flag(), 0, "Incorrect sub flag");
        
    }

    #[test]
    fn check_and_immediate_to_a() {
        let mut state = RunningState::new();
        state.registers.a = 0b1001;
        state.registers.pc = 0x100;
        state.write_memory(0x100, 0b1000);
        state.and_immediate_to_a();
        assert_eq!(state.registers.a, 0b1000, 
            "Did not perform and operation correctly {}, 0b1000", state.registers.a);
        assert_eq!(state.registers.get_carry_flag(), 0, "Incorrect carry flag");
        assert_eq!(state.registers.get_half_carry_flag(), 1, "Incorrect half carry flag");
        assert_eq!(state.registers.get_zero_flag(), 0, "Incorrect zero flag");
        assert_eq!(state.registers.get_sub_flag(), 0, "Incorrect sub flag");
        state.write_memory(0x101, 0b0110);
        state.and_immediate_to_a();
        assert_eq!(state.registers.a, 0, 
            "Did not perform and operation correctly {}, 0", state.registers.a);
        assert_eq!(state.registers.get_carry_flag(), 0, "Incorrect carry flag");
        assert_eq!(state.registers.get_half_carry_flag(), 1, "Incorrect half carry flag");
        assert_eq!(state.registers.get_zero_flag(), 1, "Incorrect zero flag");
        assert_eq!(state.registers.get_sub_flag(), 0, "Incorrect sub flag");
    }

    #[test]
    fn check_and_register_to_a() {
        let mut state = RunningState::new();
        for i in 0..6 {
            state.registers.a = 0b1001;
            state.write_register(i, 0b1000);
            state.and_register_to_a(i);
            assert_eq!(state.registers.a, 0b1000, 
                "Did not perform and operation correctly {}, 0b1000", state.registers.a);
            assert_eq!(state.registers.get_carry_flag(), 0, "Incorrect carry flag");
            assert_eq!(state.registers.get_half_carry_flag(), 1, "Incorrect half carry flag");
            assert_eq!(state.registers.get_zero_flag(), 0, "Incorrect zero flag");
            assert_eq!(state.registers.get_sub_flag(), 0, "Incorrect sub flag");
            state.write_register(i, 0b0110);
            state.and_register_to_a(i);
            assert_eq!(state.registers.a, 0, 
                "Did not perform and operation correctly {}, 0", state.registers.a);
            assert_eq!(state.registers.get_carry_flag(), 0, "Incorrect carry flag");
            assert_eq!(state.registers.get_half_carry_flag(), 1, "Incorrect half carry flag");
            assert_eq!(state.registers.get_zero_flag(), 1, "Incorrect zero flag");
            assert_eq!(state.registers.get_sub_flag(), 0, "Incorrect sub flag");
        }
        state.write_register(7, 0b1000);
        state.and_register_to_a(7);
        assert_eq!(state.registers.a, 8, 
            "Did not perform and operation correctly {}, 8", state.registers.a);
        assert_eq!(state.registers.get_carry_flag(), 0, "Incorrect carry flag");
        assert_eq!(state.registers.get_half_carry_flag(), 1, "Incorrect half carry flag");
        assert_eq!(state.registers.get_zero_flag(), 0, "Incorrect zero flag");
        assert_eq!(state.registers.get_sub_flag(), 0, "Incorrect sub flag");
    }

    #[test]
    fn check_or_hl_data_to_a() {
        let mut state = RunningState::new();
        state.registers.a = 0b0001;
        state.registers.h = 0;
        state.registers.l = 0;
        state.write_memory(0, 0b1000);
        state.or_hl_data_to_a();
        assert_eq!(state.registers.a, 0b1001, 
            "Did not perform and operation correctly {}, 0b1001", state.registers.a);
        assert_eq!(state.registers.get_carry_flag(), 0, "Incorrect carry flag");
        assert_eq!(state.registers.get_half_carry_flag(), 0, "Incorrect half carry flag");
        assert_eq!(state.registers.get_zero_flag(), 0, "Incorrect zero flag");
        assert_eq!(state.registers.get_sub_flag(), 0, "Incorrect sub flag");
        state.write_memory(0, 0);
        state.registers.a = 0;
        state.or_hl_data_to_a();
        assert_eq!(state.registers.a, 0, 
            "Did not perform and operation correctly {}, 0", state.registers.a);
        assert_eq!(state.registers.get_carry_flag(), 0, "Incorrect carry flag");
        assert_eq!(state.registers.get_half_carry_flag(), 0, "Incorrect half carry flag");
        assert_eq!(state.registers.get_zero_flag(), 1, "Incorrect zero flag");
        assert_eq!(state.registers.get_sub_flag(), 0, "Incorrect sub flag");
        
    }

    #[test]
    fn check_or_immediate_to_a() {
        let mut state = RunningState::new();
        state.registers.a = 0b1000;
        state.registers.pc = 0x100;
        state.write_memory(0x100, 0b0001);
        state.or_immediate_to_a();
        assert_eq!(state.registers.a, 0b1001, 
            "Did not perform and operation correctly {}, 0b1001", state.registers.a);
        assert_eq!(state.registers.get_carry_flag(), 0, "Incorrect carry flag");
        assert_eq!(state.registers.get_half_carry_flag(), 0, "Incorrect half carry flag");
        assert_eq!(state.registers.get_zero_flag(), 0, "Incorrect zero flag");
        assert_eq!(state.registers.get_sub_flag(), 0, "Incorrect sub flag");
        state.write_memory(0x101, 0b0000);
        state.registers.a = 0;
        state.or_immediate_to_a();
        assert_eq!(state.registers.a, 0, 
            "Did not perform and operation correctly {}, 0", state.registers.a);
        assert_eq!(state.registers.get_carry_flag(), 0, "Incorrect carry flag");
        assert_eq!(state.registers.get_half_carry_flag(), 0, "Incorrect half carry flag");
        assert_eq!(state.registers.get_zero_flag(), 1, "Incorrect zero flag");
        assert_eq!(state.registers.get_sub_flag(), 0, "Incorrect sub flag");
    }

    #[test]
    fn check_or_register_to_a() {
        let mut state = RunningState::new();
        for i in 0..6 {
            state.registers.a = 0b1000;
            state.write_register(i, 0b0001);
            state.or_register_to_a(i);
            assert_eq!(state.registers.a, 0b1001, 
                "Did not perform and operation correctly {}, 0b1000", state.registers.a);
            assert_eq!(state.registers.get_carry_flag(), 0, "Incorrect carry flag");
            assert_eq!(state.registers.get_half_carry_flag(), 0, "Incorrect half carry flag");
            assert_eq!(state.registers.get_zero_flag(), 0, "Incorrect zero flag");
            assert_eq!(state.registers.get_sub_flag(), 0, "Incorrect sub flag");
            state.write_register(i, 0b0000);
            state.registers.a = 0;
            state.or_register_to_a(i);
            assert_eq!(state.registers.a, 0, 
                "Did not perform and operation correctly {}, 0", state.registers.a);
            assert_eq!(state.registers.get_carry_flag(), 0, "Incorrect carry flag");
            assert_eq!(state.registers.get_half_carry_flag(), 0, "Incorrect half carry flag");
            assert_eq!(state.registers.get_zero_flag(), 1, "Incorrect zero flag");
            assert_eq!(state.registers.get_sub_flag(), 0, "Incorrect sub flag");
        }
        state.write_register(7, 0b1000);
        state.or_register_to_a(7);
        assert_eq!(state.registers.a, 8, 
            "Did not perform and operation correctly {}, 8", state.registers.a);
        assert_eq!(state.registers.get_carry_flag(), 0, "Incorrect carry flag");
        assert_eq!(state.registers.get_half_carry_flag(), 0, "Incorrect half carry flag");
        assert_eq!(state.registers.get_zero_flag(), 0, "Incorrect zero flag");
        assert_eq!(state.registers.get_sub_flag(), 0, "Incorrect sub flag");
    }

    #[test]
    fn check_xor_hl_data_to_a() {
        let mut state = RunningState::new();
        state.registers.a = 0b0001;
        state.registers.h = 0;
        state.registers.l = 0;
        state.write_memory(0, 0b1000);
        state.xor_hl_data_to_a();
        assert_eq!(state.registers.a, 0b1001, 
            "Did not perform and operation correctly {}, 0b1001", state.registers.a);
        assert_eq!(state.registers.get_carry_flag(), 0, "Incorrect carry flag");
        assert_eq!(state.registers.get_half_carry_flag(), 0, "Incorrect half carry flag");
        assert_eq!(state.registers.get_zero_flag(), 0, "Incorrect zero flag");
        assert_eq!(state.registers.get_sub_flag(), 0, "Incorrect sub flag");
        state.write_memory(0, 0b1001);
        state.xor_hl_data_to_a();
        assert_eq!(state.registers.a, 0, 
            "Did not perform and operation correctly {}, 0", state.registers.a);
        assert_eq!(state.registers.get_carry_flag(), 0, "Incorrect carry flag");
        assert_eq!(state.registers.get_half_carry_flag(), 0, "Incorrect half carry flag");
        assert_eq!(state.registers.get_zero_flag(), 1, "Incorrect zero flag");
        assert_eq!(state.registers.get_sub_flag(), 0, "Incorrect sub flag");
        
    }

    #[test]
    fn check_xor_immediate_to_a() {
        let mut state = RunningState::new();
        state.registers.a = 0b1000;
        state.registers.pc = 0x100;
        state.write_memory(0x100, 0b0001);
        state.xor_immediate_to_a();
        assert_eq!(state.registers.a, 0b1001, 
            "Did not perform and operation correctly {}, 0b1001", state.registers.a);
        assert_eq!(state.registers.get_carry_flag(), 0, "Incorrect carry flag");
        assert_eq!(state.registers.get_half_carry_flag(), 0, "Incorrect half carry flag");
        assert_eq!(state.registers.get_zero_flag(), 0, "Incorrect zero flag");
        assert_eq!(state.registers.get_sub_flag(), 0, "Incorrect sub flag");
        state.write_memory(0x101, 0b1001);
        state.xor_immediate_to_a();
        assert_eq!(state.registers.a, 0, 
            "Did not perform and operation correctly {}, 0", state.registers.a);
        assert_eq!(state.registers.get_carry_flag(), 0, "Incorrect carry flag");
        assert_eq!(state.registers.get_half_carry_flag(), 0, "Incorrect half carry flag");
        assert_eq!(state.registers.get_zero_flag(), 1, "Incorrect zero flag");
        assert_eq!(state.registers.get_sub_flag(), 0, "Incorrect sub flag");
    }

    #[test]
    fn check_xor_register_to_a() {
        let mut state = RunningState::new();
        for i in 0..6 {
            state.registers.a = 0b1000;
            state.write_register(i, 0b0001);
            state.xor_register_to_a(i);
            assert_eq!(state.registers.a, 0b1001, 
                "Did not perform and operation correctly {}, 0b1000", state.registers.a);
            assert_eq!(state.registers.get_carry_flag(), 0, "Incorrect carry flag");
            assert_eq!(state.registers.get_half_carry_flag(), 0, "Incorrect half carry flag");
            assert_eq!(state.registers.get_zero_flag(), 0, "Incorrect zero flag");
            assert_eq!(state.registers.get_sub_flag(), 0, "Incorrect sub flag");
            state.write_register(i, 0b1001);
            state.xor_register_to_a(i);
            assert_eq!(state.registers.a, 0, 
                "Did not perform and operation correctly {}, 0", state.registers.a);
            assert_eq!(state.registers.get_carry_flag(), 0, "Incorrect carry flag");
            assert_eq!(state.registers.get_half_carry_flag(), 0, "Incorrect half carry flag");
            assert_eq!(state.registers.get_zero_flag(), 1, "Incorrect zero flag");
            assert_eq!(state.registers.get_sub_flag(), 0, "Incorrect sub flag");
        }
        state.write_register(7, 0b1000);
        state.xor_register_to_a(7);
        assert_eq!(state.registers.a, 0, 
            "Did not perform and operation correctly {}, 0", state.registers.a);
        assert_eq!(state.registers.get_carry_flag(), 0, "Incorrect carry flag");
        assert_eq!(state.registers.get_half_carry_flag(), 0, "Incorrect half carry flag");
        assert_eq!(state.registers.get_zero_flag(), 1, "Incorrect zero flag");
        assert_eq!(state.registers.get_sub_flag(), 0, "Incorrect sub flag");
    }

    #[test]
    fn check_ccflag() {
        let mut state = RunningState::new();
        state.registers.set_carry_flag(true);
        state.ccflag();
        assert_eq!(state.registers.get_carry_flag(), 0, "Did not flip carry flag");
        assert_eq!(state.registers.get_half_carry_flag(), 0, "Did not set half carry to 0");
        assert_eq!(state.registers.get_sub_flag(), 0, "Did not set sub flag to 0");
        state.ccflag();
        assert_eq!(state.registers.get_carry_flag(), 1, "Did not flip carry flag");
        assert_eq!(state.registers.get_half_carry_flag(), 0, "Did not set half carry to 0");
        assert_eq!(state.registers.get_sub_flag(), 0, "Did not set sub flag to 0");
    }

    #[test]
    fn check_scflag() {
        let mut state = RunningState::new();
        state.registers.set_carry_flag(false);
        state.scflag();
        assert_eq!(state.registers.get_carry_flag(), 1, "Did not set carry flag");
        assert_eq!(state.registers.get_half_carry_flag(), 0, "Did not set half carry to 0");
        assert_eq!(state.registers.get_sub_flag(), 0, "Did not set sub flag to 0");
    }

    #[test]
    fn check_decimal_adjust_accumulator() {
        let mut state = RunningState::new();
        state.registers.a = 0b0001_1001; //DAA 19
        state.registers.b = 0b0010_1000;//DAA 28
        state.add_register_to_a(0);
        state.decimal_adjust_accumulator();
        assert_eq!(state.registers.a, 0b0100_0111, "Did not do DAA conversion properly");
        assert_eq!(state.registers.get_carry_flag(), 0, "Set carry flag");
        assert_eq!(state.registers.get_zero_flag(), 0, "Set zero flag");
        assert_eq!(state.registers.get_half_carry_flag(), 0, "Halfcarry flag should never be set");

        state.registers.a = 0b100_0111; //DAA 47
        state.registers.b = 0b10_1000;//DAA 28
        state.sub_register_to_a(0);
        state.decimal_adjust_accumulator();
        assert_eq!(state.registers.a, 0b0001_1001, "Did not do DAA conversion properly");
        assert_eq!(state.registers.get_carry_flag(), 0, "Set carry flag");
        assert_eq!(state.registers.get_zero_flag(), 0, "Set zero flag");
        assert_eq!(state.registers.get_half_carry_flag(), 0, "Halfcarry flag should never be set");

        state.registers.a = 0b1001_0001; //DAA 91
        state.registers.b = 0b0001_0000;//DAA 10
        state.add_register_to_a(0);
        state.decimal_adjust_accumulator();
        assert_eq!(state.registers.get_carry_flag(), 1, "Did not set carry flag");
        assert_eq!(state.registers.get_zero_flag(), 0, "Set zero flag");
        assert_eq!(state.registers.get_half_carry_flag(), 0, "Halfcarry flag should never be set");

        state.registers.a = 0b000_0001; //DAA 1
        state.registers.b = 0b00_0001;//DAA 1
        state.sub_register_to_a(0);
        state.decimal_adjust_accumulator();
        assert_eq!(state.registers.get_carry_flag(), 0, "Set carry flag");
        assert_eq!(state.registers.get_zero_flag(), 1, "Did not set zero flag");
        assert_eq!(state.registers.get_half_carry_flag(), 0, "Halfcarry flag should never be set");

    }

    #[test]
    fn check_complement_accumulator() {
        let mut state = RunningState::new();
        state.registers.a = 0b0110_1001;
        state.registers.set_sub_flag(false);
        state.registers.set_half_carry_flag(false);
        state.complement_accumulator();
        assert_eq!(state.registers.a, 0b1001_0110, "Did not complement accumulator");
        assert_eq!(state.registers.get_sub_flag(), 1, "Did not set sub flag");
        assert_eq!(state.registers.get_half_carry_flag(), 1, "Did not set halfcarry flag");
    }

    #[test]
    fn check_increment_register_pair() {
        let mut state = RunningState::new();
        for i in 0..6 {
            if i % 2 == 1 {
                state.write_register(i, 0xFF);
            } else {
                state.write_register(i, 0);
            }
        }
        state.registers.sp = 0xF;
        state.increment_register_pair(RegisterPairs::BC);
        state.increment_register_pair(RegisterPairs::DE);
        state.increment_register_pair(RegisterPairs::HL);
        state.increment_register_pair(RegisterPairs::AF);
        assert_eq!(state.registers.b, 1, "Did not increment BC properly");
        assert_eq!(state.registers.c, 0, "Did not increment BC properly");
        assert_eq!(state.registers.d, 1, "Did not increment DE properly");
        assert_eq!(state.registers.e, 0, "Did not increment DE properly");
        assert_eq!(state.registers.h, 1, "Did not increment HL properly");
        assert_eq!(state.registers.l, 0, "Did not increment HL properly");
        assert_eq!(state.registers.sp, 0x10, "Did not increment SP properly");
        for i in 0..6 {
            state.write_register(i, 0xFF);
        }
        state.registers.sp = 0xFFFF;
        //Checks to see if overflow is allowed
        state.increment_register_pair(RegisterPairs::BC);
        state.increment_register_pair(RegisterPairs::DE);
        state.increment_register_pair(RegisterPairs::HL);
        state.increment_register_pair(RegisterPairs::AF);

    }
    
    #[test]
    fn check_decrement_register_pair() {
        let mut state = RunningState::new();
        for i in 0..6 {
            if i % 2 == 1 {
                state.write_register(i, 0x00);
            } else {
                state.write_register(i, 0xFF);
            }
        }
        state.registers.sp = 0xFF00;
        state.decrement_register_pair(RegisterPairs::BC);
        state.decrement_register_pair(RegisterPairs::DE);
        state.decrement_register_pair(RegisterPairs::HL);
        state.decrement_register_pair(RegisterPairs::AF);
        assert_eq!(state.registers.b, 0xFE, "Did not decrement BC properly");
        assert_eq!(state.registers.c, 0xFF, "Did not decrement BC properly");
        assert_eq!(state.registers.d, 0xFE, "Did not decrement DE properly");
        assert_eq!(state.registers.e, 0xFF, "Did not decrement DE properly");
        assert_eq!(state.registers.h, 0xFE, "Did not decrement HL properly");
        assert_eq!(state.registers.l, 0xFF, "Did not decrement HL properly");
        assert_eq!(state.registers.sp, 0xFEFF, "Did not decrement SP properly");
        for i in 0..6 {
            state.write_register(i, 0x0);
        }
        state.registers.sp = 0x0;
        //Checks to see if overflow is allowed
        state.decrement_register_pair(RegisterPairs::BC);
        state.decrement_register_pair(RegisterPairs::DE);
        state.decrement_register_pair(RegisterPairs::HL);
        state.decrement_register_pair(RegisterPairs::AF);

    }

    #[test]
    fn check_add_register_pair_to_hl() {
        let mut state = RunningState::new();
        state.registers.b = 0x1;
        state.registers.c = 0x2;
        state.registers.d = 0x3;
        state.registers.e = 0x4;
        state.registers.h = 0x5;
        state.registers.l = 0x6;
        state.registers.sp = 0x0708;
        state.add_register_pair_to_hl(RegisterPairs::BC);
        assert_eq!(state.registers.get_hl_value(), 0x0608, "Does not add BC correctly");
        assert_eq!(state.registers.get_sub_flag(), 0, "Sets sub flag");
        assert_eq!(state.registers.get_carry_flag(), 0, "Sets carry flag");
        assert_eq!(state.registers.get_half_carry_flag(), 0, "Sets halfcarry flag");
        state.add_register_pair_to_hl(RegisterPairs::DE);
        assert_eq!(state.registers.get_hl_value(), 0x090C, "Does not add DE correctly");
        assert_eq!(state.registers.get_sub_flag(), 0, "Sets sub flag");
        assert_eq!(state.registers.get_carry_flag(), 0, "Sets carry flag");
        assert_eq!(state.registers.get_half_carry_flag(), 0, "Sets halfcarry flag");
        state.add_register_pair_to_hl(RegisterPairs::HL);
        assert_eq!(state.registers.get_hl_value(), 0x1218, "Does not add HL correctly");
        assert_eq!(state.registers.get_sub_flag(), 0, "Sets sub flag");
        assert_eq!(state.registers.get_carry_flag(), 0, "Sets carry flag");
        assert_eq!(state.registers.get_half_carry_flag(), 1, "Does not set halfcarry flag");
        state.add_register_pair_to_hl(RegisterPairs::AF);
        assert_eq!(state.registers.get_hl_value(), 0x1920, "Does not add SP correctly");
        assert_eq!(state.registers.get_sub_flag(), 0, "Sets sub flag");
        assert_eq!(state.registers.get_carry_flag(), 0, "Sets carry flag");
        assert_eq!(state.registers.get_half_carry_flag(), 0, "Sets halfcarry flag");
        state.registers.sp = 0xFFFF;
        state.add_register_pair_to_hl(RegisterPairs::AF);
        assert_eq!(state.registers.get_hl_value(), 0x191F, "Does not add SP correctly");
        assert_eq!(state.registers.get_sub_flag(), 0, "Sets sub flag");
        assert_eq!(state.registers.get_carry_flag(), 1, "Does not set carry flag");
        assert_eq!(state.registers.get_half_carry_flag(), 1, "Does not set halfcarry flag");
    }

    #[test]
    fn check_increase_stack_pointer() {
        let mut state = RunningState::new();
        state.registers.sp = 0;
        state.registers.pc = 0x100;
        state.write_memory(0x100, 4);
        state.write_memory(0x101, 0b1111_1100);
        state.write_memory(0x102, 0b1111_1111);

        state.increase_stack_pointer();
        assert_eq!(state.registers.sp, 4, "Did not add correctly");
        assert_eq!(state.registers.get_carry_flag(), 0, "Set carry flag");
        assert_eq!(state.registers.get_half_carry_flag(), 0, "Set halfcarry flag");
        assert_eq!(state.registers.get_sub_flag(), 0, "Set sub flag");
        assert_eq!(state.registers.get_zero_flag(), 0, "Set zero flag");
        state.increase_stack_pointer();
        assert_eq!(state.registers.sp, 0, "Did not add correctly");
        assert_eq!(state.registers.get_carry_flag(), 0, "Set carry flag");
        assert_eq!(state.registers.get_half_carry_flag(), 0, "Set halfcarry flag");
        assert_eq!(state.registers.get_sub_flag(), 0, "Set sub flag");
        assert_eq!(state.registers.get_zero_flag(), 0, "Set zero flag");
        state.increase_stack_pointer();
        assert_eq!(state.registers.sp, 0xFFFF, "Did not add correctly");
        assert_eq!(state.registers.get_carry_flag(), 1, "Didn't set carry flag");
        assert_eq!(state.registers.get_half_carry_flag(), 1, "Didn't set halfcarry flag");
        assert_eq!(state.registers.get_sub_flag(), 0, "Set sub flag");
        assert_eq!(state.registers.get_zero_flag(), 0, "Set zero flag");

    }

    #[test]
    fn check_rotate_hl_data_left_circular() {
        let mut state = RunningState::new();
        state.registers.h = 0;
        state.registers.l = 0;
        state.write_memory(0, 0b10000000);
        state.registers.set_carry_flag(false);
        state.registers.set_half_carry_flag(true);
        state.registers.set_zero_flag(true);
        state.registers.set_sub_flag(true);

        state.rotate_hl_data_left_circular();
        assert_eq!(state.read_memory(0), 0b1, "Did not rotate correctly");
        assert_eq!(state.registers.get_carry_flag(), 1, "Did not set carry flag");
        assert_eq!(state.registers.get_half_carry_flag(), 0, "Set halfcarry flag");
        assert_eq!(state.registers.get_zero_flag(), 0, "Set zero flag");
        assert_eq!(state.registers.get_sub_flag(), 0, "Set sub flag");
        state.rotate_hl_data_left_circular();
        assert_eq!(state.read_memory(0), 0b10, "Did not rotate correctly");
        assert_eq!(state.registers.get_carry_flag(), 0, "Set carry flag");
        assert_eq!(state.registers.get_half_carry_flag(), 0, "Set halfcarry flag");
        assert_eq!(state.registers.get_zero_flag(), 0, "Set zero flag");
        assert_eq!(state.registers.get_sub_flag(), 0, "Set sub flag");
    }

    #[test]
    fn check_rotate_hl_data_right_circular() {
        let mut state = RunningState::new();
        state.registers.h = 0;
        state.registers.l = 0;
        state.write_memory(0, 0b1);
        state.registers.set_carry_flag(false);
        state.registers.set_half_carry_flag(true);
        state.registers.set_zero_flag(true);
        state.registers.set_sub_flag(true);

        state.rotate_hl_data_right_circular();
        assert_eq!(state.read_memory(0), 0b1000_0000, "Did not rotate correctly");
        assert_eq!(state.registers.get_carry_flag(), 1, "Did not set carry flag");
        assert_eq!(state.registers.get_half_carry_flag(), 0, "Set halfcarry flag");
        assert_eq!(state.registers.get_zero_flag(), 0, "Set zero flag");
        assert_eq!(state.registers.get_sub_flag(), 0, "Set sub flag");
        state.rotate_hl_data_right_circular();
        assert_eq!(state.read_memory(0), 0b0100_0000, "Did not rotate correctly");
        assert_eq!(state.registers.get_carry_flag(), 0, "Set carry flag");
        assert_eq!(state.registers.get_half_carry_flag(), 0, "Set halfcarry flag");
        assert_eq!(state.registers.get_zero_flag(), 0, "Set zero flag");
        assert_eq!(state.registers.get_sub_flag(), 0, "Set sub flag");
    }

    #[test]
    fn check_rotate_hl_data_left() {
        let mut state = RunningState::new();
        state.registers.h = 0;
        state.registers.l = 0;
        state.write_memory(0, 0b10000000);
        state.registers.set_carry_flag(false);
        state.registers.set_half_carry_flag(true);
        state.registers.set_zero_flag(true);
        state.registers.set_sub_flag(true);

        state.rotate_hl_data_left();
        assert_eq!(state.read_memory(0), 0b0, "Did not rotate correctly");
        assert_eq!(state.registers.get_carry_flag(), 1, "Did not set carry flag");
        assert_eq!(state.registers.get_half_carry_flag(), 0, "Set halfcarry flag");
        assert_eq!(state.registers.get_zero_flag(), 1, "Does not set zero flag");
        assert_eq!(state.registers.get_sub_flag(), 0, "Set sub flag");
        state.rotate_hl_data_left();
        assert_eq!(state.read_memory(0), 0b1, "Did not rotate correctly");
        assert_eq!(state.registers.get_carry_flag(), 0, "Set carry flag");
        assert_eq!(state.registers.get_half_carry_flag(), 0, "Set halfcarry flag");
        assert_eq!(state.registers.get_zero_flag(), 0, "Set zero flag");
        assert_eq!(state.registers.get_sub_flag(), 0, "Set sub flag");
    }

    #[test]
    fn check_rotate_hl_data_right() {
        let mut state = RunningState::new();
        state.registers.h = 0;
        state.registers.l = 0;
        state.write_memory(0, 0b1);
        state.registers.set_carry_flag(false);
        state.registers.set_half_carry_flag(true);
        state.registers.set_zero_flag(true);
        state.registers.set_sub_flag(true);

        state.rotate_hl_data_right();
        assert_eq!(state.read_memory(0), 0b0000_0000, "Did not rotate correctly");
        assert_eq!(state.registers.get_carry_flag(), 1, "Did not set carry flag");
        assert_eq!(state.registers.get_half_carry_flag(), 0, "Set halfcarry flag");
        assert_eq!(state.registers.get_zero_flag(), 1, "Does not set zero flag");
        assert_eq!(state.registers.get_sub_flag(), 0, "Set sub flag");
        state.rotate_hl_data_right();
        assert_eq!(state.read_memory(0), 0b1000_0000, "Did not rotate correctly");
        assert_eq!(state.registers.get_carry_flag(), 0, "Set carry flag");
        assert_eq!(state.registers.get_half_carry_flag(), 0, "Set halfcarry flag");
        assert_eq!(state.registers.get_zero_flag(), 0, "Set zero flag");
        assert_eq!(state.registers.get_sub_flag(), 0, "Set sub flag");
    }

    #[test]
    fn check_rotate_register_left_circular() {
        let mut state = RunningState::new();
        state.registers.set_carry_flag(false);
        state.registers.set_half_carry_flag(true);
        state.registers.set_zero_flag(true);
        state.registers.set_sub_flag(true);
        for i in 0..8 {
            if i != 6 {
                state.write_register(i, 0b10000000);
                state.rotate_register_left_circular(i);
                assert_eq!(state.read_register(i), 0b1, "Did not rotate correctly");
                assert_eq!(state.registers.get_carry_flag(), 1, "Did not set carry flag");
                assert_eq!(state.registers.get_half_carry_flag(), 0, "Set halfcarry flag");
                assert_eq!(state.registers.get_zero_flag(), 0, "Sets the zero flag");
                assert_eq!(state.registers.get_sub_flag(), 0, "Set sub flag");
                state.rotate_register_left_circular(i);
                assert_eq!(state.read_register(i), 0b10, "Did not rotate correctly");
                assert_eq!(state.registers.get_carry_flag(), 0, "Set carry flag");
                assert_eq!(state.registers.get_half_carry_flag(), 0, "Set halfcarry flag");
                assert_eq!(state.registers.get_zero_flag(), 0, "Sets the zero flag");
                assert_eq!(state.registers.get_sub_flag(), 0, "Set sub flag");
            }
        }
    }

    #[test]
    fn check_rotate_register_right_circular() {
        let mut state = RunningState::new();
        state.registers.set_carry_flag(false);
        state.registers.set_half_carry_flag(true);
        state.registers.set_zero_flag(true);
        state.registers.set_sub_flag(true);

        for i in 0..8 {
            if i != 6 {
                state.write_register(i, 0b1);
                state.rotate_register_right_circular(i);
                assert_eq!(state.read_register(i), 0b1000_0000, "Did not rotate correctly");
                assert_eq!(state.registers.get_carry_flag(), 1, "Did not set carry flag");
                assert_eq!(state.registers.get_half_carry_flag(), 0, "Set halfcarry flag");
                assert_eq!(state.registers.get_zero_flag(), 0, "Sets the zero flag");
                assert_eq!(state.registers.get_sub_flag(), 0, "Set sub flag");
                state.rotate_register_right_circular(i);
                assert_eq!(state.read_register(i), 0b0100_0000, "Did not rotate correctly");
                assert_eq!(state.registers.get_carry_flag(), 0, "Set carry flag");
                assert_eq!(state.registers.get_half_carry_flag(), 0, "Set halfcarry flag");
                assert_eq!(state.registers.get_zero_flag(), 0, "Sets the zero flag");
                assert_eq!(state.registers.get_sub_flag(), 0, "Set sub flag");
            }
        }
    }

    #[test]
    fn check_rotate_register_left() {
        let mut state = RunningState::new();
        state.registers.set_carry_flag(false);
        state.registers.set_half_carry_flag(true);
        state.registers.set_zero_flag(true);
        state.registers.set_sub_flag(true);
        for i in 0..8 {
            if i != 6 {
                state.write_register(i, 0b10000000);
                state.rotate_register_left(i);
                assert_eq!(state.read_register(i), 0b0, "Did not rotate correctly");
                assert_eq!(state.registers.get_carry_flag(), 1, "Did not set carry flag");
                assert_eq!(state.registers.get_half_carry_flag(), 0, "Set halfcarry flag");
                if i != 7 {
                    assert_eq!(state.registers.get_zero_flag(), 1, "Does not set zero flag");
                } else {
                    assert_eq!(state.registers.get_zero_flag(), 0, "Sets the zero flag");
                }
                assert_eq!(state.registers.get_sub_flag(), 0, "Set sub flag");
                state.rotate_register_left(i);
                assert_eq!(state.read_register(i), 0b1, "Did not rotate correctly");
                assert_eq!(state.registers.get_carry_flag(), 0, "Set carry flag");
                assert_eq!(state.registers.get_half_carry_flag(), 0, "Set halfcarry flag");
                assert_eq!(state.registers.get_zero_flag(), 0, "Sets the zero flag");
                assert_eq!(state.registers.get_sub_flag(), 0, "Set sub flag");
            }
        }
    }

    #[test]
    fn check_rotate_register_right() {
        let mut state = RunningState::new();
        state.registers.h = 0;
        state.registers.l = 0;
        state.registers.set_carry_flag(false);
        state.registers.set_half_carry_flag(true);
        state.registers.set_zero_flag(true);
        state.registers.set_sub_flag(true);
        for i in 0..8 {
            if i != 6 {
                state.write_register(i, 0b1);
                state.rotate_register_right(i);
                assert_eq!(state.read_register(i), 0b0000_0000, "Did not rotate correctly");
                assert_eq!(state.registers.get_carry_flag(), 1, "Did not set carry flag");
                assert_eq!(state.registers.get_half_carry_flag(), 0, "Set halfcarry flag");
                if i != 7 {
                    assert_eq!(state.registers.get_zero_flag(), 1, "Does not set zero flag");
                } else {
                    assert_eq!(state.registers.get_zero_flag(), 0, "Sets the zero flag");
                }
                assert_eq!(state.registers.get_sub_flag(), 0, "Set sub flag");
                state.rotate_register_right(i);
                assert_eq!(state.read_register(i), 0b1000_0000, "Did not rotate correctly");
                assert_eq!(state.registers.get_carry_flag(), 0, "Set carry flag");
                assert_eq!(state.registers.get_half_carry_flag(), 0, "Set halfcarry flag");
                assert_eq!(state.registers.get_zero_flag(), 0, "Sets the zero flag");
                assert_eq!(state.registers.get_sub_flag(), 0, "Set sub flag");
            }
        }
    }

    #[test]
    fn check_shift_hl_data_left() {
        let mut state = RunningState::new();
        state.registers.h = 0;
        state.registers.l = 0;
        state.write_memory(0, 0b1000_0000);
        state.registers.set_carry_flag(false);
        state.registers.set_zero_flag(false);
        state.registers.set_half_carry_flag(true);
        state.registers.set_sub_flag(true);
        state.shift_hl_data_left();
        assert_eq!(state.read_memory(0), 0, "Did not shift left properly");
        assert_eq!(state.registers.get_carry_flag(), 1, "Did not set carry flag");
        assert_eq!(state.registers.get_zero_flag(), 1, "Did not set zero flag");
        assert_eq!(state.registers.get_half_carry_flag(), 0, "Set halfcarry flag");
        assert_eq!(state.registers.get_sub_flag(), 0, "Set sub flag");
        state.write_memory(0, 0b0100_0000);
        state.shift_hl_data_left();
        assert_eq!(state.registers.get_carry_flag(), 0, "Set carry flag");
        assert_eq!(state.registers.get_zero_flag(), 0, "Set zero flag");        
    }

    #[test]
    fn check_shift_hl_data_right_log() {
        let mut state = RunningState::new();
        state.registers.h = 0;
        state.registers.l = 0;
        state.write_memory(0, 1);
        state.registers.set_carry_flag(false);
        state.registers.set_zero_flag(false);
        state.registers.set_half_carry_flag(true);
        state.registers.set_sub_flag(true);
        state.shift_hl_data_right_log();

        assert_eq!(state.read_memory(0), 0, "Did not shift right logically");
        assert_eq!(state.registers.get_carry_flag(), 1, "Did not set carry flag");
        assert_eq!(state.registers.get_zero_flag(), 1, "Did not set zero flag");
        assert_eq!(state.registers.get_half_carry_flag(), 0, "Set halfcarry flag");
        assert_eq!(state.registers.get_sub_flag(), 0, "Set sub flag");
        state.write_memory(0, 0b1000_0000);
        state.shift_hl_data_right_log();
        assert_eq!(state.read_memory(0), 0b0100_0000, "Did not shift right logically");
        assert_eq!(state.registers.get_carry_flag(), 0, "Set carry flag");
        assert_eq!(state.registers.get_zero_flag(), 0, "Set zero flag");   
    }

    #[test]
    fn check_shift_hl_data_right_ari() {
        let mut state = RunningState::new();
        state.registers.h = 0;
        state.registers.l = 0;
        state.write_memory(0, 1);
        state.registers.set_carry_flag(false);
        state.registers.set_zero_flag(false);
        state.registers.set_half_carry_flag(true);
        state.registers.set_sub_flag(true);
        state.shift_hl_data_right_ari();

        assert_eq!(state.read_memory(0), 0, "Did not shift right arithmetically");
        assert_eq!(state.registers.get_carry_flag(), 1, "Did not set carry flag");
        assert_eq!(state.registers.get_zero_flag(), 1, "Did not set zero flag");
        assert_eq!(state.registers.get_half_carry_flag(), 0, "Set halfcarry flag");
        assert_eq!(state.registers.get_sub_flag(), 0, "Set sub flag");
        state.write_memory(0, 0b1000_0000);
        state.shift_hl_data_right_ari();
        assert_eq!(state.read_memory(0), 0b1100_0000, "Did not shift right arithmetically");
        assert_eq!(state.registers.get_carry_flag(), 0, "Set carry flag");
        assert_eq!(state.registers.get_zero_flag(), 0, "Set zero flag");  
    }

    #[test]
    fn check_shift_register_left() {
        let mut state = RunningState::new();
        state.registers.set_carry_flag(false);
        state.registers.set_zero_flag(false);
        state.registers.set_half_carry_flag(true);
        state.registers.set_sub_flag(true);

        for i in 0..8 {
            if i != 6 {
                state.write_register(i, 0b1000_0000);
                state.shift_register_left(i);
                assert_eq!(state.read_register(i), 0, "Did not shift left properly");
                assert_eq!(state.registers.get_carry_flag(), 1, "Did not set carry flag");
                assert_eq!(state.registers.get_zero_flag(), 1, "Did not set zero flag");
                assert_eq!(state.registers.get_half_carry_flag(), 0, "Set halfcarry flag");
                assert_eq!(state.registers.get_sub_flag(), 0, "Set sub flag");
                state.write_register(i, 0b0100_0000);
                state.shift_register_left(i);
                assert_eq!(state.registers.get_carry_flag(), 0, "Set carry flag");
                assert_eq!(state.registers.get_zero_flag(), 0, "Set zero flag");
            }
        }
    }

    #[test]
    fn check_shift_register_right_log() {
        let mut state = RunningState::new();
        state.registers.set_carry_flag(false);
        state.registers.set_zero_flag(false);
        state.registers.set_half_carry_flag(true);
        state.registers.set_sub_flag(true);

        for i in 0..8 {
            if i != 6 {
                state.write_register(i, 1);
                state.shift_register_right_log(i);
                assert_eq!(state.read_register(i), 0, "Did not shift right logically");
                assert_eq!(state.registers.get_carry_flag(), 1, "Did not set carry flag");
                assert_eq!(state.registers.get_zero_flag(), 1, "Did not set zero flag");
                assert_eq!(state.registers.get_half_carry_flag(), 0, "Set halfcarry flag");
                assert_eq!(state.registers.get_sub_flag(), 0, "Set sub flag");
                state.write_register(i, 0b1000_0000);
                state.shift_register_right_log(i);
                assert_eq!(state.read_register(i), 0b0100_0000, "Did not shift right logically");
                assert_eq!(state.registers.get_carry_flag(), 0, "Set carry flag");
                assert_eq!(state.registers.get_zero_flag(), 0, "Set zero flag");
            }
        }
    }

    #[test]
    fn check_shift_register_right_ari() {
        let mut state = RunningState::new();
        state.registers.set_carry_flag(false);
        state.registers.set_zero_flag(false);
        state.registers.set_half_carry_flag(true);
        state.registers.set_sub_flag(true);

        for i in 0..8 {
            if i != 6 {
                state.write_register(i, 1);
                state.shift_register_right_ari(i);
                assert_eq!(state.read_register(i), 0, "Did not shift right arithmetically");
                assert_eq!(state.registers.get_carry_flag(), 1, "Did not set carry flag");
                assert_eq!(state.registers.get_zero_flag(), 1, "Did not set zero flag");
                assert_eq!(state.registers.get_half_carry_flag(), 0, "Set halfcarry flag");
                assert_eq!(state.registers.get_sub_flag(), 0, "Set sub flag");
                state.write_register(i, 0b1000_0000);
                state.shift_register_right_ari(i);
                assert_eq!(state.read_register(i), 0b1100_0000, "Did not shift right arithmetically");
                assert_eq!(state.registers.get_carry_flag(), 0, "Set carry flag");
                assert_eq!(state.registers.get_zero_flag(), 0, "Set zero flag");  
            }
        }
    }

    #[test]
    fn check_swap_hl_data_nibble() {
        let mut state = RunningState::new();
        state.registers.h = 0;
        state.registers.l = 0;
        state.write_memory(0, 0x4d);
        state.registers.set_carry_flag(true);
        state.registers.set_half_carry_flag(true);
        state.registers.set_zero_flag(true);
        state.registers.set_sub_flag(true);

        state.swap_hl_data_nibble();
        assert_eq!(state.read_memory(0), 0xd4, "Did not swap nibbles properly");
        assert_eq!(state.registers.get_carry_flag(), 0, "Set carry flag");
        assert_eq!(state.registers.get_half_carry_flag(), 0, "Set halfcarry flag");
        assert_eq!(state.registers.get_zero_flag(), 0, "Set zero flag");
        assert_eq!(state.registers.get_sub_flag(), 0, "Set sub flag");

        state.write_memory(0, 0);
        state.swap_hl_data_nibble();
        assert_eq!(state.registers.get_zero_flag(), 1, "Did not set zero flag");
    }

    #[test]
    fn check_swap_register_nibble() {
        let mut state = RunningState::new();
        state.registers.set_carry_flag(true);
        state.registers.set_half_carry_flag(true);
        state.registers.set_zero_flag(true);
        state.registers.set_sub_flag(true);

        for i in 0..8 {
            if i != 6 {
                state.write_register(i, 0x4d);
                state.swap_register_nibble(i);
                assert_eq!(state.read_register(i), 0xd4, "Did not swap nibbles properly");
                assert_eq!(state.registers.get_carry_flag(), 0, "Set carry flag");
                assert_eq!(state.registers.get_half_carry_flag(), 0, "Set halfcarry flag");
                assert_eq!(state.registers.get_zero_flag(), 0, "Set zero flag");
                assert_eq!(state.registers.get_sub_flag(), 0, "Set sub flag");
        
                state.write_register(i, 0);
                state.swap_register_nibble(i);
                assert_eq!(state.registers.get_zero_flag(), 1, "Did not set zero flag");
            }
        }
    }

    #[test]
    fn check_test_hl_data_bit() {
        let mut state = RunningState::new();
        state.registers.h = 0;
        state.registers.l = 0;
        state.registers.set_sub_flag(true);
        state.registers.set_half_carry_flag(false);
        state.write_memory(0, 0b01010101);
        for i in 0..8 {
            state.test_hl_data_bit(i);
            assert_eq!(state.registers.get_zero_flag(), i % 2, "Did not test bit {i} correctly");
            assert_eq!(state.registers.get_half_carry_flag(), 1, "Did not set halfcarry flag");
            assert_eq!(state.registers.get_sub_flag(), 0, "Set sub flag");
        }
    }

    #[test]
    fn check_set_hl_data_bit() {
        let mut state = RunningState::new();
        state.registers.h = 0;
        state.registers.l = 0;
        state.write_memory(0, 0);
        for i in 0..8 {
            state.set_hl_data_bit(i);
            assert_eq!((state.read_memory(0) >> i) & 1, 1, "Did not set bit {i} correctly");
        }
    }

    #[test]
    fn check_reset_hl_data_bit() {
        let mut state = RunningState::new();
        state.registers.h = 0;
        state.registers.l = 0;
        state.write_memory(0, 0xFF);
        for i in 0..8 {
            state.reset_hl_data_bit(i);
            assert_eq!((state.read_memory(0) >> i) & 1, 0, "Did not reset bit {i} correctly");
        }
    }

    #[test]
    fn check_test_register_bit() {
        let mut state = RunningState::new();
        state.registers.set_sub_flag(true);
        state.registers.set_half_carry_flag(false);
        for i in 0..8 {
            if i != 6 {
                state.write_register(i, 0b01010101);
                for j in 0..8 {
                    state.test_register_bit(j, i);
                    assert_eq!(state.registers.get_zero_flag(), j % 2, "Did not test bit {j} correctly");
                    assert_eq!(state.registers.get_half_carry_flag(), 1, "Did not set halfcarry flag");
                    assert_eq!(state.registers.get_sub_flag(), 0, "Set sub flag");
                }    
            }
        }
    }

    #[test]
    fn check_set_register_bit() {
        let mut state = RunningState::new();
        for i in 0..8 {
            if i != 6 {
                state.write_register(i, 0);
                for j in 0..8 {
                    state.set_register_bit(j, i);
                    assert_eq!((state.read_register(i) >> j) & 1, 1, "Did not set bit {j} correctly");
        
                }
            }
        }
    }

    #[test]
    fn check_reset_register_bit() {
        let mut state = RunningState::new();
        state.registers.h = 0;
        state.registers.l = 0;
        state.write_memory(0, 0xFF);
        for i in 0..8 {
            if i != 6 {
                for j in 0..8 {
                    state.reset_register_bit(j, i);
                    assert_eq!((state.read_register(i) >> j) & 1, 0, "Did not reset bit {j} correctly");        
                }
            }
        }
    }

    #[test]
    fn check_perform_cb_action() {
        let mut state = RunningState::new();
        state.write_memory(0x100, 0xc7);
        state.registers.a = 0;
        state.perform_cb_action();
        assert_eq!(state.registers.a, 1, "Did not perform SET 0, A");
    }

    #[test]
    fn check_jump_to_hl_data() {
        let mut state = RunningState::new();
        state.registers.h = 0;
        state.registers.l = 0;
        let sp = state.registers.sp;
        state.jump_to_hl_data();
        assert_eq!(state.registers.pc, 0, "Did not set PC to HL");
        assert!(state.registers.sp == sp, "Changed stack pointer");
    }

    #[test]
    fn check_jump_to_immediate() {
        let mut state = RunningState::new();
        state.write_memory(0x100, 0x45);
        state.write_memory(0x101, 0xA2);
        let sp = state.registers.sp;
        state.jump_to_immediate();
        assert_eq!(state.registers.pc, 0xA245, "Did not set PC to immediate");
        assert!(state.registers.sp == sp, "Changed stack pointer");
    }

    #[test]
    fn check_jump_cond_immediate() {
        let mut state = RunningState::new();
        state.registers.pc = 0x100;
        state.write_memory(0xA245, 0x45);
        state.write_memory(0xA246, 0xA2);        
        state.write_memory(0x100, 0x45);
        state.write_memory(0x101, 0xA2);
        state.write_memory(0xA247, 0x45);
        state.write_memory(0xA248, 0xA2);
        let sp = state.registers.sp;
        for condition in [Cond::C, Cond::NC, Cond::Z, Cond::NZ] {
            for truism in [true, false] {
                match condition {
                    Cond::C => state.registers.set_carry_flag(truism),
                    Cond::NC => state.registers.set_carry_flag(!truism),
                    Cond::Z => state.registers.set_zero_flag(truism),
                    Cond::NZ => state.registers.set_zero_flag(!truism)
                }
                state.jump_cond_immediate(condition);
                if truism {
                    match condition {
                        Cond::C => assert_eq!(state.registers.pc, 0xA245, "Did not set PC to immediate"),
                        Cond::NC => assert_eq!(state.registers.pc, 0xA245, "Did not set PC to immediate"),
                        Cond::Z => assert_eq!(state.registers.pc, 0xA245, "Did not set PC to immediate"),
                        Cond::NZ => assert_eq!(state.registers.pc, 0xA245, "Did not set PC to immediate"),
                    }
    
                } else {
                    match condition {
                        Cond::C => assert_eq!(state.registers.pc, 0xA247, "Set PC to immediate"),
                        Cond::NC => assert_eq!(state.registers.pc, 0xA247, "Set PC to immediate"),
                        Cond::Z => assert_eq!(state.registers.pc, 0xA247, "Set PC to immediate"),
                        Cond::NZ => assert_eq!(state.registers.pc, 0xA247, "Set PC to immediate"),
                    }
                }
                assert!(state.registers.sp == sp, "Changed stack pointer");
            }
        }

    }

    #[test]
    fn check_jump_rel_immediate() {
        let mut state = RunningState::new();
        state.write_memory(0x100, 4);
        state.jump_rel_immediate();
        assert_eq!(state.registers.pc, 0x105, "Did not jump forward properly");
        state.write_memory(0x105, 0b1111_1000);
        state.jump_rel_immediate();
        assert_eq!(state.registers.pc, 0xFE, "Did not jump backward properly");
    }

    #[test]
    fn check_jump_rel_cond_immediate() {
        let mut state = RunningState::new();
        state.write_memory(0x100, 5);
        state.write_memory(0x106, 0b1111_0100);
        state.write_memory(0x107, 0b1111_0011);
        state.write_memory(0xFB, 10);
        state.write_memory(0xFC, 9);
        let sp = state.registers.sp;
        for condition in [Cond::C, Cond::NC, Cond::Z, Cond::NZ] {
            for truism in [true, false] {
                match condition {
                    Cond::C => state.registers.set_carry_flag(truism),
                    Cond::NC => state.registers.set_carry_flag(!truism),
                    Cond::Z => state.registers.set_zero_flag(truism),
                    Cond::NZ => state.registers.set_zero_flag(!truism)
                }
                state.jump_rel_cond_immediate(condition);
                if truism {
                    match condition {
                        Cond::C => assert_eq!(state.registers.pc, 0x106, "Did not set PC to immediate"),
                        Cond::NC => assert_eq!(state.registers.pc, 0xFB, "Did not set PC to immediate"),
                        Cond::Z => assert_eq!(state.registers.pc, 0x106, "Did not set PC to immediate"),
                        Cond::NZ => assert_eq!(state.registers.pc, 0xFB, "Did not set PC to immediate"),
                    }
    
                } else {
                    match condition {
                        Cond::C => assert_eq!(state.registers.pc, 0x107, "Set PC to immediate"),
                        Cond::NC => assert_eq!(state.registers.pc, 0xFC, "Set PC to immediate"),
                        Cond::Z => assert_eq!(state.registers.pc, 0x107, "Set PC to immediate"),
                        Cond::NZ => assert_eq!(state.registers.pc, 0xFC, "Set PC to immediate"),
                    }
                }
                assert!(state.registers.sp == sp, "Changed stack pointer");
            }
        }
    }

    #[test]
    fn check_call_immediate() {
        let mut state = RunningState::new();
        state.write_memory(0x100, 0x26);
        state.write_memory(0x101, 0xDF);
        let sp = state.registers.sp;
        state.call_immediate();
        assert_eq!(state.registers.pc, 0xDF26, "Did not jump to function call");
        assert_eq!(state.registers.sp, sp-2, "Stack did not increase by 2");
        assert_eq!(state.read_memory(sp-1), 0x1, "Did not save PC correctly");
        assert_eq!(state.read_memory(sp-2), 0x2, "Did not save PC correctly");
    }

    #[test]
    fn check_ret() {
        let mut state = RunningState::new();
        state.write_memory(0x100, 0x15);
        state.write_memory(0x100, 0xBA);
        let sp = state.registers.sp;
        state.call_immediate();
        state.ret();
        assert_eq!(state.registers.pc, 0x102, "Did not return to correct area");
        assert_eq!(sp, state.registers.sp, "Did not pop stack pointer");
    }

    #[test]
    fn check_call_cond_immediate() {
        let mut state = RunningState::new();
        state.write_memory(0x100, 0x00);
        state.write_memory(0x101, 0x50);
        state.write_memory(0x102, 0x00);
        state.write_memory(0x103, 0x50);
        state.write_memory(0x104, 0x00);
        state.write_memory(0x105, 0x50);
        state.write_memory(0x106, 0x00);
        state.write_memory(0x107, 0x50);
        state.write_memory(0x108, 0x00);
        state.write_memory(0x109, 0x50);
        state.write_memory(0x10A, 0x00);
        state.write_memory(0x10B, 0x50);
        state.write_memory(0x10C, 0x00);
        state.write_memory(0x10D, 0x50);
        state.write_memory(0x10E, 0x00);
        state.write_memory(0x10F, 0x50);
        for condition in [Cond::C, Cond::NC, Cond::Z, Cond::NZ] {
            for truism in [true, false] {
                match condition {
                    Cond::C => state.registers.set_carry_flag(truism),
                    Cond::NC => state.registers.set_carry_flag(!truism),
                    Cond::Z => state.registers.set_zero_flag(truism),
                    Cond::NZ => state.registers.set_zero_flag(!truism)
                }
                state.call_cond_immediate(condition);
                if truism {
                    assert_eq!(state.registers.pc, 0x5000, "Did not set PC to immediate");
                    state.ret();
                } else {
                    match condition {
                        Cond::C => assert_eq!(state.registers.pc, 0x104, "Set PC to immediate"),
                        Cond::NC => assert_eq!(state.registers.pc, 0x108, "Set PC to immediate"),
                        Cond::Z => assert_eq!(state.registers.pc, 0x10C, "Set PC to immediate"),
                        Cond::NZ => assert_eq!(state.registers.pc, 0x110, "Set PC to immediate"),
                    };
                }
            }
        }
    }

    #[test]
    fn check_ret_cond() {
        let mut state = RunningState::new();
        state.write_memory(0x100, 0x00);
        state.write_memory(0x101, 0x50);
        state.write_memory(0x102, 0x00);
        state.write_memory(0x103, 0x50);
        state.write_memory(0x104, 0x00);
        state.write_memory(0x105, 0x50);
        state.write_memory(0x106, 0x00);
        state.write_memory(0x107, 0x50);
        state.write_memory(0x108, 0x00);
        state.write_memory(0x109, 0x50);
        state.call_immediate();
        for condition in [Cond::C, Cond::NC, Cond::Z, Cond::NZ] {
            for truism in [true, false] {
                match condition {
                    Cond::C => state.registers.set_carry_flag(truism),
                    Cond::NC => state.registers.set_carry_flag(!truism),
                    Cond::Z => state.registers.set_zero_flag(truism),
                    Cond::NZ => state.registers.set_zero_flag(!truism)
                }
                state.ret_cond(condition);
                if truism {
                    match condition {
                        Cond::C => assert_eq!(state.registers.pc, 0x102, "Did not set PC to immediate"),
                        Cond::NC => assert_eq!(state.registers.pc, 0x104, "Did not set PC to immediate"),
                        Cond::Z => assert_eq!(state.registers.pc, 0x106, "Did not set PC to immediate"),
                        Cond::NZ => assert_eq!(state.registers.pc, 0x108, "Did not set PC to immediate"),
                    };
                    state.call_immediate();
                } else {
                    assert_eq!(state.registers.pc, 0x5000, "Set PC to immediate");
                }
            }
        }
    }

    #[test]
    fn check_ret_interrupt_handler() {
        let mut state = RunningState::new();
        state.interrupts = false;
        state.call_immediate();
        state.ret_interrupt_handler();
        assert_eq!(state.registers.pc, 0x102, "Did not go back to correct location");
        assert_eq!(state.interrupts, true, "Did not turn on interrupts");
    }

    #[test]
    fn check_global_interrupt_handler() {
        let mut state = RunningState::new();
        state.ei();
        assert!(state.interrupts, "Did not turn on interrupts");
        state.di();
        assert!(!state.interrupts, "Did not turn off interrupts");
    }

    #[test]
    fn check_restart() {
        let mut state = RunningState::new();
        for i in 0..8u8 {
            state.restart(i);
            let x = ((i >> 1) as u16) << 4;
            let y;
            if i % 2 == 1 {
                y = 8;
            } else {
                y = 0;
            }
            assert_eq!(state.registers.pc, x + y, "Did not jump to correct location");
            state.ret();
            assert_eq!(state.registers.pc, 0x100, "Did not save PC correctly");
        }
    }
}

