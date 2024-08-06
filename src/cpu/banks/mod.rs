#[derive(Debug)]
pub enum MBC {
  NoMBC,
  MBC1,
  MBC2,
  MBC3,
  MBC4,
  MBC5,
  MBC6,
  MBC7
}

pub struct Banks {
  pub ram_enable: bool,
  banking_mode: u8,
  ram_bank: Bank,
  rom_bank: Bank,
  pub mbc: MBC,
}

pub struct Bank {
  pub available_banks: usize,
  pub current_bank_number: usize,
  pub banks: Vec<[u8; 0x4000]>,
}

impl Bank {
  pub fn new(length: usize) -> Bank {
    return Bank {
      available_banks: length,
      current_bank_number: 0,
      banks: vec![[0;0x4000]; length],
    }
  }

  pub fn set_current_bank_number(&mut self, bank_number: usize) {
    if bank_number > self.available_banks {
      panic!("Attempting to set overflowing bank number");
    }
    self.current_bank_number = bank_number;
  }

  pub fn get_current_bank_number(&mut self) -> usize {
    self.current_bank_number
  }

  pub fn write_bank(&mut self, position: usize, data: u8) {
    self.banks[self.current_bank_number][position] = data;
  }

  pub fn read_bank(&mut self, position: usize) -> u8 {
    self.banks[self.current_bank_number][position]
  }

  pub fn read_bank_0(&mut self, position: usize) -> u8 {
    self.banks[0][position]
  }


}

impl Banks {

  pub fn new(cart_type: u8, rom_size: u8, ram_size: u8) -> Banks {
    let ram_enable = false;
    let bank;
    let mbc = match cart_type {
      0 => MBC::NoMBC,
      1 => MBC::MBC1,
      2 => MBC::MBC1,
      3 => MBC::MBC1,
      5 => MBC::MBC2,
      6 => MBC::MBC2,
      0x0f => MBC::MBC3,
      0x10 => MBC::MBC3,
      0x11 => MBC::MBC3,
      0x12 => MBC::MBC3,
      0x13 => MBC::MBC3,
      0x19 => MBC::MBC5,
      0x1A => MBC::MBC5,
      0x1B => MBC::MBC5,
      0x1C => MBC::MBC5,
      0x1D => MBC::MBC5,
      0x1E => MBC::MBC5,
      0x20 => MBC::MBC6,
      0x22 => MBC::MBC7,
      _ => panic!("Unrecognized MBC type")
    };
    let bank2= 2*(1 << rom_size);
    if bank2 > 512 {
      panic!("Unsupported and Unknown ROM size");
    }
    match ram_size {
      0 => bank = 1,
      1 => {panic!("Unused ram type!")},
      2 => bank = 2,
      3 => bank = 5,
      4 => bank = 17,
      5 => bank = 9,
      _ => {panic!("Unsupported and Unknown RAM size")},
    }
    
    let mut bank = Banks {
      ram_enable,
      banking_mode: 0,
      ram_bank: Bank::new(bank),
      rom_bank: Bank::new(bank2),
      mbc
    };
    bank.rom_bank.current_bank_number = 1;
    bank
  }

  pub fn enable_ram(&mut self) {
    self.ram_enable = true;
  }

  pub fn set_banking_mode(&mut self, mode: u8) {
    self.banking_mode = mode;
  }

  pub fn set_rom_bank_lower(&mut self, bank: u8) -> bool {
    if self.banking_mode == 0 {
      return false;
    } 
    let new_bank = ((self.rom_bank.current_bank_number >> 5) << 5) + usize::from(bank);
    self.set_rom_bank(new_bank);
    return true;
  }

  pub fn set_rom_bank_upper_or_ram(&mut self, bank: u8) {
    if self.ram_bank.available_banks > 1 {
      self.set_ram_bank(usize::from(bank));
    } else if self.rom_bank.available_banks > 32 {
      let new_bank = (self.rom_bank.current_bank_number % 32) + usize::from(bank << 5);
      self.set_rom_bank(new_bank);
    }
  }

  pub fn set_rom(&mut self, bank: usize, data: &[u8]) {
    self.rom_bank.banks[bank].clone_from_slice(data);
  }

  pub fn read_rom(&mut self, position: usize) -> u8 {
    self.rom_bank.read_bank(position - 0x4000)
  }

  pub fn read_rom_0(&mut self, position: usize) -> u8 {
    self.rom_bank.read_bank_0(position)
  }

  pub fn write_ram(&mut self, position: usize, data: u8) {
    self.ram_bank.write_bank(position - 0xA000, data);
  }

  pub fn read_ram(&mut self, position: usize) -> u8 {
    self.ram_bank.read_bank(position - 0xA000)
  }

  pub fn read_ram_0(&mut self, position: usize) -> u8 {
    self.ram_bank.read_bank_0(position - 0xA000)
  }

  pub fn dump_rom_bank(&self, bank: usize) {
    println!("======ROM START======");
    for i in 0..0x400 {
      for j in 0..0x10 {
        print!("{} ", self.rom_bank.banks[bank][i*10+j]);
      }
      println!("");
    }
    println!("======ROM END======");
  }

  pub fn dump_ram_bank(&self, bank: usize) {
    println!("======RAM START======");
    for i in 0..0x200 {
      for j in 0..0x10 {
        print!("{} ", self.ram_bank.banks[bank][i*10+j]);
      }
      println!("");
    }
    println!("======RAM END======");
  }

  fn set_rom_bank(&mut self, bank: usize) {
    self.rom_bank.set_current_bank_number(bank);
  }

  fn set_ram_bank(&mut self, bank: usize) {
    self.ram_bank.set_current_bank_number(bank);
  }
  
}