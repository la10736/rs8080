use ::{Byte, Address};
use ::cpu::Result;
use std::fmt::{self, Debug, Formatter};

#[derive(Clone)]
pub struct PlainMemory {
    pub bank: [u8; 0x10000]
}

impl Default for PlainMemory {
    fn default() -> Self {
        PlainMemory { bank: [0; 0x10000] }
    }
}

#[derive(PartialEq)]
pub enum MemoryError {
    Read(Address),
    Write(Address, Byte),
    Ref(Address),
}

impl Debug for MemoryError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        use self::MemoryError::*;
        match self {
            Write(a, v) => write!(f, "Write(0x{:04x}, 0x{:02x})", a, v),
            Read(a) => write!(f, "Read(0x{:04x})", a),
            Ref(a) => write!(f, "Ref(0x{:04x})", a),
        }
    }
}

pub trait Mmu {
    fn read_byte(&self, address: Address) -> Result<Byte>;

    fn write_byte(&mut self, address: Address, val: Byte) -> Result<()>;

    fn dump(&self) -> String;

    fn replace_byte(&mut self, address: Address, val: Byte) -> Result<Byte> {
        let old = self.read_byte(address)?;
        self.write_byte(address, val)?;
        Ok(old)
    }

    fn write<A: AsRef<[u8]>>(&mut self, address: Address, data: A) -> Result<()> {
        data.as_ref().iter().enumerate().map(
            |(off, v)|
                self.write_byte(address + (off as Address), *v)
        ).collect()
    }
}
