#[cfg(test)]
extern crate rstest;
#[macro_use]
extern crate log;
extern crate core;

pub type Byte = u8;
pub type Word = u16;
pub type Address = Word;

pub trait ToOpcode {
    fn opcode(self) -> Byte;
}

pub mod disassemble;
pub mod cpu;
pub mod flags;
pub mod asm;
pub mod registers;
pub mod mmu;
pub mod io_bus;
