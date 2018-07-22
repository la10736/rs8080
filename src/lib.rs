#![feature(proc_macro)]
#[cfg(test)]
extern crate rstest;

pub type Byte = u8;
pub type Word = u16;
pub type Address = Word;

pub trait ToOpcode {
    fn opcode(self) -> Byte;
}

pub mod disassemble;
pub mod cpu;
pub mod asm;
pub mod io_bus;
