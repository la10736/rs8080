#![feature(proc_macro, match_default_bindings)]
#[cfg(test)]
extern crate rstest;

pub type Word = u8;
pub type DWord = u16;
pub type Address = u16;

pub trait ToOpcode {
    fn opcode(self) -> Word;
}

pub mod disassemble;
pub mod cpu;
pub mod asm;