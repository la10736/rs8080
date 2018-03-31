#![feature(proc_macro)]
extern crate rs8080;

use std::io::Read;
use rs8080::{
    ToOpcode,
    asm::{Instruction, Instruction::*},
    disassemble::disassemble,
};

fn main() {
    let path = std::env::args().nth(1)
        .expect("At least one argument as path of code to disassemble");

    let mut code = Default::default();
    std::fs::File::open(path)
        .unwrap()
        .read_to_end(&mut code)
        .expect("Cannot read bytes data");

    let disassembled = disassemble(code).unwrap();

    println!("{}", code_formatter(&disassembled))
}

fn code_str(op: &Instruction) -> String {
    match *op {
        Jump(offset) => format!("{:02x} {:02x} {:02x}", op.opcode(), offset & 0xff, (offset >> 8) & 0xff),
        Sta(offset) => format!("{:02x} {:02x} {:02x}", op.opcode(), offset & 0xff, (offset >> 8) & 0xff),
        Mvi(_, data) => format!("{:02x} {:02x}", op.opcode(), data & 0xff),
        c => format!("{:02x}", c.opcode())
    }
}

fn code_formatter<C: AsRef<[Instruction]>>(code: C) -> String {
    let mut pos: u16 = 0;
    let mut output = String::new();
    for op in code.as_ref().iter() {
        output.push_str(&format!("   {:04x} {: <8} {}", pos, code_str(op), op));
        output.push('\n');
        pos += op.length()
    }

    return output;
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn should_read_some_nop() {
        let bytes = [0x00u8, 0x00u8, 0x00u8];

        assert_eq!(vec![Nop, Nop, Nop], disassemble(&bytes).unwrap());
    }

    #[test]
    fn should_dump_some_nop_codes() {
        let code = disassemble(&[0x00u8, 0x00u8, 0x00u8]).unwrap();

        assert_eq!(r#"
   0000 00       NOP
   0001 00       NOP
   0002 00       NOP
   "#.trim(), format!("{}", code_formatter(code)).trim())
    }
}