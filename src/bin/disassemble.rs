#![feature(proc_macro)]

extern crate rstest;

use std::io::Read;

fn main() {
    let path = std::env::args().nth(1)
        .expect("At least one argument as path of code to disassemble");

    let mut code= Default::default();
    std::fs::File::open(path)
        .unwrap()
        .read_to_end(&mut code)
        .expect("Cannot read bytes data");

    let disassembled = disassemble(code);

    println!("{}", code_formatter(&disassembled))
}

#[derive(PartialOrd, PartialEq, Debug, Copy, Clone)]
enum Reg {
    A,
    B,
    D,
    H,
    Psw,
}

impl std::fmt::Display for Reg {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}",
            match *self {
                A => "A",
                B => "B",
                D => "D",
                H => "H",
                Psw => "PSW",
            }
        )
    }
}

#[derive(PartialOrd, PartialEq, Debug, Copy, Clone)]
enum Opcode {
    Nop,
    LxiBC((u8, u8)),
    LxiDE((u8, u8)),
    LxiHL((u8, u8)),
    LxiSP(u16),
    StaxBC,
    StaxDE,
    Jump(u16),
    Sta(u16),
    Push(Reg),
    Mvi(Reg, u8)
}

use Reg::*;
use Opcode::*;

struct CodeIterator<I: Iterator<Item=u8>> {
    code_iterator: I
}

impl CodeIterator<std::vec::IntoIter<u8>> {
    fn new(data: Vec<u8>) -> CodeIterator<std::vec::IntoIter<u8>> {
        CodeIterator {
            code_iterator: data.into_iter(),
        }
    }
}

impl<I: Iterator<Item=u8>> CodeIterator<I> {
    fn next_opcode(&mut self) -> Option<Opcode> {
        Some(match self.code_iterator.next()? {
            0x00 => Nop,
            0x01 => LxiBC(self.u8_x2_data()?),
            0x02 => StaxBC,
            0x11 => LxiDE(self.u8_x2_data()?),
            0x12 => StaxDE,
            0x21 => LxiHL(self.u8_x2_data()?),
            0x31 => LxiSP(self.u16_data()?),
            0x32 => Sta(self.u16_data()?),
            0x3e => Mvi(A, self.u8_data()?),
            0xc3 => Jump(self.u16_data()?),
            0xc5 => Push(B),
            0xd5 => Push(D),
            0xe5 => Push(H),
            0xf5 => Push(Psw),
            c => {println!("Not implemented yet '{:02x}' opcode", c); Nop}
        }
        )
    }

    fn u16_data(&mut self) -> Option<u16> {
        Some((self.code_iterator.next()? as u16) | ((self.code_iterator.next()? as u16) << 8))
    }

    fn u8_x2_data(&mut self) -> Option<(u8, u8)> {
        let a = self.code_iterator.next()?;
        let b = self.code_iterator.next()?;
        Some((b, a))
    }

    fn u8_data(&mut self) -> Option<u8> {
        self.code_iterator.next()
    }
}

impl<I: Iterator<Item=u8>> Iterator for CodeIterator<I>{
    type Item = Opcode;

    fn next(&mut self) -> Option<Self::Item> {
        self.next_opcode()
    }
}

fn disassemble<C: AsRef<[u8]>>(codes: C) -> Vec<Opcode> {
    CodeIterator::new(
        codes.as_ref().iter().cloned().collect()
    ).collect()
}

impl Opcode {
    fn code(&self) -> u8 {
        match *self {
            Nop => 0x00,
            LxiBC(_) => 0x01,
            LxiDE(_) => 0x11,
            LxiHL(_) => 0x21,
            LxiSP(_) => 0x31,
            StaxBC => 0x02,
            StaxDE => 0x12,
            Mvi(A, _) => 0x3e,
            Mvi(B, _) => 0x06,
            Mvi(D, _) => 0x16,
            Mvi(H, _) => 0x26,
            Sta(_) => 0x32,
            Jump(_) => 0xc3,
            Push(B) => 0xc5,
            Push(D) => 0xd5,
            Push(H) => 0xe5,
            Push(Psw) => 0xf5,
            Push(A) => panic!("Invalid syntax!"),
            Mvi(Psw, _) => panic!("Invalid syntax!"),
        }
    }

    fn length(&self) -> u16 {
        match *self {
            Jump(_) => 3,
            Sta(_) => 3,
            LxiBC(_) => 3,
            LxiDE(_) => 3,
            LxiHL(_) => 3,
            LxiSP(_) => 3,
            Mvi(_, _) => 2,
            _ => 1
        }
    }
}

fn code_str(op: &Opcode) -> String {
    match *op {
        Jump(offset) => format!("{:02x} {:02x} {:02x}", op.code(), offset & 0xff, (offset >> 8) & 0xff),
        Sta(offset) => format!("{:02x} {:02x} {:02x}", op.code(), offset & 0xff, (offset >> 8) & 0xff),
        Mvi(_, data) => format!("{:02x} {:02x}", op.code(), data & 0xff),
        c => format!("{:02x}", c.code())
    }
}

impl std::fmt::Display for Opcode {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match *self {
            Nop => write!(f, "NOP"),
            LxiBC((d0, d1)) => write!(f, "LXI    B,#0x{:02x} C,#0x{:02x}", d0, d1),
            LxiDE((d0, d1)) => write!(f, "LXI    D,#0x{:02x} E,#0x{:02x}", d0, d1),
            LxiHL((d0, d1)) => write!(f, "LXI    H,#0x{:02x} L,#0x{:02x}", d0, d1),
            LxiSP(addr) => write!(f, "LXI    SP,${:04x}", addr),
            StaxBC => write!(f, "STAX   BC"),
            StaxDE => write!(f, "STAX   DE"),
            Mvi(reg, data) => write!(f, "MVI    {},#0x{:02x}", reg, data),
            Sta(offset) => write!(f, "STA    ${:04x}", offset),
            Jump(offset) => write!(f, "JMP    ${:04x}", offset),
            Push(reg) => write!(f, "PUSH   {}", reg),
        }
    }
}

fn code_formatter<C: AsRef<[Opcode]>>(code: C) -> String {
    let mut pos: u16 = 0;
    let mut output = String::new();
    for op in code.as_ref().iter() {
        output.push_str(&format!("   {:04x} {: <8} {}", pos, code_str(op), op));
        output.push('\n');
        pos += op.length()
    }

    return output;
}

#[test]
fn should_read_some_nop() {
    let bytes = [0x00u8, 0x00u8, 0x00u8];

    assert_eq!(vec![Nop, Nop, Nop], disassemble(&bytes));
}

#[test]
fn should_dump_some_nop_codes() {
    let code = disassemble(&[0x00u8, 0x00u8, 0x00u8]);

    assert_eq!(r#"
   0000 00       NOP
   0001 00       NOP
   0002 00       NOP
   "#.trim(), format!("{}", code_formatter(code)).trim())
}

#[cfg(test)]
mod test {
    use super::*;

    use rstest::rstest_parametrize;

    fn parse_str_bytes<S: AsRef<str>>(s: S) -> Vec<u8> {
        s.as_ref().split(' ')
            .map(|v| u8::from_str_radix(v, 16).unwrap())
            .collect()
    }


    #[rstest_parametrize(
        bytes, code, length, desc,
        case("00", 0x00, 1, "NOP"),
        case("01 a4 32", 0x01, 3, "LXI    B,#0x32 C,#0xa4"),
        case("02 2f 12", 0x02, 1, "STAX   BC"),
        case("11 13 aa", 0x11, 3, "LXI    D,#0xaa E,#0x13"),
        case("12", 0x12, 1, "STAX   DE"),
        case("21 af 1d", 0x21, 3, "LXI    H,#0x1d L,#0xaf"),
        case("31 af d3", 0x31, 3, "LXI    SP,$d3af"),
        case("32 72 20", 0x32, 3, "STA    $2072"),
        case("3e 80", 0x3e, 2, "MVI    A,#0x80"),
        case("c3 d4 18", 0xc3, 3, "JMP    $18d4"),
        case("c5", 0xc5, 1, "PUSH   B"),
        case("d5", 0xd5, 1, "PUSH   D"),
        case("e5", 0xe5, 1, "PUSH   H"),
        case("f5", 0xf5, 1, "PUSH   PSW")
    )
    ]
    fn opcode_parser(bytes: &str, code: u8, length: u16, desc: &str) {
        let bytes = parse_str_bytes(bytes);

        let d = disassemble(&bytes)[0];

        assert_eq!(code, d.code());
        assert_eq!(length, d.length());
        assert_eq!(desc, &format!("{}", d));
    }
}