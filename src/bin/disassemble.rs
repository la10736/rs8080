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
    C,
    D,
    E,
    H,
    L,
    M,
    Psw,
}

#[derive(PartialOrd, PartialEq, Debug, Copy, Clone)]
enum RegPair {
    BC,
    DE,
    HL,
    SP
}

impl std::fmt::Display for Reg {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}",
            match *self {
                A => "A",
                B => "B",
                C => "C",
                D => "D",
                E => "E",
                H => "H",
                L => "L",
                M => "M",
                Psw => "PSW",
            }
        )
    }
}

impl std::fmt::Display for RegPair {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}",
               match *self {
                   BC => "BC",
                   DE => "DE",
                   HL => "HL",
                   SP => "SP",
               }
        )
    }
}

use Reg::*;
use RegPair::*;
use Opcode::*;

#[derive(PartialOrd, PartialEq, Debug, Copy, Clone)]
enum Opcode {
    Nop,
    Lxi(RegPair, u16),
    Inx(RegPair),
    Stax(RegPair),
    Inr(Reg),
    Dcr(Reg),
    Jump(u16),
    Sta(u16),
    Push(Reg),
    Mvi(Reg, u8)
}

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
            0x01 => Lxi(BC, self.u16_data()?),
            0x02 => Stax(BC),
            0x03 => Inx(BC),
            0x04 => Inr(B),
            0x05 => Dcr(B),
            0x0c => Inr(C),
            0x0d => Dcr(C),
            0x11 => Lxi(DE, self.u16_data()?),
            0x12 => Stax(DE),
            0x13 => Inx(DE),
            0x14 => Inr(D),
            0x15 => Dcr(D),
            0x1c => Inr(E),
            0x1d => Dcr(E),
            0x21 => Lxi(HL, self.u16_data()?),
            0x23 => Inx(HL),
            0x24 => Inr(H),
            0x25 => Dcr(H),
            0x2c => Inr(L),
            0x2d => Dcr(L),
            0x31 => Lxi(SP, self.u16_data()?),
            0x32 => Sta(self.u16_data()?),
            0x33 => Inx(SP),
            0x34 => Inr(M),
            0x35 => Dcr(M),
            0x3c => Inr(A),
            0x3d => Dcr(A),
            0x3e => Mvi(A, self.u8_data()?),
            0xc3 => Jump(self.u16_data()?),
            0xc5 => Push(B),
            0xd5 => Push(D),
            0xe5 => Push(H),
            0xf5 => Push(Psw),
            c => {eprint!("Not implemented yet '{:02x}' opcode", c); Nop}
        }
        )
    }

    fn u16_data(&mut self) -> Option<u16> {
        Some((self.code_iterator.next()? as u16) | ((self.code_iterator.next()? as u16) << 8))
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
            Lxi(BC, _) => 0x01,
            Lxi(DE, _) => 0x11,
            Lxi(HL, _) => 0x21,
            Lxi(SP, _) => 0x31,
            Stax(BC) => 0x02,
            Stax(DE) => 0x12,
            Inx(BC) => 0x03,
            Inx(DE) => 0x13,
            Inx(HL) => 0x23,
            Inx(SP) => 0x33,
            Inr(B) => 0x04,
            Inr(C) => 0x0c,
            Inr(D) => 0x14,
            Inr(E) => 0x1c,
            Inr(H) => 0x24,
            Inr(L) => 0x2c,
            Inr(M) => 0x34,
            Inr(A) => 0x3c,
            Dcr(B) => 0x05,
            Dcr(C) => 0x0d,
            Dcr(D) => 0x15,
            Dcr(E) => 0x1d,
            Dcr(H) => 0x25,
            Dcr(L) => 0x2d,
            Dcr(M) => 0x35,
            Dcr(A) => 0x3d,
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
            Push(_) => panic!("Invalid syntax!"),
            Mvi(_, _) => panic!("Invalid syntax!"),
            Stax(_) => panic!("Invalid syntax!"),
            Inr(_) => panic!("Invalid syntax!"),
            Dcr(_) => panic!("Invalid syntax!"),
        }
    }

    fn length(&self) -> u16 {
        match *self {
            Jump(_) => 3,
            Sta(_) => 3,
            Lxi(_, _) => 3,
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
            Lxi(SP, addr) => write!(f, "LXI    SP,${:04x}", addr),
            Lxi(rp, data) => write!(f, "LXI    {},#0x{:04x}", rp, data),
            Stax(rp) => write!(f, "STAX   {}", rp),
            Inx(rp) => write!(f,  "INX    {}", rp),
            Inr(r) => write!(f,  "INR    {}", r),
            Dcr(r) => write!(f,  "DCR    {}", r),
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
        case("01 a4 32", 0x01, 3, "LXI    BC,#0x32a4"),
        case("02", 0x02, 1, "STAX   BC"),
        case("03", 0x03, 1, "INX    BC"),
        case("04", 0x04, 1, "INR    B"),
        case("05", 0x05, 1, "DCR    B"),
        case("0c", 0x0c, 1, "INR    C"),
        case("0d", 0x0d, 1, "DCR    C"),
        case("11 13 aa", 0x11, 3, "LXI    DE,#0xaa13"),
        case("12", 0x12, 1, "STAX   DE"),
        case("13", 0x13, 1, "INX    DE"),
        case("14", 0x14, 1, "INR    D"),
        case("15", 0x15, 1, "DCR    D"),
        case("1c", 0x1c, 1, "INR    E"),
        case("1d", 0x1d, 1, "DCR    E"),
        case("21 af 1d", 0x21, 3, "LXI    HL,#0x1daf"),
        case("23", 0x23, 1, "INX    HL"),
        case("24", 0x24, 1, "INR    H"),
        case("25", 0x25, 1, "DCR    H"),
        case("2c", 0x2c, 1, "INR    L"),
        case("2d", 0x2d, 1, "DCR    L"),
        case("31 af d3", 0x31, 3, "LXI    SP,$d3af"),
        case("32 72 20", 0x32, 3, "STA    $2072"),
        case("33", 0x33, 1, "INX    SP"),
        case("34", 0x34, 1, "INR    M"),
        case("35", 0x35, 1, "DCR    M"),
        case("3c", 0x3c, 1, "INR    A"),
        case("3d", 0x3d, 1, "DCR    A"),
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