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
}

impl From<u8> for Reg {
    fn from(v: u8) -> Self {
        use Reg::*;
        match v {
            0x00 => B,
            0x01 => C,
            0x02 => D,
            0x03 => E,
            0x04 => H,
            0x05 => L,
            0x06 => M,
            0x07 => A,
            invalid => panic!("Invalid reg opcode {}", invalid)
        }
    }
}

impl Reg {
    fn opcode(self) -> u8 {
        use Reg::*;
        match self {
            B => 0x0,
            C => 0x1,
            D => 0x2,
            E => 0x3,
            H => 0x4,
            L => 0x5,
            M => 0x6,
            A => 0x7,
         }
    }
}

#[derive(PartialOrd, PartialEq, Debug, Copy, Clone)]
enum RegPair {
    BC,
    DE,
    HL,
    SP
}

#[derive(PartialOrd, PartialEq, Debug, Copy, Clone)]
enum BytePair {
    BC,
    DE,
    HL,
    AF
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
                M => "(HL)",
            }
        )
    }
}

impl RegPair {
    fn opcode(self) -> u8 {
        use RegPair::*;
        match self {
            BC => 0x00,
            DE => 0x10,
            HL => 0x20,
            SP => 0x30,
        }
    }

    fn is_basic(self) -> bool {
        use RegPair::*;
        [BC, DE].contains(&self)
    }
}

impl std::fmt::Display for RegPair {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use RegPair::*;
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

impl BytePair {
    fn opcode(self) -> u8 {
        use BytePair::*;
        match self {
            BC => 0xc0,
            DE => 0xd0,
            HL => 0xe0,
            AF => 0xf0,
        }
    }
}

impl From<u8> for BytePair {
    fn from(v: u8) -> Self {
        use BytePair::*;
        match v {
            0xc0 => BC,
            0xd0 => DE,
            0xe0 => HL,
            0xf0 => AF,
            invalid => panic!("Invalid byte pair opcode {}", invalid)
        }
    }
}


impl std::fmt::Display for BytePair {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use BytePair::*;
        write!(f, "{}",
               match *self {
                   BC => "BC",
                   DE => "DE",
                   HL => "HL",
                   AF => "PSW",
               }
        )
    }
}

#[derive(PartialOrd, PartialEq, Debug, Copy, Clone)]
enum IrqAddr {
    I0,
    I1,
    I2,
    I3,
}

impl IrqAddr {
    fn irq_address(&self) -> u8 {
        use IrqAddr::*;
        match *self {
            I0 => 0x08,
            I1 => 0x10,
            I2 => 0x18,
            I3 => 0x20,
        }
    }
}


use Reg::*;
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
    JumpZ(u16),
    Cnz(u16),
    Sta(u16),
    Push(BytePair),
    Mvi(Reg, u8),
    Rlc,
    Rrc,
    Ral,
    Rar,
    Dad(RegPair),
    Ldax(RegPair),
    Dcx(RegPair),
    Rim,
    Daa,
    Cma,
    Sim,
    Cmc,
    Lhld(u16),
    Stc,
    Lda(u16),
    Mov(Reg, Reg),
    Add(Reg),
    Adc(Reg),
    Sub(Reg),
    Sbb(Reg),
    Ana(Reg),
    Xra(Reg),
    Ora(Reg),
    Cmp(Reg),
    Hlt,
    Rnz,
    Pop(BytePair),
    Adi(u8),
    Sui(u8),
    Ani(u8),
    Ori(u8),
    Rst(IrqAddr)
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
            0x01 => Lxi(RegPair::BC, self.u16_data()?),
            0x02 => Stax(RegPair::BC),
            0x03 => Inx(RegPair::BC),
            0x04 => Inr(B),
            0x05 => Dcr(B),
            0x06 => Mvi(B, self.u8_data()?),
            0x07 => Rlc,
            0x09 => Dad(RegPair::BC),
            0x0a => Ldax(RegPair::BC),
            0x0b => Dcx(RegPair::BC),
            0x0c => Inr(C),
            0x0d => Dcr(C),
            0x0e => Mvi(C, self.u8_data()?),
            0x0f => Rrc,
            0x11 => Lxi(RegPair::DE, self.u16_data()?),
            0x12 => Stax(RegPair::DE),
            0x13 => Inx(RegPair::DE),
            0x14 => Inr(D),
            0x15 => Dcr(D),
            0x16 => Mvi(D, self.u8_data()?),
            0x17 => Ral,
            0x19 => Dad(RegPair::DE),
            0x1a => Ldax(RegPair::DE),
            0x1b => Dcx(RegPair::DE),
            0x1c => Inr(E),
            0x1d => Dcr(E),
            0x1e => Mvi(E, self.u8_data()?),
            0x1f => Rar,
            0x20 => Rim,
            0x21 => Lxi(RegPair::HL, self.u16_data()?),
            0x23 => Inx(RegPair::HL),
            0x24 => Inr(H),
            0x25 => Dcr(H),
            0x26 => Mvi(H, self.u8_data()?),
            0x27 => Daa,
            0x29 => Dad(RegPair::HL),
            0x2a => Lhld(self.u16_data()?),
            0x2b => Dcx(RegPair::HL),
            0x2c => Inr(L),
            0x2d => Dcr(L),
            0x2e => Mvi(L, self.u8_data()?),
            0x2f => Cma,
            0x30 => Sim,
            0x31 => Lxi(RegPair::SP, self.u16_data()?),
            0x32 => Sta(self.u16_data()?),
            0x33 => Inx(RegPair::SP),
            0x34 => Inr(M),
            0x35 => Dcr(M),
            0x36 => Mvi(M, self.u8_data()?),
            0x37 => Stc,
            0x39 => Dad(RegPair::SP),
            0x3a => Lda(self.u16_data()?),
            0x3b => Dcx(RegPair::SP),
            0x3c => Inr(A),
            0x3d => Dcr(A),
            0x3e => Mvi(A, self.u8_data()?),
            0x3f => Cmc,
            0x76 => Hlt,
            v if v >= 0x40 && v <= 0x7f => Mov(((v-0x40)>>3).into(), (v & 0x07).into()),
            v if v >= 0x80 && v <= 0x87 => Add((v-0x80).into()),
            v if v >= 0x88 && v <= 0x8f => Adc((v-0x88).into()),
            v if v >= 0x90 && v <= 0x97 => Sub((v-0x90).into()),
            v if v >= 0x98 && v <= 0x9f => Sbb((v-0x98).into()),
            v if v >= 0xa0 && v <= 0xa7 => Ana((v-0xa0).into()),
            v if v >= 0xa8 && v <= 0xaf => Xra((v-0xa8).into()),
            v if v >= 0xb0 && v <= 0xb7 => Ora((v-0xb0).into()),
            v if v >= 0xb8 && v <= 0xbf => Cmp((v-0xb8).into()),
            0xc0 => Rnz,
            v if (v & 0xf0) >= 0xc0 && (v & 0x0f) == 0x1 => Pop((v & 0xf0).into()),
            0xc2 => JumpZ(self.u16_data()?),
            0xc3 => Jump(self.u16_data()?),
            0xc4 => Cnz(self.u16_data()?),
            v if (v & 0xf0) >= 0xc0 && (v & 0x0f) == 0x5 => Push((v & 0xf0).into()),
            0xc6 => Adi(self.u8_data()?),
            0xc7 => Rst(IrqAddr::I0),
            0xd6 => Sui(self.u8_data()?),
            0xd7 => Rst(IrqAddr::I1),
            0xe6 => Ani(self.u8_data()?),
            0xe7 => Rst(IrqAddr::I2),
            0xf6 => Ori(self.u8_data()?),
            0xf7 => Rst(IrqAddr::I3),
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
            Lxi(rp, _) => 0x01 | rp.opcode(),
            Stax(rp) if rp.is_basic() => 0x02 | rp.opcode(),
            Inx(rp) => 0x03 | rp.opcode(),
            Ldax(rp) if rp.is_basic() => 0x0a | rp.opcode(),
            Dcx(rp) => 0x0b | rp.opcode(),
            Inr(r) => 0x04 | r.opcode() << 3,
            Dcr(r) => 0x05 | r.opcode() << 3,
            Mvi(r, _) => 0x06 | r.opcode() << 3,
            Hlt => 0x76,
            Sta(_) => 0x32,
            JumpZ(_) => 0xc2,
            Jump(_) => 0xc3,
            Cnz(_) => 0xc4,
            Push(bp) => 0x05 | bp.opcode(),
            Adi(_) => 0xc6,
            Sui(_) => 0xd6,
            Ani(_) => 0xe6,
            Ori(_) => 0xf6,
            Rlc => 0x07,
            Rrc => 0x0f,
            Ral => 0x17,
            Rar => 0x1f,
            Rim => 0x20,
            Daa => 0x27,
            Cma => 0x2f,
            Sim => 0x30,
            Stc => 0x37,
            Cmc => 0x3f,
            Lhld(_) => 0x2a,
            Lda(_) => 0x3a,
            Dad(rp) => 0x09 | rp.opcode(),
            Mov(r0, r1) => (0x40 + (r0.opcode() << 3)) | r1.opcode(),
            Add(r) => 0x80 + r.opcode(),
            Adc(r) => 0x88 + r.opcode(),
            Sub(r) => 0x90 + r.opcode(),
            Sbb(r) => 0x98 + r.opcode(),
            Ana(r) => 0xa0 + r.opcode(),
            Xra(r) => 0xa8 + r.opcode(),
            Ora(r) => 0xb0 + r.opcode(),
            Cmp(r) => 0xb8 + r.opcode(),
            Rnz => 0xc0,
            Pop(bp) => 0x01 | bp.opcode(),
            Rst(IrqAddr::I0) => 0xc7,
            Rst(IrqAddr::I1) => 0xd7,
            Rst(IrqAddr::I2) => 0xe7,
            Rst(IrqAddr::I3) => 0xf7,
            Ldax(_) => panic!("Invalid syntax!"),
            Stax(_) => panic!("Invalid syntax!"),
        }
    }

    fn length(&self) -> u16 {
        match *self {
            Jump(_) | JumpZ(_) | Cnz(_) | Sta(_) |
            Lda(_) | Lhld(_) | Lxi(_, _) => 3,
            Mvi(_, _) | Adi(_) | Sui(_) | Ani(_) | Ori(_) => 2,
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
            Lxi(RegPair::SP, addr) => write!(f, "LXI    SP,${:04x}", addr),
            Lxi(rp, data) => write!(f, "LXI    {},#0x{:04x}", rp, data),
            Ldax(rp) => write!(f, "LDAX   {}", rp),
            Stax(rp) => write!(f, "STAX   {}", rp),
            Inx(rp) => write!(f,  "INX    {}", rp),
            Inr(r) => write!(f,  "INR    {}", r),
            Dcr(r) => write!(f,  "DCR    {}", r),
            Mvi(reg, data) => write!(f, "MVI    {},#0x{:02x}", reg, data),
            Sta(offset) => write!(f, "STA    ${:04x}", offset),
            JumpZ(offset) => write!(f, "JNZ    ${:04x}", offset),
            Jump(offset) => write!(f, "JMP    ${:04x}", offset),
            Cnz(offset) => write!(f, "CNZ    ${:04x}", offset),
            Lda(addr) => write!(f, "LDA    ${:04x}", addr),
            Lhld(addr) => write!(f, "LHLD   ${:04x}", addr),
            Push(reg) => write!(f, "PUSH   {}", reg),
            Pop(reg) => write!(f, "POP    {}", reg),
            Adi(data) => write!(f, "ADI    #0x{:02x}", data),
            Sui(data) => write!(f, "SUI    #0x{:02x}", data),
            Ani(data) => write!(f, "ANI    #0x{:02x}", data),
            Ori(data) => write!(f, "ORI    #0x{:02x}", data),
            Rlc => write!(f, "RLC"),
            Rrc => write!(f, "RRC"),
            Ral => write!(f, "RAL"),
            Rar => write!(f, "RAR"),
            Rim => write!(f, "RIM"),
            Daa => write!(f, "DAA"),
            Cma => write!(f, "CMA"),
            Sim => write!(f, "SIM"),
            Stc => write!(f, "STC"),
            Cmc => write!(f, "CMC"),
            Hlt => write!(f, "HLT"),
            Dad(rp) => write!(f, "DAD    {}", rp),
            Dcx(rp) => write!(f, "DCX    {}", rp),
            Mov(r0, r1) => write!(f, "MOV    {} <- {}", r0, r1),
            Add(r) => write!(f, "ADD    {}", r),
            Adc(r) => write!(f, "ADC    {}", r),
            Sub(r) => write!(f, "SUB    {}", r),
            Sbb(r) => write!(f, "SBB    {}", r),
            Ana(r) => write!(f, "ANA    {}", r),
            Xra(r) => write!(f, "XRA    {}", r),
            Ora(r) => write!(f, "ORA    {}", r),
            Cmp(r) => write!(f, "CMP    {}", r),
            Rst(i) => write!(f, "RST    ${:02x}", i.irq_address()),
            Rnz => write!(f, "RNZ"),
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
        case("06 32", 0x06, 2, "MVI    B,#0x32"),
        case("07", 0x07, 1, "RLC"),
        case("09", 0x09, 1, "DAD    BC"),
        case("0a", 0x0a, 1, "LDAX   BC"),
        case("0b", 0x0b, 1, "DCX    BC"),
        case("0c", 0x0c, 1, "INR    C"),
        case("0d", 0x0d, 1, "DCR    C"),
        case("0e ae", 0x0e, 2, "MVI    C,#0xae"),
        case("0f", 0x0f, 1, "RRC"),
        case("11 13 aa", 0x11, 3, "LXI    DE,#0xaa13"),
        case("12", 0x12, 1, "STAX   DE"),
        case("13", 0x13, 1, "INX    DE"),
        case("14", 0x14, 1, "INR    D"),
        case("15", 0x15, 1, "DCR    D"),
        case("16 d1", 0x16, 2, "MVI    D,#0xd1"),
        case("17", 0x17, 1, "RAL"),
        case("19", 0x19, 1, "DAD    DE"),
        case("1a", 0x1a, 1, "LDAX   DE"),
        case("1b", 0x1b, 1, "DCX    DE"),
        case("1c", 0x1c, 1, "INR    E"),
        case("1d", 0x1d, 1, "DCR    E"),
        case("1e ae", 0x1e, 2, "MVI    E,#0xae"),
        case("1f", 0x1f, 1, "RAR"),
        case("20", 0x20, 1, "RIM"),
        case("21 af 1d", 0x21, 3, "LXI    HL,#0x1daf"),
        case("23", 0x23, 1, "INX    HL"),
        case("24", 0x24, 1, "INR    H"),
        case("25", 0x25, 1, "DCR    H"),
        case("26 31", 0x26, 2, "MVI    H,#0x31"),
        case("27", 0x27, 1, "DAA"),
        case("29", 0x29, 1, "DAD    HL"),
        case("2a 12 fa", 0x2a, 3, "LHLD   $fa12"),
        case("2b", 0x2b, 1, "DCX    HL"),
        case("2c", 0x2c, 1, "INR    L"),
        case("2d", 0x2d, 1, "DCR    L"),
        case("2e 0e", 0x2e, 2, "MVI    L,#0x0e"),
        case("2f", 0x2f, 1, "CMA"),
        case("30", 0x30, 1, "SIM"),
        case("31 af d3", 0x31, 3, "LXI    SP,$d3af"),
        case("32 72 20", 0x32, 3, "STA    $2072"),
        case("33", 0x33, 1, "INX    SP"),
        case("34", 0x34, 1, "INR    (HL)"),
        case("35", 0x35, 1, "DCR    (HL)"),
        case("36 c2", 0x36, 2, "MVI    (HL),#0xc2"),
        case("37", 0x37, 1, "STC"),
        case("39", 0x39, 1, "DAD    SP"),
        case("3a ad 13", 0x3a, 3, "LDA    $13ad"),
        case("3b", 0x3b, 1, "DCX    SP"),
        case("3c", 0x3c, 1, "INR    A"),
        case("3d", 0x3d, 1, "DCR    A"),
        case("3e 80", 0x3e, 2, "MVI    A,#0x80"),
        case("3f", 0x3f, 1, "CMC"),
        case("40", 0x40, 1, "MOV    B <- B"),
        case("41", 0x41, 1, "MOV    B <- C"),
        case("42", 0x42, 1, "MOV    B <- D"),
        case("43", 0x43, 1, "MOV    B <- E"),
        case("44", 0x44, 1, "MOV    B <- H"),
        case("45", 0x45, 1, "MOV    B <- L"),
        case("46", 0x46, 1, "MOV    B <- (HL)"),
        case("47", 0x47, 1, "MOV    B <- A"),
        case("48", 0x48, 1, "MOV    C <- B"),
        case("49", 0x49, 1, "MOV    C <- C"),
        case("4a", 0x4a, 1, "MOV    C <- D"),
        case("4b", 0x4b, 1, "MOV    C <- E"),
        case("4c", 0x4c, 1, "MOV    C <- H"),
        case("4d", 0x4d, 1, "MOV    C <- L"),
        case("4e", 0x4e, 1, "MOV    C <- (HL)"),
        case("4f", 0x4f, 1, "MOV    C <- A"),
        case("50", 0x50, 1, "MOV    D <- B"),
        case("51", 0x51, 1, "MOV    D <- C"),
        case("52", 0x52, 1, "MOV    D <- D"),
        case("53", 0x53, 1, "MOV    D <- E"),
        case("54", 0x54, 1, "MOV    D <- H"),
        case("55", 0x55, 1, "MOV    D <- L"),
        case("56", 0x56, 1, "MOV    D <- (HL)"),
        case("57", 0x57, 1, "MOV    D <- A"),
        case("58", 0x58, 1, "MOV    E <- B"),
        case("59", 0x59, 1, "MOV    E <- C"),
        case("5a", 0x5a, 1, "MOV    E <- D"),
        case("5b", 0x5b, 1, "MOV    E <- E"),
        case("5c", 0x5c, 1, "MOV    E <- H"),
        case("5d", 0x5d, 1, "MOV    E <- L"),
        case("5e", 0x5e, 1, "MOV    E <- (HL)"),
        case("5f", 0x5f, 1, "MOV    E <- A"),
        case("60", 0x60, 1, "MOV    H <- B"),
        case("61", 0x61, 1, "MOV    H <- C"),
        case("62", 0x62, 1, "MOV    H <- D"),
        case("63", 0x63, 1, "MOV    H <- E"),
        case("64", 0x64, 1, "MOV    H <- H"),
        case("65", 0x65, 1, "MOV    H <- L"),
        case("66", 0x66, 1, "MOV    H <- (HL)"),
        case("67", 0x67, 1, "MOV    H <- A"),
        case("68", 0x68, 1, "MOV    L <- B"),
        case("69", 0x69, 1, "MOV    L <- C"),
        case("6a", 0x6a, 1, "MOV    L <- D"),
        case("6b", 0x6b, 1, "MOV    L <- E"),
        case("6c", 0x6c, 1, "MOV    L <- H"),
        case("6d", 0x6d, 1, "MOV    L <- L"),
        case("6e", 0x6e, 1, "MOV    L <- (HL)"),
        case("6f", 0x6f, 1, "MOV    L <- A"),
        case("70", 0x70, 1, "MOV    (HL) <- B"),
        case("71", 0x71, 1, "MOV    (HL) <- C"),
        case("72", 0x72, 1, "MOV    (HL) <- D"),
        case("73", 0x73, 1, "MOV    (HL) <- E"),
        case("74", 0x74, 1, "MOV    (HL) <- H"),
        case("75", 0x75, 1, "MOV    (HL) <- L"),
        case("76", 0x76, 1, "HLT"),
        case("77", 0x77, 1, "MOV    (HL) <- A"),
        case("78", 0x78, 1, "MOV    A <- B"),
        case("79", 0x79, 1, "MOV    A <- C"),
        case("7a", 0x7a, 1, "MOV    A <- D"),
        case("7b", 0x7b, 1, "MOV    A <- E"),
        case("7c", 0x7c, 1, "MOV    A <- H"),
        case("7d", 0x7d, 1, "MOV    A <- L"),
        case("7e", 0x7e, 1, "MOV    A <- (HL)"),
        case("7f", 0x7f, 1, "MOV    A <- A"),
        case("80", 0x80, 1, "ADD    B"),
        case("81", 0x81, 1, "ADD    C"),
        case("82", 0x82, 1, "ADD    D"),
        case("83", 0x83, 1, "ADD    E"),
        case("84", 0x84, 1, "ADD    H"),
        case("85", 0x85, 1, "ADD    L"),
        case("86", 0x86, 1, "ADD    (HL)"),
        case("87", 0x87, 1, "ADD    A"),
        case("88", 0x88, 1, "ADC    B"),
        case("89", 0x89, 1, "ADC    C"),
        case("8a", 0x8a, 1, "ADC    D"),
        case("8b", 0x8b, 1, "ADC    E"),
        case("8c", 0x8c, 1, "ADC    H"),
        case("8d", 0x8d, 1, "ADC    L"),
        case("8e", 0x8e, 1, "ADC    (HL)"),
        case("8f", 0x8f, 1, "ADC    A"),
        case("90", 0x90, 1, "SUB    B"),
        case("91", 0x91, 1, "SUB    C"),
        case("92", 0x92, 1, "SUB    D"),
        case("93", 0x93, 1, "SUB    E"),
        case("94", 0x94, 1, "SUB    H"),
        case("95", 0x95, 1, "SUB    L"),
        case("96", 0x96, 1, "SUB    (HL)"),
        case("97", 0x97, 1, "SUB    A"),
        case("98", 0x98, 1, "SBB    B"),
        case("99", 0x99, 1, "SBB    C"),
        case("9a", 0x9a, 1, "SBB    D"),
        case("9b", 0x9b, 1, "SBB    E"),
        case("9c", 0x9c, 1, "SBB    H"),
        case("9d", 0x9d, 1, "SBB    L"),
        case("9e", 0x9e, 1, "SBB    (HL)"),
        case("9f", 0x9f, 1, "SBB    A"),
        case("a0", 0xa0, 1, "ANA    B"),
        case("a1", 0xa1, 1, "ANA    C"),
        case("a2", 0xa2, 1, "ANA    D"),
        case("a3", 0xa3, 1, "ANA    E"),
        case("a4", 0xa4, 1, "ANA    H"),
        case("a5", 0xa5, 1, "ANA    L"),
        case("a6", 0xa6, 1, "ANA    (HL)"),
        case("a7", 0xa7, 1, "ANA    A"),
        case("a8", 0xa8, 1, "XRA    B"),
        case("a9", 0xa9, 1, "XRA    C"),
        case("aa", 0xaa, 1, "XRA    D"),
        case("ab", 0xab, 1, "XRA    E"),
        case("ac", 0xac, 1, "XRA    H"),
        case("ad", 0xad, 1, "XRA    L"),
        case("ae", 0xae, 1, "XRA    (HL)"),
        case("af", 0xaf, 1, "XRA    A"),
        case("b0", 0xb0, 1, "ORA    B"),
        case("b1", 0xb1, 1, "ORA    C"),
        case("b2", 0xb2, 1, "ORA    D"),
        case("b3", 0xb3, 1, "ORA    E"),
        case("b4", 0xb4, 1, "ORA    H"),
        case("b5", 0xb5, 1, "ORA    L"),
        case("b6", 0xb6, 1, "ORA    (HL)"),
        case("b7", 0xb7, 1, "ORA    A"),
        case("b8", 0xb8, 1, "CMP    B"),
        case("b9", 0xb9, 1, "CMP    C"),
        case("ba", 0xba, 1, "CMP    D"),
        case("bb", 0xbb, 1, "CMP    E"),
        case("bc", 0xbc, 1, "CMP    H"),
        case("bd", 0xbd, 1, "CMP    L"),
        case("be", 0xbe, 1, "CMP    (HL)"),
        case("bf", 0xbf, 1, "CMP    A"),
        case("c0", 0xc0, 1, "RNZ"),
        case("c1", 0xc1, 1, "POP    BC"),
        case("c2 18 d4", 0xc2, 3, "JNZ    $d418"),
        case("c3 d4 18", 0xc3, 3, "JMP    $18d4"),
        case("c4 af de", 0xc4, 3, "CNZ    $deaf"),
        case("c5", 0xc5, 1, "PUSH   BC"),
        case("c6 02", 0xc6, 2, "ADI    #0x02"),
        case("c7", 0xc7, 1, "RST    $08"),
        case("d1", 0xd1, 1, "POP    DE"),
        case("d5", 0xd5, 1, "PUSH   DE"),
        case("d6 e3", 0xd6, 2, "SUI    #0xe3"),
        case("d7", 0xd7, 1, "RST    $10"),
        case("e1", 0xe1, 1, "POP    HL"),
        case("e5", 0xe5, 1, "PUSH   HL"),
        case("e6 32", 0xe6, 2, "ANI    #0x32"),
        case("e7", 0xe7, 1, "RST    $18"),
        case("f1", 0xf1, 1, "POP    PSW"),
        case("f5", 0xf5, 1, "PUSH   PSW"),
        case("f6 a4", 0xf6, 2, "ORI    #0xa4"),
        case("f7", 0xf7, 1, "RST    $20"),
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