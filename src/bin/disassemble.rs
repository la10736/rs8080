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
                M => "M",
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
    Mov(Reg, Reg)
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
            v if v >= 0x40 && v <= 0x7f => Mov(((v-0x40)>>3).into(), (v & 0x07).into()),
            0xc3 => Jump(self.u16_data()?),
            0xc5 => Push(BytePair::BC),
            0xd5 => Push(BytePair::DE),
            0xe5 => Push(BytePair::HL),
            0xf5 => Push(BytePair::AF),
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
            Mvi(B, _) => 0x06,
            Mvi(C, _) => 0x0e,
            Mvi(D, _) => 0x16,
            Mvi(E, _) => 0x1e,
            Mvi(H, _) => 0x26,
            Mvi(L, _) => 0x2e,
            Mvi(M, _) => 0x36,
            Mvi(A, _) => 0x3e,
            Sta(_) => 0x32,
            Jump(_) => 0xc3,
            Push(bp) => 0x05 | bp.opcode(),
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
            Ldax(_) => panic!("Invalid syntax!"),
            Stax(_) => panic!("Invalid syntax!"),
        }
    }

    fn length(&self) -> u16 {
        match *self {
            Jump(_) => 3,
            Sta(_) => 3,
            Lda(_) => 3,
            Lhld(_) => 3,
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
            Lxi(RegPair::SP, addr) => write!(f, "LXI    SP,${:04x}", addr),
            Lxi(rp, data) => write!(f, "LXI    {},#0x{:04x}", rp, data),
            Ldax(rp) => write!(f, "LDAX   {}", rp),
            Stax(rp) => write!(f, "STAX   {}", rp),
            Inx(rp) => write!(f,  "INX    {}", rp),
            Inr(r) => write!(f,  "INR    {}", r),
            Dcr(r) => write!(f,  "DCR    {}", r),
            Mvi(reg, data) => write!(f, "MVI    {},#0x{:02x}", reg, data),
            Sta(offset) => write!(f, "STA    ${:04x}", offset),
            Jump(offset) => write!(f, "JMP    ${:04x}", offset),
            Lda(addr) => write!(f, "LDA    ${:04x}", addr),
            Lhld(addr) => write!(f, "LHLD   ${:04x}", addr),
            Push(reg) => write!(f, "PUSH   {}", reg),
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
            Dad(rp) => write!(f, "DAD    {}", rp),
            Dcx(rp) => write!(f, "DCX    {}", rp),
            Mov(r0, M) => write!(f, "MOV    {} <- (HL)", r0),
            Mov(r0, r1) => write!(f, "MOV    {} <- {}", r0, r1),
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
        case("34", 0x34, 1, "INR    M"),
        case("35", 0x35, 1, "DCR    M"),
        case("36 c2", 0x36, 2, "MVI    M,#0xc2"),
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
        case("c3 d4 18", 0xc3, 3, "JMP    $18d4"),
        case("c5", 0xc5, 1, "PUSH   BC"),
        case("d5", 0xd5, 1, "PUSH   DE"),
        case("e5", 0xe5, 1, "PUSH   HL"),
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