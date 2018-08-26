use {ToOpcode, Byte, Address};

#[derive(PartialOrd, PartialEq, Debug, Copy, Clone)]
pub enum Reg {
    A,
    B,
    C,
    D,
    E,
    H,
    L,
    M,
}

impl From<Byte> for Reg {
    fn from(v: Byte) -> Self {
        use self::Reg::*;
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

impl ToOpcode for Reg {
    fn opcode(self) -> Byte {
        use self::Reg::*;
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

use ::std::fmt::{Formatter, Display, Result};

impl Display for Reg {
    fn fmt(&self, f: &mut Formatter) -> Result {
        use self::Reg::*;
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

#[derive(PartialOrd, PartialEq, Debug, Copy, Clone)]
pub enum RegPair {
    BC,
    DE,
    HL,
    SP,
}

impl From<Byte> for RegPair {
    fn from(v: Byte) -> Self {
        use self::RegPair::*;
        match v {
            0x00 => BC,
            0x10 => DE,
            0x20 => HL,
            0x30 => SP,
            invalid => panic!("Invalid reg pair opcode 0x{:02x}", invalid)
        }
    }
}

impl ToOpcode for RegPair {
    fn opcode(self) -> Byte {
        use self::RegPair::*;
        match self {
            BC => 0x00,
            DE => 0x10,
            HL => 0x20,
            SP => 0x30,
        }
    }
}

impl RegPair {
    pub fn is_basic(self) -> bool {
        use self::RegPair::*;
        [BC, DE].contains(&self)
    }
}

impl Display for RegPair {
    fn fmt(&self, f: &mut Formatter) -> Result {
        use self::RegPair::*;
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

#[derive(PartialOrd, PartialEq, Debug, Copy, Clone)]
pub enum RegPairValue {
    BC(Byte, Byte),
    DE(Byte, Byte),
    HL(Byte, Byte),
    SP(Address),
}

impl Display for RegPairValue {
    fn fmt(&self, f: &mut Formatter) -> Result {
        use self::RegPairValue::*;
        match *self {
            BC(b, c) => write!(f, "BC,#0x{:02x}{:02x}", b, c),
            DE(d, e) => write!(f, "DE,#0x{:02x}{:02x}", d, e),
            HL(h, l) => write!(f, "HL,#0x{:02x}{:02x}", h, l),
            SP(addr) => write!(f, "SP,${:04x}", addr),
        }
    }
}

impl Into<RegPair> for RegPairValue {
    fn into(self) -> RegPair {
        use self::RegPairValue::*;
        match self {
            BC(_, _) => RegPair::BC,
            DE(_, _) => RegPair::DE,
            HL(_, _) => RegPair::HL,
            SP(_) => RegPair::SP,
        }
    }
}

impl From<[Byte; 3]> for RegPairValue {
    fn from(data: [Byte; 3]) -> Self {
        use self::RegPairValue::*;
        match data[0] & 0x30 {
            0x00 => BC(data[2], data[1]),
            0x10 => DE(data[2], data[1]),
            0x20 => HL(data[2], data[1]),
            0x30 => SP(((data[2] as Address) << 8) | (data[1] as Address)),
            _ => unreachable!("Should never be vaild {:02x}", data[0])
        }
    }
}

impl ToOpcode for RegPairValue {
    fn opcode(self) -> Byte {
        let v: RegPair = self.into();
        v.opcode()
    }
}

#[derive(PartialOrd, PartialEq, Debug, Copy, Clone)]
pub enum BytePair {
    BC,
    DE,
    HL,
    AF,
}

impl ToOpcode for BytePair {
    fn opcode(self) -> Byte {
        use self::BytePair::*;
        match self {
            BC => 0x00,
            DE => 0x10,
            HL => 0x20,
            AF => 0x30,
        }
    }
}

impl From<Byte> for BytePair {
    fn from(v: Byte) -> Self {
        use self::BytePair::*;
        match v {
            0x00 => BC,
            0x10 => DE,
            0x20 => HL,
            0x30 => AF,
            invalid => panic!("Invalid byte pair opcode 0x{:02x}", invalid)
        }
    }
}

impl Display for BytePair {
    fn fmt(&self, f: &mut Formatter) -> Result {
        use self::BytePair::*;
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
pub enum IrqAddr {
    I0,
    I1,
    I2,
    I3,
    I4,
    I5,
    I6,
    I7,
}

impl From<Byte> for IrqAddr {
    fn from(c: Byte) -> Self {
        use self::IrqAddr::*;
        match c {
            0x00 => I0,
            0x08 => I1,
            0x10 => I2,
            0x18 => I3,
            0x20 => I4,
            0x28 => I5,
            0x30 => I6,
            0x38 => I7,
            _ => panic!("Invalid irq address opcode 0x{:02x}", c)
        }
    }
}

impl ToOpcode for IrqAddr {
    fn opcode(self) -> Byte {
        use self::IrqAddr::*;
        match self {
            I0 => 0x00,
            I1 => 0x08,
            I2 => 0x10,
            I3 => 0x18,
            I4 => 0x20,
            I5 => 0x28,
            I6 => 0x30,
            I7 => 0x38,
        }
    }
}

impl IrqAddr {
    pub fn irq_address(&self) -> Byte {
        use self::IrqAddr::*;
        match *self {
            I0 => 0x00,
            I1 => 0x08,
            I2 => 0x10,
            I3 => 0x18,
            I4 => 0x20,
            I5 => 0x28,
            I6 => 0x30,
            I7 => 0x38,
        }
    }
}

#[derive(PartialOrd, PartialEq, Debug, Copy, Clone)]
pub enum CondFlag {
    NZ,
    Z,
    NC,
    C,
    PO,
    PE,
    P,
    M,
}

impl Display for CondFlag {
    fn fmt(&self, f: &mut Formatter) -> Result {
        use self::CondFlag::*;
        write!(f, "{}", match *self {
            NZ => "NZ",
            Z => "Z",
            NC => "NC",
            C => "C",
            PO => "PO",
            PE => "PE",
            P => "P",
            M => "M"
        })
    }
}

impl From<Byte> for CondFlag {
    fn from(c: Byte) -> Self {
        use self::CondFlag::*;
        match c {
            0x00 => NZ,
            0x08 => Z,
            0x10 => NC,
            0x18 => C,
            0x20 => PO,
            0x28 => PE,
            0x30 => P,
            0x38 => M,
            c => panic!("Invalid opcode 0x{:02x}", c)
        }
    }
}

impl ToOpcode for CondFlag {
    fn opcode(self) -> Byte {
        use self::CondFlag::*;
        match self {
            NZ => 0x00,
            Z => 0x08,
            NC => 0x10,
            C => 0x18,
            PO => 0x20,
            PE => 0x28,
            P => 0x30,
            M => 0x38
        }
    }
}

#[derive(PartialOrd, PartialEq, Debug, Copy, Clone)]
pub enum Instruction {
    Nop,
    Lxi(RegPairValue),
    Inx(RegPair),
    Stax(RegPair),
    Inr(Reg),
    Dcr(Reg),
    Jump(Address),
    J(CondFlag, Address),
    C(CondFlag, Address),
    Sta(Address),
    Push(BytePair),
    Mvi(Reg, Byte),
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
    Shld(Address),
    Lhld(Address),
    Stc,
    Lda(Address),
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
    R(CondFlag),
    Pop(BytePair),
    Adi(Byte),
    Sui(Byte),
    Ani(Byte),
    Ori(Byte),
    Xri(Byte),
    Rst(IrqAddr),
    Ret,
    In(Byte),
    Out(Byte),
    Aci(Byte),
    Sbi(Byte),
    Pchl,
    Xchg,
    Xthl,
    Di,
    Sphl,
    Ei,
    Cpi(Byte),
    Call(Address),
}

impl Display for Instruction {
    fn fmt(&self, fmt: &mut Formatter) -> Result {
        use self::Instruction::*;
        match *self {
            Nop => write!(fmt, "NOP"),
            Lxi(rpv) => write!(fmt, "LXI    {}", rpv),
            Ldax(rp) => write!(fmt, "LDAX   {}", rp),
            Stax(rp) => write!(fmt, "STAX   {}", rp),
            Inx(rp) => write!(fmt, "INX    {}", rp),
            Inr(r) => write!(fmt, "INR    {}", r),
            Dcr(r) => write!(fmt, "DCR    {}", r),
            Mvi(reg, data) => write!(fmt, "MVI    {},#0x{:02x}", reg, data),
            Sta(offset) => write!(fmt, "STA    ${:04x}", offset),
            J(cf, offset) => write!(fmt, "J{:2}    ${:04x}", cf.to_string(), offset),
            Jump(offset) => write!(fmt, "JMP    ${:04x}", offset),
            C(cf, offset) => write!(fmt, "C{:2}    ${:04x}", cf.to_string(), offset),
            Lda(addr) => write!(fmt, "LDA    ${:04x}", addr),
            Shld(addr) => write!(fmt, "SHLD   ${:04x}", addr),
            Lhld(addr) => write!(fmt, "LHLD   ${:04x}", addr),
            Push(reg) => write!(fmt, "PUSH   {}", reg),
            Pop(reg) => write!(fmt, "POP    {}", reg),
            Adi(data) => write!(fmt, "ADI    #0x{:02x}", data),
            Sui(data) => write!(fmt, "SUI    #0x{:02x}", data),
            Ani(data) => write!(fmt, "ANI    #0x{:02x}", data),
            Ori(data) => write!(fmt, "ORI    #0x{:02x}", data),
            Xri(data) => write!(fmt, "XRI    #0x{:02x}", data),
            In(data) => write!(fmt, "IN     #0x{:02x}", data),
            Out(data) => write!(fmt, "OUT    #0x{:02x}", data),
            Aci(data) => write!(fmt, "ACI    #0x{:02x}", data),
            Sbi(data) => write!(fmt, "SBI    #0x{:02x}", data),
            Cpi(data) => write!(fmt, "CPI    #0x{:02x}", data),
            Call(data) => write!(fmt, "CALL   ${:02x}", data),
            Rlc => write!(fmt, "RLC"),
            Rrc => write!(fmt, "RRC"),
            Ral => write!(fmt, "RAL"),
            Rar => write!(fmt, "RAR"),
            Rim => write!(fmt, "RIM"),
            Daa => write!(fmt, "DAA"),
            Cma => write!(fmt, "CMA"),
            Sim => write!(fmt, "SIM"),
            Stc => write!(fmt, "STC"),
            Cmc => write!(fmt, "CMC"),
            Hlt => write!(fmt, "HLT"),
            Dad(rp) => write!(fmt, "DAD    {}", rp),
            Dcx(rp) => write!(fmt, "DCX    {}", rp),
            Add(r) => write!(fmt, "ADD    {}", r),
            Mov(f, t) => write!(fmt, "MOV    {} <- {}", t, f),
            Adc(r) => write!(fmt, "ADC    {}", r),
            Sub(r) => write!(fmt, "SUB    {}", r),
            Sbb(r) => write!(fmt, "SBB    {}", r),
            Ana(r) => write!(fmt, "ANA    {}", r),
            Xra(r) => write!(fmt, "XRA    {}", r),
            Ora(r) => write!(fmt, "ORA    {}", r),
            Cmp(r) => write!(fmt, "CMP    {}", r),
            Rst(i) => write!(fmt, "RST    ${:02x}", i.irq_address()),
            R(cf) => write!(fmt, "R{}", cf),
            Ret => write!(fmt, "RET"),
            Pchl => write!(fmt, "PCHL"),
            Xchg => write!(fmt, "XCHG"),
            Xthl => write!(fmt, "XTHL"),
            Di => write!(fmt, "DI"),
            Sphl => write!(fmt, "SPHL"),
            Ei => write!(fmt, "EI"),
        }
    }
}

impl ToOpcode for Instruction {
    fn opcode(self) -> Byte {
        use self::Instruction::*;
        match self {
            Nop => 0x00,
            Lxi(rpv) => 0x01 | rpv.opcode(),
            Stax(rp) if rp.is_basic() => 0x02 | rp.opcode(),
            Inx(rp) => 0x03 | rp.opcode(),
            Ldax(rp) if rp.is_basic() => 0x0a | rp.opcode(),
            Dcx(rp) => 0x0b | rp.opcode(),
            Inr(r) => 0x04 | r.opcode() << 3,
            Dcr(r) => 0x05 | r.opcode() << 3,
            Mvi(r, _) => 0x06 | r.opcode() << 3,
            Hlt => 0x76,
            Sta(_) => 0x32,
            J(cf, _) => 0xc2 | cf.opcode(),
            Jump(_) => 0xc3,
            C(cf, _) => 0xc4 | cf.opcode(),
            Push(bp) => 0xc5 | bp.opcode(),
            Adi(_) => 0xc6,
            Sui(_) => 0xd6,
            Ani(_) => 0xe6,
            Xri(_) => 0xee,
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
            Shld(_) => 0x22,
            Lhld(_) => 0x2a,
            Lda(_) => 0x3a,
            Dad(rp) => 0x09 | rp.opcode(),
            Mov(f, t) => (0x40 + (t.opcode() << 3)) | f.opcode(),
            Add(r) => 0x80 + r.opcode(),
            Adc(r) => 0x88 + r.opcode(),
            Sub(r) => 0x90 + r.opcode(),
            Sbb(r) => 0x98 + r.opcode(),
            Ana(r) => 0xa0 + r.opcode(),
            Xra(r) => 0xa8 + r.opcode(),
            Ora(r) => 0xb0 + r.opcode(),
            Cmp(r) => 0xb8 + r.opcode(),
            R(cf) => 0xc0 | cf.opcode(),
            Pop(bp) => 0xc1 | bp.opcode(),
            Rst(irq) => 0xc7 | irq.opcode(),
            Ret => 0xc9,
            Call(_) => 0xcd,
            In(_) => 0xdb,
            Out(_) => 0xd3,
            Aci(_) => 0xce,
            Sbi(_) => 0xde,
            Pchl => 0xe9,
            Xthl => 0xe3,
            Xchg => 0xeb,
            Di => 0xf3,
            Sphl => 0xf9,
            Ei => 0xfb,
            Cpi(_) => 0xfe,
            Ldax(_) => panic!("Invalid syntax!"),
            Stax(_) => panic!("Invalid syntax!"),
        }
    }
}

impl Instruction {
    pub fn length(&self) -> u16 {
        use self::Instruction::*;
        match *self {
            Jump(_) | J(_, _) | C(_, _) | Sta(_) |
            Lda(_) | Shld(_) | Lhld(_) | Lxi(_) | Call(_) => 3,
            Mvi(_, _) | Adi(_) | Sui(_) | Ani(_) | Xri(_) | Ori(_) |
            In(_) | Out(_) | Aci(_)  | Sbi(_) | Cpi(_) => 2,
            _ => 1
        }
    }
}
