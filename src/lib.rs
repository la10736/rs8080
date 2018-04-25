#![feature(proc_macro)]
extern crate rstest;

pub type Word = u8;
pub type DWord = u16;
pub type Address = u16;

pub trait ToOpcode {
    fn opcode(self) -> Word;
}

pub mod disassemble;

pub mod asm {
    use {ToOpcode, Word, Address};

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

    impl From<Word> for Reg {
        fn from(v: Word) -> Self {
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
        fn opcode(self) -> Word {
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

    impl From<Word> for RegPair {
        fn from(v: Word) -> Self {
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
        fn opcode(self) -> Word {
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
        BC(Word, Word),
        DE(Word, Word),
        HL(Word, Word),
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

    impl From<[Word; 3]> for RegPairValue {
        fn from(data: [Word; 3]) -> Self {
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
        fn opcode(self) -> Word {
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
        fn opcode(self) -> Word {
            use self::BytePair::*;
            match self {
                BC => 0x00,
                DE => 0x10,
                HL => 0x20,
                AF => 0x30,
            }
        }
    }

    impl From<Word> for BytePair {
        fn from(v: Word) -> Self {
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

    impl From<Word> for IrqAddr {
        fn from(c: Word) -> Self {
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
        fn opcode(self) -> Word {
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
        pub fn irq_address(&self) -> Word {
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

    impl From<Word> for CondFlag {
        fn from(c: Word) -> Self {
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
        fn opcode(self) -> Word {
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
        Mvi(Reg, Word),
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
        Adi(Word),
        Sui(Word),
        Ani(Word),
        Ori(Word),
        Rst(IrqAddr),
        Ret,
        In(Word),
        Out(Word),
        Sbi(Word),
        Pchl,
        Xchg,
        Xthl,
        Di,
        Sphl,
        Ei,
        Cpi(Word),
        Call(Address),
    }

    impl Display for Instruction {
        fn fmt(&self, f: &mut Formatter) -> Result {
            use self::Instruction::*;
            match *self {
                Nop => write!(f, "NOP"),
                Lxi(rpv) => write!(f, "LXI    {}", rpv),
                Ldax(rp) => write!(f, "LDAX   {}", rp),
                Stax(rp) => write!(f, "STAX   {}", rp),
                Inx(rp) => write!(f, "INX    {}", rp),
                Inr(r) => write!(f, "INR    {}", r),
                Dcr(r) => write!(f, "DCR    {}", r),
                Mvi(reg, data) => write!(f, "MVI    {},#0x{:02x}", reg, data),
                Sta(offset) => write!(f, "STA    ${:04x}", offset),
                J(cf, offset) => write!(f, "J{:2}    ${:04x}", cf.to_string(), offset),
                Jump(offset) => write!(f, "JMP    ${:04x}", offset),
                C(cf, offset) => write!(f, "C{:2}    ${:04x}", cf.to_string(), offset),
                Lda(addr) => write!(f, "LDA    ${:04x}", addr),
                Shld(addr) => write!(f, "SHLD   ${:04x}", addr),
                Lhld(addr) => write!(f, "LHLD   ${:04x}", addr),
                Push(reg) => write!(f, "PUSH   {}", reg),
                Pop(reg) => write!(f, "POP    {}", reg),
                Adi(data) => write!(f, "ADI    #0x{:02x}", data),
                Sui(data) => write!(f, "SUI    #0x{:02x}", data),
                Ani(data) => write!(f, "ANI    #0x{:02x}", data),
                Ori(data) => write!(f, "ORI    #0x{:02x}", data),
                In(data) => write!(f, "IN     #0x{:02x}", data),
                Out(data) => write!(f, "OUT    #0x{:02x}", data),
                Sbi(data) => write!(f, "SBI    #0x{:02x}", data),
                Cpi(data) => write!(f, "CPI    #0x{:02x}", data),
                Call(data) => write!(f, "CALL   ${:02x}", data),
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
                Add(r) => write!(f, "ADD    {}", r),
                Mov(r0, r1) => write!(f, "MOV    {} <- {}", r0, r1),
                Adc(r) => write!(f, "ADC    {}", r),
                Sub(r) => write!(f, "SUB    {}", r),
                Sbb(r) => write!(f, "SBB    {}", r),
                Ana(r) => write!(f, "ANA    {}", r),
                Xra(r) => write!(f, "XRA    {}", r),
                Ora(r) => write!(f, "ORA    {}", r),
                Cmp(r) => write!(f, "CMP    {}", r),
                Rst(i) => write!(f, "RST    ${:02x}", i.irq_address()),
                R(cf) => write!(f, "R{}", cf),
                Ret => write!(f, "RET"),
                Pchl => write!(f, "PCHL"),
                Xchg => write!(f, "XCHG"),
                Xthl => write!(f, "XTHL"),
                Di => write!(f, "DI"),
                Sphl => write!(f, "SPHL"),
                Ei => write!(f, "EI"),
            }
        }
    }

    impl ToOpcode for Instruction {
        fn opcode(self) -> Word {
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
                Mov(r0, r1) => (0x40 + (r0.opcode() << 3)) | r1.opcode(),
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
                Mvi(_, _) | Adi(_) | Sui(_) | Ani(_) | Ori(_) |
                In(_) | Out(_) | Sbi(_) | Cpi(_) => 2,
                _ => 1
            }
        }
    }
}

pub mod cpu {
    use super::{
        Word, Address,
        asm::{Instruction, Instruction::*, RegPair, RegPairValue},
    };

    use std::num::Wrapping;

    #[derive(Default, Debug, Clone, Copy, Eq, PartialEq)]
    struct RegWord(Wrapping<Word>);

    #[derive(Default, Debug, Clone, Copy, Eq, PartialEq)]
    struct RegAddress(Wrapping<Address>);

    impl ::std::ops::AddAssign<Word> for RegWord {
        fn add_assign(&mut self, rhs: Word) {
            self.0 += Wrapping(rhs);
        }
    }

    impl ::std::ops::Add<Word> for RegWord {
        type Output = RegWord;

        fn add(self, rhs: Word) -> <Self as ::std::ops::Add<Word>>::Output {
            (self.0 + Wrapping(rhs)).into()
        }
    }

    impl ::std::ops::Sub<Word> for RegWord {
        type Output = RegWord;

        fn sub(self, rhs: Word) -> <Self as ::std::ops::Sub<Word>>::Output {
            (self.0 - Wrapping(rhs)).into()
        }
    }

    impl PartialEq<Word> for RegWord {
        fn eq(&self, other: &Word) -> bool {
            self == &RegWord::from(*other)
        }
    }

    impl From<Word> for RegWord {
        fn from(v: Word) -> Self {
            RegWord(Wrapping(v))
        }
    }

    impl From<Wrapping<Word>> for RegWord {
        fn from(v: Wrapping<Word>) -> Self {
            RegWord(v)
        }
    }

    impl Into<Word> for RegWord {
        fn into(self) -> Word {
            (self.0).0
        }
    }

    impl ::std::ops::AddAssign<Address> for RegAddress {
        fn add_assign(&mut self, rhs: Address) {
            self.0 += Wrapping(rhs);
        }
    }

    impl ::std::ops::Add<Address> for RegAddress {
        type Output = RegAddress;

        fn add(self, rhs: Address) -> <Self as ::std::ops::Add<Address>>::Output {
            (self.0 + Wrapping(rhs)).into()
        }
    }

    impl ::std::ops::Sub<Address> for RegAddress {
        type Output = RegAddress;

        fn sub(self, rhs: Address) -> <Self as ::std::ops::Add<Address>>::Output {
            (self.0 - Wrapping(rhs)).into()
        }
    }

    impl PartialEq<Address> for RegAddress {
        fn eq(&self, other: &Address) -> bool {
            self == &RegAddress::from(*other)
        }
    }

    impl From<Address> for RegAddress {
        fn from(v: Address) -> Self {
            RegAddress(Wrapping(v))
        }
    }

    impl From<(Word, Word)> for RegAddress {
        fn from(v: (Word, Word)) -> Self {
            let (h, l) = v;
            RegAddress(Wrapping((h as Address) << 8 | (l as Address)))
        }
    }

    impl From<(RegWord, RegWord)> for RegAddress {
        fn from(v: (RegWord, RegWord)) -> Self {
            let (h, l) = v;
            ((h.0).0, (l.0).0).into()
        }
    }

    const WORD_SIZE: u8 = 8;
    const WORD_MASK: Address = 0xff;

    impl From<RegAddress> for (RegWord, RegWord) {
        fn from(v: RegAddress) -> Self {
            let inner = (v.0).0;
            ((((inner >> WORD_SIZE) & WORD_MASK) as Word).into(),
             ((inner & WORD_MASK) as Word).into())
        }
    }

    impl From<Wrapping<Address>> for RegAddress {
        fn from(v: Wrapping<Address>) -> Self {
            RegAddress(v)
        }
    }

    impl Into<Address> for RegAddress {
        fn into(self) -> Address {
            (self.0).0
        }
    }

    impl Into<(Word, Word)> for RegAddress {
        fn into(self) -> (Word, Word) {
            let a = (self.0).0;
            ((a >> 8) as Word, (a & 0xff) as Word)
        }
    }

    #[derive(Default, Clone)]
    struct State {
        b: RegWord,
        c: RegWord,
        d: RegWord,
        e: RegWord,
        h: RegWord,
        l: RegWord,
        pc: RegAddress,
        sp: RegAddress,
    }

    impl State {
        fn set_b(&mut self, v: Word) -> &mut Self {
            self.b = v.into();
            self
        }

        fn set_c(&mut self, v: Word) -> &mut Self {
            self.c = v.into();
            self
        }
        fn set_d(&mut self, v: Word) -> &mut Self {
            self.d = v.into();
            self
        }

        fn set_e(&mut self, v: Word) -> &mut Self {
            self.e = v.into();
            self
        }
        fn set_h(&mut self, v: Word) -> &mut Self {
            self.h = v.into();
            self
        }

        fn set_l(&mut self, v: Word) -> &mut Self {
            self.l = v.into();
            self
        }

        fn set_pc(&mut self, v: Address) -> &mut Self {
            self.pc = v.into();
            self
        }

        fn bc(&self) -> RegAddress {
            (self.b, self.c).into()
        }

        fn set_bc<A: Into<RegAddress>>(&mut self, val: A) {
            let (b, c) = val.into().into();
            self.b = b;
            self.c = c;
        }

        fn de(&self) -> RegAddress {
            (self.d, self.e).into()
        }

        fn set_de<A: Into<RegAddress>>(&mut self, val: A) {
            let (d, e) = val.into().into();
            self.d = d;
            self.e = e;
        }

        fn hl(&self) -> RegAddress {
            (self.h, self.l).into()
        }

        fn set_hl<A: Into<RegAddress>>(&mut self, val: A) {
            let (h, l) = val.into().into();
            self.h = h;
            self.l = l;
        }

        fn set_sp<A: Into<RegAddress>>(&mut self, v: A) -> &mut Self {
            self.sp = v.into();
            self
        }
    }

    #[derive(Default, Clone)]
    pub struct Cpu {
        state: State
    }

    impl Cpu {
        fn lxi(&mut self, v: self::RegPairValue) {
            use self::RegPairValue::*;
            match v {
                BC(b, c) => {
                    self.state.set_bc((b, c));
                }
                DE(d, e) => {
                    self.state.set_de((d, e));
                }
                HL(h, l) => {
                    self.state.set_hl((h, l));
                }
                SP(addr) => {
                    self.state.set_sp(addr);
                }
            }
        }

        fn inx(&mut self, rp: RegPair) -> () {
            use self::RegPair::*;
            match rp {
                BC => {
                    let v = self.state.bc() + 1;
                    self.state.set_bc(v);
                }
                DE => {
                    let v = self.state.de() + 1;
                    self.state.set_de(v);
                }
                HL => {
                    let v = self.state.hl() + 1;
                    self.state.set_hl(v);
                }
                SP => {
                    let v = self.state.sp + 1;
                    self.state.set_sp(v);
                }
            }
        }

        fn dcx(&mut self, rp: RegPair) -> () {
            use self::RegPair::*;
            match rp {
                BC => {
                    let v = self.state.bc() - 1;
                    self.state.set_bc(v);
                }
                DE => {
                    let v = self.state.de() - 1;
                    self.state.set_de(v);
                }
                HL => {
                    let v = self.state.hl() - 1;
                    self.state.set_hl(v);
                }
                SP => {
                    let v = self.state.sp - 1;
                    self.state.set_sp(v);
                }
            }
        }
    }

    impl Cpu {
        pub fn exec(&mut self, instruction: Instruction) -> () {
            match instruction {
                Lxi(rp) => {
                    self.lxi(rp);
                }
                Inx(rp) => {
                    self.inx(rp)
                }
                Dcx(rp) => {
                    self.dcx(rp)
                }
                _ => unimplemented!("Instruction {:?} not implemented yet!", instruction)
            }
            self.state.pc += instruction.length();
        }
    }

    #[cfg(test)]
    mod test {
        use super::*;
        use rstest::rstest;

        #[derive(Default)]
        struct StateBuilder {
            proto: State
        }

        impl StateBuilder {
            fn create(&self) -> State {
                self.proto.clone()
            }

            fn b(mut self, val: Word) -> Self {
                self.proto.set_b(val);
                self
            }

            fn c(mut self, val: Word) -> Self {
                self.proto.set_c(val);
                self
            }

            fn d(mut self, val: Word) -> Self {
                self.proto.set_d(val);
                self
            }

            fn e(mut self, val: Word) -> Self {
                self.proto.set_e(val);
                self
            }

            fn h(mut self, val: Word) -> Self {
                self.proto.set_h(val);
                self
            }

            fn l(mut self, val: Word) -> Self {
                self.proto.set_l(val);
                self
            }

            fn sp(mut self, addr: Address) -> Self {
                self.proto.set_sp(addr);
                self
            }

            fn pc(mut self, address: Address) -> Self {
                self.proto.set_pc(address);
                self
            }
        }

        #[derive(Default)]
        struct CpuBuilder {
            proto: Cpu
        }

        impl CpuBuilder {
            fn create(&self) -> Cpu {
                self.proto.clone()
            }

            fn state(mut self, state: State) -> Self {
                self.proto.state = state;
                self
            }
        }

        use rstest::rstest_parametrize;

        #[derive(Debug)]
        enum WrapperStr<'a> {
            Inner(&'a str),
            Wrapper { name: &'a str, body: Box<WrapperStr<'a>> },
        }

        impl<'a> WrapperStr<'a> {
            fn inner(self) -> &'a str {
                match self {
                    WrapperStr::Inner(inner) => inner,
                    _ => panic!("Wrong state {:?}", self)
                }
            }
        }

        impl<'a> Into<WrapperStr<'a>> for &'a str {
            fn into(self) -> WrapperStr<'a> {
                let mut s = self.splitn(2, '(');
                let cmd = s.next().unwrap();

                match s.next() {
                    None => WrapperStr::Inner(cmd),
                    Some(b) => {
                        let end = "".rfind(')').unwrap_or(b.len());
                        WrapperStr::Wrapper {
                            name: cmd,
                            body:
                            Box::new((&b[..end - 1]).into()),
                        }
                    }
                }
            }
        }

        fn u8_from_str(s: &str) -> u8 {
            {
                if s.len() <= 2 {
                    s.parse()
                } else {
                    match &s[..2] {
                        "0x" => u8::from_str_radix(&s[2..], 16),
                        _ => s.parse(),
                    }
                }
            }.unwrap()
        }

        fn u16_from_str(s: &str) -> u16 {
            {
                if s.len() <= 2 {
                    s.parse()
                } else {
                    match &s[..2] {
                        "0x" => u16::from_str_radix(&s[2..], 16),
                        _ => s.parse(),
                    }
                }
            }.unwrap()
        }

        fn u8x2_from_str<S: AsRef<str>>(s: S) -> (u8, u8) {
            let mut splitter = s.as_ref().splitn(2, ',');
            (u8_from_str(splitter.next().unwrap()), u8_from_str(splitter.next().unwrap()))
        }

        impl RegPairValue {
            fn bc(inner: &str) -> Self {
                let d = u8x2_from_str(inner);
                RegPairValue::BC(d.0, d.1)
            }
            fn de(inner: &str) -> Self {
                let d = u8x2_from_str(&inner);
                RegPairValue::DE(d.0, d.1)
            }
            fn hl(inner: &str) -> Self {
                let d = u8x2_from_str(&inner);
                RegPairValue::HL(d.0, d.1)
            }
            fn sp(inner: &str) -> Self {
                RegPairValue::SP(u16_from_str(&inner))
            }
        }

        impl<'a> Into<RegPairValue> for Box<WrapperStr<'a>> {
            fn into(self) -> RegPairValue {
                use self::WrapperStr::*;
                match *self {
                    Wrapper { name, body } => {
                        match name {
                            "BC" => RegPairValue::bc(body.inner()),
                            "DE" => RegPairValue::de(body.inner()),
                            "HL" => RegPairValue::hl(body.inner()),
                            "SP" => RegPairValue::sp(body.inner()),
                            _ => panic!("RegPairValue {} not exists", name)
                        }
                    }
                    _ => panic!("RegPairValue should be a Wrapper not {:?}", self)
                }
            }
        }

        impl<'a> Into<Instruction> for WrapperStr<'a> {
            fn into(self) -> Instruction {
                use self::WrapperStr::*;
                match self {
                    Wrapper { name: "Lxi", body } => Lxi(body.into()),
                    _ => unimplemented!()
                }
            }
        }

        impl<'a> Into<Instruction> for &'a str {
            fn into(self) -> Instruction {
                let w: WrapperStr = self.into();
                w.into()
            }
        }

        #[rstest_parametrize(
        ins, start, expected,
        case("Lxi(BC(0xae,0x02))", 0x3245, 0x3248),
        case("Lxi(DE(0xae,0x02))", 0x1234, 0x1237),
        case("Lxi(HL(0xae,0x02))", 0x4321, 0x4324),
        case("Lxi(SP(0xae02))", 0x1010, 0x1013),
        )]
        fn lxi_should_advance_pc(ins: &str, start: Address, expected: Address) {
            let instruction = ins.into();

            let mut cpu = CpuBuilder::default()
                .state(StateBuilder::default()
                    .pc(start)
                    .create()
                )
                .create();

            cpu.exec(instruction);

            assert_eq!(cpu.state.pc, expected);
        }

        fn cpu() -> Cpu {
            CpuBuilder::default()
                .state(StateBuilder::default()
                    .b(0x10)
                    .c(0xa6)
                    .d(0x20)
                    .e(0xe6)
                    .h(0x22)
                    .l(0xee)
                    .sp(0x1234)
                    .create()
                )
                .create()
        }

        #[rstest]
        fn lxi_bc(mut cpu: Cpu) {
            cpu.exec(Lxi(RegPairValue::BC(0xae, 0x02)));

            assert_eq!(cpu.state.b, 0xae);
            assert_eq!(cpu.state.c, 0x02);
        }

        trait CpuQuery {
            type Result: PartialEq + Eq;

            fn ask(&self, cpu: &Cpu) -> Self::Result;
        }

        #[derive(PartialEq, Clone, Copy)]
        enum WordReg {
            B,
            C,
            D,
            E,
            H,
            L,
        }

        struct SP;

        trait QueryResult: PartialEq + Eq + Clone + Copy + ::std::fmt::Debug {}

        impl CpuQuery for WordReg {
            type Result = Word;

            fn ask(&self, cpu: &Cpu) -> <Self as CpuQuery>::Result {
                use self::WordReg::*;
                match *self {
                    B => cpu.state.b,
                    C => cpu.state.c,
                    D => cpu.state.d,
                    E => cpu.state.e,
                    H => cpu.state.h,
                    L => cpu.state.l,
                }.into()
            }
        }

        impl CpuQuery for SP {
            type Result = Address;

            fn ask(&self, cpu: &Cpu) -> <Self as CpuQuery>::Result {
                cpu.state.sp.into()
            }
        }

        impl QueryResult for Word {}

        impl QueryResult for Address {}

        impl<T0: QueryResult, T1: QueryResult> QueryResult for (T0, T1) {}

        impl<T0: QueryResult, R0, T1: QueryResult, R1> CpuQuery for (R0, R1) where
            R0: CpuQuery<Result=T0>,
            R1: CpuQuery<Result=T1>,
        {
            type Result = (T0, T1);

            fn ask(&self, cpu: &Cpu) -> <Self as CpuQuery>::Result {
                match *self {
                    (ref r0, ref r1) => (r0.ask(&cpu), r1.ask(&cpu))
                }
            }
        }

        #[rstest_parametrize(
        rp, query, expected,
        case(Unwrap("RegPairValue::BC(0xe4, 0xf1)"),
        Unwrap("(WordReg::B, WordReg::C)"), Unwrap("(0xe4, 0xf1)")),
        case(Unwrap("RegPairValue::DE(0x20, 0xb1)"),
        Unwrap("(WordReg::D, WordReg::E)"), Unwrap("(0x20, 0xb1)")),
        case(Unwrap("RegPairValue::HL(0x02, 0xae)"),
        Unwrap("(WordReg::H, WordReg::L)"), Unwrap("(0x02, 0xae)")),
        case(Unwrap("RegPairValue::SP(0x4321)"), Unwrap("SP"), 0x4321),
        )]
        fn lxi<R: QueryResult, Q: CpuQuery<Result=R>>(mut cpu: Cpu, rp: RegPairValue, query: Q, expected: R) {
            cpu.exec(Lxi(rp));

            assert_eq!(query.ask(&cpu), expected);
        }

        #[rstest_parametrize(
        rp, query, expected,
        case(Unwrap("RegPair::BC"), Unwrap("(WordReg::B, WordReg::C)"), Unwrap("(0x10, 0xa7)")),
        case(Unwrap("RegPair::DE"), Unwrap("(WordReg::D, WordReg::E)"), Unwrap("(0x20, 0xe7)")),
        case(Unwrap("RegPair::HL"), Unwrap("(WordReg::H, WordReg::L)"), Unwrap("(0x22, 0xef)")),
        case(Unwrap("RegPair::SP"), Unwrap("SP"), Unwrap("0x1235")),
        )]
        fn inx<R: QueryResult, Q: CpuQuery<Result=R>>(mut cpu: Cpu, rp: RegPair, query: Q, expected: R) {
            cpu.exec(Inx(rp));

            assert_eq!(query.ask(&cpu), expected);
        }

        trait ApplyState {
            fn apply(self, state: &mut State);
        }

        impl ApplyState for RegPairValue {
            fn apply(self, state: &mut State) {
                use self::RegPairValue::*;
                match self {
                    BC(b, c) => {
                        state.set_b(b);
                        state.set_c(c);
                    }
                    DE(d, e) => {
                        state.set_d(d);
                        state.set_e(e);
                    }
                    HL(h, l) => {
                        state.set_h(h);
                        state.set_l(l);
                    }
                    SP(sp) => {
                        state.set_sp(sp);
                    }
                }
            }
        }

        #[rstest_parametrize(
        init, rp, query, expected,
        case(Unwrap("RegPairValue::BC(0x00, 0xff)"), Unwrap("RegPair::BC"), Unwrap("(WordReg::B, WordReg::C)"), Unwrap("(0x01, 0x00)")),
        case(Unwrap("RegPairValue::BC(0xff, 0xff)"), Unwrap("RegPair::BC"), Unwrap("(WordReg::B, WordReg::C)"), Unwrap("(0x00, 0x00)")),
        case(Unwrap("RegPairValue::DE(0x12, 0xff)"), Unwrap("RegPair::DE"), Unwrap("(WordReg::D, WordReg::E)"), Unwrap("(0x13, 0x00)")),
        case(Unwrap("RegPairValue::DE(0xff, 0xff)"), Unwrap("RegPair::DE"), Unwrap("(WordReg::D, WordReg::E)"), Unwrap("(0x00, 0x00)")),
        case(Unwrap("RegPairValue::HL(0xae, 0xff)"), Unwrap("RegPair::HL"), Unwrap("(WordReg::H, WordReg::L)"), Unwrap("(0xaf, 0x00)")),
        case(Unwrap("RegPairValue::HL(0xff, 0xff)"), Unwrap("RegPair::HL"), Unwrap("(WordReg::H, WordReg::L)"), Unwrap("(0x00, 0x00)")),
        case(Unwrap("RegPairValue::SP(0xffff)"), Unwrap("RegPair::SP"), Unwrap("SP"), 0x0000),
        )]
        fn inx_should_wrap<R, Q>(mut cpu: Cpu, init: RegPairValue, rp: RegPair, query: Q, expected: R)
            where R: QueryResult, Q: CpuQuery<Result=R>
        {
            init.apply(&mut cpu.state);

            cpu.exec(Inx(rp));

            assert_eq!(query.ask(&cpu), expected);
        }

        #[rstest_parametrize(
        rp, query, expected,
        case(Unwrap("RegPair::BC"), Unwrap("(WordReg::B, WordReg::C)"), Unwrap("(0x10, 0xa5)")),
        case(Unwrap("RegPair::DE"), Unwrap("(WordReg::D, WordReg::E)"), Unwrap("(0x20, 0xe5)")),
        case(Unwrap("RegPair::HL"), Unwrap("(WordReg::H, WordReg::L)"), Unwrap("(0x22, 0xed)")),
        case(Unwrap("RegPair::SP"), Unwrap("SP"), Unwrap("0x1233")),
        )]
        fn dcx<R: QueryResult, Q: CpuQuery<Result=R>>(mut cpu: Cpu, rp: RegPair, query: Q, expected: R) {
            cpu.exec(Dcx(rp));

            assert_eq!(query.ask(&cpu), expected);
        }

        #[rstest_parametrize(
        init, rp, query, expected,
        case(Unwrap("RegPairValue::BC(0xff, 0x00)"), Unwrap("RegPair::BC"), Unwrap("(WordReg::B, WordReg::C)"), Unwrap("(0xfe, 0xff)")),
        case(Unwrap("RegPairValue::BC(0x00, 0x00)"), Unwrap("RegPair::BC"), Unwrap("(WordReg::B, WordReg::C)"), Unwrap("(0xff, 0xff)")),
        case(Unwrap("RegPairValue::DE(0x12, 0x00)"), Unwrap("RegPair::DE"), Unwrap("(WordReg::D, WordReg::E)"), Unwrap("(0x11, 0xff)")),
        case(Unwrap("RegPairValue::DE(0x00, 0x00)"), Unwrap("RegPair::DE"), Unwrap("(WordReg::D, WordReg::E)"), Unwrap("(0xff, 0xff)")),
        case(Unwrap("RegPairValue::HL(0xae, 0x00)"), Unwrap("RegPair::HL"), Unwrap("(WordReg::H, WordReg::L)"), Unwrap("(0xad, 0xff)")),
        case(Unwrap("RegPairValue::HL(0x00, 0x00)"), Unwrap("RegPair::HL"), Unwrap("(WordReg::H, WordReg::L)"), Unwrap("(0xff, 0xff)")),
        case(Unwrap("RegPairValue::SP(0x0000)"), Unwrap("RegPair::SP"), Unwrap("SP"), 0xffff),
        )]
        fn dcx_should_wrap<R, Q>(mut cpu: Cpu, init: RegPairValue, rp: RegPair, query: Q, expected: R)
            where R: QueryResult, Q: CpuQuery<Result=R>
        {
            init.apply(&mut cpu.state);

            cpu.exec(Dcx(rp));

            assert_eq!(query.ask(&cpu), expected);
        }

    }
}