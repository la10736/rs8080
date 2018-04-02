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
        asm::{Instruction, Instruction::*, RegPairValue},
    };

    #[derive(Default, Clone)]
    struct State {
        b: Word,
        c: Word,
        pc: Address,
    }

    #[derive(Default, Clone)]
    pub struct Cpu {
        state: State
    }

    impl Cpu {
        pub fn exec(&mut self, instruction: Instruction) -> () {
            match instruction {
                Lxi(RegPairValue::BC(b, c)) => {
                    self.state.b = b;
                    self.state.c = c;
                }
                _ => unimplemented!("Instruction {:?} not implemented yet!", instruction)
            }
            self.state.pc += instruction.length();
        }
    }

    #[cfg(test)]
    mod test {
        use super::*;

        #[derive(Default)]
        struct StateBuilder {
            proto: State
        }

        impl StateBuilder {
            fn create(&self) -> State {
                self.proto.clone()
            }

            fn b(mut self, val: Word) -> Self {
                self.proto.b = val;
                self
            }

            fn c(mut self, val: Word) -> Self {
                self.proto.c = val;
                self
            }

            fn pc(mut self, address: Address) -> Self {
                self.proto.pc = address;
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

        #[test]
        fn lxi() {
            let state = StateBuilder::default()
                .b(0x10)
                .c(0xa6)
                .pc(0x3245)
                .create();

            let mut cpu = CpuBuilder::default()
                .state(state)
                .create();

            cpu.exec(Lxi(RegPairValue::BC(0xae, 0x02)));

            assert_eq!(cpu.state.b, 0xae);
            assert_eq!(cpu.state.c, 0x02);

            assert_eq!(cpu.state.pc, 0x3248);
        }
    }
}