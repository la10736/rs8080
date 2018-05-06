use super::{
    Word, Address,
    asm::{Instruction, Instruction::*, Reg, RegPair, RegPairValue},
};

#[derive(Default, Debug, Clone, Copy, Eq, PartialEq)]
struct RegWord(Word);

#[derive(Default, Debug, Clone, Copy, Eq, PartialEq)]
struct RegAddress(Address);

impl RegWord {
    fn increment(&mut self) { *self += 1; }
    fn decrement(&mut self) { *self -= 1; }
    fn one_complement(&mut self) { *self = (0xff ^ self.0).into() }
    fn is_zero(&self) -> bool { self.0 == 0 }
    fn sign_bit(&self) -> bool { (self.0 & 0x80) != 0x00 }
    fn parity(&self) -> bool { (self.0.count_ones() % 2) == 0 }
}

impl ::std::ops::AddAssign<Word> for RegWord {
    fn add_assign(&mut self, rhs: Word) {
        let (a, _b) = self.0.overflowing_add(rhs);
        self.0 = a;
    }
}

impl ::std::ops::SubAssign<Word> for RegWord {
    fn sub_assign(&mut self, rhs: u8) {
        let (a, _b) = self.0.overflowing_sub(rhs);
        self.0 = a;
    }
}

impl ::std::ops::Add<Word> for RegWord {
    type Output = RegWord;

    fn add(self, rhs: Word) -> <Self as ::std::ops::Add<Word>>::Output {
        let (a, _b) = self.0.overflowing_add(rhs);
        RegWord(a)
    }
}

impl ::std::ops::Sub<Word> for RegWord {
    type Output = RegWord;

    fn sub(self, rhs: Word) -> <Self as ::std::ops::Sub<Word>>::Output {
        let (a, _b) = self.0.overflowing_sub(rhs);
        RegWord(a)
    }
}

impl PartialEq<Word> for RegWord {
    fn eq(&self, other: &Word) -> bool {
        self.0 == *other
    }
}

impl From<Word> for RegWord {
    fn from(v: Word) -> Self {
        RegWord(v)
    }
}

impl Into<Word> for RegWord {
    fn into(self) -> Word {
        self.0
    }
}

impl ::std::ops::AddAssign<Address> for RegAddress {
    fn add_assign(&mut self, rhs: Address) {
        self.0 += rhs;
    }
}

impl ::std::ops::Add<Address> for RegAddress {
    type Output = RegAddress;

    fn add(self, rhs: Address) -> <Self as ::std::ops::Add<Address>>::Output {
        let (a, _b) = self.0.overflowing_add(rhs);
        RegAddress(a)
    }
}

impl ::std::ops::Sub<Address> for RegAddress {
    type Output = RegAddress;

    fn sub(self, rhs: Address) -> <Self as ::std::ops::Add<Address>>::Output {
        let (a, _b) = self.0.overflowing_sub(rhs);
        RegAddress(a)
    }
}

impl PartialEq<Address> for RegAddress {
    fn eq(&self, other: &Address) -> bool {
        self == &RegAddress::from(*other)
    }
}

impl From<Address> for RegAddress {
    fn from(v: Address) -> Self {
        RegAddress(v)
    }
}

impl From<(Word, Word)> for RegAddress {
    fn from(v: (Word, Word)) -> Self {
        let (h, l) = v;
        RegAddress((h as Address) << 8 | (l as Address))
    }
}

impl From<(RegWord, RegWord)> for RegAddress {
    fn from(v: (RegWord, RegWord)) -> Self {
        let (h, l) = v;
        (h.0, l.0).into()
    }
}

impl Into<Address> for RegAddress {
    fn into(self) -> Address {
        self.0
    }
}

const WORD_SIZE: u8 = 8;
const WORD_MASK: Address = 0xff;

impl From<RegAddress> for (RegWord, RegWord) {
    fn from(v: RegAddress) -> Self {
        let inner = v.0;
        ((((inner >> WORD_SIZE) & WORD_MASK) as Word).into(),
         ((inner & WORD_MASK) as Word).into())
    }
}

impl Into<(Word, Word)> for RegAddress {
    fn into(self) -> (Word, Word) {
        let a = self.0;
        ((a >> 8) as Word, (a & 0xff) as Word)
    }
}

#[derive(Default, Clone, Eq, PartialEq, Debug)]
struct State {
    a: RegWord,
    b: RegWord,
    c: RegWord,
    d: RegWord,
    e: RegWord,
    h: RegWord,
    l: RegWord,
    pc: RegAddress,
    sp: RegAddress,

    zero: bool,
    sign: bool,
    parity: bool,
    carry: bool,
}

impl State {
    fn set_a(&mut self, v: Word) {
        self.a = v.into();
    }

    fn set_b(&mut self, v: Word) {
        self.b = v.into();
    }

    fn set_c(&mut self, v: Word) {
        self.c = v.into();
    }
    fn set_d(&mut self, v: Word) {
        self.d = v.into();
    }

    fn set_e(&mut self, v: Word) {
        self.e = v.into();
    }
    fn set_h(&mut self, v: Word) {
        self.h = v.into();
    }

    fn set_l(&mut self, v: Word) {
        self.l = v.into();
    }

    fn set_pc(&mut self, v: Address) {
        self.pc = v.into();
    }

    fn set_sp<A: Into<RegAddress>>(&mut self, v: A) -> &mut Self {
        self.sp = v.into();
        self
    }
}

#[derive(Default, Clone)]
pub struct Cpu {
    state: State,
    m: RegWord,
}

/// Utilities
impl Cpu {
    fn mut_reg(&mut self, r: self::Reg) -> &mut RegWord {
        use self::Reg::*;
        match r {
            A => &mut self.state.a,
            B => &mut self.state.b,
            C => &mut self.state.c,
            D => &mut self.state.d,
            E => &mut self.state.e,
            H => &mut self.state.h,
            L => &mut self.state.l,
            M => &mut self.m,
        }
    }

    fn reg(&mut self, r: self::Reg) -> &RegWord {
        use self::Reg::*;
        match r {
            A => &self.state.a,
            B => &self.state.b,
            C => &self.state.c,
            D => &self.state.d,
            E => &self.state.e,
            H => &self.state.h,
            L => &self.state.l,
            M => &self.m,
        }
    }

    fn bc(&self) -> RegAddress {
        (self.state.b, self.state.c).into()
    }

    fn set_bc<A: Into<RegAddress>>(&mut self, val: A) {
        let (b, c) = val.into().into();
        self.state.b = b;
        self.state.c = c;
    }

    fn de(&self) -> RegAddress {
        (self.state.d, self.state.e).into()
    }

    fn set_de<A: Into<RegAddress>>(&mut self, val: A) {
        let (d, e) = val.into().into();
        self.state.d = d;
        self.state.e = e;
    }

    fn hl(&self) -> RegAddress {
        (self.state.h, self.state.l).into()
    }

    fn set_hl<A: Into<RegAddress>>(&mut self, val: A) {
        let (h, l) = val.into().into();
        self.state.h = h;
        self.state.l = l;
    }

    fn set_m(&mut self, val: Word) {
        self.m = val.into();
    }
}

/// Immediate Instructions
impl Cpu {
    fn lxi(&mut self, v: self::RegPairValue) {
        use self::RegPairValue::*;
        match v {
            BC(b, c) => {
                self.set_bc((b, c));
            }
            DE(d, e) => {
                self.set_de((d, e));
            }
            HL(h, l) => {
                self.set_hl((h, l));
            }
            SP(addr) => {
                self.state.set_sp(addr);
            }
        }
    }
}

/// Single Register Instructions
impl Cpu {
    fn cma(&mut self) {
        self.mut_reg(self::Reg::A).one_complement();
    }

    fn inr(&mut self, r: self::Reg) {
        self.mut_reg(r).increment();
        self.fix_static_flags(r);
    }

    fn dcr(&mut self, r: self::Reg) {
        self.mut_reg(r).decrement();
        self.fix_static_flags(r);
    }

    fn fix_static_flags(&mut self, r: Reg) {
        self.state.zero = self.reg(r).is_zero();
        self.state.sign = self.reg(r).sign_bit();
        self.state.parity = self.reg(r).parity();
    }
}

/// Register Pair Instructions
impl Cpu {
    fn inx(&mut self, rp: RegPair) -> () {
        use self::RegPair::*;
        match rp {
            BC => {
                let v = self.bc() + 1;
                self.set_bc(v);
            }
            DE => {
                let v = self.de() + 1;
                self.set_de(v);
            }
            HL => {
                let v = self.hl() + 1;
                self.set_hl(v);
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
                let v = self.bc() - 1;
                self.set_bc(v);
            }
            DE => {
                let v = self.de() - 1;
                self.set_de(v);
            }
            HL => {
                let v = self.hl() - 1;
                self.set_hl(v);
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
            Nop => {}
            Lxi(rp) => {
                self.lxi(rp);
            }
            Inx(rp) => {
                self.inx(rp)
            }
            Dcx(rp) => {
                self.dcx(rp)
            }
            Inr(r) => {
                self.inr(r)
            }
            Dcr(r) => {
                self.dcr(r)
            }
            Cma => {
                self.cma()
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

    trait CpuQuery {
        type Result: PartialEq + Eq;

        fn ask(&self, cpu: &Cpu) -> Self::Result;
    }

    #[derive(PartialEq, Clone, Copy)]
    enum WordReg {
        A,
        B,
        C,
        D,
        E,
        H,
        L,
        M,
    }

    struct SP;

    trait QueryResult: PartialEq + Eq + Clone + Copy + ::std::fmt::Debug {}

    impl CpuQuery for WordReg {
        type Result = Word;

        fn ask(&self, cpu: &Cpu) -> <Self as CpuQuery>::Result {
            use self::WordReg::*;
            match *self {
                A => cpu.state.a,
                B => cpu.state.b,
                C => cpu.state.c,
                D => cpu.state.d,
                E => cpu.state.e,
                H => cpu.state.h,
                L => cpu.state.l,
                M => cpu.m,
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

    impl QueryResult for bool {}

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
    instruction, start, expected,
    case(Unwrap("Lxi(RegPairValue::BC(0xae,0x02))"), 0x3245, 0x3248),
    case(Unwrap("Lxi(RegPairValue::DE(0xae,0x02))"), 0x1234, 0x1237),
    case(Unwrap("Lxi(RegPairValue::HL(0xae,0x02))"), 0x4321, 0x4324),
    case(Unwrap("Lxi(RegPairValue::SP(0xae02))"), 0x1010, 0x1013),
    )]
    fn lxi_should_advance_pc(instruction: Instruction, start: Address, expected: Address) {
        let mut cpu = CpuBuilder::default()
            .state(StateBuilder::default()
                .pc(start)
                .create()
            )
            .create();

        cpu.exec(instruction);

        assert_eq!(cpu.state.pc, expected);
    }

    #[rstest]
    fn lxi_bc(mut cpu: Cpu) {
        cpu.exec(Lxi(RegPairValue::BC(0xae, 0x02)));

        assert_eq!(cpu.state.b, 0xae);
        assert_eq!(cpu.state.c, 0x02);
    }

    #[rstest_parametrize(
    rp, query, expected,
    case(Unwrap("RegPairValue::BC(0xe4, 0xf1)"), Unwrap("(WordReg::B, WordReg::C)"), Unwrap("(0xe4, 0xf1)")),
    case(Unwrap("RegPairValue::DE(0x20, 0xb1)"), Unwrap("(WordReg::D, WordReg::E)"), Unwrap("(0x20, 0xb1)")),
    case(Unwrap("RegPairValue::HL(0x02, 0xae)"), Unwrap("(WordReg::H, WordReg::L)"), Unwrap("(0x02, 0xae)")),
    case(Unwrap("RegPairValue::SP(0x4321)"), Unwrap("SP"), 0x4321),
    )]
    fn lxi<R: QueryResult, Q: CpuQuery<Result=R>>(mut cpu: Cpu, rp: RegPairValue, query: Q, expected: R) {
        cpu.exec(Lxi(rp));

        assert_eq!(query.ask(&cpu), expected);
    }

    trait ApplyState {
        fn apply(self, cpu: &mut Cpu);
    }

    impl ApplyState for RegPairValue {
        fn apply(self, cpu: &mut Cpu) {
            use self::RegPairValue::*;
            use self::RegValue::*;
            match self {
                BC(b, c) => {
                    B(b).apply(cpu);
                    C(c).apply(cpu);
                }
                DE(d, e) => {
                    D(d).apply(cpu);
                    E(e).apply(cpu);
                }
                HL(h, l) => {
                    H(h).apply(cpu);
                    L(l).apply(cpu);
                }
                SP(sp) => {
                    cpu.state.set_sp(sp);
                }
            }
        }
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
        init.apply(&mut cpu);

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
        init.apply(&mut cpu);

        cpu.exec(Dcx(rp));

        assert_eq!(query.ask(&cpu), expected);
    }

    #[derive(Copy, Clone)]
    enum RegValue {
        A(Word),
        B(Word),
        C(Word),
        D(Word),
        E(Word),
        H(Word),
        L(Word),
        M(Word),
    }

    impl ApplyState for RegValue {
        fn apply(self, cpu: &mut Cpu) {
            use self::RegValue::*;
            match self {
                A(v) => { cpu.state.set_a(v); }
                B(v) => { cpu.state.set_b(v); }
                C(v) => { cpu.state.set_c(v); }
                D(v) => { cpu.state.set_d(v); }
                E(v) => { cpu.state.set_e(v); }
                H(v) => { cpu.state.set_h(v); }
                L(v) => { cpu.state.set_l(v); }
                M(v) => { cpu.set_m(v); }
            }
        }
    }

    #[derive(Copy, Clone)]
    enum StateFlag {
        Zero,
        Sign,
        Parity,
    }

    impl CpuQuery for StateFlag {
        type Result = bool;

        fn ask(&self, cpu: &Cpu) -> <Self as CpuQuery>::Result {
            use self::StateFlag::*;
            match *self {
                Zero => cpu.state.zero,
                Sign => cpu.state.sign,
                Parity => cpu.state.parity,
            }
        }
    }

    impl From<RegValue> for Reg {
        fn from(r: RegValue) -> Self {
            use self::RegValue::*;
            match r {
                A(_) => Reg::A,
                B(_) => Reg::B,
                C(_) => Reg::C,
                D(_) => Reg::D,
                E(_) => Reg::E,
                H(_) => Reg::H,
                L(_) => Reg::L,
                M(_) => Reg::M,
            }
        }
    }

    impl From<(Reg, Word)> for RegValue {
        fn from(vals: (Reg, u8)) -> Self {
            let (r, v) = vals;
            use self::Reg::*;
            match r {
                A => RegValue::A(v),
                B => RegValue::B(v),
                C => RegValue::C(v),
                D => RegValue::D(v),
                E => RegValue::E(v),
                H => RegValue::H(v),
                L => RegValue::L(v),
                M => RegValue::M(v),
            }
        }
    }

    impl From<Reg> for WordReg {
        fn from(r: Reg) -> Self {
            use self::Reg::*;
            match r {
                A => WordReg::A,
                B => WordReg::B,
                C => WordReg::C,
                D => WordReg::D,
                E => WordReg::E,
                H => WordReg::H,
                L => WordReg::L,
                M => WordReg::M,
            }
        }
    }


    impl CpuQuery for Reg {
        type Result = Word;

        fn ask(&self, cpu: &Cpu) -> <Self as CpuQuery>::Result {
            WordReg::from(*self).ask(&cpu)
        }
    }

    #[derive(Copy, Clone)]
    enum SRegCmd {
        Inr,
        Dcr,
    }

    impl From<(SRegCmd, Reg)> for Instruction {
        fn from(vals: (SRegCmd, Reg)) -> Self {
            let (cmd, r) = vals;
            match cmd {
                SRegCmd::Inr => Inr(r),
                SRegCmd::Dcr => Dcr(r),
            }
        }
    }

    #[rstest_parametrize(
    init, reg, query, expected,
    case(Unwrap("RegValue::A(0x33)"), Unwrap("Reg::A"), Unwrap("WordReg::A"), 0x34),
    case(Unwrap("RegValue::B(0x00)"), Unwrap("Reg::B"), Unwrap("WordReg::B"), 0x01),
    case(Unwrap("RegValue::C(0x23)"), Unwrap("Reg::C"), Unwrap("WordReg::C"), 0x24),
    case(Unwrap("RegValue::D(0xaf)"), Unwrap("Reg::D"), Unwrap("WordReg::D"), 0xb0),
    case(Unwrap("RegValue::E(0x01)"), Unwrap("Reg::E"), Unwrap("WordReg::E"), 0x02),
    case(Unwrap("RegValue::H(0xd1)"), Unwrap("Reg::H"), Unwrap("WordReg::H"), 0xd2),
    case(Unwrap("RegValue::L(0x53)"), Unwrap("Reg::L"), Unwrap("WordReg::L"), 0x54),
    case(Unwrap("RegValue::M(0x12)"), Unwrap("Reg::M"), Unwrap("WordReg::M"), 0x13),
    )]
    fn inr_should_increment_register<I, Q, R>(mut cpu: Cpu, init: I, reg: Reg, query: Q, expected: R)
        where I: ApplyState, R: QueryResult, Q: CpuQuery<Result=R>
    {
        init.apply(&mut cpu);

        cpu.inr(reg);

        assert_eq!(query.ask(&cpu), expected);
    }

    #[rstest]
    fn inr_should_set_flags(mut cpu: Cpu)
    {
        cpu.state.set_b(0xfd);

        cpu.inr(Reg::B);

        //0xfe
        assert!(!cpu.state.zero);
        assert!(cpu.state.sign);
        assert!(!cpu.state.parity);

        cpu.inr(Reg::B);

        //0xff
        assert!(!cpu.state.zero);
        assert!(cpu.state.sign);
        assert!(cpu.state.parity);

        cpu.inr(Reg::B);

        //0x00
        assert!(cpu.state.zero);
        assert!(!cpu.state.sign);
        assert!(cpu.state.parity);
    }

    #[rstest_parametrize(
    init, query, expected,
    case(Unwrap("RegValue::B(0x00)"), Unwrap("StateFlag::Zero"), Unwrap("true")),
    case(Unwrap("RegValue::B(0x12)"), Unwrap("StateFlag::Zero"), Unwrap("false")),
    case(Unwrap("RegValue::B(0xff)"), Unwrap("StateFlag::Zero"), Unwrap("false")),
    case(Unwrap("RegValue::A(0x80)"), Unwrap("StateFlag::Sign"), Unwrap("true")),
    case(Unwrap("RegValue::A(0xA3)"), Unwrap("StateFlag::Sign"), Unwrap("true")),
    case(Unwrap("RegValue::A(0xff)"), Unwrap("StateFlag::Sign"), Unwrap("true")),
    case(Unwrap("RegValue::A(0x00)"), Unwrap("StateFlag::Sign"), Unwrap("false")),
    case(Unwrap("RegValue::A(0x12)"), Unwrap("StateFlag::Sign"), Unwrap("false")),
    case(Unwrap("RegValue::A(0x7f)"), Unwrap("StateFlag::Sign"), Unwrap("false")),
    case(Unwrap("RegValue::D(0x00)"), Unwrap("StateFlag::Parity"), Unwrap("true")),
    case(Unwrap("RegValue::D(0x03)"), Unwrap("StateFlag::Parity"), Unwrap("true")),
    case(Unwrap("RegValue::D(0xff)"), Unwrap("StateFlag::Parity"), Unwrap("true")),
    case(Unwrap("RegValue::D(0x01)"), Unwrap("StateFlag::Parity"), Unwrap("false")),
    case(Unwrap("RegValue::D(0x1f)"), Unwrap("StateFlag::Parity"), Unwrap("false")),
    case(Unwrap("RegValue::D(0x37)"), Unwrap("StateFlag::Parity"), Unwrap("false")),
    )]
    fn static_flags<Q, R>(mut cpu: Cpu, init: RegValue, query: Q, expected: R)
        where R: QueryResult, Q: CpuQuery<Result=R>
    {
        init.apply(&mut cpu);

        cpu.fix_static_flags(init.into());

        assert_eq!(query.ask(&cpu), expected);
    }

    #[rstest]
    fn dcr_should_decrement_register(mut cpu: Cpu)
    {
        cpu.state.set_d(0x12);

        cpu.dcr(Reg::D);

        assert_eq!(cpu.state.d, 0x11);
    }

    #[rstest]
    fn dcr_should_set_flags(mut cpu: Cpu)
    {
        cpu.state.set_b(0x02);

        cpu.dcr(Reg::B);

        //0x01
        assert!(!cpu.state.zero);
        assert!(!cpu.state.sign);
        assert!(!cpu.state.parity);

        cpu.dcr(Reg::B);

        //0x00
        assert!(cpu.state.zero);
        assert!(!cpu.state.sign);
        assert!(cpu.state.parity);

        cpu.dcr(Reg::B);

        //0xff
        assert!(!cpu.state.zero);
        assert!(cpu.state.sign);
        assert!(cpu.state.parity);
    }

    #[rstest_parametrize(
    cmd, reg, before, after,
    case(Unwrap("SRegCmd::Inr"), Unwrap("Reg::A"), 0xa3, 0xa4),
    case(Unwrap("SRegCmd::Dcr"), Unwrap("Reg::B"), 0x32, 0x31),
    case(Unwrap("SRegCmd::Dcr"), Unwrap("Reg::M"), 0xaf, 0xae),
    )]
    fn single_register_command(mut cpu: Cpu, cmd: SRegCmd, reg: Reg, before: Word, after: Word) {
        RegValue::from((reg, before)).apply(&mut cpu);

        cpu.exec(Instruction::from((cmd, reg)));

        assert_eq!(reg.ask(&cpu), after);
    }

    #[rstest]
    fn cma(mut cpu: Cpu) {
        cpu.state.set_a(0x51);

        cpu.exec(Cma);

        assert_eq!(cpu.state.a, 0xae)
    }

    #[rstest]
    fn cma_should_not_set_flags(mut cpu: Cpu) {
        cpu.state.set_a(0xff);

        cpu.exec(Cma);

        assert!(!cpu.state.zero)
    }

    #[rstest]
    fn cma_should_not_change_flags(mut cpu: Cpu) {
        cpu.state.set_a(0xff);
        cpu.state.zero = true;

        cpu.exec(Cma);

        assert!(cpu.state.zero)
    }

    #[rstest]
    fn nop_should_just_change_pc(mut cpu: Cpu) {
        let mut state = cpu.state.clone();

        cpu.exec(Nop);

        assert_eq!(state.pc + 1, cpu.state.pc);

        state.pc += 1;

        assert_eq!(state, cpu.state);
    }
}
