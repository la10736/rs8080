use super::{
    Address, asm::{BytePair, Instruction, Instruction::*, Reg, RegPair, RegPairValue},
    Word,
};

#[derive(Default, Debug, Clone, Copy, Eq, PartialEq)]
struct RegWord {
    val: Word,
    carry: bool,
}

#[derive(Default, Debug, Clone, Copy, Eq, PartialEq)]
struct RegAddress(Address);

impl RegWord {
    fn increment(&mut self) { *self += 1; }
    fn decrement(&mut self) { *self -= 1; }
    fn one_complement(&mut self) { *self = (0xff ^ self.val).into() }
    fn is_zero(&self) -> bool { self.val == 0 }
    fn sign_bit(&self) -> bool { (self.val & 0x80) != 0x00 }
    fn parity(&self) -> bool { (self.val.count_ones() % 2) == 0 }
    fn rotate_left(&mut self) {
        self.carry = (self.val & 0x80) == 0x80;
        self.val = self.val.rotate_left(1);
    }
    fn rotate_right(&mut self) {
        self.carry = (self.val & 0x01) == 0x01;
        self.val = self.val.rotate_right(1);
    }
    fn update_flags(&self, flags: &mut Flags) {
        use self::Flag::*;

        for (f, v) in &[
            (Zero, self.is_zero()),
            (Sign, self.sign_bit()),
            (Parity, self.parity())
        ] {
            flags.val(*f, *v)
        }
    }
}

impl ::std::ops::AddAssign<Word> for RegWord {
    fn add_assign(&mut self, rhs: Word) {
        *self = self.val.overflowing_add(rhs).into();
    }
}

impl ::std::ops::SubAssign<Word> for RegWord {
    fn sub_assign(&mut self, rhs: u8) {
        *self = self.val.overflowing_sub(rhs).into();
    }
}

impl ::std::ops::Add<Word> for RegWord {
    type Output = RegWord;

    fn add(self, rhs: Word) -> <Self as ::std::ops::Add<Word>>::Output {
        let (val, carry) = self.val.overflowing_add(rhs);
        RegWord { val, carry }
    }
}

impl ::std::ops::Sub<Word> for RegWord {
    type Output = RegWord;

    fn sub(self, rhs: Word) -> <Self as ::std::ops::Sub<Word>>::Output {
        let (val, carry) = self.val.overflowing_sub(rhs);
        RegWord { val, carry }
    }
}

impl PartialEq<Word> for RegWord {
    fn eq(&self, other: &Word) -> bool {
        self.val == *other
    }
}

impl From<Word> for RegWord {
    fn from(val: Word) -> Self {
        RegWord { val, ..Default::default() }
    }
}

impl From<(Word, bool)> for RegWord {
    fn from(data: (Word, bool)) -> Self {
        RegWord { val: data.0, carry: data.1 }
    }
}

impl Into<Word> for RegWord {
    fn into(self) -> Word {
        self.val
    }
}

impl ::std::ops::AddAssign<Address> for RegAddress {
    fn add_assign(&mut self, rhs: Address) {
        self.0 += rhs;
    }
}

impl ::std::ops::SubAssign<Address> for RegAddress {
    fn sub_assign(&mut self, rhs: Address) {
        self.0 -= rhs;
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
        (h.val, l.val).into()
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
struct Flags (Word);

impl Flags {
    fn mask(f: Flag) -> Word {
        0x1 << Self::flag_bit(f)
    }

    fn flag_bit(f: Flag) -> u8 {
        use self::Flag::*;
        match f {
            Sign => 7,
            Zero => 6,
            AuxCarry => 4,
            Parity => 2,
            Carry => 0,
        }
    }

    fn get(&self, f: Flag) -> bool {
        (self.0 & Self::mask(f)) != 0
    }

    fn val(&mut self, f: Flag, val: bool) {
        if val {
            self.0 |= Self::mask(f)
        } else {
            self.0 &= !(Self::mask(f))
        }
    }

    fn set(&mut self, f: Flag) {
        self.val(f, true)
    }

    fn clear(&mut self, f: Flag) {
        self.val(f, false)
    }

    fn toggle(&mut self, f: Flag) {
        let toggled = !self.get(f);
        self.val(f, toggled)
    }
}

impl From<Word> for Flags {
    fn from(w: Word) -> Self {
        Flags(w)
    }
}

impl Into<Word> for Flags {
    fn into(self) -> Word {
        self.0
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

    flags: Flags,
}

#[derive(Copy, Clone)]
enum Flag {
    Sign,
    Zero,
    AuxCarry,
    Parity,
    Carry,
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
    fn set_sp<A: Into<RegAddress>>(&mut self, v: A) {
        self.sp = v.into();
    }
}

/// Flags stuff
impl State {
    fn flag(&self, f: Flag) -> bool {
        self.flags.get(f)
    }

    fn pack_flags(&self) -> Word {
        let mask: Word = self.flags.clone().into();
        0x02 | mask
    }
}

#[derive(Clone)]
struct MemoryBus {
    bank: [u8; 0x10000]
}

impl Default for MemoryBus {
    fn default() -> Self {
        MemoryBus { bank: [0; 0x10000] }
    }
}

impl MemoryBus {
    fn read_byte(&self, address: Address) -> u8 {
        self.bank[address as usize]
    }

    fn write_byte(&mut self, address: Address, val: u8) {
        self.bank[address as usize] = val
    }
}

#[derive(Default, Clone)]
pub struct Cpu {
    state: State,

    bus: MemoryBus,
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

    fn address(&self, rp: self::RegPair) -> Address {
        use self::RegPair::*;
        match rp {
            BC => self.bc(),
            DE => self.de(),
            HL => self.hl(),
            SP => self.state.sp,
        }.into()
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

    fn fix_static_flags(&mut self, r: Reg) {
        self.reg(r).clone().update_flags(&mut self.state.flags)
    }

    fn push_val(&mut self, val: Word) {
        self.state.sp -= 1;
        self.bus.write_byte(self.state.sp.into(), val);
    }

    fn push_reg(&mut self, r: Reg) {
        let val = self.reg(r).val;
        self.push_val(val);
    }

    fn push_flags(&mut self) {
        let val = self.state.pack_flags();
        self.push_val(val);
    }
}

/// Carry bit Instructions
impl Cpu {
    fn nop(&mut self) {}
}

/// Carry bit Instructions
impl Cpu {
    fn cmc(&mut self) {
        self.state.flags.toggle(Flag::Carry);
    }
    fn stc(&mut self) {
        self.state.flags.set(Flag::Carry);
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
}

/// Data transfer Instruction
impl Cpu {
    fn mov(&mut self, f: Reg, t: Reg) {
        *self.mut_reg(t) = *self.reg(f)
    }

    fn stax(&mut self, rp: RegPair) {
        let address = self.address(rp);
        self.bus.write_byte(address, self.state.a.into());
    }

    fn ldax(&mut self, rp: RegPair) {
        let address = self.address(rp);
        self.state.a = self.bus.read_byte(address).into();
    }
}

/// Carry
impl Cpu {
    fn carry(&self) -> bool {
        self.state.flag(Flag::Carry)
    }

    fn carry_clear(&mut self) {
        self.set_carry_state(false)
    }

    fn carry_set(&mut self) {
        self.set_carry_state(true)
    }

    fn set_carry_state(&mut self, state: bool) {
        self.state.flags.val(Flag::Carry, state)
    }

    fn store_a_carry(&mut self) {
        let val = self.state.a.carry;
        self.set_carry_state(val)
    }
}

/// Accumulator
impl Cpu {
    fn add(&mut self, r: Reg) {
        self.state.a += self.reg(r).val;
        self.store_a_carry();
        self.fix_static_flags(Reg::A)
    }

    fn adc(&mut self, r: Reg) {
        let v = self.reg(r).val;
        self.state.a += if self.carry() { v + 1 } else { v };
        self.store_a_carry();
        self.fix_static_flags(Reg::A)
    }

    fn sub(&mut self, r: Reg) {
        self.state.a -= self.reg(r).val;
        self.store_a_carry();
        self.fix_static_flags(Reg::A)
    }

    fn sbb(&mut self, r: Reg) {
        let v = self.reg(r).val;
        self.state.a -= if self.carry() { v + 1 } else { v };
        self.store_a_carry();
        self.fix_static_flags(Reg::A)
    }

    fn ana(&mut self, r: Reg) {
        self.state.a = (self.state.a.val & self.reg(r).val).into();
        self.fix_static_flags(Reg::A);
        self.carry_clear();
    }

    fn xra(&mut self, r: Reg) {
        self.state.a = (self.state.a.val ^ self.reg(r).val).into();
        self.fix_static_flags(Reg::A);
        self.carry_clear();
    }

    fn ora(&mut self, r: Reg) {
        self.state.a = (self.state.a.val | self.reg(r).val).into();
        self.fix_static_flags(Reg::A);
        self.carry_clear();
    }

    fn cmp(&mut self, r: Reg) {
        let old = self.state.a;
        self.sub(r);
        self.state.a = old;
    }
}

const RIGHT_BIT: u8 = 0;
const LEFT_BIT: u8 = 7;

/// Rotate
impl Cpu {
    fn rlc(&mut self) {
        self.state.a.rotate_left();
        self.store_a_carry();
    }
    fn rrc(&mut self) {
        self.state.a.rotate_right();
        self.store_a_carry();
    }
    fn ral(&mut self) {
        self.state.a.rotate_left();
        self.swap_carry_bit(RIGHT_BIT);
    }
    fn rar(&mut self) {
        self.state.a.rotate_right();
        self.swap_carry_bit(LEFT_BIT);
    }

    fn swap_carry_bit(&mut self, bit: u8) {
        let bit_mask = 0x1 << bit;
        let new_carry = (self.state.a.val & bit_mask) == bit_mask;
        if self.carry() {
            self.state.a.val |= bit_mask;
        } else {
            self.state.a.val &= !bit_mask;
        }
        self.set_carry_state(new_carry)
    }
}

/// Register Pair Instructions
impl Cpu {
    fn push(&mut self, bp: BytePair) {
        use self::BytePair::*;
        match bp {
            BC => {
                self.push_reg(Reg::B);
                self.push_reg(Reg::C);
            }
            DE => {
                self.push_reg(Reg::D);
                self.push_reg(Reg::E);
            }
            HL => {
                self.push_reg(Reg::H);
                self.push_reg(Reg::L);
            }
            AF => {
                self.push_reg(Reg::A);
                self.push_flags();
            }
        }
    }

    fn inx(&mut self, rp: RegPair) {
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

    fn dcx(&mut self, rp: RegPair) {
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
    pub fn exec(&mut self, instruction: Instruction) {
        match instruction {
            Cmc => {
                self.cmc()
            }
            Stc => {
                self.stc()
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
            Daa => {
                unimplemented!("Because nobody use it!")
            }
            Nop => {
                self.nop()
            }
            Mov(f, t) => {
                self.mov(f, t)
            }
            Stax(rp) if rp.is_basic() => {
                self.stax(rp)
            }
            Ldax(rp) if rp.is_basic() => {
                self.ldax(rp)
            }
            Add(r) => {
                self.add(r)
            }
            Adc(r) => {
                self.adc(r)
            }
            Sub(r) => {
                self.sub(r)
            }
            Sbb(r) => {
                self.sbb(r)
            }
            Ana(r) => {
                self.ana(r)
            }
            Xra(r) => {
                self.xra(r)
            }
            Ora(r) => {
                self.ora(r)
            }
            Cmp(r) => {
                self.cmp(r)
            }
            Rlc => {
                self.rlc()
            }
            Rrc => {
                self.rrc()
            }
            Ral => {
                self.ral()
            }
            Rar => {
                self.rar()
            }
            Push(bp) => {
                self.push(bp)
            }
            // Continue
            Inx(rp) => {
                self.inx(rp)
            }
            Dcx(rp) => {
                self.dcx(rp)
            }
            Lxi(rp) => {
                self.lxi(rp)
            }
            _ => unimplemented!("Instruction {:?} not implemented yet!", instruction)
        }
        self.state.pc += instruction.length();
    }
}

#[cfg(test)]
mod test {
    use rstest::rstest;
    use rstest::rstest_parametrize;
    use super::*;
    use self::Flag::*;

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

    trait ApplyState {
        fn apply(&self, cpu: &mut Cpu);
    }

    impl ApplyState for RegPairValue {
        fn apply(&self, cpu: &mut Cpu) {
            use self::RegPairValue::*;
            use self::RegValue::*;
            match self {
                BC(b, c) => {
                    B(*b).apply(cpu);
                    C(*c).apply(cpu);
                }
                DE(d, e) => {
                    D(*d).apply(cpu);
                    E(*e).apply(cpu);
                }
                HL(h, l) => {
                    H(*h).apply(cpu);
                    L(*l).apply(cpu);
                }
                SP(sp) => {
                    cpu.state.set_sp(*sp);
                }
            }
        }
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
        fn apply(&self, cpu: &mut Cpu) {
            use self::RegValue::*;
            match self {
                A(v) => { cpu.state.set_a(*v); }
                B(v) => { cpu.state.set_b(*v); }
                C(v) => { cpu.state.set_c(*v); }
                D(v) => { cpu.state.set_d(*v); }
                E(v) => { cpu.state.set_e(*v); }
                H(v) => { cpu.state.set_h(*v); }
                L(v) => { cpu.state.set_l(*v); }
                M(v) => { cpu.set_m(*v); }
            }
        }
    }

    impl CpuQuery for Flag {
        type Result = bool;

        fn ask(&self, cpu: &Cpu) -> <Self as CpuQuery>::Result {
            cpu.state.flag(*self)
        }
    }

    impl ApplyState for Flag {
        fn apply(&self, cpu: &mut Cpu) {
            cpu.state.flags.set(*self)
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
        Add,
        Adc,
        Sub,
        Sbb,
        Ana,
        Xra,
        Ora,
    }

    impl From<(SRegCmd, Reg)> for Instruction {
        fn from(vals: (SRegCmd, Reg)) -> Self {
            let (cmd, r) = vals;
            match cmd {
                SRegCmd::Inr => Inr(r),
                SRegCmd::Dcr => Dcr(r),
                SRegCmd::Add => Add(r),
                SRegCmd::Adc => Adc(r),
                SRegCmd::Sub => Sub(r),
                SRegCmd::Sbb => Sbb(r),
                SRegCmd::Ana => Ana(r),
                SRegCmd::Xra => Xra(r),
                SRegCmd::Ora => Ora(r),
            }
        }
    }

    mod pair_register {
        use super::*;

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

        #[rstest]
        fn push_should_put_register_pair_on_the_stack(mut cpu: Cpu) {
            cpu.set_de(0x8f9d);
            cpu.state.set_sp(0x321c);

            cpu.exec(Push(BytePair::DE));

            assert_eq!(cpu.bus.read_byte(0x321c - 1), 0x8f);
            assert_eq!(cpu.bus.read_byte(0x321c - 2), 0x9d);
        }

        #[rstest]
        fn push_should_decrease_stack_by_two(mut cpu: Cpu) {
            cpu.state.set_sp(0x1233);

            cpu.exec(Push(BytePair::BC));

            assert_eq!(cpu.state.sp, 0x1231)
        }

        #[rstest]
        fn push_should_save_accumulator_and_flags(mut cpu: Cpu) {
            let sp = 0x321c;
            cpu.state.set_sp(sp);
            cpu.state.set_a(0xde);
            cpu.state.flags.set(Carry);
            cpu.state.flags.set(Zero);

            cpu.exec(Push(BytePair::AF));

            assert_eq!(cpu.bus.read_byte(0x321c - 1), 0xde);
            assert_eq!(cpu.bus.read_byte(0x321c - 2), 0x43);
        }

        impl ApplyState for () {
            fn apply(&self, cpu: &mut Cpu) {}
        }

        #[rstest_parametrize(
        init, expected,
        case(Unwrap("()"), 0x02),
        case(Sign, 0x82),
        case(Zero, 0x42),
        case(AuxCarry, 0x12),
        case(Parity, 0x06),
        case(Carry, 0x03),
        )]
        fn push_should_store_flags_correctly<I: ApplyState>(mut cpu: Cpu, init: I, expected: Word) {
            init.apply(&mut cpu);

            let flags = cpu.state.pack_flags();

            assert_eq!(flags, expected);
        }
    }

    #[rstest_parametrize(
    init, query, expected,
    case(Unwrap("RegValue::B(0x00)"), Unwrap("Flag::Zero"), true),
    case(Unwrap("RegValue::B(0x12)"), Unwrap("Flag::Zero"), false),
    case(Unwrap("RegValue::B(0xff)"), Unwrap("Flag::Zero"), false),
    case(Unwrap("RegValue::A(0x80)"), Unwrap("Flag::Sign"), true),
    case(Unwrap("RegValue::A(0xA3)"), Unwrap("Flag::Sign"), true),
    case(Unwrap("RegValue::A(0xff)"), Unwrap("Flag::Sign"), true),
    case(Unwrap("RegValue::A(0x00)"), Unwrap("Flag::Sign"), false),
    case(Unwrap("RegValue::A(0x12)"), Unwrap("Flag::Sign"), false),
    case(Unwrap("RegValue::A(0x7f)"), Unwrap("Flag::Sign"), false),
    case(Unwrap("RegValue::D(0x00)"), Unwrap("Flag::Parity"), true),
    case(Unwrap("RegValue::D(0x03)"), Unwrap("Flag::Parity"), true),
    case(Unwrap("RegValue::D(0xff)"), Unwrap("Flag::Parity"), true),
    case(Unwrap("RegValue::D(0x01)"), Unwrap("Flag::Parity"), false),
    case(Unwrap("RegValue::D(0x1f)"), Unwrap("Flag::Parity"), false),
    case(Unwrap("RegValue::D(0x37)"), Unwrap("Flag::Parity"), false),
    )]
    fn static_flags<Q, R>(mut cpu: Cpu, init: RegValue, query: Q, expected: R)
        where R: QueryResult, Q: CpuQuery<Result=R>
    {
        init.apply(&mut cpu);

        cpu.fix_static_flags(init.into());

        assert_eq!(query.ask(&cpu), expected);
    }

    mod single_register {
        use super::*;

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
        fn inr_should_update_flags(mut cpu: Cpu)
        {
            cpu.state.set_b(0xfd);

            cpu.inr(Reg::B);

            //0xfe
            assert!(!cpu.state.flag(Zero));
            assert!(cpu.state.flag(Sign));
            assert!(!cpu.state.flag(Parity));

            cpu.inr(Reg::B);

            //0xff
            assert!(!cpu.state.flag(Zero));
            assert!(cpu.state.flag(Sign));
            assert!(cpu.state.flag(Parity));

            cpu.inr(Reg::B);

            //0x00
            assert!(cpu.state.flag(Zero));
            assert!(!cpu.state.flag(Sign));
            assert!(cpu.state.flag(Parity));
        }

        #[rstest]
        fn inr_should_not_care_carry_flag(mut cpu: Cpu) {
            cpu.state.set_b(0xff);

            cpu.inr(Reg::B);

            assert!(!cpu.carry());

            cpu.carry_set();

            cpu.inr(Reg::B);

            assert!(cpu.carry());
        }

        #[rstest]
        fn dcr_should_decrement_register(mut cpu: Cpu)
        {
            cpu.state.set_d(0x12);

            cpu.dcr(Reg::D);

            assert_eq!(cpu.state.d, 0x11);
        }

        #[rstest]
        fn dcr_should_update_flags(mut cpu: Cpu)
        {
            cpu.state.set_b(0x02);

            cpu.dcr(Reg::B);

            //0x01
            assert!(!cpu.state.flag(Zero));
            assert!(!cpu.state.flag(Sign));
            assert!(!cpu.state.flag(Parity));

            cpu.dcr(Reg::B);

            //0x00
            assert!(cpu.state.flag(Zero));
            assert!(!cpu.state.flag(Sign));
            assert!(cpu.state.flag(Parity));

            cpu.dcr(Reg::B);

            //0xff
            assert!(!cpu.state.flag(Zero));
            assert!(cpu.state.flag(Sign));
            assert!(cpu.state.flag(Parity));
        }

        #[rstest]
        fn dcr_should_not_care_carry_flag(mut cpu: Cpu) {
            cpu.state.set_b(0x00);

            cpu.dcr(Reg::B);

            assert!(!cpu.carry());

            cpu.carry_set();

            cpu.dcr(Reg::B);

            assert!(cpu.carry());
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
    }

    mod data_transfer {
        use super::*;

        #[rstest]
        fn mov_should_move_register_content(mut cpu: Cpu) {
            RegValue::A(0x33).apply(&mut cpu);

            cpu.exec(Mov(Reg::A, Reg::B));

            assert_eq!(Reg::B.ask(&cpu), 0x33)
        }

        #[rstest]
        fn mov_should_move_not_change_source(mut cpu: Cpu) {
            RegValue::D(0xfa).apply(&mut cpu);

            cpu.exec(Mov(Reg::D, Reg::C));

            assert_eq!(Reg::D.ask(&cpu), 0xfa)
        }

        #[rstest]
        fn stax_should_store_accumulator(mut cpu: Cpu) {
            cpu.set_bc(0x3f16);
            cpu.state.set_a(0x32);

            cpu.exec(Stax(RegPair::BC));

            assert_eq!(cpu.bus.read_byte(0x3f16), 0x32);
        }

        #[rstest_parametrize(
        reg_pair,
        case(Unwrap("RegPair::HL")),
        case(Unwrap("RegPair::SP")),
        )]
        #[should_panic]
        fn stax_should_panic(mut cpu: Cpu, reg_pair: RegPair) {
            cpu.exec(Stax(reg_pair));
        }

        #[rstest]
        fn ldax_should_load_accumulator(mut cpu: Cpu) {
            let address = 0x78a2;
            cpu.bus.write_byte(address, 0x21);
            cpu.set_de(address);
            cpu.state.set_a(0xad);

            cpu.exec(Ldax(RegPair::DE));

            assert_eq!(cpu.state.a, 0x21);
        }

        #[rstest_parametrize(
        reg_pair,
        case(Unwrap("RegPair::HL")),
        case(Unwrap("RegPair::SP")),
        )]
        #[should_panic]
        fn ldax_should_panic(mut cpu: Cpu, reg_pair: RegPair) {
            cpu.exec(Ldax(reg_pair));
        }
    }

    mod accumulator {
        use super::*;

        #[rstest_parametrize(
        start, r, v, expected,
        case(0x12, Unwrap("Reg::B"), 0xa0, 0xb2),
        case(0x54, Unwrap("Reg::M"), 0x33, 0x87),
        case(0xfd, Unwrap("Reg::C"), 0x04, 0x01),
        )]
        fn add_should_perform_addition(mut cpu: Cpu, start: Word, r: Reg, v: Word, expected: Word) {
            cpu.state.set_a(start);
            RegValue::from((r, v)).apply(&mut cpu);

            cpu.exec(Add(r));

            assert_eq!(cpu.state.a, expected);
        }

        #[rstest]
        fn add_should_double_accumulator_when_refer_itself(mut cpu: Cpu) {
            cpu.state.set_a(0x26);

            cpu.exec(Add(Reg::A));

            assert_eq!(cpu.state.a, 0x4c);
        }

        #[rstest]
        fn add_should_update_the_carry_flag(mut cpu: Cpu) {
            cpu.state.set_a(0xf0);
            cpu.state.set_b(0x13);

            cpu.add(Reg::B);

            assert!(cpu.carry());
        }

        #[rstest_parametrize(
        start, carry, r, v, expected,
        case(0x12, false, Unwrap("Reg::B"), 0xa0, 0xb2),
        case(0x12, true, Unwrap("Reg::B"), 0xa0, 0xb3),
        case(0x54, false, Unwrap("Reg::M"), 0x33, 0x87),
        case(0x54, true, Unwrap("Reg::M"), 0x33, 0x88),
        case(0xfd, false, Unwrap("Reg::C"), 0x04, 0x01),
        case(0xfd, true, Unwrap("Reg::C"), 0x04, 0x02),
        )]
        fn adc_should_perform_addition_by_care_carry_flag(mut cpu: Cpu, start: Word, carry: bool,
                                                          r: Reg, v: Word, expected: Word) {
            cpu.state.set_a(start);
            cpu.set_carry_state(carry);
            RegValue::from((r, v)).apply(&mut cpu);

            cpu.exec(Adc(r));

            assert_eq!(cpu.state.a, expected);
        }

        #[rstest_parametrize(
        carry, v, expected,
        case(false, 0x01, false),
        case(true, 0x01, false),
        case(false, 0x0f, false),
        case(true, 0x0f, true),
        case(false, 0x10, true),
        case(true, 0x10, true),
        )]
        fn adc_should_update_carry_flag(mut cpu: Cpu, carry: bool, v: Word, expected: bool) {
            cpu.state.set_a(0xf0);
            cpu.set_carry_state(carry);
            cpu.state.set_b(v);

            cpu.exec(Adc(Reg::B));

            assert_eq!(cpu.carry(), expected);
        }

        #[rstest_parametrize(
        start, r, v, expected,
        case(0xfd, Unwrap("Reg::C"), 0x04, 0xf9),
        case(0x12, Unwrap("Reg::B"), 0xa0, 0x72),
        case(0x54, Unwrap("Reg::M"), 0x33, 0x21),
        )]
        fn sub_should_perform_subtraction(mut cpu: Cpu, start: Word, r: Reg, v: Word, expected: Word) {
            cpu.state.set_a(start);
            RegValue::from((r, v)).apply(&mut cpu);

            cpu.exec(Sub(r));

            assert_eq!(cpu.state.a, expected);
        }

        #[rstest]
        fn sub_should_update_carry_flag(mut cpu: Cpu) {
            cpu.state.set_a(0x10);
            cpu.state.set_b(0x13);

            cpu.sub(Reg::B);

            assert!(cpu.carry());
        }

        #[rstest_parametrize(
        start, carry, r, v, expected,
        case(0xa2, false, Unwrap("Reg::B"), 0xa0, 0x02),
        case(0xa2, true, Unwrap("Reg::B"), 0xa0, 0x01),
        case(0x33, false, Unwrap("Reg::M"), 0x54, 0xdf),
        case(0x33, true, Unwrap("Reg::M"), 0x54, 0xde),
        case(0xfd, false, Unwrap("Reg::C"), 0x04, 0xf9),
        case(0xfd, true, Unwrap("Reg::C"), 0x04, 0xf8),
        )]
        fn sbb_should_perform_subtraction_by_care_carry_flag(mut cpu: Cpu, start: Word, carry: bool,
                                                             r: Reg, v: Word, expected: Word) {
            cpu.state.set_a(start);
            cpu.set_carry_state(carry);
            RegValue::from((r, v)).apply(&mut cpu);

            cpu.sbb(r);

            assert_eq!(cpu.state.a, expected);
        }

        #[rstest_parametrize(
        carry, v, expected,
        case(false, 0x01, false),
        case(true, 0x01, false),
        case(false, 0x10, false),
        case(true, 0x10, true),
        case(false, 0x11, true),
        case(true, 0x11, true),
        )]
        fn sbb_should_update_carry_flag(mut cpu: Cpu, carry: bool, v: Word, expected: bool) {
            cpu.state.set_a(0x10);
            cpu.set_carry_state(carry);
            cpu.state.set_b(v);

            cpu.sbb(Reg::B);

            assert_eq!(cpu.carry(), expected);
        }

        #[rstest_parametrize(
        start, r, v, expected,
        case(0x81, Unwrap("Reg::D"), 0x7e, 0x00),
        case(0xa6, Unwrap("Reg::E"), 0xa2, 0xa2),
        case(0x5a, Unwrap("Reg::M"), 0xff, 0x5a),
        )]
        fn ana_should_perform_logical_and(mut cpu: Cpu, start: Word, r: Reg, v: Word, expected: Word) {
            cpu.state.set_a(start);
            RegValue::from((r, v)).apply(&mut cpu);

            cpu.exec(Ana(r));

            assert_eq!(cpu.state.a, expected);
        }

        #[rstest_parametrize(
        start, r, v, expected,
        case(0x81, Unwrap("Reg::H"), 0x7e, 0xff),
        case(0xa6, Unwrap("Reg::L"), 0xa2, 0x04),
        case(0x5a, Unwrap("Reg::B"), 0xff, 0xa5),
        )]
        fn xra_should_perform_logical_xor(mut cpu: Cpu, start: Word, r: Reg, v: Word, expected: Word) {
            cpu.state.set_a(start);
            RegValue::from((r, v)).apply(&mut cpu);

            cpu.exec(Xra(r));

            assert_eq!(cpu.state.a, expected);
        }

        #[rstest_parametrize(
        start, r, v, expected,
        case(0x81, Unwrap("Reg::H"), 0x7e, 0xff),
        case(0xa6, Unwrap("Reg::L"), 0xa2, 0xa6),
        case(0x01, Unwrap("Reg::B"), 0x80, 0x81),
        case(0x00, Unwrap("Reg::B"), 0x00, 0x00),
        )]
        fn ora_should_perform_logical_or(mut cpu: Cpu, start: Word, r: Reg, v: Word, expected: Word) {
            cpu.state.set_a(start);
            RegValue::from((r, v)).apply(&mut cpu);

            cpu.exec(Ora(r));

            assert_eq!(cpu.state.a, expected);
        }

        #[rstest_parametrize(
        a, r, v, zero, carry,
        case(0x34, Unwrap("Reg::B"), 0x34, true, false),
        case(0x10, Unwrap("Reg::M"), 0x12, false, true),
        case(0x33, Unwrap("Reg::E"), 0x32, false, false),
        )]
        fn cmp_should(mut cpu: Cpu, a: Word, r: Reg, v: Word, zero: bool, carry: bool) {
            cpu.state.set_a(a);
            RegValue::from((r, v)).apply(&mut cpu);

            cpu.exec(Cmp(r));

            assert_eq!(cpu.state.flag(Zero), zero);
            assert_eq!(cpu.carry(), carry);
            assert_eq!(cpu.state.a, a);
            assert_eq!(*cpu.reg(r), v);
        }

        #[rstest_parametrize(
        op, a, b,
        case(Unwrap("SRegCmd::Add"), 0xfe, 0x01),
        case(Unwrap("SRegCmd::Adc"), 0xfe, 0x01),
        case(Unwrap("SRegCmd::Sub"), 0x01, 0x02),
        case(Unwrap("SRegCmd::Sbb"), 0x01, 0x02),
        case(Unwrap("SRegCmd::Ana"), 0xfe, 0xf6),
        case(Unwrap("SRegCmd::Xra"), 0xfe, 0x07),
        case(Unwrap("SRegCmd::Ora"), 0xfe, 0x07),
        )]
        fn should_update_flags(mut cpu: Cpu, op: SRegCmd, a: Word, b: Word) {
            cpu.state.set_a(a);
            cpu.state.set_b(b);
            cpu.state.flags.set(Zero);

            let cmd = (op, Reg::B).into();
            cpu.exec(cmd);

            assert!(!cpu.state.flag(Zero));
            assert!(cpu.state.flag(Sign));
            assert!(cpu.state.flag(Parity));
        }

        #[rstest_parametrize(
        op,
        case(Unwrap("SRegCmd::Ana")),
        case(Unwrap("SRegCmd::Xra")),
        case(Unwrap("SRegCmd::Ora")),
        )]
        fn should_reset_carry_flag(mut cpu: Cpu, op: SRegCmd) {
            cpu.state.set_a(0xae);
            cpu.state.set_b(0x36);
            cpu.carry_set();

            let cmd = (op, Reg::B).into();

            cpu.exec(cmd);

            assert!(!cpu.carry());

            cpu.exec(cmd);

            assert!(!cpu.carry());
        }
    }

    mod carry_bit {
        use super::*;

        #[rstest]
        fn cmc_should_reverse_carry_bit(mut cpu: Cpu) {
            cpu.carry_clear();

            cpu.exec(Cmc);

            assert!(cpu.carry());

            cpu.exec(Cmc);

            assert!(!cpu.carry());
        }

        #[rstest]
        fn stc_should_set_carry_bit(mut cpu: Cpu) {
            cpu.carry_clear();

            cpu.exec(Stc);

            assert!(cpu.carry());

            cpu.exec(Stc);

            assert!(cpu.carry());
        }
    }

    mod rotate_accumulator {
        use super::*;

        #[rstest_parametrize(
        op, before, after, carry,
        case(Rlc, 0xf2, 0xe5, true),
        case(Rlc, 0x00, 0x00, false),
        case(Rlc, 0xff, 0xff, true),
        case(Rlc, 0x80, 0x01, true),
        case(Rrc, 0xf3, 0xf9, true),
        case(Rrc, 0x00, 0x00, false),
        case(Rrc, 0xff, 0xff, true),
        case(Rrc, 0x01, 0x80, true),
        )]
        fn should_rotate_accumulator(mut cpu: Cpu, op: Instruction, before: Word,
                                     after: Word, carry: bool) {
            cpu.state.set_a(before);

            cpu.exec(op);

            assert_eq!(cpu.state.a, after);
            assert_eq!(cpu.carry(), carry);
        }

        #[rstest_parametrize(
        op, before, carry_before, after, carry,
        case(Ral, 0xf2, true, 0xe5, true),
        case(Ral, 0x00, true, 0x01, false),
        case(Ral, 0x00, false, 0x00, false),
        case(Ral, 0xff, true, 0xff, true),
        case(Ral, 0x80, false, 0x00, true),
        case(Rar, 0xf3, true, 0xf9, true),
        case(Rar, 0x00, true, 0x80, false),
        case(Rar, 0x00, false, 0x00, false),
        case(Rar, 0xff, true, 0xff, true),
        case(Rar, 0x01, false, 0x00, true),
        )]
        fn should_rotate_accumulator_through_carry_bit(mut cpu: Cpu, op: Instruction,
                                                       before: Word, carry_before: bool,
                                                       after: Word, carry: bool) {
            cpu.set_carry_state(carry_before);
            cpu.state.set_a(before);

            cpu.exec(op);

            assert_eq!(cpu.state.a, after);
            assert_eq!(cpu.carry(), carry);
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

        assert!(!cpu.state.flag(Zero))
    }

    #[rstest]
    fn cma_should_not_change_flags(mut cpu: Cpu) {
        cpu.state.set_a(0xff);
        cpu.state.flags.set(Zero);

        cpu.exec(Cma);

        assert!(cpu.state.flag(Zero))
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
