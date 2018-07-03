use ::std::mem::swap;
use self::Flag::*;
use super::{
    Address, asm::{BytePair, Instruction, Instruction::*, Reg, RegPair, RegPairValue},
    Byte,
};

#[derive(Default, Debug, Clone, Copy, Eq, PartialEq)]
struct RegByte {
    val: Byte
}

#[derive(Default, Debug, Clone, Copy, Eq, PartialEq)]
struct RegAddress {
    val: Address,
}

impl RegByte {
    fn overflow_add(&mut self, val: Byte) -> bool {
        let (val, carry) = self.val.overflowing_add(val);
        self.val = val;
        carry
    }
    fn overflow_sub(&mut self, val: Byte) -> bool {
        let (val, carry) = self.val.overflowing_sub(val);
        self.val = val;
        carry
    }
    fn increment(&mut self) -> bool { self.overflow_add(1) }
    fn decrement(&mut self) -> bool { self.overflow_sub(1) }
    fn one_complement(&mut self) { *self = (0xff ^ self.val).into() }
    fn is_zero(&self) -> bool { self.val == 0 }
    fn sign_bit(&self) -> bool { (self.val & 0x80) != 0x00 }
    fn parity(&self) -> bool { (self.val.count_ones() % 2) == 0 }
    fn rotate_left(&mut self) -> bool {
        let carry = (self.val & 0x80) == 0x80;
        self.val = self.val.rotate_left(1);
        carry
    }
    fn rotate_right(&mut self) -> bool {
        let carry = (self.val & 0x01) == 0x01;
        self.val = self.val.rotate_right(1);
        carry
    }
    fn update_flags(&self, flags: &mut Flags) {
        for (f, v) in &[
            (Zero, self.is_zero()),
            (Sign, self.sign_bit()),
            (Parity, self.parity())
        ] {
            flags.val(*f, *v)
        }
    }
    fn low(&self) -> Byte {
        self.val & 0x0f
    }
    fn high(&self) -> Byte {
        self.val & 0xf0
    }
}

impl ::std::ops::AddAssign<Byte> for RegByte {
    fn add_assign(&mut self, rhs: Byte) {
        *self = *self + rhs;
    }
}

impl ::std::ops::SubAssign<Byte> for RegByte {
    fn sub_assign(&mut self, rhs: u8) {
        *self = *self - rhs;
    }
}

impl ::std::ops::Add<Byte> for RegByte {
    type Output = RegByte;

    fn add(self, rhs: Byte) -> <Self as ::std::ops::Add<Byte>>::Output {
        let (val, _) = self.val.overflowing_add(rhs);
        RegByte { val }
    }
}

impl ::std::ops::Sub<Byte> for RegByte {
    type Output = RegByte;

    fn sub(self, rhs: Byte) -> <Self as ::std::ops::Sub<Byte>>::Output {
        let (val, _) = self.val.overflowing_sub(rhs);
        RegByte { val }
    }
}

impl PartialEq<Byte> for RegByte {
    fn eq(&self, other: &Byte) -> bool {
        self.val == *other
    }
}

impl From<Byte> for RegByte {
    fn from(val: Byte) -> Self {
        RegByte { val }
    }
}

impl Into<Byte> for RegByte {
    fn into(self) -> Byte {
        self.val
    }
}

impl RegAddress {
    fn overflow_add(&mut self, val: Address) -> bool {
        let (val, carry) = self.val.overflowing_add(val);
        self.val = val;
        carry
    }
    fn overflow_sub(&mut self, val: Address) -> bool {
        let (val, carry) = self.val.overflowing_sub(val);
        self.val = val;
        carry
    }
}

impl PartialEq<Address> for RegAddress {
    fn eq(&self, other: &Address) -> bool {
        self == &RegAddress::from(*other)
    }
}

impl From<Address> for RegAddress {
    fn from(val: Address) -> Self {
        RegAddress { val, ..Default::default() }
    }
}

impl From<(Byte, Byte)> for RegAddress {
    fn from(v: (Byte, Byte)) -> Self {
        let (h, l) = v;
        ((h as Address) << 8 | (l as Address)).into()
    }
}

impl From<(RegByte, RegByte)> for RegAddress {
    fn from(v: (RegByte, RegByte)) -> Self {
        let (h, l) = v;
        (h.val, l.val).into()
    }
}

impl Into<Address> for RegAddress {
    fn into(self) -> Address {
        self.val
    }
}

const WORD_SIZE: u8 = 8;
const WORD_MASK: Address = 0xff;

impl From<RegAddress> for (RegByte, RegByte) {
    fn from(v: RegAddress) -> Self {
        let inner = v.val;
        ((((inner >> WORD_SIZE) & WORD_MASK) as Byte).into(),
         ((inner & WORD_MASK) as Byte).into())
    }
}

impl Into<(Byte, Byte)> for RegAddress {
    fn into(self) -> (Byte, Byte) {
        let a = self.val;
        ((a >> 8) as Byte, (a & 0xff) as Byte)
    }
}

#[derive(Default, Clone, Eq, PartialEq, Debug)]
struct Flags(Byte);

impl Flags {
    fn mask(f: Flag) -> Byte {
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

    fn clear_all(&mut self) {
        self.0 = 0x0
    }
}

impl From<Byte> for Flags {
    fn from(w: Byte) -> Self {
        Flags(w)
    }
}

impl Into<Byte> for Flags {
    fn into(self) -> Byte {
        self.0
    }
}

#[derive(Default, Clone, Eq, PartialEq, Debug)]
struct State {
    a: RegByte,
    b: RegByte,
    c: RegByte,
    d: RegByte,
    e: RegByte,
    h: RegByte,
    l: RegByte,
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
    fn set_a(&mut self, v: Byte) {
        self.a = v.into();
    }
    fn set_b(&mut self, v: Byte) {
        self.b = v.into();
    }
    fn set_c(&mut self, v: Byte) {
        self.c = v.into();
    }
    fn set_d(&mut self, v: Byte) {
        self.d = v.into();
    }
    fn set_e(&mut self, v: Byte) {
        self.e = v.into();
    }
    fn set_h(&mut self, v: Byte) {
        self.h = v.into();
    }
    fn set_l(&mut self, v: Byte) {
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

    fn pack_flags(&self) -> Byte {
        let mask: Byte = self.flags.clone().into();
        0x02 | mask
    }

    fn unpack_flags(&mut self, flags: Byte) {
        self.flags = flags.into();
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

    fn write<A: AsRef<[u8]>>(&mut self, address: Address, data: A) {
        data.as_ref().iter().enumerate().for_each(
            |(off, v)|
                self.write_byte(address + (off as Address), *v)
        )
    }

    fn ref_mut(&mut self, address: Address) -> &mut u8 {
        &mut self.bank[address as usize]
    }
}

#[derive(Default, Clone)]
pub struct Cpu {
    state: State,

    bus: MemoryBus,
}

/// Utilities
impl Cpu {
    fn reg(&self, r: self::Reg) -> RegByte {
        use self::Reg::*;
        match r {
            A => self.state.a,
            B => self.state.b,
            C => self.state.c,
            D => self.state.d,
            E => self.state.e,
            H => self.state.h,
            L => self.state.l,
            M => self.bus.read_byte(self.hl().val).into(),
        }
    }

    fn reg_set<R: Into<RegByte>>(&mut self, r: self::Reg, val: R) {
        use self::Reg::*;
        let reg = val.into();
        match r {
            A => self.state.a = reg,
            B => self.state.b = reg,
            C => self.state.c = reg,
            D => self.state.d = reg,
            E => self.state.e = reg,
            H => self.state.h = reg,
            L => self.state.l = reg,
            M => self.set_m(reg.val),
        }
    }

    fn reg_apply<F: FnMut(&mut RegByte)>(&mut self, r: self::Reg, mut f: F) {
        let mut val = self.reg(r);
        f(&mut val);
        self.reg_set(r, val);
    }

    fn reg_address(&self, r: self::RegPair) -> RegAddress {
        use self::RegPair::*;
        match r {
            BC => self.bc(),
            DE => self.de(),
            HL => self.hl(),
            SP => self.state.sp,
        }
    }

    fn reg_address_set<R: Into<RegAddress>>(&mut self, r: self::RegPair, address: R) {
        use self::RegPair::*;
        match r {
            BC => self.set_bc(address),
            DE => self.set_de(address),
            HL => self.set_hl(address),
            SP => { self.state.sp = address.into(); }
        }
    }

    fn reg_address_apply<F: FnMut(&mut RegAddress)>(&mut self, r: self::RegPair, mut f: F) {
        let mut val = self.reg_address(r);
        f(&mut val);
        self.reg_address_set(r, val);
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

    fn get_m(&self) -> RegByte {
        let address = self.hl().val;
        self.bus.read_byte(address).into()
    }

    fn set_m<W: Into<Byte>>(&mut self, val: W) {
        let address = self.hl().val;
        self.bus.write_byte(address, val.into());
    }

    fn fix_static_flags(&mut self, r: Reg) {
        self.reg(r).clone().update_flags(&mut self.state.flags)
    }

    fn push_val(&mut self, val: Byte) {
        self.state.sp.overflow_sub(1);
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

    fn pop_val(&mut self) -> Byte {
        let val = self.bus.read_byte(self.state.sp.into());
        self.state.sp.overflow_add(1);
        val
    }

    fn pop_reg(&mut self, r: Reg) {
        let val = self.pop_val();
        self.reg_set(r, val);
    }

    fn pop_flags(&mut self) {
        let val = self.pop_val();
        self.state.unpack_flags(val);
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

    fn mvi(&mut self, r: Reg, val: Byte) {
        self.reg_set(r, val);
    }

    fn adi(&mut self, val: Byte) {
        let carry = self.state.a.overflow_add(val);
        self.state.flags.val(Flag::Carry, carry);
        self.fix_static_flags(Reg::A);
    }
}

/// Single Register Instructions
impl Cpu {
    fn cma(&mut self) {
        self.reg_apply(self::Reg::A, |r| r.one_complement());
    }

    fn inr(&mut self, reg: self::Reg) {
        self.reg_apply(reg, |r| { r.increment(); });
        self.fix_static_flags(reg);
        let auxcarry = self.reg(reg).low() == 0x00;
        self.state.flags.val(AuxCarry, auxcarry);
    }

    fn dcr(&mut self, reg: self::Reg) {
        self.reg_apply(reg, |r| { r.decrement(); });
        self.fix_static_flags(reg);
        let auxcarry = self.reg(reg).low() == 0x0f;
        self.state.flags.val(AuxCarry, auxcarry);
    }

    fn daa(&mut self) {
        let low = self.state.a.low();
        let new_aux = low > 9;
        if new_aux || self.state.flags.get(AuxCarry) {
            self.state.a.overflow_add(6);
        }
        self.state.flags.val(AuxCarry, new_aux);

        let high = self.state.a.high();
        let new_carry = high > 0x90;
        if new_carry || self.state.flags.get(Carry) {
            self.state.a.overflow_add(0x60);
        }
        self.set_carry_state(new_carry);
        self.fix_static_flags(Reg::A);
    }
}

/// Data transfer Instruction
impl Cpu {
    fn mov(&mut self, f: Reg, t: Reg) {
        let orig = self.reg(f);
        self.reg_apply(t, |dest| { *dest = orig; })
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
}

/// Accumulator
impl Cpu {
    fn add(&mut self, r: Reg) {
        let other = self.reg(r).val;
        let carry = self.state.a.overflow_add(other);
        self.set_carry_state(carry);
        self.fix_static_flags(Reg::A)
    }

    fn adc(&mut self, r: Reg) {
        let v = self.reg(r).val;
        let other = if self.carry() { v + 1 } else { v };
        let carry = self.state.a.overflow_add(other);
        self.set_carry_state(carry);
        self.fix_static_flags(Reg::A)
    }

    fn sub(&mut self, r: Reg) {
        let other = self.reg(r).val;
        let carry = self.state.a.overflow_sub(other);
        self.set_carry_state(carry);
        self.fix_static_flags(Reg::A)
    }

    fn sbb(&mut self, r: Reg) {
        let v = self.reg(r).val;
        let other = if self.carry() { v + 1 } else { v };
        let carry = self.state.a.overflow_sub(other);
        self.set_carry_state(carry);
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
        let carry = self.state.a.rotate_left();
        self.set_carry_state(carry);
    }
    fn rrc(&mut self) {
        let carry = self.state.a.rotate_right();
        self.set_carry_state(carry);
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

    fn pop(&mut self, bp: BytePair) {
        use self::BytePair::*;
        match bp {
            BC => {
                self.pop_reg(Reg::C);
                self.pop_reg(Reg::B);
            }
            DE => {
                self.pop_reg(Reg::E);
                self.pop_reg(Reg::D);
            }
            HL => {
                self.pop_reg(Reg::L);
                self.pop_reg(Reg::H);
            }
            AF => {
                self.pop_flags();
                self.pop_reg(Reg::A);
            }
        }
    }

    fn dad(&mut self, rp: RegPair) {
        let delta = match rp {
            RegPair::BC => self.bc(),
            RegPair::DE => self.de(),
            RegPair::HL => self.hl(),
            RegPair::SP => self.state.sp,
        }.into();
        let mut a = self.hl();
        let carry = a.overflow_add(delta);
        self.set_hl(a);
        self.set_carry_state(carry);
    }

    fn inx(&mut self, rp: RegPair) {
        self.reg_address_apply(rp, |a| { a.overflow_add(1); });
    }

    fn dcx(&mut self, rp: RegPair) {
        self.reg_address_apply(rp, |a| { a.overflow_sub(1); });
    }

    fn xchg(&mut self) {
        swap(&mut self.state.d, &mut self.state.h);
        swap(&mut self.state.e, &mut self.state.l);
    }

    fn xthl(&mut self) {
        let sp = self.state.sp.into();
        swap(self.bus.ref_mut(sp), &mut self.state.h.val);
        swap(self.bus.ref_mut(sp + 1), &mut self.state.l.val);
    }

    fn sphl(&mut self) {
        self.state.sp = self.hl().into();
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
                self.daa()
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
            Pop(bp) => {
                self.pop(bp)
            }
            Dad(rp) => {
                self.dad(rp)
            }
            Inx(rp) => {
                self.inx(rp)
            }
            Dcx(rp) => {
                self.dcx(rp)
            }
            Xchg => {
                self.xchg()
            }
            Xthl => {
                self.xthl()
            }
            Sphl => {
                self.sphl()
            }
            Lxi(rp) => {
                self.lxi(rp)
            }
            Mvi(r, val) => {
                self.mvi(r, val)
            }
            Adi(val) => {
                self.adi(val)
            }
            _ => unimplemented!("Instruction {:?} not implemented yet!", instruction)
        }
        self.state.pc.overflow_add(instruction.length());
    }
}

#[cfg(test)]
mod test {
    use rstest::rstest;
    use rstest::rstest_parametrize;
    use super::*;

    #[derive(Default)]
    struct StateBuilder {
        proto: State
    }

    impl StateBuilder {
        fn create(&self) -> State {
            self.proto.clone()
        }

        fn b(mut self, val: Byte) -> Self {
            self.proto.set_b(val);
            self
        }

        fn c(mut self, val: Byte) -> Self {
            self.proto.set_c(val);
            self
        }

        fn d(mut self, val: Byte) -> Self {
            self.proto.set_d(val);
            self
        }

        fn e(mut self, val: Byte) -> Self {
            self.proto.set_e(val);
            self
        }

        fn h(mut self, val: Byte) -> Self {
            self.proto.set_h(val);
            self
        }

        fn l(mut self, val: Byte) -> Self {
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
    enum ByteReg {
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

    impl CpuQuery for ByteReg {
        type Result = Byte;

        fn ask(&self, cpu: &Cpu) -> <Self as CpuQuery>::Result {
            use self::ByteReg::*;
            match *self {
                A => cpu.state.a,
                B => cpu.state.b,
                C => cpu.state.c,
                D => cpu.state.d,
                E => cpu.state.e,
                H => cpu.state.h,
                L => cpu.state.l,
                M => cpu.get_m(),
            }.into()
        }
    }

    impl CpuQuery for SP {
        type Result = Address;

        fn ask(&self, cpu: &Cpu) -> <Self as CpuQuery>::Result {
            cpu.state.sp.into()
        }
    }

    impl QueryResult for Byte {}

    impl QueryResult for Address {}

    impl QueryResult for bool {}

    impl<T0: QueryResult, T1: QueryResult> QueryResult for (T0, T1) {}

    impl CpuQuery for Flag {
        type Result = bool;

        fn ask(&self, cpu: &Cpu) -> <Self as CpuQuery>::Result {
            cpu.state.flag(*self)
        }
    }

    impl CpuQuery for Reg {
        type Result = Byte;

        fn ask(&self, cpu: &Cpu) -> <Self as CpuQuery>::Result {
            ByteReg::from(*self).ask(&cpu)
        }
    }

    impl CpuQuery for BytePair {
        type Result = (Byte, Byte);

        fn ask(&self, cpu: &Cpu) -> <Self as CpuQuery>::Result {
            use self::BytePair::*;
            match self {
                BC => cpu.bc(),
                DE => cpu.de(),
                HL => cpu.hl(),
                AF => unimplemented!(),
            }.into()
        }
    }

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

    impl ApplyState for () {
        fn apply(&self, _cpu: &mut Cpu) {}
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
        A(Byte),
        B(Byte),
        C(Byte),
        D(Byte),
        E(Byte),
        H(Byte),
        L(Byte),
        M(Byte),
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

    impl ApplyState for Flag {
        fn apply(&self, cpu: &mut Cpu) {
            cpu.state.flags.set(*self)
        }
    }

    impl<A, B> ApplyState for (A, B) where
        A: ApplyState,
        B: ApplyState
    {
        fn apply(&self, cpu: &mut Cpu) {
            self.0.apply(cpu);
            self.1.apply(cpu);
        }
    }

    impl ApplyState for (BytePair, (Byte, Byte)) {
        fn apply(&self, cpu: &mut Cpu) {
            let (rp, v) = self.clone();
            use self::BytePair::*;
            match rp {
                BC => cpu.set_bc(v),
                DE => cpu.set_de(v),
                HL => cpu.set_hl(v),
                AF => unimplemented!(),
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

    impl From<(Reg, Byte)> for RegValue {
        fn from(vals: (Reg, Byte)) -> Self {
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

    impl From<Reg> for ByteReg {
        fn from(r: Reg) -> Self {
            use self::Reg::*;
            match r {
                A => ByteReg::A,
                B => ByteReg::B,
                C => ByteReg::C,
                D => ByteReg::D,
                E => ByteReg::E,
                H => ByteReg::H,
                L => ByteReg::L,
                M => ByteReg::M,
            }
        }
    }

    impl From<(RegPair, Address)> for RegPairValue {
        fn from(vals: (RegPair, Address)) -> Self {
            let (rp, addr) = vals;
            let (r0, r1) = RegAddress::from(addr).into();
            use self::RegPair::*;
            match rp {
                BC => RegPairValue::BC(r0, r1),
                DE => RegPairValue::DE(r0, r1),
                HL => RegPairValue::HL(r0, r1),
                SP => RegPairValue::SP(addr),
            }
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
        use self::BytePair::*;
        use super::*;

        #[rstest]
        fn push_should_put_register_pair_on_the_stack(mut cpu: Cpu) {
            let sp = 0x321c;
            cpu.set_de(0x8f9d);
            cpu.state.set_sp(sp);

            cpu.exec(Push(BytePair::DE));

            assert_eq!(cpu.bus.read_byte(sp - 1), 0x8f);
            assert_eq!(cpu.bus.read_byte(sp - 2), 0x9d);
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

            assert_eq!(cpu.bus.read_byte(sp - 1), 0xde);
            assert_eq!(cpu.bus.read_byte(sp - 2), 0x43);
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
        fn push_should_store_flags_correctly<I: ApplyState>(mut cpu: Cpu, init: I, expected: Byte) {
            init.apply(&mut cpu);

            let flags = cpu.state.pack_flags();

            assert_eq!(flags, expected);
        }

        #[rstest]
        fn pop_should_recover_regs(mut cpu: Cpu) {
            let sp = 0x321a;
            cpu.bus.write_byte(sp, 0x8f);
            cpu.bus.write_byte(sp + 1, 0x9d);
            cpu.state.set_sp(sp);

            cpu.exec(Pop(BytePair::DE));

            assert_eq!(cpu.state.d, 0x9d);
            assert_eq!(cpu.state.e, 0x8f);
        }

        #[rstest]
        fn pop_should_move_stack_pointer(mut cpu: Cpu) {
            let sp = 0x1230;
            cpu.state.set_sp(sp);

            cpu.exec(Pop(BytePair::BC));

            assert_eq!(cpu.state.sp, sp + 2);
        }

        #[rstest]
        fn pop_should_recover_flags_and_accumulator(mut cpu: Cpu) {
            let sp = 0x2c00;
            cpu.state.set_sp(sp);
            cpu.bus.write_byte(sp, 0xc3);
            cpu.bus.write_byte(sp + 1, 0xff);

            cpu.exec(Pop(BytePair::AF));

            assert!(cpu.state.flag(Sign));
            assert!(cpu.state.flag(Zero));
            assert!(!cpu.state.flag(AuxCarry));
            assert!(!cpu.state.flag(Parity));
            assert!(cpu.state.flag(Carry));
            assert_eq!(cpu.state.a, 0xff);
        }

        #[rstest]
        fn pop_should_recover_flags_from_push(mut cpu: Cpu) {
            cpu.state.flags.set(Zero);
            cpu.state.flags.set(Parity);
            cpu.exec(Push(BytePair::AF));
            cpu.state.flags.clear_all();

            cpu.exec(Pop(BytePair::AF));

            assert!(!cpu.state.flags.get(Sign));
            assert!(cpu.state.flags.get(Zero));
            assert!(!cpu.state.flags.get(AuxCarry));
            assert!(cpu.state.flags.get(Parity));
            assert!(!cpu.state.flags.get(Carry));
        }

        #[rstest_parametrize(
        r, r0, r1,
        case(BC, 0x32, 0xf1),
        case(DE, 0x02, 0xac),
        case(HL, 0xa3, 0x01),
        )]
        fn pop_should_recover_all_registers_stored_by_posh(mut cpu: Cpu, r: BytePair, r0: Byte, r1: Byte) {
            (r, (r0, r1)).apply(&mut cpu);
            cpu.exec(Push(r));
            (r, (0x00, 0x00)).apply(&mut cpu);

            cpu.exec(Pop(r));

            assert_eq!(r.ask(&cpu), (r0, r1));
        }

        #[rstest]
        fn dad_should_add_reg_pair_to_hl(mut cpu: Cpu) {
            cpu.set_bc(0x339f);
            cpu.set_hl(0xa17b);

            cpu.exec(Dad(RegPair::BC));

            assert_eq!(cpu.hl(), 0xd51a);
        }

        #[rstest_parametrize(
        hl, rp, sum, expected,
        case(0xffff, Unwrap("RegPair::DE"), 0x0001, true),
        case(0xffff, Unwrap("RegPair::BC"), 0x0000, false),
        case(0x0001, Unwrap("RegPair::BC"), 0xffff, true),
        case(0x0000, Unwrap("RegPair::SP"), 0xffff, false),
        case(0xa7f2, Unwrap("RegPair::SP"), 0x8f31, true),
        )]
        fn dad_should_update_carry_bit(mut cpu: Cpu, hl: Address, rp: RegPair, sum: Address, expected: bool) {
            RegPairValue::from((rp, sum)).apply(&mut cpu);
            cpu.set_hl(hl);

            cpu.exec(Dad(rp));

            assert_eq!(cpu.carry(), expected)
        }

        #[rstest_parametrize(
        rp, query, expected,
        case(Unwrap("RegPair::BC"), Unwrap("(ByteReg::B, ByteReg::C)"), Unwrap("(0x10, 0xa7)")),
        case(Unwrap("RegPair::DE"), Unwrap("(ByteReg::D, ByteReg::E)"), Unwrap("(0x20, 0xe7)")),
        case(Unwrap("RegPair::HL"), Unwrap("(ByteReg::H, ByteReg::L)"), Unwrap("(0x22, 0xef)")),
        case(Unwrap("RegPair::SP"), Unwrap("SP"), Unwrap("0x1235")),
        )]
        fn inx<R: QueryResult, Q: CpuQuery<Result=R>>(mut cpu: Cpu, rp: RegPair, query: Q, expected: R) {
            cpu.exec(Inx(rp));

            assert_eq!(query.ask(&cpu), expected);
        }

        #[rstest_parametrize(
        init, rp, query, expected,
        case(Unwrap("RegPairValue::BC(0x00, 0xff)"), Unwrap("RegPair::BC"), Unwrap("(ByteReg::B, ByteReg::C)"), Unwrap("(0x01, 0x00)")),
        case(Unwrap("RegPairValue::BC(0xff, 0xff)"), Unwrap("RegPair::BC"), Unwrap("(ByteReg::B, ByteReg::C)"), Unwrap("(0x00, 0x00)")),
        case(Unwrap("RegPairValue::DE(0x12, 0xff)"), Unwrap("RegPair::DE"), Unwrap("(ByteReg::D, ByteReg::E)"), Unwrap("(0x13, 0x00)")),
        case(Unwrap("RegPairValue::DE(0xff, 0xff)"), Unwrap("RegPair::DE"), Unwrap("(ByteReg::D, ByteReg::E)"), Unwrap("(0x00, 0x00)")),
        case(Unwrap("RegPairValue::HL(0xae, 0xff)"), Unwrap("RegPair::HL"), Unwrap("(ByteReg::H, ByteReg::L)"), Unwrap("(0xaf, 0x00)")),
        case(Unwrap("RegPairValue::HL(0xff, 0xff)"), Unwrap("RegPair::HL"), Unwrap("(ByteReg::H, ByteReg::L)"), Unwrap("(0x00, 0x00)")),
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
        case(Unwrap("RegPair::BC"), Unwrap("(ByteReg::B, ByteReg::C)"), Unwrap("(0x10, 0xa5)")),
        case(Unwrap("RegPair::DE"), Unwrap("(ByteReg::D, ByteReg::E)"), Unwrap("(0x20, 0xe5)")),
        case(Unwrap("RegPair::HL"), Unwrap("(ByteReg::H, ByteReg::L)"), Unwrap("(0x22, 0xed)")),
        case(Unwrap("RegPair::SP"), Unwrap("SP"), Unwrap("0x1233")),
        )]
        fn dcx<R: QueryResult, Q: CpuQuery<Result=R>>(mut cpu: Cpu, rp: RegPair, query: Q, expected: R) {
            cpu.exec(Dcx(rp));

            assert_eq!(query.ask(&cpu), expected);
        }

        #[rstest_parametrize(
        init, rp, query, expected,
        case(Unwrap("RegPairValue::BC(0xff, 0x00)"), Unwrap("RegPair::BC"), Unwrap("(ByteReg::B, ByteReg::C)"), Unwrap("(0xfe, 0xff)")),
        case(Unwrap("RegPairValue::BC(0x00, 0x00)"), Unwrap("RegPair::BC"), Unwrap("(ByteReg::B, ByteReg::C)"), Unwrap("(0xff, 0xff)")),
        case(Unwrap("RegPairValue::DE(0x12, 0x00)"), Unwrap("RegPair::DE"), Unwrap("(ByteReg::D, ByteReg::E)"), Unwrap("(0x11, 0xff)")),
        case(Unwrap("RegPairValue::DE(0x00, 0x00)"), Unwrap("RegPair::DE"), Unwrap("(ByteReg::D, ByteReg::E)"), Unwrap("(0xff, 0xff)")),
        case(Unwrap("RegPairValue::HL(0xae, 0x00)"), Unwrap("RegPair::HL"), Unwrap("(ByteReg::H, ByteReg::L)"), Unwrap("(0xad, 0xff)")),
        case(Unwrap("RegPairValue::HL(0x00, 0x00)"), Unwrap("RegPair::HL"), Unwrap("(ByteReg::H, ByteReg::L)"), Unwrap("(0xff, 0xff)")),
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
        fn xchg_should_swap_hl_and_de_registers(mut cpu: Cpu) {
            let de = 0x2345;
            let hl = 0xaf43;
            cpu.set_de(de);
            cpu.set_hl(hl);

            cpu.exec(Xchg);

            assert_eq!(cpu.de(), hl);
            assert_eq!(cpu.hl(), de);
        }

        #[rstest]
        fn xthl_should_swap_hl_and_two_bytes_pointed_by_stack_pointer(mut cpu: Cpu) {
            let sp = 0x10ad;
            cpu.set_hl(0x0b3c);
            cpu.state.set_sp(sp);
            cpu.bus.write(sp, &[0xf0, 0x0d]);

            cpu.exec(Xthl);

            assert_eq!(cpu.hl(), 0xf00d);
            assert_eq!(cpu.bus.read_byte(sp), 0x0b);
            assert_eq!(cpu.bus.read_byte(sp + 1), 0x3c);
        }

        #[rstest]
        fn sphl_should_copy_hl_in_sp(mut cpu: Cpu) {
            cpu.set_hl(0x506c);

            cpu.exec(Sphl);

            assert_eq!(cpu.state.sp, 0x506c)
        }
    }

    mod immediate {
        use super::*;

        #[rstest_parametrize(
        rp, query, expected,
        case(Unwrap("RegPairValue::BC(0xe4, 0xf1)"), Unwrap("(ByteReg::B, ByteReg::C)"), Unwrap("(0xe4, 0xf1)")),
        case(Unwrap("RegPairValue::DE(0x20, 0xb1)"), Unwrap("(ByteReg::D, ByteReg::E)"), Unwrap("(0x20, 0xb1)")),
        case(Unwrap("RegPairValue::HL(0x02, 0xae)"), Unwrap("(ByteReg::H, ByteReg::L)"), Unwrap("(0x02, 0xae)")),
        case(Unwrap("RegPairValue::SP(0x4321)"), Unwrap("SP"), 0x4321),
        )]
        fn lxi<R: QueryResult, Q: CpuQuery<Result=R>>(mut cpu: Cpu, rp: RegPairValue, query: Q, expected: R) {
            cpu.exec(Lxi(rp));

            assert_eq!(query.ask(&cpu), expected);
        }

        #[rstest_parametrize(
        r, val,
        case(Unwrap("Reg::B"), 0x34),
        case(Unwrap("Reg::D"), 0xf3),
        case(Unwrap("Reg::L"), 0x01),
        case(Unwrap("Reg::M"), 0x54),
        )]
        fn mvi_should_store_data_in_register(mut cpu: Cpu, r: Reg, val: Byte) {
            cpu.exec(Mvi(r, val));

            assert_eq!(r.ask(&cpu), val);
        }

        #[rstest]
        fn mvi_should_store_m_register_to_hl_pointed_address(mut cpu: Cpu) {
            let address = 0x3421;
            let val = 0xa2;
            cpu.set_hl(address);

            cpu.exec(Mvi(Reg::M, val));

            assert_eq!(cpu.bus.read_byte(address), val);
        }

        #[rstest_parametrize(
        a, data, expected,
        case(0x34, 0x1f, 0x53),
        case(0x56, 0xbe, 0x14),
        )]
        fn adi_should_add_immediate_data_to_accumulator(mut cpu: Cpu, a: Byte, data: Byte, expected: Byte) {
            cpu.state.a = a.into();

            cpu.exec(Adi(data));

            assert_eq!(cpu.state.a, expected);
        }

        #[rstest_parametrize(
        a, data, start, expected,
        case(0x34, 0x1f, false, false),
        case(0x34, 0x1f, true, false),
        case(0x56, 0xbe, false, true),
        case(0x56, 0xbe, true, true),
        case(0xff, 0x01, false, true),
        case(0xa4, 0x02, true, false),
        case(0xb4, 0x62, true, true),
        )]
        fn adi_should_affect_carry_bit(mut cpu: Cpu, a: Byte, data: Byte, start: bool, expected: bool) {
            cpu.state.flags.val(Flag::Carry, start);
            cpu.state.a = a.into();

            cpu.exec(Adi(data));

            assert_eq!(expected, cpu.state.flags.get(Flag::Carry));
        }

        #[rstest]
        fn adi_should_fix_static_flags(mut cpu: Cpu) {
            let a = 0x56;
            let data = 0xbe;
            cpu.state.a = a.into();

            cpu.exec(Adi(data));

            assert_eq!(Parity.ask(&cpu), true);
            assert_eq!(Zero.ask(&cpu), false);
            assert_eq!(Sign.ask(&cpu), false);
        }
    }

    #[rstest_parametrize(
    init, query, expected,
    case(Unwrap("RegValue::B(0x00)"), Zero, true),
    case(Unwrap("RegValue::B(0x12)"), Zero, false),
    case(Unwrap("RegValue::B(0xff)"), Zero, false),
    case(Unwrap("RegValue::A(0x80)"), Sign, true),
    case(Unwrap("RegValue::A(0xA3)"), Sign, true),
    case(Unwrap("RegValue::A(0xff)"), Sign, true),
    case(Unwrap("RegValue::A(0x00)"), Sign, false),
    case(Unwrap("RegValue::A(0x12)"), Sign, false),
    case(Unwrap("RegValue::A(0x7f)"), Sign, false),
    case(Unwrap("RegValue::D(0x00)"), Parity, true),
    case(Unwrap("RegValue::D(0x03)"), Parity, true),
    case(Unwrap("RegValue::D(0xff)"), Parity, true),
    case(Unwrap("RegValue::D(0x01)"), Parity, false),
    case(Unwrap("RegValue::D(0x1f)"), Parity, false),
    case(Unwrap("RegValue::D(0x37)"), Parity, false),
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
        case(Unwrap("RegValue::A(0x33)"), Unwrap("Reg::A"), Unwrap("ByteReg::A"), 0x34),
        case(Unwrap("RegValue::B(0x00)"), Unwrap("Reg::B"), Unwrap("ByteReg::B"), 0x01),
        case(Unwrap("RegValue::C(0x23)"), Unwrap("Reg::C"), Unwrap("ByteReg::C"), 0x24),
        case(Unwrap("RegValue::D(0xaf)"), Unwrap("Reg::D"), Unwrap("ByteReg::D"), 0xb0),
        case(Unwrap("RegValue::E(0x01)"), Unwrap("Reg::E"), Unwrap("ByteReg::E"), 0x02),
        case(Unwrap("RegValue::H(0xd1)"), Unwrap("Reg::H"), Unwrap("ByteReg::H"), 0xd2),
        case(Unwrap("RegValue::L(0x53)"), Unwrap("Reg::L"), Unwrap("ByteReg::L"), 0x54),
        case(Unwrap("RegValue::M(0x12)"), Unwrap("Reg::M"), Unwrap("ByteReg::M"), 0x13),
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
        fn single_register_command(mut cpu: Cpu, cmd: SRegCmd, reg: Reg, before: Byte, after: Byte) {
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
        fn add_should_perform_addition(mut cpu: Cpu, start: Byte, r: Reg, v: Byte, expected: Byte) {
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
        fn adc_should_perform_addition_by_care_carry_flag(mut cpu: Cpu, start: Byte, carry: bool,
                                                          r: Reg, v: Byte, expected: Byte) {
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
        fn adc_should_update_carry_flag(mut cpu: Cpu, carry: bool, v: Byte, expected: bool) {
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
        fn sub_should_perform_subtraction(mut cpu: Cpu, start: Byte, r: Reg, v: Byte, expected: Byte) {
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
        fn sbb_should_perform_subtraction_by_care_carry_flag(mut cpu: Cpu, start: Byte, carry: bool,
                                                             r: Reg, v: Byte, expected: Byte) {
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
        fn sbb_should_update_carry_flag(mut cpu: Cpu, carry: bool, v: Byte, expected: bool) {
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
        fn ana_should_perform_logical_and(mut cpu: Cpu, start: Byte, r: Reg, v: Byte, expected: Byte) {
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
        fn xra_should_perform_logical_xor(mut cpu: Cpu, start: Byte, r: Reg, v: Byte, expected: Byte) {
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
        fn ora_should_perform_logical_or(mut cpu: Cpu, start: Byte, r: Reg, v: Byte, expected: Byte) {
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
        fn cmp_should(mut cpu: Cpu, a: Byte, r: Reg, v: Byte, zero: bool, carry: bool) {
            cpu.state.set_a(a);
            RegValue::from((r, v)).apply(&mut cpu);

            cpu.exec(Cmp(r));

            assert_eq!(cpu.state.flag(Zero), zero);
            assert_eq!(cpu.carry(), carry);
            assert_eq!(cpu.state.a, a);
            assert_eq!(cpu.reg(r), v);
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
        fn should_update_flags(mut cpu: Cpu, op: SRegCmd, a: Byte, b: Byte) {
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
        fn should_rotate_accumulator(mut cpu: Cpu, op: Instruction, before: Byte,
                                     after: Byte, carry: bool) {
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
                                                       before: Byte, carry_before: bool,
                                                       after: Byte, carry: bool) {
            cpu.set_carry_state(carry_before);
            cpu.state.set_a(before);

            cpu.exec(op);

            assert_eq!(cpu.state.a, after);
            assert_eq!(cpu.carry(), carry);
        }
    }


    #[rstest_parametrize(
    instruction, start, expected,
    case(Unwrap("Lxi(RegPairValue::BC(0xae,0x02))")),
    case(Nop),
    case(Rar),
    case(Rrc),
    )]
    fn should_advance_pc(mut cpu: Cpu, instruction: Instruction) {
        let start = 0x3241;
        cpu.state.pc = start.into();

        cpu.exec(instruction);

        assert_eq!(cpu.state.pc, start + instruction.length());
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

    #[rstest_parametrize(
    state, after,
    case(Unwrap("(0x9b, false, false)"), Unwrap("(0x01, true, true)")),
    case(Unwrap("(0x00, false, false)"), Unwrap("(0x00, false, false)")),
    case(Unwrap("(0x99, false, false)"), Unwrap("(0x99, false, false)")),
    case(Unwrap("(0x0a, false, false)"), Unwrap("(0x10, false, true)")),
    case(Unwrap("(0xa0, false, false)"), Unwrap("(0x00, true, false)")),
    case(Unwrap("(0x0b, false, false)"), Unwrap("(0x11, false, true)")),
    case(Unwrap("(0x75, true, true)"), Unwrap("(0xdb, false, false)")),
    case(Unwrap("(0xff, false, false)"), Unwrap("(0x05, false, true)")),
    case(Unwrap("(0x99, true, true)"), Unwrap("(0xff, false, false)")),
    )]
    fn daa_should_adjust_accumulator(mut cpu: Cpu,
                                     state: (Byte, bool, bool),
                                     after: (Byte, bool, bool)) {
        let (a, carry, auxcarry) = state;
        cpu.state.set_a(a);
        cpu.state.flags.val(Carry, carry);
        cpu.state.flags.val(AuxCarry, auxcarry);

        cpu.exec(Daa);

        let (a, carry, auxcarry) = after;
        assert_eq!(cpu.state.a, a);
        assert_eq!(Carry.ask(&cpu), carry);
        assert_eq!(AuxCarry.ask(&cpu), auxcarry);
    }

    #[rstest]
    fn daa_should_update_static_flags(mut cpu: Cpu) {
        cpu.state.set_a(0x75);
        cpu.state.flags.set(Carry);
        cpu.state.flags.set(AuxCarry);

        //Become 0xdb Z=false sign=true parity=true
        cpu.exec(Daa);

        println!("{:?}", cpu.state.a);

        assert_eq!(Zero.ask(&cpu), false);
        assert_eq!(Sign.ask(&cpu), true);
        assert_eq!(Parity.ask(&cpu), true);
    }

    #[rstest_parametrize(
    init, cmd, expected,
    case(Unwrap("RegValue::B(0xaf)"), Unwrap("Inr(Reg::B)"), true),
    case(Unwrap("RegValue::D(0xf8)"), Unwrap("Inr(Reg::D)"), false),
    case(Unwrap("RegValue::D(0xff)"), Unwrap("Inr(Reg::D)"), true),
    case(Unwrap("RegValue::D(0x0f)"), Unwrap("Inr(Reg::D)"), true),
    case(Unwrap("RegValue::B(0x0e)"), Unwrap("Inr(Reg::B)"), false),
    case(Unwrap("RegValue::E(0x30)"), Unwrap("Dcr(Reg::E)"), true),
    case(Unwrap("RegValue::M(0x43)"), Unwrap("Dcr(Reg::M)"), false),
    case(Unwrap("RegValue::M(0x00)"), Unwrap("Dcr(Reg::M)"), true),
    case(Unwrap("RegValue::C(0xa0)"), Unwrap("Dcr(Reg::C)"), true),
    case(Unwrap("RegValue::C(0x01)"), Unwrap("Dcr(Reg::C)"), false),
    case(Unwrap("(RegValue::A(0x3d), RegValue::C(0x3d))"), Unwrap("Add(Reg::C)"), true),
    case(Unwrap("(RegValue::A(0x12), RegValue::B(0x12))"), Unwrap("Add(Reg::B)"), false),
    case(Unwrap("(RegValue::A(0x14), RegValue::D(0x14))"), Unwrap("Add(Reg::D)"), true),
    case(Unwrap("(RegValue::A(0x0f), RegValue::D(0x0f))"), Unwrap("Add(Reg::D)"), true),
    case(Unwrap("(RegValue::A(0xa1), RegValue::H(0xa1))"), Unwrap("Add(Reg::H)"), false),
    )]
    fn should_affect_aux_carry<I: ApplyState>(mut cpu: Cpu, init: I, cmd: Instruction, expected: bool) {
        init.apply(&mut cpu);

        cpu.exec(cmd);

        assert_eq!(expected, cpu.state.flags.get(AuxCarry));
    }

    #[test]
    fn who_influence_aux_carry_flag() {
        panic!("Add");
        panic!("Adc");
        panic!("Sub");
        panic!("Sbb");
        panic!("Xra");
        panic!("Ora");
        panic!("Adi");
        panic!("Aci");
        panic!("Sui");
        panic!("Sbi");
        panic!("Cpi");
    }

    #[rstest]
    fn nop_should_just_change_pc(mut cpu: Cpu) {
        let mut state = cpu.state.clone();

        cpu.exec(Nop);

        assert_eq!(state.pc.val + 1, cpu.state.pc.into());

        state.pc.overflow_add(1);

        assert_eq!(state, cpu.state);
    }
}
