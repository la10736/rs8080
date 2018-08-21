use ::std::mem::swap;
use self::Flag::*;
use super::{
    Address, asm::{BytePair, Instruction, Instruction::*,
                   Reg, RegPair, RegPairValue, CondFlag, IrqAddr},
    Byte,
};
use io_bus::{OutputBus, InputBus};

#[cfg(test)]
mod test;

use ::registers::*;
use Word;

impl Into<Address> for IrqAddr {
    fn into(self) -> Address {
        (match self {
            IrqAddr::I0 => 0x0,
            IrqAddr::I1 => 0x1,
            IrqAddr::I2 => 0x2,
            IrqAddr::I3 => 0x3,
            IrqAddr::I4 => 0x4,
            IrqAddr::I5 => 0x5,
            IrqAddr::I6 => 0x6,
            IrqAddr::I7 => 0x7,
        } as Address) << 3
    }
}

impl RegByte {
    fn update_flags(&self, flags: &mut Flags) {
        for (f, v) in &[
            (Zero, self.is_zero()),
            (Sign, self.sign_bit()),
            (Parity, self.parity())
        ] {
            flags.val(*f, *v)
        }
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
struct PlainMemory {
    bank: [u8; 0x10000]
}

impl Default for PlainMemory {
    fn default() -> Self {
        PlainMemory { bank: [0; 0x10000] }
    }
}

pub trait Mmu {
    fn read_byte(&self, address: Address) -> u8;

    fn write_byte(&mut self, address: Address, val: u8);

    fn ref_mut(&mut self, address: Address) -> &mut u8;

    fn read_word(&self, address: Address) -> u16 {
        ((self.read_byte(address) as u16) << 8) | (self.read_byte(address + 1) as u16)
    }

    fn write_word(&mut self, address: Address, val: Word) {
        self.write_byte(address, (val >> 8) as Byte);
        self.write_byte(address + 1, (val & 0xff) as Byte);
    }

    fn write<A: AsRef<[u8]>>(&mut self, address: Address, data: A) {
        data.as_ref().iter().enumerate().for_each(
            |(off, v)|
                self.write_byte(address + (off as Address), *v)
        )
    }
}

impl Mmu for PlainMemory {
    fn read_byte(&self, address: Address) -> Byte {
        self.bank[address as usize]
    }

    fn write_byte(&mut self, address: Address, val: Byte) {
        self.bank[address as usize] = val
    }

    fn ref_mut(&mut self, address: Address) -> &mut u8 {
        &mut self.bank[address as usize]
    }
}

#[derive(Clone, Copy, Eq, PartialEq)]
pub enum CpuState {
    Running,
    Stopped,
}

impl Default for CpuState {
    fn default() -> Self {
        CpuState::Running
    }
}

#[derive(Clone, Copy, PartialEq)]
pub enum IrqCmd {
    Irq0,
    Irq1,
    Irq2,
    Irq3,
    Irq4,
    Irq5,
    Irq6,
    Irq7,
    RawCmd(Instruction)
}

impl Into<Instruction> for IrqCmd {
    fn into(self) -> Instruction {
        match self {
            IrqCmd::Irq0 => Rst(IrqAddr::I0),
            IrqCmd::Irq1 => Rst(IrqAddr::I1),
            IrqCmd::Irq2 => Rst(IrqAddr::I2),
            IrqCmd::Irq3 => Rst(IrqAddr::I3),
            IrqCmd::Irq4 => Rst(IrqAddr::I4),
            IrqCmd::Irq5 => Rst(IrqAddr::I5),
            IrqCmd::Irq6 => Rst(IrqAddr::I6),
            IrqCmd::Irq7 => Rst(IrqAddr::I7),
            IrqCmd::RawCmd(instruction) => instruction,
        }
    }
}

#[derive(Clone)]
pub struct Cpu<M: Mmu, O: OutputBus, I: InputBus> {
    state: State,
    run_state: CpuState,

    interrupt_enabled: bool,

    bus: M,

    output: O,
    input: I,
}

impl<M, O, I>
    Default for Cpu<M, O, I>
    where M: Mmu + Default,
          O: OutputBus + Default,
          I: InputBus + Default
{
    fn default() -> Self {
        Cpu {interrupt_enabled: true,
            bus: Default::default(),
            output: Default::default(),
            state: Default::default(),
            run_state: Default::default(),
            input: Default::default(),
        }
    }
}

/// External interface
impl<M: Mmu, O: OutputBus, I: InputBus> Cpu<M, O, I> {
    pub fn exec(&mut self, instruction: Instruction) {
        if !self.is_running() {
            return;
        }
        self.state.pc.overflow_add(instruction.length());
        self.apply(instruction)
    }

    pub fn apply(&mut self, instruction: Instruction) -> () {
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
            Aci(val) => {
                self.aci(val)
            }
            Sui(val) => {
                self.sui(val)
            }
            Sbi(val) => {
                self.sbi(val)
            }
            Ani(val) => {
                self.ani(val)
            }
            Xri(val) => {
                self.xri(val)
            }
            Ori(val) => {
                self.ori(val)
            }
            Cpi(val) => {
                self.cpi(val)
            }
            Sta(address) => {
                self.sta(address)
            }
            Lda(address) => {
                self.lda(address)
            }
            Shld(address) => {
                self.shld(address)
            }
            Lhld(address) => {
                self.lhld(address)
            }
            Pchl => {
                self.pchl()
            }
            Jump(address) => {
                self.jump(address)
            }
            J(cf, address) => {
                let (flag, expected) = cf.into();
                self.jump_conditionals(address, flag, expected)
            }
            Call(address) => {
                self.call(address)
            }
            C(cf, address) => {
                let (flag, expected) = cf.into();
                self.call_conditionals(address, flag, expected)
            }
            Ret => {
                self.ret()
            }
            R(cf) => {
                let (flag, expected) = cf.into();
                self.ret_conditionals(flag, expected)
            }
            Rst(irq) => {
                self.rst(irq)
            }
            Ei => {
                self.ei()
            }
            Di => {
                self.di()
            }
            In(id) => {
                self.input(id)
            }
            Out(id) => {
                self.output(id)
            }
            Hlt => {
                self.halt()
            }
            Stax(_) | Ldax(_) | Rim | Sim =>
                unimplemented!("Instruction {:?} should not exist!", instruction)
        }
    }

    pub fn irq(&mut self, cmd: IrqCmd) {
        self.interrupt_enabled = false;
        self.run_state = CpuState::Running;
        self.apply(cmd.into());
    }

    pub fn is_running(&self) -> bool {
        self.run_state == CpuState::Running
    }

    pub fn is_stopped(&self) -> bool {
        self.run_state == CpuState::Stopped
    }
}

/// Utilities
impl<M: Mmu, O: OutputBus, I: InputBus> Cpu<M, O, I> {

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

    fn push_addr(&mut self, address: Address) {
        self.push_val((address & 0xff) as Byte);
        self.push_val(((address >> 8) & 0xff) as Byte);
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

    fn pop_addr(&mut self) -> Address {
        let (hi, lo) = (self.pop_val(), self.pop_val());
        (hi as Address) << 8 | (lo as Address)
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
impl<M: Mmu, O: OutputBus, I: InputBus> Cpu<M, O, I> {
    fn nop(&mut self) {}
}

/// Carry bit Instructions
impl<M: Mmu, O: OutputBus, I: InputBus> Cpu<M, O, I> {
    fn cmc(&mut self) {
        self.state.flags.toggle(Flag::Carry);
    }
    fn stc(&mut self) {
        self.state.flags.set(Flag::Carry);
    }
}


/// Immediate Instructions
impl<M: Mmu, O: OutputBus, I: InputBus> Cpu<M, O, I> {
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
        self.accumulator_add(val);
    }

    fn aci(&mut self, val: Byte) {
        let other = if self.carry() { val + 1 } else { val };
        self.accumulator_add(other);
    }

    fn sui(&mut self, val: Byte) {
        self.accumulator_sub(val);
    }

    fn sbi(&mut self, val: Byte) {
        let other = if self.carry() { val + 1 } else { val };
        self.accumulator_sub(other);
    }

    fn cpi(&mut self, val: Byte) {
        self.compare(val.into());
    }
}

/// Single Register Instructions
impl<M: Mmu, O: OutputBus, I: InputBus> Cpu<M, O, I> {
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
impl<M: Mmu, O: OutputBus, I: InputBus> Cpu<M, O, I> {
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

/// Carry and AuxCarry
impl<M: Mmu, O: OutputBus, I: InputBus> Cpu<M, O, I> {
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

    fn aux_carry_clear(&mut self) {
        self.set_aux_carry_state(false)
    }

    fn set_aux_carry_state(&mut self, auxcarry: bool) {
        self.state.flags.val(AuxCarry, auxcarry);
    }
}

/// Accumulator
impl<M: Mmu, O: OutputBus, I: InputBus> Cpu<M, O, I> {
    fn accumulator_add(&mut self, other: u8) -> () {
        let auxcarry = (self.state.a.low() + (other & 0x0f)) > 0x0f;
        let carry = self.state.a.overflow_add(other);
        self.set_carry_state(carry);
        self.set_aux_carry_state(auxcarry);
        self.fix_static_flags(Reg::A)
    }

    fn accumulator_sub(&mut self, other: u8) -> () {
        let auxcarry = self.state.a.low() < (other & 0x0f);
        let carry = self.state.a.overflow_sub(other);
        self.set_carry_state(carry);
        self.set_aux_carry_state(auxcarry);
        self.fix_static_flags(Reg::A)
    }

    fn add(&mut self, r: Reg) {
        let other = self.reg(r).val;
        self.accumulator_add(other)
    }

    fn adc(&mut self, r: Reg) {
        let v = self.reg(r).val;
        let other = if self.carry() { v + 1 } else { v };
        self.accumulator_add(other);
    }

    fn sub(&mut self, r: Reg) {
        let other = self.reg(r).val;
        self.accumulator_sub(other)
    }

    fn sbb(&mut self, r: Reg) {
        let v = self.reg(r).val;
        let other = if self.carry() { v + 1 } else { v };
        self.accumulator_sub(other)
    }

    fn ana(&mut self, r: Reg) {
        let other = self.reg(r).val;
        self.accumulator_and(other);
    }

    fn xra(&mut self, r: Reg) {
        let other = self.reg(r).val;
        self.accumulator_xor(other);
    }

    fn ora(&mut self, r: Reg) {
        let other = self.reg(r).val;
        self.accumulator_or(other);
    }

    fn ani(&mut self, other: Byte) {
        self.accumulator_and(other);
    }

    fn xri(&mut self, other: Byte) {
        self.accumulator_xor(other);
    }

    fn ori(&mut self, other: Byte) {
        self.accumulator_or(other);
    }

    fn accumulator_and(&mut self, other: u8) {
        self.state.a = (self.state.a.val & other).into();
        self.fix_static_flags(Reg::A);
        self.carry_clear();
    }

    fn accumulator_or(&mut self, other: u8) {
        self.state.a = (self.state.a.val | other).into();
        self.fix_static_flags(Reg::A);
        self.carry_clear();
    }

    fn accumulator_xor(&mut self, other: u8) {
        self.state.a = (self.state.a.val ^ other).into();
        self.fix_static_flags(Reg::A);
        self.carry_clear();
        self.aux_carry_clear();
    }

    fn cmp(&mut self, r: Reg) {
        let other = self.reg(r);
        self.compare(other)
    }

    fn compare(&mut self, other: RegByte) -> () {
        let old = self.state.a;
        let inverse = old.sign_bit() != other.sign_bit();
        self.accumulator_sub(other.into());
        self.state.a = old;
        if inverse {
            self.state.flags.toggle(Carry);
        }
    }
}

const RIGHT_BIT: u8 = 0;
const LEFT_BIT: u8 = 7;

/// Rotate
impl<M: Mmu, O: OutputBus, I: InputBus> Cpu<M, O, I> {
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
impl<M: Mmu, O: OutputBus, I: InputBus> Cpu<M, O, I> {
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

/// Direct Addressing Instructions
impl<M: Mmu, O: OutputBus, I: InputBus> Cpu<M, O, I> {
    fn sta(&mut self, address: Address) {
        let value = self.state.a.val;
        self.bus.write_byte(address, value);
    }

    fn lda(&mut self, address: Address) {
        self.state.a = self.bus.read_byte(address).into();
    }

    fn shld(&mut self, address: Address) {
        let hl = self.hl().into();
        let bus = &mut self.bus;
        bus.write_word(address, hl);
    }

    fn lhld(&mut self, address: Address) {
        self.state.h = self.bus.read_byte(address).into();
        self.state.l = self.bus.read_byte(address + 1).into();
    }
}

impl Into<(Flag, bool)> for CondFlag {
    fn into(self) -> (Flag, bool) {
        use self::CondFlag::*;
        match self {
            C => (Carry, true),
            NC => (Carry, false),
            Z => (Zero, true),
            NZ => (Zero, false),
            M => (Sign, true),
            P => (Sign, false),
            PE => (Parity, true),
            PO => (Parity, false),
        }
    }
}

/// Jump Instructions
impl<M: Mmu, O: OutputBus, I: InputBus> Cpu<M, O, I> {
    fn pchl(&mut self) {
        self.state.pc = self.hl();
    }

    fn jump(&mut self, address: Address) {
        self.state.pc = address.into();
    }

    fn jump_conditionals(&mut self, address: Address, flag: Flag, should_be: bool) {
        if self.state.flags.get(flag) == should_be {
            self.jump(address);
        }
    }

    fn call(&mut self, address: Address) {
        let addr = self.state.pc.into();
        self.push_addr(addr);
        self.jump(address);
    }

    fn call_conditionals(&mut self, address: Address, flag: Flag, should_be: bool) {
        if self.state.flags.get(flag) == should_be {
            self.call(address);
        }
    }

    fn ret(&mut self) {
        let addr = self.pop_addr();
        self.jump(addr);
    }

    fn ret_conditionals(&mut self, flag: Flag, should_be: bool) {
        if self.state.flags.get(flag) == should_be {
            self.ret();
        }
    }

    fn rst(&mut self, irq: IrqAddr) {
        self.call(irq.into())
    }
}

/// Interrupts
impl<M: Mmu, O: OutputBus, I: InputBus> Cpu<M, O, I> {
    fn ei(&mut self) {
        self.interrupt_enabled = true;
    }

    fn di(&mut self) {
        self.interrupt_enabled = false;
    }
}

/// IO
impl<M: Mmu, O: OutputBus, I: InputBus> Cpu<M, O, I> {
    fn input(&mut self, id: Byte) {
        let val = self.input.read(id);
        self.state.set_a(val);
    }

    fn output(&mut self, id: Byte) {
        self.output.send(id, self.state.a.val);
    }
}

/// Halt
impl<M: Mmu, O: OutputBus, I: InputBus> Cpu<M, O, I> {
    fn halt(&mut self) {
        self.run_state = CpuState::Stopped;
    }
}
