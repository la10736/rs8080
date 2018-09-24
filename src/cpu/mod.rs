use std::{
    fmt::{self, Display, Formatter},
    result::Result as StdResult,
    collections::VecDeque,
    mem::swap
};

use super::{
    Address, Byte,
    asm::{Reg,
          Instruction, Instruction::*,
          BytePair, RegPair, RegPairValue, CondFlag, IrqAddr
    },
    registers::*,
    disassemble::{opcode, OpcodeError},
    io_bus::{OutputBus, InputBus},
    mmu::*,
    flags::*,
    flags::Flag::*,
    hook::CallHook,
};

#[cfg(test)]
mod test;

pub type Result<V> = StdResult<V, CpuError>;
pub type Periods = u16;
pub const DUMP_MEMORY_COLUMNS: usize = 32;

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
pub struct State {
    pub a: RegByte,
    pub b: RegByte,
    pub c: RegByte,
    pub d: RegByte,
    pub e: RegByte,
    pub h: RegByte,
    pub l: RegByte,
    pub pc: RegAddress,
    pub sp: RegAddress,

    pub flags: Flags,
}

#[allow(dead_code)]
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

fn str_bytes(data: &[Byte]) -> String {
    data.iter().map(|v| format!("{:02x}", v)).collect::<Vec<_>>().join(" ")
}

fn str_memory_row(data: &[Byte], address: usize) -> String {
    format!("0x{:04x} {}", address, str_bytes(data))
}

pub fn str_memory(data: &[Byte], offset: usize, columns: usize) -> String {
    data.chunks(columns)
        .into_iter()
        .enumerate()
        .map(|(pos, d)| str_memory_row(d, offset + pos * columns))
        .collect::<Vec<_>>().join("\n")
}

impl Mmu for PlainMemory {
    fn read_byte(&self, address: Address) -> Result<Byte> {
        Ok(self.bank[address as usize])
    }
    fn write_byte(&mut self, address: Address, val: Byte) -> Result<()> {
        self.bank[address as usize] = val;
        Ok(())
    }

    fn dump(&self) -> String {
        str_memory(&self.bank, 0x0, DUMP_MEMORY_COLUMNS)
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
    RawCmd(Instruction),
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

pub const DEFAULT_HISTORY_SIZE: usize = 128;

#[derive(Clone)]
struct OpCodesHistory {
    buffer: VecDeque<(Address, State, Instruction)>,
    size: usize,
}

impl OpCodesHistory {
    pub fn new(size: usize) -> Self {
        OpCodesHistory {
            size,
            buffer: VecDeque::with_capacity(size),
        }
    }

    pub fn store(&mut self, pc: Address, state: State, opcode: Instruction) {
        self.buffer.push_front((pc, state, opcode));
        self.buffer.truncate(self.size)
    }

    pub fn clear(&mut self) {
        self.buffer.clear();
    }
}

impl Display for OpCodesHistory {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        for (pos, (pc, state, op)) in self.buffer.iter().rev().enumerate() {
            write!(f, "   [{:8}] [0x{:04x}] - {:15} | {:?}\n", pos, pc, format!("{}", op), state)?;
        }
        Ok(())
    }
}

impl Default for OpCodesHistory {
    fn default() -> Self {
        Self::new(DEFAULT_HISTORY_SIZE)
    }
}

#[derive(Clone)]
pub struct Cpu<M: Mmu, O: OutputBus, I: InputBus, H: CallHook> {
    state: State,
    run_state: CpuState,

    op_code_history: OpCodesHistory,

    interrupt_enabled: bool,

    mmu: M,

    output: O,
    input: I,

    hook: H
}

impl<M, O, I, H>
Default for Cpu<M, O, I, H>
    where M: Mmu + Default,
          O: OutputBus + Default,
          I: InputBus + Default,
          H: CallHook + Default
{
    fn default() -> Self {
        Cpu {
            interrupt_enabled: true,
            op_code_history: Default::default(),
            mmu: Default::default(),
            output: Default::default(),
            state: Default::default(),
            run_state: Default::default(),
            input: Default::default(),
            hook: Default::default(),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum CpuError {
    InvalidCode(String),
    Memory(MemoryError),
    NotRunning,
    NoCode,
}

impl CpuError {
    pub fn memory_read<V>(address: Address) -> Result<V> {
        Err(CpuError::Memory(MemoryError::Read(address)))
    }
    pub fn memory_write<V>(address: Address, val: Byte) -> Result<V> {
        Err(CpuError::Memory(MemoryError::Write(address, val)))
    }
}

impl From<OpcodeError> for CpuError {
    fn from(op: OpcodeError) -> Self {
        CpuError::InvalidCode(op.into())
    }
}

/// External interface
impl<M: Mmu, O: OutputBus, I: InputBus, H: CallHook> Cpu<M, O, I, H> {
    pub fn new(mmu: M, output: O, input: I, hook: H) -> Self {
        Cpu {
            state: Default::default(),
            run_state: Default::default(),
            op_code_history: Default::default(),
            interrupt_enabled: true,
            mmu,
            output,
            input,
            hook,
        }
    }

    /// Load instruction at pc and run it
    pub fn run(&mut self) -> Result<Periods> {
        let pc = self.pc();
        let op = opcode(self)?;
        debug!("State: {:?} | Exec: {}", self.state, op);
        self.op_code_history.store(pc, self.state.clone(), op);
        let r = self.exec(op)?;
        Ok(r)
    }

    pub fn exec(&mut self, instruction: Instruction) -> Result<Periods> {
        if !self.is_running() {
            return Err(CpuError::NotRunning);
        }
        self.apply(instruction)
    }

    pub fn apply(&mut self, instruction: Instruction) -> Result<Periods> {
        match instruction {
            Cmc => {
                Ok(self.cmc())
            }
            Stc => {
                Ok(self.stc())
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
                Ok(self.nop())
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
                Ok(self.rlc())
            }
            Rrc => {
                Ok(self.rrc())
            }
            Ral => {
                Ok(self.ral())
            }
            Rar => {
                Ok(self.rar())
            }
            Push(bp) => {
                self.push(bp)
            }
            Pop(bp) => {
                self.pop(bp)
            }
            Dad(rp) => {
                Ok(self.dad(rp))
            }
            Inx(rp) => {
                Ok(self.inx(rp))
            }
            Dcx(rp) => {
                Ok(self.dcx(rp))
            }
            Xchg => {
                Ok(self.xchg())
            }
            Xthl => {
                self.xthl()
            }
            Sphl => {
                Ok(self.sphl())
            }
            Lxi(rp) => {
                Ok(self.lxi(rp))
            }
            Mvi(r, val) => {
                self.mvi(r, val)
            }
            Adi(val) => {
                Ok(self.adi(val))
            }
            Aci(val) => {
                Ok(self.aci(val))
            }
            Sui(val) => {
                Ok(self.sui(val))
            }
            Sbi(val) => {
                Ok(self.sbi(val))
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
                Ok(self.cpi(val))
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
                Ok(self.pchl())
            }
            Jump(address) => {
                Ok(self.jump(address))
            }
            J(cf, address) => {
                let (flag, expected) = cf.into();
                Ok(self.jump_conditionals(address, flag, expected))
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
                Ok(self.ei())
            }
            Di => {
                Ok(self.di())
            }
            In(id) => {
                Ok(self.input(id))
            }
            Out(id) => {
                Ok(self.output(id))
            }
            Hlt => {
                Ok(self.halt())
            }
            Stax(_) | Ldax(_) | Rim | Sim =>
                Err(CpuError::InvalidCode(format!("Instruction {:?} should not exist!", instruction)))
        }
    }

    pub fn irq(&mut self, cmd: IrqCmd) -> Result<Periods> {
        self.interrupt_enabled = false;
        self.run_state = CpuState::Running;
        self.apply(cmd.into())
    }

    pub fn pc(&self) -> Address {
        self.state.pc.into()
    }

    pub fn set_pc(&mut self, address: Address) {
        self.state.pc = address.into()
    }

    pub fn sp(&self) -> Address {
        self.state.sp.into()
    }

    pub fn is_running(&self) -> bool {
        self.run_state == CpuState::Running
    }

    pub fn is_stopped(&self) -> bool {
        self.run_state == CpuState::Stopped
    }

    pub fn get_state(&self) -> State {
        self.state.clone()
    }
}

/// Dump interface
impl<M: Mmu, O: OutputBus, I: InputBus, H: CallHook> Cpu<M, O, I, H> {
    pub fn dump_state(&self) -> String {
        format!("{:?}", self.state)
    }

    pub fn dump_memory(&self) -> String {
        self.dump()
    }

    pub fn dump_stack(&self) -> String {
        let address = self.state.sp.val;
        format!("0x{:04x} => ", address) + &match (address..0x2400)
            .map(|a| self.read_byte(a))
            .map(|e| e.map(|v| format!("{:02x}", v)))
            .collect::<Result<Vec<_>>>() {
            Ok(vals) => vals.join(" "),
            Err(_) => format!("Invalid stack address 0x{:04x}", address)
        }
    }

    pub fn dump_opcodes(&self) -> String {
        format!("Code:\n{}", self.op_code_history)
    }

    pub fn clear_history(&mut self) {
        self.op_code_history.clear()
    }
}

impl<M: Mmu, O: OutputBus, I: InputBus, H: CallHook> Iterator for Cpu<M, O, I, H> {
    type Item = Result<Byte>;

    fn next(&mut self) -> Option<<Self as Iterator>::Item> {
        let data = self.read_byte(self.state.pc.val);
        self.state.pc.overflow_add(0x01);
        Some(data)
    }
}

/// Utilities
impl<M: Mmu, O: OutputBus, I: InputBus, H: CallHook> Cpu<M, O, I, H> {
    fn reg(&self, r: self::Reg) -> Result<RegByte> {
        use self::Reg::*;
        Ok(
            match r {
                A => self.state.a,
                B => self.state.b,
                C => self.state.c,
                D => self.state.d,
                E => self.state.e,
                H => self.state.h,
                L => self.state.l,
                M => self.get_m()?,
            }
        )
    }

    fn reg_set<R: Into<RegByte>>(&mut self, r: self::Reg, val: R) -> Result<()> {
        use self::Reg::*;
        let reg = val.into();
        Ok(
            match r {
                A => self.state.a = reg,
                B => self.state.b = reg,
                C => self.state.c = reg,
                D => self.state.d = reg,
                E => self.state.e = reg,
                H => self.state.h = reg,
                L => self.state.l = reg,
                M => self.set_m(reg.val)?,
            }
        )
    }

    fn reg_apply<F: FnMut(&mut RegByte)>(&mut self, r: self::Reg, mut f: F) -> Result<()> {
        let mut val = self.reg(r)?;
        f(&mut val);
        self.reg_set(r, val)
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

    fn get_m(&self) -> Result<RegByte> {
        let address = self.hl().val;
        self.read_byte(address).map(|v| v.into())
    }

    fn set_m<W: Into<Byte>>(&mut self, val: W) -> Result<()> {
        let address = self.hl().val;
        self.write_byte(address, val.into())
    }

    fn fix_static_flags(&mut self, r: Reg) -> Result<()> {
        self.reg(r).map(|register| register.update_flags(&mut self.state.flags))
    }

    fn push_val(&mut self, val: Byte) -> Result<()> {
        self.state.sp.overflow_sub(1);
        let address = self.state.sp.into();
        self.write_byte(address, val)
    }

    fn push_addr(&mut self, address: Address) -> Result<()> {
        self.push_val(((address >> 8) & 0xff) as Byte)?;
        self.push_val((address & 0xff) as Byte)
    }

    fn push_reg(&mut self, r: Reg) -> Result<()> {
        let val = self.reg(r)?.val;
        self.push_val(val)
    }

    fn push_flags(&mut self) -> Result<()> {
        let val = self.state.pack_flags();
        self.push_val(val)
    }

    fn pop_val(&mut self) -> Result<Byte> {
        let val = self.read_byte(self.state.sp.into())?;
        self.state.sp.overflow_add(1);
        Ok(val)
    }

    fn pop_addr(&mut self) -> Result<Address> {
        let (lo, hi) = (self.pop_val()?, self.pop_val()?);
        Ok((hi as Address) << 8 | (lo as Address))
    }

    fn pop_reg(&mut self, r: Reg) -> Result<()> {
        let val = self.pop_val()?;
        self.reg_set(r, val)
    }

    fn pop_flags(&mut self) -> Result<()> {
        let val = self.pop_val()?;
        self.state.unpack_flags(val);
        Ok(())
    }
}

/// Carry bit Instructions
impl<M: Mmu, O: OutputBus, I: InputBus, H: CallHook> Cpu<M, O, I, H> {
    fn nop(&mut self) -> Periods { 4 }
}

/// Carry bit Instructions
impl<M: Mmu, O: OutputBus, I: InputBus, H: CallHook> Cpu<M, O, I, H> {
    fn cmc(&mut self) -> Periods {
        self.state.flags.toggle(Carry);
        4
    }
    fn stc(&mut self) -> Periods {
        self.state.flags.set(Carry);
        4
    }
}


/// Immediate Instructions
impl<M: Mmu, O: OutputBus, I: InputBus, H: CallHook> Cpu<M, O, I, H> {
    fn lxi(&mut self, v: self::RegPairValue) -> Periods {
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
        10
    }

    fn mvi(&mut self, r: Reg, val: Byte) -> Result<Periods> {
        self.reg_set(r, val).map(
            |_| match r {
                Reg::M => 10,
                _ => 7
            }
        )
    }

    fn adi(&mut self, val: Byte) -> Periods {
        self.accumulator_add(val);
        7
    }

    fn aci(&mut self, val: Byte) -> Periods {
        let other = if self.carry() { val + 1 } else { val };
        self.accumulator_add(other);
        7
    }

    fn sui(&mut self, val: Byte) -> Periods {
        self.accumulator_sub(val);
        7
    }

    fn sbi(&mut self, val: Byte) -> Periods {
        let other = if self.carry() { val + 1 } else { val };
        self.accumulator_sub(other);
        7
    }

    fn cpi(&mut self, val: Byte) -> Periods {
        self.compare(val.into());
        7
    }
}

/// Single Register Instructions
impl<M: Mmu, O: OutputBus, I: InputBus, H: CallHook> Cpu<M, O, I, H> {
    fn cma(&mut self) -> Result<Periods> {
        self.reg_apply(self::Reg::A, |r| r.one_complement())
            .map(|_| 4)
    }

    fn inr(&mut self, reg: self::Reg) -> Result<Periods> {
        self.reg_apply(reg, |r| { r.increment(); })?;
        self.fix_static_flags(reg)?;
        let auxcarry = self.reg(reg)?.low() == 0x00;
        self.state.flags.val(AuxCarry, auxcarry);
        Ok(match reg {
            Reg::M => 10,
            _ => 5
        })
    }

    fn dcr(&mut self, reg: self::Reg) -> Result<Periods> {
        self.reg_apply(reg, |r| { r.decrement(); })?;
        self.fix_static_flags(reg)?;
        let auxcarry = self.reg(reg)?.low() == 0x0f;
        self.state.flags.val(AuxCarry, auxcarry);
        Ok(match reg {
            Reg::M => 10,
            _ => 5
        })
    }

    fn daa(&mut self) -> Result<Periods> {
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
        self.fix_static_flags(Reg::A)?;
        Ok(4)
    }
}

/// Data transfer Instruction
impl<M: Mmu, O: OutputBus, I: InputBus, H: CallHook> Cpu<M, O, I, H> {
    fn mov(&mut self, f: Reg, t: Reg) -> Result<Periods> {
        let orig = self.reg(f)?;
        self.reg_set(t, orig)?;
        Ok(match (f, t) {
            (Reg::M, _) | (_, Reg::M) => 7,
            _ => 5
        })
    }

    fn stax(&mut self, rp: RegPair) -> Result<Periods> {
        let address = self.address(rp);
        let val = self.state.a.into();
        self.write_byte(address, val)?;
        Ok(7)
    }

    fn ldax(&mut self, rp: RegPair) -> Result<Periods> {
        let address = self.address(rp);
        self.state.a = self.read_byte(address)?.into();
        Ok(7)
    }
}

/// Carry and AuxCarry
#[allow(dead_code)]
impl<M: Mmu, O: OutputBus, I: InputBus, H: CallHook> Cpu<M, O, I, H> {
    fn carry(&self) -> bool {
        self.state.flag(Carry)
    }

    fn carry_clear(&mut self) {
        self.set_carry_state(false)
    }

    fn carry_set(&mut self) {
        self.set_carry_state(true)
    }

    fn set_carry_state(&mut self, state: bool) {
        self.state.flags.val(Carry, state)
    }

    fn aux_carry_clear(&mut self) {
        self.set_aux_carry_state(false)
    }

    fn set_aux_carry_state(&mut self, auxcarry: bool) {
        self.state.flags.val(AuxCarry, auxcarry);
    }
}

/// Accumulator
impl<M: Mmu, O: OutputBus, I: InputBus, H: CallHook> Cpu<M, O, I, H> {
    fn accumulator_periods(&self, r: Reg) -> Result<Periods> {
        Ok(match r {
            Reg::M => 7,
            _ => 4
        })
    }

    fn accumulator_add(&mut self, other: u8) {
        let auxcarry = (self.state.a.low() + (other & 0x0f)) > 0x0f;
        let carry = self.state.a.overflow_add(other);
        self.set_carry_state(carry);
        self.set_aux_carry_state(auxcarry);
        self.fix_static_flags(Reg::A).unwrap()
    }

    fn accumulator_sub(&mut self, other: u8) -> () {
        let auxcarry = self.state.a.low() < (other & 0x0f);
        let carry = self.state.a.overflow_sub(other);
        self.set_carry_state(carry);
        self.set_aux_carry_state(auxcarry);
        self.fix_static_flags(Reg::A).unwrap()
    }

    fn add(&mut self, r: Reg) -> Result<Periods> {
        let other = self.reg(r)?.val;
        self.accumulator_add(other);
        self.accumulator_periods(r)
    }

    fn adc(&mut self, r: Reg) -> Result<Periods> {
        let v = self.reg(r)?.val;
        let other = if self.carry() { v + 1 } else { v };
        self.accumulator_add(other);
        self.accumulator_periods(r)
    }

    fn sub(&mut self, r: Reg) -> Result<Periods> {
        let other = self.reg(r)?.val;
        self.accumulator_sub(other);
        self.accumulator_periods(r)
    }

    fn sbb(&mut self, r: Reg) -> Result<Periods> {
        let v = self.reg(r)?.val;
        let other = if self.carry() { v + 1 } else { v };
        self.accumulator_sub(other);
        self.accumulator_periods(r)
    }

    fn ana(&mut self, r: Reg) -> Result<Periods> {
        let other = self.reg(r)?.val;
        self.accumulator_and(other)?;
        self.accumulator_periods(r)
    }

    fn xra(&mut self, r: Reg) -> Result<Periods> {
        let other = self.reg(r)?.val;
        self.accumulator_xor(other)?;
        self.accumulator_periods(r)
    }

    fn ora(&mut self, r: Reg) -> Result<Periods> {
        let other = self.reg(r)?.val;
        self.accumulator_or(other)?;
        self.accumulator_periods(r)
    }

    fn ani(&mut self, other: Byte) -> Result<Periods> {
        self.accumulator_and(other)?;
        Ok(7)
    }

    fn xri(&mut self, other: Byte) -> Result<Periods> {
        self.accumulator_xor(other)?;
        Ok(7)
    }

    fn ori(&mut self, other: Byte) -> Result<Periods> {
        self.accumulator_or(other)?;
        Ok(7)
    }

    fn accumulator_and(&mut self, other: u8) -> Result<()> {
        self.state.a = (self.state.a.val & other).into();
        self.fix_static_flags(Reg::A)?;
        self.carry_clear();
        Ok(())
    }

    fn accumulator_or(&mut self, other: u8) -> Result<()> {
        self.state.a = (self.state.a.val | other).into();
        self.fix_static_flags(Reg::A)?;
        self.carry_clear();
        Ok(())
    }

    fn accumulator_xor(&mut self, other: u8) -> Result<()> {
        self.state.a = (self.state.a.val ^ other).into();
        self.fix_static_flags(Reg::A)?;
        self.carry_clear();
        self.aux_carry_clear();
        Ok(())
    }

    fn cmp(&mut self, r: Reg) -> Result<Periods> {
        let other = self.reg(r)?;
        self.compare(other);
        Ok(7)
    }

    fn compare(&mut self, other: RegByte) {
        let old = self.state.a;
        self.accumulator_sub(other.into());
        self.state.a = old;
    }
}

const RIGHT_BIT: u8 = 0;
const LEFT_BIT: u8 = 7;

/// Rotate
impl<M: Mmu, O: OutputBus, I: InputBus, H: CallHook> Cpu<M, O, I, H> {
    fn rlc(&mut self) -> Periods {
        let carry = self.state.a.rotate_left();
        self.set_carry_state(carry);
        4
    }
    fn rrc(&mut self) -> Periods {
        let carry = self.state.a.rotate_right();
        self.set_carry_state(carry);
        4
    }
    fn ral(&mut self) -> Periods {
        self.state.a.rotate_left();
        self.swap_carry_bit(RIGHT_BIT);
        4
    }
    fn rar(&mut self) -> Periods {
        self.state.a.rotate_right();
        self.swap_carry_bit(LEFT_BIT);
        4
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
impl<M: Mmu, O: OutputBus, I: InputBus, H: CallHook> Cpu<M, O, I, H> {
    fn push(&mut self, bp: BytePair) -> Result<Periods> {
        use self::BytePair::*;
        match bp {
            BC => {
                self.push_reg(Reg::B)?;
                self.push_reg(Reg::C)?;
            }
            DE => {
                self.push_reg(Reg::D)?;
                self.push_reg(Reg::E)?;
            }
            HL => {
                self.push_reg(Reg::H)?;
                self.push_reg(Reg::L)?;
            }
            AF => {
                self.push_reg(Reg::A)?;
                self.push_flags()?;
            }
        };
        Ok(11)
    }

    fn pop(&mut self, bp: BytePair) -> Result<Periods> {
        use self::BytePair::*;
        match bp {
            BC => {
                self.pop_reg(Reg::C)?;
                self.pop_reg(Reg::B)?;
            }
            DE => {
                self.pop_reg(Reg::E)?;
                self.pop_reg(Reg::D)?;
            }
            HL => {
                self.pop_reg(Reg::L)?;
                self.pop_reg(Reg::H)?;
            }
            AF => {
                self.pop_flags()?;
                self.pop_reg(Reg::A)?;
            }
        };
        Ok(10)
    }

    fn dad(&mut self, rp: RegPair) -> Periods {
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
        10
    }

    fn inx(&mut self, rp: RegPair) -> Periods {
        self.reg_address_apply(rp, |a| { a.overflow_add(1); });
        5
    }

    fn dcx(&mut self, rp: RegPair) -> Periods {
        self.reg_address_apply(rp, |a| { a.overflow_sub(1); });
        5
    }

    fn xthl(&mut self) -> Result<Periods> {
        let sp = self.state.sp.into();
        let l = self.state.l.val;
        self.state.l = self.replace_byte(sp, l)?.into();
        let h = self.state.h.val;
        self.state.h = self.replace_byte(sp + 1, h)?.into();
        Ok(18)
    }

    fn xchg(&mut self) -> Periods {
        swap(&mut self.state.d, &mut self.state.h);
        swap(&mut self.state.e, &mut self.state.l);
        4
    }

    fn sphl(&mut self) -> Periods {
        self.state.sp = self.hl().into();
        5
    }
}

/// Direct Addressing Instructions
impl<M: Mmu, O: OutputBus, I: InputBus, H: CallHook> Cpu<M, O, I, H> {
    fn sta(&mut self, address: Address) -> Result<Periods> {
        let value = self.state.a.val;
        self.write_byte(address, value)?;
        Ok(13)
    }

    fn lda(&mut self, address: Address) -> Result<Periods> {
        self.state.a = self.read_byte(address)?.into();
        Ok(13)
    }

    fn shld(&mut self, address: Address) -> Result<Periods> {
        let i = self.state.l.val;
        self.write_byte(address, i)?;
        let h = self.state.h.val;
        self.write_byte(address + 1, h)?;
        Ok(16)
    }

    fn lhld(&mut self, address: Address) -> Result<Periods> {
        self.state.l = self.read_byte(address)?.into();
        self.state.h = self.read_byte(address + 1)?.into();
        Ok(16)
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

impl<M: Mmu, O: OutputBus, I: InputBus, H: CallHook> Mmu for Cpu<M, O, I, H> {
    fn read_byte(&self, address: u16) -> Result<u8> {
        let res = self.mmu.read_byte(address)?;
        Ok(res)
    }

    fn write_byte(&mut self, address: u16, val: u8) -> Result<()> {
        self.mmu.write_byte(address, val)
    }

    fn dump(&self) -> String {
        self.mmu.dump()
    }
}

/// Jump Instructions
impl<M: Mmu, O: OutputBus, I: InputBus, H: CallHook> Cpu<M, O, I, H> {
    fn pchl(&mut self) -> Periods {
        self.state.pc = self.hl();
        5
    }

    fn jump(&mut self, address: Address) -> Periods {
        self.state.pc = address.into();
        10
    }

    fn jump_conditionals(&mut self, address: Address, flag: Flag, should_be: bool) -> Periods {
        if self.state.flags.get(flag) == should_be {
            self.jump(address);
        }
        10
    }

    fn call(&mut self, address: Address) -> Result<Periods> {
        if let Some(r) = self.hook.handle(address, &self.state, &self.mmu) {
            return r.map(|_| 0);
        }
        let addr = self.state.pc.into();
        self.push_addr(addr)?;
        self.jump(address);
        Ok(17)
    }

    fn call_conditionals(&mut self, address: Address, flag: Flag, should_be: bool) -> Result<Periods> {
        if self.state.flags.get(flag) == should_be {
            self.call(address)
        } else {
            Ok(11)
        }
    }

    fn ret(&mut self) -> Result<Periods> {
        let addr = self.pop_addr()?;
        self.jump(addr);
        Ok(10)
    }

    fn ret_conditionals(&mut self, flag: Flag, should_be: bool) -> Result<Periods> {
        if self.state.flags.get(flag) == should_be {
            self.ret()?;
            Ok(11)
        } else {
            Ok(5)
        }
    }

    fn rst(&mut self, irq: IrqAddr) -> Result<Periods> {
        self.call(irq.into())?;
        Ok(11)
    }
}

/// Interrupts
impl<M: Mmu, O: OutputBus, I: InputBus, H: CallHook> Cpu<M, O, I, H> {
    fn ei(&mut self) -> Periods {
        self.interrupt_enabled = true;
        4
    }

    fn di(&mut self) -> Periods {
        self.interrupt_enabled = false;
        4
    }
}

/// IO
impl<M: Mmu, O: OutputBus, I: InputBus, H: CallHook> Cpu<M, O, I, H> {
    fn input(&mut self, id: Byte) -> Periods {
        let val = self.input.read(id);
        self.state.set_a(val);
        10
    }

    fn output(&mut self, id: Byte) -> Periods {
        self.output.send(id, self.state.a.val);
        10
    }
}

/// Halt
impl<M: Mmu, O: OutputBus, I: InputBus, H: CallHook> Cpu<M, O, I, H> {
    fn halt(&mut self) -> Periods {
        self.run_state = CpuState::Stopped;
        7
    }
}
