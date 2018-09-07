use super::*;
use super::Cpu as GenCpu;
use rstest::rstest;
use rstest::rstest_parametrize;
use std::cell::RefCell;
use std::rc::Rc;
use io_bus::{VoidIO, test::Loopback};

type Cpu = GenCpu<PlainMemory, VoidIO, VoidIO>;
type PlainMemoryCpu<O, I> = GenCpu<PlainMemory, O, I>;

fn cpu() -> Cpu {
    let state = State {
        b: 0x10.into(),
        c: 0xa6.into(),
        d: 0x20.into(),
        e: 0xe6.into(),
        h: 0x22.into(),
        l: 0xee.into(),
        sp: 0x1234.into(),
        ..Default::default()
    };
    Cpu { state, ..Default::default() }
}

trait CpuQuery {
    type Result: PartialEq;

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
            M => cpu.get_m().unwrap(),
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

impl CpuQuery for Address {
    type Result = Byte;

    fn ask(&self, cpu: &Cpu) -> <Self as CpuQuery>::Result {
        cpu.mmu.read_byte(*self).unwrap()
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
            M(v) => { cpu.set_m(*v).unwrap(); }
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

mod pair_register {
    use self::BytePair::*;
    use super::*;

    #[rstest]
    fn push_should_put_register_pair_on_the_stack(mut cpu: Cpu) {
        let sp = 0x321c;
        cpu.set_de(0x8f9d);
        cpu.state.set_sp(sp);

        cpu.exec(Push(BytePair::DE));

        assert_eq!(cpu.mmu.read_byte(sp - 1), Ok(0x8f));
        assert_eq!(cpu.mmu.read_byte(sp - 2), Ok(0x9d));
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

        assert_eq!(cpu.mmu.read_byte(sp - 1), Ok(0xde));
        assert_eq!(cpu.mmu.read_byte(sp - 2), Ok(0x43));
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
        cpu.mmu.write_byte(sp, 0x8f);
        cpu.mmu.write_byte(sp + 1, 0x9d);
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
        cpu.mmu.write_byte(sp, 0xc3);
        cpu.mmu.write_byte(sp + 1, 0xff);

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
        cpu.mmu.write(sp, &[0x0d, 0xf0]);

        cpu.exec(Xthl);

        assert_eq!(cpu.hl(), 0xf00d);
        assert_eq!(cpu.mmu.read_byte(sp), Ok(0x3c));
        assert_eq!(cpu.mmu.read_byte(sp + 1), Ok(0x0b));
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

        assert_eq!(cpu.mmu.read_byte(address), Ok(val));
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
    cmd, init, after,
    case(Unwrap("Inr(Reg::A)"), Unwrap("RegValue::A(0xa3)"), 0xa4),
    case(Unwrap("Dcr(Reg::B)"), Unwrap("RegValue::B(0x32)"), 0x31),
    case(Unwrap("Dcr(Reg::M)"), Unwrap("RegValue::M(0xaf)"), 0xae),
    )]
    fn single_register_command<I>(mut cpu: Cpu, cmd: Instruction, init: I, after: Byte)
        where
            I: ApplyState + Into<Reg>
    {
        init.apply(&mut cpu);

        cpu.exec(cmd);

        assert_eq!(init.into().ask(&cpu), after);
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

        assert_eq!(cpu.mmu.read_byte(0x3f16), Ok(0x32));
    }

    #[rstest_parametrize(
    reg_pair,
    case(Unwrap("RegPair::HL")),
    case(Unwrap("RegPair::SP")),
    )]
    fn stax_should_return_error(mut cpu: Cpu, reg_pair: RegPair) {
        assert!(cpu.exec(Stax(reg_pair)).is_err());
    }

    #[rstest]
    fn ldax_should_load_accumulator(mut cpu: Cpu) {
        let address = 0x78a2;
        cpu.mmu.write_byte(address, 0x21);
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
    fn ldax_should_return_error(mut cpu: Cpu, reg_pair: RegPair) {
        assert!(cpu.exec(Ldax(reg_pair)).is_err());
    }
}

mod accumulator {
    use super::*;

    #[rstest_parametrize(
    start, value, expected,
    case(0x12, 0xa0, 0xb2),
    case(0x54, 0x33, 0x87),
    case(0xfd, 0x04, 0x01),
    )]
    fn accumulator_add_should_perform_addition(mut cpu: Cpu, start: Byte, value: Byte, expected: Byte) {
        cpu.state.set_a(start);

        cpu.accumulator_add(value);

        assert_eq!(cpu.state.a, expected);
    }

    #[rstest]
    fn accumulator_add_should_update_the_carry_flag(mut cpu: Cpu) {
        cpu.state.set_a(0xf0);

        cpu.accumulator_add(0x13);

        assert!(cpu.carry());
    }

    #[rstest]
    fn accumulator_add_should_fix_static_flags(mut cpu: Cpu) {
        let a = 0x56;
        let data = 0xbe;
        cpu.state.a = a.into();

        cpu.accumulator_add(data);

        assert_eq!(Parity.ask(&cpu), true);
        assert_eq!(Zero.ask(&cpu), false);
        assert_eq!(Sign.ask(&cpu), false);
    }

    #[rstest_parametrize(
    start, init, cmd, expected,
    case(0x12, Unwrap("RegValue::B(0xa0)"), Unwrap("Add(Reg::B)"), 0xb2),
    case(0x12, Unwrap("()"), Unwrap("Add(Reg::A)"), 0x24),
    case(0x03, Unwrap("RegValue::E(0xc2)"), Unwrap("Adc(Reg::E)"), 0xc5),
    case(0x03, Unwrap("(RegValue::E(0xc2), Carry)"), Unwrap("Adc(Reg::E)"), 0xc6),
    case(0x12, Unwrap("()"), Unwrap("Adi(0xa0)"), 0xb2),
    case(0x12, Unwrap("Carry"), Unwrap("Aci(0xa0)"), 0xb3),
    )]
    fn additions_ops_result<I>(mut cpu: Cpu, start: Byte, init: I, cmd: Instruction, expected: Byte)
        where
            I: ApplyState,
    {
        cpu.state.set_a(start);
        init.apply(&mut cpu);

        cpu.exec(cmd);

        assert_eq!(expected, cpu.state.a.val);
    }

    #[rstest_parametrize(
    start, value, expected,
    case(0xfd, 0x04, 0xf9),
    case(0x12, 0xa0, 0x72),
    case(0x54, 0x33, 0x21),
    )]
    fn accumulator_sub_should_perform_subtraction(mut cpu: Cpu, start: Byte, value: Byte, expected: Byte) {
        cpu.state.set_a(start);

        cpu.accumulator_sub(value);

        assert_eq!(cpu.state.a, expected);
    }

    #[rstest]
    fn accumulator_sub_should_update_carry_flag(mut cpu: Cpu) {
        cpu.state.set_a(0x10);
        cpu.state.set_b(0x13);

        cpu.sub(Reg::B);

        assert!(cpu.carry());
    }

    #[rstest_parametrize(
    start, init, cmd, expected,
    case(0x32, Unwrap("RegValue::B(0x03)"), Unwrap("Sub(Reg::B)"), 0x2f),
    case(0x12, Unwrap("()"), Unwrap("Sub(Reg::A)"), 0x0),
    case(0xc2, Unwrap("RegValue::E(0x03)"), Unwrap("Sbb(Reg::E)"), 0xbf),
    case(0xc2, Unwrap("(RegValue::E(0x03), Carry)"), Unwrap("Sbb(Reg::E)"), 0xbe),
    case(0x32, Unwrap("()"), Unwrap("Sui(0x3)"), 0x2f),
    case(0x32, Unwrap("Carry"), Unwrap("Sbi(0x2)"), 0x2f),
    )]
    fn subtraction_ops_result<I>(mut cpu: Cpu, start: Byte, init: I, cmd: Instruction, expected: Byte)
        where
            I: ApplyState,
    {
        cpu.state.set_a(start);
        init.apply(&mut cpu);

        cpu.exec(cmd);

        assert_eq!(expected, cpu.state.a.val);
    }

    #[rstest_parametrize(
    start, other, expected,
    case(0x81, 0x7e, 0x00),
    case(0xa6, 0xa2, 0xa2),
    case(0x5a, 0xff, 0x5a),
    )]
    fn ana_should_perform_logical_and(mut cpu: Cpu, start: Byte, other: Byte, expected: Byte) {
        cpu.state.set_a(start);

        cpu.accumulator_and(other);

        assert_eq!(cpu.state.a, expected);
    }

    #[rstest_parametrize(
    start, other, expected,
    case(0x81, 0x7e, 0xff),
    case(0xa6, 0xa2, 0x04),
    case(0x5a, 0xff, 0xa5),
    )]
    fn accumulator_xor_should_perform_logical_xor(mut cpu: Cpu, start: Byte, other: Byte, expected: Byte)
    {
        cpu.state.set_a(start);

        cpu.accumulator_xor(other);

        assert_eq!(cpu.state.a, expected);
    }

    #[rstest_parametrize(
    start, other, expected,
    case(0x81, 0x7e, 0xff),
    case(0xa6, 0xa2, 0xa6),
    case(0x01, 0x80, 0x81),
    case(0x00, 0x00, 0x00),
    )]
    fn accumulator_or_should_perform_logical_or(mut cpu: Cpu, start: Byte, other: Byte, expected: Byte) {
        cpu.state.set_a(start);

        cpu.accumulator_or(other);

        assert_eq!(cpu.state.a, expected);
    }

    #[rstest_parametrize(
    start, init, cmd, expected,
    case(0xa6, Unwrap("RegValue::L(0xa2)"), Unwrap("Xra(Reg::L)"), 0x04),
    case(0x81, Unwrap("RegValue::H(0x7e)"), Unwrap("Ora(Reg::H)"), 0xff),
    case(0xae, Unwrap("()"), Unwrap("Ani(0x9b)"), 0x8a),
    case(0xa6, Unwrap("()"), Unwrap("Xri(0xa2)"), 0x04),
    case(0x81, Unwrap("()"), Unwrap("Ori(0x7e)"), 0xff),
    )]
    fn bit_logic_integration<I>(mut cpu: Cpu, start: Byte, init: I, cmd: Instruction, expected: Byte)
        where
            I: ApplyState
    {
        cpu.state.set_a(start);
        init.apply(&mut cpu);

        cpu.exec(cmd);

        assert_eq!(cpu.state.a, expected);
    }

    #[rstest_parametrize(
    a, v, zero, carry,
    case(0x34, 0x34, true, false),
    case(0x10, 0x12, false, true),
    case(0x33, 0x32, false, false),
    case(0x10, 0xff, false, true),
    case(0x83, 0x00, false, false),
    )]
    fn compare_should(mut cpu: Cpu, a: Byte, v: Byte, zero: bool, carry: bool) {
        cpu.state.set_a(a);

        cpu.compare(v.into());

        assert_eq!(cpu.state.flag(Zero), zero);
        assert_eq!(cpu.carry(), carry);
        assert_eq!(cpu.state.a, a);
    }

    #[rstest_parametrize(
    a, init, cmd, carry,
    case(0x10, Unwrap("RegValue::B(0x12)"), Unwrap("Cmp(Reg::B)"), true),
    case(0x10, Unwrap("RegValue::E(0x0f)"), Unwrap("Cmp(Reg::E)"), false),
    case(0x10, Unwrap("RegValue::B(0x82)"), Unwrap("Cmp(Reg::B)"), true),
    case(0x10, Unwrap("()"), Unwrap("Cpi(0x12)"), true),
    case(0x10, Unwrap("()"), Unwrap("Cpi(0x0f)"), false),
    case(0x10, Unwrap("()"), Unwrap("Cpi(0x82)"), true),
    )]
    fn compare_ops<I: ApplyState>(mut cpu: Cpu, a: Byte, init: I, cmd: Instruction, carry: bool) {
        cpu.state.set_a(a);
        init.apply(&mut cpu);

        cpu.exec(cmd);

        assert_eq!(cpu.carry(), carry);
    }

    #[rstest_parametrize(
    cmd, a, b,
    case(Unwrap("Add(Reg::B)"), 0xfe, 0x01),
    case(Unwrap("Adc(Reg::B)"), 0xfe, 0x01),
    case(Unwrap("Sub(Reg::B)"), 0x01, 0x02),
    case(Unwrap("Sbb(Reg::B)"), 0x01, 0x02),
    case(Unwrap("Ana(Reg::B)"), 0xfe, 0xf6),
    case(Unwrap("Xra(Reg::B)"), 0xfe, 0x07),
    case(Unwrap("Ora(Reg::B)"), 0xfe, 0x07),
    )]
    fn should_update_flags(mut cpu: Cpu, cmd: Instruction, a: Byte, b: Byte) {
        cpu.state.set_a(a);
        cpu.state.set_b(b);
        cpu.state.flags.set(Zero);

        cpu.exec(cmd);

        assert!(!cpu.state.flag(Zero));
        assert!(cpu.state.flag(Sign));
        assert!(cpu.state.flag(Parity));
    }

    #[rstest_parametrize(
    cmd,
    case(Unwrap("Ana(Reg::B)")),
    case(Unwrap("Xra(Reg::B)")),
    case(Unwrap("Ora(Reg::B)")),
    )]
    fn should_reset_carry_flag(mut cpu: Cpu, cmd: Instruction) {
        cpu.state.set_a(0xae);
        cpu.state.set_b(0x36);
        cpu.carry_set();

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

#[rstest]
fn sta_should_store_accumulator(mut cpu: Cpu) {
    let accumulator = 0xae;
    let addr = 0x320f;
    cpu.state.set_a(accumulator);

    cpu.exec(Sta(addr));

    assert_eq!(Ok(accumulator), cpu.mmu.read_byte(addr));
}

#[rstest]
fn lda_should_load_accumulator(mut cpu: Cpu) {
    let expected = 0xae;
    let addr = 0x320f;
    cpu.mmu.write_byte(addr, expected);

    cpu.exec(Lda(addr));

    assert_eq!(cpu.state.a, expected);
}

#[rstest]
fn shld_should_store_hl_pair(mut cpu: Cpu) {
    let hl = 0xd1f2;
    let addr = 0x2031;
    cpu.set_hl(hl);

    cpu.exec(Shld(addr));

    assert_eq!(Ok(cpu.state.l.val), cpu.mmu.read_byte(addr));
    assert_eq!(Ok(cpu.state.h.val), cpu.mmu.read_byte(addr + 1));
}

#[rstest]
fn lhld_should_load_hl_pair(mut cpu: Cpu) {
    let hl = 0xd1f2;
    let addr = 0x2031;

    cpu.mmu.write_byte(addr, 0xf2);
    cpu.mmu.write_byte(addr + 1, 0xd1);

    cpu.exec(Lhld(addr));

    assert_eq!(cpu.hl(), hl);
}

#[rstest]
fn pchl_should_load_pc(mut cpu: Cpu) {
    let addr = 0xad12;
    cpu.set_hl(addr);

    cpu.exec(Pchl);

    assert_eq!(cpu.state.pc, addr);
}

#[rstest]
fn jump_should_load_pc(mut cpu: Cpu) {
    let addr = 0xad12;

    cpu.exec(Jump(addr));

    assert_eq!(cpu.state.pc, addr);
}

#[rstest_parametrize(
start, init, cmd, expected,
case(0x3202, Carry, Unwrap("J(CondFlag::C, 0xa030)"), 0xa030),
case(0x3202, Unwrap("()"), Unwrap("J(CondFlag::C, 0xa030)"), 0x3202),
case(0x3202, Carry, Unwrap("J(CondFlag::NC, 0xa030)"), 0x3202),
case(0x3202, Unwrap("()"), Unwrap("J(CondFlag::NC, 0xa030)"), 0xa030),
case(0x3202, Zero, Unwrap("J(CondFlag::Z, 0xa030)"), 0xa030),
case(0x3202, Unwrap("()"), Unwrap("J(CondFlag::Z, 0xa030)"), 0x3202),
case(0x3202, Zero, Unwrap("J(CondFlag::NZ, 0xa030)"), 0x3202),
case(0x3202, Unwrap("()"), Unwrap("J(CondFlag::NZ, 0xa030)"), 0xa030),
case(0x3202, Sign, Unwrap("J(CondFlag::M, 0xa030)"), 0xa030),
case(0x3202, Unwrap("()"), Unwrap("J(CondFlag::M, 0xa030)"), 0x3202),
case(0x3202, Sign, Unwrap("J(CondFlag::P, 0xa030)"), 0x3202),
case(0x3202, Unwrap("()"), Unwrap("J(CondFlag::P, 0xa030)"), 0xa030),
case(0x3202, Parity, Unwrap("J(CondFlag::PE, 0xa030)"), 0xa030),
case(0x3202, Unwrap("()"), Unwrap("J(CondFlag::PE, 0xa030)"), 0x3202),
case(0x3202, Parity, Unwrap("J(CondFlag::PO, 0xa030)"), 0x3202),
case(0x3202, Unwrap("()"), Unwrap("J(CondFlag::PO, 0xa030)"), 0xa030),
)]
fn jump_conditionals<A>(mut cpu: Cpu, start: Address, init: A,
                        cmd: Instruction, expected: Address)
    where
        A: ApplyState
{
    cpu.state.set_pc(start);
    init.apply(&mut cpu);

    cpu.exec(cmd);

    assert_eq!(cpu.state.pc, expected)
}

#[rstest]
fn push_addr_should_push_high_part_first(mut cpu: Cpu) {
    let sp = 0x4212;
    let addr = 0xad12;
    cpu.state.set_sp(sp);

    cpu.push_addr(addr);

    assert_eq!(Ok(0xad), cpu.mmu.read_byte(sp - 1));
    assert_eq!(Ok(0x12), cpu.mmu.read_byte(sp - 2));
}

#[rstest]
fn pop_addr_should_pop_low_part_first(mut cpu: Cpu) {
    let sp = 0x4212;
    let addr = 0xad12;
    cpu.state.set_sp(sp);

    cpu.mmu.write_byte(sp, 0x12).unwrap();
    cpu.mmu.write_byte(sp + 1, 0xad).unwrap();

    assert_eq!(addr, cpu.pop_addr().unwrap());
}

#[rstest]
fn push_and_pop_addr_round_trip(mut cpu: Cpu) {
    let sp = 0x4212;
    let addr = 0xad12;
    cpu.state.set_sp(sp);

    cpu.push_addr(addr);

    assert_eq!(addr, cpu.pop_addr().unwrap());
}

#[rstest]
fn call_should_change_pc_and_push_return_address_on_stack(mut cpu: Cpu) {
    let start = 0x4212;
    let addr = 0xad12;

    cpu.state.set_pc(start);

    cpu.exec(Call(addr));

    assert_eq!(cpu.state.pc, addr);
    assert_eq!(cpu.pop_addr(), Ok(start));
}

#[rstest_parametrize(
start, init, cmd, expected,
case(0x3202, Carry, Unwrap("C(CondFlag::C, 0xa030)"), 0xa030),
case(0x3202, Unwrap("()"), Unwrap("C(CondFlag::C, 0xa030)"), 0x3202),
case(0x3202, Carry, Unwrap("C(CondFlag::NC, 0xa030)"), 0x3202),
case(0x3202, Unwrap("()"), Unwrap("C(CondFlag::NC, 0xa030)"), 0xa030),
case(0x3202, Zero, Unwrap("C(CondFlag::Z, 0xa030)"), 0xa030),
case(0x3202, Unwrap("()"), Unwrap("C(CondFlag::Z, 0xa030)"), 0x3202),
case(0x3202, Zero, Unwrap("C(CondFlag::NZ, 0xa030)"), 0x3202),
case(0x3202, Unwrap("()"), Unwrap("C(CondFlag::NZ, 0xa030)"), 0xa030),
case(0x3202, Sign, Unwrap("C(CondFlag::M, 0xa030)"), 0xa030),
case(0x3202, Unwrap("()"), Unwrap("C(CondFlag::M, 0xa030)"), 0x3202),
case(0x3202, Sign, Unwrap("C(CondFlag::P, 0xa030)"), 0x3202),
case(0x3202, Unwrap("()"), Unwrap("C(CondFlag::P, 0xa030)"), 0xa030),
case(0x3202, Parity, Unwrap("C(CondFlag::PE, 0xa030)"), 0xa030),
case(0x3202, Unwrap("()"), Unwrap("C(CondFlag::PE, 0xa030)"), 0x3202),
case(0x3202, Parity, Unwrap("C(CondFlag::PO, 0xa030)"), 0x3202),
case(0x3202, Unwrap("()"), Unwrap("C(CondFlag::PO, 0xa030)"), 0xa030),
)]
fn call_conditionals<A>(mut cpu: Cpu, start: Address, init: A,
                        cmd: Instruction, expected: Address)
    where
        A: ApplyState
{
    cpu.state.set_pc(start);
    init.apply(&mut cpu);

    cpu.exec(cmd);

    assert_eq!(cpu.state.pc, expected)
}

#[rstest]
fn ret_should_pop_address_from_stack_and_then_jump_to_it(mut cpu: Cpu) {
    let start = 0x4212;
    let addr = 0xad12;
    cpu.state.set_pc(start);
    cpu.push_addr(addr);

    cpu.exec(Ret);

    assert_eq!(cpu.state.pc, addr);
}

#[rstest]
fn ret_should_walk_the_stack(mut cpu: Cpu) {
    let addr = vec![0xad12, 0xe210, 0x0020];
    cpu.push_addr(addr[0]);
    cpu.push_addr(addr[1]);
    cpu.push_addr(addr[2]);

    cpu.exec(Ret);
    assert_eq!(cpu.state.pc, addr[2]);

    cpu.exec(Ret);
    assert_eq!(cpu.state.pc, addr[1]);

    cpu.exec(Ret);
    assert_eq!(cpu.state.pc, addr[0]);
}

#[rstest_parametrize(
start, addr, init, cmd, expected,
case(0x3202, 0xa030, Carry, Unwrap("R(CondFlag::C)"), 0xa030),
case(0x3202, 0xa030, Unwrap("()"), Unwrap("R(CondFlag::C)"), 0x3202),
case(0x3202, 0xa030, Carry, Unwrap("R(CondFlag::NC)"), 0x3202),
case(0x3202, 0xa030, Unwrap("()"), Unwrap("R(CondFlag::NC)"), 0xa030),
case(0x3202, 0xa030, Zero, Unwrap("R(CondFlag::Z)"), 0xa030),
case(0x3202, 0xa030, Unwrap("()"), Unwrap("R(CondFlag::Z)"), 0x3202),
case(0x3202, 0xa030, Zero, Unwrap("R(CondFlag::NZ)"), 0x3202),
case(0x3202, 0xa030, Unwrap("()"), Unwrap("R(CondFlag::NZ)"), 0xa030),
case(0x3202, 0xa030, Sign, Unwrap("R(CondFlag::M)"), 0xa030),
case(0x3202, 0xa030, Unwrap("()"), Unwrap("R(CondFlag::M)"), 0x3202),
case(0x3202, 0xa030, Sign, Unwrap("R(CondFlag::P)"), 0x3202),
case(0x3202, 0xa030, Unwrap("()"), Unwrap("R(CondFlag::P)"), 0xa030),
case(0x3202, 0xa030, Parity, Unwrap("R(CondFlag::PE)"), 0xa030),
case(0x3202, 0xa030, Unwrap("()"), Unwrap("R(CondFlag::PE)"), 0x3202),
case(0x3202, 0xa030, Parity, Unwrap("R(CondFlag::PO)"), 0x3202),
case(0x3202, 0xa030, Unwrap("()"), Unwrap("R(CondFlag::PO)"), 0xa030),
)]
fn return_conditionals<A>(mut cpu: Cpu, start: Address, addr: Address, init: A,
                          cmd: Instruction, expected: Address)
    where
        A: ApplyState
{
    cpu.state.set_pc(start);
    cpu.push_addr(addr);
    init.apply(&mut cpu);

    cpu.exec(cmd);

    assert_eq!(cpu.state.pc, expected)
}

#[rstest]
fn return_conditional_should_not_pop_address_if_condition_dont_meet(mut cpu: Cpu) {
    cpu.push_addr(0x4320);

    cpu.exec(R(CondFlag::C));

    assert_eq!(cpu.pop_addr(), Ok(0x4320))
}

#[rstest]
fn rst_should_push_pc_and_jump_to_isr(mut cpu: Cpu) {
    let start = 0x3ad0;
    let irq = IrqAddr::I3;
    let cmd = Rst(irq);
    cpu.state.set_pc(start);

    cpu.exec(cmd);

    assert_eq!(cpu.state.pc.val, 0x18);
    assert_eq!(cpu.pop_addr(), Ok(start));
}

#[rstest]
fn enable_interrupt(mut cpu: Cpu) {
    cpu.interrupt_enabled = false;

    cpu.exec(Ei);

    assert_eq!(cpu.interrupt_enabled, true);
}

#[rstest]
fn disable_interrupt(mut cpu: Cpu) {
    cpu.interrupt_enabled = true;

    cpu.exec(Di);

    assert_eq!(cpu.interrupt_enabled, false);
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
case(Unwrap("(RegValue::A(0x3d), RegValue::C(0x03))"), Unwrap("Add(Reg::C)"), true),
case(Unwrap("(RegValue::A(0x12), RegValue::B(0xf2))"), Unwrap("Add(Reg::B)"), false),
case(Unwrap("(RegValue::A(0xa4), RegValue::D(0x0d))"), Unwrap("Add(Reg::D)"), true),
case(Unwrap("(RegValue::A(0x0f), RegValue::D(0x01))"), Unwrap("Add(Reg::D)"), true),
case(Unwrap("(RegValue::A(0x0f), RegValue::H(0xa0))"), Unwrap("Add(Reg::H)"), false),
case(Unwrap("(RegValue::A(0x3d), (RegValue::C(0x02), Carry))"), Unwrap("Adc(Reg::C)"), true),
case(Unwrap("(RegValue::A(0x12), (RegValue::B(0xfc), Carry))"), Unwrap("Adc(Reg::B)"), false),
case(Unwrap("(RegValue::A(0xa4), (RegValue::D(0x0c), Carry))"), Unwrap("Adc(Reg::D)"), true),
case(Unwrap("(RegValue::A(0x0f), (RegValue::D(0x00), Carry))"), Unwrap("Adc(Reg::D)"), true),
case(Unwrap("(RegValue::A(0x0e), (RegValue::H(0xa0), Carry))"), Unwrap("Adc(Reg::H)"), false),
case(Unwrap("(RegValue::A(0x37), RegValue::C(0x08))"), Unwrap("Sub(Reg::C)"), true),
case(Unwrap("(RegValue::A(0x12), RegValue::B(0xf2))"), Unwrap("Sub(Reg::B)"), false),
case(Unwrap("(RegValue::A(0xa4), RegValue::D(0x05))"), Unwrap("Sub(Reg::D)"), true),
case(Unwrap("(RegValue::A(0x00), RegValue::D(0x01))"), Unwrap("Sub(Reg::D)"), true),
case(Unwrap("(RegValue::A(0x01), RegValue::H(0xa0))"), Unwrap("Sub(Reg::H)"), false),
case(Unwrap("(RegValue::A(0x37), (RegValue::C(0x07), Carry))"), Unwrap("Sbb(Reg::C)"), true),
case(Unwrap("(RegValue::A(0x12), (RegValue::B(0xf1), Carry))"), Unwrap("Sbb(Reg::B)"), false),
case(Unwrap("(RegValue::A(0xa4), (RegValue::D(0x04), Carry))"), Unwrap("Sbb(Reg::D)"), true),
case(Unwrap("(RegValue::A(0x00), (RegValue::D(0x00), Carry))"), Unwrap("Sbb(Reg::D)"), true),
case(Unwrap("(RegValue::A(0x01), (RegValue::H(0x9f), Carry))"), Unwrap("Sbb(Reg::H)"), false),
case(Unwrap("(RegValue::A(0x37), RegValue::C(0x08))"), Unwrap("Cmp(Reg::C)"), true),
case(Unwrap("(RegValue::A(0x12), RegValue::B(0xf2))"), Unwrap("Cmp(Reg::B)"), false),
case(Unwrap("(RegValue::A(0xa4), RegValue::D(0x05))"), Unwrap("Cmp(Reg::D)"), true),
case(Unwrap("(RegValue::A(0x00), RegValue::D(0x01))"), Unwrap("Cmp(Reg::D)"), true),
case(Unwrap("(RegValue::A(0x01), RegValue::H(0xa0))"), Unwrap("Cmp(Reg::H)"), false),
case(Unwrap("RegValue::A(0x3d)"), Unwrap("Adi(0x03)"), true),
case(Unwrap("RegValue::A(0x12)"), Unwrap("Adi(0xf2)"), false),
case(Unwrap("RegValue::A(0xa4)"), Unwrap("Adi(0x0d)"), true),
case(Unwrap("RegValue::A(0x0f)"), Unwrap("Adi(0x01)"), true),
case(Unwrap("RegValue::A(0x0f)"), Unwrap("Adi(0xa0)"), false),
case(Unwrap("(RegValue::A(0x3c), Carry)"), Unwrap("Aci(0x03)"), true),
case(Unwrap("RegValue::A(0x3c)"), Unwrap("Aci(0x03)"), false),
case(Unwrap("RegValue::A(0x37)"), Unwrap("Sui(0x08)"), true),
case(Unwrap("RegValue::A(0x12)"), Unwrap("Sui(0xf2)"), false),
case(Unwrap("(RegValue::A(0x37), Carry)"), Unwrap("Sbi(0x07)"), true),
case(Unwrap("(RegValue::A(0x12), Carry)"), Unwrap("Sbi(0xf1)"), false),
case(Unwrap("RegValue::A(0x37)"), Unwrap("Cpi(0x08)"), true),
case(Unwrap("RegValue::A(0x12)"), Unwrap("Cpi(0xf2)"), false),
case(Unwrap("RegValue::A(0xa4)"), Unwrap("Cpi(0x05)"), true),
case(Unwrap("RegValue::A(0x00)"), Unwrap("Cpi(0x01)"), true),
case(Unwrap("RegValue::A(0x01)"), Unwrap("Cpi(0xa0)"), false),
)]
fn should_affect_aux_carry<I: ApplyState>(mut cpu: Cpu, init: I, cmd: Instruction, expected: bool) {
    init.apply(&mut cpu);

    cpu.exec(cmd);

    assert_eq!(expected, cpu.state.flags.get(AuxCarry));
}

#[rstest_parametrize(
init, cmd, expected,
case(Unwrap("RegValue::B(0xaf)"), Unwrap("Xra(Reg::B)")),
)]
fn should_always_reset_aux_carry<I: ApplyState>(mut cpu: Cpu, init: I, cmd: Instruction) {
    init.apply(&mut cpu);
    cpu.set_aux_carry_state(true);

    cpu.exec(cmd);

    assert!(!cpu.state.flags.get(AuxCarry));
}

#[rstest]
fn nop_should_do_nothing(mut cpu: Cpu) {
    let state = cpu.state.clone();

    cpu.exec(Nop);

    assert_eq!(state, cpu.state);
}

mod io {
    use super::*;

    #[test]
    fn input_should_read_from_bus_into_accumulator() {
        #[derive(Default)]
        struct Loop;

        impl InputBus for Loop { fn read(&self, id: Byte) -> Byte { id } }

        let mut cpu = PlainMemoryCpu { input: Loop::default(), output: VoidIO::default(), ..Default::default() };
        let input_val = 0x42;

        cpu.exec(In(input_val));

        assert_eq!(cpu.state.a, input_val)
    }

    #[test]
    fn output_should_send_accumulator_to_output_bus() {
        #[derive(Default)]
        struct Out {id: RefCell<Option<Byte>>, data: RefCell<Option<Byte>>};
        impl OutputBus for Out {
            fn send(&self, id: Byte, data: Byte) {
                self.id.replace(Some(id));
                self.data.replace(Some(data));
            }
        }

        let mut cpu = PlainMemoryCpu { input: VoidIO::default(), output: Out::default(), ..Default::default() };
        let out_val = 0x31;
        let id = 0x12;

        cpu.state.set_a(out_val);

        cpu.exec(Out(id));

        assert_eq!(cpu.output.id.borrow().unwrap(), id);
        assert_eq!(cpu.output.data.borrow().unwrap(), out_val);
    }

    #[test]
    fn input_output_loop() {
        let io_loop: Rc<Loopback> = Rc::default();
        let mut cpu = PlainMemoryCpu { input: io_loop.clone(), output: io_loop.clone(), ..Default::default() };
        let val = 0x31;
        let device_id = 0x12;

        cpu.state.set_a(val);
        cpu.exec(Out(device_id));

        cpu.state.set_a(0x00);
        cpu.exec(In(device_id));

        assert_eq!(cpu.state.a, val)
    }
}

mod halt {
    use super::*;

    #[rstest]
    fn should_enter_in_stopped_state(mut cpu: Cpu) {
        cpu.exec(Hlt);

        assert!(cpu.is_stopped());
    }

    #[rstest]
    fn cpu_in_stopped_state_should_ignore_commands(mut cpu: Cpu) {
        let state = cpu.state.clone();

        cpu.run_state = CpuState::Stopped;
        assert!(cpu.is_stopped());

        cpu.exec(Mov(Reg::B, Reg::H));
        cpu.exec(Jump(0x3214));
        cpu.exec(Add(Reg::C));
        cpu.exec(Pop(BytePair::DE));

        assert_eq!(state, cpu.state);
    }
}

mod irq_instruction {
    use super::*;

    #[rstest]
    fn should_not_advance_pc(mut cpu: Cpu) {
        let address = 0x3200;
        cpu.state.set_pc(address);

        cpu.irq(IrqCmd::RawCmd(Nop));

        assert_eq!(cpu.state.pc, address);
    }

    #[rstest]
    fn should_disable_irq(mut cpu: Cpu) {
        assert!(cpu.interrupt_enabled);

        cpu.irq(IrqCmd::Irq3);

        assert!(!cpu.interrupt_enabled);
    }

    #[rstest]
    fn should_wake_up_cpu(mut cpu: Cpu) {
        cpu.run_state = CpuState::Stopped;

        cpu.irq(IrqCmd::Irq3);

        assert!(cpu.is_running());
    }

    #[rstest]
    fn should_execute_given_instruction(mut cpu: Cpu) {
        cpu.state.set_pc(0x1234);

        cpu.irq(IrqCmd::Irq3);

        assert_eq!(cpu.state.pc, 0x0018);
    }

    #[rstest]
    fn raw_execution(mut cpu: Cpu) {
        cpu.state.set_pc(0x1234);

        cpu.irq(IrqCmd::RawCmd(Ei));

        assert!(cpu.interrupt_enabled);
    }

}

#[rstest]
fn run_should_load_instruction_at_pc_and_exec_it(mut cpu: Cpu) {
    // 0xec, 0x22, 0x14 -> Cpe(0x1422)
    cpu.mmu.write(0x1234, &[0xec, 0x22, 0x14]);
    cpu.state.set_pc(0x1234);

    cpu.state.flags.clear(Parity);
    // cpu.apply(C(CondFlag::PE, (0x1422));
    // No call

    cpu.run().unwrap();

    assert_eq!(0x1237, cpu.state.pc.val);

    cpu.state.set_pc(0x1234);
    cpu.state.flags.set(Parity);

    // Should call
    cpu.run().unwrap();

    assert_eq!(0x1422, cpu.state.pc.val);
}
