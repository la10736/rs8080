use super::{
    *,
    asm::{
        Instruction::*,
        Instruction,
        Reg,
        RegPair,
    },
};
use cpu::CpuError;

trait It : Iterator<Item=Result<Byte, CpuError>> {}

struct CodeIterator<I: Iterator<Item=Result<Byte, CpuError>>> {
    code_iterator: I
}

impl<I: Iterator<Item=Result<Byte, CpuError>>> CodeIterator<I> {
    fn new(data: I) -> CodeIterator<I> {
        CodeIterator {
            code_iterator: data,
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct OpcodeError(String);

impl Into<String> for OpcodeError {
    fn into(self) -> String {
        self.0
    }
}

pub fn disassemble<C: AsRef<[u8]>>(codes: C) -> Result<Vec<Instruction>, CpuError> {
    CodeIterator::new(
        codes.as_ref().iter().cloned().map(|d| Ok(d))
    ).collect()
}

fn next_code(it: &mut impl Iterator<Item=Result<Byte, CpuError>>) -> Result<Byte, CpuError> {
    it.next().ok_or(CpuError::NoCode)?
}

pub fn opcode(it: &mut impl Iterator<Item=Result<Byte, CpuError>>) -> Result<Instruction, CpuError> {
    Ok(match next_code(it)? {
        0x00 => Nop,
        v if (v & 0xcf) == 0x01 => {
            Lxi(cmd_3(it, v)?.into())
        }
        v if (v & 0xef) == 0x02 => Stax((v & 0x10).into()),
        v if (v & 0xcf) == 0x03 => Inx((v & 0x30).into()),
        v if (v & 0xc7) == 0x04 => Inr(((v & 0x38) >> 3).into()),
        v if (v & 0xc7) == 0x05 => Dcr(((v & 0x38) >> 3).into()),
        v if (v & 0xc7) == 0x06 => Mvi(((v & 0x38) >> 3).into(), u8_data(it)?),
        0x07 => Rlc,
        v if (v & 0xcf) == 0x09 => Dad((v & 0x30).into()),
        0x0a => Ldax(RegPair::BC),
        v if (v & 0xcf) == 0x0b => Dcx((v & 0x30).into()),
        0x0f => Rrc,
        0x17 => Ral,
        0x1a => Ldax(RegPair::DE),
        0x1f => Rar,
        0x20 => Rim,
        0x27 => Daa,
        0x22 => Shld(u16_data(it)?),
        0x2a => Lhld(u16_data(it)?),
        0x2f => Cma,
        0x30 => Sim,
        0x32 => Sta(u16_data(it)?),
        0x37 => Stc,
        0x3a => Lda(u16_data(it)?),
        0x3c => Inr(Reg::A),
        0x3f => Cmc,
        0x76 => Hlt,
        v if v >= 0x40 && v <= 0x7f => Mov((v & 0x07).into(), ((v - 0x40) >> 3).into()),
        v if v >= 0x80 && v <= 0x87 => Add((v - 0x80).into()),
        v if v >= 0x88 && v <= 0x8f => Adc((v - 0x88).into()),
        v if v >= 0x90 && v <= 0x97 => Sub((v - 0x90).into()),
        v if v >= 0x98 && v <= 0x9f => Sbb((v - 0x98).into()),
        v if v >= 0xa0 && v <= 0xa7 => Ana((v - 0xa0).into()),
        v if v >= 0xa8 && v <= 0xaf => Xra((v - 0xa8).into()),
        v if v >= 0xb0 && v <= 0xb7 => Ora((v - 0xb0).into()),
        v if v >= 0xb8 && v <= 0xbf => Cmp((v - 0xb8).into()),
        v if (v & 0xc7) == 0xc0 => R((v & 0x38).into()),
        v if (v & 0xcf) == 0xc1 => Pop((v & 0x30).into()),
        v if (v & 0xc7) == 0xc2 => J((v & 0x38).into(), u16_data(it)?),
        0xc3 => Jump(u16_data(it)?),
        v if (v & 0xc7) == 0xc4 => C((v & 0x38).into(), u16_data(it)?),
        v if (v & 0xcf) == 0xc5 => Push((v & 0x30).into()),
        0xc6 => Adi(u8_data(it)?),
        v if (v & 0xc7) == 0xc7 => Rst((v & 0x38).into()),
        0xc9 => Ret,
        0xcd => Call(u16_data(it)?),
        0xce => Aci(u8_data(it)?),
        0xd3 => Out(u8_data(it)?),
        0xd6 => Sui(u8_data(it)?),
        0xdb => In(u8_data(it)?),
        0xde => Sbi(u8_data(it)?),
        0xe3 => Xthl,
        0xe6 => Ani(u8_data(it)?),
        0xe9 => Pchl,
        0xeb => Xchg,
        0xee => Xri(u8_data(it)?),
        0xf3 => Di,
        0xf6 => Ori(u8_data(it)?),
        0xf9 => Sphl,
        0xfb => Ei,
        0xfe => Cpi(u8_data(it)?),
        c => {
            return Err(CpuError::InvalidCode(format!("0x{:02x} Is not valid opcode!", c)));
        }
    }
    )
}

fn cmd_3(it: &mut impl Iterator<Item=Result<Byte, CpuError>>, code: u8) -> Result<[u8; 3], CpuError> {
    Ok([code, next_code(it)?, next_code(it)?])
}

fn u16_data(it: &mut impl Iterator<Item=Result<Byte, CpuError>>) -> Result<Word, CpuError> {
    Ok((next_code(it)? as u16) | ((next_code(it)? as u16) << 8))
}

fn u8_data(it: &mut impl Iterator<Item=Result<Byte, CpuError>>) -> Result<Byte, CpuError> {
    next_code(it).map(|v| v.into())
}

impl<I: Iterator<Item=Result<Byte, CpuError>>> Iterator for CodeIterator<I> {
    type Item = Result<Instruction, CpuError>;

    fn next(&mut self) -> Option<Self::Item> {
        match opcode(&mut self.code_iterator) {
            Err(CpuError::NoCode) => None,
            v => Some(v)
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    use rstest::rstest_parametrize;

    fn parse_str_bytes<S: AsRef<str>>(s: S) -> Vec<Byte> {
        s.as_ref().split(' ')
            .map(|v| Byte::from_str_radix(v, 16).unwrap())
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
    case("22 12 33", 0x22, 3, "SHLD   $3312"),
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
    case("34", 0x34, 1, "INR    (HL)"),
    case("35", 0x35, 1, "DCR    (HL)"),
    case("36 c2", 0x36, 2, "MVI    (HL),#0xc2"),
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
    case("50", 0x50, 1, "MOV    D <- B"),
    case("51", 0x51, 1, "MOV    D <- C"),
    case("52", 0x52, 1, "MOV    D <- D"),
    case("53", 0x53, 1, "MOV    D <- E"),
    case("54", 0x54, 1, "MOV    D <- H"),
    case("55", 0x55, 1, "MOV    D <- L"),
    case("56", 0x56, 1, "MOV    D <- (HL)"),
    case("57", 0x57, 1, "MOV    D <- A"),
    case("58", 0x58, 1, "MOV    E <- B"),
    case("59", 0x59, 1, "MOV    E <- C"),
    case("5a", 0x5a, 1, "MOV    E <- D"),
    case("5b", 0x5b, 1, "MOV    E <- E"),
    case("5c", 0x5c, 1, "MOV    E <- H"),
    case("5d", 0x5d, 1, "MOV    E <- L"),
    case("5e", 0x5e, 1, "MOV    E <- (HL)"),
    case("5f", 0x5f, 1, "MOV    E <- A"),
    case("60", 0x60, 1, "MOV    H <- B"),
    case("61", 0x61, 1, "MOV    H <- C"),
    case("62", 0x62, 1, "MOV    H <- D"),
    case("63", 0x63, 1, "MOV    H <- E"),
    case("64", 0x64, 1, "MOV    H <- H"),
    case("65", 0x65, 1, "MOV    H <- L"),
    case("66", 0x66, 1, "MOV    H <- (HL)"),
    case("67", 0x67, 1, "MOV    H <- A"),
    case("68", 0x68, 1, "MOV    L <- B"),
    case("69", 0x69, 1, "MOV    L <- C"),
    case("6a", 0x6a, 1, "MOV    L <- D"),
    case("6b", 0x6b, 1, "MOV    L <- E"),
    case("6c", 0x6c, 1, "MOV    L <- H"),
    case("6d", 0x6d, 1, "MOV    L <- L"),
    case("6e", 0x6e, 1, "MOV    L <- (HL)"),
    case("6f", 0x6f, 1, "MOV    L <- A"),
    case("70", 0x70, 1, "MOV    (HL) <- B"),
    case("71", 0x71, 1, "MOV    (HL) <- C"),
    case("72", 0x72, 1, "MOV    (HL) <- D"),
    case("73", 0x73, 1, "MOV    (HL) <- E"),
    case("74", 0x74, 1, "MOV    (HL) <- H"),
    case("75", 0x75, 1, "MOV    (HL) <- L"),
    case("76", 0x76, 1, "HLT"),
    case("77", 0x77, 1, "MOV    (HL) <- A"),
    case("78", 0x78, 1, "MOV    A <- B"),
    case("79", 0x79, 1, "MOV    A <- C"),
    case("7a", 0x7a, 1, "MOV    A <- D"),
    case("7b", 0x7b, 1, "MOV    A <- E"),
    case("7c", 0x7c, 1, "MOV    A <- H"),
    case("7d", 0x7d, 1, "MOV    A <- L"),
    case("7e", 0x7e, 1, "MOV    A <- (HL)"),
    case("7f", 0x7f, 1, "MOV    A <- A"),
    case("80", 0x80, 1, "ADD    B"),
    case("81", 0x81, 1, "ADD    C"),
    case("82", 0x82, 1, "ADD    D"),
    case("83", 0x83, 1, "ADD    E"),
    case("84", 0x84, 1, "ADD    H"),
    case("85", 0x85, 1, "ADD    L"),
    case("86", 0x86, 1, "ADD    (HL)"),
    case("87", 0x87, 1, "ADD    A"),
    case("88", 0x88, 1, "ADC    B"),
    case("89", 0x89, 1, "ADC    C"),
    case("8a", 0x8a, 1, "ADC    D"),
    case("8b", 0x8b, 1, "ADC    E"),
    case("8c", 0x8c, 1, "ADC    H"),
    case("8d", 0x8d, 1, "ADC    L"),
    case("8e", 0x8e, 1, "ADC    (HL)"),
    case("8f", 0x8f, 1, "ADC    A"),
    case("90", 0x90, 1, "SUB    B"),
    case("91", 0x91, 1, "SUB    C"),
    case("92", 0x92, 1, "SUB    D"),
    case("93", 0x93, 1, "SUB    E"),
    case("94", 0x94, 1, "SUB    H"),
    case("95", 0x95, 1, "SUB    L"),
    case("96", 0x96, 1, "SUB    (HL)"),
    case("97", 0x97, 1, "SUB    A"),
    case("98", 0x98, 1, "SBB    B"),
    case("99", 0x99, 1, "SBB    C"),
    case("9a", 0x9a, 1, "SBB    D"),
    case("9b", 0x9b, 1, "SBB    E"),
    case("9c", 0x9c, 1, "SBB    H"),
    case("9d", 0x9d, 1, "SBB    L"),
    case("9e", 0x9e, 1, "SBB    (HL)"),
    case("9f", 0x9f, 1, "SBB    A"),
    case("a0", 0xa0, 1, "ANA    B"),
    case("a1", 0xa1, 1, "ANA    C"),
    case("a2", 0xa2, 1, "ANA    D"),
    case("a3", 0xa3, 1, "ANA    E"),
    case("a4", 0xa4, 1, "ANA    H"),
    case("a5", 0xa5, 1, "ANA    L"),
    case("a6", 0xa6, 1, "ANA    (HL)"),
    case("a7", 0xa7, 1, "ANA    A"),
    case("a8", 0xa8, 1, "XRA    B"),
    case("a9", 0xa9, 1, "XRA    C"),
    case("aa", 0xaa, 1, "XRA    D"),
    case("ab", 0xab, 1, "XRA    E"),
    case("ac", 0xac, 1, "XRA    H"),
    case("ad", 0xad, 1, "XRA    L"),
    case("ae", 0xae, 1, "XRA    (HL)"),
    case("af", 0xaf, 1, "XRA    A"),
    case("b0", 0xb0, 1, "ORA    B"),
    case("b1", 0xb1, 1, "ORA    C"),
    case("b2", 0xb2, 1, "ORA    D"),
    case("b3", 0xb3, 1, "ORA    E"),
    case("b4", 0xb4, 1, "ORA    H"),
    case("b5", 0xb5, 1, "ORA    L"),
    case("b6", 0xb6, 1, "ORA    (HL)"),
    case("b7", 0xb7, 1, "ORA    A"),
    case("b8", 0xb8, 1, "CMP    B"),
    case("b9", 0xb9, 1, "CMP    C"),
    case("ba", 0xba, 1, "CMP    D"),
    case("bb", 0xbb, 1, "CMP    E"),
    case("bc", 0xbc, 1, "CMP    H"),
    case("bd", 0xbd, 1, "CMP    L"),
    case("be", 0xbe, 1, "CMP    (HL)"),
    case("bf", 0xbf, 1, "CMP    A"),
    case("c0", 0xc0, 1, "RNZ"),
    case("c1", 0xc1, 1, "POP    BC"),
    case("c2 18 d4", 0xc2, 3, "JNZ    $d418"),
    case("c3 d4 18", 0xc3, 3, "JMP    $18d4"),
    case("c4 af de", 0xc4, 3, "CNZ    $deaf"),
    case("c5", 0xc5, 1, "PUSH   BC"),
    case("c6 02", 0xc6, 2, "ADI    #0x02"),
    case("c7", 0xc7, 1, "RST    $00"),
    case("c8", 0xc8, 1, "RZ"),
    case("c9", 0xc9, 1, "RET"),
    case("ca 22 44", 0xca, 3, "JZ     $4422"),
    case("cc ad 43", 0xcc, 3, "CZ     $43ad"),
    case("cd aa bb", 0xcd, 3, "CALL   $bbaa"),
    case("ce 21", 0xce, 2, "ACI    #0x21"),
    case("cf", 0xcf, 1, "RST    $08"),
    case("d0", 0xd0, 1, "RNC"),
    case("d1", 0xd1, 1, "POP    DE"),
    case("d2 1a 32", 0xd2, 3, "JNC    $321a"),
    case("d3 41", 0xd3, 2, "OUT    #0x41"),
    case("d4 11 44", 0xd4, 3, "CNC    $4411"),
    case("d5", 0xd5, 1, "PUSH   DE"),
    case("d6 e3", 0xd6, 2, "SUI    #0xe3"),
    case("d7", 0xd7, 1, "RST    $10"),
    case("d8", 0xd8, 1, "RC"),
    case("da af 12", 0xda, 3, "JC     $12af"),
    case("db 0a", 0xdb, 2, "IN     #0x0a"),
    case("dc 12 14", 0xdc, 3, "CC     $1412"),
    case("de 11", 0xde, 2, "SBI    #0x11"),
    case("df", 0xdf, 1, "RST    $18"),
    case("e0", 0xe0, 1, "RPO"),
    case("e1", 0xe1, 1, "POP    HL"),
    case("e2 03 10", 0xe2, 3, "JPO    $1003"),
    case("e3", 0xe3, 1, "XTHL"),
    case("e4 12 24", 0xe4, 3, "CPO    $2412"),
    case("e5", 0xe5, 1, "PUSH   HL"),
    case("e6 32", 0xe6, 2, "ANI    #0x32"),
    case("e7", 0xe7, 1, "RST    $20"),
    case("e8", 0xe8, 1, "RPE"),
    case("e9", 0xe9, 1, "PCHL"),
    case("ea de 08", 0xea, 3, "JPE    $08de"),
    case("eb", 0xeb, 1, "XCHG"),
    case("ec 22 14", 0xec, 3, "CPE    $1422"),
    case("ee a4", 0xee, 2, "XRI    #0xa4"),
    case("ef", 0xef, 1, "RST    $28"),
    case("f0", 0xf0, 1, "RP"),
    case("f1", 0xf1, 1, "POP    PSW"),
    case("f2 00 20", 0xf2, 3, "JP     $2000"),
    case("f3", 0xf3, 1, "DI"),
    case("f4 12 a4", 0xf4, 3, "CP     $a412"),
    case("f5", 0xf5, 1, "PUSH   PSW"),
    case("f6 a4", 0xf6, 2, "ORI    #0xa4"),
    case("f7", 0xf7, 1, "RST    $30"),
    case("f8", 0xf8, 1, "RM"),
    case("f9", 0xf9, 1, "SPHL"),
    case("fa af f7", 0xfa, 3, "JM     $f7af"),
    case("fb", 0xfb, 1, "EI"),
    case("fc 02 e4", 0xfc, 3, "CM     $e402"),
    case("fe 4a", 0xfe, 2, "CPI    #0x4a"),
    case("ff", 0xff, 1, "RST    $38"),
    )
    ]
    fn opcode_parser(bytes: &str, code: Byte, length: u16, desc: &str) {
        let bytes = parse_str_bytes(bytes);

        let d = disassemble(&bytes).unwrap()[0];

        assert_eq!(code, d.opcode());
        assert_eq!(length, d.length());
        assert_eq!(desc, &format!("{}", d));
    }

    #[rstest_parametrize(opcode,
    case(0x08),
    case(0x10),
    case(0x18),
    case(0x28),
    case(0x38),
    case(0xcb),
    case(0xd9),
    case(0xdd),
    case(0xed),
    case(0xfd),
    )
    ]
    fn opcode_that_doesnt_exists(opcode: Byte) {
        let codes = [opcode];

        let d = disassemble(&codes);

        assert!(d.is_err(), "But it is {:?}", d);
        assert_eq!(CpuError::InvalidCode(format!("0x{:02x} Is not valid opcode!", opcode)),
                   d.unwrap_err())
    }
}
