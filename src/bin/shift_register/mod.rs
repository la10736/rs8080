#[derive(Clone, Copy, Default)]
pub struct ShiftRegister {
    value: u16,
    offset: u8,
}

impl ShiftRegister {
    pub fn get(&self) -> u8 {
        (self.value >> (8 - self.offset)) as u8
    }

    pub fn push(mut self, upper: u8) -> Self {
        self.value = ((upper as u16) << 8) | (self.value >> 8);
        self
    }

    pub fn set_offset(mut self, offset: u8) -> Self {
        self.offset = offset & 0x07;
        self
    }
}

impl From<u16> for ShiftRegister {
    fn from(v: u16) -> Self {
        ShiftRegister { value: v, offset: 0 }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use rstest::rstest_parametrize;

    #[rstest_parametrize(
    value, offset, expected,
    case(0b0110_0111_1010_1100, 0, 0b0110_0111),
    case(0b0110_0111_1010_1100, 1, 0b1100_1111),
    case(0b0110_0111_1010_1100, 2, 0b1001_1110),
    case(0b0110_0111_1010_1100, 7, 0b1101_0110),
    )
    ]
    fn should_return_value(value: u16, offset: u8, expected: u8) {
        let sr = ShiftRegister { value, offset};

        assert_eq!(expected, sr.get());
    }

    #[test]
    fn push_upper_byte() {
        let sr: ShiftRegister = 0b0110_0111_1010_1100.into();

        assert_eq!(sr.push(0b1101_0100).value, 0b1101_0100_0110_0111);
    }

    #[test]
    fn offset_should_use_just_lower_3_bits() {
        let sr: ShiftRegister = 0b0110_0111_1010_1100.into();

        assert_eq!(0b1100_1111, sr.set_offset(9).get())
    }
}
