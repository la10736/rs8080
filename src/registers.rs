use Byte;
use Address;

#[derive(Default, Debug, Clone, Copy, Eq, PartialEq)]
pub struct RegByte {
    pub val: Byte
}

#[derive(Default, Debug, Clone, Copy, Eq, PartialEq)]
pub struct RegAddress {
    pub val: Address,
}

impl RegByte {
    pub fn overflow_add(&mut self, val: Byte) -> bool {
        let (val, carry) = self.val.overflowing_add(val);
        self.val = val;
        carry
    }
    pub fn overflow_sub(&mut self, val: Byte) -> bool {
        let (val, carry) = self.val.overflowing_sub(val);
        self.val = val;
        carry
    }
    pub fn increment(&mut self) -> bool { self.overflow_add(1) }
    pub fn decrement(&mut self) -> bool { self.overflow_sub(1) }
    pub fn one_complement(&mut self) { *self = (0xff ^ self.val).into() }
    pub fn is_zero(&self) -> bool { self.val == 0 }
    pub fn sign_bit(&self) -> bool { (self.val & 0x80) != 0x00 }
    pub fn parity(&self) -> bool { (self.val.count_ones() % 2) == 0 }
    pub fn rotate_left(&mut self) -> bool {
        let carry = (self.val & 0x80) == 0x80;
        self.val = self.val.rotate_left(1);
        carry
    }
    pub fn rotate_right(&mut self) -> bool {
        let carry = (self.val & 0x01) == 0x01;
        self.val = self.val.rotate_right(1);
        carry
    }
    pub fn low(&self) -> Byte {
        self.val & 0x0f
    }
    pub fn high(&self) -> Byte {
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
    pub fn overflow_add(&mut self, val: Address) -> bool {
        let (val, carry) = self.val.overflowing_add(val);
        self.val = val;
        carry
    }
    pub fn overflow_sub(&mut self, val: Address) -> bool {
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
