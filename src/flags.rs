use std::fmt::{self, Debug, Formatter};
use Byte;


#[derive(Copy, Clone)]
pub enum Flag {
    Sign,
    Zero,
    AuxCarry,
    Parity,
    Carry,
}

#[derive(Default, Clone, Eq, PartialEq)]
pub struct Flags(Byte);

impl Flags {
    pub fn get(&self, f: Flag) -> bool {
        (self.0 & Self::mask(f)) != 0
    }

    pub fn mask(f: Flag) -> Byte {
        0x1 << Self::flag_bit(f)
    }

    pub fn flag_bit(f: Flag) -> u8 {
        use self::Flag::*;
        match f {
            Sign => 7,
            Zero => 6,
            AuxCarry => 4,
            Parity => 2,
            Carry => 0,
        }
    }

    pub fn val(&mut self, f: Flag, val: bool) {
        if val {
            self.0 |= Self::mask(f)
        } else {
            self.0 &= !(Self::mask(f))
        }
    }

    pub fn set(&mut self, f: Flag) {
        self.val(f, true)
    }

    pub fn clear(&mut self, f: Flag) {
        self.val(f, false)
    }

    pub fn toggle(&mut self, f: Flag) {
        let toggled = !self.get(f);
        self.val(f, toggled)
    }

    pub fn clear_all(&mut self) {
        self.0 = 0x0
    }
}

impl Debug for Flags {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "0x{:02x}", self.0)
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
