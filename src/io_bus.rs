use std::ops::Deref;

use Byte;

pub trait OutputBus {
    fn send(&self, id: Byte, data: Byte);
}

pub trait InputBus {
    fn read(&self, id: Byte) -> Byte;
}

#[derive(Default, Clone, Copy)]
pub struct VoidIO;

impl OutputBus for VoidIO { fn send(&self, _id: u8, _data: u8) {} }

impl InputBus for VoidIO { fn read(&self, _id: u8) -> u8 { 0x00 } }

impl<T: InputBus, O: Deref<Target=T>> InputBus for O {
    fn read(&self, id: Byte) -> Byte {
        self.deref().read(id)
    }
}

impl<T: OutputBus, O: Deref<Target=T>> OutputBus for O {
    fn send(&self, id: Byte, data: Byte) {
        self.deref().send(id, data)
    }
}

#[cfg(test)]
pub mod test {
    use super::*;
    use std::cell::RefCell;

    pub struct Loopback {
        data: RefCell<[Option<u8>; 256]>
    }

    impl Default for Loopback {
        fn default() -> Self {
            Loopback { data: RefCell::new([None; 256]) }
        }
    }

    impl OutputBus for Loopback {
        fn send(&self, id: u8, data: u8) {
            self.data.borrow_mut()[id as usize] = Some(data)
        }
    }

    impl InputBus for Loopback {
        fn read(&self, id: u8) -> u8 {
            self.data.borrow()[id as usize].unwrap()
        }
    }
}
