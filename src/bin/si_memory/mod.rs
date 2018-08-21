use rs8080::cpu::Mmu;
use rs8080::Address;
use rs8080::Byte;
use std::ptr;

trait MBank: Mmu {
    fn offset(&self) -> usize;
    fn size(&self) -> usize;
    fn contains(&self, address: Address) -> bool {
        let address = address as usize;
        address >= self.offset() && address < (self.offset() + self.size())
    }
    fn address(&self, address: Address) -> usize {
        address as usize - self.offset()
    }
}

const ROM_OFFSET: usize = 0x0000;
const ROM_SIZE: usize = 0x2000;

const RAM_OFFSET: usize = 0x2000;
const RAM_SIZE: usize = 0x0400;

const VRAM_OFFSET: usize = 0x2400;
const VRAM_SIZE: usize = 0x1C00;

const MIRROR_OFFSET: usize = 0x4000;
const MIRROR_SIZE: usize = 0xC000;

struct Rom {
    data: [Byte; ROM_SIZE],
    black_hole: Byte,
}

impl Default for Rom {
    fn default() -> Self {
        Rom { data: [0; ROM_SIZE], black_hole: 0x00 }
    }
}

impl MBank for Rom {
    fn offset(&self) -> usize {
        ROM_OFFSET
    }

    fn size(&self) -> usize {
        self.data.len()
    }
}

impl Mmu for Rom {
    fn read_byte(&self, address: Address) -> Byte {
        self.data[self.address(address)]
    }

    fn write_byte(&mut self, address: Address, val: Byte) {
        warn!("Try to write in rom [{:04x}]={:02x}", address, val);
    }

    fn ref_mut(&mut self, address: Address) -> &mut Byte {
        warn!("Try to get a ref_mut in rom at {:04x}", address);
        &mut self.black_hole
    }
}

struct Ram {
    data: [Byte; RAM_SIZE],
}

impl Default for Ram {
    fn default() -> Self {
        Ram { data: [0; RAM_SIZE] }
    }
}

impl MBank for Ram {
    fn offset(&self) -> usize {
        RAM_OFFSET
    }

    fn size(&self) -> usize {
        self.data.len()
    }
}

impl Mmu for Ram {
    fn read_byte(&self, address: Address) -> Byte {
        self.data[self.address(address)]
    }

    fn write_byte(&mut self, address: Address, val: Byte) {
        let address = self.address(address);
        self.data[address] = val;
    }

    fn ref_mut(&mut self, address: Address) -> &mut Byte {
        &mut self.data[self.address(address)]
    }
}

struct VRam {
    ptr: *mut Byte,
}

impl VRam {
    pub fn new(ptr: *mut Byte) -> Self {
        VRam { ptr }
    }
}

impl Default for VRam {
    fn default() -> Self {
        VRam { ptr: ptr::null_mut() }
    }
}

impl MBank for VRam {
    fn offset(&self) -> usize {
        VRAM_OFFSET
    }

    fn size(&self) -> usize {
        VRAM_SIZE
    }
}

impl Mmu for VRam {
    fn read_byte(&self, address: Address) -> Byte {
        unsafe { *self.ptr.offset(address as isize) }
    }

    fn write_byte(&mut self, address: Address, val: Byte) {
        unsafe { *self.ptr.offset(address as isize) = val }
    }

    fn ref_mut(&mut self, address: u16) -> &mut u8 {
        unsafe { &mut *self.ptr.offset(address as isize) }
    }
}

#[derive(Default)]
struct Mirror { black_hole: Byte }

impl MBank for Mirror {
    fn offset(&self) -> usize {
        MIRROR_OFFSET
    }

    fn size(&self) -> usize {
        MIRROR_SIZE
    }
}

const MIRROR_DEFAULT: Byte = 0xDE;

impl Mmu for Mirror {
    fn read_byte(&self, address: Address) -> Byte {
        warn!("Read at mirror address {:04x}", address);
        MIRROR_DEFAULT
    }

    fn write_byte(&mut self, address: Address, val: Byte) {
        warn!("Write at mirror [{:04x}]= {:02x}", address, val);
    }

    fn ref_mut(&mut self, address: Address) -> &mut u8 {
        warn!("Try to get a ref_mut in mirror at {:04x}", address);
        &mut self.black_hole
    }
}

#[derive(Default)]
struct SIMmu {
    rom: Rom,
    ram: Ram,
    vram: VRam,
    mirror: Mirror,
}

impl Mmu for SIMmu {
    fn read_byte(&self, address: Address) -> Byte {
        if self.rom.contains(address) {
            self.rom.read_byte(address)
        } else if self.ram.contains(address) {
            self.ram.read_byte(address)
        } else if self.vram.contains(address) {
            self.vram.read_byte(address)
        } else if self.mirror.contains(address) {
            self.mirror.read_byte(address)
        } else {
            unreachable!()
        }
    }

    fn write_byte(&mut self, address: Address, val: Byte) {
        if self.rom.contains(address) {
            self.rom.write_byte(address, val);
        } else if self.ram.contains(address) {
            self.ram.write_byte(address, val);
        } else if self.vram.contains(address) {
            self.vram.write_byte(address, val);
        } else if self.mirror.contains(address) {
            self.mirror.write_byte(address, val);
        } else {
            unreachable!()
        }
    }

    fn ref_mut(&mut self, address: u16) -> &mut u8 {
        if self.rom.contains(address) {
            self.rom.ref_mut(address)
        } else if self.ram.contains(address) {
            self.ram.ref_mut(address)
        } else if self.vram.contains(address) {
            self.vram.ref_mut(address)
        } else if self.mirror.contains(address) {
            self.mirror.ref_mut(address)
        } else { unreachable!() }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use rstest::{rstest, rstest_parametrize};
    use rs8080::Address;
    use rs8080::Byte;

    fn zmem() -> SIMmu {
        SIMmu::default()
    }

    mod ram {
        use super::*;

        #[rstest_parametrize(
        address, value,
        case(0x2000, 0xE1),
        case(0x2020, 0xA5),
        case(0x23FF, 0x1A),
        )]
        fn write_and_read(mut zmem: SIMmu, address: Address, value: Byte) {
            zmem.write_byte(address, value);

            assert_eq!(value, zmem.read_byte(address))
        }

        #[rstest]
        fn ref_mut(mut zmem: SIMmu) {
            let address = (RAM_OFFSET + 0x0032) as Address;
            let val = 0x42;

            {
                let r = zmem.ref_mut(address);

                *r = val;
            }
            assert_eq!(val, zmem.read_byte(address));
        }
    }

    mod rom {
        use super::*;

        #[rstest_parametrize(
        address, value,
        case(0x0000, 0xA2),
        case(0x1203, 0xE1),
        case(0x1FFF, 0x01),
        )]
        fn write_byte_should_be_ignored(mut zmem: SIMmu, address: Address, value: Byte) {
            zmem.write_byte(address, value);

            assert_eq!(0x00, zmem.read_byte(address))
        }

        #[rstest_parametrize(
        address, value,
        case(0x0000, 0xA2),
        case(0x1203, 0xE1),
        case(0x1FFF, 0x01),
        )]
        fn read_byte(mut zmem: SIMmu, address: Address, value: Byte) {
            let raw_address = zmem.rom.address(address);
            zmem.rom.data[raw_address] = value;

            assert_eq!(value, zmem.read_byte(address))
        }

        #[rstest]
        fn ref_mut_should_be_ignored(mut zmem: SIMmu) {
            let address = (ROM_OFFSET + 0x1278) as Address;
            let val = 0x32;

            {
                let r = zmem.ref_mut(address);

                *r = val;
            }
            assert_eq!(0x00, zmem.read_byte(address));
        }
    }

    mod vram {
        use super::*;

        type Context = (SIMmu, [Byte; VRAM_SIZE]);

        fn mem() -> Context {
            let mut vram = [0; VRAM_SIZE];
            (SIMmu { vram: VRam::new(vram.as_mut_ptr()), ..Default::default() }, vram)
        }

        #[rstest_parametrize(
        address, value,
        case(0x2400, 0xE1),
        case(0x2420, 0xA5),
        case(0x2FFF, 0x1A),
        )]
        fn write_and_read(mem: Context, address: Address, value: Byte) {
            let (mut mem, _) = mem;
            mem.write_byte(address, value);

            assert_eq!(value, mem.read_byte(address))
        }

        #[rstest]
        fn ref_mut(mem: Context) {
            let (mut mem, _) = mem;
            let address = (VRAM_OFFSET + 0x10A2) as Address;
            let val = 0xF2;

            {
                let r = mem.ref_mut(address);

                *r = val;
            }
            assert_eq!(val, mem.read_byte(address));
        }
    }

    mod mirror {
        use super::*;

        #[rstest_parametrize(
        address, value,
        case(0x4000, 0xE1),
        case(0x5420, 0xA5),
        case(0xFFFF, 0x1A),
        )]
        fn write_ignored(mut zmem: SIMmu, address: Address, value: Byte) {
            zmem.write_byte(address, value);

            assert_eq!(MIRROR_DEFAULT, zmem.read_byte(address));
        }

        #[rstest]
        fn ref_mut_should_be_ignored(mut zmem: SIMmu) {
            let address = (MIRROR_OFFSET + 0x1234) as Address;
            let val = 0x32;

            {
                let r = zmem.ref_mut(address);

                *r = val;
            }
            assert_eq!(MIRROR_DEFAULT, zmem.read_byte(address));
        }
    }
}
