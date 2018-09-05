use rs8080::cpu::Mmu;
use rs8080::Address;
use rs8080::Byte;
use std::ptr;

use rs8080::cpu::Result;
use std::result::Result as StdResult;
use rs8080::cpu::CpuError;
use rs8080::cpu::{str_memory, DUMP_MEMORY_COLUMNS};

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

pub const ROM_SIZE: usize = 0x2000;
pub const RAM_SIZE: usize = 0x0400;
pub const VRAM_SIZE: usize = 0x1C00;
pub const MIRROR_SIZE: usize = 0xC000;

const ROM_OFFSET: usize = 0x0000;
const RAM_OFFSET: usize = 0x2000;
const VRAM_OFFSET: usize = 0x2400;

const MIRROR_OFFSET: usize = 0x4000;

pub struct Rom {
    data: [Byte; ROM_SIZE],
    black_hole: Byte,
}

impl From<[Byte; ROM_SIZE]> for Rom {
    fn from(data: [u8; ROM_SIZE]) -> Self {
        Rom {
            data,
            black_hole: 0,
        }
    }
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
    fn read_byte(&self, address: Address) -> Result<Byte> {
        Ok(self.data[self.address(address)])
    }

    fn write_byte(&mut self, address: Address, val: Byte) -> Result<()> {
        error!("Try to write in rom [{:04x}]={:02x}", address, val);
        CpuError::memory_write(address, val)
    }

    fn dump(&self) -> String {
        str_memory(&self.data, self.offset(), DUMP_MEMORY_COLUMNS)
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
    fn read_byte(&self, address: Address) -> Result<Byte> {
        Ok(self.data[self.address(address)])
    }

    fn write_byte(&mut self, address: Address, val: Byte) -> Result<()> {
        let address = self.address(address);
        self.data[address] = val;
        Ok(())
    }

    fn dump(&self) -> String {
        str_memory(&self.data, self.offset(), DUMP_MEMORY_COLUMNS)
    }
}

pub struct VRam {
    ptr: *mut Byte,
}

impl From<*mut Byte> for VRam {
    fn from(ptr: *mut Byte) -> Self {
        VRam { ptr }
    }
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
    fn read_byte(&self, address: Address) -> Result<Byte> {
        let addr = self.address(address);
        let val = unsafe {
            *self.ptr.offset(addr as isize)
        };
        debug!("Read Vram [0x{:04x}]=0x{:02x}", address, val);
        Ok(val)
//        error!("Try to read in vram 0x{:04x}", address);
//        CpuError::memory_read(address)
    }

    fn write_byte(&mut self, address: Address, val: Byte) -> Result<()> {
        debug!("Write Vram [0x{:04x}]=0x{:02x}", address, val);
        let address = self.address(address);
        unsafe { *self.ptr.offset(address as isize) = val }
        Ok(())
    }

    fn dump(&self) -> String {
        format!("No Data!")
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
    fn read_byte(&self, address: Address) -> Result<Byte> {
//        CpuError::memory_read(address)
        error!("Try to read in mirror 0x{:04x}", address);
        Ok(0x00)
    }

    fn write_byte(&mut self, address: Address, val: Byte) -> Result<()> {
        error!("Try to write in mirror 0x{:04x}", address);
        //CpuError::memory_write(address, val)
        Ok(())
    }

    fn dump(&self) -> String {
        format!("Just mirror bank!")
    }
}

#[derive(Default)]
pub struct SIMmu {
    rom: Rom,
    ram: Ram,
    vram: VRam,
    mirror: Mirror,
}

impl SIMmu {
    pub fn new(rom: Rom, vram: VRam) -> SIMmu {
        SIMmu {
            rom,
            vram,
            ..Default::default()
        }
    }
}

impl Mmu for SIMmu {
    fn read_byte(&self, address: Address) -> Result<Byte> {
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

    fn write_byte(&mut self, address: Address, val: Byte) -> Result<()> {
        if self.rom.contains(address) {
            self.rom.write_byte(address, val)
        } else if self.ram.contains(address) {
            self.ram.write_byte(address, val)
        } else if self.vram.contains(address) {
            self.vram.write_byte(address, val)
        } else if self.mirror.contains(address) {
            self.mirror.write_byte(address, val)
        } else {
            unreachable!()
        }
    }

    fn dump(&self) -> String {
        format!(r#"Rom:
{}
Ram:
{}
VRam:
{}
Mirror:
{}"#, self.rom.dump(), self.ram.dump(), self.vram.dump(), self.mirror.dump() )
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

            assert_eq!(Ok(value), zmem.read_byte(address))
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
        fn write_byte_error(mut zmem: SIMmu, address: Address, value: Byte) {
            assert!(zmem.write_byte(address, value).is_err());
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

            assert_eq!(Ok(value), zmem.read_byte(address))
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

            assert_eq!(Ok(value), mem.read_byte(address))
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
        fn write_error(mut zmem: SIMmu, address: Address, value: Byte) {
            assert!(zmem.write_byte(address, value).is_err());
        }
    }
}
