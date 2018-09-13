extern crate rs8080;
#[macro_use]
extern crate log;
extern crate simple_logger;
#[cfg(test)]
extern crate rstest;

use std::path::Path;
use std::fs::File;
use std::rc::Rc;

use std::io::Read;
use rs8080::{
    cpu::{Cpu as Cpu8080,
          CpuError, State,
          hook::AddressCallBack},
    mmu::{Mmu, PlainMemory},
    io_bus::VoidIO,
    Address,
    Byte,
};

#[derive(Default)]
struct DoMessage {}

impl AddressCallBack for DoMessage {
    fn is_mine(&self, address: Address) -> bool {
        address == 0x0005
    }

    fn exec<M: Mmu>(&self, _address: Address, state: &State, mmu: &M) -> Result<(), CpuError> {
        const MESSAGE_CMD: Byte = 0x09;
        const NO_MESSAGE_CMD: Byte = 0x02;
        match state.c.val {
            MESSAGE_CMD => {
                let mut address = (state.d.val as Address) << 8 | (state.e.val as Address) + 3;
                let mut message = String::new();
                loop {
                    let c = mmu.read_byte(address)? as char;
                    if c == '$' {
                        break;
                    }
                    message.push(c);
                    address += 1;
                }
                println!("Message: {}", message);
            }
            NO_MESSAGE_CMD => {
                println!("No Message");
            }
            _ => {
                //No command
            }
        };
        Ok(())
    }
}

#[derive(Default)]
struct Done;

impl AddressCallBack for Done {
    fn is_mine(&self, address: Address) -> bool {
        address == 0x0000
    }
    fn exec<M: Mmu>(&self, _address: Address, _state: &State, _mmu: &M) -> Result<(), CpuError> {
        panic!("Done")
    }
}

type Cpu = Cpu8080<PlainMemory, Rc<VoidIO>, Rc<VoidIO>, (DoMessage, Done)>;

fn main() {
    simple_logger::init_with_level(log::Level::Info).unwrap();

    info!("Test Rs 8080");


    let mut mmu = PlainMemory::default();
    load_rom(&mut mmu.bank, "resources", 0x100);
    //Fix the stack pointer from 0x6ad to 0x7ad
    // this 0x06 byte 112 in the code, which is
    // byte 112 + 0x100 = 368 in memory
    mmu.write_byte(368, 0x7).unwrap();


    let io = Rc::new(VoidIO::default());

    let cpu_hook: (DoMessage, Done) = Default::default();

    let mut cpu = Cpu::new(mmu, io.clone(), io.clone(), cpu_hook);
    cpu.set_pc(0x100);

    loop {
        match cpu.run() {
            Ok(periods) => periods,
            Err(e) => critical(&cpu, e)
        };
    }
}

fn critical(cpu: &Cpu, e: CpuError) -> ! {
    dump(cpu);
    panic!(e);
}

fn dump(cpu: &Cpu) {
    println!("State: \n{}", cpu.dump_state());
    println!("{}", cpu.dump_memory());
    println!("Stack: \n{}", cpu.dump_stack());
    println!("last instructions: \n{}", cpu.dump_opcodes());
}


fn load_rom<P: AsRef<Path>>(rom: &mut [u8], dir_name: P, offset: usize) {
    let dir = dir_name.as_ref();
    let files = vec!["cpudiag.bin"].into_iter();
    let rom = &mut rom[offset..];
    let size = rom.len();
    let n = files.len();

    for (name, chunk) in files.zip(rom.chunks_mut(size / n)) {
        let mut path = dir.to_path_buf();
        path.push(name);
        let mut file = File::open(&path).expect(&format!("Cannot find rom {:?}", path));
        file.read(chunk).expect(&format!("Cannot read {:?}", path));
    }
}
