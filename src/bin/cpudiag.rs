#![feature(proc_macro)]
#![feature(fn_traits)]
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
use rs8080::cpu::Cpu as Cpu8080;
use rs8080::cpu::{CpuError};
use std::time;
use rs8080::cpu::PlainMemory;
use rs8080::cpu::Mmu;
use rs8080::io_bus::VoidIO;

mod si_memory;
mod si_io;

type Cpu = Cpu8080<PlainMemory, Rc<VoidIO>, Rc<VoidIO>>;

const CLOCK: u64 = 2_000_000;

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

    let mut cpu = Cpu::new(mmu, io.clone(), io.clone());
    cpu.set_pc(0x100);

    let start = time::Instant::now();
    let mut clocks: u64 = 0;

    loop {
        clocks += match cpu.run() {
            Ok(periods) => periods,
            Err(e) => critical(&cpu, e)
        } as u64;
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
    let mut rom = &mut rom[0x100..];
    let size = rom.len();
    let n = files.len();

    for (name, chunk) in files.zip(rom.chunks_mut(size / n)) {
        let mut path = dir.to_path_buf();
        path.push(name);
        let mut file = File::open(&path).expect(&format!("Cannot find rom {:?}", path));
        file.read(chunk).expect(&format!("Cannot read {:?}", path));
    }
}
