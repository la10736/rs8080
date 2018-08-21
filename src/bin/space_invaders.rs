#![feature(proc_macro)]
extern crate rs8080;
#[cfg(test)]
extern crate rstest;
#[macro_use]
extern crate log;
extern crate simple_logger;

use std::path::Path;
use std::fs::File;
use si_memory::ROM_SIZE;
use std::io::Read;
use si_memory::VRAM_SIZE;
use si_memory::SIMmu;
use si_io::IO;
use std::rc::Rc;
use rs8080::cpu::Cpu;

mod si_memory;
mod si_io;

fn main() {
    simple_logger::init().unwrap();

    info!("SPACE INVADERS");

    let mut rom = [0; ROM_SIZE];
    let mut vram = [0; VRAM_SIZE];
    load_rom(&mut rom, "resources");

    let mmu = SIMmu::new(rom.into(), vram.as_mut_ptr().into());

    let io = Rc::new(IO::default());

    let cpu = Cpu::new(mmu, io.clone(), io.clone());
}

fn load_rom<P: AsRef<Path>>(rom: &mut [u8], dir_name: P) {
    let dir= dir_name.as_ref();
    let files = vec!["invaders.h", "invaders.g", "invaders.f", "invaders.e"].into_iter();

    for (name, chunk) in files.zip(rom.chunks_mut(ROM_SIZE/4)) {
        let mut path = dir.to_path_buf();
        path.push(name);
        let mut file = File::open(&path).expect(&format!("Cannot find rom {:?}", path));
        file.read(chunk).expect(&format!("Cannot read {:?}", path));
    }
}
