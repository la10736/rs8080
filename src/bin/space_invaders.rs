#![feature(proc_macro)]
extern crate rs8080;
extern crate minifb;
#[macro_use]
extern crate log;
extern crate simple_logger;
#[cfg(test)]
extern crate rstest;

use minifb::{Key, WindowOptions, Window};

use std::path::Path;
use std::fs::File;
use std::rc::Rc;

use std::io::Read;
use si_memory::{SIMmu, ROM_SIZE, VRAM_SIZE};
use si_io::IO;
use rs8080::cpu::{Cpu, IrqCmd};
use gpu::Gpu;
use std::time;
use graphics::{WHITE, BLACK, Canvas, Rect};
use std::panic;

mod si_memory;
mod si_io;

fn main() {
    simple_logger::init_with_level(log::Level::Warn).unwrap();

    info!("SPACE INVADERS");

    let mut rom = [0; ROM_SIZE];
    let mut vram = [0; VRAM_SIZE];
    load_rom(&mut rom, "resources");

    let mmu = SIMmu::new(rom.into(), vram.as_mut_ptr().into());

    let io = Rc::new(IO::default());

    let mut cpu = Cpu::new(mmu, io.clone(), io.clone());
    let gpu = Gpu::new(W, H, vram.as_ptr());

    let w = W + 2 * W_MARGIN;
    let h = H + 2 * H_MARGIN;

    let mut window = Window::new("Space Invaders - ESC to exit",
                                 w,
                                 h,
                                 WindowOptions::default()).unwrap_or_else(|e| {
        panic!("{}", e);
    });
    let mut fb = vec![0; w * h];
    let canvas = Canvas {
        width: w,
        height: h,
        fg: WHITE,
        bg: BLACK,
    };
    let (up, down) = Rect::from(&canvas).split_horizontal(h/2);

    let start = time::Instant::now();
    let mut last_frame = start;
    let mut frames: u64 = 1;
    let mut upper = true;
    let mut clocks: u64 = 0;
    const CLOCK: u64 = 2_000_000;
    const CLOCKS_PER_HALF_FRAME: u64 = CLOCK / 120;
    const CLOCKS_PER_FRAME: u64 = CLOCKS_PER_HALF_FRAME * 2;

    loop {
        if !window.is_open() || window.is_key_down(Key::Escape) {
            debug!("Should terminate");
            break;
        }


        let expected_clocks = (frames * CLOCKS_PER_FRAME) +
            if upper {
                0
            } else {
                CLOCKS_PER_HALF_FRAME
            };

        while clocks < expected_clocks {
            match cpu.run() {
                Ok(c) => {clocks += c as u64;},
                Err(e) => {
                    eprintln!("State: \n{}", cpu.dump_state());
                    eprintln!("{}", cpu.dump_memory());
                    eprintln!("Stack: \n{}", cpu.dump_stack());
                    eprintln!("last instructions: \n{}", cpu.dump_opcodes());
                    panic!("Received panic: {:?}", e);
                }
            }
        }
        // 1. Update Frame buffer
        // 2. Fire IRQ
        if upper {
            gpu.fill_canvas(fb.as_mut(), &canvas, Some(up));
            cpu.irq(IrqCmd::Irq1);
        } else {
            gpu.fill_canvas(fb.as_mut(), &canvas, Some(down));
            cpu.irq(IrqCmd::Irq2);
            window.update_with_buffer(&fb);
            frames += 1;
        };

        upper = !upper;
    }

    info!("Game Done");
}
mod graphics;

mod gpu;
const W: usize = 256;
const H: usize = 224;
const W_MARGIN: usize = 20;

const H_MARGIN: usize = 20;

fn load_rom<P: AsRef<Path>>(rom: &mut [u8], dir_name: P) {
    let dir = dir_name.as_ref();
    let files = vec!["invaders.h", "invaders.g", "invaders.f", "invaders.e"].into_iter();

    for (name, chunk) in files.zip(rom.chunks_mut(ROM_SIZE / 4)) {
        let mut path = dir.to_path_buf();
        path.push(name);
        let mut file = File::open(&path).expect(&format!("Cannot find rom {:?}", path));
        file.read(chunk).expect(&format!("Cannot read {:?}", path));
    }
}
