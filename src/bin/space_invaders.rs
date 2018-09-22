#![feature(fn_traits)]
extern crate rs8080;
extern crate minifb;
#[macro_use]
extern crate log;
extern crate simple_logger;
#[cfg(test)]
extern crate rstest;

use minifb::{Key, WindowOptions, Window, Scale};

use std::path::Path;
use std::fs::File;
use std::rc::Rc;
use std::io::Read;
use std::time;
use std::panic;
use std::ops::Deref;

use si_memory::{SIMmu, ROM_SIZE, VRAM_SIZE};
use si_io::IO;
use rs8080::cpu::Cpu as Cpu8080;
use rs8080::cpu::{CpuError, IrqCmd};
use rs8080::hook::NoneHook;
use gpu::Gpu;
use graphics::{WHITE, BLACK, Canvas, Rect};
use self::key::{FlipFlopKey, DirectKey, ActiveKey, WindowKey, DKey};
use std::sync::mpsc::Sender;

mod si_memory;
mod si_io;
mod graphics;
mod gpu;
mod key;


type Cpu = Cpu8080<SIMmu, Rc<IO>, Rc<IO>, NoneHook>;

const CLOCK: u64 = 2_000_000;
const CLOCKS_PER_HALF_FRAME: u64 = CLOCK / 120;
const CLOCKS_PER_FRAME: u64 = CLOCKS_PER_HALF_FRAME * 2;
const FRAMES_PER_SECONDS: u64 = 60;

const W: usize = 256;
const H: usize = 224;
const W_MARGIN: usize = 0;

const H_MARGIN: usize = 20;


enum Command {
    Dump,
    Pause,
    Continue,
    Step(usize),
    PrintFrames(bool),
    PrintStat(bool),
}

fn main() {
    simple_logger::init_with_level(log::Level::Info).unwrap();

    info!("SPACE INVADERS");

    let mut rom = [0; ROM_SIZE];
    let mut vram = [0; VRAM_SIZE];
    load_rom(&mut rom, "resources");

    let mmu = SIMmu::new(rom.into(), vram.as_mut_ptr().into());

    let io = Rc::new(IO::default());

    let mut cpu = Cpu::new(mmu, io.clone(), io.clone(), Default::default());
    let gpu = Gpu::new(W, H, vram.as_ptr());

    let w = H + 2 * H_MARGIN;
    let h = W + 2 * W_MARGIN;

    let mut window = Window::new("Space Invaders - ESC to exit",
                                 w,
                                 h,
                                 WindowOptions {
                                     borderless: true,
                                     title: false,
                                     resize: false,
                                     scale: Scale::FitScreen,
                                 }).unwrap_or_else(|e| {
        panic!("{}", e);
    });
    let mut fb = vec![0; w * h];

    let mut start = time::Instant::now();
    let mut frames: u64 = 1;
    let mut last_frame_start = frames;
    let mut clocks: u64 = 0;
    let (tx, rx) = std::sync::mpsc::channel();

    let mut pause_in_frames = None;
    let mut late = time::Duration::default();
    let mut should_print_frame = false;
    let mut should_print_late_stat = false;

    let mut keys = keys(io, tx, should_print_frame, should_print_late_stat);

    loop {
        if !window.is_open() || window.is_key_down(Key::Escape) {
            debug!("Should terminate");
            break;
        }

        if let Ok(cmd) = rx.try_recv() {
            match cmd {
                Command::Dump => dump(&cpu),
                Command::Pause => { pause_in_frames = Some(0) }
                Command::Continue => {
                    pause_in_frames = None;
                    start = time::Instant::now();
                    last_frame_start = frames;
                }
                Command::Step(n) => { pause_in_frames = Some(n) }
                Command::PrintFrames(v) => { should_print_frame = v }
                Command::PrintStat(v) => { should_print_late_stat = v }
            }
        }

        keys_apply(&mut window, &mut keys);

        if pause_in_frames != Some(0) {
            clocks = next_frame(&mut cpu, &gpu, w, h, &mut fb, frames, clocks)
                .unwrap_or_else(|e| critical(&cpu, e));

            window.update_with_buffer(&fb).unwrap();

            if should_print_frame {
                info!("Frame nr: {}", frames);
            }
            frames += 1;

            pause_in_frames = pause_in_frames.and_then(|n| if n > 0 { Some(n - 1) } else { None });
            let when = start + time::Duration::from_millis(((frames - last_frame_start) * 1000) / FRAMES_PER_SECONDS);
            let now = time::Instant::now();
            if when > now {
                let diff = when - now;
                std::thread::sleep(diff);
            } else {
                let diff = now - when;
                if should_print_late_stat {
                    info!("I'm late: {:?}", diff);
                }
                late += diff;
            }
        } else {
            std::thread::sleep(time::Duration::from_millis(100));
            window.update();
        }
    }
    if should_print_late_stat {
        let tot = time::Duration::from_millis((frames * 1000) / FRAMES_PER_SECONDS);
        info!("Total late: {:?} on {:?}", late, tot);
    }
    info!("Game Done");
}

fn next_frame(cpu: &mut Cpu, gpu: &Gpu, w: usize, h: usize, fb: &mut Vec<u32>,
              frames: u64, mut clocks: u64) -> Result<u64, CpuError> {
    let expected_clocks = frames * CLOCKS_PER_FRAME;
    let canvas = Canvas {
        width: w,
        height: h,
        fg: WHITE,
        bg: BLACK,
    };
    let (left, right) = Rect::from(&canvas).split_vertical(w / 2);

    // Up Frame
    clocks = cpu_run_till(cpu, clocks, expected_clocks)?;
    gpu.fill_canvas(fb.as_mut(), &canvas, Some(left));
    cpu.irq(IrqCmd::Irq1)?;

    // Down Frame
    clocks = cpu_run_till(cpu, clocks, expected_clocks + CLOCKS_PER_HALF_FRAME)?;
    gpu.fill_canvas(fb.as_mut(), &canvas, Some(right));
    cpu.irq(IrqCmd::Irq2)?;
    Ok(clocks)
}

fn cpu_run_till(cpu: &mut Cpu, mut clocks: u64, expected_clocks: u64) -> Result<u64, CpuError> {
    while clocks < expected_clocks {
        clocks += cpu.run()? as u64;
    }
    Ok(clocks)
}

fn keys_apply(window: &mut Window, keys: &mut Vec<Box<WindowKey>>) {
    keys.iter_mut().for_each(|b| { b.update(&window); });
}

fn keys(io: Rc<IO>, tx: Sender<Command>, should_print_frame: bool, should_print_late_stat: bool) -> Vec<Box<WindowKey>> {
    let mut buttons = si_keys(io);
    buttons.extend(ui_keys(tx, should_print_frame, should_print_late_stat).into_iter());
    buttons
}

fn ui_keys(tx: Sender<Command>, should_print_frame: bool, should_print_late_stat: bool) -> Vec<Box<dyn WindowKey>> {
    let pause = ui_key(Key::P,
                       |s| if s { Command::Pause } else { Command::Continue },
                       false, tx.clone());
    let print_frame = ui_key(Key::F,
                             |s| Command::PrintFrames(s),
                             should_print_frame, tx.clone());
    let print_stat = ui_key(Key::G,
                            |s| Command::PrintStat(s),
                            should_print_late_stat, tx.clone());
    let dump_state = ui_key(Key::D,
                            |_| Command::Dump,
                            false, tx.clone());
    let mut keys = vec![pause, print_frame, print_stat, dump_state];
    let steps_key: Vec<_> = vec![(Key::S, 1), (Key::Q, 5), (Key::W, 10), (Key::E, 20), (Key::R, 60), (Key::T, 600)];
    keys.extend(steps_key.into_iter().map(
        |(k, frames)| ui_key(k,
                             move |_| Command::Step(frames),
                             false, tx.clone())
    ));
    keys
}

fn ui_key<'a>(key: Key, action: impl Fn(bool) -> Command + 'a, state: bool, tx: Sender<Command>) -> Box<WindowKey + 'a> {
    box_key(ActiveKey::new(FlipFlopKey::new(DirectKey::from(key), state),
                           move |state| tx.send(action(state)).unwrap(),
    ))
}

fn si_keys(io: Rc<IO>) -> Vec<Box<dyn WindowKey>> {
    [
        (Key::Key5, si_io::Ev::Coin),
        (Key::Key1, si_io::Ev::P1Start),
        (Key::Left, si_io::Ev::P1Left),
        (Key::Right, si_io::Ev::P1Right),
        (Key::Space, si_io::Ev::P1Shoot),
    ].into_iter().cloned()
        .map(|(k, ev)| game_key(k, io.clone(), ev))
        .collect()
}

fn game_key<'a, I: Deref<Target=IO> + 'a>(key: Key, io: I, ev: si_io::Ev) -> Box<WindowKey + 'a> {
    box_key(DKey::new(key.into(), move |state| io.ui_event(ev, state)))
}

fn box_key<'a, K: WindowKey + 'a>(k: K) -> Box<WindowKey + 'a> {
    Box::new(k)
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
