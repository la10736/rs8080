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

mod si_memory;
mod si_io;
pub mod graphics;
mod gpu;


type Cpu = Cpu8080<SIMmu, Rc<IO>, Rc<IO>, NoneHook>;


#[derive(Default)]
struct FlipFlop {
    pub state: bool
}

impl FlipFlop {
    pub fn change(&mut self) {
        self.state = !self.state;
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
enum KeyState {
    Pressed,
    Released,
}

impl From<bool> for KeyState {
    fn from(state: bool) -> Self {
        match state {
            true => KeyState::Pressed,
            false => KeyState::Released
        }
    }
}

impl Into<bool> for KeyState {
    fn into(self) -> bool {
        match self {
            KeyState::Pressed => true,
            KeyState::Released => false,
        }
    }
}

trait WindowKey {
    fn update(&mut self, window: &Window) -> bool;
    fn active(&self) -> bool;
    fn change_state(&mut self, val: bool) -> bool;
}

struct DirectKey {
    key: Key,
    last: KeyState,
}

const CLOCK: u64 = 2_000_000;
const CLOCKS_PER_HALF_FRAME: u64 = CLOCK / 120;
const CLOCKS_PER_FRAME: u64 = CLOCKS_PER_HALF_FRAME * 2;
const FRAMES_PER_SECONDS: u64 = 60;

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

    let pause = Box::new(ActiveKey {
        key: FlipFlopKey::from(DirectKey::from(Key::P)),
        action: {
            let tx = tx.clone();
            move |state| tx.send(if state { Command::Pause } else { Command::Continue }).unwrap()
        },
    });

    let steps: Vec<_> = vec![(Key::S, 1), (Key::Q, 5), (Key::W, 10), (Key::E, 20), (Key::R, 60), (Key::T, 600)]
        .into_iter().map(|(key, s)|
        Box::new(ActiveKey {
            key: FlipFlopKey::from(DirectKey::from(key)),
            action: {
                let tx = tx.clone();
                move |_| { tx.send(Command::Step(s)).unwrap(); }
            },
        }) as Box<WindowKey>
    ).collect();
    let mut late = time::Duration::default();
    let mut should_print_frame = false;
    let mut should_print_late_stat = false;
    let mut print_frame = Box::new(ActiveKey {
        key: FlipFlopKey::from(DirectKey::from(Key::F)),
        action: {
            let tx = tx.clone();
            move |state| { tx.send(Command::PrintFrames(state)).unwrap(); }
        },
    });
    print_frame.change_state(should_print_frame);
    let mut print_stat = Box::new(ActiveKey {
        key: FlipFlopKey::from(DirectKey::from(Key::G)),
        action: {
            let tx = tx.clone();
            move |state| { tx.send(Command::PrintStat(state)).unwrap(); }
        },
    });
    print_stat.change_state(should_print_late_stat);

    let dump_state = Box::new(ActiveKey {
        key: FlipFlopKey::from(DirectKey::from(Key::D)),
        action: {
            let tx = tx.clone();
            move |_| { tx.send(Command::Dump).unwrap(); }
        },
    });

    let game_buttons = [
        (Key::Key5, si_io::Ev::Coin),
        (Key::Key1, si_io::Ev::P1Start),
        (Key::Left, si_io::Ev::P1Left),
        (Key::Right, si_io::Ev::P1Right),
        (Key::Space, si_io::Ev::P1Shoot),
    ];

    let mut buttons: Vec<_> = game_buttons.into_iter().cloned()
        .map(|(k, ev)| game_key(k, io.clone(), ev))
        .collect();
    buttons.push(pause);
    buttons.push(print_frame);
    buttons.push(print_stat);
    buttons.extend(steps.into_iter());
    buttons.push(dump_state);

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

        buttons.iter_mut().for_each(|b| { b.update(&window); });

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

impl From<Key> for DirectKey {
    fn from(key: Key) -> Self {
        DirectKey {
            key,
            last: KeyState::Released,
        }
    }
}

impl WindowKey for DirectKey {
    fn update(&mut self, window: &Window) -> bool {
        let prev = self.last;
        self.last = window.is_key_down(self.key).into();
        prev != self.last
    }

    fn active(&self) -> bool {
        self.last.into()
    }

    fn change_state(&mut self, val: bool) -> bool {
        let old = self.last;
        self.last = val.into();
        old != self.last
    }
}

struct FlipFlopKey<K: WindowKey> {
    key: K,
    flip_flop: FlipFlop,
}

impl<K: WindowKey> From<K> for FlipFlopKey<K> {
    fn from(key: K) -> Self {
        FlipFlopKey {
            key,
            flip_flop: Default::default(),
        }
    }
}

impl<K: WindowKey> WindowKey for FlipFlopKey<K> {
    fn update(&mut self, window: &Window) -> bool {
        let changed = self.key.update(window);

        if changed && !self.key.active() {
            self.flip_flop.change();
            true
        } else {
            false
        }
    }

    fn active(&self) -> bool {
        self.flip_flop.state
    }

    fn change_state(&mut self, val: bool) -> bool {
        if val != self.flip_flop.state {
            self.flip_flop.change();
            true
        } else {
            false
        }
    }
}

struct ActiveKey<K: WindowKey, F: Fn(bool)> {
    key: K,
    action: F,
}

type DKey<F> = ActiveKey<DirectKey, F>;

impl<K: WindowKey, F: Fn(bool)> WindowKey for ActiveKey<K, F> {
    fn update(&mut self, window: &Window) -> bool {
        let changed = self.key.update(window);
        if changed {
            self.call_action();
        }
        changed
    }

    fn active(&self) -> bool {
        self.key.active()
    }

    fn change_state(&mut self, val: bool) -> bool {
        let changed = self.key.change_state(val);
        if changed {
            self.call_action();
        }
        changed
    }
}

impl<K: WindowKey, F: Fn(bool)> ActiveKey<K, F> {
    fn call_action(&self) {
        self.action.call((self.active(), ));
    }
}

fn game_key<'a, I: Deref<Target=IO> + 'a>(key: Key, io: I, ev: si_io::Ev) -> Box<WindowKey + 'a> {
    Box::new(DKey {
        key: key.into(),
        action: move |state| io.ui_event(ev, state),
    })
}

fn cpu_run_till(cpu: &mut Cpu, mut clocks: u64, expected_clocks: u64) -> Result<u64, CpuError> {
    while clocks < expected_clocks {
        clocks += cpu.run()? as u64;
    }
    Ok(clocks)
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

const W: usize = 256;
const H: usize = 224;
const W_MARGIN: usize = 0;

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
