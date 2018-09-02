#![feature(proc_macro)]
#![feature(fn_traits)]
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
use std::ops::Deref;
use std::cell::RefCell;

mod si_memory;
mod si_io;

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
}

struct DirectKey {
    key: Key,
    last: KeyState,
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
            self.action.call((self.active(), ));
        }
        changed
    }

    fn active(&self) -> bool {
        self.key.active()
    }
}

fn game_key<'a, I: Deref<Target=IO> + 'a>(key: Key, io: I, ev: si_io::Ev) -> Box<WindowKey + 'a> {
    Box::new(DKey {
        key: key.into(),
        action: move |state| io.ui_event(ev, state),
    })
}


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
    let (up, down) = Rect::from(&canvas).split_horizontal(h / 2);

    let start = time::Instant::now();
    let mut last_frame = start;
    let mut frames: u64 = 1;
    let mut upper = true;
    let mut clocks: u64 = 0;
    const CLOCK: u64 = 2_000_000;
    const CLOCKS_PER_HALF_FRAME: u64 = CLOCK / 120;
    const CLOCKS_PER_FRAME: u64 = CLOCKS_PER_HALF_FRAME * 2;

    let paused = RefCell::new(false);
    let mut pause = Box::new(ActiveKey {
        key: FlipFlopKey::from(DirectKey::from(Key::P)),
        action: |state| {*paused.borrow_mut() = state;},
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

    loop {
        if !window.is_open() || window.is_key_down(Key::Escape) {
            debug!("Should terminate");
            break;
        }

        buttons.iter_mut().for_each(|b| { b.update(&window); });

        if !*paused.borrow() {
            let expected_clocks = (frames * CLOCKS_PER_FRAME) +
                if upper {
                    0
                } else {
                    CLOCKS_PER_HALF_FRAME
                };

            while clocks < expected_clocks {
                match cpu.run() {
                    Ok(c) => { clocks += c as u64; }
                    Err(e) => {
                        dump(&cpu);
                        panic!("Received panic: {:?}", e);
                    }
                }
            }
            let limit = 25465654;

            if clocks > limit - CLOCKS_PER_FRAME * 30 {
//                println!("CLOCK = {}", clocks);
            }

//        if clocks >= limit + CLOCKS_PER_FRAME * 3 {
//            dump(cpu);
//            loop {
//                std::thread::sleep(time::Duration::from_millis(1000));
//            }
//        }
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
        } else {
            std::thread::sleep(time::Duration::from_millis(100));
            window.update();
        }
    }

    info!("Game Done");
}

fn dump(cpu: &Cpu<SIMmu, Rc<IO>, Rc<IO>>) {
    println!("State: \n{}", cpu.dump_state());
    println!("{}", cpu.dump_memory());
    println!("Stack: \n{}", cpu.dump_stack());
    println!("last instructions: \n{}", cpu.dump_opcodes());
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
