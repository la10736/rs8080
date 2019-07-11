#![feature(fn_traits)]
extern crate rs8080;
#[cfg(test)]
extern crate rstest;
extern crate simple_logger;
extern crate structopt;
#[macro_use]
extern crate log;
extern crate sdl2;

use std::{
    fs::File,
    io::Read,
    ops::Deref,
    path::Path,
    rc::Rc,
    sync::mpsc::Sender,
    time,
};
use structopt::StructOpt;

use rs8080::{
    cpu::{
        Cpu as Cpu8080,
        CpuError, IrqCmd,
    },
    hook::NoneHook
};

mod key;
mod glutils;

mod si;
pub mod graphics;

use si::{
    io::{self, IO},
    memory::{ROM_SIZE, SIMmu, VRAM_SIZE},
    gpu::Gpu
};
use graphics::{BLACK, Canvas, Rect, WHITE};

type Cpu = Cpu8080<SIMmu, Rc<IO>, Rc<IO>, NoneHook>;

const CLOCK: u64 = 2_000_000;
const CLOCKS_PER_HALF_FRAME: u64 = CLOCK / 120;
const CLOCKS_PER_FRAME: u64 = CLOCKS_PER_HALF_FRAME * 2;
const FRAMES_PER_SECONDS: u64 = 60;

const W: usize = 256;
const H: usize = 224;
const W_MARGIN: usize = 0;

const H_MARGIN: usize = 20;

#[derive(StructOpt, Debug)]
#[structopt(name = "space_invaders")]
struct SiOpt {
    /// Lives
    #[structopt(short = "l", long = "lives", default_value="3")]
    lives: u8,

    /// Activate coin info
    #[structopt(short = "c", long = "coin-info")]
    coin_info: bool,

    /// Bonus life at 1500
    #[structopt(short = "b", long = "bonus-1500")]
    bonus_life_1500: bool,

    /// Play just some frames and the pause it
    #[structopt(short = "p", long = "pause")]
    pause_in_frames: Option<usize>,

    /// Dump frames number
    #[structopt(short = "f", long = "frame-number")]
    print_frame_number: bool,

    /// Dump stats info
    #[structopt(short = "S", long = "stats")]
    dump_stats: bool,

    /// Run at full speed
    #[structopt(short = "F", long = "fast")]
    fast: bool,

    // The number of occurences of the `v/verbose` flag
    /// Verbose mode (-v, -vv, -vvv, etc.)
    #[structopt(short = "v", long = "verbose", parse(from_occurrences))]
    verbose: u8,
}

pub struct VideoConfig {
    pub window_title: String,
    pub width: isize,
    pub height: isize,
    pub fps: isize,
}

enum Command {
    Quit,
    Dump,
    Pause,
    Continue,
    Step(usize),
    PrintFrames(bool),
    PrintStat(bool),
    Fast(bool),
}


pub struct Output {
    vcfg: Rc<VideoConfig>,
    context: sdl2::Sdl,
    video: Option<Video>,
    framecount: i64,
}

impl Output {
    pub fn new(vcfg: VideoConfig) -> Result<Output, String> {
        Ok(Output {
            vcfg: Rc::new(vcfg),
            context: sdl2::init()?,
            video: None,
            framecount: 0,
        })
    }

    pub fn enable_video(&mut self) -> Result<(), String> {
        self.video = Some(Video::new(self.vcfg.clone(), &self.context)?);
        Ok(())
    }
}

use sdl2::video::{GLContext, GLProfile, Window};
use sdl2::VideoSubsystem;
use std::time::{Duration, Instant};
use glutils::{SurfaceRenderer, GfxBuffer, OwnedGfxBuffer};
use sdl2::event::Event;
use sdl2::keyboard::Keycode;
use key::{ActiveKey, FlipFlopKey, DirectKey, WindowKey, DKey};

struct Video {
    video: VideoSubsystem,
    window: Window,
    renderer: SurfaceRenderer,
    _gl_context: GLContext,

    cfg: Rc<VideoConfig>,
    fps_clock: Instant,
    fps_counter: isize,
}

impl Video {
    fn new(cfg: Rc<VideoConfig>, context: &sdl2::Sdl) -> Result<Video, String> {
        let video = context
            .video()
            .or_else(|e| Err(format!("error creating video subsystem: {:?}", e)))?;

        // Request OpenGL Core profile (for GL 3.2 extensions, required by imgui-opengl-renderer).
        {
            let gl_attr = video.gl_attr();
            gl_attr.set_context_profile(GLProfile::Core);
            gl_attr.set_context_version(3, 0);
        }

        let window = video
            .window(&cfg.window_title, 640 * 2, 480 * 2)
            .resizable()
            .position_centered()
            .opengl()
            .allow_highdpi()
            .build()
            .or_else(|e| Err(format!("error creating window: {:?}", e)))?;

        let gl_context = window
            .gl_create_context()
            .expect("couldn't create GL context");

        let video2 = video.clone();
        let renderer = SurfaceRenderer::new(move |s| video2.gl_get_proc_address(s) as _);

        Ok(Video {
            cfg,
            video,
            window,
            renderer,
            _gl_context: gl_context,
            fps_clock: Instant::now(),
            fps_counter: 0,
        })
    }

    fn render_frame(&mut self, frame: &GfxBuffer) {
        self.renderer.render(frame);
    }

    fn update_fps(&mut self) {
        self.fps_counter += 1;
        if self.fps_clock.elapsed() >= Duration::new(1, 0) {
            self.window
                .set_title(&format!(
                    "{} - {} FPS",
                    &self.cfg.window_title, self.fps_counter
                ))
                .unwrap();
            self.fps_counter = 0;
            self.fps_clock += Duration::new(1, 0);
        }
    }
}


fn main() {
    let opt = SiOpt::from_args();
    let level = match opt.verbose {
        0 => log::Level::Info,
        _ => log::Level::Debug
    };

    simple_logger::init_with_level(level).unwrap();

    info!("SPACE INVADERS");


    let mut rom = [0; ROM_SIZE];
    let mut vram = [0; VRAM_SIZE];
    load_rom(&mut rom, "space_invaders/resources");

    let mmu = SIMmu::new(rom.into(), vram.as_mut_ptr().into());

    let si_io = IO::default()
        .change_lives(opt.lives)
        .coin_info_set(opt.coin_info)
        .lower_bonus_life(!opt.bonus_life_1500);

    let io = Rc::new(si_io);

    let mut cpu = Cpu::new(mmu, io.clone(), io.clone(), Default::default());
    let gpu = Gpu::new(W, H, vram.as_ptr());

    let w = H + 2 * H_MARGIN;
    let h = W + 2 * W_MARGIN;

    let mut out = Output::new(
        VideoConfig {
            window_title: "Space Invaders Emulator".into(),
            width: w as isize,
            height: h as isize,
            fps: 60,
        },
    ).expect("Cannot create output window");
    out.enable_video().expect("Cannot enable video");
    let mut screen = OwnedGfxBuffer::new(w, h);

    let mut fb = vec![0; w * h];

    let mut start = time::Instant::now();
    let mut frames: u64 = 1;
    let mut last_frame_start = frames;
    let mut clocks: u64 = 0;
    let (tx, rx) = std::sync::mpsc::channel();
    let mut event_pump = out.context.event_pump().unwrap();

    let mut pause_in_frames = opt.pause_in_frames;
    let mut late = time::Duration::default();
    let mut should_print_frame = opt.print_frame_number;
    let mut should_dump_stat = opt.dump_stats;
    let mut fast = opt.fast;

    let mut keys = keys(io, tx.clone(), fast, should_print_frame, should_dump_stat);

    'main: loop {
        event_pump.poll_iter()
            .for_each(
                |e| match e {
                    Event::Quit{..} => {tx.send(Command::Quit).unwrap();},
                    _ => keys_apply(&e, &mut keys)

                }
            );

        while let Ok(cmd) = rx.try_recv() {
            match cmd {
                Command::Quit => break 'main,
                Command::Dump => dump(&cpu),
                Command::Pause => { pause_in_frames = Some(0) }
                Command::Continue => {
                    pause_in_frames = None;
                    start = time::Instant::now();
                    last_frame_start = frames;
                }
                Command::Step(n) => { pause_in_frames = Some(n) }
                Command::PrintFrames(v) => { should_print_frame = v }
                Command::PrintStat(v) => { should_dump_stat = v }
                Command::Fast(v) => {
                    fast = v;
                    start = time::Instant::now();
                    last_frame_start = frames;
                    info!("Fast = {}", v);
                }
            }
        }

        if pause_in_frames != Some(0) {
            let v = out.video.as_mut().unwrap();

            clocks = next_frame(&mut cpu, &gpu, w, h, &mut fb, frames, clocks)
                .unwrap_or_else(|e| critical(&cpu, e));


            let mut out_buf = screen.buf_mut();
            for r in 0..h {
                let mut row = out_buf.line(r);
                for c in 0..w {
                    row.set(c, fb[(r * w) + c].into())
                }
            }

            v.render_frame(&screen.buf());
            v.window.gl_swap_window();

            v.update_fps();

            if should_print_frame {
                info!("Frame nr: {}", frames);
            }
            frames += 1;

            pause_in_frames = pause_in_frames.and_then(|n| if n > 0 { Some(n - 1) } else { None });

            if !fast {
                let when = start + time::Duration::from_millis(((frames - last_frame_start) * 1000) / FRAMES_PER_SECONDS);
                let now = time::Instant::now();
                if when > now {
                    let diff = when - now;
                    std::thread::sleep(diff);
                } else {
                    let diff = now - when;
                    if should_dump_stat {
                        info!("I'm late: {:?}", diff);
                    }
                    late += diff;
                }
            }
        } else {
            std::thread::sleep(time::Duration::from_millis(100));
        }
    }
    if should_dump_stat {
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

fn keys_apply(event: &Event, keys: &mut Vec<Box<dyn WindowKey>>) {
    keys.iter_mut().for_each(|b| {
        b.update(event);
    });
}

fn keys(io: Rc<IO>, tx: Sender<Command>, fast: bool, should_print_frame: bool, should_print_late_stat: bool) -> Vec<Box<dyn WindowKey>> {
    let mut buttons = si_keys(io);
    buttons.extend(ui_keys(tx, fast, should_print_frame, should_print_late_stat).into_iter());
    buttons
}

fn ui_keys(tx: Sender<Command>, fast: bool, should_print_frame: bool, should_print_late_stat: bool) -> Vec<Box<dyn WindowKey>> {
    let quit = ui_key(Keycode::Escape,
                      |_| Command::Quit, false, tx.clone());
    let pause = ui_key(Keycode::P,
                       |s| if s { Command::Pause } else { Command::Continue },
                       false, tx.clone());
    let print_frame = ui_key(Keycode::F,
                             |s| Command::PrintFrames(s),
                             should_print_frame, tx.clone());
    let print_stat = ui_key(Keycode::G,
                            |s| Command::PrintStat(s),
                            should_print_late_stat, tx.clone());
    let dump_state = ui_key(Keycode::D,
                            |_| Command::Dump,
                            false, tx.clone());
    let fast = ui_key(Keycode::Plus,
                      |s| Command::Fast(s),
                      fast, tx.clone());
    let mut keys = vec![quit, pause, print_frame, print_stat, dump_state, fast];
    let steps_key: Vec<_> = vec![(Keycode::S, 1), (Keycode::Q, 5), (Keycode::W, 10), (Keycode::E, 20), (Keycode::R, 60), (Keycode::T, 600)];
    keys.extend(steps_key.into_iter().map(
        |(k, frames)| ui_key(k,
                             move |_| Command::Step(frames),
                             false, tx.clone())
    ));
    keys
}

fn ui_key<'a>(key: Keycode, action: impl Fn(bool) -> Command + 'a, state: bool, tx: Sender<Command>) -> Box<dyn WindowKey + 'a> {
    box_key(ActiveKey::new(FlipFlopKey::new(DirectKey::from(key), state),
                           move |state| tx.send(action(state)).unwrap(),
    ))
}

fn si_keys(io: Rc<IO>) -> Vec<Box<dyn WindowKey>> {
    [
        (Keycode::Num5, io::Ev::Coin),
        (Keycode::Num1, io::Ev::P1Start),
        (Keycode::Left, io::Ev::P1Left),
        (Keycode::Right, io::Ev::P1Right),
        (Keycode::Space, io::Ev::P1Shoot),
    ].into_iter().cloned()
        .map(|(k, ev)| game_key(k, io.clone(), ev))
        .collect()
}

fn game_key<'a, I: Deref<Target=IO> + 'a>(key: Keycode, io: I, ev: io::Ev) -> Box<dyn WindowKey + 'a> {
    box_key(DKey::new(key.into(), move |state| io.ui_event(ev, state)))
}

fn box_key<'a, K: WindowKey + 'a>(k: K) -> Box<dyn WindowKey + 'a> {
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
