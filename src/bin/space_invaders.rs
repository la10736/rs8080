#![feature(proc_macro)]
extern crate rs8080;
#[cfg(test)]
extern crate rstest;
#[macro_use]
extern crate log;
extern crate simple_logger;

mod si_memory;
mod si_io;

fn main() {
    simple_logger::init().unwrap();

    info!("SPACE INVADERS");
}
