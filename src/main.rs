extern crate clap;

use clap::{App, Arg};
use std::fs::File;

mod memdev;
//mod rom;
mod gbz80core;

fn main() {
    println!("feo3boy");

    let argparser = App::new("FEO3Boy")
        .version("0.1.0")
        .author("Ciaran Cain <ciaran2@umbc.edu>")
        .arg(
            Arg::with_name("bios")
                .short("b")
                .long("bios")
                .takes_value(true)
                .help("File containing BIOS dump"),
        )
        .arg(
            Arg::with_name("rom")
                .short("r")
                .long("rom")
                .takes_value(true)
                .help("File containing ROM dump"),
        )
        .get_matches();

    let mut mmu = memdev::GbMmu::new();
    let mut cpustate = gbz80core::Gbz80state::new();

    match argparser.value_of("bios") {
        Some(filename) => {
            let mut bios_file = File::open(filename).unwrap();
            mmu.set_bios(memdev::BiosRom::new(&mut bios_file));
        }
        None => (),
    }

    loop {
        gbz80core::tick(&mut cpustate, &mut mmu);
    }
}
