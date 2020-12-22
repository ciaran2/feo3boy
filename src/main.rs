use std::fs::File;
use std::io::Read;

use clap::{App, Arg};

use memdev::{BiosRom, Cartridge, GbMmu};

mod gbz80core;
mod memdev;

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

    let bios = match argparser.value_of("bios") {
        Some(filename) => {
            let mut bios_file = File::open(filename).unwrap();
            let mut bios = Vec::with_capacity(0x100);
            bios_file.read_to_end(&mut bios);
            assert!(bios.len() == 0x100, "Bios file was wrong length");
            BiosRom::new(&bios)
        }
        None => BiosRom::default(),
    };

    let cart = Cartridge::default();

    // Box to keep it off the stack.
    let mut mmu = Box::new(GbMmu::new(bios, cart));
    let mut cpustate = gbz80core::Gbz80state::new();

    loop {
        gbz80core::tick(&mut cpustate, &mut *mmu);
    }
}
