mod memdev;
//mod rom;
mod gbz80core;

fn main() {
    println!("feo3boy");
    let mut mmu = memdev::GbMmu::new();
    let mut cpustate = gbz80core::Gbz80state::new();

    loop {
        gbz80core::tick(&mut cpustate, &mut mmu);
    }
}
