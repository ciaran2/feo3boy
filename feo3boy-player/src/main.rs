use std::fs::File;
use std::io::{self, Read, Write};

use clap::{App, Arg};
use log::{info, error};

use pixels::{Pixels, SurfaceTexture, Error};
use winit::event::{Event, DeviceEvent, WindowEvent, VirtualKeyCode, ElementState};
use winit::event_loop::{ControlFlow, EventLoop};
use winit::window::WindowBuilder;
use winit_input_helper::WinitInputHelper;

use feo3boy::gb::Gb;
use feo3boy::memdev::{BiosRom, Cartridge};
use feo3boy::input::{InputContext, ButtonStates};

fn ascii_render(screen_buffer: &[(u8, u8, u8)]) {
    let brightness_scale = ".'`^\",:;Il!i><~+_-?][}{1)(|\\/tfjrxnuvczXYUJCLQ0OZmwqpdbkhao*#MW&8%B@$";

    for line in screen_buffer.chunks(160) {
        let mut ascii_buffer = Vec::new();
        for pixel in line {
            ascii_buffer.push(brightness_scale.as_bytes()[brightness_scale.len() * ((pixel.0 as usize + pixel.1 as usize + pixel.2 as usize) / 3) / 256]);
        }
        print!("{}", std::str::from_utf8(&ascii_buffer).unwrap())
    }
}

fn pixels_render(pixels: &mut Pixels, screen_buffer: &[(u8,u8,u8)]) {
    let frame = pixels.get_frame_mut();

    for pixel_pair in screen_buffer.iter().zip(frame.chunks_mut(4)) {
        let (in_pixel, out_pixel) = pixel_pair;

        out_pixel[0] = in_pixel.0;
        out_pixel[1] = in_pixel.1;
        out_pixel[2] = in_pixel.2;
        out_pixel[3] = 0xff;
    }
}

fn gen_button_states(input_helper: &WinitInputHelper, bindings: &Vec<(VirtualKeyCode, ButtonStates)>) -> ButtonStates {
    let mut button_states = ButtonStates::empty();

    for (key_code, button) in bindings {
        if input_helper.key_held(*key_code) {
            button_states.insert(*button);
        }
    }

    button_states
}

fn main() {
    env_logger::init();
    info!("feo3boy");

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
        .arg(
            Arg::with_name("ascii-video")
                .long("ascii-video")
                .takes_value(false)
                .help("Dump screen buffer to terminal rendered with ASCII characters"),
        )
        .get_matches();

    let bios = match argparser.value_of("bios") {
        Some(filename) => {
            let mut bios_file = File::open(filename).unwrap();
            let mut bios = Vec::with_capacity(0x100);
            bios_file
                .read_to_end(&mut bios)
                .expect("Unable to read bios file");
            BiosRom::try_from_slice(&bios).expect("Bios file was wrong length")
        }
        None => BiosRom::default(),
    };

    let cart = match argparser.value_of("rom") {
        Some(filename) => {
            let cart_file = File::open(filename).unwrap();
            Cartridge::parse(cart_file).unwrap()
        }
        None => Cartridge::None,
    };

    // Box to keep it off the stack.
    let mut gb = Gb::new(bios, cart);

    let mut stdout = io::stdout();

    let event_loop = EventLoop::new();
    let window = WindowBuilder::new().build(&event_loop).unwrap();
    let mut input_helper = WinitInputHelper::new();

    let mut pixels = {
        let window_size = window.inner_size();
        let surface_texture = SurfaceTexture::new(window_size.width, window_size.height, &window);
        Pixels::new(160, 144, surface_texture).unwrap()
    };

    let mut bindings = vec![ (VirtualKeyCode::W, ButtonStates::UP),
                              (VirtualKeyCode::A, ButtonStates::LEFT),
                              (VirtualKeyCode::S, ButtonStates::DOWN),
                              (VirtualKeyCode::D, ButtonStates::RIGHT),
                              (VirtualKeyCode::C, ButtonStates::SELECT),
                              (VirtualKeyCode::Return, ButtonStates::START),
                              (VirtualKeyCode::Slash, ButtonStates::A),
                              (VirtualKeyCode::Period, ButtonStates::B) ];

    event_loop.run(move |event, _, control_flow| {
        control_flow.set_poll();

        if input_helper.update(&event) {
            if input_helper.close_requested() { control_flow.set_exit(); }

            gb.set_button_states(gen_button_states(&input_helper, &bindings));

            for _i in 0..100 {
                {
                    let bytes = gb.serial.stream.receive_bytes();
                    if bytes.len() != 0 {
                        for byte in bytes {
                            stdout.write(&[byte]).unwrap();
                        }
                        stdout.flush().unwrap();
                    }
                }
                match gb.tick() {
                    Some(screen_buffer) => {
                        pixels_render(&mut pixels, screen_buffer);
                        if let Err(err) = pixels.render() {
                            error!("Render failed: {}", err);
                            control_flow.set_exit()
                        }
                        //window.request_redraw();
                    },
                    None => (),
                }
            }
        }
    });
}
