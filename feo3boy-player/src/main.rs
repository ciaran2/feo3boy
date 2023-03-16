use std::fs::File;
use std::io::{self, Read, Write};

use clap::{App, Arg};
use log::{info, error};

use pixels::{Pixels, SurfaceTexture, Error};
use winit::event::{Event, DeviceEvent, WindowEvent};
use winit::event_loop::{ControlFlow, EventLoop};
use winit::window::WindowBuilder;

use feo3boy::gb::Gb;
use feo3boy::memdev::{BiosRom, Cartridge};
use feo3boy::input::{ButtonStates};

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

    let mut button_state = ButtonStates::empty();

    let mut pixels = {
        let window_size = window.inner_size();
        let surface_texture = SurfaceTexture::new(window_size.width, window_size.height, &window);
        Pixels::new(160, 144, surface_texture).unwrap()
    };

    event_loop.run(move |event, _, control_flow| {
        control_flow.set_poll();

        match event {
            Event::WindowEvent {
                event: WindowEvent::CloseRequested,
                ..
            } => control_flow.set_exit(),
            Event::DeviceEvent {
                event: device_event,
                device_id,
            } => {
                match device_event {
                    DeviceEvent::Key(keyboard_input) => {
                        info!("{:#?} {} {:#?}", keyboard_input.virtual_keycode, keyboard_input.scancode, keyboard_input.state);
                    }
                    _ => ()
                }
            }
            Event::MainEventsCleared => {
                let mut output_state = gb.tick();
                while output_state == None {
                    {
                        let bytes = gb.serial.stream.receive_bytes();
                        if bytes.len() != 0 {
                            for byte in bytes {
                                stdout.write(&[byte]).unwrap();
                            }
                            stdout.flush().unwrap();
                        }
                    }
                    output_state = gb.tick();
                }
                match output_state {
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
            },
            //Event::RedrawRequested(_) => if let Err(err) = pixels.render() {
            //    error!("Render failed: {}", err);
            //    control_flow.set_exit()
            //},
            _ => ()
        }
    });
}
