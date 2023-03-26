use std::fs::File;
use std::io::{self, Read, Write};
use std::sync::mpsc::{self, Sender, Receiver};

use clap::{App, Arg};
use log::{info, warn, error, debug};

use pixels::{Pixels, PixelsBuilder, SurfaceTexture, Error};
use winit::event::{Event, DeviceEvent, WindowEvent, VirtualKeyCode, ElementState};
use winit::event_loop::{ControlFlow, EventLoop};
use winit::window::WindowBuilder;
use winit_input_helper::WinitInputHelper;

use cpal::{Data, SampleRate, SampleFormat, Stream};
use cpal::traits::{DeviceTrait, HostTrait, StreamTrait};
use std::sync::Arc;
use ringbuf::{HeapRb, Consumer};

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

fn init_audio_stream(mut sample_consumer: Consumer<(i16,i16), Arc<HeapRb<(i16,i16)>>>) -> Option<(Stream, SampleRate)> {
    if let Some(device) = cpal::default_host().default_output_device() {
        let supported_config = {
            match device.supported_output_configs() {
                Ok(mut supported_configs) => {
                    loop {
                        if let Some(config) = supported_configs.next() {
                            if config.channels() == 2 && config.sample_format() == SampleFormat::F32 {
                                break Some(config.with_max_sample_rate());
                            }
                            else {
                                continue;
                            }
                        }
                        else {
                            error!("Default audio output device does not support stereo, continuing in silence.");
                            break None;
                        }
                    }
                }
                Err(err) => {
                    error!("Error querying supported output configs: {:#?}\nContinuing in silence.", err);
                    None
                }
            }
        };

        if let Some(supported_config) = supported_config {
            let err_handler = |err| error!("Error in output audio stream: {}", err);
            let sample_rate = supported_config.sample_rate();

            let callback = move |data: &mut [f32], _: &cpal::OutputCallbackInfo| {
                            info!("Running f32 audio callback");
                            for stereo_sample in data.chunks_mut(2) {
                                let sample_pair = match sample_consumer.pop() {
                                    Some((left, right)) => {
                                        let sample_pair = (left as f32 * (f32::MAX / 255.0), right as f32 * (f32::MAX / 255.0));
                                        debug!("Writing ({},{}) to audio buffer", sample_pair.0, sample_pair.1);
                                        sample_pair
                                    }
                                    None => {
                                        warn!("Sample FIFO empty");
                                        (0.0,0.0)
                                    }
                                };
                                stereo_sample[0] = sample_pair.0;
                                stereo_sample[1] = sample_pair.1;
                            }
                        };

            match device.build_output_stream(&supported_config.into(), callback, err_handler, None) {
                Ok(stream) => Some((stream, sample_rate)),
                Err(err) => {
                    error!("Error building audio output stream: {}", err);
                    None
                }
            }
        }
        else {
            None
        }
    }
    else {
        error!("No audio output device found, continuing in silence.");
        None
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
        PixelsBuilder::new(160, 144, surface_texture).enable_vsync(true).build().unwrap()
    };

    let mut rb = HeapRb::new(200);
    let (mut sample_producer, mut sample_consumer) = rb.split();
    let audio_output_config = init_audio_stream(sample_consumer);

    match audio_output_config {
        Some((stream, sample_rate)) => {
            gb.set_sample_rate(sample_rate.0);
            if let Err(err) = stream.play() {
                error!("Error playing audio stream: {}", err);
            }
        }
        _ => error!("No audio stream set up"),
    }

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
                            stdout.write_all(&[byte]).unwrap();
                        }
                        stdout.flush().unwrap();
                    }
                }
                let (display, audio_sample) = gb.tick();
                if let Some(sample) = audio_sample {
                    debug!("Sending ({},{}) to sample FIFO", sample.0, sample.1);
                    if let Err(sample) = sample_producer.push(sample) {
                        debug!("Error sending {} {} to sample FIFO", sample.0, sample.1);
                    }
                }
                match display {
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
