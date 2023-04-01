use std::fs::File;
use std::io::{self, Read, Write};
use std::path::PathBuf;

use clap::Parser;
use log::{debug, error, info};

use pixels::{Pixels, PixelsBuilder, SurfaceTexture};
use winit::event::VirtualKeyCode;
use winit::event_loop::EventLoop;
use winit::window::WindowBuilder;
use winit_input_helper::WinitInputHelper;

use cpal::traits::{DeviceTrait, HostTrait, StreamTrait};
use cpal::{SampleFormat, SampleRate, Stream};
use ringbuf::{Consumer, HeapRb};
use std::sync::Arc;

use feo3boy::gb::Gb;
use feo3boy::input::{ButtonStates, InputContext};
use feo3boy::memdev::{BiosRom, Cartridge};

fn init_audio_stream(
    mut sample_consumer: Consumer<(i16, i16), Arc<HeapRb<(i16, i16)>>>,
) -> Option<(Stream, SampleRate)> {
    if let Some(device) = cpal::default_host().default_output_device() {
        let supported_config = {
            match device.supported_output_configs() {
                Ok(mut supported_configs) => loop {
                    if let Some(config) = supported_configs.next() {
                        if config.channels() == 2 && config.sample_format() == SampleFormat::F32 {
                            break Some(config.with_max_sample_rate());
                        } else {
                            continue;
                        }
                    } else {
                        error!("Default audio output device does not support stereo, continuing in silence.");
                        break None;
                    }
                },
                Err(err) => {
                    error!(
                        "Error querying supported output configs: {:#?}\nContinuing in silence.",
                        err
                    );
                    None
                }
            }
        };

        if let Some(supported_config) = supported_config {
            let err_handler = |err| error!("Error in output audio stream: {}", err);
            let sample_rate = supported_config.sample_rate();

            let callback = move |data: &mut [f32], _: &cpal::OutputCallbackInfo| {
                info!("Running f32 audio callback");
                let mut last_sample = (0.0, 0.0);
                for stereo_sample in data.chunks_mut(2) {
                    let sample_pair = match sample_consumer.pop() {
                        Some((left, right)) => {
                            let sample_pair = (left as f32 / 255.0, right as f32 / 255.0);
                            info!(
                                "Writing ({},{}) to audio buffer",
                                sample_pair.0, sample_pair.1
                            );
                            sample_pair
                        }
                        None => {
                            //warn!("Sample FIFO empty");
                            last_sample
                        }
                    };
                    stereo_sample[0] = sample_pair.0;
                    stereo_sample[1] = sample_pair.1;
                    last_sample = sample_pair;
                }
            };

            match device.build_output_stream(&supported_config.into(), callback, err_handler, None)
            {
                Ok(stream) => Some((stream, sample_rate)),
                Err(err) => {
                    error!("Error building audio output stream: {}", err);
                    None
                }
            }
        } else {
            None
        }
    } else {
        error!("No audio output device found, continuing in silence.");
        None
    }
}

fn pixels_render(pixels: &mut Pixels, screen_buffer: &[(u8, u8, u8)]) {
    let frame = pixels.frame_mut();

    for pixel_pair in screen_buffer.iter().zip(frame.chunks_mut(4)) {
        let (in_pixel, out_pixel) = pixel_pair;

        out_pixel[0] = in_pixel.0;
        out_pixel[1] = in_pixel.1;
        out_pixel[2] = in_pixel.2;
        out_pixel[3] = 0xff;
    }
}

fn gen_button_states(
    input_helper: &WinitInputHelper,
    bindings: &Vec<(VirtualKeyCode, ButtonStates)>,
) -> ButtonStates {
    let mut button_states = ButtonStates::empty();

    for (key_code, button) in bindings {
        if input_helper.key_held(*key_code) {
            button_states.insert(*button);
        }
    }

    button_states
}

/// Feo3boy is a GameBoy emulator.
#[derive(Parser, Debug)]
struct Args {
    /// File containing the BIOS dump.
    #[arg(short, long)]
    bios: Option<PathBuf>,
    /// File containing the ROM dump.
    #[arg(short, long)]
    rom: Option<PathBuf>,
    /// Mute the emulator (don't set up an audio stream).
    #[arg(short, long)]
    mute: bool,
}

fn main() {
    env_logger::init();
    info!("feo3boy");
    let args = Args::parse();

    let bios = match args.bios {
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

    let cart = match args.rom {
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
        PixelsBuilder::new(160, 144, surface_texture)
            .enable_vsync(true)
            .build()
            .unwrap()
    };

    let rb = HeapRb::new(200);
    let (mut sample_producer, sample_consumer) = rb.split();
    let audio_output_config = init_audio_stream(sample_consumer);

    if !args.mute {
        match audio_output_config {
            Some((ref stream, sample_rate)) => {
                gb.set_sample_rate(sample_rate.0);
                if let Err(err) = stream.play() {
                    error!("Error playing audio stream: {}", err);
                }
                //Some(stream)
            }
            _ => {
                error!("No audio stream set up");
                //None
            }
        }
    }

    let bindings = vec![
        (VirtualKeyCode::W, ButtonStates::UP),
        (VirtualKeyCode::A, ButtonStates::LEFT),
        (VirtualKeyCode::S, ButtonStates::DOWN),
        (VirtualKeyCode::D, ButtonStates::RIGHT),
        (VirtualKeyCode::C, ButtonStates::SELECT),
        (VirtualKeyCode::Return, ButtonStates::START),
        (VirtualKeyCode::Slash, ButtonStates::A),
        (VirtualKeyCode::Period, ButtonStates::B),
    ];

    event_loop.run(move |event, _, control_flow| {
        control_flow.set_poll();

        if input_helper.update(&event) {
            if input_helper.close_requested() {
                control_flow.set_exit();
            }

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
                    }
                    None => (),
                }
            }
        }
    });
}
