use std::fs::File;
use std::io::{self, Read, Write};
use std::path::PathBuf;
use std::thread::sleep;
use std::time::{Duration, Instant};

use clap::Parser;
use log::{debug, error, info, warn};

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
use feo3boy::memdev::{BiosRom, Cartridge, SaveData};

fn init_audio_stream(
    mut sample_consumer: Consumer<(f32, f32), Arc<HeapRb<(f32, f32)>>>,
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
            info!(
                "Using supported config with supported buffer size {:?}",
                supported_config.buffer_size()
            );

            let mut last_sample = (0.0, 0.0);
            let callback = move |data: &mut [f32], _: &cpal::OutputCallbackInfo| {
                debug!("Running f32 audio callback");
                for stereo_sample in data.chunks_mut(2) {
                    let sample_pair = match sample_consumer.pop() {
                        Some((left, right)) => {
                            debug!("Writing ({},{}) to audio buffer", left, right);
                            (0.25 * left, 0.25 * right)
                        }
                        None => {
                            warn!("Sample FIFO empty");
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

    let (cart, save_filename) = match args.rom {
        Some(ref filename) => {
            let cart_file = File::open(filename).unwrap();
            let mut save_filename = PathBuf::from(filename);
            save_filename.set_extension("sav");
            let mut cart = Cartridge::parse(cart_file).unwrap();

            match File::open(save_filename.as_path()) {
                Ok(save_file) => {
                    cart.load_save_data(save_file).unwrap();
                }
                _ => (),
            }

            (cart, Some(save_filename))
        }
        None => (Cartridge::None, None),
    };

    // Box to keep it off the stack.
    let mut gb = Gb::new(bios, cart);

    let mut stdout = io::stdout();

    let event_loop = EventLoop::new();
    let window = WindowBuilder::new()
        .with_title("feo3boy-player")
        .build(&event_loop)
        .unwrap();
    let mut input_helper = WinitInputHelper::new();

    let mut pixels = {
        let window_size = window.inner_size();
        let surface_texture = SurfaceTexture::new(window_size.width, window_size.height, &window);
        PixelsBuilder::new(160, 144, surface_texture)
            .enable_vsync(true)
            .build()
            .unwrap()
    };

    // TODO: make this dynamic to the actual sample rate
    // exceeding 48K in the output should be pretty rare though
    let rb = HeapRb::new(48000 / 60 * 2);
    let (mut sample_producer, sample_consumer) = rb.split();
    let audio_output_config = init_audio_stream(sample_consumer);

    if !args.mute {
        match audio_output_config {
            Some((ref stream, sample_rate)) => {
                info!("Setting output sample rate: {}", sample_rate.0);
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

    let fast_forward_binding = VirtualKeyCode::Tab;

    let mut avg_sample_interval: f64 = 0.0;
    let mut num_sample_intervals: f64 = 0.0;
    let mut sample_instant = Instant::now();

    event_loop.run(move |event, _, control_flow| {
        control_flow.set_poll();

        if input_helper.update(&event) {
            if input_helper.close_requested() {
                if gb.has_save_data() {
                    match File::create(
                        save_filename
                            .as_ref()
                            .expect("No known path for save file")
                            .as_path(),
                    ) {
                        Ok(save_file) => match gb.write_save_data(save_file) {
                            Ok(_) => (),
                            Err(err) => error!("Error writing to save file: {}", err),
                        },
                        Err(err) => error!("Error opening save file: {}", err),
                    }
                }
                info!(
                    "Average interval between samples: {} microseconds",
                    avg_sample_interval
                );
                control_flow.set_exit();
            }

            gb.set_button_states(gen_button_states(&input_helper, &bindings));

            let fast_forward = input_helper.key_held(fast_forward_binding);

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
                    avg_sample_interval = if num_sample_intervals == 0.0 {
                        sample_instant.elapsed().as_micros() as f64
                    } else {
                        avg_sample_interval * (num_sample_intervals - 1.0) / num_sample_intervals
                            + sample_instant.elapsed().as_micros() as f64 / num_sample_intervals
                    };
                    num_sample_intervals += 1.0;
                    sample_instant = Instant::now();

                    debug!("Sending ({},{}) to sample FIFO", sample.0, sample.1);

                    if !fast_forward {
                        while let Err(sample) = sample_producer.push(sample) {
                            debug!("Error sending ({},{}) to sample FIFO", sample.0, sample.1);
                        }
                    } else {
                        if let Err(sample) = sample_producer.push(sample) {
                            debug!("Error sending ({},{}) to sample FIFO", sample.0, sample.1);
                        }
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
