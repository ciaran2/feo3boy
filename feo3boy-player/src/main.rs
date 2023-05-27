use std::fs::File;
use std::io::{self, Read, Write};
use std::path::PathBuf;
use std::time::{Duration, Instant};

use clap::{ArgAction, Parser};
use executor_selector::ExecutorSelector;
use feo3boy::apu::Sample;
use feo3boy::gbz80core::executor::Executor;
use log::{debug, error, info, trace};

use pixels::{Pixels, PixelsBuilder, SurfaceTexture};
use winit::event::VirtualKeyCode;
use winit::event_loop::EventLoop;
use winit::window::WindowBuilder;
use winit_input_helper::WinitInputHelper;

use cpal::traits::{DeviceTrait, HostTrait, StreamTrait};
use cpal::{BufferSize, SampleFormat, SampleRate, Stream, StreamConfig, SupportedBufferSize};

use feo3boy::gb::Gb;
use feo3boy::input::{ButtonStates, InputContext};
use feo3boy::memdev::{BiosRom, Cartridge, SaveData};

use crate::audio::{AudioStream, RbAudioSink, RbAudioStream};

mod audio;
mod executor_selector;

fn init_audio_stream() -> Option<(Stream, SampleRate, RbAudioSink)> {
    if let Some(device) = cpal::default_host().default_output_device() {
        let supported_config = {
            match device.supported_output_configs() {
                Ok(mut supported_configs) => loop {
                    const SAMPLE_RATE_48_KHZ: SampleRate = SampleRate(48000);
                    if let Some(config) = supported_configs.next() {
                        if config.channels() == 2 && config.sample_format() == SampleFormat::F32 {
                            let sample_rate = SAMPLE_RATE_48_KHZ
                                .clamp(config.min_sample_rate(), config.max_sample_rate());
                            break Some(config.with_sample_rate(sample_rate));
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

            let desired_buffer_base_size = sample_rate.0 / 60;
            // Increase the buffer size by 5% over necessary.
            let desired_buffer_size = desired_buffer_base_size + desired_buffer_base_size / 20;

            let buffer_size = match supported_config.buffer_size() {
                SupportedBufferSize::Unknown => BufferSize::Default,
                &SupportedBufferSize::Range { min, max }
                    if min <= desired_buffer_size && max >= desired_buffer_size =>
                {
                    BufferSize::Fixed(desired_buffer_size)
                }
                &SupportedBufferSize::Range { min, .. } if min > desired_buffer_size => {
                    BufferSize::Fixed(min)
                }
                &SupportedBufferSize::Range { max, .. } if max < desired_buffer_size => {
                    BufferSize::Fixed(max)
                }
                _ => unreachable!(),
            };
            info!(
                "Using supported config with supported buffer size {:?}",
                buffer_size
            );

            let ringbuf_size = match buffer_size {
                // Give the ringbuffer a margin over the sample rate in case of slower
                // event loop cycles during frame rendering. This adds latency but keeps
                // the audio smooth.
                BufferSize::Fixed(size) => size * 2,
                // We have no idea how big the buffer actually is, so we may produce a lot
                // of unnecessary latency this way, or we might end up with broken audio,
                // there's no way to know. And we can't dynamically resize the buffer with
                // the current implementation, which would be a way to solve this.
                BufferSize::Default => desired_buffer_size * 4,
            };
            let (sample_producer, mut sample_consumer) = RbAudioStream::new(ringbuf_size as usize);

            let callback = move |data: &mut [f32], _: &cpal::OutputCallbackInfo| {
                trace!(
                    "Running f32 audio callback: samples requested: {}",
                    data.len()
                );
                for stereo_sample in data.chunks_mut(2) {
                    let sample = sample_consumer.get_next_sample();
                    stereo_sample[0] = sample.left;
                    stereo_sample[1] = sample.right;
                }
            };

            let mut config: StreamConfig = supported_config.into();
            config.buffer_size = buffer_size;
            match device.build_output_stream(&config, callback, err_handler, None) {
                Ok(stream) => Some((stream, sample_rate, sample_producer)),
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
    /// Choose which executor to use for the CPU.
    #[arg(long, value_enum, default_value_t)]
    executor: ExecutorSelector,
    /// Whether to enable vsync (defaults to true).
    #[arg(long, action = ArgAction::Set, default_value_t = true)]
    vsync: bool,
    /// Run this many times slower than realtime.
    #[arg(long, default_value_t = 1)]
    slomo: u32,
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
    let mut gb = Gb::for_config(bios, cart, &args.executor);

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
            .enable_vsync(args.vsync)
            .build()
            .unwrap()
    };

    let audio_output_config = init_audio_stream();

    let slomo = args.slomo.max(1);

    let mut sample_producer = if !args.mute {
        match audio_output_config {
            Some((ref stream, sample_rate, sample_producer)) => {
                info!("Setting output sample rate: {}", sample_rate.0);
                gb.set_sample_rate(sample_rate.0 * slomo);
                if let Err(err) = stream.play() {
                    error!("Error playing audio stream: {}", err);
                }
                sample_producer
            }
            _ => {
                error!("No audio stream set up");
                let (producer, _) = RbAudioStream::new(1);
                producer
            }
        }
    } else {
        let (producer, _) = RbAudioStream::new(1);
        producer
    };

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

    let mut pending_audio_sample: Option<Sample> = None;

    let mut last_loop_start = Instant::now();
    let mut scaled_time_since_startup = Duration::ZERO;

    event_loop.run(move |event, _, control_flow| {
        let loop_start = Instant::now();
        let last_loop_time = loop_start - last_loop_start;
        last_loop_start = loop_start;
        // This is fine because Duration is an integer-based type, so we can do it
        // cumulatively like this.
        scaled_time_since_startup += last_loop_time / slomo;

        // Amount of time allocated for fast-forward.
        let ff_end = loop_start + last_loop_time.mul_f32(0.9).max(Duration::from_millis(5));
        trace!("Event loop time: {}s", last_loop_time.as_secs_f64());

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
                        Ok(save_file) => {
                            if let Err(err) = gb.write_save_data(save_file) {
                                error!("Error writing to save file: {}", err);
                            }
                        }
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

            let mut had_frame = false;
            loop {
                if let Some(sample) = pending_audio_sample.take() {
                    match sample_producer.push(sample) {
                        Ok(()) => {}
                        Err(sample) if fast_forward => {
                            debug!("Discarding {sample:?} due to fast-forward");
                        }
                        Err(sample) if args.mute => {
                            debug!("Discarding {sample:?} due to muted output");
                        }
                        Err(sample) => {
                            debug!("Sample queue full, saving sample for future iteration.");
                            pending_audio_sample = Some(sample);
                            break;
                        }
                    }
                }

                write_serial_data(&mut gb, &mut stdout);
                gb.tick();

                if let Some(sample) = gb.consume_audio_sample() {
                    avg_sample_interval = if num_sample_intervals == 0.0 {
                        sample_instant.elapsed().as_micros() as f64
                    } else {
                        avg_sample_interval * (num_sample_intervals - 1.0) / num_sample_intervals
                            + sample_instant.elapsed().as_micros() as f64 / num_sample_intervals
                    };
                    num_sample_intervals += 1.0;
                    sample_instant = Instant::now();
                    debug!("Sending {sample:?} to sample FIFO");
                    pending_audio_sample = Some(sample);
                }
                had_frame = had_frame || gb.display_ready();
                if fast_forward {
                    if Instant::now() > ff_end {
                        break;
                    }
                } else if args.mute {
                    let emu_time = gb.clock().elapsed_time();
                    if emu_time > scaled_time_since_startup {
                        // Emulator has gotten ahead of realtime, time to break.
                        break;
                    }
                }
            }
            if had_frame {
                pixels_render(&mut pixels, gb.ppu.screen_buffer());
                if let Err(err) = pixels.render() {
                    error!("Render failed: {}", err);
                    control_flow.set_exit()
                }
            }
        }
    });
}

/// Read all serial data from the GameBoy and write it to the given [`Write`] destination.
fn write_serial_data<E: Executor>(gb: &mut Gb<E>, mut dest: impl Write) {
    let bytes = gb.serial.stream.receive_bytes();
    if bytes.len() != 0 {
        for byte in bytes {
            dest.write_all(&[byte]).unwrap();
        }
        dest.flush().unwrap();
    }
}
