use std::sync::Arc;

use feo3boy::apu::Sample;
use log::warn;
use ringbuf::{Consumer, HeapRb, Producer};

/// Stream of audio coming from the GameBoy which can be fed into the audio stream.
pub trait AudioStream {
    /// Get the next sample from the GameBoy's audio stream.
    fn get_next_sample(&mut self) -> Sample;
}

/// An [`AudioStream`] which pulls samples from the consumer-side of a heap-allocated ring
/// buffer.
pub struct RbAudioStream {
    /// Buffer to pull samples from.
    buf: Consumer<Sample, Arc<HeapRb<Sample>>>,
    /// Most recent sample. Re-used whenever the ringbuffer falls behind.
    last_sample: Sample,
}

pub type RbAudioSink = Producer<Sample, Arc<HeapRb<Sample>>>;

impl RbAudioStream {
    /// Create a new RingBuffer audio stream with capacity for the given number of
    /// samples.
    pub fn new(num_samples: usize) -> (RbAudioSink, Self) {
        let buf = HeapRb::new(num_samples);
        let (sink, stream) = buf.split();
        (
            sink,
            RbAudioStream {
                buf: stream,
                last_sample: Default::default(),
            },
        )
    }
}

impl AudioStream for RbAudioStream {
    fn get_next_sample(&mut self) -> Sample {
        match self.buf.pop() {
            Some(sample) => {
                self.last_sample = sample;
                sample
            }
            None => {
                warn!("Sample FIFO empty");
                self.last_sample
            }
        }
    }
}
