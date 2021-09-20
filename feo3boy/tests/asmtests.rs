use std::{iter, mem};

use feo3boy::gbz80core::{self, Gbz80State};
use feo3boy::memdev::{Addr, MemDevice};

#[test]
fn fibonacci() {
    const OUTPUT: usize = 0xC000;

    let mut mem = ExtendMem::from(include_bytes!("fibonacci.bin"));
    let mut cpu = Gbz80State::default();
    while !cpu.halted {
        gbz80core::tick((&mut cpu, &mut mem));
    }

    let (mut f1, mut f2) = (0, 1);
    for (i, fib) in iter::from_fn(move || {
        let res = f1;
        f1 += f2;
        mem::swap(&mut f1, &mut f2);
        Some(res)
    })
    .enumerate()
    {
        if fib > u8::MAX as usize {
            break;
        }
        assert_eq!(mem.0[OUTPUT + i], fib as u8);
    }
}

/// Creates a MemDevice from a Vec<u8>. This memory device will return 0 for any read beyond the
/// end of the vec and will automatically extend the vec to cover any write beyond the end.
struct ExtendMem(Vec<u8>);

impl From<&[u8]> for ExtendMem {
    /// Creates an ExtendMem with initail data set by copying from a byte slice.
    fn from(bytes: &[u8]) -> Self {
        Self(Vec::from(bytes))
    }
}

impl<const N: usize> From<&[u8; N]> for ExtendMem {
    /// Creates an ExtendMem with initail data set by copying from a byte slice.
    fn from(bytes: &[u8; N]) -> Self {
        Self(Vec::from(&bytes[..]))
    }
}

impl MemDevice for ExtendMem {
    fn read(&self, addr: Addr) -> u8 {
        self.0.get(addr.index()).copied().unwrap_or(0)
    }

    fn write(&mut self, addr: Addr, data: u8) {
        if addr.index() >= self.0.len() {
            self.0.resize(addr.index() + 1, 0);
        }
        self.0[addr.index()] = data;
    }
}
