//! Exported macro definitions. This is a separate module because macros must be defined
//! before the module they are used in (one of the few cases of declaration
//! order-dependence in Rust).

/// Build a MemDevice from fields of a struct.
#[macro_export]
macro_rules! memdev_fields {
    ($ty:ident, len: $len:literal, {
        $($start:literal $(..= $end:literal)? => $target:tt,)*
    }) => {
        impl $crate::memdev::MemDevice for $ty {
            const LEN: usize = $len;

            fn read_byte_relative(&self, addr: $crate::memdev::RelativeAddr) -> u8 {
                $crate::dispatch_memdev_byte!($ty, addr, |addr| {
                    $($start $(..= $end)? => $crate::memdev_fields!(@reader: self, addr, $target),)*
                })
            }

            fn read_bytes_relative(&self, addr: $crate::memdev::RelativeAddr, data: &mut [u8]) {
                $crate::dispatch_memdev_bytes!($ty, addr, data, |addr, mut data| {
                    $($start $(..= $end)? => {
                        $crate::memdev_fields!(@range_reader: self, addr, data, $target)
                    },)*
                })
            }

            fn write_byte_relative(&mut self, addr: $crate::memdev::RelativeAddr, val: u8) {
                $crate::dispatch_memdev_byte!($ty, addr, |addr| {
                    $($start $(..= $end)? => $crate::memdev_fields!(@writer: self, addr, val, $target),)*
                })
            }

            fn write_bytes_relative(&mut self, addr: $crate::memdev::RelativeAddr, data: &[u8]) {
                $crate::dispatch_memdev_bytes!($ty, addr, data, |addr, ref data| {
                    $($start $(..= $end)? => {
                        $crate::memdev_fields!(@range_writer: self, addr, data, $target)
                    },)*
                });
            }
        }
    };

    (@reader: $self:ident, $addr:expr, $target:ident) => {
        $crate::memdev::MemDevice::read_byte_relative(&$self.$target, $addr)
    };
    (@reader: $self:ident, $addr:expr, $target:literal) => {
        $target
    };
    (@reader: $self:ident, $addr:expr, { $target:ident, skip_over: $skip:literal }) => {
        $crate::memdev_fields!(@reader: $self, $addr.skip_over($skip), $target)
    };
    (@reader: $self:ident, $addr:expr, { $target:ident, readonly }) => {
        $crate::memdev_fields!(@reader: $self, $addr, $target)
    };

    (@range_reader: $self:ident, $addr:expr, $data:ident, $target:ident) => {
        $crate::memdev::MemDevice::read_bytes_relative(&$self.$target, $addr, $data)
    };
    (@range_reader: $self:ident, $addr:expr, $data:ident, $target:literal) => {
        $data.fill($target)
    };
    (@range_reader: $self:ident, $addr:expr, $data:ident, { $target:ident, skip_over: $skip:literal }) => {
        $crate::memdev_fields!(@range_reader: $self, $addr.skip_over($skip), $data, $target)
    };
    (@range_reader: $self:ident, $addr:expr, $data:ident, { $target:ident, readonly }) => {
        $crate::memdev_fields!(@range_reader: $self, $addr, $data, $target)
    };

    (@writer: $self:ident, $addr:expr, $val:ident, $target:ident) => {
        $crate::memdev::MemDevice::write_byte_relative(&mut $self.$target, $addr, $val)
    };
    (@writer: $self:ident, $addr:expr, $val:ident, $target:literal) => {
        ()
    };
    (@writer: $self:ident, $addr:expr, $val:ident, { $target:ident, skip_over: $skip:literal }) => {
        $crate::memdev_fields!(@writer: $self, $addr.skip_over($skip), $val, $target)
    };
    (@writer: $self:ident, $addr:expr, $val:ident, { $target:ident, readonly }) => {
        ()
    };

    (@range_writer: $self:ident, $addr:expr, $data:ident, $target:ident) => {
        $crate::memdev::MemDevice::write_bytes_relative(&mut $self.$target, $addr, $data)
    };
    (@range_writer: $self:ident, $addr:expr, $data:ident, $target:literal) => {
        ()
    };
    (@range_writer: $self:ident, $addr:expr, $data:ident, { $target:ident, skip_over: $skip:literal }) => {
        $crate::memdev_fields!(@range_writer: $self, $addr.skip_over($skip), $data, $target)
    };
    (@range_writer: $self:ident, $addr:expr, $data:ident, { $target:ident, readonly }) => {
        ()
    };

    (@addr: $base:ident, $addr:tt) => { $base.offset_by($addr) };
    (@addr: $base:ident, $addr:literal..=$end:literal) => { $base.offset_by($addr) };
}

/// Dispatch a memdev read/write for a single byte to handlers based on their address.
///
/// usage:
/// ```
/// # use feo3boy::memdev::{MemDevice, RelativeAddr};
/// # use feo3boy::{dispatch_memdev_bytes, dispatch_memdev_byte};
/// # struct MyType(u8, [u8; 3]);
/// # impl MemDevice for MyType {
/// # const LEN: usize = 5;
/// # fn read_bytes_relative(&self, addr: RelativeAddr, data: &mut [u8]) {
/// #     dispatch_memdev_bytes!(MyType, addr, data, |addr, mut data| {
/// #         0x00 => data[0] = self.0.read_byte_relative(addr),
/// #         0x01..=0x04 => self.1.read_bytes_relative(addr, data),
/// #     })
/// # }
/// #
/// fn read_byte_relative(&self, addr: RelativeAddr) -> u8 {
///     dispatch_memdev_byte!(MyType, addr, |addr| {
///         0x00 => self.0.read_byte_relative(addr),
///         0x01..=0x04 => self.1.read_byte_relative(addr),
///     })
/// }
/// #
/// # fn write_bytes_relative(&mut self, addr: RelativeAddr, data: &[u8]) {
/// #     dispatch_memdev_bytes!(MyType, addr, data, |addr, ref data| {
/// #         0x00 => self.0.write_byte_relative(addr, data[0]),
/// #         0x01..=0x04 => self.1.write_bytes_relative(addr, data),
/// #     })
/// # }
///
/// fn write_byte_relative(&mut self, addr: RelativeAddr, val: u8) {
///     dispatch_memdev_byte!(MyType, addr, |addr| {
///         0x00 => self.0.write_byte_relative(addr, val),
///         0x01..=0x04 => self.1.write_byte_relative(addr, val),
///     })
/// }
/// # }
/// ```
///
/// The first argument should be the name of the type being implemented. The second
/// argument is an identifier for the address pased to
/// `read_byte_relative`/`write_byte_relative`.
///
/// The section in `||` defines the field that will be available to the code in the block
/// that matches each range being dispatched.  This must always be exactly one identifier
/// which will be filled with the offset-addr for the matched block.
///
/// The addr provided to the handler for a given address range be the address being
/// read-from `offset_by` the address of the start of the matched range. This means you do
/// not need to use `offset_by` with this macro, which reduces the risk of error relaative
/// to writing this match block yourself. You may still choose to use skip_over if mapping
/// in a block of memory oddly.
///
/// The generated dispatch operation will panic if the slice overflows the mapping in the
/// mem device or if an un-mapped are is encountered.
#[macro_export]
macro_rules! dispatch_memdev_byte {
    ($dev:ty, $addr:ident, |$outaddr:ident| {
        $($start:literal $(..= $end:literal)? $(if $cond:expr)? => $target:expr,)*
    }) => {{
        let addr: $crate::memdev::RelativeAddr = $addr;
        // Block use of the $addr identifier in this block. This is
        // to prevent an easy-to-make mistake where you use the wrong ident
        // and read from the un-offset address rather than the offset one.
        #[allow(unused)]
        let $addr = ();
        $crate::check_addr!($dev, addr);
        match addr.relative() {
            $($start $(..= $end)? $(if $cond)? => {
                #[allow(unused)]
                let $outaddr = addr.offset_by($start);
                { $target }
            })*
            #[allow(unreachable_patterns)]
            _ => panic!(concat!("Unmapped Address {} in device ", stringify!($dev)), addr),
        }
    }};
}

/// Dispatch a memdev read/write over multiple bytes to handlers based on their address.
///
/// usage:
/// ```
/// # use feo3boy::memdev::{MemDevice, RelativeAddr};
/// # use feo3boy::{dispatch_memdev_bytes, dispatch_memdev_byte};
/// # struct MyType(u8, [u8; 4]);
/// # impl MemDevice for MyType {
/// # const LEN: usize = 5;
/// fn read_bytes_relative(&self, addr: RelativeAddr, data: &mut [u8]) {
///     dispatch_memdev_bytes!(MyType, addr, data, |addr, mut data| {
///         0x00 => data[0] = self.0.read_byte_relative(addr),
///         0x01..=0x04 => self.1.read_bytes_relative(addr, data),
///     })
/// }
///
/// fn write_bytes_relative(&mut self, addr: RelativeAddr, data: &[u8]) {
///     dispatch_memdev_bytes!(MyType, addr, data, |addr, ref data| {
///         0x00 => self.0.write_byte_relative(addr, data[0]),
///         0x01..=0x04 => self.1.write_bytes_relative(addr, data),
///     })
/// }
/// # }
/// ```
///
/// The first argument should be the name of the type being implemented. The second
/// argument is an identifier for the address pased to
/// `read_bytes_relative`/`write_bytes_relative`. The third argument is the identifier for
/// the `data` argument passed to the method.
///
/// The section in `||` defines the fields that will be available to the code in the block
/// that matches each range being dispatched.  This must always be two identifiers: the
/// first one will be provided with the address of the read to that segment, offset to the
/// start of the range being matched. The second argument is the original data, sliced to
/// the portion corresponding to the range being read/written. The `data` slice can have
/// an optional `mut` modifier which will cause it to be re-sliced mutably rather than
/// immutably.
///
/// The data provided to the dispatch range is guaranteed to be non-empty, and will not
/// exceed the size of the range specified.
///
/// The addr provided to the handler for a given address range will vary slightly
/// depending on whether this is the first segment being read. If it is the first segment,
/// the address provided will be the address of the original read, `offset_by` the start
/// address of the segment that was matched. If it is one of the other segments, then the
/// address provided will be the address of that segment `offset_by` the address of the
/// segment. In either case, the address will already be offset by the start of the
/// address range being read from.
///
/// The generated dispatch operation will panic if the slice overflows the mapping in the
/// mem device or if an un-mapped are is encountered.
#[macro_export]
macro_rules! dispatch_memdev_bytes {
    ($dev:ty, $addr:ident, $data:ident, |$outaddr:ident, $refmut:tt $outdata:ident| {
        $($start:literal $(..= $end:literal)? $(if $cond:expr)? => $target:expr,)*
    }) => {{
        let orig_data = $data;
        // `addr` is always the address we are reading from relative to the start of
        // *this* mem device. It starts out as the requested read address, and then after
        // reading a subset of data, it will be moved to the address of the next segement
        // to match on.
        let mut addr: $crate::memdev::RelativeAddr = $addr;
        let mut data_len: usize = orig_data.len();
        // Block use of the $addr and $data identifiers in this block. This is
        // to prevent an easy-to-make mistake where you use the wrong ident
        // and read from the *whole* data instead of the subslice.
        #[allow(unused)]
        let $addr = ();
        #[allow(unused)]
        let $data = ();
        $crate::check_addr!($dev, addr, data_len);
        let mut start = 0;
        while data_len > 0 {
            match addr.relative() {
                $($start $(..= $end)? $(if $cond)? => {
                           // Number of bytes available in what is left of this range.
                    let available = ($crate::dispatch_memdev_bytes!(@get_end: $start $(..= $end)?) - addr.relative() + 1) as usize;
                    // Number of bytes we will actually read this iteration.
                    let read = available.min(data_len);
                    // Decrease the number of bytes remaining based on how many we will
                    // read this iteration.
                    data_len -= read;
                    // Compute the range of the input data that is actually being read and
                    // update the start for next time.
                    let end_exclusive = start+read;
                    let datarange = start..end_exclusive;

                    #[allow(unused)]
                    let $outaddr = addr.offset_by($start);
                    #[allow(unused)]
                    let $outdata = $crate::dispatch_memdev_bytes!(@slicedata: $refmut orig_data[datarange]);

                    start = end_exclusive;
                    // We use move_forward_by_wrapping rather than move_forward_by becuase
                    // if we are reading 0xffff, we don't want to crash when incrementing.
                    // If the read covers the full range, this will just end up moving
                    // forward by 0, but that's fine becuase there will be nothing left to
                    // read afterward, so we will break before reading from the same spot
                    // again.
                    addr = addr.move_forward_by_wrapping(read as u16);

                    { $target; }
                })*
                #[allow(unreachable_patterns)]
                _ => panic!(concat!("Unmapped Address {} in device ", stringify!($dev)), addr),
            }
        }
    }};

    (@get_end: $start:literal) => { $start };
    (@get_end: $start:literal ..= $end:literal) => { $end };

    (@slicedata: mut $sliced:expr) => { &mut $sliced };
    (@slicedata: ref $sliced:expr) => { &$sliced };
}

/// Assert that the address is in range for a MemDevice. Takes two args, the first is a
/// MemDevice type, the second is the identifier for an Addr. Note that the MemDevice
/// should be the explicit type rather than Self for the device type to get better error
/// messages.
#[macro_export]
macro_rules! check_addr {
    ($dev:ty, $addr:ident) => {
        assert!(
            $addr.index() < <$dev as $crate::memdev::MemDevice>::LEN,
            concat!("Address {} out of range for ", stringify!($dev)),
            $addr,
        );
    };
    ($dev:ty, $addr:ident, $len:expr) => {{
        let len = $len;
        assert!(
            $crate::memdev::RangeOverlaps::encloses(
                &(0..<$dev as $crate::memdev::MemDevice>::LEN),
                &$addr.range(len),
            ),
            concat!(
                "Address {} + slice of length {} not fully enclosed by ",
                stringify!($dev)
            ),
            $addr,
            len,
        );
    }};
}

/// Implement the memdev range functions from the single-byte reader functions. This
/// effectively does the reverse of the default implementation of read_byte_relative.
#[macro_export]
macro_rules! memdev_bytes_from_byte {
    ($dev:ty) => {
        fn read_bytes_relative(&self, addr: $crate::memdev::RelativeAddr, data: &mut [u8]) {
            $crate::check_addr!($dev, addr, data.len());
            for (offset, dest) in data.iter_mut().enumerate() {
                *dest = $crate::memdev::MemDevice::read_byte_relative(
                    self,
                    addr.move_forward_by(offset as u16),
                );
            }
        }

        fn write_bytes_relative(&mut self, addr: $crate::memdev::RelativeAddr, data: &[u8]) {
            $crate::check_addr!($dev, addr, data.len());
            for (offset, source) in data.iter().enumerate() {
                $crate::memdev::MemDevice::write_byte_relative(
                    self,
                    addr.move_forward_by(offset as u16),
                    *source,
                );
            }
        }
    };
}
