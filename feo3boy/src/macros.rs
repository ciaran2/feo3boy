//! Exported macro definitions. This is a separate module because macros must be defined
//! before the module they are used in (one of the few cases of declaration
//! order-dependence in Rust).

#[macro_export]
macro_rules! memdev_fields {
    ($ty:ident {
        $($start:literal $(..= $end:literal)? => $target:tt,)*
    }) => {
        impl $crate::memdev::MemDevice for $ty {
            fn read(&self, addr: $crate::memdev::Addr) -> u8 {
                match addr.relative() {
                    $($start $(..= $end)? => memdev_fields!(@reader: self, addr.offset_by($start), $target),)*
                    _ => panic!(concat!("Address {} out of range for ", stringify!($ty)), addr),
                }
            }

            fn write(&mut self, addr: $crate::memdev::Addr, val: u8) {
                match addr.relative() {
                    $($start $(..= $end)? => memdev_fields!(@writer: self, addr.offset_by($start), val, $target),)*
                    _ => panic!(concat!("Address {} out of range for ", stringify!($ty)), addr),
                }
            }
        }
    };

    (@reader: $self:ident, $addr:expr, $target:ident) => {
        $crate::memdev::MemDevice::read(&$self.$target, $addr)
    };
    (@reader: $self:ident, $addr:expr, $target:literal) => {
        $target
    };
    (@reader: $self:ident, $addr:expr, { $target:ident, skip_over: $skip:literal }) => {
        $crate::memdev::MemDevice::read(&$self.$target, $addr.skip_over($skip))
    };
    (@reader: $self:ident, $addr:expr, { $target:ident, readonly }) => {
        memdev_fields!(@reader: $self, $addr, $target)
    };

    (@writer: $self:ident, $addr:expr, $val:ident, $target:ident) => {
        $crate::memdev::MemDevice::write(&mut $self.$target, $addr, $val)
    };
    (@writer: $self:ident, $addr:expr, $val:ident, $target:literal) => {
        ()
    };
    (@writer: $self:ident, $addr:expr, $val:ident, { $target:ident, skip_over: $skip:literal }) => {
        $crate::memdev::MemDevice::write(&mut $self.$target, $addr.skip_over($skip), $val)
    };
    (@writer: $self:ident, $addr:expr, $val:ident, { $target:ident, readonly }) => {
        ()
    };

    (@addr: $base:ident, $addr:tt) => { $base.offset_by($addr) };
    (@addr: $base:ident, $addr:literal..=$end:literal) => { $base.offset_by($addr) };
}
