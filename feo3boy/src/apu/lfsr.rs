/// This table contains the LFSR sequence for 15-bit mode. Each entry in the table is the bit
/// pattern for the LFSR indexed by the number of times the LFSR has been updated, minus one. So
/// after the first update, the LFSR value (before shifting) is `LFSR15_PATTERN[0]`, and the bit
/// output is `LFSR15_PATTERN[0] & 1`.
pub(super) static LFSR15_PATTERN: [u16; 0x7fff] = include!("lfsr15.txt");

/// Reverse lookup table for the position in the LFSR. To find the current position in the pattern
/// table for a given value, shift it to the right by one and index it here. That will tell you the
/// equivalent index in LFSR15_PATTERN. Note that 0xfffe and 0xffff are not covered: those result in
/// the LFSR locking up and are not part of the normal pattern.
pub(super) static LFSR15_REVERSE_LOOKUP: [usize; 0x7fff] = include!("lfsr15_rev.txt");

/// This table contains the LFSR sequence for 7-bit mode. Each entry in the table is the bit
/// pattern for the LFSR indexed by the number of times the LFSR has been updated, minus one. So
/// after the first update, the LFSR value (before shifting) is `LFSR15_PATTERN[0]`, and the bit
/// output is `LFSR15_PATTERN[0] & 1`.
pub(super) static LFSR7_PATTERN: [u16; 0x7f] = include!("lfsr7.txt");

/// Reverse lookup table for the position in the LFSR. To find the current position in the pattern
/// table for a given value, shift it to the right by one and index it here. That will tell you the
/// equivalent index in LFSR7_PATTERN. Note that 0xfe and 0xff are not covered: those result in
/// the LFSR locking up and are not part of the normal pattern.
pub(super) static LFSR7_REVERSE_LOOKUP: [usize; 0x7f] = include!("lfsr7_rev.txt");
