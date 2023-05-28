#!/usr/bin/env python3

def lfsr_next15(lfsr):
    """Compute the next value of the LFSR in 15 bit mode."""
    bit = (~((lfsr & 0b01) ^ ((lfsr & 0b10) >> 1))) & 1
    return ((lfsr & 0x7fff) | (bit << 15)) >> 1

def lfsr_next7(lfsr):
    """Compute the next value of the LFSR in 7 bit mode.

    This will also set bits in the 15-bit space.
    """
    bit = (~((lfsr & 0b01) ^ ((lfsr & 0b10) >> 1))) & 1
    return ((lfsr & 0x7f7f) | (bit << 15) | (bit << 7)) >> 1

def build_pattern(lfsr_next, *, match_bits = 0x7fff):
    """Compute the pattern for the given LFSR function.

    `match_bits` is the set of bits that are used to determine whether the sequence has looped.

    Once the pattern loops, the `lfsr_next` function will be reapplied to the first several patterns
    until it reaches the first pattern where reapplying does not change the pattern.

    The reason we have reapplication is because when we want to switch from 7-bit mode to 15-bit
    mode, we need to reconstruct the 8 extra bits of the 15-bit pattern. Those upper 8 bits are
    predictable: they're identical to the 7 bits of the 7 bit pattern + the last bit that was
    shifted out of the 7 bit pattern. That last bit shifted out is the problem, since it's gone and
    we would have to look backwards through the pattern to find it again. So instead, we just make
    sure that the 7 bit pattern values include the correct upper 8 bits of 15 bit mode.

    When first starting in 7-bit mode, the upper bits of 15-bit mode will not match what is in the
    table, since there are multiple 15-bit patterns that have the same 7-bit value. To cover this
    case, the LFSR based on the table must save the upper bits of the 15-bit mode whenever it
    switches to 7-bit mode and then reapply them (with appropriate shift) when switching back. This
    reapplication covers those first 8 cycles of 7-bit mode where the 15-bit pattern is not fully
    predictable.
    """
    pat = []
    seen = set()
    lfsr = 0
    while True:
        if (lfsr & match_bits) not in seen:
            seen.add(lfsr & match_bits)
            pat.append(lfsr)
            lfsr = lfsr_next(lfsr)
        else:
            break

    reapplied_times = 0
    for i in range(len(pat)):
        if pat[i] & match_bits != lfsr & match_bits:
            raise ValueError(f"Mismatched pattern: {pat[i]:x} and lfsr: {lfsr:x} at {i}")
        if pat[i] == lfsr:
            break
        reapplied_times += 1
        pat[i] = lfsr
        lfsr = lfsr_next(lfsr)

    print(f"Pat len: {len(pat)}, first: {pat[0]}, last: {pat[-1]}, reapplied: {reapplied_times}")
    return pat

def print_sequence(seq, *, file=None, per_line=10):
    """Print a sequence of numbers in lines of 10 values in hex format separated by commas."""
    print('[', file = file)
    for line_start in range(0, len(seq), per_line):
        print('    ', file = file, end = '')
        for idx in range(line_start, min(line_start + per_line, len(seq))):
            print(f"0x{seq[idx]:0>4x}", end = ", ", file = file)
        print(file = file)
    print(']', file = file)

def build_inverse_lookup(pat, *, match_bits = 0x7fff):
    """Compute the index lookup table for the given pattern."""
    # Create a dictionary mapping from the values in the pattern (truncated to the relevant part to
    # match) to the index where they appear in the pattern.
    rev = {pat[i] & match_bits: i for i in range(len(pat))}
    # Turn the reverse mapping into a table by effectively sorting it. We do this by selecting the
    # pattern-index mapping values in order.
    rev = [rev[i] for i in range(len(pat))]
    # validate the reverse mapping.
    for i in range(len(pat)):
        if (pat[rev[i]] & match_bits) != i:
            raise ValueError(f"Pattern value at {i} doesn't match from reverse lookup mapping")
    return rev

def main():
    pat15 = build_pattern(lfsr_next15)
    rev15 = build_inverse_lookup(pat15)
    pat7 = build_pattern(lfsr_next7, match_bits = 0x007f)
    rev7 = build_inverse_lookup(pat7, match_bits = 0x007f)

    with open('lfsr15.txt', 'w') as lfsr15:
        print_sequence(pat15, file=lfsr15)
    with open('lfsr15_rev.txt', 'w') as lfsr15_rev:
        print_sequence(rev15, file=lfsr15_rev)

    with open('lfsr7.txt', 'w') as lfsr7:
        print_sequence(pat7, file=lfsr7)
    with open('lfsr7_rev.txt', 'w') as lfsr7_rev:
        print_sequence(rev7, file=lfsr7_rev)


if __name__ == '__main__':
    main()
