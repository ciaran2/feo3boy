;; Compute all 8 bit perfect squares.
SECTION "Main", ROM0
Squares:
	ld hl, Output
	ld b, 1

MACRO MulBit
	: sla c
	bit \1, b
	jr z, :+
	add a, c
ENDM

.loop:
	ld a, b
	;; 15*15 is the largest perfect square that fits in u8, so stop when b = 16.
	cp a, 16
	jp z, .done

	;; There are no multiplication instructions so we have to do long
	;; multiplication.
	ld c, b
	;; A is already B << 0, so set it back to 0 if the 0th bit doesn't match.
	bit 0, b
	jr nz, :+
	ld a, 0
	MulBit 1
	MulBit 2
	MulBit 3
	MulBit 4
	MulBit 5
	MulBit 6
	MulBit 7

	: ld [hli], a
	inc b
	jp .loop

.done:
	halt

SECTION "Output", WRAM0[$C000]
Output:
