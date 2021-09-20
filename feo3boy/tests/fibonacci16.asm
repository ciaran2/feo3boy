;; Compute all 8 bit fibonacci numbers.
SECTION "Main", ROM0
Fibonacci:
	ld hl, Output
	ld de, 0
	ld bc, 1

.loop:
	ld [hl], e
	inc hl
	ld [hl], d
	inc hl
	jr c, .overflow

	ld a, e
	add a, c
	ld e, c
	ld c, a

	ld a, d
	adc a, b
	ld d, b
	ld b, a

	jr .loop

.overflow:
	halt

SECTION "Output", WRAM0[$C000]
Output:
