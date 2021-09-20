;; Compute all 8 bit fibonacci numbers.
SECTION "Main", ROM0
Fibonacci:
	ld hl, Output
	ld a, 0
	ld c, 1

.loop:
	ld [hli], a
	jr c, .overflow
	add a, c
	ld b, a
	ld a, c
	ld c, b
	jr .loop

.overflow:
	halt

SECTION "Output", WRAM0[$C000]
Output:
