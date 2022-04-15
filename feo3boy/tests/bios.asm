SECTION "BIOS", ROM0
jp exit
DS $f9
exit:
ld a, 1
ldh [$ff50], a
