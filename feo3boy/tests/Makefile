TARGETS := $(patsubst %.asm,%.bin,$(wildcard *.asm))

FULL_ROMS := testcart.gb

.PHONY: all

all: $(TARGETS) $(FULL_ROMS)

%.bin: %.asm
	rgbasm $< -o - | rgblink -w -o $@ -

bios.bin: bios.asm
	rgbasm $< -o - | rgblink -w -o - - | head -c 256 > $@

%.gb: %.bin
	cp $< $@
	rgbfix -p0 -v $@
