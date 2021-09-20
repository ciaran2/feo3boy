TARGETS := $(patsubst %.asm,%.bin,$(wildcard *.asm))

.PHONY: all

all: $(TARGETS)

%.bin: %.asm
	rgbasm $< -o - | rgblink -w -o $@ -
