;; Home bank -- starts in ROM0.
SECTION "Start", ROM0
; $0000 - $003F: RST handlers.
ret
DS 7
; $0008
ret
DS 7
; $0010
ret
DS 7
; $0018
ret
DS 7
; $0020
ret
DS 7
; $0028
ret
DS 7
; $0030
ret
DS 7
; $0038
ret
DS 7

; $0040 - $0067: Interrupt handlers.
jp draw
DS 5
; $0048
jp stat
DS 5
; $0050
jp timer
DS 5
; $0058
jp serial
DS 5
; $0060
jp joypad
DS 5

; $0068 - $00FF: Free space.
DS $98

; $0100 - $0103: Startup handler.
nop
jp main

; $0104 - $0133: The Nintendo Logo.
DB $CE, $ED, $66, $66, $CC, $0D, $00, $0B
DB $03, $73, $00, $83, $00, $0C, $00, $0D
DB $00, $08, $11, $1F, $88, $89, $00, $0E
DB $DC, $CC, $6E, $E6, $DD, $DD, $D9, $99
DB $BB, $BB, $67, $63, $6E, $0E, $EC, $CC
DB $DD, $DC, $99, $9F, $BB, $B9, $33, $3E

; $0134 - $013E: The title, in upper-case letters, followed by zeroes.
DB "TEST"
DS 7 ; padding

; $013F - $0142: The manufacturer code.
DS 4

; $0143: Gameboy Color compatibility flag.
GBC_UNSUPPORTED EQU $00
GBC_COMPATIBLE EQU $80
GBC_EXCLUSIVE EQU $C0
DB GBC_UNSUPPORTED

; $0144 - $0145: "New" Licensee Code, a two character name.
DB "OK"

; $0146: Super Gameboy compatibility flag.
SGB_UNSUPPORTED EQU $00
SGB_SUPPORTED EQU $03
DB SGB_UNSUPPORTED

; $0147: Cartridge type. Either no ROM or MBC5 is recommended.
CART_ROM_ONLY EQU $00
CART_MBC1 EQU $01
CART_MBC1_RAM EQU $02
CART_MBC1_RAM_BATTERY EQU $03
CART_MBC2 EQU $05
CART_MBC2_BATTERY EQU $06
CART_ROM_RAM EQU $08
CART_ROM_RAM_BATTERY EQU $09
CART_MMM01 EQU $0B
CART_MMM01_RAM EQU $0C
CART_MMM01_RAM_BATTERY EQU $0D
CART_MBC3_TIMER_BATTERY EQU $0F
CART_MBC3_TIMER_RAM_BATTERY EQU $10
CART_MBC3 EQU $11
CART_MBC3_RAM EQU $12
CART_MBC3_RAM_BATTERY EQU $13
CART_MBC4 EQU $15
CART_MBC4_RAM EQU $16
CART_MBC4_RAM_BATTERY EQU $17
CART_MBC5 EQU $19
CART_MBC5_RAM EQU $1A
CART_MBC5_RAM_BATTERY EQU $1B
CART_MBC5_RUMBLE EQU $1C
CART_MBC5_RUMBLE_RAM EQU $1D
CART_MBC5_RUMBLE_RAM_BATTERY EQU $1E
CART_POCKET_CAMERA EQU $FC
CART_BANDAI_TAMA5 EQU $FD
CART_HUC3 EQU $FE
CART_HUC1_RAM_BATTERY EQU $FF
DB CART_ROM_RAM

; $0148: Rom size.
ROM_32K EQU $00
ROM_64K EQU $01
ROM_128K EQU $02
ROM_256K EQU $03
ROM_512K EQU $04
ROM_1024K EQU $05
ROM_2048K EQU $06
ROM_4096K EQU $07
ROM_1152K EQU $52
ROM_1280K EQU $53
ROM_1536K EQU $54
DB ROM_32K

; $0149: Ram size.
RAM_NONE EQU $00
RAM_2K EQU $01
RAM_8K EQU $02
RAM_32K EQU $03
DB RAM_2K

; $014A: Destination code.
DEST_JAPAN EQU $00
DEST_INTERNATIONAL EQU $01
DB DEST_INTERNATIONAL
; $014B: Old licensee code.
; $33 indicates new license code will be used.
; $33 must be used for SGB games.
DB $33
; $014C: ROM version number
DB $00
; $014D: Header checksum.
; Assembler needs to patch this.
DB $00
; $014E- $014F: Global checksum.
; Assembler needs to patch this.
DW $0000

; $0150: Code!
main:
    ei
    nop
    di
    ld sp,$dfff
call ldinstrs
.loop:
    call doprint
    jr .loop

doprint:
    ld a,$08
    ld [$ffff],a
    ld hl,hello
    ld a,13
    call serial
.loop:
    halt
    ld b,a
    ld a,[$ffff]
    cp a,0
    ld a,b
    jr nz,.loop
    ret

draw:
stat:
timer:
joypad:
    reti
serial:
    dec a
    push af
    ld a,[hli]
    ld [$ff01],a
    ld a,$81
    ld [$ff02],a
    pop af
    jr z,.done
    reti
.done:
    ld a,0
    ld [$ffff],a
    reti

hello:
DB "Hello World",10

ldinstrs:
    ldh a,[$ff00]
    ld a,[$feff]
    ldh a,[c]
    ld a,a
    ld a,b
    ret
