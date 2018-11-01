  .inesprg 1                  ; 1x 16KiB PRG code
  .ineschr 1                  ; 1x  8KiB CHR data
  .inesmap 0                  ; No bank swapping
  .inesmir 1                  ; background mirroring

;MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM;
  .bank 0
  .org $C000

;==============================================================================;
; Init code
; ref: https://wiki.nesdev.com/w/index.php/Init_code

RESET:
  sei                         ; Disable interrupt requests
  cld                         ; Disable decimal mode 
  ldx #$40
  stx $4017                   ; Disable APU frame interrupt requests
  ldx #$FF
  txs                         ; Set up stack
  inx                         ; X overflows to #$00
  stx $2000                   ; #$00 => $2000; disable NMI
  stx $2001                   ; #$00 => $2001; disable rendering
  stx $4010                   ; #$00 => $4010; disable DMC interrupt requests

;------------------------------------------------------------------------------;
; Wait for the vblank to make sure PPU is ready
vblankwait1:
; Here's a little lesson in trickery: $2002 is PPUSTATUS - the 7th bit indicates
;   whether or not the PPU is in vblank (0=no, 1=yes). The BIT operand copies
;   the 7th bit in memory to the 'Negative' (N) flag (because that's what it 
;   normally represents). BPL branches if the N flag is set to zero, and so this
;   loop will only break if we get a 1 back, indicating the PPU is in vblank.
; ref: http://www.obelisk.me.uk/6502/reference.html#BIT
; ref: https://wiki.nesdev.com/w/index.php/PPU_registers#PPUSTATUS
  bit $2002
  bpl vblankwait1             ; Loop

;------------------------------------------------------------------------------;
; Clear the memory
; The nesdev wiki init code page says to skip $0200, because RAM page 2 is used
;   for the display list to be copied to OAM, and if it's not initialized to
;   #EF-FF, you'll get a bunch of garbage sprites at (0,0).  That being said...
;   the example I followed initialized $0300 to #FE instead.  Consider this a
;   TODO, I guess.
  ldx #$00
  txa
clrmem:
  sta $0000, x
  sta $0100, x
  ; skip $0200
  sta $0300, x
  sta $0400, x
  sta $0500, x
  sta $0600, x
  sta $0700, x

  inx
  bne clrmem

;------------------------------------------------------------------------------;
; Second loop to wait for PPU spinup
vblankwait2:
  bit $2002
  bpl vblankwait2

;==============================================================================;

  lda #%10000000              ; Intensify blues
  sta $2001

forever:
  jmp forever:

NMI:
  rti                         ; Immediately exit the interrupt

;MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM;
  .bank 1
  .org $FFFA                  ; Memory address for the first vector

  .dw NMI                     ; Non-Mistakable Interrupt
                              ; ref: https://wiki.nesdev.com/w/index.php/NMI

  .dw RESET                   ; First jump when the machine turns on

  .dw 0                       ; External interrupt request (currently unused)

;MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM;
  .bank 2
  .org $0000

  .incbin "mario.chr"