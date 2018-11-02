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
  ; skip $0200, because it's where we write sprite data? TODO
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
; Load palette info
; In order to write to the PPU, we have to use ports/registers on the CPU; so to
;   tell the CPU that we'd like to start writing to $3F00 (palette registers) on
;   the PPU, we have to talk to $2006 on the CPU... twice, because we can only
;   write a byte at a time. So we write the first part of the register, then
;   the second, then shove a bunch of info into $2007.
; ref: https://wiki.nesdev.com/w/index.php/PPU_registers#PPUSTATUS
; ref: https://wiki.nesdev.com/w/index.php/PPU_registers#PPUADDR
loadPalette:
  lda $2002                   ; Read to reset the latch on $2006...
  lda #$3F                    ; 
  sta $2006                   ; $3F...
  lda #$00                    ; 
  sta $2006                   ; ...00

;------------------------------------------------------------------------------;
; Brother may I have some loops
; The pseudo-code here really says it all, but we're going to push 32 bytes from
;   our stored 'palette' variable into $2007, which (per above) should write
;   into $3F00-$3F1F on the PPU
  ldx #$00                    ; x = 0
loadPaletteLoop:
  lda palette, x              ; a = palette[x]
  sta $2007                   ; a => $2007
  inx                         ; x++
  cpx #$20                    ; x==32 ?
  bne loadPaletteLoop         ;   jmp loadPaletteLoop

;------------------------------------------------------------------------------;
; Spritefall
; Alright, time to push data into sprite land.  Each sprite requires 4 bytes of
;   info to write:
; $0200, $0204...  Y Position (#00-#EF)
; $0201, $0205...  Tile Number (from the Pattern Table)
; $0202, $0206...  Attributes (see below)
; $0203, $0207...  X Position (#00-#F9)
;
; Attributes:
; +-------- Flip sprite vertically
; |+------- Flip sprite horizontally
; ||+------ ? In front of background : Behind background
; |||   ++- Color palette of sprite (0-3)
; 765---10
;
  lda #$00                    
  sta $0201                   ; Tile 0
  sta $0202                   ; Palette 0, Behind BG
  lda #$80                    
  sta $0200                   ; Middle of the screen (Y)
  sta $0203                   ; Middle of the screen (X)

;------------------------------------------------------------------------------;
; Settings
  lda #%10000000              ; Enable NMI, use Pattern Table 0
  sta $2000

  lda #%00010000              ; Enable sprites
  sta $2001

forever:
  jmp forever

; Looks like this just copies $0200-$02FF into the PPU through OAM? TODO
NMI:
  lda #$00
  sta $2003
  lda #$02
  sta $4014

  rti                         ; Immediately exit the interrupt

;MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM;
  .bank 1

;==============================================================================;
  .org $E000
palette:
  .db $30,$21,$22,$12,$30,$23,$14,$04,$30,$25,$16,$07,$30,$29,$2A,$1B
  .db $30,$21,$22,$12,$30,$23,$14,$04,$30,$25,$16,$07,$30,$29,$2A,$1B

;==============================================================================;
  .org $FFFA                  ; Memory address for the first vector

  .dw NMI                     ; Non-Mistakable Interrupt
                              ; ref: https://wiki.nesdev.com/w/index.php/NMI

  .dw RESET                   ; First jump when the machine turns on

  .dw 0                       ; External interrupt request (currently unused)

;MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM;
  .bank 2
  .org $0000

  .incbin "mario.chr"