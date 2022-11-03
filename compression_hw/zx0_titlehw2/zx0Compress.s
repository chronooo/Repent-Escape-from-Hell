    processor 6502

; KERNAL [sic] routines
CHROUT =    $ffd2
CHRIN  =    $ffcf
W =         $20

    org     $1001

    dc.w    stubend
    dc.w    12345
    dc.b    $9e, "4109", 0 

stubend
    dc.w    0

start

    ;setup screen
    jsr     screen_init ; initializing colors on the screen
    jsr     full_decomp ; calling the decompressor
    jsr     loop    ; waiting for the user to press z

screen_init
    lda     #$02 
    ldx     #0
color_ram1 ; fill color ram 0x9600 to 0x96ff with red (02)
    STA     $9600,X
    INX
    cpx     #255
    BNE     color_ram1
    STA     $9600,X
    lda     #$00
    ldx     #0
color_ram2 ; fill color ram 0x9700 to 0x97ff with black (00)
    STA     $9700,X
    INX
    cpx     #255
    BNE     color_ram2
    STA     $9700,X
    lda     #$00
    ldx     #0
    rts

loop
    lda     $00C5
    cmp     #64
    beq     loop
    cmp     #33 ; Z in current key table
    beq     exit_prg
    jmp     loop

exit_prg
    rts

    include "zx0-6502.s"


    org     $1a00 ; zx0 compressed screen
zx0screen
    HEX 2f 20 fc f5 12 05 10 05 0e 14 3a 53 ec 82 01 0e
    HEX 88 05 13 03 01 b0 20 06 12 0f 0d a1 08 05 0c 0c
    HEX 3f 01 a8 a0 10 12 05 13 cb 20 1a 20 aa 9a aa a0
   