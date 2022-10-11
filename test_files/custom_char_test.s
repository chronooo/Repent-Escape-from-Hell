    processor 6502

;   KERNAL [sic] routines

CHROUT =    $ffd2
CHRIN  =    $ffcf

    org     $1001
    dc.w    stubend
    dc.w    12345
    dc.b    $9e, "4109", 0 
stubend
    dc.w    0

start

    lda     #00
    ldx     #0
whitescreen
    STA     $1e00,X
    INX     
    cpx     #$FF
    BNE     whitescreen
    STA     $1e00,X

    lda     #01
    ldx     #0
whitescreen2
    STA     $1f00,X
    INX     
    cpx     #$FF
    BNE     whitescreen2
    STA     $1f00,X

;   Switching character set pointer to 0x1c00:
    lda     #255
    STA     $9005 ; POKE 36869 255 (from book)

;   Next, fill 0x9600 - 0x9700 (color RAM) with $00 

;   loop to fill 0x9600 - 0x96FF with FF:
    lda     #$00
    ldx     #$00
color_ram 
    STA     $9600,X
    INX   
    cpx     #255
    BNE     color_ram
    STA     $9600,X

;   loop to fill 0x9700 - 0x97FF with FF:
    lda     #$00
    ldx     #0
color_ram1
    STA     $9700,X
    INX   
    cpx     #255
    BNE     color_ram1
    STA     $9700,X

;   Now, all the pixels on the display are enabled

loop
    lda     $00C5           ; loads the current pressed key from memory
    cmp     #64
    beq     loop
    cmp     #17 
    beq     is_a            ; if A is pressed
    cmp     #18 
    beq     is_d            ; if D is pressed
    cmp     #33
    beq     exit_prg
    jmp     loop

exit_prg
    rts

is_a
    lda     #00             ; prints character at 0x1c00
    jmp     printchar
is_d
    lda     #01             ; prints character at 0x1c08
    jmp     printchar

printchar
    STA     $1e00           ; stores the character selected to 
                            ; first byte of screen memory
    jmp     loop

    org     $1c00

    ;       CHAR 0
    dc.b    %11111111    
    dc.b    %11111111
    dc.b    %11000011
    dc.b    %11000011 
    dc.b    %11000011
    dc.b    %11000011
    dc.b    %11111111 
    dc.b    %11111111

    ;       CHAR 1
    dc.b    %00000000    
    dc.b    %00000000 
    dc.b    %00111100
    dc.b    %00111100
    dc.b    %00111100
    dc.b    %00111100
    dc.b    %00000000    
    dc.b    %00000000