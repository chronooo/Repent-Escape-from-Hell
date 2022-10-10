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

    lda     #00
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

    ldx     #2               ; we will use x register as frame counter / character set index here

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

is_a                        ; if a was pressed, decrease frame

    DEX     
    CPX     #$1             ; compares to 1, if 1 then wrap to 9
    BNE     printchar
    ldx     #$9            
    jmp     printchar

    ;lda     #00            ; prints character at 0x1c00
    ;jmp     printchar
is_d                        ; if d was pressed, decrease frame

    INX
    CPX     #$A             ; compares to 10, if 10 then wrap to 2
    BNE     printchar
    ldx     #$2     
    jmp     printchar

    ;lda     #01             ; prints character at 0x1c08
    ;jmp     printchar

printchar
    STX     $1e08           ; character set offset in X, into screen memory location (arbitary)
                            ; first byte of screen memory
                            ; $1e00 is first byte of screen memory
    sty     $0
waste_time_loop
    INY
    CPY     $FF             ; waste time by counting up to 255 in Y reg
    BNE     waste_time_loop

    jmp     loop            ; go to top of while loop


/*
------------------
CHARACTER SET DATA:
------------------ 
*/
    org     $1c00

    ;       CHAR 00         ; all empty
    ds      8, $00          ; declares 8 bytes of value 0x00
    ;       CHAR 01         ; all filled
    ds      8, $FF          ; declares 8 bytes of value 0xFF

    ;       CHAR 02
    dc.b    %00000000    
    dc.b    %00000000 
    dc.b    %00111100
    dc.b    %00111100
    dc.b    %00111100
    dc.b    %00111100
    dc.b    %00000000    
    dc.b    %00000000

    ;       CHAR 03
    dc.b    %00000000    
    dc.b    %00000000 
    dc.b    %01111000
    dc.b    %01111000
    dc.b    %01111000
    dc.b    %01111000
    dc.b    %00000000    
    dc.b    %00000000

    ;       CHAR 04
    dc.b    %00000000    
    dc.b    %00000000 
    dc.b    %11110000
    dc.b    %11110000
    dc.b    %11110000
    dc.b    %11110000
    dc.b    %00000000    
    dc.b    %00000000

    ;       CHAR 05
    dc.b    %00000000    
    dc.b    %00000000 
    dc.b    %11100001
    dc.b    %11100001
    dc.b    %11100001
    dc.b    %11100001
    dc.b    %00000000    
    dc.b    %00000000

    ;       CHAR 06
    dc.b    %00000000    
    dc.b    %00000000 
    dc.b    %11000011
    dc.b    %11000011
    dc.b    %11000011
    dc.b    %11000011
    dc.b    %00000000    
    dc.b    %00000000

    ;       CHAR 07
    dc.b    %00000000    
    dc.b    %00000000 
    dc.b    %10000111
    dc.b    %10000111
    dc.b    %10000111
    dc.b    %10000111
    dc.b    %00000000    
    dc.b    %00000000

    ;       CHAR 08
    dc.b    %00000000    
    dc.b    %00000000 
    dc.b    %00001111
    dc.b    %00001111
    dc.b    %00001111
    dc.b    %00001111
    dc.b    %00000000    
    dc.b    %00000000

    ;       CHAR 09
    dc.b    %00000000    
    dc.b    %00000000 
    dc.b    %00011110
    dc.b    %00011110
    dc.b    %00011110
    dc.b    %00011110
    dc.b    %00000000    
    dc.b    %00000000
