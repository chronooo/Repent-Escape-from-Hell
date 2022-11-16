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

    lda     #02
    ldx     #0
whitescreen
    STA     $1e00,X
    INX     
    cpx     #$FF
    BNE     whitescreen
    STA     $1e00,X

    lda     #02
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

is_a                        ; if a was pressed, decrease frame

    ldx     #0              ; init counter to to loop though 8 byte character
ror_loop

    lda     $1c10,X
    and     #%00000001      ; bit mask testing for last bit of the byte
    bne     ror_set         ; if zero flag is 0, need to set carry, else fall through

    CLC                     ; clear carry cause bit was 0 (else)   
    jmp     ror_p2
ror_set                     ; set carry cause bit was 1 (if)
    SEC

ror_p2
    ror     $1c10,X
    INX     
    cpx     #8
    bne     ror_loop
    jmp     printchar

is_d                        ; if d was pressed, decrease frame

    ldx     #0              ; init counter to to loop though 8 byte character
rol_loop

    lda     $1c10,X
    and     #%10000000      ; bit mask testing for last bit of the byte
    bne     rol_set         ; if zero flag is 0, need to set carry, else fall through

    CLC                     ; clear carry cause bit was 0 (else)   
    jmp     rol_p2
rol_set                     ; set carry cause bit was 1 (if)
    SEC

rol_p2
    rol     $1c10,X
    INX     
    cpx     #8
    bne     rol_loop
    jmp     printchar

printchar

    ldy     $0
waste_time_loop
    INY

;     ldx     $0
; waste_time_inner
;     INX 
;     CPX     #$FF
;     BNE     waste_time_inner

    CPY     #$FF             ; waste time by counting up to 255 in Y reg
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
    dc.b    %00000000       ; 0x1c10
    dc.b    %11111110       ; 0x1c11
    dc.b    %11000010      ; 0x1c12
    dc.b    %11000010       ; 0x1c13
    dc.b    %11000010       ; 0x1c14
    dc.b    %11000010       ; 0x1c15
    dc.b    %11111110       ; 0x1c16
    dc.b    %11111111       ; 0x1c17
