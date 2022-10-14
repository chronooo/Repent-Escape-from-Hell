    processor 6502

; KERNAL [sic] routines
CHROUT =    $ffd2
CHRIN  =    $ffcf

    org     $1001

    dc.w    stubend
    dc.w    12345
    dc.b    $9e, "4109", 0 



stubend
    dc.w    0

start

    lda     #32
    ldx     #0
whitescreen
    STA     $1e00,X
    INX     

    cpx     #255
    BNE     whitescreen

    lda     #$00
    ldx     #0
whitescreen2
    STA     $1f00,X
    INX     

    cpx     #255
    BNE     whitescreen2

    lda     #$00
    ldx     #0

    lda     #255
    STA     $9005 ; POKE 36869 255
    ;       now we should have our character set at 7168, or 0x1c00

    lda     #$55 ; 01010101
    ldx     #0
fillchar ; loop to fill 1c08 - 1c10 with FF
    STA     $1c08,X
    INX   
    cpx     #8
    BNE     fillchar

    lda     #$AA ; 10101010 
    ldx     #0
equalschar ; loop to fill 1c20 - 1c28 with AA
    STA     $1c20,X
    INX   
    cpx     #8
    BNE     equalschar

    lda     #$00 
    ldx     #0
colorRamFill ; fill color ram 0x9600 to 0x96ff with black (00)
    STA     $9600,X
    INX
    cpx     #255
    BNE     colorRamFill
    STA     $9600,X
    lda     #$00
    ldx     #0

colorRamFill2 ; fill color ram 0x9700 to 0x97ff with black (00)
    STA     $9700,X
    INX
    cpx     #255
    BNE     colorRamFill2
    STA     $9700,X
    lda     #$00
    ldx     #0

loop
    lda     $00C5
    cmp     #64
    beq     loop
    cmp     #17 
    beq     is_a
    cmp     #18 
    beq     is_d
    cmp     #33
    beq     exit_prg
    jmp     loop

is_a
;    lda     #65 
    lda     #01
    jmp     printchar
is_d
;    lda     #68 
    lda     #04
    jmp     printchar

printchar
;    jsr     CHROUT
    STA     $1e00
    jmp     loop


exit_prg
    rts
    
