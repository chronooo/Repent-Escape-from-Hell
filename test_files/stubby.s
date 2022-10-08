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

    lda     #01
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
    