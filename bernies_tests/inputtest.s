    processor 6502

CHROUT = $ffd2
GETIN = $ffe4
SCNKEY = $ff9f
CURKEY = $00c5

    org     $1001

    dc.w    stubend
    dc.w    12345
    dc.b    $9e, "4109", 0
stubend 
    dc.w    0

start
    lda     CURKEY
    cmp     #9 ; W
    beq     printw
    cmp     #17 ; A
    beq     printa
    cmp     #41 ; S
    beq     prints
    cmp     #18 ; D
    beq     end

    jmp     start

printw
    lda     #'W
    jsr     CHROUT
    jmp     start

printa
    lda     #'A
    jsr     CHROUT
    jmp     start

prints
    lda     #'S
    jsr     CHROUT
    jmp     start

end