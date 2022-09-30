    processor 6502

; KERNAL [sic] routines
CHROUT = $ffd2

    org     $1001

    dc.w    stubend
    dc.w    12345
    dc.b    $9e, "4109", 0

stubend
    dc.w    0

start
    lda     #'X
    jsr     CHROUT
    rts