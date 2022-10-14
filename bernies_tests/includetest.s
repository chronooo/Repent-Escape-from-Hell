    processor 6502

CHROUT = $ffd2
GETIN = $ffe4
SCNKEY = $ff9f
CURKEY = $00c5
; Include and custom subroutine tests
; Program that tests includes and compiling with multiple files
; Press z to exit the title screen and jump into our input test
; press A W S to test keyboard inputs and D to close program
    org     $1001

    dc.w    stubend
    dc.w    12345
    dc.b    $9e, "4109", 0
stubend 
    dc.w    0
    jsr     whitesetup
    jsr     colorsetup
    jsr     printtitle
    jsr     whitesetup

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
    rts
    include "includetest1.s"