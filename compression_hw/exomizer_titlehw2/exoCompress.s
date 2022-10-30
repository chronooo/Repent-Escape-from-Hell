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
; if decrunching forwards then the following line must be uncommented.
DECRUNCH_FORWARDS = 1
 IFNCONST DECRUNCH_FORWARDS
DECRUNCH_FORWARDS = 0
 ENDIF
; -------------------------------------------------------------------
; we begin here (this is code from main.s from the dasm decruncher exomizer)
; -------------------------------------------------------------------
 IF DECRUNCH_FORWARDS == 0
        lda $04
        sta _byte_lo
        lda $05
        sta _byte_hi
 ELSE
        lda $02
        sta _byte_lo
        lda $03
        sta _byte_hi
 ENDIF
        jmp exod_decrunch
; -------------------------------------------------------------------
exod_get_crunched_byte:
 IF DECRUNCH_FORWARDS == 0
        lda _byte_lo
        bne _byte_skip_hi
        dec _byte_hi
_byte_skip_hi:
        dec _byte_lo
 ENDIF
_byte_lo = * + 1
_byte_hi = * + 2
        lda $ffff               ; needs to be set correctly before
 IF DECRUNCH_FORWARDS != 0
        inc _byte_lo
        bne _byte_skip_hi
        inc _byte_hi
_byte_skip_hi:
 ENDIF
        rts                     ; decrunch_file is called.
; end_of_data needs to point to the address just after the address
; of the last byte of crunched data.
; -------------------------------------------------------------------
 INCLUDE "exodecrunch.s"
; their code ends here

    org     $1a00 ; Exomizer compressed screen

    HEX 80 00 ae 40 82 04 c1 57 b0 00 00 00 00 22 68 06
    HEX e0 00 00 00 00 08 80 6c e0 04 02 20 0b c0 ea 12
    HEX 05 10 71 0e 14 3a de 6a 01 0e f6 05 13 03 36 ef
    HEX 06 12 0f 0d 5e 08 bd 0c 0b 80 61 10 2f a1 13 a0
    HEX 1a 5f 00 00 00 80 00 01    