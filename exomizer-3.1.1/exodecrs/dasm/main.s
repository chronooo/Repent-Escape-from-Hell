; -------------------------------------------------------------------
; this file is intended to be assembled with dasm.
; It has not been tested with any other assemblers or linkers.
; -------------------------------------------------------------------
; -------------------------------------------------------------------
; example usage of the standard decruncher
; this program decrunches data to memory
; -------------------------------------------------------------------
; if decrunching forwards then the following line must be uncommented.
;DECRUNCH_FORWARDS = 1
 IFNCONST DECRUNCH_FORWARDS
DECRUNCH_FORWARDS = 0
 ENDIF
; -------------------------------------------------------------------
 PROCESSOR 6502
 ORG $0801
        .byte $0b,$08,<2019,>2019,$9e,$32,$30,$36,$31,0,0,0
; -------------------------------------------------------------------
; we begin here
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
        lda $ffff               ; needs to be set correctly before (placeholder)
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
 INCLUDE exodecrunch.s
