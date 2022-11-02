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

    ;setup screen
    jsr     screen_init
    jsr     exod_get_crunched_byte ;
    ;exod_get_crunched_byte is how it gets the information
    ;exod_zp_bitbuf is the load address
    ;try recrunching code with mem to set a load address
    ;make sure to use right flag to set crunching forward/backwards
    ;and adjust forwards/backwards flag accordingly
    /*tldr is that my assumption was correct that its essentially
    a low level api call and I need to set a load address and return address
    theres a ffff somewhere that was a placeholder that i need to change to be
    the return address iirc*/
    
; if decrunching forwards then the following line must be uncommented.
;DECRUNCH_FORWARDS = 1
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
        lda $1a00             /* needs to be set correctly before 
        use to be $ffff as a placeholder, and i think this is where it begins
        reading bytes from*/
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

screen_init
    lda     #$02 
    ldx     #0
color_ram1 ; fill color ram 0x9600 to 0x96ff with red (02)
    STA     $9600,X
    INX
    cpx     #255
    BNE     color_ram1
    STA     $9600,X
    lda     #$00
    ldx     #0
color_ram2 ; fill color ram 0x9700 to 0x97ff with black (00)
    STA     $9700,X
    INX
    cpx     #255
    BNE     color_ram2
    STA     $9700,X
    lda     #$00
    ldx     #0
    rts

    org     $1a00 ; Exomizer compressed screen

    HEX 01 00 7c 2f 7c 2e 0e 14 3a d7 1d 0e aa db 03 01
    HEX 10 fc 5d 06 12 0f 0d 7f 08 05 0c bd 60 80 10 12
    HEX 05 1d 13 25 1a 7d 20 10 02 00 00 02 04 a0 24 e0
    HEX 0a a0 04 00 00 c0 4a 02 6a 00 00 00 00 00 b0 57
    HEX c1 44 48 ae 00 80       