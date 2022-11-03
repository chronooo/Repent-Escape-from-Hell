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
    jsr     loop 
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

loop
    lda     $00C5
    cmp     #64
    beq     loop
    cmp     #33 ; Z in current key table
    beq     exit_prg
    jmp     loop

exit_prg
    rts

    org     $1a00 ; Exomizer compressed screen

    HEX 00 1e 20 20 80 00 ae 40 82 04 c1 57 b0 00 00 00
    HEX 00 22 68 06 e0 00 00 00 00 08 80 6c e0 04 02 20
    HEX 0b a0 ea 12 05 10 71 0e 14 3a de 6a 01 0e f6 05
    HEX 13 03 36 ef 06 12 0f 0d 5e 08 bd 0c 0b 80 61 10
    HEX 2f a1 13 a0 1a 5f 00 00 00 80 00 01    