processor 6502

;   KERNAL [sic] routines
IDLE_TOP_1 = "12345"
IDLE_TOP_2 = "12345"
LEFT_TOP_1 = "12345"
LEFT_TOP_2 = "12345"
RIGHT_TOP_1 = "12345"
RIGHT_TOP_2 = "12345"
;   zero PAGE USage
;   20:current status(left/right/idle + frame/1/2/3)
;   2221:current position
CHROUT =    $ffd2
CHRIN  =    $ffcf

;store some basic initial data
    org     $
    dc.b
    org     $1001
    dc.w    stubend
    dc.w    12345
    dc.b    $9e, "4109", 0
stubend
    dc.w    0

main
    jsr     clear_screen

    ;charcterset to from 1c00
    lda     #255
    sta     $9005

    ;store spirit basic status
    lda     #$00
    sta     #$00





;function:clear screen
clear_screen
    lda     #00
    ldx     #0
clear_char
    STA     $1e00,X
    STA     $1f00,X
    INX
    BNE     clear_char

    lda     #01
    ldx     #0
white_color
    STA     $9600,X
    STA     $96ff,x
    INX
    BNE     white_color
    rts
;function:clear screen end

    org     $1c00
    ;#0top half of idle frame 1
    dc.b    %01111110
    dc.b    %00010000
    dc.b    %00010000
    dc.b    %00010000
    dc.b    %00010000
    dc.b    %00010000
    dc.b    %00010000
    dc.b    %00010000

    ;#1bottom half of idle frame 1
    dc.b    %00011000
    dc.b    %00111000
    dc.b    %00011000
    dc.b    %00011000
    dc.b    %00011000
    dc.b    %00011000
    dc.b    %00011000
    dc.b    %01111110

    ;#2top half of idle frame 2
    dc.b    %00010000
    dc.b    %00010000
    dc.b    %01111110
    dc.b    %00010000
    dc.b    %00010000
    dc.b    %00010000
    dc.b    %00010000
    dc.b    %00011110
    ;#3bottom half of idle frame 2
    dc.b    %00111000
    dc.b    %01000100
    dc.b    %00000100
    dc.b    %00000100
    dc.b    %00001000
    dc.b    %00010000
    dc.b    %00100000
    dc.b    %01111100

    ;#4top half of left frame 1
    dc.b    %00100000
    dc.b    %00100000
    dc.b    %00100000
    dc.b    %00100000
    dc.b    %00100000
    dc.b    %00100000
    dc.b    %00100000
    dc.b    %00111110
    ;#5bottom half of left frame 1
    dc.b    %00011000
    dc.b    %00111000
    dc.b    %00011000
    dc.b    %00011000
    dc.b    %00011000
    dc.b    %00011000
    dc.b    %00011000
    dc.b    %01111110

    ;#6top half of left frame 1
    dc.b    %01110000
    dc.b    %00010000
    dc.b    %00010000
    dc.b    %00010000
    dc.b    %00010000
    dc.b    %00010000
    dc.b    %00010000
    dc.b    %00011100
    ;#7bottom half of left frame 2
    dc.b    %00111000
    dc.b    %01000100
    dc.b    %00000100
    dc.b    %00000100
    dc.b    %00001000
    dc.b    %00010000
    dc.b    %00100000
    dc.b    %01111100

    ;#8top hlaf of right frame 1
    dc.b    %01111100
    dc.b    %01000100
    dc.b    %01000110
    dc.b    %01000100
    dc.b    %01111000
    dc.b    %01001000
    dc.b    %01000100
    dc.b    %01000110

    ;#9bottom half of right  frame 1
    dc.b    %00011000
    dc.b    %00111000
    dc.b    %00011000
    dc.b    %00011000
    dc.b    %00011000
    dc.b    %00011000
    dc.b    %00011000
    dc.b    %01111110

    ;#10top hlaf of right frame 2
    dc.b    %00000100
    dc.b    %00111100
    dc.b    %00100000
    dc.b    %00100000
    dc.b    %00100000
    dc.b    %00100000
    dc.b    %00100000
    dc.b    %00000000

    ;#11bottom half of right frame 2
    dc.b    %00111000
    dc.b    %01000100
    dc.b    %00000100
    dc.b    %00000100
    dc.b    %00001000
    dc.b    %00010000
    dc.b    %00100000
    dc.b    %01111100

    ;#12 just a square

    dc.b #$ffffffffffffffff


