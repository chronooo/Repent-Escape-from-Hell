    org     $1c08
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