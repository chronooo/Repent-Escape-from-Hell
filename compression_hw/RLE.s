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

    ;set up screen
    jsr     screen_init

; decode the screen data and put it into screen memory
rle_decode
    LDX     #0              ; store 0 in x, use it as index for Read loop
    stx     $22             ; store x into zp location for later
    LDY     #1              
    sty     $23             ; y overflow flag to 1 in the beginning
    LDY     #0              ; use Y as counter for the write loop

decode_loop
    LDX     $22             ; load x into zp location for later
    LDA     $1a00,X         ; LDA will turn zero flag on if it loaded zero (termination)
    beq     loop            ; if reached termination, exit
    PHA                     ; A to stack
    INX     ; x++
    LDA     $1a00,X         ; get the loop amount in A
    INX     ; increment X in anticipation of next loop
    stx     $22             ; store x into zp location for later
    TAX     ; how many time to repeat char into X
write_rle
    cpx     #0  ; is loop done?
    beq     decode_loop      ; if it is, decode another char
    ; has Y overflown once?
    lda     $23              ; will set 0 flag on load
    beq     write_rle_overflow      ; y has overflown once
; y has not overflown yet.. ->
    PLA     ; bring value of character back into A
    sta     $1e00,Y          ; store char value at Y
    PHA     ; store A back
    DEX     ; decrement the amount of char repeats left
    INY     ; inc the screen mem address
    beq     y_overflow_set ; if y is 0 after increase, it has overflown
    jmp     write_rle

write_rle_overflow
    cpx     #0  ; is loop done?
    beq     decode_loop      ; if it is, decode another char
    PLA     ; bring value of character back into A
    sta     $1f00,Y          ; store char value at Y
    PHA     ; store A back
    DEX     ; decrement the amount of char repeats left
    INY     ; inc the screen mem address
    jmp     write_rle


y_overflow_set  ;set y overflow to true
    sty     $23 ;storing 0 at the flag location
    jmp     write_rle

loop
    lda     $00C5
    cmp     #64
    beq     loop
    cmp     #33 ; Z in current key table
    beq     exit_prg


exit_prg
    rts

screen_init
    lda     #$00 
    ldx     #0
color_ram1 ; fill color ram 0x9600 to 0x96ff with black (00)
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

    org     $1a00 ; RLE encoded screen!; terminate with 00 just because (value, count)


    HEX 20 60 12 01 05 01 10 01 05 01 0E 01 14 01 3A 01
    HEX 20 1E 05 01 13 01 03 01 01 01 10 01 05 01 20 01
    HEX 05 01 12 01 0F 01 0D 01 20 01 08 01 05 01 0C 01
    HEX 0C 01 20 BA 10 01 12 01 05 01 13 02 20 01 21 01  
    HEX 20 A7


    ;RLE
    ;Hex value (HH) is how many chars, Following letter/value is what is on the screen
    ;$60W $01R $01E $01P $01E $01N $01T $01: $1EW $01E $01S $01C $01A $01P $01E
    ;$01W $01F $01R $01O $01M $01W $01H $01E $01L $01L $BAW $01P $01R $01E $01S
    ;$01S $01W $01Z $A7W
    ;Corresponding screen code for each letter
    ;A'=01 C'=03 E'=05 F'=06 
    ;H'=08 L'=0C
    ;M'=0D N'=0E O'=0F P'=10 R'=12 
    ;S'=13 T'=14 W(whitespace)=20
    ; :'=3A
    
