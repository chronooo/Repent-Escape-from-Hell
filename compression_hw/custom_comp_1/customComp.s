 processor 6502

; KERNAL [sic] routines
CHROUT =    $ffd2
CHRIN  =    $ffcf
W =         $20
    org     $1001

    dc.w    stubend
    dc.w    12345
    dc.b    $9e, "4109", 0

; Custom compression. modified RLE scheme to not store the first and last blocks of spaces.
; This allows us to have a writable region of less than a byte, which gets rid of a lot of unnecessary 
; duplicate code we had to account for the screen memory space spanning more than a byte.
; this scheme, however, needs a white screen fill before the decompression takes place, but in the end it ends up being more efficient. 

stubend
    dc.w    0
start
    ;set up screen
    jsr     screen_init

; decode the screen data and put it into screen memory
rle_decode
    LDX     #0              ; store 0 in x, use it as index for Read loop
    LDY     #0              ; use Y as counter for the write loop
    stx     $22             ; store x into zp location for later
    pha                     ; push something because first line is pull from stack
decode_loop
    pla
    LDX     $22             ; load x into zp location for later
    LDA     data_label,X         ; LDA will turn zero flag on if it loaded zero (termination)
    beq     loop            ; if reached termination, exit
    PHA                     ; A to stack
    cmp     #$20             ; Check if is a continuous space
    bne     case_not_space  ;
case_continuous_space       ;if so, then load the number of space
    INX     ; x++
    LDA     data_label,X         ; get the loop amount in A
    jmp     number_of_times
case_not_space
    LDA     #1  ;      Case 1 time only.
number_of_times
    INX     ; increment X in anticipation of next loop
    stx     $22             ; store x into zp location for later
    TAX     ; how many time to repeat char into X

write_rle
    cpx     #0  ; is loop done?
    beq     decode_loop      ; if it is, decode another char
    PLA     ; bring value of character back into A
    sta     $1e60,Y          ; store char value at Y
    PHA     ; store A back
    DEX     ; decrement the amount of char repeats left
    INY     ; inc the screen mem address
    jmp     write_rle

loop
    lda     $00C5 ;test current key
    cmp     #33 ; Z in current key table
    beq     exit_prg
    jmp     loop

exit_prg
    rts

screen_init

    lda     #$02
    ldx     #0
color_ram1 ; fill color ram 0x9600 to 0x96ff with red (02)
    STA     $9600,X
    INX
    bne     color_ram1

    lda     #$00    ; don't need to load x cause it is 0 now. x just wrapped around.
color_ram2 ; fill color ram 0x9700 to 0x97ff with black (00)
    STA     $9700,X
    INX
    bne     color_ram2

    lda     #32     ; same thing with X here.
whitescreen
    STA     $1e00,X
    INX     
    BNE     whitescreen

    lda     #32     ; same thing with X here.
whitescreen2
    STA     $1f00,X
    INX     
    BNE     whitescreen2

    rts

data_label
    HEX 12 05 10 05 0E 14 3A 20 1E 01 0E 60 05 13 03 01 
    HEX 10 05 60 06 12 0F 0D 60 08 05 0C 0C 20 BA 10 12 
    HEX 05 13 13 60 1A 00 
