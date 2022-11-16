 processor 6502
REFERENCE_PG0   = $10C1 ;1AC2-1
REFERENCE_PG1   = $10CF ;1AD0-1
SPACES_LENGTH   =    $10D3
START_SCREEN    = $10D7
PT_SPACES_LENGTH    = $0A
SCREEN_LSB  = $0B
SCREEN_MSB  = $0C
LAST_READING_INDEX  = $0D
; KERNAL [sic] routines

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
b4_decode
    LDA     #00             ;End of screen data
    STA     PT_SPACES_LENGTH ;initial number of spaces pointer
    pha
    LDY     #0              ; store 0 in Y, use it as index for Read loop
    LDX     #0
decode_loop
    LDA     START_SCREEN,Y
    CMP     #$00 ; end of screen
    BEQ     decode_loop_done
    LSR
    LSR
    LSR
    LSR     ;find the MS 4 bit of this byte
2nd_char
    LDX     #$0
    STX     $0
    JMP     decode_a_char
fst_char
    LDA     START_SCREEN,Y
    AND     #$0F    ;find the LS 4 bit of this byte
    INC     $0
    Jmp     decode_a_char
fst_char_done
    INY
    jmp     decode_loop
;******
decode_a_char
    cmp     #$F
    beq     decode_page_1
decode_page_0
    STA     LAST_READING_INDEX ;store the char to memory in case of usage for page 1
    TAX
    LDA     REFERENCE_PG0,X ;load a char from memory
    PHA
    jmp     decode_a_char_done
decode_page_1
    PLA     ;push out the last char for stack
    LDX     LAST_READING_INDEX ; find the index
    LDA     REFERENCE_PG1,X ;load a char from memory
    PHA
decode_a_char_done
    lda     $0
    cmp     #$1
    beq     fst_char_done
    bne     fst_char
;******
decode_loop_done
;###########################################
; draw chars on screen based on stack
;###########################################
draw_on_screen
    LDY     #$0
    LDA     #$00
    STA     SCREEN_LSB
    LDA     #$1e ;initialize the screen page
    STA     SCREEN_MSB
draw_on_screen_loop
    PLA     ;get a char from stack
    CMP     #$00
    BEQ     loop  ;end of screen
    CMP     #$20 ; if continous space?
    BEQ     draw_contionous_spaces
;not contiionous space char
    sta     (SCREEN_LSB),Y
    jsr     draw_on_screen_pt_update
    jmp     draw_on_screen_loop
;*****draw_contionous_spaces******
draw_contionous_spaces
    LDX     PT_SPACES_LENGTH
    LDA     SPACES_LENGTH,X ;get curreent space length
    INC     PT_SPACES_LENGTH
    TAX     ;store current space length to X
draw_contionous_spaces_loop
    TXA
    CMP     #$00
    BEQ     draw_contionous_spaces_done
    LDA     #$20
    STA     (SCREEN_LSB),Y
    jsr     draw_on_screen_pt_update
    DEX     ; X--
    jmp     draw_contionous_spaces_loop
draw_contionous_spaces_done
    jmp     draw_on_screen_loop

;###########################################
; final clear
;###########################################
loop
    lda     $00C5 ;test current key
    cmp     #33 ; Z in current key table
    beq     exit_prg
    jmp     loop
exit_prg
    rts
;###########################################
; draw_on_screen_pt_update
;###########################################
draw_on_screen_pt_update
    INY     ; inc the screen mem address
    beq    draw_on_screen_pt_update_y_overflow_set ; if y is 0 after increase, it has overflown
    rts
draw_on_screen_pt_update_y_overflow_set
    LDA     #$1f
    STA     SCREEN_MSB
    rts
;###########################################

;###########################################
; initialize the screen with color
;###########################################
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
;###########################################

    ;   01 02 03 04 05 06 07 08 09 0A 0B 0C 0D 0E F1 F2
    ;   F3
    ;   00 01 02 03 04 05 06 07 08 09 0A 0B 0C 0D 0E 0F
    HEX 20 60 12 10 13 05 0E 01 0C 14 3A 03 06 0F 0D 08 ;....1A references
    HEX 1A 60 1E BA A7 13 F2 55 63 41 99 62 F2 1F E3 D2 ;60,1E,BA,A7 space lenghth
    HEX 64 8C 56 27 81 BA 76 46 31 00 ;remanining reveresed screendata.

