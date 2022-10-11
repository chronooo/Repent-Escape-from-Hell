    processor 6502

;   KERNAL [sic] routines
IDLE_TOP_1 = "12345"
IDLE_TOP_2 = "12345"
LEFT_TOP_1 = "12345"
LEFT_TOP_2 = "12345"
RIGHT_TOP_1 = "12345"
RIGHT_TOP_2 = "12345"
;   zero PAGE USage
;   20:current status(idle/left/right + frame/1/2)
    ; idle frame 1/2: 0/1
    ; left frame 1/2: 2/3
    ; right frame 1/2: 4/5
;   21: x movement
;   22: y movement
    org     $1001
    dc.w    stubend
    dc.w    12345
    dc.b    $9e, "4109", 0
stubend
    dc.w    0

main
    jsr     store_char
    jsr     clear_screen

    ;charcterset to from 1c00
    lda     #255
    sta     $9005

    ;start location
    lda     #1
    sta     $21
    lda     #1
    sta     $22

main_loop
    jsr     shift_on_monitor
    lda     #$0
    ldy     #$0
    sta     ($02),y
    sta     ($04),y
    ldy     #$16
    sta     ($02),y
    sta     ($04),y
    ;jmp     s_down       ;test purpose
    lda     $00C5           ; loads the current pressed key from memory

    cmp     #64 ;if nothing held down
    beq     case_idle
    cmp     #17 ; if A is pressed
    beq     a_left
    cmp     #18 ; if D is pressed
    beq     d_right
    cmp     #9  ;if W is pressed
    beq     w_top
    cmp     #41 ;if S is pressed
    beq     s_down
    cmp     #33
    beq     exit_prg
main_update_shift
    lda     $20
case_idle
    ;coordinate not updated
    jmp     main_update_shift_end
a_left
    ;check if x<1
    lda     $21
    cmp     #1
    bcc     case_idle ;tooleft,cannotmove
    dec     $21
    jmp     main_update_shift_end
d_right
     ;check if x> 20 => 20< X (21 is maximum)
    lda     #20
    cmp     $21
    bcc     case_idle ;too right
    inc     $21
    jmp     main_update_shift_end
w_top
    ;check y <1
    lda     $22
    cmp     #1
    bcc     case_idle ;too top
    dec     $22
    jmp     main_update_shift_end
s_down
    ;check if y>21 => 21<y (22 is maximum)
    lda     #20
    cmp     $22
    bcc     case_idle ;too bottom
    inc     $22
    jmp     main_update_shift_end
main_update_shift_end
    jsr     shift_on_monitor
    ldy     #$0
    lda     #01 ;a squre
    sta     ($02),y
    ldy     #$16
    sta     ($02),y

    lda     #02 ;color red
    ldy     #$0
    sta     ($04),y
    ldy     #$16
    sta     ($04),y

    jsr     interval_start
    jmp     main_loop
exit_prg
    rts




;take take y@[22],x@[21],store to [01][00] as shift,
;[03][02] as character movment, [05][04]as screen color movment
shift_on_monitor
    lda $21
    sta $03 ;->xto[3]
    lda $22
    sta $04; -> y to [4]
    lda #$00
    sta $01
    sta $00
som_loop
som_y
    lda $04
    cmp #$0
    beq som_x
    clc
    lda #22
    adc $0
    sta $0

    lda $01
    adc #$0
    sta $01

    dec $04
    jmp som_y
som_x
    clc
    lda $03
    adc $0
    sta $0
    lda $01
    adc #$0
    sta $01

;calculate movement of characters
;not out of border is assumend
    ;last 2 digit always 0
    lda $00
    sta $02
    sta $04
    clc

    lda #$1e
    adc $01
    sta $03

    lda #$96
    adc $01
    sta $05
    rts

;function:clear screen
clear_screen
    lda     #0
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

;loopoverdelay255
interval_start
    lda #$0
    sta $11
interval_loop
    lda $11
    cmp #$0a
    beq interval_done
    inc $11
    jsr delay255_start
    jmp interval_loop
interval_done
    rts

;delay loop
delay255_start
    lda #$0
    sta $01
delay255_loop
    lda $01
    cmp #$FF
    beq delay255_done
    inc $01
    jmp delay255_loop
delay255_done
    rts

    ;include "shift_on_monitor.s"
    ;include "tempChars.s"
store_char
    ldx     #$0
    lda     #$0
char1
    cpx     #8
    beq     char2
    sta     $1c00,x
    inx
    jmp     char1
char2
    lda     #$ff
    cpx     #16
    beq     store_char_end
    sta     $1c00,x
    inx
    jmp     char2
store_char_end
    rts
