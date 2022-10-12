    processor 6502

;   KERNAL [sic] routines
;   zero PAGE USage
;   20:current status(idle/left/right + frame/1/2)
    ; not frame 1: 0
    ; idle frame 1: 1
    ; left frame 1: 2
    ; right frame 1: 3
    ; memo: 5bits remaining
;   21: x movement
;   22: y movement
    org     $1001
    dc.w    stubend
    dc.w    12345
    dc.b    $9e, "4109", 0
stubend
    dc.w    0

main
    jsr     clear_screen

    ;start top half location
    lda     #$e6
    sta     $21
    lda     #$1e
    sta     $22
    lda     #$e6
    sta     $23
    lda     #$96
    sta     $24


main_loop

    lda     $00C5           ; loads the current pressed key from memory

case_fall
    cmp     #41 ;if s is presssed "test stage design)
    bne     case_rise
    lda     #121
    sta     $0
    lda     #48
    sta     $1
    lda     #0
    sta     $20
    jmp     main_loop_finishing
case_rise
    cmp     #9  ;if w is pressed
    bne     case_left
    lda     #120
    sta     $0
    lda     #48
    sta     $1
    lda     #0
    sta     $20
    jmp     main_loop_finishing
case_left
    cmp     #17 ; if A is pressed
    bne     case_right
    lda     $20
    cmp     #2 ;now is in left frame1->switch frame2
    beq     left_f2
    jmp     left_f1
case_right
    cmp     #18 ; if D is pressed
    bne     case_idle
    lda     $20
    cmp     #3 ;now is in left frame1->switch frame2
    beq     right_f2
    jmp     right_f1
case_idle
    lda     $20
    cmp     #1 ;now is in idle frame1->switch frame2
    beq     idle_f2
    jmp     idle_f1
left_f1
    lda     #115
    sta     $0
    lda     #'1
    sta     $1
    lda     #2
    sta     $20
    jmp     main_loop_finishing
left_f2
    lda     #116
    sta     $0
    lda     #'2
    sta     $1
    lda     #0
    sta     $20
    jmp     main_loop_finishing
right_f1
    lda     #107
    sta     $0
    lda     #'1
    sta     $1
    lda     #3
    sta     $20
    jmp     main_loop_finishing
right_f2
    lda     #118
    sta     $0
    lda     #'2
    sta     $1
    lda     #0
    sta     $20
    jmp     main_loop_finishing
idle_f1
    lda     #69
    sta     $0
    lda     #'1
    sta     $1
    lda     #1
    sta     $20
    jmp     main_loop_finishing
idle_f2
    lda     #70
    sta     $0
    lda     #'2
    sta     $1
    lda     #0
    sta     $20
    jmp     main_loop_finishing
main_loop_finishing
    ;drawing characters to the location
    lda     $0
    ldy     #$0
    sta     ($21),y
    lda     $1
    ldy     #$16
    sta     ($21),y

    ;drawing character with color
    lda     #$2
    ldy     #$0
    sta     ($23),y
    ldy     #$16
    sta     ($23),y
    jsr     interval_start
    jmp     main_loop
exit_prg
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
    lda $02
    cmp #$FF
    beq delay255_done
    inc $01
    jmp delay255_loop
delay255_done
    rts

    ;include "shift_on_monitor.s"
    ;include "tempChars.s"
