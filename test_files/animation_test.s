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

    lda     $00C5           ; loads the current pressed key from memory

    cmp     #18 ;if s is presssed "test stage design)
    beq     fall_f1
    cmp     #9  ;if w is pressed
    beq     top_f1
    cmp     #17 ; if A is pressed
    beq     left_f1
    cmp     #18 ; if D is pressed
    beq     right_f1
    cmp     #9  ;if W is pressed
    beq     top_f1
;any other case should be seen as idle?
idle_f1
    lda     $20
    cmp     #1 ;now is in idle frame1->switch frame2
    beq     idle_f2

    lda     #1
    sta     $20
    jmp     main_loop
idle_f2


    lda     #0
    sta     $20
    jmp     main_loop
left_f1
    lda     #$20
    cmp     #2 ;now is in left frame1->switch frame2
    beq     left_f2

    lda     #0
    sta     $20
    jmp     main_loop
left_f2

    lda     #0
    sta     $20
    jmp     main_loop
right_f1
    lda     #$20
    cmp     #4 ;now is in left frame1->switch frame2
    beq     right_f2

    lda     #3
    sta     $20
    jmp     main_loop
right_f2

    lda     #0
    sta     $20
    jmp     main_loop
top_f1

    jmp     main_update_shift_end
fall_f1


;finishing
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
    lda $01
    cmp #$FF
    beq delay255_done
    inc $01
    jmp delay255_loop
delay255_done
    rts

    ;include "shift_on_monitor.s"
    ;include "tempChars.s"
