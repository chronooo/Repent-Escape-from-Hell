    processor 6502

;   jumpting test developed based on moving test.
;   Usage:
        ;W: jump to air without crashing with anything
        ; can only jump on green ground
        ;Z: exit

;   temporarily zeropage usage,
;   19: (12,10) is an obstacle flag, will be fixed after collision problem is solved.
;   zero PAGE USage
;   20:current status(idle/left/right + frame/1/2)
    ; ******XX
    ; not frame 1: 0
    ; idle frame 1: 1
    ; left frame 1: 2
    ; right frame 1: 3
    ;*****X**
    ; falling flag: 0 is not falling (either jumping to air or waling on ground)m
    ; memo: 5 bits remaining
;   21: x movement
;   22: y movement
;   23: vertical speed
    org     $1001
    dc.w    stubend
    dc.w    12345
    dc.b    $9e, "4109", 0
stubend
    dc.w    0

main
    jsr     startup
main_loop
    jsr     player_location
    lda     #$0
    ldy     #$0
    sta     ($02),y
    sta     ($04),y
    ldy     #$16
    sta     ($02),y
    sta     ($04),y

    lda     $00C5           ; loads the current pressed key from memory
keyboard_triggers
    cmp     #17 ; if A is pressed
    beq     a_left
    cmp     #18 ; if D is pressed
    beq     d_right
    cmp     #9  ;if W is pressed
    beq     w_jump
    cmp     #41 ;if S is pressed
    beq     s_down
    cmp     #33
    beq     exit_prg; if z is pressed
    jmp     case_idle
exit_prg
    rts

case_idle
    ;coordinate not updated
    jmp     vertical_movement
a_left
    ;check if x<1
    lda     $21
    cmp     #1
    bcc     case_idle ;tooleft,cannotmove
    dec     $21
    jmp     vertical_movement
d_right
     ;check if x> 20 => 20< X (21 is maximum)
    lda     #20
    cmp     $21
    bcc     case_idle ;too right
    inc     $21
    jmp     vertical_movement
w_jump
    ;if 2 squares below is on ground, and falling flag is 1,
    ;set falling flag to be 0 and initial vertical speed.
    ;check 2 squares from head below is ground or not.
    lda     $21
    sta     $0
    lda     $22
    clc
    adc     #2
    sta     $1
    jsr     shift_on_monitor
    ldy     #$0
    lda     ($04),y
    cmp     #05     ;use color for verifying ground at the moment
    bne     w_jump_verification_done
    ;check falling flag
    lda     #%00000100
    and     $20
    cmp     #%00000100
    bne     w_jump_verification_done
    ;set up jump flag and initial vertical speed
    lda     #%11111011
    and     $20
    sta     $20
    lda     #3
    sta     $23
w_jump_verification_done
    jmp     vertical_movement

s_down
    ;used for reset
    lda     #0
    sta     $21
    lda     #12
    sta     $22
    lda     #%00000100
    sta     $20     ;reset jump flag
    lda     #0
    sta     $23


vertical_movement
revise_vertical_speed
    ;   check is jumping
    lda     #%00000100
    and     $20
    cmp     #%00000100
    beq     revise_vertical_falling_speed
;case jumping
    lda     $23
    cmp     #0  ;change to fall when vertical spped is 0
    beq     stop_jumping
    lda     $22
    cmp     #0  ;change to fall when at top border
    beq     stop_jumping

on_top_checking
    ldx     #$0
on_top_checking_loop
    jmp     normal_jumping
    ;       things from now to b4 crashed jumping will be handled in collision test stage later
    cpx     $23
    beq     normal_jumping
    ;update y for shift on monitor
    sec
    stx     $0
    lda     $22
    sbc     $0
    sta     $1
    ;x for shift on monitor
    lda     $21
    sta     $0
    jsr     shift_on_monitor

    ldy     #$0
    lda     ($04),y
    cmp     #01     ;use color for verifying ground at the moment
    bne     crashed_jumping
    inx
    jmp     on_top_checking_loop
normal_jumping
    dec     $23
    jmp     revise_vertical_jumping_done
crashed_jumping
    ;dex     ;can only jump to the squre next to obstacle
    stx     $23
    jmp     revise_vertical_jumping_done
stop_jumping
    lda     #%00000100
    ora     $20
    sta     $20
    lda     #0
    sta     $23
revise_vertical_jumping_done
    jmp     main_update_vertical_shift
revise_vertical_falling_speed

main_update_vertical_shift
    ;is vertical speed 0?
    lda     $23
    cmp     #0
    beq     main_update_shift_end
    ;falling or jumping?
    lda     #%00000100
    and     $20
    cmp     #%00000000
    bne     update_vertical_shift_falling
;case jumping
    sec
    lda     $22
    sbc     $23
    sta     $22
    jmp     main_update_shift_end
update_vertical_shift_falling
    clc
    lda     $23
    adc     $22
    sta     $22

main_update_shift_end
    jsr     player_location
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

;=====================================
;    tempordary code for jumping and falling tests
;=====================================
startup

    ;charcterset to from 1c00
    lda     #255
    sta     $9005

    jsr     store_char
    jsr     clear_screen



draw_an_obstacle
    lda     #12
    sta     $00
    lda     #10
    sta     $01
    jsr     shift_on_monitor
    ldy     #$0
    lda     #01 ;a squre
    sta     ($02),y
    lda     #06 ;color orange
    ldy     #$0
    sta     ($04),y
    ldy     #$16
    sta     ($04),y

drawing_ground
    ldx     #0
drawing_ground_loop
    lda     #14
    sta     $01
    cpx     #20
    beq     drawing_ground_done
    stx     $00
    jsr     shift_on_monitor
    ldy     #$0
    lda     #01 ;a squre
    sta     ($02),y
    lda     #05 ;color green
    ldy     #$0
    sta     ($04),y
    inx
    jmp     drawing_ground_loop
drawing_ground_done

    ;start location
    lda     #12
    sta     $21
    lda     #12
    sta     $22
    lda     #%00000100
    sta     $20
    lda     #0
    sta     $23
    rts

;============================
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

;=====================================
;player location
player_location
    lda     $21
    sta     $00
    lda     $22
    sta     $01
    jsr     shift_on_monitor
    rts


;updated shift on monitor
;take take y@[01],x@[00],store to [01][00] as shift,(small endian)
;[03][02] as character movment, [05][04]as screen color movment
;take take y@[22],x@[21],store to [01][00] as shift,
;[03][02] as character movment, [05][04]as screen color movment
shift_on_monitor
    lda $00
    sta $03 ;->xto[3]
    lda $01
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
