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
CHROUT =    $ffd2
CHRIN  =    $ffcf

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
    sta     #$20

    ;start location
    lda     #$28
    sta     #$21
    lda     #$1f
    sta     #$22

main_loop
    lda     $00C5           ; loads the current pressed key from memory
    cmp     #64
    beq     idle
    cmp     #17
    beq     a_left            ; if A is pressed
    cmp     #18
    beq     d_right            ; if D is pressed
    cmp     #33                 ;
    beq     exit_prg

main_erase_current
    jsr shift_on_monitor
    lda     $00
    sta     $03
    lda     $04
    sta     $05
    ;calculate the current position of char
    clc
    lda     #$00
    adc     $20
    sta     $00
    lda     #$1e
    adc     $21
    sta     $01

    ;erase_char
    lda     #0
    sta     $0100

    lda     #$00
    adc     $20
    sta     $00
    lda     #$96
    adc     $21
    sta     $01

    ;set_color_to white
    lda     #0
    sta     $0100


main_update_shift
    lda     $20
case_idle
    cmp     #02
    bcs     case_left ;00,01 idle
    jmp     main_update_shift_end
case_left
    cmp    #04
    bcs    #case_right ;02,03 left



case_right


main_update_shift_end
    jmp     main_loop


idle
    jmp update_position_main

erase_current
    lda     #0

    ;calculate the current position of char
    clc
    lda     #$00
    adc     $20
    sta     $00
    lda     #$1e
    adc     $21
    sta     $01

    ;erase_char
    lda     #0
    sta     $0100

    lda     #$00
    adc     $20
    sta     $00
    lda     #$96
    adc     $21
    sta     $01

    ;set_color_to white
    lda     #0
    sta     $0100
    rts

;take take y@[21],x@[20],store to [01][00] as shift,
;[03][02] as character movment, [05][04]as screen color movment
shift_on_monitor
    lda $20
    sta $03
    lda $21
    sta $04
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


