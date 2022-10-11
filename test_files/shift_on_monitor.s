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