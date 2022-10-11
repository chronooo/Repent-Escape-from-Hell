    processor 6502

    org     $1001
    dc.w    stubend
    dc.w    12345
    dc.b    $9e, "4109", 0
stubend
    dc.w    0
main
    lda #21
    sta $21
    lda #12
    sta $22
    jsr shift_on_monitor
end
    jmp end
    rts

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