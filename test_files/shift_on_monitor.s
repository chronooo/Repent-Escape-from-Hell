processor 6502
;take take y@[21],x@[20],store to [01][00]
org     $1001
    dc.w    stubend
    dc.w    12345
    dc.b    $9e, "4109", 0
stubend
    dc.w    0

randomdata

    lda #20
    sta $20
    lda #15
    sta $21

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
    b.eq stm_x
    clc
    lda #$22
    adc $0
    sta $0
    lda $01
    adc #$0
    sta $01
    dec $04
    jmp stm_y
som_x
    clc
    lda $03
    adc $0
    sta $0
    lda $01
    adc #$0
    sta $01
dead
    jmp dead
    rts