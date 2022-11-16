    processor 6502

;   KERNAL [sic] routines

CHROUT =    $ffd2
CHRIN  =    $ffcf

;   Audio SPEAKERS
AU_VOL =    36878   ; 0 to 15
AU_LOW =    36874   ; 128 to 255
AU_MID =    36875   ; 128 to 255
AU_HI  =    36876   ; 128 to 255
AU_NO  =    36877   ; 128 to 255


    org     $1001
    dc.w    stubend
    dc.w    12345
    dc.b    $9e, "4109", 0 
stubend
    dc.w    0

start
;   AUDIO
    lda     #15
    STA     AU_VOL ; POKE 36878 15 (from book)

loop

    lda     #172
    STA     AU_LOW ; POKE 36874 156 (from book)

    lda     #176
    STA     AU_MID ; POKE 36874 156 (from book)

    jsr     waste_time

    lda     #192
    STA     AU_MID ; POKE 36874 156 (from book)

    jsr     waste_time


    lda     #202
    STA     AU_MID ; POKE 36874 156 (from book)
    jsr     waste_time

    lda     #202
    STA     AU_LOW ; POKE 36874 156 (from book)
    lda     #212
    STA     AU_MID ; POKE 36874 156 (from book)
    jsr     waste_time
    jsr     waste_time

    lda     #196
    STA     AU_LOW ; POKE 36874 156 (from book)
    lda     #223
    STA     AU_MID ; POKE 36874 156 (from book)
    jsr     waste_time
    jsr     waste_time




    lda     $00C5           ; loads the current pressed key from memory
    cmp     #64
    beq     loop
    cmp     #33
    beq     exit_prg
    jmp     loop
exit_prg
    rts

    jmp     loop            ; go to top of while loop
waste_time
    ldy     $0

waste_time_loop
    INY
    ldx     $0
waste_time_inner
    INX 
    CPX     #$14
    BNE     waste_time_inner

    CPY     #$FF            ; waste time by counting up to 255 in Y reg
    BNE     waste_time_loop
    rts