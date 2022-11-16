    processor 6502

;   HEALTH / LOSE TEST
;   USE A / D to decrease / increase health counter.
;   when health reaches 0, lose message is displayed.
;   use R to reset program, use Z to exit to BASIC prompt.

;   KERNAL [sic] routines
CHROUT =    $ffd2
CHRIN  =    $ffcf

    org     $1001
    dc.w    stubend
    dc.w    12345
    dc.b    $9e, "4109", 0 
stubend
    dc.w    0

start

    lda     #$20
    ldx     #0
whitescreen
    STA     $1e00,X
    INX     
    cpx     #$FF
    BNE     whitescreen
    STA     $1e00,X

    lda     #$20
    ldx     #0
whitescreen2
    STA     $1f00,X
    INX     
    cpx     #$FF
    BNE     whitescreen2
    STA     $1f00,X

;   loop to fill 0x9600 - 0x96FF with FF:
    lda     #$00
    ldx     #$00
color_ram 
    STA     $9600,X
    INX   
    cpx     #255
    BNE     color_ram
    STA     $9600,X

;   loop to fill 0x9700 - 0x97FF with FF:
    lda     #$00
    ldx     #0
color_ram1
    STA     $9700,X
    INX   
    cpx     #255
    BNE     color_ram1
    STA     $9700,X

;   Now, all the pixels on the display are enabled

    ldx     #3 ; load 3 to x
    jmp     printchar
loop
    lda     $00C5           ; loads the current pressed key from memory
    cmp     #64
    beq     loop
    cmp     #17 
    beq     is_a            ; if A is pressed
    cmp     #18 
    beq     is_d            ; if D is pressed
    cmp     #10
    beq     start
    cmp     #33
    beq     exit_prg
    jmp     loop

exit_prg
    rts

is_a    
    CPX     #1              ; compares to 1
    beq     lose_screen     ; if 1 then lose
    DEX             
    jmp     printchar

is_d                        

    CPX     #9             
    BEQ     printchar
    INX    
    jmp     printchar

printchar
    TXA                     ; puts health into A
    CLC                 
    ADC     #$30            ; adds 0x30 to get to the numbers in character
    STA     $1e00           ; stores the character selected to 
                            ; first byte of screen memory  
    lda     #0
long_waste          ; wastes time 0x70 times
    ADC     #1
    jsr     waste_time
    cmp     #$70
    BNE     long_waste

    jmp     loop

waste_time
    ldy     $0 
waste_time_loop             ; waste time routine
    INY
    CPY     $FF             ; waste time by counting up to 255 in Y reg
    BNE     waste_time_loop
    rts

lose_screen
    lda     #48
    STA     $1e00            
    lda     #25
    STA     $1ed0           
    lda     #15
    STA     $1ed1         
    lda     #21
    STA     $1ed2          
    lda     #32
    STA     $1ed3
    lda     #12
    STA     $1ed4           
    lda     #15
    STA     $1ed5              
    lda     #19
    STA     $1ed6
    lda     #5
    STA     $1ed7
    jmp     loop


