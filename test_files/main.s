    processor 6502

;   KERNAL [sic] routines

CHROUT =    $ffd2
CHRIN  =    $ffcf
;   ZERO PAGE MACROS
X_POS  =    $21     ; ZP 0x21: player X location (0 <= X_POS <= 20) x = 0 is leftmost tile.
Y_POS  =    $22     ; ZP 0x22: player Y location (0 <= Y_POS <= 11) y = 0 is topmost tile.

    org     $1001
    dc.w    stubend
    dc.w    12345
    dc.b    $9e, "4109", 0 
stubend
    dc.w    0

start
    ;       setting init values of player x y coords
    LDA     #0
    STA     X_POS
    LDA     #0
    STA     Y_POS

;   Switching character set pointer to 0x1c00:
    lda     #255
    STA     $9005 ; POKE 36869 255 (from book)

;   Switching to 8x16 character set (setting bit 0 of 0x9003 to 1) 
;   AND setting number of rows on screen to 12 (setting bits 1-6 of 0x9003 to 12)
;   this gives us 252 or 0xFC characters displayed on screen at once - fits in one byte. (nice)
    lda     $9003           
    and     #%10000000      ; clear bits 0-6 
    ora     #%10011001      ; setting bit 0 to 1, bits 1-6 to 001100 (12)
    STA     $9003           ; store it
;setting columns # to 21: (bits 0-6 of 0x9002 control the # of col)
    lda     $9002
    and     #%10000000  ; clear bits 0-6
    ora     #21         ; set bits 0-6 to #21 (# of columns)
    STA     $9002

; clearing screen in beginning of game (writing char 00 to all 252 tiles)
    lda     #0          ; selecting char 0 
    ldx     #0          ; loop counter
whitescreen             ; probably change this to a JSRable function later 
    STA     $1b04,X
    INX     
    cpx     #$FC    ; 252 (size of screen) the execution falls through when x == 252
    BNE     whitescreen

;   Next, fill 0x9600 - 0x96FC (color RAM) with $00 (turn them all on, black color)
    lda     #$00
    ldx     #$00
color_ram 
    STA     $9600,X
    INX   
    cpx     #$FC
    BNE     color_ram
    STA     $9600,X

;   loop to set the bottom row of the characters to character 00001, written to map array
    lda     #1          ; selecting char 1
    ldx     #0          ; loop counter
draw_ground             ; fills the bottom row (0x1bff - 21) with "ground" character (01 right now)
    STA     $1beb,X     
    INX     
    cpx     #21    ; 
    BNE     draw_ground

loop
    jsr     coord_to_index      ; get index into map array inside X_reg
    lda     #0                  ; store 0 (whitespace) in where the player currently is,
    sta     $1b04,X             ; in case the player position changes. If it doesn't,
                                ; that is okay, since we will restore the same value in update_next_frame.

            ; GET PLAYER INPUT!
    lda     $00C5       ; loads the current pressed key from memory
    cmp     #64         ; if nothing held down
    beq     update_next_frame
    cmp     #17         ; if A is pressed
    beq     a_left
    cmp     #18         ; if D is pressed
    beq     d_right
    cmp     #9          ; if W is pressed
    beq     w_top
    cmp     #41         ; if S is pressed
    beq     s_down
    cmp     #10         ; if R is pressed, restart whole program
    beq     start
    cmp     #33         ; if Z is pressed, exit
    beq     exit_prg
    jmp     update_next_frame

exit_prg
    rts

a_left
    dec     X_POS               ; X_POS -= 1
    jmp     key_pressed
d_right
    inc     X_POS               ; X_POS += 1
    jmp     key_pressed
w_top
    dec     Y_POS               ; Y_POS -= 1
    jmp     key_pressed
s_down
    inc     Y_POS               ; Y_POS += 1 ; no jump here as we just fall through..
key_pressed
    ;   need to fixup the x and y values in case we went out of bounds:
    ;   we do NOT need to check both x and y for out of bounds, 
    ;   since we know only one case can be satisfied at a time. (we only check one keypress per loop)

    ;   fixing x:   bound is (0 <= X_POS <= 20)
    lda     X_POS           ; load X_POS into A_reg (this sets the negative flag if it was negative!)
    bmi     neg_x           ; branch if X_POS is negative, i.e X_POS = FF because we just did X_POS = 0 - 1
    cmp     #21             ; cmp X_POS to 21; if it is equal, then we need to do X_POS--
    beq     pos_x

    ;  fixing y:    bound is (0 <= Y_POS <= 11)
    lda     Y_POS           ; load Y_POS into A_reg (this sets the negative flag if it was negative!
    bmi     neg_y           ; branch if Y_POS is negative, i.e Y_POS = FF because we just did Y_POS = 0 - 1
    cmp     #12             ; cmp Y_POS to 12 ; if it is equal, then we need to do Y_POS--
    beq     pos_y
    jmp     update_next_frame   ; if our code reached here, means everything is in bounds, continue execution.    
pos_y
    dec     Y_POS
    jmp     update_next_frame 
neg_y       ; X_POS is negative (is FF), so need to do X_POS++, bring it back to 0
    inc     Y_POS
    jmp     update_next_frame   
pos_x
    dec     X_POS
    jmp     update_next_frame 
neg_x       ; X_POS is negative (is FF), so need to do X_POS++, bring it back to 0
    inc     X_POS           ; no jmp here, just fall through.


; once we grabbed the input, update all the other stuff (projectile movement, enemy movement, death check, etc..)
; We will put all our game update stuff in here, then display at the end of the loop ("draw" label)
update_next_frame

    ; update player pos on map depending on the x, y stored in zero page:
    jsr     coord_to_index  ; compute player pos offset in map array (returned in X_reg)
    lda     #2              ; player char ptr into A_reg
    sta     $1b04,X         ; store it at the player's position




draw    ; label to jump to when we want to skip to rendering step from some reason

        ; loop that goes through all the 252 bytes of the map object array (0x1B04-0x1BFF) 
        ; and writes the corresponsing character to screen memory.
        ; does not really matter that we are drawing stuff every frame, vic is too fast anyway (i think)

    ldx     #0              ; loop counter init (count to 0xFC)
drawloop
    lda     $1b04,X         ; load value of (0x1B04 + X) into A_reg
                            ; bitmask 0 from bits 765 to get the character array index
    AND     #%00011111      ; get the the least significant 5 bits in A_reg (char index)
;   now that we have the character index in A_Reg, store that at corresponsing screen memory location
    STA     $1e00,X         ; use same offset (X) for screen memory storage.
    INX                         
    cpx     #$FC            ; is X == 0xFC?
    bne     drawloop        ; if not, draw next character. If X == 0xFC, fall through.



    jsr     waste_time
    jsr     waste_time
    jsr     waste_time
    jsr     waste_time
    jsr     waste_time
    jsr     waste_time
    jmp     loop            ; go back to very top of while loop


coord_to_index              ; routine to compute offset from $1b04 from x, y value (array index)
                            ; formula is index(x,y) = 21y + x 
                            ; stores the computed index inside X_reg (!)
    LDX     Y_POS           ; load Y_POS into X, use as loop counter when adding
    LDA     #0
x_mult_loop                 ; loop to compute A_reg = 21 * X
    cpx     #0              ; if x is 0, quit multiplication loop
    beq     add_x           
    clc                     ; clear carry before add
    adc     #21             ; add 21
    DEX                     ; decrement X
    jmp     x_mult_loop     ; go to top of loop
add_x
    clc                     ; clc before add
    adc     X_POS           ; A_reg += Y_POS
    tax                     ; return in X for convenience 
                            ; (all the times we want to use it, we use it as an offset (ex: sta 00,X)
    rts                     ; return from routine

; routine to waste time. changes the Y register.
waste_time
    ldy     $0 
waste_time_loop             ; waste time routine
    INY
    CPY     $FF             ; waste time by counting up to 255 in Y reg
    BNE     waste_time_loop
    rts

/*
;   END OF CODE, START OF DATA
*/

    ; map array. in case we want to have something there at game start,
    ; put that here (probably will not use, because then we cannot replay the game)
    org     $1b04           
;   in this array, each entry with bits %76543210:

;   bits 43210 represent the character that is displayed in the custom character set.
;   Since our character set starts at 0x1C00 and screen memory begins at 0x1E00, we have 
;   32 characters to choose from. bits 43210 represent a number x from 0 to 31 such that
;   %76543210 will display character at address 0x1c00 + 16x.

;   We have bits 7, 6, 5 to store potential additional information if we need in a number from 0 to 7.
;   possible additional information to store: projectile movement timer, enemy attack timer, enemy movement timer,
;   tough enemy's health points, etc...

; CHAR DATA -------------------
    org     $1c00
    
    ;       CHAR 00 - whitespace
    dc.b    #%00000000
    dc.b    #%00000000
    dc.b    #%00000000
    dc.b    #%00000000
    dc.b    #%00000000
    dc.b    #%00000000
    dc.b    #%00000000
    dc.b    #%00000000
    dc.b    #%00000000
    dc.b    #%00000000
    dc.b    #%00000000
    dc.b    #%00000000
    dc.b    #%00000000
    dc.b    #%00000000
    dc.b    #%00000000
    dc.b    #%00000000

    ;       CHAR 01 - ground tile
    dc.b    #%11111101    
    dc.b    #%11011010
    dc.b    #%10010111
    dc.b    #%11011101 
    dc.b    #%10111111
    dc.b    #%11000011
    dc.b    #%00000000
    dc.b    #%00000000
    dc.b    #%00000000
    dc.b    #%00000000
    dc.b    #%00000000
    dc.b    #%00000000
    dc.b    #%00000000
    dc.b    #%00000000
    dc.b    #%00000000
    dc.b    #%00000000

    ;       CHAR 02 - PLAYER placeholder
    dc.b    #%11111111
    dc.b    #%10000001
    dc.b    #%10000001
    dc.b    #%10100101
    dc.b    #%10000001
    dc.b    #%10000001
    dc.b    #%10010001
    dc.b    #%10000001
    dc.b    #%10000001
    dc.b    #%10100101
    dc.b    #%10111101
    dc.b    #%10000001
    dc.b    #%10000001
    dc.b    #%10000001
    dc.b    #%11111111
    dc.b    #%11111111

    ;       CHAR 03 - PLAYER SAD placeholder (lol)
    dc.b    #%11111111
    dc.b    #%10000001
    dc.b    #%10000001
    dc.b    #%10100101
    dc.b    #%10000101
    dc.b    #%10000001
    dc.b    #%10010001
    dc.b    #%10000001
    dc.b    #%10000001
    dc.b    #%10011001
    dc.b    #%10100101
    dc.b    #%10100101
    dc.b    #%10000001
    dc.b    #%10000001
    dc.b    #%11111111
    dc.b    #%11111111
