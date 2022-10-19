    processor 6502

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

        ;   TODO: integrate movement code into here
a_left
    lda     #00             ; prints character at 0x1c00
    jmp     key_pressed
d_right
    lda     #01             ; prints character at 0x1c10
    jmp     key_pressed
w_top
    lda     #02             ; prints character at 0x1c20
    jmp     key_pressed
s_down
    lda     #03             ; prints character at 0x1c30
    jmp     key_pressed

key_pressed
    STA     $1bd6           ; stores the character selected to 
                            ; first byte of map array.... for now....

; once we grabbed the input, update all the other stuff (projectile movement, enemy movement, death check, etc..)
; We will put all our game update stuff in here, then display at the end of the loop ("draw" label)
update_next_frame







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


    jmp     loop            ; go back to very top of while loop

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
