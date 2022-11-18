    processor 6502

;   KERNAL [sic] routines
CHROUT =    $ffd2
CHRIN  =    $ffcf
CURKEY =    $00c5
;   ZERO PAGE MACROS
X_POS  =    $21     ; ZP 0x21: player X location (0 <= X_POS <= 20) x = 0 is leftmost tile.
Y_POS  =    $22     ; ZP 0x22: player Y location (0 <= Y_POS <= 11) y = 0 is topmost tile.
X_TMP  =    $23     ; ZP 0x23: temp variable for X coordinate value. only used in index -> coordinate routine right now.
Y_TMP  =    $24     ; ZP 0x24: temp variable for Y coordinate value. only used in index -> coordinate routine right now.
STATUS =    $25     ; ZP 0x25: player character status. [god mode flag][2bit prev ladder symbol][2bit life][3bit counter for falling down]

C_COL           =    $30     ; current column variable (which column are we on?)
MAP_READ_PTR    =    $31     ; current map storage pointer. the one we read new cols from.
; MAP_READ_PTR    =    $31     ; current map storage pointer. the one we read new cols from.

MAP     =    $1b04   ; map array pointer.
TMP_COL =    $1afc   ; where column is loaded when map array is read

    org     $1001
    dc.w    stubend
    dc.w    12345
    dc.b    $9e, "4109", 0 
stubend
    dc.w    0
; --------- Code for Compressed Title before adjusting resolution ------------
; Custom compression. modified RLE scheme to not store the first and last blocks of spaces.
; This allows us to have a writable region of less than a byte, which gets rid of a lot of unnecessary 
; duplicate code we had to account for the screen memory space spanning more than a byte.
; this scheme, however, needs a white screen fill before the decompression takes place, but in the end it ends up being more efficient. 
title 
    jsr     title_screen_init

; decode the screen data and put it into screen memory
rle_decode
    ldx     #0              ; store 0 in x, use it as index for Read loop
    ldy     #0              ; use Y as counter for the write loop
    stx     $25             ; store x into zp location for later
    pha                     ; push something because first line is pull from stack
decode_loop
    pla
    ldx     $25             ; load x into zp location for later
    lda     data_label,X         ; lda will turn zero flag on if it loaded zero (termination)
    beq     wait_for_z            ; if reached termination, exit
    pha                     ; A to stack
    cmp     #$20             ; Check if is a continuous space
    bne     case_not_space  ;
case_continuous_space       ;if so, then load the number of space
    inx     ; x++
    lda     data_label,X         ; get the loop amount in A
    jmp     number_of_times
case_not_space
    lda     #1  ;      Case 1 time only.
number_of_times
    inx     ; increment X in anticipation of next loop
    stx     $25             ; store x into zp location for later
    tax     ; how many time to repeat char into X

write_rle
    cpx     #0  ; is loop done?
    beq     decode_loop      ; if it is, decode another char
    pla     ; bring value of character back into A
    sta     $1e60,Y          ; store char value at Y
    pha     ; store A back
    dex     ; decrement the amount of char repeats left
    iny     ; inc the screen mem address
    jmp     write_rle

wait_for_z
    lda     CURKEY ;test current key
    cmp     #33 ; Z in current key table
    beq     start_prg
    jmp     wait_for_z

start_prg
    jmp     start

title_screen_init

    lda     #$02
    ldx     #0
color_ram1 ; fill color ram 0x9600 to 0x96ff with red (02)
    sta     $9600,X
    inx
    bne     color_ram1

    lda     #$00    ; don't need to load x cause it is 0 now. x just wrapped around.
color_ram2 ; fill color ram 0x9700 to 0x97ff with black (00)
    sta     $9700,X
    inx
    bne     color_ram2

    lda     #32     ; same thing with X here.
titlewhitescreen
    sta     $1e00,X
    inx     
    bne     titlewhitescreen

    lda     #32     ; same thing with X here.
titlewhitescreen2
    sta     $1f00,X
    inx     
    bne     titlewhitescreen2

    rts

data_label
    HEX 12 05 10 05 0E 14 3A 20 1E 01 0E 60 05 13 03 01 
    HEX 10 05 60 06 12 0F 0D 60 08 05 0C 0C 20 BA 10 12 
    HEX 05 13 13 60 1A 00 
; ----------------- Code for Title ends -----------------------------

start
    lda     #0 
    ldx     #0 
    ldy     #0
    jsr     init

;   loop to set the bottom row of the characters to character 00001, written to map array
    lda     #1          ; selecting char 1
    ldx     #0          ; loop counter
draw_ground             ; fills the bottom row (0x1bff - 21) with "ground" character (01 right now)
    sta     $1beb,X     
    inx     
    cpx     #21    ; 
    bne     draw_ground

   ;   loop to set the bottom row of the characters to character 00001, written to map array
    lda     #5          ; selecting char 1
    ldx     #0          ; loop counter
add_clouds              ; puts clouds in chars 0 to 21
    sta     MAP,X     
    inx     
    cpx     #21    ; 
    bne     add_clouds 

    ; init sad guys (for testing)
    lda     #3
    ldx     #101
    sta     MAP,X     
    ldx     #142
    sta     MAP,X     
    ldx     #208
    sta     MAP,X     
    ldx     #82
    sta     MAP,X     
;test: draw ladders
    jsr     draw_ladder_test

loop
refresh_life_count
    ldx     #21
    lda     #0
refresh_life_count_loop
    sta     MAP,X
    inx
    cpx     #27
    bne     refresh_life_count_loop
refresh_player_position
    jsr     player_pos_to_tmp   ; store player position into the temporary positions
    jsr     coord_to_index      ; get index into map array inside X_reg
    lda     STATUS
    and     #%01100000          ;clear unnessary bits
refresh_player_position_check_if_ladder
    cmp     #%00100000                  ;is ladder stored?
    bne     refresh_player_position_check_if_connector
    lda     #6
    jmp     refresh_player_position_draw_prev_cell
refresh_player_position_check_if_connector
    cmp     #%01000000                  ;is ladder connector stored?
    bne     refresh_player_position_air
    lda     #7         ; store 0 (whitespace) in where the player currently is,[updated based on status]
    jmp     refresh_player_position_draw_prev_cell
refresh_player_position_air
    lda     #0                  ; store 0 (whitespace) in where the player currently is,[updated based on status]
refresh_player_position_draw_prev_cell
    sta     MAP,X             ; in case the player position changes. If it doesn't,
                                ; that is okay, since we will restore the same value in update_next_frame.
refresh_player_position_clear; clear the status correspoinding bits
    lda     STATUS
    and     #%10011111
    sta     STATUS                            
;-----
; GET PLAYER INPUT!
;-----
    lda     CURKEY       ; loads the current pressed key from memory
    cmp     #10         ; if R is pressed, restart whole program
    beq     start
    cmp     #17         ; if A is pressed
    beq     a_left
    cmp     #18         ; if D is pressed
    beq     d_right
    cmp     #9          ; if W is pressed
    beq     w_top
    cmp     #41         ; if S is pressed
    beq     s_down
    cmp     #20         ; if J is pressed
    beq     j_shoot
    cmp     #50         ;T for Test purpose
    beq     test_code
    cmp     #13
    beq     p_scroll
    jmp     update_next_frame

exit_prg
    rts
test_code       ;[temporary code]
    ; init sad guys (for testing)
    lda     #3
    ldx     #101
    sta     MAP,X     
    ldx     #142
    sta     MAP,X     
    ldx     #208
    sta     MAP,X     
    ldx     #82
    sta     MAP,X     
    jmp     update_next_frame
j_shoot
    lda     X_POS               ; load X_POS into A_reg
    cmp     #20                 ; cannot spawn projectil if we are on the rightmost block
    beq     label_j   ; skip if x == 20
    ; store projectile in player x + 1
    inx                         ;X load in refresh_player_position
    lda     MAP,X
    cmp     #0                 ;check if air []
    bne     update_next_frame   ;cannot swan if projectil not in air []
    lda     #%01100100          ; char 4 is projectile, timer set to 100 so that it advances quicker when just fired
    sta     MAP,X             ; store projectile at current index + 1
    dex                         ; decrement X to bering its value back
label_j
    jmp     update_next_frame   ; leave
p_scroll
    jsr     scroll_one_column
    jmp     update_next_frame
a_left
    ldy     #$0                 ;check move left flag
    jsr     check_legal_move
    tya
    cmp     #$1
    bne     others
    dec     X_POS               ; X_POS -= 1
    jsr     parallax_left
    jmp     key_pressed
d_right
    ldy     #$1                 ;check move right flag
    jsr     check_legal_move
    tya
    cmp     #$1
    bne     others
    inc     X_POS               ; X_POS += 1
    jsr     parallax_right
    jmp     key_pressed
w_top
    ldy     #$2                 ;check move up flag
    jsr     check_legal_move
    tya
    cmp     #$1
    bne     others
    dec     Y_POS               ; Y_POS -= 1
    jmp     key_pressed
s_down
    ldy     #$3                 ;check move down flag
    jsr     check_legal_move
    tya
    cmp     #$1
    bne     others
    inc     Y_POS               ; Y_POS += 1 ; no jump here as we just fall through..
    jmp     key_pressed
others
    jmp     update_next_frame   ;other cannot move cases

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
    jsr     player_pos_to_tmp   ; put players coords into X_TMP, Y_TMP ZP vars
    jsr     coord_to_index      ; compute player pos offset in map array (returned in X_reg)
    lda     MAP,X               ;load the exist map element
    ;check existed element on map of the target cell
update_next_frame_check_exist_ladder
    cmp     #6       ;is a ldder
    bne     update_next_frame_check_exist_ladder_connector
    ;exist ladder in the target cell
    lda     STATUS
    ora     #%00100000      ;*01*****; store ladder to status
    sta     STATUS
    jmp     update_next_frame_draw_man_over_ladder
update_next_frame_check_exist_ladder_connector
    cmp     #7          ;is ladder ground connector
    bne     update_next_frame_exist_air
    ;exist ladder ground connector in target cell
    lda     STATUS
    ora     #%01000000      ;*10*****; store ladder connector to status
    sta     STATUS
update_next_frame_draw_man_over_ladder
    lda     #8
    jmp     update_next_frame_player
update_next_frame_exist_air
    lda     #2       ; player char ptr into A_reg[player chars?basd on ladder]
update_next_frame_player
    sta     MAP,X             ; store it at the player's position

    ;   showing number of lifs available at the top
    ;   loop to set the bottom row of the characters to character 00001, written to map array
    clc
    jsr     load_life
    sta     $0
    adc     $0      ;loop step 2,double to get the maximum
    adc     #21     ;start from line 2
    sta     $0
    ldx     #21
add_life_symbol              ; puts add life symbols in line2
    lda     #9              ;a life symol
    sta     MAP,X
    inx
    lda     #0          ;a white space
    inx
    cpx     $0;
    bne     add_life_symbol

falling_event
    lda     X_POS
    sta     X_TMP
    lda     Y_POS
    sta     Y_TMP
    inc     Y_TMP
    jsr     coord_to_index 
    lda     MAP,X
    cmp     #0      ;check the cell below player
    beq     falling_counting
    lda     STATUS ;if the cell below is not air ,reset falling timer to 0
    and     #%11111000
    sta     STATUS
    jmp     proj_event
falling_counting 
    lda     STATUS
    and     #%00000111 ;load the timer value
    cmp     #%1   ;chceck if it reached the timer
    beq     falling_falls ;fall player if timer reach
    inc     STATUS  ;if not, increase timer by 1 
    jmp     proj_event
falling_falls
    dec     Y_TMP
    jsr     coord_to_index
    lda     #0             ;load air
    sta     MAP,X          ;store the air to the position before
    inc     Y_POS
    lda     STATUS
    and     #%11111000 ;reset the timer back to 000
    sta     STATUS

proj_event
    ;   important: do not destroy value of X during the tile updates. we are using it to iterate thru the array.
    ;   if using X for something, store it and return it back (for example on stack) (or try Y first)
    ldx     #0              ; loop counter init (count to 0xFC)
update_map_loop
    lda     MAP,X         ; load value of (0x1B04 + X) into A_reg
                            ; bitmask 0 from bits 765 to get the character array index
    tay                     ; save A_reg in Y, because we are going to destroy the original A_reg next line
    and     #%00011111      ; get the the least significant 5 bits in A_reg (char index)
    sta     $09             ;store the projectile type for futrue useage
    cmp     #%00000100              ; is the object a projectile?
    beq     proj_update     ; update the projectile if it is one!
    cmp     #%00000011          ;if this an enemy charging  UNCOMMENT THESE 2 LINES TO MAKE ENEMY CHARGE AGAIN!
    beq     proj_update
post_proj_update
    inx                         
    cpx     #$FC            ; is X == 0xFC?
    bne     update_map_loop ; if not, update next character. If X == 0xFC, fall through.
    jmp     draw            ; go to draw section

proj_update                  
    tya                         ; restore A_reg
    and     #%11100000          ; get the the most significant 3 bits in A_reg (proj timer)
    cmp     #%11100000          ; is the proj timer == 111?
    beq     proj_move           ; if so, move proj

                                ; else, add 1 to the last 3 bits, and store back to same tile.
    tya                         ; restore A_reg AGAIN (we ruined it by bitmasking)
    clc                         ; clear carry before add
    adc     #%00100000          ; add "%00100000" to increase the bytes 765 value by "1"
    sta     MAP,X             ; store the updated value into corresponding map array position.
    jmp     post_proj_update    ; go back to loop .

proj_move                       ; MOVE the projectile if its time has come (bits 765 == 111)
    txa                         ; transfer index (offset) to A in order to retrieve coordinates via routine
    jsr     index_to_coord      ; call routine, get (x, y) of index in X_TMP, Y_TMP
    lda     X_TMP               ; load X value of projectile coordinate for comparison
    cmp     #20                 ; if x == 20, we are on edge of map. delete the projectile. 
    beq     proj_gone
    cmp     #0
    beq     proj_gone           ; if x == 0, we are on edge of map. delete the projectile. 

    ; is the projectile going to hit something? check what is in front of projectile.
    ;else, x+=1, and store that.
    ldy     $09
    cpy     #$04
    beq     proj_right_move
    cpy     #$03
    beq     proj_left_move
proj_left_move
    dex     ; get index of tile left in front of projectile
    jmp     proj_check 
proj_right_move
    inx                         ; get index of tile right in front of projectile
proj_check
    lda     MAP,X               ; load value of (0x1B04 + X) into A_reg
    and     #%00011111          ; bitmask 765 for comparison
    cpy     #$03
    beq     proj_check_shoting_player
proj_check_shoting_enemy
    cmp     #%00000011          ; is it an enemy (char 3)
    beq     proj_hit_enemy            ; projectile hit an enemy
    jmp     proj_check_in_air
proj_check_shoting_player
    cmp     #%00000010          ; is it a player (char 2)
    beq     proj_hit_player
proj_check_in_air
    cmp     #%0                 ; projectile in air? [updated]
    bne     proj_kill           ; kill projectile if it crash somthing else [updated]
proj_update_to_map
    cpy     #$03
    beq     proj_update_to_map_shoting_player
proj_update_to_map_shoting_enemy
    ; else, advance projectile one tile forward (X is still incremented right now!!)
    lda     #%00000100          ; load projectile with timer 0 into A_reg
    sta     MAP,X               ; store projectile into the tile to the right of it
    dex                         ; set X back
    jmp     proj_upate_to_map_cleared_existed_projecticle
proj_update_to_map_shoting_player
    lda      #%00000011         ; load projectile with timer 0 into A_reg
    sta      MAP,X              ; store projectile into the tile to the left of it
    inx                         ; set X back
proj_upate_to_map_cleared_existed_projecticle
    lda     #0               
    sta     MAP,X               ; write 0 to where projectile just was
    jmp     post_proj_update    ; go back to loop .

proj_hit_enemy
    lda     #0                  
    sta     MAP,X               ; write 0 to enemy location (kill it)
proj_kill
    cpy     #$03
    beq     proj_kill_to_player
proj_kill_to_enemy
    dex                         ; restore X original value
                                ; then, fall through, and kill projectile too
    jmp     proj_gone
proj_kill_to_player
    inx
    jmp     proj_gone
proj_hit_player
    ldy     #$0
    jsr     event_life          ;make player lose life by one
    inx                         ;fall back  in order to remove proj
proj_gone
    lda     #0      
    sta     MAP,X               ; store 0 – whitespace, into where the projectile location.
    jmp     post_proj_update    ; go back to loop .



    jsr     player_pos_to_tmp   ; store player position into the temporary positions
    jsr     coord_to_index      ; get index into map array inside X_reg
    lda     #2
    sta     MAP,X             ; in case the player position changes. If it doesn't,

draw    ; label to jump to when we want to skip to rendering step from some reason

        ; loop that goes through all the 252 bytes of the map object array (0x1B04-0x1BFF) 
        ; and writes the corresponsing character to screen memory.
        ; does not really matter that we are drawing stuff every frame, vic is too fast anyway (i think)

    ; affter all the updates, the player might have been moved down by falling. Let's update the player coords in the map now.

    ldx     #0              ; loop counter init (count to 0xFC)
drawloop
    lda     MAP,X         ; load value of (0x1B04 + X) into A_reg
                            ; bitmask 0 from bits 765 to get the character array index
    and     #%00011111      ; get the the least significant 5 bits in A_reg (char index)
;   now that we have the character index in A_Reg, store that at corresponsing screen memory location
    sta     $1e00,X         ; use same offset (X) for screen memory storage.
    inx                         
    cpx     #$FC            ; is X == 0xFC?
    bne     drawloop        ; if not, draw next character. If X == 0xFC, fall through.


    ldx     #$30             ; run waste time X times
    jsr     waste_time

    jmp     loop            ; go back to very top of while loop

;*************************************
; ––––––––––––– ROUTINES:
;*************************************

coord_to_index              ; routine to compute offset from MAP from x, y value (array index)
                            ; Input: X value in X_TMP, Y value in Y_TMP ZP locations.
                            ; Output: index in X register
                            ; Modifies: A_reg, X_reg
                            ; formula is index(x,y) = 21y + x 
                            
    ldx     Y_TMP           ; load Y_POS into X, use as loop counter when adding
    lda     #0
y_mult_loop                 ; loop to compute A_reg = 21 * X
    cpx     #0              ; if x is 0, quit multiplication loop
    beq     add_x           
    clc                     ; clear carry before add
    adc     #21             ; add 21
    dex                     ; decrement X
    jmp     y_mult_loop     ; go to top of loop
add_x
    clc                     ; clc before add
    adc     X_TMP           ; A_reg += Y_POS
    tax                     ; return in X for convenience 
                            ; (all the times we want to use it, we use it as an offset (ex: sta 00,X)
    rts                     ; return from routine


index_to_coord              ; routine to compute (x, y) coordinates and store them in X_TMP, Y_TMP 
                            ; Input: map array index in A_reg
                            ; Output: X value in X_TMP, Y value in Y_TMP ZP locations.
                            ; Modifies: A_reg, Y_reg
    ldy     #0              ; use Y_reg to count the Y coordinate
y_div_loop    
    cmp     #21             ; is A_reg less than 21?
    bcc     remainder       ; if it is, done with loop
    iny                     ; add 1 to the y coordinate counter
    sec                     ; SET carry before subtraction (reverse (stupid)) !!!!!!
    sbc     #21             ; A_reg -= 21
    jmp     y_div_loop      ; repeat check now that A_reg is smaller
remainder                   ; when we are here, we just have the remainder (x coordinate) left in A_reg
    sta     X_TMP           ; store X coordinate in corresponding zero page location
    STY     Y_TMP           ; store Y coordinate in corresponding zero page location
    rts                     ; return from routine

    ;       saves memory compared to copy pasting the code, if used more than 2 times in whole program.
player_pos_to_tmp
    ldx     X_POS
    STX     X_TMP
    ldx     Y_POS
    STX     Y_TMP
    rts

; routine to waste time. changes the Y register. input: amount of outer loops in X register.
waste_time
waste_time_loop_outer
    ldy     #0
waste_time_loop_inner             
    iny
    cpy     $FF             ; waste time by counting up to 255 in Y reg
    bne     waste_time_loop_inner
    dex     
    cpx     #0
    bne     waste_time_loop_outer
    rts

;--------
;check if allow movment horizontal
;allow move left/right: if left/right is ladder or air
;allow to move up; if currently on a ladder
;   (dont do stupid things like build a ladder to celling )
;allow to move down; if and only if below is ladder
;read:  Y = 0 check if allow move left
;       Y = 1 check if allow move right
;       Y = 2 check if allow move up
;       Y = 3 check iff allow move down                     
;output: set Y to 1 if leagl move, or Y to 0 illegal move, or set Y to FF to be a error flag.
;-------
check_legal_move
    lda     X_POS
    sta     X_TMP
    lda     Y_POS
    sta     Y_TMP
    tya
check_legal_move_horizontal
    cmp     #$0
    beq     check_legal_move_left
    cmp     #$1
    beq     check_legal_move_right
    cmp     #$2
    beq     check_legal_move_up
    cmp     #$3
    beq     check_legal_move_down
    ldy     #$FF
    rts     ;others, destory Y.
check_legal_move_left
    dec     X_TMP
    jmp     check_legal_move_horizontal_chceck 
check_legal_move_right
    inc     X_TMP
check_legal_move_horizontal_chceck
    jsr     coord_to_index
    lda     MAP,X
    cmp     #0     ;is air?
    beq     check_legal_move_true
    cmp     #6     ;is ladder
    beq     check_legal_move_true
    jmp     check_legal_move_false
check_legal_move_down
    inc     Y_TMP   ;check one below
    jmp     check_legal_move_vertical_check
check_legal_move_up
check_legal_move_vertical_check
    jsr     coord_to_index
    lda     MAP,X
    cmp     #6      ;is a ladder []
    beq     check_legal_move_true
    cmp     #7      ;is a ladder connector? []
    beq     check_legal_move_true
check_legal_move_false
    ldy     #$0
    rts
check_legal_move_true
    ldy     #$1
    rts
;---------
;life events:add or lose life(trigger dead)
;if y ==0? lose life by one. If others, incrase life by 1.
;---------
event_life
    tya
    cmp     #$0 ;if a ==0? lose life by one. If others, incrase life by 1.
    beq     event_life_lose_life
event_life_increase_life
    jsr     load_life
    adc     #1                  ;add life by 1 (carry cleared in the previous step)
    cmp     #%00000100          ;check if life overflowed
    beq     event_life_end      ;maximum life reached, nothing to do
    tay
    jsr     set_life
    rts
event_life_lose_life
    jsr     load_life
    cmp     #$1
    beq     event_game_over     ;last life lost result in game over.
    tay
    dey
    jsr     set_life
    rts
event_life_end
    rts
;---
; function for displaying game over, still under construction
;---
event_game_over
    jmp     start

;---
; load life to accmulator
;---
load_life
    lda     STATUS
    and     #%00011000
    lsr
    lsr
    lsr
    rts

;---
; set life to status based on value in $0               ; TODO: NEED TO CHANGE TO MACRO
;---
set_life
    tya
    and     #%00000011
    asl
    asl
    asl
    sta     $0
    lda     STATUS
    and     #%11100111
    ora     $0
    sta     STATUS
    rts

;***********************************
; initialize the game
;***********************************
init            ; call routine in the beginning.

    ;       setting init values of player x y coords
    lda     #5
    sta     X_POS
    lda     #10
    sta     Y_POS

;   Switching character set pointer to 0x1c00:
    lda     #255
    sta     $9005 ; POKE 36869 255 (from book)

;   Switching to 8x16 character set (setting bit 0 of 0x9003 to 1) 
;   and setting number of rows on screen to 12 (setting bits 1-6 of 0x9003 to 12)
;   this gives us 252 or 0xFC characters displayed on screen at once - fits in one byte. (nice)
    lda     $9003           
    and     #%10000000      ; clear bits 0-6 
    ora     #%10011001      ; setting bit 0 to 1, bits 1-6 to 001100 (12)
    sta     $9003           ; store it
;setting columns # to 21: (bits 0-6 of 0x9002 control the # of col)
    lda     $9002
    and     #%10000000  ; clear bits 0-6
    ora     #21         ; set bits 0-6 to #21 (# of columns)
    sta     $9002

; clearing screen in beginning of game (writing char 00 to all 252 tiles)
    lda     #0          ; selecting char 0 
    ldx     #0          ; loop counter
    stx     C_COL       ; 0 initialized variables:
    stx     MAP_READ_PTR
whitescreen             ; probably change this to a JSRable function later 
    sta     MAP,X
    inx     
    cpx     #$FC    ; 252 (size of screen) the execution falls through when x == 252
    bne     whitescreen

;   Next, fill 0x9600 - 0x96FC (color RAM) with $00 (turn them all on, black color)
    lda     #$00
    ldx     #$00
color_ram 
    sta     $9600,X
    inx   
    cpx     #$FC
    bne     color_ram
    sta     $9600,X
status_init
    lda     #%00011000      ; iniital life of 3,other flags to be 0
    sta     STATUS

    rts                     ; get out of routine and go to the game loop


; routine to shift the parallax scrolling part of MAP (indices 0 to 20) one to the left. uses A and X registers.
parallax_left
    ldx     #0              ; init counter to to loop though 8 byte character
ror_loop

    lda     $1c50,X
    and     #%00000001      ; bit mask testing for last bit of the byte
    bne     ror_set         ; if zero flag is 0, need to set carry, else fall through

    clc                     ; clear carry cause bit was 0 (else)   
    jmp     ror_p2
ror_set                     ; set carry cause bit was 1 (if)
    sec

ror_p2
    ror     $1c50,X
    inx
    cpx     #16
    bne     ror_loop
    rts

; routine to shift the parallax scrolling part of MAP (indices 0 to 20) one to the right. uses A and X registers.
parallax_right                        ; if d was pressed, decrease frame
    ldx     #0              ; init counter to to loop though 8 byte character
rol_loop
    lda     $1c50,X            ; 1c50 is char 5 (clouds)
    and     #%10000000      ; bit mask testing for last bit of the byte
    bne     rol_set         ; if zero flag is 0, need to set carry, else fall through
    clc                     ; clear carry cause bit was 0 (else)   
    jmp     rol_p2
rol_set                     ; set carry cause bit was 1 (if)
    sec

rol_p2
    rol     $1c50,X
    inx     
    cpx     #16
    bne     rol_loop
    rts



;******
;test usage code
;*****
draw_ladder_test

    lda     #10
    sta     X_TMP
    lda     #10
    sta     Y_TMP
    jsr     coord_to_index
    lda     #6 ;ladder
    sta     MAP,X
    dec     Y_TMP
    jsr     coord_to_index
    lda     #6 ;ladder
    sta     MAP,X
    dec     Y_TMP
    jsr     coord_to_index
    lda     #6 ;ladder
    sta     MAP,X
    dec     Y_TMP
    jsr     coord_to_index
    lda     #6
    sta     MAP,X
    dec     Y_TMP
    jsr     coord_to_index
    lda     #6
    sta     MAP,X
    dec     Y_TMP
    jsr     coord_to_index
    lda     #6
    sta     MAP,X
    dec     Y_TMP
    jsr     coord_to_index
    lda     #7  ;connector
    sta     MAP,X
    dec     X_TMP
    jsr     coord_to_index
    lda     #1 ;ground
    sta     MAP,X
    dec     X_TMP
    jsr     coord_to_index
    lda     #1 ;ground
    sta     MAP,X
    dec     X_TMP
    jsr     coord_to_index
    lda     #1 ;ground
    sta     MAP,X
    dec     X_TMP
    jsr     coord_to_index
    lda     #1 ;ground
    sta     MAP,X
    dec     X_TMP
    jsr     coord_to_index
    lda     #1 ;ground
    sta     MAP,X
    dec     X_TMP
    jsr     coord_to_index
    lda     #1 ;ground
    sta     MAP,X
    dec     X_TMP
    jsr     coord_to_index
    lda     #1 ;ground
    sta     MAP,X
    dec     X_TMP
    jsr     coord_to_index
    lda     #1 ;ground
    sta     MAP,X
    dec     X_TMP
    jsr     coord_to_index
    lda     #1 ;ground
    sta     MAP,X
    lda     #12
    sta     X_TMP
    lda     #10
    sta     Y_TMP
    jsr     coord_to_index
    lda     #1 ;ground
    sta     MAP,X
    rts




;   read column function: reads a column in from the map array and puts it into TMP_COL array.
read_column        ; routine to read the next column in from the map.
                            ; Input: none
                            ; Output: TMP_COL[8] will contain the characters of the column just read
                            ; Modifies: A_reg, Y_reg, X_reg
    ; first fill with air:
    lda     #0
    tay     ;y as counter to go through 0 to 7 y value!
col_blank_fill
    sta     TMP_COL,Y
    iny
    cpy     #8
    bne     col_blank_fill
    ldx     MAP_READ_PTR ; load current column array index into X_reg 
read_col_loop
    lda     map1,X     ; read from map storage at index s
    beq     read_col_done
    cmp     #$FF
    beq     read_col_done
    ; else, check what we just loaded and put it in correct location.
    lsr
    lsr     ; DIVIDE BY 32 ! HAHA!  (only takes 5 bytes)
    lsr     ; since y value is stored in bits 765, we need to put it 
    lsr     ; in bits 210 so that we can use it as an INDEX!!
    lsr
    tay     ;; store that in Y
    lda     map1,X     ; read same thing into A again
    and     #%00011111  ; bit mask out the 765 bits (we don't need them anymore)
    sta     TMP_COL,Y   ; store the char value at Y that we just Read
    inx     
    jmp     read_col_loop
read_col_done
    inx     ; inx so that we have increased x for next read
    stx     MAP_READ_PTR
    rts ; return from routine


write_col_to_x_on_screen        ; Routine to write the column read from map encoding,
                                ;         onto main map array at a certain x value.
                                ;
                                ; Input:  a column loaded in at TMP_COL via the read_column routine,
                                ;         X_reg - the value on screen to write the temp column to.
                                ; Output: none
                                ; Modifies: A_reg, Y_reg, X_reg
    ldy     #0 
    ; tax     
    ; clc                         ; clear carry before add. adding 63 (3x21) to it
    ; adc     #63                 ; now X_reg holds the index of where first tile should Go
    ldx     #104
write_col_loop
    lda     TMP_COL,Y
    sta     MAP,X
    txa     
    clc                         ; clear carry before add.
    adc     #21                 ; add 21 to go to next line !!!!
    tax
    iny                         ; y++
    cpy     #8                  ; if 8 POG!!! get out of function we are done
    beq     write_col_done      ; get out rts
    jmp     write_col_loop      ; 
write_col_done
    rts

scroll_one_column               ; Routine to move all columns to the left by 1, 
                                ; destroy the leftmost column, and insert a column on the left.
                                ;         
                                ; Input:  none
                                ;        
                                ; Output: none
                                ; Modifies: A_reg, Y_reg, X_reg

    ; first, move all entries by 1 to the left.
    ; starting from tile with index 63, as top three tiles are not scrolled.
    ldx     #84
scroll_one_col_shift_loop ; this loop applies MAP[X] = MAP[X + 1] (C syntax)
    inx             ; get the address of the tile to the right of current tile
    lda     MAP,X   ; load teh tile in front of us into A_reg
    dex             ; put X_reg back to current tile index
    sta     MAP,X   ; store it
    inx             ; increase X for next iteration 
    cpx     #251    ; if we are at last tile, fall through. else, jump back up.
    bne     scroll_one_col_shift_loop
    
    dec     X_POS
    lda     X_POS           ; load X_POS into A_reg (this sets the negative flag if it was negative!)
    bmi     neg_x_scroll_func          ; branch if X_POS is negative, i.e X_POS = FF because we just did X_POS = 0 - 1
    jmp     x_notneg
neg_x_scroll_func
    inc     X_POS           ; no jmp here, just fall through.
x_notneg    

    jsr     read_column         ; read a column and place it into TMP_COL[8] array
    jsr     write_col_to_x_on_screen    ; put new column into rightmost column......... lets hope it works.........
    rts


/*
;   END OF CODE, START OF DATA
*/


; MAP 1 ENCODING:
map1          
    HEX 00 e1 00 a6 c6 e1 00 a1 e1 00 83 a1 e1 00 66 86 
    HEX a1 e1 00 61 00 61 00 61 e1 00 83 a1 e1 00 83 a1 
    HEX e1 00 e1 00 e1 00 e1 00 c3 e1 00 c3 e1 00 e1 00 
    HEX e1 00 e1 00 a6 c6 e1 00 a1 e1 00 a1 e1 00 a1 e1 
    HEX 00 26 46 66 86 a1 e1 00 21 e1 00 03 21 e1 00 21 
    HEX e1 00 03 21 e1 00 03 21 e1 00 26 46 00 41 e1 00 
    HEX 23 41 e1 00 23 41 e1 00 46 66 00 61 a1 e1 00 43 
    HEX 61 a1 e1 00 43 61 a1 e1 00 61 a1 e1 00 86 a6 c6 
    HEX e1 00 e1 00 c3 e1 00 c3 e1 00 c3 e1 FF
;     HEX 00 e1 00 a6 c6 e1 00 a1 e1 00 83 a1 e1 00 66 86 
;     HEX a1 e1 00 61 00 61 00 61 e1 00 83 a1 e1 00 83 a1 
;     HEX e1 00 e1 00 e1 00 e1 00 c3 e1 00 c3 e1 00 e1 00 
;     HEX e1 00 e1 00 a6 c6 e1 00 a1 e1 00 a1 e1 00 a1 e1 
;     HEX 00 26 46 66 86 a1 e1 00 21 e1 00 03 21 e1 00 21 
;     HEX e1 00 03 21 e1 00 03 21 e1 00 26 46 00 41 e1 00 
;     HEX 23 41 e1 00 23 41 e1 00 46 66 00 61 e1 00 43 61 
;     HEX e1 00 43 61 e1 00 61 e1 00 86 a6 c6 e1 00 e1 00 
;     HEX c3 e1 00 c3 e1 00 c3 e1 FF


    ; map array. in case we want to have something there at game start,
    ; put that here (probably will not use, because then we cannot replay the game)
    org     MAP          
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

    ;       CHAR 04 - projectile placeholder 
    dc.b    #%00000000
    dc.b    #%01110000
    dc.b    #%01111000
    dc.b    #%01111100
    dc.b    #%00001110
    dc.b    #%00000111
    dc.b    #%00000011
    dc.b    #%11110011
    dc.b    #%11111111
    dc.b    #%11110011
    dc.b    #%00000111
    dc.b    #%00001110
    dc.b    #%00011110
    dc.b    #%01111110
    dc.b    #%01111100
    dc.b    #%00000000


    ;       CHAR 05 - "cloud" 

    dc.b    #%00000000
    dc.b    #%00000000
    dc.b    #%00000000
    dc.b    #%00000000
    dc.b    #%00111000
    dc.b    #%01000100
    dc.b    #%10000010
    dc.b    #%00000001
    dc.b    #%01000001
    dc.b    #%00011110
    dc.b    #%01111110
    dc.b    #%01011100
    dc.b    #%00000000
    dc.b    #%00000000
    dc.b    #%00000000
    dc.b    #%00000000

    ;       CHAR 06 - "ladder"
    dc.b    #%11000011
    dc.b    #%11000011
    dc.b    #%11111111
    dc.b    #%11111111
    dc.b    #%11000011
    dc.b    #%11000011
    dc.b    #%11111111
    dc.b    #%11111111
    dc.b    #%11000011
    dc.b    #%11000011
    dc.b    #%11111111
    dc.b    #%11111111
    dc.b    #%11000011
    dc.b    #%11000011
    dc.b    #%11111111
    dc.b    #%11111111
    
    ;      CHAR 07 -'ladder ground connector'

    dc.b    #%11111101    
    dc.b    #%11011010
    dc.b    #%10010111
    dc.b    #%11011101 
    dc.b    #%10111111
    dc.b    #%11000011
    dc.b    #%11000011
    dc.b    #%11111111
    dc.b    #%11111111
    dc.b    #%11000011
    dc.b    #%11000011
    dc.b    #%11111111
    dc.b    #%11111111
    dc.b    #%11000011
    dc.b    #%11000011
    dc.b    #%11111111

    
    ;       CHAR 08 a man over a ladder
    dc.b    #%11111111
    dc.b    #%11111111
    dc.b    #%11111111
    dc.b    #%11111111
    dc.b    #%11111111
    dc.b    #%11111111
    dc.b    #%11111111
    dc.b    #%11111111
    dc.b    #%11111111
    dc.b    #%11111111
    dc.b    #%11111111
    dc.b    #%11111111
    dc.b    #%11111111
    dc.b    #%11111111
    dc.b    #%11111111
    dc.b    #%11111111

    ;       CHAR 09 HEART
    dc.b    #%00000000
    dc.b    #%00000000
    dc.b    #%01111110
    dc.b    #%10000001
    dc.b    #%01111110
    dc.b    #%00000000
    dc.b    #%01100110
    dc.b    #%11111111
    dc.b    #%11111111
    dc.b    #%11111111
    dc.b    #%01111110
    dc.b    #%00111100
    dc.b    #%00011000
    dc.b    #%00000000
    dc.b    #%00000000
    dc.b    #%00000000
