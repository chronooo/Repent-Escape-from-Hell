    processor 6502

; KERNAL [sic] routines
CHROUT =    $ffd2
CHRIN  =    $ffcf

    org     $1001

    dc.w    stubend
    dc.w    12345
    dc.b    $9e, "4109", 0 

stubend
    dc.w    0

start

    lda     #32
    ldx     #0
whitescreen
    STA     $1e00,X
    INX     

    cpx     #255
    BNE     whitescreen
    STA     $1e00,X

    lda     #32
    ldx     #0
whitescreen2
    STA     $1f00,X
    INX     

    cpx     #255
    BNE     whitescreen2
    STA     $1f00,X

;Setting black for color ram and setting up loop counter
    lda     #$00 
    ldx     #0

colorRamFill ; fill color ram 0x9600 to 0x96ff with black (00)
    STA     $9600,X
    INX
    cpx     #255
    BNE     colorRamFill
    STA     $9600,X
    lda     #$00
    ldx     #0

colorRamFill2 ; fill color ram 0x9700 to 0x97ff with black (00)
    STA     $9700,X
    INX
    cpx     #255
    BNE     colorRamFill2
    STA     $9700,X
    lda     #$00
    ldx     #0

;REPENT text
;print R
    lda     #$02 ; Red
    STA     $9660 ; color ram location for this
    lda     #$12 ;screen code for R 
    STA     $1e60; screen location 

;print E
    lda     #$02 ; Red
    STA     $9661 ; color ram location for this
    lda     #$05 ;screen code for E
    STA     $1e61; screen location 

;print P
    lda     #$02 ; Red
    STA     $9662 ; color ram location for this
    lda     #$10 ;screen code for P
    STA     $1e62; screen location 

;print E
    lda     #$02 ; Red
    STA     $9663 ; color ram location for this
    lda     #$05 ;screen code for E
    STA     $1e63; screen location 

;print N
    lda     #$02 ; Red
    STA     $9664 ; color ram location for this
    lda     #$0E ;screen code for N
    STA     $1e64; screen location 

;print T
    lda     #$02 ; Red
    STA     $9665 ; color ram location for this
    lda     #$14 ;screen code for T
    STA     $1e65; screen location 

;print : (unsure of whether to keep this red or black)
    ;lda     #$02 ; Red
    ;STA     $9666 ; color ram location for this
    lda     #$3A ;screen code for :
    STA     $1e66; screen location 

;AN text
;print A
    lda     #$02 ; Red
    STA     $9685 ; color ram location for this
    lda     #$01 ;screen code for A 
    STA     $1e85; screen location 

;print N
    lda     #$02 ; Red
    STA     $9686 ; color ram location for this
    lda     #$0E ;screen code for N
    STA     $1e86; screen location 

;Escape text
;print E
    lda     #$02 ; Red
    STA     $9688 ; color ram location for this
    lda     #$05 ;screen code for S 
    STA     $1e88; screen location 

;print S
    lda     #$02 ; Red
    STA     $9689 ; color ram location for this
    lda     #$13 ;screen code for S 
    STA     $1e89; screen location 

;print C
    lda     #$02 ; Red
    STA     $968a ; color ram location for this
    lda     #$03 ;screen code for C 
    STA     $1e8a; screen location 

;print A
    lda     #$02 ; Red
    STA     $968b ; color ram location for this
    lda     #$01 ;screen code for A 
    STA     $1e8b; screen location 

;print P
    lda     #$02 ; Red
    STA     $968c ; color ram location for this
    lda     #$10 ;screen code for P 
    STA     $1e8c    ; screen location   

;print E
    lda     #$02 ; Red
    STA     $968d ; color ram location for this
    lda     #$05 ;screen code for E 
    STA     $1e8d ; screen location      

;From text

;print F
    lda     #$02 ; Red
    STA     $968f ; color ram location for this
    lda     #$06 ;screen code for F 
    STA     $1e8f; screen location 

;print R
    lda     #$02 ; Red
    STA     $9690 ; color ram location for this
    lda     #$12 ;screen code for R 
    STA     $1e90; screen location 

;print O
    lda     #$02 ; Red
    STA     $9691 ; color ram location for this
    lda     #$0F ;screen code for O 
    STA     $1e91; screen location 

;print M
    lda     #$02 ; Red
    STA     $9692 ; color ram location for this
    lda     #$0D ;screen code for M 
    STA     $1e92; screen location          

;Hell text

;print H
    lda     #$02 ; Red
    STA     $9694 ; color ram location for this
    lda     #$08 ;screen code for H 
    STA     $1e94 ; screen location   

;print E
    lda     #$02 ; Red
    STA     $9695 ; color ram location for this
    lda     #$05 ;screen code for E 
    STA     $1e95  

;print L
    lda     #$02 ; Red
    STA     $9696 ; color ram location for this
    lda     #$0C ;screen code for L 
    STA     $1e96  

;print L
    lda     #$02 ; Red
    STA     $9697 ; color ram location for this
    lda     #$0C ;screen code for L 
    STA     $1e97  

;Press Z text
;print P
    lda     #$10 ;screen code for P 
    STA     $1f52    ; screen location 

;print r
    lda     #$12 ;screen code for r 
    STA     $1f53    ; screen location 

;print e
    lda     #$05 ;screen code for e 
    STA     $1f54    ; screen location 

;print s
    lda     #$13 ;screen code for s 
    STA     $1f55    ; screen location 

;print s
    lda     #$13 ;screen code for s 
    STA     $1f56    ; screen location 

;print z
    lda     #$1A ;screen code for z 
    STA     $1f58    ; screen location 

loop
    lda     $00C5
    cmp     #64
    beq     loop
    cmp     #33 ; Z in current key table
    beq     exit_prg
    jmp     loop

exit_prg
    rts
    
