;-----------------------------------------MACROS-------------------------------------------------------------
EXT MACRO ;PRESS ANY KEY TO EXIT APPLICATION
                MOV  AH , 0
                INT  16h
                MOV         AH,4CH
                INT         21H
                ENDM        EXT

movecursor MACRO x,y ;move cursor
                mov         ah,2
                mov         dh,y
                mov         dl,x
                int         10h
                ENDM        movecursor

cin MACRO MyMessage ;cin STRING
                mov ah,0AH
                mov dx,offset MyMessage
                int 21h
                ENDM        cin

ShowMessage MACRO MyMessage ;PRINT STRING
                MOV         AH,9H
                MOV         DX,offset MyMessage
                INT         21H
                ENDM        ShowMessage

ShowCMessage MACRO MyMessage, X ;PRINT STRING WITH COLOR X
                local clp 
                LOCAL otc
                mov SI,offset MyMessage
                
           clp: 
                MOV AL,byte ptr [SI]
                cmp al,'$'
                je otc
                mov ah,9 ;Display
                mov bh,0 ;Page 0
                mov cx,1h ;1 times
                mov bl,X ;Green (A) on white(F) background
                int 10h
                inc SI
                
                cmp byte ptr [si],'$'
                je otc
                mov ah,3h
                mov bh,0h
                int 10h
                
                mov ah,2
                INC DL
                int 10h
                
                jmp clp
                otc: 
                mov ah,3h
                mov bh,0h
                int 10h
                
                mov ah,2
                INC DL
                int 10h
ENDM        ShowCMessage

TOSTRING MACRO OutMessage
    
    MOV SI,0
    divide:
        mov cl, 10D
        div cl         ; div number(in ax) by 10 
        add ah, '0'     ;Make into a character
        MOV BX,AX
        PUSH BX
        INC SI 
        MOV AH,0 
        cmp AL, 0
        jne divide
        
    mov di,offset OutMessage    
    MAKESTR:
    CMP SI,0
    JE OUTH
    DEC SI
    POP BX
    mov [di],BH
    INC DI
    JMP MAKESTR
    OUTH:
ENDM TOSTRING

DRAW MACRO imgdata,imgwidth,imgheight,X,Y       ;DRAW IMAGE
                LOCAL drawLoop
                LOCAL innerloop
                LOCAL skp
                
                mov cx,X
                mov dx,Y
                LEA BX,imgdata
        ; Drawing loop
                      mov di,0
        drawLoop:     
                      mov si,0
                      innerloop:
                      MOV  AL,[BX]
                      MOV AH,0ch
                      cmp al,0FFH
                      je skp
                      INT 10H
                      skp:
                      INC BX
                      INC CX
                      INC SI
                      CMP SI,imgwidth
                      JNE innerloop
                      SUB CX,SI
                      INC DX
                      INC DI
                      CMP DI,imgheight
                      JNE  drawLoop
                ENDM        DRAW

OpenFile MACRO  imgfilename, imgfilehandle                                ;OPEN FILE
                  MOV  AH, 3Dh
                  MOV  AL, 0                    ; read only
                  LEA  DX, imgfilename          ;GET NAME
                  INT  21h
                  MOV  [imgfilehandle], AX      ;GET HANDLE OF THE FILE
ENDM OpenFile

ReadData MACRO imgfilehandle ,imgwidth,imgheight,imgdata                                        ;READ FILE CONTENT
                  MOV      AH,3Fh
                  MOV      BX, [imgfilehandle]
                  MOV      CX,imgwidth*imgheight         ; number of bytes to read
                  LEA      DX, imgdata                   ;PUT DATA IN IMGDATA
                  INT      21h
ENDM ReadData

CloseFile MACRO imgfilehandle                                          ;CLOSE FILE
                  MOV      AH, 3Eh
                  MOV      BX, [imgfilehandle]
                  INT      21h

ENDM CloseFile

DRAWCELL MACRO X,Y,Z       ;DRAW colored CELL WITH COLOR Z
        LOCAL drawLoop
        LOCAL innerloop
                
        mov cx,X
        mov dx,Y
        ; Drawing loop
        mov di,0
        drawLoop:     
            mov si,0
            innerloop:
            MOV  AL,Z
            MOV AH,0ch
            INT 10H
            INC BX
            INC CX
            INC SI
            CMP SI,60D
            JNE innerloop
            SUB CX,SI
            INC DX
            INC DI
            CMP DI,60D
            JNE  drawLoop
ENDM        DRAWCELL

DrawGrid MACRO X,Y,A,B                                           ;DRAW WHITE GRID
        LOCAL BIGGERLOOP
        LOCAL BIGGERLOOP2
        LOCAL BIGGERLOOP3
        LOCAL BIGGERLOOP4
        LOCAL BIGGERLOOP5
        LOCAL BIGGERLOOP6
        LOCAL BIGGERLOOP7
        LOCAL BIGGERLOOP8

            MOV AX,X
            MOV BX,Y 
            MOV CX,0
            ADD AX,0D
            ADD BX,0D  
        BIGGERLOOP:
            PUSH AX
            PUSH BX
            PUSH CX
            DRAWCELL AX,BX,A
            POP CX
            POP BX
            POP AX
            PUSH AX
            PUSH BX
            PUSH CX
            ADD AX,60D
            DRAWCELL AX,BX,B
            POP CX
            POP BX
            POP AX
            ADD AX,120D
            INC CX
            CMP CX,4D
        JNE  BIGGERLOOP

            MOV AX,X
            MOV BX,Y 
            MOV CX,0
            ADD AX,60D
            ADD BX,60D
        BIGGERLOOP2:
            PUSH AX
            PUSH BX
            PUSH CX
            DRAWCELL AX,BX,A
            POP CX
            POP BX
            POP AX
            PUSH AX
            PUSH BX
            PUSH CX
            ADD AX,-60D
            DRAWCELL AX,BX,B
            POP CX
            POP BX
            POP AX
            ADD AX,120D
            INC CX
            CMP CX,4D
        JNE  BIGGERLOOP2

            MOV AX,X
            MOV BX,Y 
            MOV CX,0
            ADD AX,0D
            ADD BX,120D
        BIGGERLOOP3:
            PUSH AX
            PUSH BX
            PUSH CX
            DRAWCELL AX,BX,A
            POP CX
            POP BX
            POP AX
            PUSH AX
            PUSH BX
            PUSH CX
            ADD AX,60D
            DRAWCELL AX,BX,B
            POP CX
            POP BX
            POP AX
            ADD AX,120D
            INC CX
            CMP CX,4D
        JNE  BIGGERLOOP3

            MOV AX,X
            MOV BX,Y 
            MOV CX,0
            ADD AX,60D
            ADD BX,180D
        BIGGERLOOP4:
            PUSH AX
            PUSH BX
            PUSH CX
            DRAWCELL AX,BX,A
            POP CX
            POP BX
            POP AX
            PUSH AX
            PUSH BX
            PUSH CX
            ADD AX,-60D
            DRAWCELL AX,BX,B
            POP CX
            POP BX
            POP AX
            ADD AX,120D
            INC CX
            CMP CX,4D
        JNE  BIGGERLOOP4        

            MOV AX,X
            MOV BX,Y 
            MOV CX,0
            ADD AX,0D
            ADD BX,240D
        BIGGERLOOP5:
            PUSH AX
            PUSH BX
            PUSH CX
            DRAWCELL AX,BX,A
            POP CX
            POP BX
            POP AX
            PUSH AX
            PUSH BX
            PUSH CX
            ADD AX,60D
            DRAWCELL AX,BX,B
            POP CX
            POP BX
            POP AX
            ADD AX,120D
            INC CX
            CMP CX,4D
        JNE  BIGGERLOOP5

            MOV AX,X
            MOV BX,Y 
            MOV CX,0
            ADD AX,60D
            ADD BX,300D
        BIGGERLOOP6:
            PUSH AX
            PUSH BX
            PUSH CX
            DRAWCELL AX,BX,A
            POP CX
            POP BX
            POP AX
            PUSH AX
            PUSH BX
            PUSH CX
            ADD AX,-60D
            DRAWCELL AX,BX,B
            POP CX
            POP BX
            POP AX
            ADD AX,120D
            INC CX
            CMP CX,4D
        JNE  BIGGERLOOP6

            MOV AX,X
            MOV BX,Y 
            MOV CX,0
            ADD AX,0D
            ADD BX,360D
        BIGGERLOOP7:
            PUSH AX
            PUSH BX
            PUSH CX
            DRAWCELL AX,BX,A
            POP CX
            POP BX
            POP AX
            PUSH AX
            PUSH BX
            PUSH CX
            ADD AX,60D
            DRAWCELL AX,BX,B
            POP CX
            POP BX
            POP AX
            ADD AX,120D
            INC CX
            CMP CX,4D
        JNE  BIGGERLOOP7

            MOV AX,X
            MOV BX,Y 
            MOV CX,0
            ADD AX,60D
            ADD BX,420D
        BIGGERLOOP8:
            PUSH AX
            PUSH BX
            PUSH CX
            DRAWCELL AX,BX,A
            POP CX
            POP BX
            POP AX
            PUSH AX
            PUSH BX
            PUSH CX
            ADD AX,-60D
            DRAWCELL AX,BX,B
            POP CX
            POP BX
            POP AX
            ADD AX,120D
            INC CX
            CMP CX,4D
        JNE  BIGGERLOOP8 

ENDM DrawGrid

;(0,0),(0,1)
;(1,0)......
;      (7,7)
;
getDrawPosition MACRO ROW,COL ;Takes the row and col and set the dx to the required values to draw
MOV AL,ROW
MOV CL,60D
MUL CL
MOV dx,AL
MOV AL,COL
MUL CL
MOV CX,AL 
ENDM getDrawPosition   

.MODEL SMALL
.STACK 64
;-----------
.Data
    nameq             db  'Please enter your name:','$'
    thename           db  15 dup('$')
    proceed           db  'Please Enter key to continue','$'
    op1               db  'To start chatting press F1','$'
    op2               db  'To start the game press F1','$'
    op3               db  'To end the program press ESC','$'
    ;------------black pieces---------------
    bbishopwidth      equ 60D
    bbishopheight     equ 60D
    bbishopfilename   db  'bbishop.bin',0
    bbishopfilehandle DW  ?
    bbishopdata       db  bbishopwidth*bbishopheight dup(0)

    bkingwidth        equ 60D
    bkingheight       equ 60D
    bkingfilename     db  'bking.bin',0
    bkingfilehandle   DW  ?
    bkingdata         db  bkingwidth*bkingheight dup(0)

    bknightwidth      equ 60D
    bknightheight     equ 60D
    bknightfilename   db  'bknight.bin',0
    bknightfilehandle DW  ?
    bknightdata       db  bknightwidth*bknightheight dup(0)

    bpawnwidth        equ 60D
    bpawnheight       equ 60D
    bpawnfilename     db  'bpawn.bin',0
    bpawnfilehandle   DW  ?
    bpawndata         db  bpawnwidth*bpawnheight dup(0)

    bqueenwidth       equ 60D
    bqueenheight      equ 60D
    bqueenfilename    db  'bqueen.bin',0
    bqueenfilehandle  DW  ?
    bqueendata        db  bqueenwidth*bqueenheight dup(0)

    brockwidth        equ 60D
    brockheight       equ 60D
    brockfilename     db  'brock.bin',0
    brockfilehandle   DW  ?
    brockdata         db  brockwidth*brockheight dup(0)

    ;------------white pieces---------------
    wbishopwidth      equ 60D
    wbishopheight     equ 60D
    wbishopfilename   db  'wbishop.bin',0
    wbishopfilehandle DW  ?
    wbishopdata       db  wbishopwidth*wbishopheight dup(0)

    wkingwidth        equ 60D
    wkingheight       equ 60D
    wkingfilename     db  'wking.bin',0
    wkingfilehandle   DW  ?
    wkingdata         db  wkingwidth*wkingheight dup(0)

    wknightwidth      equ 60D
    wknightheight     equ 60D
    wknightfilename   db  'wknight.bin',0
    wknightfilehandle DW  ?
    wknightdata       db  wknightwidth*wknightheight dup(0)

    wpawnwidth        equ 60D
    wpawnheight       equ 60D
    wpawnfilename     db  'wpawn.bin',0
    wpawnfilehandle   DW  ?
    wpawndata         db  wpawnwidth*wpawnheight dup(0)

    wqueenwidth       equ 60D
    wqueenheight      equ 60D
    wqueenfilename    db  'wqueen.bin',0
    wqueenfilehandle  DW  ?
    wqueendata        db  wqueenwidth*wqueenheight dup(0)

    wrockwidth        equ 60D
    wrockheight       equ 60D
    wrockfilename     db  'wrock.bin',0
    wrockfilehandle   DW  ?
    wrockdata         db  wrockwidth*wrockheight dup(0)
    ;--------------
    borderwidth       equ 60D
    borderheight      equ 60D
    borderfilename    db  'border.bin',0
    borderfilehandle  DW  ?
    borderdata        db  borderwidth*borderheight dup(0)
    ;---------------------------------------------------------------------------------------------------
 


.CODE
MAIN PROC FAR
    ;INITIALIZING
                  call            GETDATA
                  CALL            CLS
    ;OPENING AND READING BIN FILES
                  OpenFile        bbishopfilename, bbishopfilehandle
                  ReadData        bbishopfilehandle ,bbishopwidth,bbishopheight,bbishopdata
                  OpenFile        bkingfilename, bkingfilehandle
                  ReadData        bkingfilehandle ,bkingwidth,bkingheight,bkingdata
                  OpenFile        bknightfilename, bknightfilehandle
                  ReadData        bknightfilehandle ,bknightwidth,bknightheight,bknightdata
                  OpenFile        bpawnfilename, bpawnfilehandle
                  ReadData        bpawnfilehandle ,bpawnwidth,bpawnheight,bpawndata
                  OpenFile        bqueenfilename, bqueenfilehandle
                  ReadData        bqueenfilehandle ,bqueenwidth,bqueenheight,bqueendata
                  OpenFile        brockfilename, brockfilehandle
                  ReadData        brockfilehandle ,brockwidth,brockheight,brockdata
    ;--white piecies----
                  OpenFile        wbishopfilename, wbishopfilehandle
                  ReadData        wbishopfilehandle ,wbishopwidth,bbishopheight,wbishopdata
                  OpenFile        wkingfilename, wkingfilehandle
                  ReadData        wkingfilehandle ,wkingwidth,wkingheight,wkingdata
                  OpenFile        wknightfilename, wknightfilehandle
                  ReadData        wknightfilehandle ,wknightwidth,wknightheight,wknightdata
                  OpenFile        wpawnfilename, wpawnfilehandle
                  ReadData        wpawnfilehandle ,wpawnwidth,wpawnheight,wpawndata
                  OpenFile        wqueenfilename, wqueenfilehandle
                  ReadData        wqueenfilehandle ,wqueenwidth,wqueenheight,wqueendata
                  OpenFile        wrockfilename, wrockfilehandle
                  ReadData        wrockfilehandle ,wrockwidth,wrockheight,wrockdata
    ;--border-----
                  OpenFile        borderfilename, borderfilehandle
                  ReadData        borderfilehandle ,borderwidth,borderheight,borderdata
    ;START MENU
                  movecursor      17H,05H
                  ShowMessage     nameq
                  movecursor      17H,06H
                  cin             thename
                  movecursor      17H,0AH
                  ShowMessage     proceed
                  call            waitkey
    ;CHOICE MENU
                  call            CLS
                  movecursor      17H,03H
                  ShowMessage     op1
                  movecursor      17H,08H
                  ShowMessage     op2
                  movecursor      17H,0DH
                  ShowMessage     op3
                  call            waitkey
    ;GAME SCREEN
                  CALL            EnterGraphics
                  DrawGrid        0D,0D,0FH,08H
    ;white
                  DRAW            wrockdata,wrockwidth,wrockheight,0D,0D                       ; col,row
                  getDrawPosition 0 ,1
                  DRAW            wknightdata,wknightwidth,wknightheight,CX,DX                ; col,row
                  DRAW            wbishopdata,wbishopwidth,wbishopheight,120D,0D               ; col,row
                  DRAW            wqueendata,wqueenwidth,wqueenheight,180D,0D                  ; col,row
                  DRAW            wkingdata,wkingwidth,wkingheight,240D,0D                     ; col,row
                  DRAW            wbishopdata,wbishopwidth,wbishopheight,300D,0D               ; col,row
                  DRAW            wknightdata,wknightwidth,wknightheight,360D,0D               ; col,row
                  DRAW            wrockdata,wrockwidth,wrockheight,420D,0D                     ; col,row

                  DRAW            wpawndata,wpawnwidth,wpawnheight,0D,60D                      ; col,row
                  DRAW            wpawndata,wpawnwidth,wpawnheight,60D,60D                     ; col,row
                  DRAW            wpawndata,wpawnwidth,wpawnheight,120D,60D                    ; col,row
                  DRAW            wpawndata,wpawnwidth,wpawnheight,180D,60D                    ; col,row
                  DRAW            wpawndata,wpawnwidth,wpawnheight,240D,60D                    ; col,row
                  DRAW            wpawndata,wpawnwidth,wpawnheight,300D,60D                    ; col,row
                  DRAW            wpawndata,wpawnwidth,wpawnheight,360D,60D                    ; col,row
                  DRAW            wpawndata,wpawnwidth,wpawnheight,420D,60D                    ; col,row
    ;black
                  DRAW            brockdata,brockwidth,brockheight,0D,420D                     ; col,row
                  DRAW            bknightdata,bknightwidth,bknightheight,60D,420D              ; col,row
                  DRAW            bbishopdata,bbishopwidth,bbishopheight,120D,420D             ; col,row
                  DRAW            bqueendata,bqueenwidth,bqueenheight,180D,420D                ; col,row
                  DRAW            bkingdata,bkingwidth,bkingheight,240D,420D                   ; col,row
                  DRAW            bbishopdata,bbishopwidth,bbishopheight,300D,420D             ; col,row
                  DRAW            bknightdata,bknightwidth,bknightheight,360D,420D             ; col,row
                  DRAW            brockdata,brockwidth,brockheight,420D,420D                   ; col,row

                  DRAW            bpawndata,bpawnwidth,bpawnheight,0D,360D                     ; col,row
                  DRAW            bpawndata,bpawnwidth,bpawnheight,60D,360D                    ; col,row
                  DRAW            bpawndata,bpawnwidth,bpawnheight,120D,360D                   ; col,row
                  DRAW            bpawndata,bpawnwidth,bpawnheight,180D,360D                   ; col,row
                  DRAW            bpawndata,bpawnwidth,bpawnheight,240D,360D                   ; col,row
                  DRAW            bpawndata,bpawnwidth,bpawnheight,300D,360D                   ; col,row
                  DRAW            bpawndata,bpawnwidth,bpawnheight,360D,360D                   ; col,row
                  DRAW            bpawndata,bpawnwidth,bpawnheight,420D,360D                   ; col,row
    ;border
                
















    ;------------------------------------------------------------------------------
    ;CloseFile   imgfilehandle1
    ;CloseFile   imgfilehandle2
                  EXT
MAIN ENDP
    ;----------------------------------------------------------------------------------------------------------------


    ;--------------------------------------------------Functions---------------------------------------------------------
GETDATA PROC                                                                                   ;GET DATA
                  MOV             AX,@DATA
                  MOV             DS,AX
                  ret
GETDATA ENDP

CLS PROC                                                                                       ;CLEAR SCREEN
                  MOV             AX,0003H
                  INT             10H
                  ret
CLS ENDP

EnterGraphics PROC                                                                             ;ENTER GRAPHICS MODE
                  MOV             AX,4F02H
                  MOV             BX,103H                                                      ;(800x600) pixel ;grid =480*480; char=60*60
                  INT             10H
                  ret
EnterGraphics ENDP

waitkey PROC                                                                                   ;wait for key
                  MOV             AH , 0
                  INT             16h
                  ret
waitkey ENDP

END MAIN

;http://www.wagemakers.be/english/doc/vga