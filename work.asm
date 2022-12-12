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
 MOV         AL,ROW
 MOV         CL,60D
 MUL         CL
 MOV         dx,ax
 MOV         AL,COL
 MUL         CL
 MOV         CX,Ax
ENDM getDrawPosition

validateName MACRO entermsg,name,strFailed
LOCAL repeatt
LOCAL biggerthana
LOCAL outOfTheValidation
LOCAL fistcheck

movecursor  17H,05H
ShowMessage entermsg
movecursor  17H,06H
cin         name
;movecursor  17H,0AH
jmp fistcheck
;---------fist check with enter message-----------;
repeatt:
;; clear screen
mov ax, 2
int 10h
movecursor  17H,05H
ShowMessage strFailed
movecursor  17H,06H
cin         name
;movecursor  17H,0AH
fistcheck:
;---------other checks with error message-----------;
mov bx,offset name + 2
mov al,[bx]
mov bl,122;;== z 
cmp bl,al;;if greater than z jmp
jc repeatt
cmp al,65;;== A
jc repeatt;;if less than A jmp
mov bl,90;;== Z 
cmp bl,al;;if greater than Z jmp
jc biggerthana
jmp outOfTheValidation
biggerthana:
cmp al,97;;== a
jc repeatt;;if less than a jmp
outOfTheValidation:
ENDM validateName

.MODEL SMALL
.STACK 64
;-----------
.Data
    nameq             db  'Please enter your name:','$'
    erroname          db  'Please write a valid name :','$'

    wrockdata         db  60D*60D dup(0)
    wkingdata         db  60D*60D dup(0)
    wbishopdata       db  60D*60D dup(0)
    wqueendata        db  60D*60D dup(0)
    wknightdata       db  60D*60D dup(0)
    wpawndata         db  60D*60D dup(0)
    bpawndata         db  60D*60D dup(0)
    brockdata         db  60D*60D dup(0)
    bknightdata       db  60D*60D dup(0)
    bbishopdata       db  60D*60D dup(0)
    bqueendata        db  60D*60D dup(0)
    bkingdata         db  60D*60D dup(0)



    thename           db  16,?,16 dup('$'); max size 15 char last digit for $
    proceed           db  'Please Enter key to continue','$'
    op1               db  'To start chatting press F1','$'
    op2               db  'To start the game press F1','$'
    op3               db  'To end the program press ESC','$'
    ;------------black pieces---------------
    bbishopwidth      equ 60D
    bbishopheight     equ 60D
    bbishopfilename   db  'bbishop.bin',0
    bbishopfilehandle DW  ?

    bkingwidth        equ 60D
    bkingheight       equ 60D
    bkingfilename     db  'bking.bin',0
    bkingfilehandle   DW  ?

    bknightwidth      equ 60D
    bknightheight     equ 60D
    bknightfilename   db  'bknight.bin',0
    bknightfilehandle DW  ?

    bpawnwidth        equ 60D
    bpawnheight       equ 60D
    bpawnfilename     db  'bpawn.bin',0
    bpawnfilehandle   DW  ?

    bqueenwidth       equ 60D
    bqueenheight      equ 60D
    bqueenfilename    db  'bqueen.bin',0
    bqueenfilehandle  DW  ?

    brockwidth        equ 60D
    brockheight       equ 60D
    brockfilename     db  'brock.bin',0
    brockfilehandle   DW  ?

    ;------------white pieces---------------
    wbishopwidth      equ 60D
    wbishopheight     equ 60D
    wbishopfilename   db  'wbishop.bin',0
    wbishopfilehandle DW  ?

    wkingwidth        equ 60D
    wkingheight       equ 60D
    wkingfilename     db  'wking.bin',0
    wkingfilehandle   DW  ?

    wknightwidth      equ 60D
    wknightheight     equ 60D
    wknightfilename   db  'wknight.bin',0
    wknightfilehandle DW  ?

    wpawnwidth        equ 60D
    wpawnheight       equ 60D
    wpawnfilename     db  'wpawn.bin',0
    wpawnfilehandle   DW  ?

    wqueenwidth       equ 60D
    wqueenheight      equ 60D
    wqueenfilename    db  'wqueen.bin',0
    wqueenfilehandle  DW  ?

    wrockwidth        equ 60D
    wrockheight       equ 60D
    wrockfilename     db  'wrock.bin',0
    wrockfilehandle   DW  ?
    ;--------------
    borderwidth       equ 60D
    borderheight      equ 60D
    borderfilename    db  'border.bin',0
    borderfilehandle  DW  ?
    borderdata        db  borderwidth*borderheight dup(0)

    curentCursorX     DW  0D
    curentCursorY     DW  0D

    gridState         DW  128  dup(0)

    currrow           db  0
    currcol           db  0

    ;---------------------------------------------------------------------------------------------------
 


.CODE
MAIN PROC FAR
    ;INITIALIZING
                  call        GETDATA
                  CALL        CLS
    ;OPENING AND READING BIN FILES
                  OpenFile    bbishopfilename, bbishopfilehandle
                  ReadData    bbishopfilehandle ,bbishopwidth,bbishopheight,bbishopdata
                  OpenFile    bkingfilename, bkingfilehandle
                  ReadData    bkingfilehandle ,bkingwidth,bkingheight,bkingdata
                  OpenFile    bknightfilename, bknightfilehandle
                  ReadData    bknightfilehandle ,bknightwidth,bknightheight,bknightdata
                  OpenFile    bpawnfilename, bpawnfilehandle
                  ReadData    bpawnfilehandle ,bpawnwidth,bpawnheight,bpawndata
                  OpenFile    bqueenfilename, bqueenfilehandle
                  ReadData    bqueenfilehandle ,bqueenwidth,bqueenheight,bqueendata
                  OpenFile    brockfilename, brockfilehandle
                  ReadData    brockfilehandle ,brockwidth,brockheight,brockdata
    ;--white piecies----
                  OpenFile    wbishopfilename, wbishopfilehandle
                  ReadData    wbishopfilehandle ,wbishopwidth,bbishopheight,wbishopdata
                  OpenFile    wkingfilename, wkingfilehandle
                  ReadData    wkingfilehandle ,wkingwidth,wkingheight,wkingdata
                  OpenFile    wknightfilename, wknightfilehandle
                  ReadData    wknightfilehandle ,wknightwidth,wknightheight,wknightdata
                  OpenFile    wpawnfilename, wpawnfilehandle
                  ReadData    wpawnfilehandle ,wpawnwidth,wpawnheight,wpawndata
                  OpenFile    wqueenfilename, wqueenfilehandle
                  ReadData    wqueenfilehandle ,wqueenwidth,wqueenheight,wqueendata
                  OpenFile    wrockfilename, wrockfilehandle
                  ReadData    wrockfilehandle ,wrockwidth,wrockheight,wrockdata
    ;--border-----
                  OpenFile    borderfilename, borderfilehandle
                  ReadData    borderfilehandle ,borderwidth,borderheight,borderdata

    ;------------------------------------------------------------------------------------------------
    ;------------------------------------------------------------------------------------------------
    ;------------------------------------------------------------------------------------------------

    

                  mov         SI,offset wrockdata
                  mov         gridState[1],SI
                  mov         SI,offset wknightdata
                  mov         gridState[3],SI
                  mov         SI,offset wbishopdata
                  mov         gridState[5],SI
                  mov         SI,offset wqueendata
                  mov         gridState[7],SI
                  mov         SI,offset wkingdata
                  mov         gridState[9],SI
                  mov         SI,offset wbishopdata
                  mov         gridState[11],SI
                  mov         SI,offset wknightdata
                  mov         gridState[13],SI
                  mov         SI,offset wrockdata
                  mov         gridState[15],SI
                  mov         SI,offset wpawndata
                  mov         gridState[17],SI
                  mov         gridState[19],SI
                  mov         gridState[21],SI
                  mov         gridState[23],SI
                  mov         gridState[25],SI
                  mov         gridState[27],SI
                  mov         gridState[29],SI
                  mov         gridState[31],SI

                  mov         gridState[33],0
                  mov         gridState[35],0
                  mov         gridState[37],0
                  mov         gridState[39],0
                  mov         gridState[41],0
                  mov         gridState[43],0
                  mov         gridState[45],0
                  mov         gridState[47],0
                  mov         gridState[49],0
                  mov         gridState[51],0
                  mov         gridState[53],0
                  mov         gridState[55],0
                  mov         gridState[57],0
                  mov         gridState[59],0
                  mov         gridState[61],0
                  mov         gridState[63],0

                  mov         gridState[65],0
                  mov         gridState[67],0
                  mov         gridState[69],0
                  mov         gridState[71],0
                  mov         gridState[73],0
                  mov         gridState[75],0
                  mov         gridState[77],0
                  mov         gridState[79],0
                  mov         gridState[81],0
                  mov         gridState[83],0
                  mov         gridState[85],0
                  mov         gridState[87],0
                  mov         gridState[89],0
                  mov         gridState[91],0
                  mov         gridState[93],0
                  mov         gridState[95],0
 
                  mov         SI,offset bpawndata
                  mov         gridState[97],SI
                  mov         gridState[99],SI
                  mov         gridState[101],SI
                  mov         gridState[103],SI
                  mov         gridState[105],SI
                  mov         gridState[107],SI
                  mov         gridState[109],SI
                  mov         gridState[111],SI
                  mov         SI,offset brockdata
                  mov         gridState[113],SI
                  mov         SI,offset bknightdata
                  mov         gridState[115],SI
                  mov         SI,offset bbishopdata
                  mov         gridState[117],SI
                  mov         SI,offset bqueendata
                  mov         gridState[119],SI
                  mov         SI,offset bkingdata
                  mov         gridState[121],SI
                  mov         SI,offset bbishopdata
                  mov         gridState[123],SI
                  mov         SI,offset bknightdata
                  mov         gridState[125],SI
                  mov         SI,offset brockdata
                  mov         gridState[127],SI














    ;------------------------------------------------------------------------------------------------
    ;------------------------------------------------------------------------------------------------
    ;------------------------------------------------------------------------------------------------

    ;START MENU
                  ;movecursor  17H,05H
                  ;ShowMessage nameq
                  ;movecursor  17H,06H
                  ;cin         thename
                  validateName nameq,thename,erroname
                  movecursor  17H,0AH
                  ShowMessage proceed
                  call        waitkey
    ;CHOICE MENU
                  call        CLS
                  movecursor  17H,03H
                  ShowMessage op1
                  movecursor  17H,08H
                  ShowMessage op2
                  movecursor  17H,0DH
                  ShowMessage op3
                  call        waitkey
    ;GAME SCREEN
                  CALL        EnterGraphics
                  DrawGrid    0D,0D,0FH,08H
    ;white


                  mov         SI,gridState[117]
    
    ;DRAW            [SI],wrockwidth,wrockheight,0D,0D                       ; col,row

               

                 
    ;border
                
                  DRAW        borderdata,borderwidth,borderheight,curentCursorX,curentCursorY    ; col,row

    cursorLoop:   

                  DrawGrid    0D,0D,0FH,08H

                  mov         di,1D
                  mov         bh,0
                  mov         bl,0

    loop1:        

                  mov         bh,0

    loop2:        

                  mov         SI,gridState[di]
                  
                 
                  getDrawPosition bl,bh
                 

                  cmp         SI,0
                  jz          skip
                  push        BX
                  push        di
                  DRAW        [SI],wrockwidth,wrockheight,CX,DX                                  ; col,row
                  pop         di
                  pop         BX
    skip:         
                  add         di,2
                  add         bh,1
                  cmp         bh,8
                  jnz         loop2
                  add         bL,1
                  cmp         bL,8
                  jnz         loop1

                  DRAW        borderdata,borderwidth,borderheight,curentCursorX,curentCursorY    ; col,row

                  mov         ah,0
                  int         16h

                  cmp         ah,11h
                  jnz         label6
                  jmp         up
    label6:       

                  cmp         ah,1eh
                  jnz         label7
                  jmp         left
    label7:       

                  cmp         ah,20h
                  jnz         label8
                  jmp         right
    label8:       

                  cmp         ah,1fh
                  jnz         label9
                  jmp         down
    label9:       

                  cmp         ah,1ch
                  jz          temp19
                  jmp         cursorLoop
    temp19:       
                  
                  cmp         ah,1ch
                  jnz         temp1
                  jmp         label3
    temp1:        

    left:         
                  mov         DX,curentCursorX
                  cmp         DX,0D
                  jnz         temp20

                  jmp         cursorLoop
    temp20:       
                  sub         DX,60D

                  mov         curentCursorX,DX
                  DRAW        borderdata,borderwidth,borderheight,curentCursorX,curentCursorY    ; col,row
                  cmp         ah,11h
                  jz          label5

                  jmp         cursorLoop
    label5:       


    right:        
                  mov         DX,curentCursorX
                  cmp         DX,420D
                  jnz         temp22
                  jmp         cursorLoop
    temp22:       
                  add         DX,60D
                  mov         curentCursorX,DX
                  DRAW        borderdata,borderwidth,borderheight,curentCursorX,curentCursorY    ; col,row
                  cmp         ah,20h
                  jz          label4
                  jmp         cursorLoop
    label4:       

    up:           
                  mov         DX,curentCursorY
                  cmp         DX,0D
                  jnz         label10
                  jmp         cursorLoop
    label10:      

                  mov         bl,currrow
                  dec         bl
                  mov         currrow,bl
                  sub         DX,60D

                  mov         curentCursorY,DX
                  DRAW        borderdata,borderwidth,borderheight,curentCursorX,curentCursorY    ; col,row
                  cmp         ah,11h
                  jz          label2

                  jmp         cursorLoop
    label2:       


    down:         
                  mov         DX,curentCursorY
                  cmp         DX,420D
                  jnz         label11
                  jmp         cursorLoop
    label11:      
                  add         DX,60D
                  mov         curentCursorY,DX
                  DRAW        borderdata,borderwidth,borderheight,curentCursorX,curentCursorY    ; col,row
                  cmp         ah,1fh
                  jz          label1
                  jmp         cursorLoop
    label1:       


                  cmp         ah,1ch
                  jz          label3

                  jmp         cursorLoop
    label3:       





    ;------------------------------------------------------------------------------
    ;CloseFile   imgfilehandle1
    ;CloseFile   imgfilehandle2
                  EXT
MAIN ENDP
    ;----------------------------------------------------------------------------------------------------------------


    ;--------------------------------------------------Functions---------------------------------------------------------
GETDATA PROC                                                                                     ;GET DATA
                  MOV         AX,@DATA
                  MOV         DS,AX
                  ret
GETDATA ENDP

CLS PROC                                                                                         ;CLEAR SCREEN
                  MOV         AX,0003H
                  INT         10H
                  ret
CLS ENDP

EnterGraphics PROC                                                                               ;ENTER GRAPHICS MODE
                  MOV         AX,4F02H
                  MOV         BX,103H                                                            ;(800x600) pixel ;grid =480*480; char=60*60
                  INT         10H
                  ret
EnterGraphics ENDP

waitkey PROC                                                                                     ;wait for key
                  MOV         AH , 0
                  INT         16h
                  ret
waitkey ENDP

END MAIN

;http://www.wagemakers.be/english/doc/vga