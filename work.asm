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
                      INT 10H
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

DRAWCELL MACRO X,Y       ;DRAW WHITE CELL
        LOCAL drawLoop
        LOCAL innerloop
                
        mov cx,X
        mov dx,Y
        ; Drawing loop
        mov di,0
        drawLoop:     
            mov si,0
            innerloop:
            MOV  AL,0FH
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

DrawGrid MACRO X,Y                                           ;CLOSE FILE
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
            DRAWCELL AX,BX
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
            DRAWCELL AX,BX
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
            DRAWCELL AX,BX
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
            DRAWCELL AX,BX
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
            DRAWCELL AX,BX
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
            DRAWCELL AX,BX
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
            DRAWCELL AX,BX
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
            DRAWCELL AX,BX
            POP CX
            POP BX
            POP AX
            ADD AX,120D
            INC CX
            CMP CX,4D
        JNE  BIGGERLOOP8      
ENDM DrawGrid

.MODEL SMALL
.STACK 64
;-----------
.Data
    nameq          db  'Please enter your name:','$'
    thename        db  15 dup('$')
    proceed        db  'Please Enter key to continue','$'
    op1            db  'To start chatting press F1','$'
    op2            db  'To start the game press F1','$'
    op3            db  'To end the program press ESC','$'

    imgwidth1      equ 150D
    imgheight1     equ 155D
    imgfilename1   db  'test.bin',0
    imgfilehandle1 DW  ?
    imgdata1       db  imgwidth1*imgheight1 dup(0)

    imgwidth2      equ 150D
    imgheight2     equ 155D
    imgfilename2   db  'test2.bin',0
    imgfilehandle2 DW  ?
    imgdata2       db  imgwidth2*imgheight2 dup(0)
    ;---------------------------------------------------------------------------------------------------















.CODE
MAIN PROC FAR
                  call     GETDATA
                  CALL     CLS
    ;OpenFile    imgfilename1, imgfilehandle1
    ;ReadData    imgfilehandle1 ,imgwidth1,imgheight1,imgdata1
    ;OpenFile    imgfilename2, imgfilehandle2
    ;ReadData    imgfilehandle2 ,imgwidth2,imgheight2,imgdata2
    ;start menu
    ;movecursor  17H,05H
    ;ShowMessage nameq
    ;movecursor  17H,06H
    ;cin         thename
    ;movecursor  17H,0AH
    ;ShowMessage proceed
    ;call        waitkey
    ;choice menu
    ;call        CLS
    ;movecursor  17H,03H
    ;ShowMessage op1
    ;movecursor  17H,08H
    ;ShowMessage op2
    ;movecursor  17H,0DH
    ;ShowMessage op3
    ;call        waitkey
    ;game screen
                  CALL     EnterGraphics
    ;drawall
                  DrawGrid 0D,0D
    ;DRAW        imgdata1,imgwidth1,imgheight1,150D,0D            ; col,row
    ;DRAW        imgdata2,imgwidth2,imgheight2,0D,0D              ; col,row

    ;------------------------------------------------------------------------------
    ;CloseFile   imgfilehandle1
    ;CloseFile   imgfilehandle2
                  EXT
MAIN ENDP














    ;----------------------------------------------------------------------------------------------------------------

    ;--------------------------------------------------Functions---------------------------------------------------------
GETDATA PROC                                ;GET DATA
                  MOV      AX,@DATA
                  MOV      DS,AX
                  ret
GETDATA ENDP

CLS PROC                                    ;CLEAR SCREEN
                  MOV      AX,0003H
                  INT      10H
                  ret
CLS ENDP

EnterGraphics PROC                          ;ENTER GRAPHICS MODE
                  MOV      AX,4F02H
                  MOV      BX,103H          ;(800x600) pixel ;grid =480*480; char=60*60
                  INT      10H
                  ret
EnterGraphics ENDP

waitkey PROC                                ;wait for key
                  MOV      AH , 0
                  INT      16h
                  ret
waitkey ENDP

END MAIN

;http://www.wagemakers.be/english/doc/vga