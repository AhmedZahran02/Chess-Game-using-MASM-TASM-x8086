;-----------------------------------------MACROS-------------------------------------------------------------
; 0 NOTHING 1 --> 12 THE PIECES BY ORDER IN .DATA

EXT MACRO ;PRESS ANY KEY TO EXIT APPLICATION
                MOV  AH , 0
                INT  16h
                MOV         AH,4CH
                INT         21H
ENDM        EXT

movecursor MACRO x,y ;move cursor
                mov         ah,2
                mov         bh,0
                mov cl,x
                mov ch,y
                mov         dh,ch
                mov         dl,cl
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
    LOCAL divide
    LOCAL OUTH
    LOCAL MAKESTR
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

DRAW MACRO imgwidth,imgheight,X,Y,A,B      ;DRAW IMAGE
                LOCAL drawLoop
                LOCAL innerloop
                LOCAL skp
                LOCAL GOAWAY
                
                getDrawPosition A,B,X,Y
                PUSH CX
                PUSH DX
                GETIMGDATA X,Y
                POP DX
                POP CX
                CMP BX,0H
                JE GOAWAY
        ; Drawing loop
                      mov di,0
        drawLoop:     
                      mov si,0
                      innerloop:
                      MOV  AL,BYTE PTR[BX]
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
                      GOAWAY:
                ENDM        DRAW

DRAWWITHSOURCE MACRO imgdata,imgwidth,imgheight,X,Y,A,B      ;DRAW IMAGE
                LOCAL drawLoop
                LOCAL innerloop
                LOCAL skp
                
                getDrawPosition A,B,X,Y
                LEA BX,imgdata
        ; Drawing loop
                      mov di,0
        drawLoop:     
                      mov si,0
                      innerloop:
                      MOV  AL,BYTE PTR [BX]
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
                ENDM        DRAWWITHSOURCE

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

DrawGrid MACRO X,Y,B,A                                           ;DRAW grid at x,y with color a dark and b light
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

FIRSTQHANDLE MACRO
                  ;if the cell is empty get out
                ;   cmp bl,6
                ;   jz isapawn
                ;   jmp getouttt
                ;   isapawn:  
                ;   PAWNAVALIABLEMOVES curRowCursor,curColCursor
                ;   getouttt: 

                ;   isEmpty curRowCursor,curColCursor
                ;   PAWNAVALIABLEMOVES curRowCursor,curColCursor
                ;   ;KnightMovements curRowCursor,curColCursor
                ;   cmp bl,0FFH
                ;   jnz getoutt
                ;   jmp outterr
                ;   getoutt:
                  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                  GETARINDEX curRowCursor,curColCursor

                  pusha
                  ISEMPTY curRowCursor,curColCursor
                    cmp bx,1
                    jne temp100
                    jmp break80
                    temp100:
                    popa
                  pusha
                  ISWHITEBYTE curRowCursor,curColCursor
                  cmp bx,0
                  jne temp150
                  jmp break80
                  temp150:
                  popa

                  MOV cl,BYTE PTR colorState[bx] 
                  mov BYTE PTR cellColorState,cl
                  mov cl,BYTE PTR curRowCursor
                  mov ch,BYTE PTR curColCursor
                  mov BYTE PTR startRowCursor,cl
                  mov BYTE PTR startColCursor,ch
                  MOV BYTE PTR colorState[bx],0CH 
                  pusha
                  UPDATECELL     curRowCursor,curColCursor,150D,0D
                  DRAWWITHSOURCE       borderdata,borderwidth,borderheight,curRowCursor,curColCursor,150D,0D 
                  popa
                  mov bl,BYTE PTR stateOfQ

                  inc bl
                  mov BYTE PTR stateOfQ,bl

                ;------------------------
                  GETARINDEX curRowCursor,curColCursor
                  CMP BYTE PTR gridState[BX],7d
                  JNE PAWNTEMP
                  JMP PAWN
                  PAWNTEMP:
                  CMP BYTE PTR gridState[BX],8d
                  JNE ROOKTEMP
                  JMP ROOK
                  ROOKTEMP:
                  CMP BYTE PTR gridState[BX],9d
                  JNE KNIGHTTEMP
                  JMP KNIGHT
                  KNIGHTTEMP:
                  CMP BYTE PTR gridState[BX],10d
                  JNE BISHOPTEMP
                  JMP BISHOP
                  BISHOPTEMP:
                  CMP BYTE PTR gridState[BX],11d
                  JNE QUEENTEMP
                  JMP QUEEN
                  QUEENTEMP:
                  CMP BYTE PTR gridState[BX],12d
                  JNE KINGTEMP
                  JMP KING
                  KINGTEMP:
                  JMP NOACTION
                PAWN:
                  HANDLEPAWN curRowCursor,curColCursor
                  JMP NOACTION
                ROOK:
                  HANDLEROOK curRowCursor,curColCursor
                  JMP NOACTION
                KNIGHT:
                  HANDLEKNIGHT curRowCursor,curColCursor
                  JMP NOACTION
                BISHOP:
                  HANDLEBISHOP curRowCursor,curColCursor
                  JMP NOACTION
                QUEEN:
                  HANDLEQUEEN curRowCursor,curColCursor
                  JMP NOACTION
                KING:
                  HANDLEKING curRowCursor,curColCursor
                  JMP NOACTION
                NOACTION:
                PUSHA
                DRAW_AVAILABLE_PLACES
                POPA
                ;------------------------------
                  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                  outterr:
                  break80:
                  
ENDM FIRSTQHANDLE

HANDLEROOK MACRO x,y
LOCAL break1
LOCAL break2
LOCAL break3
LOCAL break4
LOCAL break12
LOCAL break22
LOCAL break32
LOCAL break42
LOCAL First_Loop
LOCAL Second_Loop
LOCAL Third_Loop
LOCAL Fourth_Loop
LOCAL temp29
LOCAL temp36
LOCAL temp35
LOCAL temp28
LOCAL temp34
LOCAL temp33
LOCAL temp27
LOCAL temp32
LOCAL temp31
LOCAL temp26
LOCAL temp40
LOCAL temp30


mov dh,x
mov dl,y
mov ah,dh
mov al,dl
First_Loop:
inc ah
mov dummyData1,ah
mov dummyData2,al
push ax
INSIDEGRID dummyData1,dummyData2
pop ax 
cmp bx,0
jne temp30
jmp break12
temp30:
push ax
ISEMPTY dummyData1,dummyData2
pop ax 
cmp bx,0
jne temp40
jmp break1
temp40:
push ax
GETARINDEXBYBYTE  dummyData1,dummyData2
pop ax
mov cursorState[bx],1
jmp First_Loop
break1:
ISWHITEBYTE dummyData1,dummyData2
cmp bx,1
jne temp26
jmp break12
temp26:
push ax
GETARINDEXBYBYTE  dummyData1,dummyData2
pop ax
mov cursorState[bx],1
break12:


mov ah,dh
mov al,dl
Second_Loop:
inc al
mov dummyData1,ah
mov dummyData2,al
push ax
INSIDEGRID dummyData1,dummyData2
pop ax 
cmp bx,0
jne temp31
jmp break22
temp31:
push ax
ISEMPTY dummyData1,dummyData2
pop ax 
cmp bx,0
jne temp32
jmp break2
temp32:
push ax
GETARINDEXBYBYTE  dummyData1,dummyData2
pop ax
mov cursorState[bx],1
jmp Second_Loop
break2:
ISWHITEBYTE dummyData1,dummyData2
cmp bx,1
jne temp27
jmp break22
temp27:
push ax
GETARINDEXBYBYTE  dummyData1,dummyData2
pop ax
mov cursorState[bx],1
break22:


mov ah,dh
mov al,dl
Third_Loop:
dec ah
mov dummyData1,ah
mov dummyData2,al
push ax
INSIDEGRID dummyData1,dummyData2
pop ax 
cmp bx,0
jne temp33
jmp break32
temp33:
push ax
ISEMPTY dummyData1,dummyData2
pop ax 
cmp bx,0
jne temp34
jmp break3
temp34:
push ax
GETARINDEXBYBYTE  dummyData1,dummyData2
pop ax
mov cursorState[bx],1
jmp Third_Loop
break3:
ISWHITEBYTE dummyData1,dummyData2
cmp bx,1
jne temp28
jmp break32
temp28:
push ax
GETARINDEXBYBYTE  dummyData1,dummyData2
pop ax
mov cursorState[bx],1
break32:


mov ah,dh
mov al,dl
Fourth_Loop:
dec al
mov dummyData1,ah
mov dummyData2,al
push ax
INSIDEGRID dummyData1,dummyData2
pop ax 
cmp bx,0
jne temp35
jmp break42
temp35:
push ax
ISEMPTY dummyData1,dummyData2
pop ax 
cmp bx,0
jne temp36
jmp break4
temp36:
push ax
GETARINDEXBYBYTE  dummyData1,dummyData2
pop ax
mov cursorState[bx],1
jmp Fourth_Loop
break4:
ISWHITEBYTE dummyData1,dummyData2
cmp bx,1
jne temp29
jmp break42
temp29:
push ax
GETARINDEXBYBYTE  dummyData1,dummyData2
pop ax
mov cursorState[bx],1
break42:

ENDM HANDLEROOK

HANDLEBISHOP MACRO x,y
LOCAL break1
LOCAL break2
LOCAL break3
LOCAL break4
LOCAL break12
LOCAL break22
LOCAL break32
LOCAL break42
LOCAL First_Loop
LOCAL Second_Loop
LOCAL Third_Loop
LOCAL Fourth_Loop
LOCAL temp29
LOCAL temp36
LOCAL temp35
LOCAL temp28
LOCAL temp34
LOCAL temp33
LOCAL temp27
LOCAL temp32
LOCAL temp31
LOCAL temp26
LOCAL temp40
LOCAL temp30


mov dh,x
mov dl,y
mov ah,dh
mov al,dl
First_Loop:
inc ah
inc al
mov dummyData1,ah
mov dummyData2,al
push ax
INSIDEGRID dummyData1,dummyData2
pop ax 
cmp bx,0
jne temp30
jmp break12
temp30:
push ax
ISEMPTY dummyData1,dummyData2
pop ax 
cmp bx,0
jne temp40
jmp break1
temp40:
push ax
GETARINDEXBYBYTE  dummyData1,dummyData2
pop ax
mov cursorState[bx],1
jmp First_Loop
break1:
ISWHITEBYTE dummyData1,dummyData2
cmp bx,1
jne temp26
jmp break12
temp26:
push ax
GETARINDEXBYBYTE  dummyData1,dummyData2
pop ax
mov cursorState[bx],1
break12:


mov ah,dh
mov al,dl
Second_Loop:
dec al
dec ah
mov dummyData1,ah
mov dummyData2,al
push ax
INSIDEGRID dummyData1,dummyData2
pop ax 
cmp bx,0
jne temp31
jmp break22
temp31:
push ax
ISEMPTY dummyData1,dummyData2
pop ax 
cmp bx,0
jne temp32
jmp break2
temp32:
push ax
GETARINDEXBYBYTE  dummyData1,dummyData2
pop ax
mov cursorState[bx],1
jmp Second_Loop
break2:
ISWHITEBYTE dummyData1,dummyData2
cmp bx,1
jne temp27
jmp break22
temp27:
push ax
GETARINDEXBYBYTE  dummyData1,dummyData2
pop ax
mov cursorState[bx],1
break22:


mov ah,dh
mov al,dl
Third_Loop:
dec ah
inc al
mov dummyData1,ah
mov dummyData2,al
push ax
INSIDEGRID dummyData1,dummyData2
pop ax 
cmp bx,0
jne temp33
jmp break32
temp33:
push ax
ISEMPTY dummyData1,dummyData2
pop ax 
cmp bx,0
jne temp34
jmp break3
temp34:
push ax
GETARINDEXBYBYTE  dummyData1,dummyData2
pop ax
mov cursorState[bx],1
jmp Third_Loop
break3:
ISWHITEBYTE dummyData1,dummyData2
cmp bx,1
jne temp28
jmp break32
temp28:
push ax
GETARINDEXBYBYTE  dummyData1,dummyData2
pop ax
mov cursorState[bx],1
break32:


mov ah,dh
mov al,dl
Fourth_Loop:
dec al
inc ah
mov dummyData1,ah
mov dummyData2,al
push ax
INSIDEGRID dummyData1,dummyData2
pop ax 
cmp bx,0
jne temp35
jmp break42
temp35:
push ax
ISEMPTY dummyData1,dummyData2
pop ax 
cmp bx,0
jne temp36
jmp break4
temp36:
push ax
GETARINDEXBYBYTE  dummyData1,dummyData2
pop ax
mov cursorState[bx],1
jmp Fourth_Loop
break4:
ISWHITEBYTE dummyData1,dummyData2
cmp bx,1
jne temp29
jmp break42
temp29:
push ax
GETARINDEXBYBYTE  dummyData1,dummyData2
pop ax
mov cursorState[bx],1
break42:

ENDM HANDLEBISHOP

HANDLEQUEEN MACRO x,y
mov dh,x
mov dl,y
pusha
HANDLEROOK x,y
popa
mov x,dh
mov y,dl
pusha
HANDLEBISHOP x,y
popa
ENDM HANDLEQUEEN

HANDLEKNIGHT MACRO X,Y
                LOCAL VALID
                LOCAL NOTVALID
                LOCAL LOOP1 
                LOCAL CHECK2
                LOCAL WHITE 
                LOCAL BLACK
                LOCAL RETURN
                LOCAL skp

                    MOV AX,  3 ; ROW 
                    MOV DX , 3 ; COL
                    MOV CX,8 
                    LEA SI , knightdx
                    LEA DI , knightdy
                    PUSH AX
                    PUSH DX
                    LOOP1:
                    LEA SI , knightdx
                    LEA DI , knightdy
                    MOV AX,  X ; ROW 
                    MOV DX , Y ; COL
                    ADD SI ,CX
                    ADD DI, CX
                        ADD AX , [SI]
                        ADD DX , [DI]
                        PUSH AX
                        INSIDEGRID AL,DL
                        POP AX
                        CMP BX,0 
                        JE NOTVALID 

                        CHECK2:

                        VALID: 
                        MOV dummyData1 , AL
                        MOV dummyData2 ,  DL

                        push ax
                        GETARINDEXBYBYTE  dummyData1,dummyData2 ;out in bx
                        pop ax

                        pusha
                        ISWHITEBYTE dummyData1,dummyData2 ;out in bx
                        cmp bx,0 ;check if black
                        jne skp  ; jmp if white
                        popa

                        mov cursorState[bx],1
                        jmp NOTVALID
                        
                        skp:
                          POPA
                          mov cursorState[bx],0

                        NOTVALID:
                          CMP CX,0
                          JE RETURN
                          DEC CX
                          JMP LOOP1
                        RETURN:      
ENDM HANDLEKNIGHT

HANDLEPAWN MACRO X,Y
LOCAL ITSVALID1
LOCAL CONTINUOUECHECK
LOCAL ITSVALID2
LOCAL CONTINUOUECHECK2
LOCAL RIGHTNOFOE
LOCAL LEFTNOFOE
LOCAL ITSVALID3
LOCAL ITSVALID4
LOCAL CANMOVEFORWARD
LOCAL ITSVALID5
LOCAL ONEMOVEONLY
LOCAL CANTMOVE
;;;;;;;;;;;;;

MOV AX,X
MOV CX,Y
DEC AX
INC CX
;;;;;;;;;;;;;;;;;;;;;;INSIDE GRID
INSIDEGRID AX,CX ;; 1 IF VALID AND 0 IF NOT
CMP BX,1
JZ ITSVALID1
MOV DL,0
JMP CONTINUOUECHECK
ITSVALID1:
;;;;;;;;;;;;;

MOV AX,X
MOV CX,Y
DEC AX
INC CX
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;TO EAT RIGHT
ISEMPTY AL,CL ;NOT EMPTY 1 IF NOT EMPETY
MOV DL,0
CMP BX,0
JNZ CONTINUOUECHECK
;;;;;;;;;;;;;

MOV AX,X
MOV CX,Y
DEC AX
INC CX
;;;;;;;;;;;;;;
ISWHITE AX,CX
CMP BX,1
JZ CONTINUOUECHECK
MOV DL,1 ; RIGHT HAS FOE
CONTINUOUECHECK:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;TO EAT LEFT
;;;;;;;;;;;;;

MOV AX,X
MOV CX,Y
DEC AX
DEC CX
;;;;;;;;;;;;;;;;;;;;;;INSIDE GRID
INSIDEGRID AX,CX ;; 1 IF VALID AND 0 IF NOT
CMP BX,1
JZ ITSVALID2
MOV DH,0
JMP CONTINUOUECHECK2
ITSVALID2:
;;;;;;;;;;;;;

MOV AX,X
MOV CX,Y
DEC AX
DEC CX
;;;;;;;;;;;;;
ISEMPTY AL,CL ;NOT EMPTY
MOV DH,0
CMP BX,0
JNZ CONTINUOUECHECK2
;;;;;;;;;;;;;;;;;;

MOV AX,X
MOV CX,Y
DEC AX
DEC CX
;;;;;;;;;;;;;;;;;;
ISWHITE AX,CX
CMP BX,1
JZ CONTINUOUECHECK2
MOV DH,1 ;LEFT HAS FOE
CONTINUOUECHECK2:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;RIGHT SIDE
CMP DL,1
JNZ RIGHTNOFOE
;;;;;;;;;;;;;

MOV AX,X
MOV CX,Y
DEC AX
INC CX
GETARINDEX AX,CX  ;GET NEW INDEX
;;;;;;;;;;;;;
MOV cursorState[BX],1
RIGHTNOFOE:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;LEFT SIDE
CMP DH,1
JNZ LEFTNOFOE
;;;;;;;;;;;;;

MOV AX,X
MOV CX,Y
DEC AX
DEC CX
GETARINDEX AX,CX  ;GET NEW INDEX
;;;;;;;;;;;;;
MOV cursorState[BX],1
LEFTNOFOE:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;MOV FOWARD
;;;;;;;;;;;;;;;;;

MOV AX,X
MOV CX,Y
DEC AX
;;;;;;;;;;;;;;;;;
INSIDEGRID AX,CX
CMP BX,1
JZ ITSVALID3
JMP CANTMOVE
ITSVALID3:
;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;

MOV AX,X
MOV CX,Y
DEC AX
;;;;;;;;;;;;;;;;;
ISWHITE AX,CX
CMP BX,1
JNZ ITSVALID4
JMP CANTMOVE
ITSVALID4:
;;;;;;;;;;;;;;;;;

MOV AX,X
MOV CX,Y
DEC AX
;;;;;;;;;;;;;;;;;
ISEMPTY AL,CL
CMP BX,1
JZ CANMOVEFORWARD
;;ELSE
JMP CANTMOVE
CANMOVEFORWARD:
MOV AX,X
MOV CX,Y
GETARINDEX AX,CX 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;CHECK FIRST ROW;;;;;;;;;;;;;;;
CMP BX,56
JC SEEIFGREATERTAHN47OREQUAL
JMP ONEMOVEONLY

SEEIFGREATERTAHN47OREQUAL:
CMP BX,48
JNC TWOMOVES
JMP ONEMOVEONLY

TWOMOVES:
;;;;;;;;;;;;;;;;;SEE IF TWO IS EMPTY
MOV AX,X
MOV CX,Y
DEC AX
DEC AX
ISEMPTY AL,CL
CMP BX,1
JZ ITSVALID5
JMP ONEMOVEONLY
ITSVALID5:
;;;;;;;;;;;;;;;;;;;;;;FRONT TO THE PAW BY TWO
MOV AX,X
MOV CX,Y
DEC AX
DEC AX
GETARINDEX AX,CX 
MOV cursorState[BX],1
;;;;;;;;;;;;;;;;;;;;;;FRONT TO THE PAW BY ONE
ONEMOVEONLY:
MOV AX,X
MOV CX,Y
DEC AX
GETARINDEX AX,CX
MOV cursorState[BX],1
CANTMOVE:


ENDM PAWNAVALIABLEMOVES

HANDLEKING MACRO X,Y

ENDM HANDLEKING

GETARINDEXBYBYTE MACRO X,Y ;OUTPUT IN BX
    MOV AL,X 
    MOV BL , 8D
    MUL BL 
    ADD AL , Y
    MOV BL,AL
    MOV BH,0
ENDM GETARINDEXBYBYTE

SECONDQHANDLE MACRO
            LOCAL SKIP

                  GETARINDEX startRowCursor,startColCursor

                  MOV cl,BYTE PTR cellColorState
                  mov BYTE PTR colorState[bx],cl
                  mov cl,BYTE PTR curRowCursor
                  mov ch,BYTE PTR curColCursor
                  mov BYTE PTR endRowCursor,cl
                  mov BYTE PTR endColCursor,ch
                  mov cx,bx ;START INDEX

                  GETARINDEX endRowCursor,endColCursor

                  CMP cursorState[BX],0
                  JE SKIP
                  PUSHA
                  mov si,cx ;START INDEX
                  mov dh,BYTE PTR gridState[si] ;DATA OF FIRST INDEX
                  mov gridState[si],0 ;CLEAR START
                  mov si,bx ;END INDEX
                  mov gridState[si],dh ; MOVE START TO END
                  POPA
                SKIP:
                  pusha
                  UPDATECELL     startRowCursor,startColCursor,150D,0D
                  popa

                  pusha
                  UPDATECELL     endRowCursor,endColCursor,150D,0D
                  popa

                  PUSHA
                  CLEAR_AVAILABLE_PLACES
                  POPA

                  DRAWWITHSOURCE       borderdata,borderwidth,borderheight,endRowCursor,endColCursor,150D,0D    ; col,row

                  mov bl,BYTE PTR stateOfQ
                  dec bl
                  mov BYTE PTR stateOfQ,bl
                  
                  
ENDM SECONDQHANDLE

CLEAR_AVAILABLE_PLACES MACRO
    LOCAL Nloop9
    LOCAL Nloop10
    LOCAL TMP
    LOCAL TMP2
    LOCAL TMP3
    LOCAL Nbreak6

                  mov                   al,0
                  mov                   ah,0
    Nloop9:       
                  mov                   AL,0
    Nloop10:      
                  mov                   dummyData1,al
                  mov                   dummyData2,ah
                  pusha
                  GETARINDEXBYBYTE      dummyData1,dummyData2
                  cmp                   cursorState[bx],0
                  JNE                   TMP
                  JMP                   Nbreak6
    TMP:          
                  MOV                   cursorState[bx],0
                  popa
                  MOV                   dummyData3,0D
                  MOV                   dummyData4,0D
                  ADD                   dummyData3,al
                  ADD                   dummyData4,ah
                  pusha
                  UPDATECELL            dummyData3,dummyData4,150D,0D
                  
    Nbreak6:      
                  popa
                  inc                   al
                  cmp                   al,8
                  JE                   TMP2
                  JMP                   Nloop10
    TMP2:         
                  inc                   ah
                  cmp                   ah,8
                  JE                   TMP3
                  JMP                   Nloop9
    TMP3:

    ENDM CLEAR_AVAILABLE_PLACES

; CURSORMOV MACRO 
;   LOCAL tmplabel10
;   LOCAL label6
;   LOCAL label7
;   LOCAL label8
;   LOCAL label9
;   LOCAL left
;   LOCAL temp20
;   LOCAL label5
;   LOCAL right
;   LOCAL temp22
;   LOCAL label4
;   LOCAL up
;   LOCAL label10
;   LOCAL label2
;   LOCAL down
;   LOCAL label11
;   LOCAL label1
;   LOCAL qpressed
;   LOCAL tmplabel20
;   LOCAL firsrQ
;   LOCAL temp23


; cursorLoop:


;                   mov             ah,0
;                   int             16h

;                   ;if f4 is pressed return to main screen  
;                   cmp ah,3Eh
;                   jnz dontexit
;                   jmp faraway  
;                   dontexit: 

;                   cmp ah,10h
;                   jnz             tmplabel10
;                   jmp qpressed
; tmplabel10:

;                   cmp             ah,40h
;                   jnz             temp23
;                   jmp             gameChat
;     temp23: 
;                   cmp             ah,11h
;                   jnz             label6
;                   jmp             up
;     label6:

;                   cmp             ah,1eh
;                   jnz             label7
;                   jmp             left
;     label7:

;                   cmp             ah,20h
;                   jnz             label8
;                   jmp             right
;     label8:

;                   cmp             ah,1fh
;                   jnz             label9
;                   jmp             down
;     label9:

;                   jmp             cursorLoop                  

;     left:
;                   mov             dx,curColCursor
;                   cmp             dx,0D
;                   jnz             temp20

;                   jmp             cursorLoop
;     temp20:
;  pusha
;                   UPDATECELL     curRowCursor,curColCursor,150D,0D
;                   popa
;                   sub             dx,1D

;                   mov             curColCursor,dx
;                   DRAWWITHSOURCE       borderdata,borderwidth,borderheight,curRowCursor,curColCursor,150D,0D    ; col,row
;                   cmp             ah,11h
;                   jz              label5

;                   jmp             cursorLoop
;     label5:


;     right:
;                   mov             dx,curColCursor
;                   cmp             dx,7d
;                   jnz             temp22
;                   jmp             cursorLoop
;     temp22:
;  pusha
;                   UPDATECELL     curRowCursor,curColCursor,150D,0D
;                   popa
;                   add             dx,1
;                   mov             curColCursor,dx
;                   DRAWWITHSOURCE       borderdata,borderwidth,borderheight,curRowCursor,curColCursor,150D,0D    ; col,row
;                   cmp             ah,20h
;                   jz              label4
;                   jmp             cursorLoop
;     label4:

;     up:
;                   mov             dx,curRowCursor
;                   cmp             dx,0D
;                   jnz             label10
;                   jmp             cursorLoop
;     label10:

;  pusha
;                   UPDATECELL     curRowCursor,curColCursor,150D,0D
;                   popa
;                   sub             dx,1D

;                   mov             curRowCursor,dx
;                   DRAWWITHSOURCE       borderdata,borderwidth,borderheight,curRowCursor,curColCursor,150D,0D    ; col,row
;                   cmp             ah,11h
;                   jz              label2

;                   jmp             cursorLoop
;     label2:


;     down:
;                   mov             dx,curRowCursor
;                   cmp             dx,7D
;                   jnz             label11
;                   jmp             cursorLoop
;     label11:
;                   pusha
;                   UPDATECELL     curRowCursor,curColCursor,150D,0D
;                   popa

;                   add             dx,1
;                   mov             curRowCursor,dx
;                   DRAWWITHSOURCE       borderdata,borderwidth,borderheight,curRowCursor,curColCursor,150D,0D    ; col,row
;                   cmp             ah,1fh
;                   jz              label1
;                   jmp             cursorLoop
;     label1:

;     qpressed:
;     mov bl,stateOfQ
;     cmp bl,0
;     jnz tmplabel20
;     jmp firsrQ
;     tmplabel20:


;    SECONDQHANDLE
;     jmp   cursorLoop   

;     firsrQ:
;     FIRSTQHANDLE
;                  jmp             cursorLoop   
;                                   gameChat:


; ENDM CURSORMOV

CURSORMOV MACRO 
  LOCAL tmplabel10
  LOCAL label6
  LOCAL label7
  LOCAL label8
  LOCAL label9
  LOCAL left
  LOCAL temp20
  LOCAL label5
  LOCAL right
  LOCAL temp22
  LOCAL label4
  LOCAL up
  LOCAL label10
  LOCAL label2
  LOCAL down
  LOCAL label11
  LOCAL label1
  LOCAL qpressed
  LOCAL tmplabel20
  LOCAL firsrQ
  LOCAL temp23

cursorLoop:


                  mov             ah,0
                  int             16h

                  ;if f4 is pressed return to main screen  
                  cmp ah,3Eh
                  jnz dontexit
                  jmp faraway  
                  dontexit: 

                  cmp ah,10h
                  jnz             tmplabel10
                  jmp qpressed
tmplabel10:

                  cmp             ah,40h
                  jnz             temp23
                  jmp             gameChat
    temp23:

                  pusha
                  GETARINDEXBYBYTE curRowCursor,startColCursor
                  popa
                 ; mov firstIndex,bx

                  cmp             ah,11h
                  jnz             label6
                  jmp             up
    label6:

                  cmp             ah,1eh
                  jnz             label7
                  jmp             left
    label7:

                  cmp             ah,20h
                  jnz             label8
                  jmp             right
    label8:

                  cmp             ah,1fh
                  jnz             label9
                  jmp             down
    label9:

                  jmp             cursorLoop                  

    left:
                  mov             dx,curColCursor
                  cmp             dx,0D
                  jnz             temp20

                  jmp             cursorLoop
    temp20:
 pusha
                  UPDATECELL     curRowCursor,curColCursor,150D,0D
                  popa

                  pusha
                  GETARINDEXBYBYTE curRowCursor,curColCursor
                  mov firstIndex,bx
                  popa
                  mov bx,firstIndex
                  cmp cursorState[bx],0
                  je skip1
                  pusha
                  DRAWWITHSOURCE       selectdata,borderwidth,borderheight,curRowCursor,curColCursor,150D,0D    ; col,row
                  popa
                  skip1:
                  
                  sub             dx,1D

                  mov             curColCursor,dx
                  DRAWWITHSOURCE       borderdata,borderwidth,borderheight,curRowCursor,curColCursor,150D,0D    ; col,row
                  cmp             ah,11h
                  jz              label5

                  jmp             cursorLoop
    label5:


    right:
                  mov             dx,curColCursor
                  cmp             dx,7d
                  jnz             temp22
                  jmp             cursorLoop
    temp22:
  pusha
                  UPDATECELL     curRowCursor,curColCursor,150D,0D
                  popa

                  pusha
                  GETARINDEXBYBYTE curRowCursor,curColCursor
                  mov firstIndex,bx
                  popa
                  mov bx,firstIndex
                  cmp cursorState[bx],0
                  je skip2
                  pusha
                  DRAWWITHSOURCE       selectdata,borderwidth,borderheight,curRowCursor,curColCursor,150D,0D    ; col,row
                  popa
                  skip2:
                  add             dx,1
                  mov             curColCursor,dx
                  DRAWWITHSOURCE       borderdata,borderwidth,borderheight,curRowCursor,curColCursor,150D,0D    ; col,row
                  cmp             ah,20h
                  jz              label4
                  jmp             cursorLoop
    label4:

    up:
                  mov             dx,curRowCursor
                  cmp             dx,0D
                  jnz             label10
                  jmp             cursorLoop
    label10:

  pusha
                  UPDATECELL     curRowCursor,curColCursor,150D,0D
                  popa

                  pusha
                  GETARINDEXBYBYTE curRowCursor,curColCursor
                  mov firstIndex,bx
                  popa
                  mov bx,firstIndex
                  cmp cursorState[bx],0
                  je skip3
                  pusha
                  DRAWWITHSOURCE       selectdata,borderwidth,borderheight,curRowCursor,curColCursor,150D,0D    ; col,row
                  popa
                  skip3:
                  sub             dx,1D

                  mov             curRowCursor,dx
                  DRAWWITHSOURCE       borderdata,borderwidth,borderheight,curRowCursor,curColCursor,150D,0D    ; col,row
                  cmp             ah,11h
                  jz              label2

                  jmp             cursorLoop
    label2:


    down:
                  mov             dx,curRowCursor
                  cmp             dx,7D
                  jnz             label11
                  jmp             cursorLoop
    label11:
                  pusha
                  UPDATECELL     curRowCursor,curColCursor,150D,0D
                  popa

                  pusha
                  GETARINDEXBYBYTE curRowCursor,curColCursor
                  mov firstIndex,bx
                  popa
                  mov bx,firstIndex
                  cmp cursorState[bx],0
                  je skip4
                  pusha
                  DRAWWITHSOURCE       selectdata,borderwidth,borderheight,curRowCursor,curColCursor,150D,0D    ; col,row
                  popa
                  skip4:

                  add             dx,1
                  mov             curRowCursor,dx
                  DRAWWITHSOURCE       borderdata,borderwidth,borderheight,curRowCursor,curColCursor,150D,0D    ; col,row
                  cmp             ah,1fh
                  jz              label1
                  jmp             cursorLoop
    label1:

    qpressed:
    mov bl,stateOfQ
    cmp bl,0
    jnz tmplabel20
    jmp firsrQ
    tmplabel20:


   SECONDQHANDLE
    jmp   cursorLoop   

    firsrQ:
    FIRSTQHANDLE
                 jmp             cursorLoop   

                 gameChat:

ENDM CURSORMOV

DRAW_AVAILABLE_PLACES MACRO
LOCAL loop9
LOCAL loop10
LOCAL break6

mov al,0
mov ah,0
loop9:
mov al,0
loop10:
 mov dummyData1,al
 mov dummyData2,ah
 pusha
 GETARINDEXBYBYTE dummyData1,dummyData2
 cmp cursorState[bx],0
 je break6
 popa
 mov dummyData1,al
 mov dummyData2,ah
 pusha
DRAWWITHSOURCE       selectdata,selectwidth,selectheight,dummyData1,dummyData2,150D,0D 
break6:
popa
inc al
cmp al,8
jne loop10
inc ah
cmp ah,8
jne loop9

ENDM DRAW_AVAILABLE_PLACES

DrawPiecies MACRO A,B
        ;white
                  DRAW        wrockwidth,wrockheight,0,0,A,B                       ; col,row
                  DRAW        wknightwidth,wknightheight,0,1,A,B                 ; col,row
                  DRAW        wbishopwidth,wbishopheight,0,2,A,B                ; col,row
                  DRAW        wqueenwidth,wqueenheight,0,3,A,B                   ; col,row
                  DRAW        wkingwidth,wkingheight,0,4,A,B                      ; col,row
                  DRAW        wbishopwidth,wbishopheight,0,5,A,B                ; col,row
                  DRAW        wknightwidth,wknightheight,0,6,A,B                ; col,row
                  DRAW        wrockwidth,wrockheight,0,7,A,B                      ; col,row

                  DRAW        wpawnwidth,wpawnheight,1,0,A,B                       ; col,row
                  DRAW        wpawnwidth,wpawnheight,1,1,A,B                      ; col,row
                  DRAW        wpawnwidth,wpawnheight,1,2,A,B                     ; col,row
                  DRAW        wpawnwidth,wpawnheight,1,3,A,B                     ; col,row
                  DRAW        wpawnwidth,wpawnheight,1,4,A,B                     ; col,row
                  DRAW        wpawnwidth,wpawnheight,1,5,A,B                     ; col,row
                  DRAW        wpawnwidth,wpawnheight,1,6,A,B                     ; col,row
                  DRAW        wpawnwidth,wpawnheight,1,7,A,B                     ; col,row
        ;black
                  DRAW        brockwidth,brockheight,7,0,A,B                      ; col,row
                  DRAW        bknightwidth,bknightheight,7,1,A,B               ; col,row
                  DRAW        bbishopwidth,bbishopheight,7,2,A,B              ; col,row
                  DRAW        bqueenwidth,bqueenheight,7,3,A,B                 ; col,row
                  DRAW        bkingwidth,bkingheight,7,4,A,B                    ; col,row
                  DRAW        bbishopwidth,bbishopheight,7,5,A,B              ; col,row
                  DRAW        bknightwidth,bknightheight,7,6,A,B              ; col,row
                  DRAW        brockwidth,brockheight,7,7,A,B                    ; col,row

                  DRAW        bpawnwidth,bpawnheight,6,0,A,B                     ; col,row
                  DRAW        bpawnwidth,bpawnheight,6,1,A,B                   ; col,row
                  DRAW        bpawnwidth,bpawnheight,6,2,A,B                   ; col,row
                  DRAW        bpawnwidth,bpawnheight,6,3,A,B                   ; col,row
                  DRAW        bpawnwidth,bpawnheight,6,4,A,B                  ; col,row
                  DRAW        bpawnwidth,bpawnheight,6,5,A,B                  ; col,row
                  DRAW        bpawnwidth,bpawnheight,6,6,A,B                  ; col,row
                  DRAW        bpawnwidth,bpawnheight,6,7,A,B                   ; col,row
ENDM DrawPiecies

;(0,0),(0,1)
;(1,0)......
;      (7,7)
;

getDrawPosition MACRO A,B,ROW,COL ;Takes the row and col and set the cx and dx to the required values to draw
 MOV         AL,BYTE PTR ROW
 MOV         CL,60D
 MUL         CL
 MOV         dx,ax
 MOV         AL,BYTE PTR COL
 MUL         CL
 MOV         CX,Ax
 ADD CX,WORD PTR A
 ADD DX,WORD PTR B
ENDM getDrawPosition

INITIALIZEGRID MACRO A,B

mov colorState[0],A
mov colorState[1],B
mov colorState[2],A
mov colorState[3],B
mov colorState[4],A
mov colorState[5],B
mov colorState[6],A
mov colorState[7],B
mov colorState[8],B
mov colorState[9],A
mov colorState[10],B
mov colorState[11],A
mov colorState[12],B
mov colorState[13],A
mov colorState[14],B
mov colorState[15],A
mov colorState[16],A
mov colorState[17],B
mov colorState[18],A
mov colorState[19],B
mov colorState[20],A
mov colorState[21],B
mov colorState[22],A
mov colorState[23],B
mov colorState[24],B
mov colorState[25],A
mov colorState[26],B
mov colorState[27],A
mov colorState[28],B
mov colorState[29],A
mov colorState[30],B
mov colorState[31],A
mov colorState[32],A
mov colorState[33],B
mov colorState[34],A
mov colorState[35],B
mov colorState[36],A
mov colorState[37],B
mov colorState[38],A
mov colorState[39],B
mov colorState[40],B
mov colorState[41],A
mov colorState[42],B
mov colorState[43],A
mov colorState[44],B
mov colorState[45],A
mov colorState[46],B
mov colorState[47],A
mov colorState[48],A
mov colorState[49],B
mov colorState[50],A
mov colorState[51],B
mov colorState[52],A
mov colorState[53],B
mov colorState[54],A
mov colorState[55],B
mov colorState[56],B
mov colorState[57],A
mov colorState[58],B
mov colorState[59],A
mov colorState[60],B
mov colorState[61],A
mov colorState[62],B
mov colorState[63],A

mov gridState[0],1 ;black rook
mov gridState[1],2 ;black knight
mov gridState[2],3 ;black bishop
mov gridState[3],4 ;black queen
mov gridState[4],5 ;black king
mov gridState[5],3 ;black bishop
mov gridState[6],2 ;black knight
mov gridState[7],1 ;black rook

mov gridState[8], 6 ;black pawn
mov gridState[9], 6 ;black pawn
mov gridState[10],6 ;black pawn
mov gridState[11],6 ;black pawn
mov gridState[12],6 ;black pawn
mov gridState[13],6 ;black pawn
mov gridState[14],6 ;black pawn
mov gridState[15],6 ;black pawn

mov gridState[16],0
mov gridState[17],0
mov gridState[18],0
mov gridState[19],0
mov gridState[20],0
mov gridState[21],0
mov gridState[22],0
mov gridState[23],0
mov gridState[24],0
mov gridState[25],0
mov gridState[26],0
mov gridState[27],0
mov gridState[28],0
mov gridState[29],0
mov gridState[30],0
mov gridState[31],0
mov gridState[32],0
mov gridState[33],0
mov gridState[34],0
mov gridState[35],0
mov gridState[36],0
mov gridState[37],0
mov gridState[38],0
mov gridState[39],0
mov gridState[40],0
mov gridState[41],0
mov gridState[42],0
mov gridState[43],0
mov gridState[44],0
mov gridState[45],0
mov gridState[46],0
mov gridState[47],0

mov gridState[48],7 ;white pawn
mov gridState[49],7 ;white pawn
mov gridState[50],7 ;white pawn
mov gridState[51],7 ;white pawn
mov gridState[52],7 ;white pawn
mov gridState[53],7 ;white pawn
mov gridState[54],7 ;white pawn
mov gridState[55],7 ;white pawn

mov gridState[56],8  ;white rook
mov gridState[57],9  ;white knight
mov gridState[58],10 ;white bishop
mov gridState[59],11 ;white queen
mov gridState[60],12 ;white king
mov gridState[61],10 ;white bishop
mov gridState[62],9  ;white knight
mov gridState[63],8  ;white rook

ENDM INITIALIZEGRID

; isEmpty MACRO X,Y 
; LOCAL notEmpty 

; MOV AX,X
; MOV CX,Y

; LEA SI,gridState 
; GETARINDEX AX,CX 
; ADD SI,BX
; MOV CH,BYTE PTR [SI]
; CMP CH , 0 
; jnz notEmpty 

; MOV BX, 0ffh;;to get farway from the array index 0
; notEmpty: 

; ENDM isEmpty

ISEMPTY MACRO x,y
LOCAL break5
LOCAL empty
pusha
GETARINDEXBYBYTE x,y
cmp  gridState[bx],0
je empty
popa
mov bx,0
jmp break5
empty:
popa
mov bx,1
break5:
ENDM ISEMPTY

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
    JMP fistcheck
;---------fist check with enter message-----------;
    repeatt:

    CALL CLS

    movecursor  17H,05H
    ShowMessage strFailed
    movecursor  17H,06H
    cin         name

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

movecursorWithPageNumber MACRO x,y,p ;move cursor
                mov         ah,2
                mov         bh,p
                mov cl,x
                mov ch,y
                mov         dh,ch
                mov         dl,cl
                int         10h
ENDM        movecursorWithPageNumber

MAINMAIN MACRO player1Name,player2Name
    LOCAL check_for_anotherkey
    LOCAL check_for_f2
    LOCAL check_for_esc

    check_for_anotherkey:
    mov ah,0
    int 16h 
    cmp ah,3bh;f1 scan code
    jz skipf2chk;;;;;;;;;;;;;;;;;;;;;;;;;;;
    jmp check_for_f2
    skipf2chk:
    ;open chat
    OPENCHAT player1Name+2,player2Name+2
    check_for_f2:
    cmp ah,3ch;f2 scane code
    jnz check_for_esc
    ;open game
    jmp play

    check_for_esc:
    cmp al,01Bh;esc ascii
    jz skipescchk
    jmp check_for_anotherkey;;;;;;;;;;;;;;;;;;
    skipescchk:
    ;exist game
    MOV AH, 4CH
    MOV AL, 01 ;your return code.
    INT 21H

ENDM MAINMAIN

OPENCHAT MACRO player1Name,player2Name
            LOCAL dead
            LOCAL mainloop
            LOCAL afterenter
            local afterenter2
            LOCAL deadmid
            local midh
            local CHK
            local AGAIN

                  mov  al, 01h   ; select display page 1
                  mov  ah, 05h   ; function 05h: select active display page
                  int  10h

                  mov         ax,0620h ;clear page
                  mov         bh,07
                  mov         cx,0000H
                  mov         dx,304FH
                  int         10h

                  movecursorWithPageNumber  00,0AH,1D
                  ShowMessage line

                  movecursorWithPageNumber  00,00H,1D
                  ShowMessage player1Name

                  movecursorWithPageNumber  00,0BH,1D
                  ShowMessage player2Name

                  mov         dx,3fbh           ; Line Control Register
                  mov         al,10000000b      ;Set Divisor Latch Access Bit
                  out         dx,al


    ;000c => 9600 baud rate
    ;Set LSB byte of the Baud Rate Divisor Latch
                  mov         dx,3f8h
                  mov         al,0ch
                  out         dx,al


    ;Set MSB byte of the Baud Rate Divisor Latch register.
                  mov         dx,3f9h
                  mov         al,00h
                  out         dx,al


    ;Set port configuration
                  mov         dx,3fbh
                  mov         al,00011011b      ;011=> even parity 0=> one stop bit 11=> 8bits
                  out         dx,al


                  mov         dh,00H
                  mov         dl,0CH
                  push        dx
                  mov         dh,00H
                  mov         dl,01H
                  push        dx

                  movecursorWithPageNumber  00H,01H,1D
                
    ;program starts here
    mainloop:     
                  
                
                  mov         ah,01
                  int         16h
                  jz          AGAIN

                  mov         ah,0
                  int         16h
                  

                  pop         dx
                  push        dx
                  push        ax
                  movecursorWithPageNumber  dh,dl,1
                  pop         ax
                  pop         dx
                  inc         dh
                  push        dx

                  push        ax

                  mov         ah,2
                  mov         dl,al
                  int         21h

                  pop         ax

                  mov         bl,ah
                  cmp         bl,3dh            ; F3 key
                  je          deadmid

                  mov         bl,al
                  cmp         bl,13
                  jne         afterenter

                  pop         dx
                  inc         dl
                  mov         dh,0
                  PUSH        dx

                  cmp         dx,0009H          ;CURSOR CHECK
                  jne         afterenter

                  mov         ax,0601h
                  mov         bh,07
                  mov         cx,0100H
                  mov         dx,094FH
                  int         10h
                  POP         DX
                  MOV         DL,8
                  PUSH        DX

    afterenter:   
    ;Sending a value

    ;Check that Transmitter Holding Register is Empty
                  mov         dx , 3FDH         ; Line Status Register

                  In          al , dx           ;Read Line Status
                  AND         al , 00100000b
                  JZ          AGAIN             ;jump untill it is empty

    ;If empty put the VALUE in Transmit data register
                  mov         dx , 3F8H         ; Transmit data register
                  mov         al,bl
                  out         dx , al
                  jmp         AGAIN
           
    ;Receiving a value
    deadmid:      
                  jmp         dead
                  
    AGAIN:        
    ;Check that Data Ready
                  mov         dx , 3FDH         ; Line Status Register
          
                  in          al , dx
                  AND         al , 00000001b
                  JZ          CHK               ;jump untill it recive data

    ;If Ready read the VALUE in Receive data register
                  mov         dx , 03F8H
                  in          al , dx
                  
                  pop         cx
                  pop         dx
                  push        dx
                  push        cx
                  movecursorWithPageNumber  dh,dl,1D
                  pop         cx
                  pop         dx
                  inc         dh
                  push        dx
                  push        cx

                  cmp         al,13
                  jne         afterenter2

                  pop         cx
                  pop         dx
                  push        dx
                  push        cx
                  movecursorWithPageNumber  dh,dl,1D
                  pop         cx
                  pop         dx
                  inc         dl
                  mov         dh,0
                  push        dx
                  push        cx

                  cmp         dx,0017H          ;CURSOR CHECK
                  jne         afterenter2
                

                  PUSH        BX
                  PUSH        AX
                  mov         ax,0601h
                  mov         bh,07
                  mov         cx,0C00H
                  mov         dx,164FH
                  int         10h
                  POP         AX
                  POP         BX
                  pop         cx
                  pop         dx
                  MOV         DL,16H
                  push        dx
                  push        cx

    afterenter2:  

                  mov         dl,al
                  mov         ah,2
                  int         21h
                  
    CHK:          

                  jmp         mainloop
    dead:  
                  mov  al, 00h   ; select display page 0
                  mov  ah, 05h   ; function 05h: select active display page
                  int  10h

ENDM OPENCHAT
                
GETARINDEX MACRO X,Y ;OUTPUT IN BX
    MOV AX,X 
    MOV BL , 8D
    MUL BL 
    ADD AX , Y
    MOV BX,AX
ENDM GETARINDEX

GETARINDEXBYTE MACRO X,Y ;OUTPUT IN BX
    MOV AL,X 
    MOV BL , 8D
    MUL BL 
    MOV CL , Y 
    MOV CH , 0 
    ADD AX , CX
    MOV BX,AX
ENDM GETARINDEX

UPDATECELL MACRO X,Y,A,B
    LOCAL NOPE
    getDrawPosition A,B,X,Y
    PUSH CX
    PUSH DX
    GETARINDEX X,Y
    MOV AL,BYTE PTR colorState[BX]
    DRAWCELL CX,DX,AL
    POP DX
    POP CX
    DRAW 60D,60D,X,Y,A,B
    NOPE:
ENDM UPDATECELL

GETIMGDATA MACRO X,Y
    LOCAL RETURN
    LOCAL EMPTY
    LOCAL EMPTY2
    LOCAL B1
    LOCAL B2
    LOCAL B3
    LOCAL B4
    LOCAL B5
    LOCAL B6
    LOCAL B7
    LOCAL B8
    LOCAL B9
    LOCAL B10
    LOCAL B11
    LOCAL B12

    ; GETS THE NUMBER IN GRID[X][Y]
    ; GETS THE IMGDATA REQUIRED FOR THE ICON IN GRID[X][Y]
    ;RETURNS THE IMAGE DATA IN BX 
    
    ; CONVERTING 2D TO 1D 
    MOV AX,X 
    MOV BL , 8D
    MUL BL 
    ADD AX , Y 

    ; GETTING THE STATE OF THE GRID AT (X,Y) WHICH IS 0 --> 12
    LEA SI,gridState
    ADD SI,AX
    MOV AL,BYTE PTR [SI]
    MOV AH,0H

    CMP AX,0
    JE EMPTY2
    
    ; DEC AX 
    ; MOV BX,360D
    ; MUL BX

    ; ; LOADING THE IMG DATA 
    ; LEA SI,BROCKDATA
    ; ADD SI,AX

    ; MOV BX,SI
    ; JMP RETURN 
    
    CMP AX,1D
    JE B1

   CMP AX,2D
   JE B2
   CMP AX,3D
   JE B3
   CMP AX,4D
   JE B4
   CMP AX,5D
   JE B5
   CMP AX,6D
   JE B6
   CMP AX,7D
   JE B7
   CMP AX,8D
   JE B8
   CMP AX,9D
   JE B9
   CMP AX,10D
   JE B10
   CMP AX,11D
   JE B11
   CMP AX,12D
   JE B12

   EMPTY2: JMP EMPTY
       

    B1: LEA BX,brockdata
    JMP RETURN
        B2: LEA BX,bknightdata       
    JMP RETURN
        B3: LEA BX,bbishopdata       
    JMP RETURN
        B4: LEA BX,bqueendata        
    JMP RETURN
        B5: LEA BX,bkingdata         
    JMP RETURN
        B6: LEA BX,bpawndata         
    JMP RETURN
        B7: LEA BX,wpawndata         
    JMP RETURN
        B8: LEA BX,wrockdata         
    JMP RETURN
        B9: LEA BX,wknightdata       
    JMP RETURN
        B10: LEA BX,wbishopdata       
    JMP RETURN
        B11: LEA BX,wqueendata        
    JMP RETURN
        B12: LEA BX,wkingdata         
    JMP RETURN

    EMPTY:
    MOV BX,0

    RETURN:

ENDM GETIMGDATA

ISWHITE MACRO X,Y 
LOCAL WHITE 
LOCAL BLACK 
LOCAL RETURN 


MOV AX,X
MOV CX,Y

LEA SI,gridState 
GETARINDEX AX,CX 
ADD SI,BX
MOV CH,BYTE PTR [SI]
CMP CH,6 
JG WHITE
CMP CH , 7 
JL BLACK 

WHITE:
MOV BX,1 
JMP RETURN
BLACK: 
MOV BX, 0
RETURN: 


ENDM ISWHITE

ISWHITEBYTE MACRO X,Y 
LOCAL WHITE 
LOCAL BLACK 
LOCAL RETURN 

MOV AX ,0 
MOV AL,X
MOV CL,Y
MOV CH,0

LEA SI,gridState 
GETARINDEX AX,CX 
ADD SI,BX
MOV CH,BYTE PTR [SI]
CMP CH,6 
JG WHITE
CMP CH , 7 
JL BLACK 

WHITE:
MOV BX,1 
JMP RETURN
BLACK: 
MOV BX, 0
RETURN: 


ENDM ISWHITEBYTE

INSIDEGRID MACRO X , Y 
LOCAL NOTVALID 
LOCAL VALID 
LOCAL RETURN 

MOV AL , X 
MOV AH , Y

CMP AL , 7 
JG NOTVALID
CMP AL , 0 
JL NOTVALID
CMP AH , 7 
JG NOTVALID
CMP AH , 0 
JL NOTVALID


VALID: 
MOV BX,1 
JMP RETURN
NOTVALID:
MOV BX, 0
RETURN: 

ENDM INSIDEGRID

ENTERGAMECHAT MACRO player1Name,player2Name
                        LOCAL dead
            LOCAL mainloop
            LOCAL afterenter
            local afterenter2
            LOCAL deadmid
            local midh
            local CHK
            local AGAIN

                  mov  al, 01h   ; select display page 1
                  mov  ah, 05h   ; function 05h: select active display page
                  int  10h

                ;   mov         ax,0620h ;clear page
                ;   mov         bh,07
                ;   mov         cx,0000H
                ;   mov         dx,304FH
                ;   int         10h

                  movecursorWithPageNumber  00,21H,1D
                  ShowMessage line

                  movecursorWithPageNumber  00,1FH,1D
                  ShowMessage player1Name

                  movecursorWithPageNumber  00,22H,1D
                  ShowMessage player2Name

                  mov         dx,3fbh           ; Line Control Register
                  mov         al,10000000b      ;Set Divisor Latch Access Bit
                  out         dx,al


    ;000c => 9600 baud rate
    ;Set LSB byte of the Baud Rate Divisor Latch
                  mov         dx,3f8h
                  mov         al,0ch
                  out         dx,al


    ;Set MSB byte of the Baud Rate Divisor Latch register.
                  mov         dx,3f9h
                  mov         al,00h
                  out         dx,al


    ;Set port configuration
                  mov         dx,3fbh
                  mov         al,00011011b      ;011=> even parity 0=> one stop bit 11=> 8bits
                  out         dx,al


                  mov         dh,00H
                  mov         dl,23H
                  push        dx
                  mov         dh,00H
                  mov         dl,20H
                  push        dx

                  movecursorWithPageNumber  00H,20H,1D
                
    ;program starts here
    mainloop:     
                  
                
                  mov         ah,01
                  int         16h
                  jz          AGAIN

                  mov         ah,0
                  int         16h
                  

                  pop         dx
                  push        dx
                  push        ax
                  movecursorWithPageNumber  dh,dl,1
                  pop         ax
                  pop         dx
                  inc         dh
                  push        dx

                  push        ax

                  mov         ah,2
                  mov         dl,al
                  int         21h

                  pop         ax

                  mov         bl,ah
                  cmp         bl,40H            ; F6 key
                  je          deadmid

                  mov         bl,al
                  cmp         bl,13
                  jne         afterenter

                  pop         dx
                  inc         dl
                  mov         dh,0
                  PUSH        dx

                  cmp         dx,0021H          ;CURSOR CHECK
                  jne         afterenter

                  pop         dx                ;clear line
                  push        dx
                  push        ax
                  MOV         DL,20H
                  movecursorWithPageNumber  dh,dl,1
                  ShowMessage clear
                  pop         ax
                  pop         dx
                  inc         dh
                  push        dx                  

                  POP         DX
                  MOV         DL,20H
                  PUSH        DX

    afterenter:   
    ;Sending a value

    ;Check that Transmitter Holding Register is Empty
                  mov         dx , 3FDH         ; Line Status Register

                  In          al , dx           ;Read Line Status
                  AND         al , 00100000b
                  JZ          AGAIN             ;jump untill it is empty

    ;If empty put the VALUE in Transmit data register
                  mov         dx , 3F8H         ; Transmit data register
                  mov         al,bl
                  out         dx , al
                  jmp         AGAIN
           
    ;Receiving a value
    deadmid:      
                  jmp         dead
                  
    AGAIN:        
    ;Check that Data Ready
                  mov         dx , 3FDH         ; Line Status Register
          
                  in          al , dx
                  AND         al , 00000001b
                  JZ          CHK               ;jump untill it recive data

    ;If Ready read the VALUE in Receive data register
                  mov         dx , 03F8H
                  in          al , dx
                  
                  pop         cx
                  pop         dx
                  push        dx
                  push        cx
                  movecursorWithPageNumber  dh,dl,1D
                  pop         cx
                  pop         dx
                  inc         dh
                  push        dx
                  push        cx

                  cmp         al,13
                  jne         afterenter2

                  pop         cx
                  pop         dx
                  push        dx
                  push        cx
                  MOV         DL,23H
                  movecursorWithPageNumber  dh,dl,1D
                  ShowMessage clear
                  pop         cx
                  pop         dx
                  inc         dl
                  mov         dh,0
                  push        dx
                  push        cx

                  cmp         dx,0024H          ;CURSOR CHECK
                  jne         afterenter2
                

                  PUSH        BX
                  PUSH        AX

                  MOV         DL,23H
                  movecursorWithPageNumber  dh,dl,1
                  ShowMessage clear

                ;   mov         ax,0601h
                ;   mov         bh,07
                ;   mov         cx,0C00H ;write alot of spaces
                ;   mov         dx,164FH
                ;   int         10h

                  POP         AX
                  POP         BX
                  pop         cx
                  pop         dx
                  MOV         DL,23H
                  push        dx
                  push        cx

    afterenter2:  

                  mov         dl,al
                  mov         ah,2
                  int         21h
                  
    CHK:          

                  jmp         mainloop
    dead:  
                  mov  al, 00h   ; select display page 0
                  mov  ah, 05h   ; function 05h: select active display page
                  int  10h

ENDM ENTERGAMECHAT

GETTIME MACRO

    MOV TIME,0
    MOV AH,2CH
    INT 21H ; SEC -> DH ;MIN -> CL ;HRS ->CH

    MOV AX,0
    MOV AL,CH
    MOV BL,24
    MUL BL

    ADD TIME,AX

    MOV AX,0
    MOV AL,CL
    MOV BL,60
    MUL BL

    ADD TIME,AX

    MOV AX,0
    MOV AL,dh

    ADD TIME,AX

ENDM GETTIME

.MODEL SMALL
.286
.STACK 64
;-----------
.Data
    nameq             db  'Please enter your name:','$'
    erroname          db  'Please write a valid name :','$'
    clear             db  '                                                                                                    ','$'
    line              db  '---------------------------------------------------','$'
    
    brockdata         db  60D*60D dup(0)
    bknightdata       db  60D*60D dup(0)
    bbishopdata       db  60D*60D dup(0)
    bqueendata        db  60D*60D dup(0)
    bkingdata         db  60D*60D dup(0)
    bpawndata         db  60D*60D dup(0)

    wpawndata         db  60D*60D dup(0)
    wrockdata         db  60D*60D dup(0)
    wknightdata       db  60D*60D dup(0)
    wbishopdata       db  60D*60D dup(0)
    wqueendata        db  60D*60D dup(0)
    wkingdata         db  60D*60D dup(0)


    knightdx          db  1,1,2,2,-2,-2, -1 , -1
    knightdy          db  -2,2,1,-1,1,-1,2,-2
    
    ; king_dx           db  1, 1 , 0,  0 , -1 , -1 , 1 , -1
    ; king_dy           db  1,-1 , 1, -1 , -1 ,  1 , 0 ,  0
    ; queen_dx          db  1, 1 , 0,  0 , -1 , -1 , 1 , -1
    ; queen_dy          db  1,-1 , 1, -1 , -1 ,  1 , 0 ,  0
    ; soldier_dx        db  1 ,
    ; soldier_dy        db  1 ,

    thename           db  16,?,16 dup('$')                                                                                              ; max size 15 char last digit for $
    proceed           db  'Please Enter key to continue','$'
    op1               db  'To start chatting press F1','$'
    op2               db  'To start the game press F2','$'
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
    selectwidth       equ 60D
    selectheight      equ 60D
    selectfilename    db  'select.bin',0
    selectfilehandle  DW  ?
    selectdata        db  selectwidth*selectheight dup(0)
    ;---------------

    borderwidth       equ 60D
    borderheight      equ 60D
    borderfilename    db  'border.bin',0
    borderfilehandle  DW  ?
    borderdata        db  borderwidth*borderheight dup(0)

    gridState         db  64  dup(0)
    colorState        db  64  dup(0)
    cursorState       db  64  dup(0)                                                                                                    ; 0 for not cursor 1 for cursor

    curRowCursor      dw  0
    curColCursor      dw  0

    startRowCursor    dw  0
    startColCursor    dw  0

    endRowCursor      dw  0
    endColCursor      dw  0

    cellColorState    db  0

    stateOfQ          db  0
    dummyData1        db  0
    dummyData2        db  0

    dummyData3        DW  0
    dummyData4        dW  0

    DUMMYX            DB  5
    DUMMYY            DB  5

    firstIndex        db  0
    TIME              DW  0
    ;---------------------------------------------------------------------------------------------------
 


.CODE
MAIN PROC FAR
    ;INITIALIZING
                  call           GETDATA
                  CALL           CLS
    ;OPENING AND READING BIN FILES
                  OpenFile       bbishopfilename, bbishopfilehandle
                  ReadData       bbishopfilehandle ,bbishopwidth,bbishopheight,bbishopdata
                  OpenFile       bkingfilename, bkingfilehandle
                  ReadData       bkingfilehandle ,bkingwidth,bkingheight,bkingdata
                  OpenFile       bknightfilename, bknightfilehandle
                  ReadData       bknightfilehandle ,bknightwidth,bknightheight,bknightdata
                  OpenFile       bpawnfilename, bpawnfilehandle
                  ReadData       bpawnfilehandle ,bpawnwidth,bpawnheight,bpawndata
                  OpenFile       bqueenfilename, bqueenfilehandle
                  ReadData       bqueenfilehandle ,bqueenwidth,bqueenheight,bqueendata
                  OpenFile       brockfilename, brockfilehandle
                  ReadData       brockfilehandle ,brockwidth,brockheight,brockdata
    ;--white piecies----
                  OpenFile       wbishopfilename, wbishopfilehandle
                  ReadData       wbishopfilehandle ,wbishopwidth,bbishopheight,wbishopdata
                  OpenFile       wkingfilename, wkingfilehandle
                  ReadData       wkingfilehandle ,wkingwidth,wkingheight,wkingdata
                  OpenFile       wknightfilename, wknightfilehandle
                  ReadData       wknightfilehandle ,wknightwidth,wknightheight,wknightdata
                  OpenFile       wpawnfilename, wpawnfilehandle
                  ReadData       wpawnfilehandle ,wpawnwidth,wpawnheight,wpawndata
                  OpenFile       wqueenfilename, wqueenfilehandle
                  ReadData       wqueenfilehandle ,wqueenwidth,wqueenheight,wqueendata
                  OpenFile       wrockfilename, wrockfilehandle
                  ReadData       wrockfilehandle ,wrockwidth,wrockheight,wrockdata
    ;--border-----
                  OpenFile       borderfilename, borderfilehandle
                  ReadData       borderfilehandle ,borderwidth,borderheight,borderdata
                  OpenFile       selectfilename, selectfilehandle
                  ReadData       selectfilehandle ,selectwidth,selectheight,selectdata
    ;------------------------------------------------------------------------------------------------
    ;------------------------------------------------------------------------------------------------
    ;------------------------------------------------------------------------------------------------
    ;------------------------------------------------------------------------------------------------

    ;START MENU
                  validateName   nameq,thename,erroname                                                   ;Veryyyyyyyyyyyyyyyy STABLE
                  movecursor     17H,0AH
                  ShowMessage    proceed
                  call           waitkey
    ;CHOICE MENU
    faraway:      

                  call           CLS
                  movecursor     17H,03H
                  ShowMessage    op1
                  movecursor     17H,08H
                  ShowMessage    op2
                  movecursor     17H,0DH
                  ShowMessage    op3
                  MAINMAIN       thename,thename
    ;GAME SCREEN
    play:         
                  CALL           EnterGraphics
                  mov            curColCursor,00h
                  mov            curRowCursor,07h
                  INITIALIZEGRID 0FH,08H
                  DrawGrid       150D,0D,colorState[1],colorState[0]
                  DrawPiecies    150D,0D

                  DRAWWITHSOURCE borderdata,borderwidth,borderheight,curRowCursor,curColCursor,150D,0D

    curs:         
                  CURSORMOV
                  ENTERGAMECHAT  thename+2,thename+2
                  JMP            curs
    ;----------------------------closing files--------------------------------------------------
                  CloseFile      bbishopfilehandle
                  CloseFile      bkingfilehandle
                  CloseFile      bknightfilehandle
                  CloseFile      bpawnfilehandle
                  CloseFile      bqueenfilehandle
                  CloseFile      brockfilehandle

                  CloseFile      wbishopfilehandle
                  CloseFile      wkingfilehandle
                  CloseFile      wknightfilehandle
                  CloseFile      wpawnfilehandle
                  CloseFile      wqueenfilehandle
                  CloseFile      wrockfilehandle

                  CloseFile      borderfilehandle

                  EXT
MAIN ENDP
    ;----------------------------------------------------------------------------------------------------------------


    ;--------------------------------------------------Functions---------------------------------------------------------
GETDATA PROC                                                                                              ;GET DATA
                  MOV            AX,@DATA
                  MOV            DS,AX
                  ret
GETDATA ENDP

CLS PROC                                                                                                  ;CLEAR SCREEN
                  MOV            AX,0003H                                                                 ;;ah == 0 set to graph mod the al = 3 return to text mode
                  INT            10H
                  ret
CLS ENDP

EnterText PROC                                                                                            ;ENTER TEXT MODE
                  MOV            AX,3H
                  INT            10H
                  ret
EnterText ENDP

EnterGraphics PROC                                                                                        ;ENTER GRAPHICS MODE
                  MOV            AX,4F02H
                  MOV            BX,103H                                                                  ;(800x600) pixel ;grid =480*480; char=60*60
                  INT            10H
                  ret
EnterGraphics ENDP

waitkey PROC                                                                                              ;wait for key
                  MOV            AH , 0
                  INT            16h
                  ret
waitkey ENDP

END MAIN

;http://www.wagemakers.be/english/doc/vga
