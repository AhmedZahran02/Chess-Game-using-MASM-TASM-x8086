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
                
                JMP far ptr clp
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
    je OUTH
    DEC SI
    POP BX
    mov [di],BH
    INC DI
    JMP far ptr MAKESTR
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
                Je  GOAWAY
        ; Drawing loop
                      mov di,0
        drawLoop:     
                      mov si,0
                      innerloop:
                      MOV  AL,BYTE PTR[BX]
                      MOV AH,0ch
                      cmp al,0FFH
                      Je  skp
                      INT 10H
                      skp:
                      INC BX
                      INC CX
                      INC SI
                      CMP SI,WORD ptr imgwidth
                      Jne  innerloop
                      SUB CX,SI
                      INC DX
                      INC DI
                      CMP DI,WORD ptr imgheight
                      Jne   drawLoop
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
                      Je  skp
                      INT 10H
                      skp:
                      INC BX
                      INC CX
                      INC SI
                      CMP SI,WORD PTR imgwidth
                      Jne  innerloop
                      SUB CX,SI
                      INC DX
                      INC DI
                      CMP DI,WORD PTR imgheight
                      Jne   drawLoop
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
            Jne innerloop
            SUB CX,SI
            INC DX
            INC DI
            CMP DI,60D
            Jne  drawLoop
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
        Jne   BIGGERLOOP

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
        Jne   BIGGERLOOP2

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
        Jne   BIGGERLOOP3

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
        Jne   BIGGERLOOP4        

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
        Jne   BIGGERLOOP5

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
        Jne   BIGGERLOOP6

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
        Jne   BIGGERLOOP7

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
        Jne   BIGGERLOOP8 
ENDM DrawGrid

FIRSTQHANDLE MACRO ;This Macro is Responsible for handling when first player press his first Q to select a piece want to move
    LOCAL PAWNTEMP
    local ROOKTEMP
    local KNIGHTTEMP
    local BISHOPTEMP
    local QUEENTEMP
    local KINGTEMP
    local PAWN
    local ROOK
    local KNIGHT
    local BISHOP
    local QUEEN
    local KING
    local NOACTION
    local outterr
    local break80
    local temp151
    local temp150
    local temp100
                  ;if the cell is empty get out
                ;   cmp bl,6
                ;   Jz far ptr isapawn
                ;   JMP far ptr getouttt
                ;   isapawn:  
                ;   PAWNAVALIABLEMOVES curRowCursor,curColCursor
                ;   getouttt: 

                ;   isEmpty curRowCursor,curColCursor
                ;   PAWNAVALIABLEMOVES curRowCursor,curColCursor
                ;   ;KnightMovements curRowCursor,curColCursor
                ;   cmp bl,0FFH
                ;   jnz getoutt
                ;   JMP far ptr outterr
                ;   getoutt:
                  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                  GETARINDEX curRowCursor,curColCursor

                  pusha
                  ISEMPTY curRowCursor,curColCursor
                    cmp bx,1
                    Jne  temp100
                    JMP  break80
                    temp100:
                    popa

                  pusha
                  ISWHITEBYTE curRowCursor,curColCursor
                  cmp bx,0
                  Jne  temp150
                  JMP  break80
                  temp150:
                  popa

                ;start handle count down
                  pusha
                  GETARINDEX curRowCursor,curColCursor
                  mov SI,BX
                  GETTIME
                  mov ax,si
                  mov cl,2D
                  mul cl
                  mov si,ax
                  dec BX
                  dec BX
                  dec BX
                  CMP word ptr timeState[si],BX
                  Jle  temp151
                  JMP  break80
                  temp151:
                  mov word ptr timeState[si],0D
                  popa
                ;end handle count down

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
                  Jne  PAWNTEMP
                  JMP  PAWN
                  PAWNTEMP:
                  CMP BYTE PTR gridState[BX],8d
                  Jne  ROOKTEMP
                  JMP  ROOK
                  ROOKTEMP:
                  CMP BYTE PTR gridState[BX],9d
                  Jne  KNIGHTTEMP
                  JMP  KNIGHT
                  KNIGHTTEMP:
                  CMP BYTE PTR gridState[BX],10d
                  Jne  BISHOPTEMP
                  JMP  BISHOP
                  BISHOPTEMP:
                  CMP BYTE PTR gridState[BX],11d
                  Jne  QUEENTEMP
                  JMP  QUEEN
                  QUEENTEMP:
                  CMP BYTE PTR gridState[BX],12d
                  Jne  KINGTEMP
                  JMP  KING
                  KINGTEMP:
                  JMP  NOACTION
                PAWN:
                  HANDLEPAWN curRowCursor,curColCursor
                  JMP  NOACTION
                ROOK:
                  HANDLEROOK curRowCursor,curColCursor
                  JMP  NOACTION
                KNIGHT:
                  HANDLEKNIGHT curRowCursor,curColCursor
                  JMP  NOACTION
                BISHOP:
                  HANDLEBISHOP curRowCursor,curColCursor
                  JMP  NOACTION
                QUEEN:
                  HANDLEQUEEN curRowCursor,curColCursor
                  JMP  NOACTION
                KING:
                  HANDLEKING curRowCursor,curColCursor
                  JMP  NOACTION
                NOACTION:
                PUSHA
                DRAW_AVAILABLE_PLACES
                POPA
                ;------------------------------
                  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                  outterr:
                  break80:
                  
ENDM FIRSTQHANDLE

;This Macro is FIRSTQHANDLE but modified and Responsible for handling some 
;corner cases when first player press his second Q to select a place he want to move the selected piece to
FIRSTQHANDLEM MACRO 
    LOCAL PAWNTEMP
    local ROOKTEMP
    local KNIGHTTEMP
    local BISHOPTEMP
    local QUEENTEMP
    local KINGTEMP
    local PAWN
    local ROOK
    local KNIGHT
    local BISHOP
    local QUEEN
    local KING
    local NOACTION
    local outterr
    local break80
    local temp151
    local temp150
    local temp100
    local skpp

                cmp stateOfQ,0
                Jne  skpp
                JMP  break80
                skpp:

                CLEAR_AVAILABLE_PLACES

                  GETARINDEX curRowCursor,curColCursor

                  pusha
                  ISEMPTY startRowCursor,startColCursor
                    cmp bx,1
                    Jne  temp100
                    JMP  break80
                    temp100:
                    popa

                  pusha
                  ISWHITEBYTE startRowCursor,startColCursor
                  cmp bx,0
                  Jne  temp150
                  JMP  break80
                  temp150:
                  popa
                 
                  mov cl,BYTE PTR startRowCursor
                  mov ch,BYTE PTR startColCursor
                  mov BYTE PTR startRowCursor,cl
                  mov BYTE PTR startColCursor,ch
                  pusha
                  UPDATECELL     curRowCursor,curColCursor,150D,0D
                  DRAWWITHSOURCE       borderdata,borderwidth,borderheight,curRowCursor,curColCursor,150D,0D 
                  popa
              

                ;------------------------
                  GETARINDEX startRowCursor,startColCursor
                  CMP BYTE PTR gridState[BX],7d
                  Jne  PAWNTEMP
                  JMP  PAWN
                  PAWNTEMP:
                  CMP BYTE PTR gridState[BX],8d
                  Jne  ROOKTEMP
                  JMP  ROOK
                  ROOKTEMP:
                  CMP BYTE PTR gridState[BX],9d
                  Jne  KNIGHTTEMP
                  JMP  KNIGHT
                  KNIGHTTEMP:
                  CMP BYTE PTR gridState[BX],10d
                  Jne  BISHOPTEMP
                  JMP  BISHOP
                  BISHOPTEMP:
                  CMP BYTE PTR gridState[BX],11d
                  Jne  QUEENTEMP
                  JMP  QUEEN
                  QUEENTEMP:
                  CMP BYTE PTR gridState[BX],12d
                  Jne  KINGTEMP
                  JMP  KING
                  KINGTEMP:
                  JMP  NOACTION
                PAWN:
                  HANDLEPAWN startRowCursor,startColCursor
                  JMP  NOACTION
                ROOK:
                  HANDLEROOK startRowCursor,startColCursor
                  JMP  NOACTION
                KNIGHT:
                  HANDLEKNIGHT startRowCursor,startColCursor
                  JMP  NOACTION
                BISHOP:
                  HANDLEBISHOP startRowCursor,startColCursor
                  JMP  NOACTION
                QUEEN:
                  HANDLEQUEEN startRowCursor,startColCursor
                  JMP  NOACTION
                KING:
                  HANDLEKING startRowCursor,startColCursor
                  JMP  NOACTION
                NOACTION:
                PUSHA
                DRAW_AVAILABLE_PLACES
                POPA
                ;------------------------------
                  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                  outterr:
                  break80:
                  
ENDM FIRSTQHANDLEM

FIRSTQHANDLE2 MACRO ;This Macro is Responsible for handling when second player press his first ENTER to select a piece want to move
    LOCAL PAWNTEMP
    local ROOKTEMP
    local KNIGHTTEMP
    local BISHOPTEMP
    local QUEENTEMP
    local KINGTEMP
    local PAWN
    local ROOK
    local KNIGHT
    local BISHOP
    local QUEEN
    local KING
    local NOACTION
    local outterr
    local break80
    local temp151
    local temp150
    local temp100

                  GETARINDEX curRowCursor2,curColCursor2

                  pusha
                  ISEMPTY curRowCursor2,curColCursor2
                    cmp bx,1
                    Jne  temp100
                    JMP  break80
                    temp100:
                    popa

                  pusha
                  ISWHITEBYTE curRowCursor2,curColCursor2
                  cmp bx,1
                  Jne  temp150
                  JMP  break80
                  temp150:
                  popa

                ;start handle count down
                  pusha
                  GETARINDEX curRowCursor2,curColCursor2
                  mov SI,BX
                  GETTIME
                  mov ax,si
                  mov cl,2D
                  mul cl
                  mov si,ax
                  dec BX
                  dec BX
                  dec BX
                  CMP word ptr timeState[si],BX
                  Jle  temp151
                  JMP  break80
                  temp151:
                  mov word ptr timeState[si],0D
                  popa
                ;end handle count down

                  MOV cl,BYTE PTR colorState[bx] 
                  mov BYTE PTR cellColorState2,cl
                  mov cl,BYTE PTR curRowCursor2
                  mov ch,BYTE PTR curColCursor2
                  mov BYTE PTR startRowCursor2,cl
                  mov BYTE PTR startColCursor2,ch
                  MOV BYTE PTR colorState[bx],0CH 
                  pusha
                  UPDATECELL     curRowCursor2,curColCursor2,150D,0D
                  DRAWWITHSOURCE       border2data,borderwidth,borderheight,curRowCursor2,curColCursor2,150D,0D 
                  popa
                  mov bl,BYTE PTR stateOfQ2

                  inc bl
                  mov BYTE PTR stateOfQ2,bl

                ;------------------------
                  GETARINDEX curRowCursor2,curColCursor2
                  CMP BYTE PTR gridState[BX],6d
                  Jne  PAWNTEMP
                  JMP  PAWN
                  PAWNTEMP:
                  CMP BYTE PTR gridState[BX],1d
                  Jne  ROOKTEMP
                  JMP  ROOK
                  ROOKTEMP:
                  CMP BYTE PTR gridState[BX],2d
                  Jne  KNIGHTTEMP
                  JMP  KNIGHT
                  KNIGHTTEMP:
                  CMP BYTE PTR gridState[BX],3d
                  Jne  BISHOPTEMP
                  JMP  BISHOP
                  BISHOPTEMP:
                  CMP BYTE PTR gridState[BX],4d
                  Jne  QUEENTEMP
                  JMP  QUEEN
                  QUEENTEMP:
                  CMP BYTE PTR gridState[BX],5d
                  Jne  KINGTEMP
                  JMP  KING
                  KINGTEMP:
                  JMP  NOACTION
                PAWN:
                  HANDLEPAWN2 curRowCursor2,curColCursor2
                  JMP  NOACTION
                ROOK:
                  HANDLEROOK2 curRowCursor2,curColCursor2
                  JMP  NOACTION
                KNIGHT:
                  HANDLEKNIGHT2 curRowCursor2,curColCursor2
                  JMP  NOACTION
                BISHOP:
                  HANDLEBISHOP2 curRowCursor2,curColCursor2
                  JMP  NOACTION
                QUEEN:
                  HANDLEQUEEN2 curRowCursor2,curColCursor2
                  JMP  NOACTION
                KING:
                  HANDLEKING2 curRowCursor2,curColCursor2
                  JMP  NOACTION
                NOACTION:
                PUSHA
                DRAW_AVAILABLE_PLACES2
                POPA
                ;------------------------------
                  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                  outterr:
                  break80:
                  
ENDM FIRSTQHANDLE2

checkqhandle MACRO 
    LOCAL PAWNTEMP
    local ROOKTEMP
    local KNIGHTTEMP
    local BISHOPTEMP
    local QUEENTEMP
    local KINGTEMP
    local PAWN
    local ROOK
    local KNIGHT
    local BISHOP
    local QUEEN
    local KING
    local NOACTION
    local outterr
    local break80
    local temp151
    local temp150
    local temp100

                  GETARINDEX curRowCursor,curColCursor

                  pusha
                  ISEMPTY curRowCursor,curColCursor
                    cmp bx,1
                    Jne  temp100
                    JMP  break80
                    temp100:
                    popa

                  pusha
                  ISWHITEBYTE curRowCursor,curColCursor
                  cmp bx,0
                  Jne  temp150
                  JMP  break80
                  temp150:
                  popa
                 
                  mov cl,BYTE PTR curRowCursor
                  mov ch,BYTE PTR curColCursor
                  mov BYTE PTR curRowCursor,cl
                  mov BYTE PTR curColCursor,ch

                ;------------------------
                  GETARINDEX curRowCursor,curColCursor
                  CMP BYTE PTR gridState[BX],7d
                  Jne  PAWNTEMP
                  JMP  PAWN
                  PAWNTEMP:
                  CMP BYTE PTR gridState[BX],8d
                  Jne  ROOKTEMP
                  JMP  ROOK
                  ROOKTEMP:
                  CMP BYTE PTR gridState[BX],9d
                  Jne  KNIGHTTEMP
                  JMP  KNIGHT
                  KNIGHTTEMP:
                  CMP BYTE PTR gridState[BX],10d
                  Jne  BISHOPTEMP
                  JMP  BISHOP
                  BISHOPTEMP:
                  CMP BYTE PTR gridState[BX],11d
                  Jne  QUEENTEMP
                  JMP  QUEEN
                  QUEENTEMP:
                  CMP BYTE PTR gridState[BX],12d
                  Jne  KINGTEMP
                  JMP  KING
                  KINGTEMP:
                  JMP  NOACTION
                PAWN:
                  HANDLEPAWN curRowCursor,curColCursor
                  JMP  NOACTION
                ROOK:
                  HANDLEROOK curRowCursor,curColCursor
                  JMP  NOACTION
                KNIGHT:
                  HANDLEKNIGHT curRowCursor,curColCursor
                  JMP  NOACTION
                BISHOP:
                  HANDLEBISHOP curRowCursor,curColCursor
                  JMP  NOACTION
                QUEEN:
                  HANDLEQUEEN curRowCursor,curColCursor
                  JMP  NOACTION
                KING:
                  HANDLEKING curRowCursor,curColCursor
                  JMP  NOACTION
                NOACTION:
                check_AVAILABLE_PLACES
                CLEAR_AVAILABLE_PLACES
                ;------------------------------
                  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                  outterr:
                  break80:
                  
ENDM checkqhandle

checkqhandle2 MACRO 
    LOCAL PAWNTEMP
    local ROOKTEMP
    local KNIGHTTEMP
    local BISHOPTEMP
    local QUEENTEMP
    local KINGTEMP
    local PAWN
    local ROOK
    local KNIGHT
    local BISHOP
    local QUEEN
    local KING
    local NOACTION
    local outterr
    local break80
    local temp151
    local temp150
    local temp100

                  GETARINDEX curRowCursor2,curColCursor2

                  pusha
                  ISEMPTY curRowCursor2,curColCursor2
                    cmp bx,1
                    Jne  temp100
                    popa
                    JMP  break80
                    temp100:
                    popa

                  pusha
                  ISWHITEBYTE curRowCursor2,curColCursor2
                  cmp bx,1
                  Jne  temp150
                  popa
                  JMP  break80
                  temp150:
                  popa
             
                ;------------------------
                  GETARINDEX curRowCursor2,curColCursor2
                  CMP BYTE PTR gridState[BX],6d
                  Jne  PAWNTEMP
                  JMP  PAWN
                  PAWNTEMP:
                  CMP BYTE PTR gridState[BX],1d
                  Jne  ROOKTEMP
                  JMP  ROOK
                  ROOKTEMP:
                  CMP BYTE PTR gridState[BX],2d
                  Jne  KNIGHTTEMP
                  JMP  KNIGHT
                  KNIGHTTEMP:
                  CMP BYTE PTR gridState[BX],3d
                  Jne  BISHOPTEMP
                  JMP  BISHOP
                  BISHOPTEMP:
                  CMP BYTE PTR gridState[BX],4d
                  Jne  QUEENTEMP
                  JMP  QUEEN
                  QUEENTEMP:
                  CMP BYTE PTR gridState[BX],5d
                  Jne  KINGTEMP
                  JMP  KING
                  KINGTEMP:
                  JMP  NOACTION


                PAWN:
                  HANDLEPAWN2 curRowCursor2,curColCursor2
                  JMP  NOACTION
                ROOK:
                  HANDLEROOK2 curRowCursor2,curColCursor2
                  JMP  NOACTION
                KNIGHT:
                  HANDLEKNIGHT2 curRowCursor2,curColCursor2
                  JMP  NOACTION
                BISHOP:
                  HANDLEBISHOP2 curRowCursor2,curColCursor2
                  JMP  NOACTION
                QUEEN:
                  HANDLEQUEEN2 curRowCursor2,curColCursor2
                  JMP  NOACTION
                KING:
                  HANDLEKING2 curRowCursor2,curColCursor2
                  JMP  NOACTION
                NOACTION:
                ;---------------------------------------------------
                check_AVAILABLE_PLACES2
                CLEAR_AVAILABLE_PLACES2
                ;------------------------------
                  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                  outterr:
                  break80:
                  
ENDM checkqhandle2

;This Macro is FIRSTQHANDLE2 but modified and Responsible for handling some 
;corner cases when second player press his second ENTER to select a place he want to move the selected piece tos
FIRSTQHANDLE2M MACRO 
    LOCAL PAWNTEMP
    local ROOKTEMP
    local KNIGHTTEMP
    local BISHOPTEMP
    local QUEENTEMP
    local KINGTEMP
    local PAWN
    local ROOK
    local KNIGHT
    local BISHOP
    local QUEEN
    local KING
    local NOACTION
    local outterr
    local break80
    local temp151
    local temp150
    local temp100
            local skpp

                cmp stateOfQ2,0
                jne skpp
                JMP far ptr break80
                skpp:

    CLEAR_AVAILABLE_PLACES2

                  GETARINDEX startRowCursor2,startColCursor2

                  pusha
                  ISEMPTY startRowCursor2,startColCursor2
                    cmp bx,1
                    jne temp100
                    popa
                    JMP far ptr break80
                    temp100:
                    popa

                  pusha
                  ISWHITEBYTE startRowCursor2,startColCursor2
                  cmp bx,1
                  jne temp150
                  popa
                  JMP far ptr break80
                  temp150:
                  popa
             
                ;------------------------
                  GETARINDEX startRowCursor2,startColCursor2
                  CMP BYTE PTR gridState[BX],6d
                  jne PAWNTEMP
                  JMP far ptr PAWN
                  PAWNTEMP:
                  CMP BYTE PTR gridState[BX],1d
                  jne ROOKTEMP
                  JMP far ptr ROOK
                  ROOKTEMP:
                  CMP BYTE PTR gridState[BX],2d
                  jne KNIGHTTEMP
                  JMP far ptr KNIGHT
                  KNIGHTTEMP:
                  CMP BYTE PTR gridState[BX],3d
                  jne BISHOPTEMP
                  JMP far ptr BISHOP
                  BISHOPTEMP:
                  CMP BYTE PTR gridState[BX],4d
                  jne QUEENTEMP
                  JMP far ptr QUEEN
                  QUEENTEMP:
                  CMP BYTE PTR gridState[BX],5d
                  jne KINGTEMP
                  JMP far ptr KING
                  KINGTEMP:
                  JMP far ptr NOACTION


                PAWN:
                  HANDLEPAWN2 startRowCursor2,startColCursor2
                  JMP far ptr NOACTION
                ROOK:
                  HANDLEROOK2 startRowCursor2,startColCursor2
                  JMP far ptr NOACTION
                KNIGHT:
                  HANDLEKNIGHT2 startRowCursor2,startColCursor2
                  JMP far ptr NOACTION
                BISHOP:
                  HANDLEBISHOP2 startRowCursor2,startColCursor2
                  JMP far ptr NOACTION
                QUEEN:
                  HANDLEQUEEN2 startRowCursor2,startColCursor2
                  JMP far ptr NOACTION
                KING:
                  HANDLEKING2 startRowCursor2,startColCursor2
                  JMP far ptr NOACTION
                NOACTION:
                ;---------------------------------------------------
                PUSHA
                DRAW_AVAILABLE_PLACES2
                POPA
                ;------------------------------
                  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                  outterr:
                  break80:
                  
ENDM FIRSTQHANDLE2M

HANDLEROOK MACRO x,y  ;This Macro is Responsible for handling White ROOK Movements Logic
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


mov dh,byte ptr x
mov dl,byte ptr y
mov ah,dh
mov al,dl
First_Loop:
inc ah
mov byte ptr dummyData1,ah
mov byte ptr dummyData2,al
push ax
INSIDEGRID dummyData1,dummyData2
pop ax 
cmp bx,0
jne temp30
JMP  break12
temp30:
push ax
ISEMPTY dummyData1,dummyData2
pop ax 
cmp bx,0
jne temp40
JMP  break1
temp40:
push ax
GETARINDEXBYBYTE  dummyData1,dummyData2
pop ax
mov byte ptr cursorState[bx],1
JMP  First_Loop
break1:
ISWHITEBYTE dummyData1,dummyData2
cmp bx,1
jne temp26
JMP  break12
temp26:
push ax
GETARINDEXBYBYTE  dummyData1,dummyData2
pop ax
mov byte ptr cursorState[bx],1
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
JMP  break22
temp31:
push ax
ISEMPTY dummyData1,dummyData2
pop ax 
cmp bx,0
jne temp32
JMP  break2
temp32:
push ax
GETARINDEXBYBYTE  dummyData1,dummyData2
pop ax
mov cursorState[bx],1
JMP  Second_Loop
break2:
ISWHITEBYTE dummyData1,dummyData2
cmp bx,1
jne temp27
JMP  break22
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
JMP  break32
temp33:
push ax
ISEMPTY dummyData1,dummyData2
pop ax 
cmp bx,0
jne temp34
JMP  break3
temp34:
push ax
GETARINDEXBYBYTE  dummyData1,dummyData2
pop ax
mov cursorState[bx],1
JMP  Third_Loop
break3:
ISWHITEBYTE dummyData1,dummyData2
cmp bx,1
jne temp28
JMP  break32
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
JMP  break42
temp35:
push ax
ISEMPTY dummyData1,dummyData2
pop ax 
cmp bx,0
jne temp36
JMP  break4
temp36:
push ax
GETARINDEXBYBYTE  dummyData1,dummyData2
pop ax
mov cursorState[bx],1
JMP  Fourth_Loop
break4:
ISWHITEBYTE dummyData1,dummyData2
cmp bx,1
jne temp29
JMP  break42
temp29:
push ax
GETARINDEXBYBYTE  dummyData1,dummyData2
pop ax
mov cursorState[bx],1
break42:

ENDM HANDLEROOK

HANDLEROOK2 MACRO x,y ;This Macro is Responsible for handling Black ROOK Movements Logic
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


mov dh,byte ptr x
mov dl,byte ptr y
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
JMP  break12
temp30:
push ax
ISEMPTY dummyData1,dummyData2
pop ax 
cmp bx,0
jne temp40
JMP  break1
temp40:
push ax
GETARINDEXBYBYTE  dummyData1,dummyData2
pop ax
mov cursorState2[bx],1
JMP  First_Loop
break1:
ISWHITEBYTE dummyData1,dummyData2
cmp bx,0
jne temp26
JMP  break12
temp26:
push ax
GETARINDEXBYBYTE  dummyData1,dummyData2
pop ax
mov cursorState2[bx],1
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
JMP  break22
temp31:
push ax
ISEMPTY dummyData1,dummyData2
pop ax 
cmp bx,0
jne temp32
JMP  break2
temp32:
push ax
GETARINDEXBYBYTE  dummyData1,dummyData2
pop ax
mov cursorState2[bx],1
JMP  Second_Loop
break2:
ISWHITEBYTE dummyData1,dummyData2
cmp bx,0
jne temp27
JMP  break22
temp27:
push ax
GETARINDEXBYBYTE  dummyData1,dummyData2
pop ax
mov cursorState2[bx],1
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
JMP  break32
temp33:
push ax
ISEMPTY dummyData1,dummyData2
pop ax 
cmp bx,0
jne temp34
JMP  break3
temp34:
push ax
GETARINDEXBYBYTE  dummyData1,dummyData2
pop ax
mov cursorState2[bx],1
JMP  Third_Loop
break3:
ISWHITEBYTE dummyData1,dummyData2
cmp bx,0
jne temp28
JMP  break32
temp28:
push ax
GETARINDEXBYBYTE  dummyData1,dummyData2
pop ax
mov cursorState2[bx],1
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
JMP  break42
temp35:
push ax
ISEMPTY dummyData1,dummyData2
pop ax 
cmp bx,0
jne temp36
JMP  break4
temp36:
push ax
GETARINDEXBYBYTE  dummyData1,dummyData2
pop ax
mov cursorState2[bx],1
JMP  Fourth_Loop
break4:
ISWHITEBYTE dummyData1,dummyData2
cmp bx,0
jne temp29
JMP  break42
temp29:
push ax
GETARINDEXBYBYTE  dummyData1,dummyData2
pop ax
mov cursorState2[bx],1
break42:

ENDM HANDLEROOK2

HANDLEBISHOP MACRO x,y ;This Macro is Responsible for handling White BISHOP Movements Logic
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


mov dh,byte ptr x
mov dl,byte ptr y
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
JMP  break12
temp30:
push ax
ISEMPTY dummyData1,dummyData2
pop ax 
cmp bx,0
jne temp40
JMP  break1
temp40:
push ax
GETARINDEXBYBYTE  dummyData1,dummyData2
pop ax
mov cursorState[bx],1
JMP  First_Loop
break1:
ISWHITEBYTE dummyData1,dummyData2
cmp bx,1
jne temp26
JMP  break12
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
JMP  break22
temp31:
push ax
ISEMPTY dummyData1,dummyData2
pop ax 
cmp bx,0
jne temp32
JMP  break2
temp32:
push ax
GETARINDEXBYBYTE  dummyData1,dummyData2
pop ax
mov cursorState[bx],1
JMP  Second_Loop
break2:
ISWHITEBYTE dummyData1,dummyData2
cmp bx,1
jne temp27
JMP  break22
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
JMP  break32
temp33:
push ax
ISEMPTY dummyData1,dummyData2
pop ax 
cmp bx,0
jne temp34
JMP  break3
temp34:
push ax
GETARINDEXBYBYTE  dummyData1,dummyData2
pop ax
mov cursorState[bx],1
JMP  Third_Loop
break3:
ISWHITEBYTE dummyData1,dummyData2
cmp bx,1
jne temp28
JMP  break32
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
JMP  break42
temp35:
push ax
ISEMPTY dummyData1,dummyData2
pop ax 
cmp bx,0
jne temp36
JMP  break4
temp36:
push ax
GETARINDEXBYBYTE  dummyData1,dummyData2
pop ax
mov cursorState[bx],1
JMP  Fourth_Loop
break4:
ISWHITEBYTE dummyData1,dummyData2
cmp bx,1
jne temp29
JMP  break42
temp29:
push ax
GETARINDEXBYBYTE  dummyData1,dummyData2
pop ax
mov cursorState[bx],1
break42:

ENDM HANDLEBISHOP

HANDLEBISHOP2 MACRO x,y ;This Macro is Responsible for handling Black BISHOP Movements Logic
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


mov dh,byte ptr x
mov dl,byte ptr y
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
JMP far ptr break12
temp30:
push ax
ISEMPTY dummyData1,dummyData2
pop ax 
cmp bx,0
jne temp40
JMP far ptr break1
temp40:
push ax
GETARINDEXBYBYTE  dummyData1,dummyData2
pop ax
mov cursorState2[bx],1
JMP far ptr First_Loop
break1:
ISWHITEBYTE dummyData1,dummyData2
cmp bx,0
jne temp26
JMP far ptr break12
temp26:
push ax
GETARINDEXBYBYTE  dummyData1,dummyData2
pop ax
mov cursorState2[bx],1
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
JMP far ptr break22
temp31:
push ax
ISEMPTY dummyData1,dummyData2
pop ax 
cmp bx,0
jne temp32
JMP far ptr break2
temp32:
push ax
GETARINDEXBYBYTE  dummyData1,dummyData2
pop ax
mov cursorState2[bx],1
JMP far ptr Second_Loop
break2:
ISWHITEBYTE dummyData1,dummyData2
cmp bx,0
jne temp27
JMP far ptr break22
temp27:
push ax
GETARINDEXBYBYTE  dummyData1,dummyData2
pop ax
mov cursorState2[bx],1
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
JMP far ptr break32
temp33:
push ax
ISEMPTY dummyData1,dummyData2
pop ax 
cmp bx,0
jne temp34
JMP far ptr break3
temp34:
push ax
GETARINDEXBYBYTE  dummyData1,dummyData2
pop ax
mov cursorState2[bx],1
JMP far ptr Third_Loop
break3:
ISWHITEBYTE dummyData1,dummyData2
cmp bx,0
jne temp28
JMP far ptr break32
temp28:
push ax
GETARINDEXBYBYTE  dummyData1,dummyData2
pop ax
mov cursorState2[bx],1
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
JMP far ptr break42
temp35:
push ax
ISEMPTY dummyData1,dummyData2
pop ax 
cmp bx,0
jne temp36
JMP far ptr break4
temp36:
push ax
GETARINDEXBYBYTE  dummyData1,dummyData2
pop ax
mov cursorState2[bx],1
JMP far ptr Fourth_Loop
break4:
ISWHITEBYTE dummyData1,dummyData2
cmp bx,0
jne temp29
JMP far ptr break42
temp29:
push ax
GETARINDEXBYBYTE  dummyData1,dummyData2
pop ax
mov cursorState2[bx],1
break42:

ENDM HANDLEBISHOP2

HANDLEQUEEN MACRO x,y ;This Macro is Responsible for handling White Queen Movements Logic
mov dh,byte ptr x
mov dl,byte ptr y
pusha
HANDLEROOK x,y
popa
mov byte ptr x,dh
mov byte ptr y,dl
pusha
HANDLEBISHOP x,y
popa
ENDM HANDLEQUEEN

HANDLEQUEEN2 MACRO x,y ;This Macro is Responsible for handling Black Queen Movements Logic
mov dh,byte ptr x
mov dl,byte ptr y
pusha
HANDLEROOK2 x,y
popa
mov byte ptr x,dh
mov byte ptr y,dl
pusha
HANDLEBISHOP2 x,y
popa
ENDM HANDLEQUEEN2

HANDLEKNIGHT MACRO X,Y ;This Macro is Responsible for handling White KNIGHT Movements Logic
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
                        je NOTVALID 

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
                        jne skp  ; JMP far ptr if white
                        popa

                        mov cursorState[bx],1
                        JMP far ptr NOTVALID
                        
                        skp:
                          POPA
                          mov cursorState[bx],0

                        NOTVALID:
                          CMP CX,0
                          je RETURN
                          DEC CX
                          JMP far ptr LOOP1
                        RETURN:      
ENDM HANDLEKNIGHT

HANDLEKNIGHT2 MACRO X,Y ;This Macro is Responsible for handling Black KNIGHT Movements Logic
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
                        je NOTVALID 

                        CHECK2:

                        VALID: 
                        MOV dummyData1 , AL
                        MOV dummyData2 ,  DL

                        push ax
                        GETARINDEXBYBYTE  dummyData1,dummyData2 ;out in bx
                        pop ax

                        pusha
                        ISWHITEBYTE2 dummyData1,dummyData2 ;out in bx
                        cmp bx,1 ;check if white
                        jne skp  ; JMP far ptr if black
                        popa

                        mov cursorState2[bx],1
                        JMP far ptr NOTVALID
                        
                        skp:
                          POPA
                          mov cursorState2[bx],0

                        NOTVALID:
                          CMP CX,0
                          je RETURN
                          DEC CX
                          JMP far ptr LOOP1
                        RETURN:      
ENDM HANDLEKNIGHT2

HANDLEPAWN MACRO X,Y ;This Macro is Responsible for handling White PAWN Movements Logic
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
LOCAL TWOMOVES
LOCAL SEEIFGREATERTAHN47OREQUAL
;;;;;;;;;;;;;

MOV AX,WORD PTR X
MOV CX,WORD PTR Y
DEC AX
INC CX
;;;;;;;;;;;;;;;;;;;;;;INSIDE GRID
INSIDEGRID AX,CX ;; 1 IF VALID AND 0 IF NOT
CMP BX,1
Jz ITSVALID1
MOV DL,0
JMP far ptr CONTINUOUECHECK
ITSVALID1:
;;;;;;;;;;;;;

MOV AX,WORD PTR X
MOV CX,WORD PTR Y
DEC AX
INC CX
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;TO EAT RIGHT
ISEMPTY AL,CL ;NOT EMPTY 1 IF NOT EMPETY
MOV DL,0
CMP BX,0
jnz CONTINUOUECHECK
;;;;;;;;;;;;;

MOV AX,WORD PTR X
MOV CX,WORD PTR Y
DEC AX
INC CX
;;;;;;;;;;;;;;
ISWHITE AX,CX
CMP BX,1
Jz  CONTINUOUECHECK
MOV DL,1 ; RIGHT HAS FOE
CONTINUOUECHECK:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;TO EAT LEFT
;;;;;;;;;;;;;

MOV AX,WORD PTR X
MOV CX,WORD PTR Y
DEC AX
DEC CX
;;;;;;;;;;;;;;;;;;;;;;INSIDE GRID
INSIDEGRID AX,CX ;; 1 IF VALID AND 0 IF NOT
CMP BX,1
Jz  ITSVALID2
MOV DH,0
JMP  CONTINUOUECHECK2
ITSVALID2:
;;;;;;;;;;;;;

MOV AX,WORD PTR X
MOV CX,WORD PTR Y
DEC AX
DEC CX
;;;;;;;;;;;;;
ISEMPTY AL,CL ;NOT EMPTY
MOV DH,0
CMP BX,0
jnz CONTINUOUECHECK2
;;;;;;;;;;;;;;;;;;

MOV AX,WORD PTR X
MOV CX,WORD PTR Y
DEC AX
DEC CX
;;;;;;;;;;;;;;;;;;
ISWHITE AX,CX
CMP BX,1
Jz  CONTINUOUECHECK2
MOV DH,1 ;LEFT HAS FOE
CONTINUOUECHECK2:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;RIGHT SIDE
CMP DL,1
jnz RIGHTNOFOE
;;;;;;;;;;;;;

MOV AX,WORD PTR X
MOV CX,WORD PTR Y
DEC AX
INC CX
GETARINDEX AX,CX  ;GET NEW INDEX
;;;;;;;;;;;;;
MOV BYTE PTR cursorState[BX],1
RIGHTNOFOE:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;LEFT SIDE
CMP DH,1
jnz LEFTNOFOE
;;;;;;;;;;;;;

MOV AX,WORD PTR X
MOV CX,WORD PTR Y
DEC AX
DEC CX
GETARINDEX AX,CX  ;GET NEW INDEX
;;;;;;;;;;;;;
MOV BYTE PTR cursorState[BX],1
LEFTNOFOE:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;MOV FOWARD
;;;;;;;;;;;;;;;;;

MOV AX,WORD PTR X
MOV CX,WORD PTR Y
DEC AX
;;;;;;;;;;;;;;;;;
INSIDEGRID AX,CX
CMP BX,1
Jz  ITSVALID3
JMP  CANTMOVE
ITSVALID3:
;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;

MOV AX,WORD PTR X
MOV CX,WORD PTR Y
DEC AX
;;;;;;;;;;;;;;;;;
ISWHITE AX,CX
CMP BX,1
jnz ITSVALID4
JMP  CANTMOVE
ITSVALID4:
;;;;;;;;;;;;;;;;;

MOV AX,WORD PTR X
MOV CX,WORD PTR Y
DEC AX
;;;;;;;;;;;;;;;;;
ISEMPTY AL,CL
CMP BX,1
Jz  CANMOVEFORWARD
;;ELSE
JMP  CANTMOVE
CANMOVEFORWARD:
MOV AX,WORD PTR X
MOV CX,WORD PTR Y
GETARINDEX AX,CX 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;CHECK FIRST ROW;;;;;;;;;;;;;;;
CMP BX,56
JC SEEIFGREATERTAHN47OREQUAL
JMP  ONEMOVEONLY

SEEIFGREATERTAHN47OREQUAL:
CMP BX,48
JNC TWOMOVES
JMP  ONEMOVEONLY

TWOMOVES:
;;;;;;;;;;;;;;;;;SEE IF TWO IS EMPTY
MOV AX,WORD PTR X
MOV CX,WORD PTR Y
DEC AX
DEC AX
ISEMPTY AL,CL
CMP BX,1
Jz  ITSVALID5
JMP  ONEMOVEONLY
ITSVALID5:
;;;;;;;;;;;;;;;;;;;;;;FRONT TO THE PAW BY TWO
MOV AX,WORD PTR X
MOV CX,WORD PTR Y
DEC AX
DEC AX
GETARINDEX AX,CX 
MOV BYTE PTR cursorState[BX],1
;;;;;;;;;;;;;;;;;;;;;;FRONT TO THE PAW BY ONE
ONEMOVEONLY:
MOV AX,WORD PTR X
MOV CX,WORD PTR Y
DEC AX
GETARINDEX AX,CX
MOV BYTE PTR cursorState[BX],1
CANTMOVE:


ENDM HANDLEPAWN

HANDLEPAWN2 MACRO X,Y ;This Macro is Responsible for handling Black PAWN Movements Logic
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
LOCAL TWOMOVES
LOCAL SEEIFGREATERTAHN47OREQUAL
;;;;;;;;;;;;;

MOV AX,WORD PTR X
MOV CX,WORD PTR Y
INC AX
INC CX
;;;;;;;;;;;;;;;;;;;;;;INSIDE GRID
INSIDEGRID AX,CX ;; 1 IF VALID AND 0 IF NOT
CMP BX,1
jz ITSVALID1
MOV DL,0
JMP far ptr CONTINUOUECHECK
ITSVALID1:
;;;;;;;;;;;;;

MOV AX,WORD PTR X
MOV CX,WORD PTR Y
INC AX
INC CX
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;TO EAT RIGHT
ISEMPTY AL,CL ;NOT EMPTY 1 IF NOT EMPETY
MOV DL,0
CMP BX,0
jnz CONTINUOUECHECK
;;;;;;;;;;;;;

MOV AX,WORD PTR X
MOV CX,WORD PTR Y
INC AX
INC CX
;;;;;;;;;;;;;;
ISWHITE2 AX,CX
CMP BX,0
jz CONTINUOUECHECK
MOV DL,1 ; RIGHT HAS FOE
CONTINUOUECHECK:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;TO EAT LEFT
;;;;;;;;;;;;;

MOV AX,WORD PTR X
MOV CX,WORD PTR Y
INC AX
DEC CX
;;;;;;;;;;;;;;;;;;;;;;INSIDE GRID
INSIDEGRID AX,CX ;; 1 IF VALID AND 0 IF NOT
CMP BX,1
jz ITSVALID2
MOV DH,0
JMP far ptr CONTINUOUECHECK2
ITSVALID2:
;;;;;;;;;;;;;

MOV AX,WORD PTR X
MOV CX,WORD PTR Y
INC AX
DEC CX
;;;;;;;;;;;;;
ISEMPTY AL,CL ;NOT EMPTY
MOV DH,0
CMP BX,0
jnz CONTINUOUECHECK2
;;;;;;;;;;;;;;;;;;

MOV AX,WORD PTR X
MOV CX,WORD PTR Y
INC AX
DEC CX
;;;;;;;;;;;;;;;;;;
ISWHITE2 AX,CX
CMP BX,0
jz CONTINUOUECHECK2
MOV DH,1 ;LEFT HAS FOE
CONTINUOUECHECK2:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;RIGHT SIDE
CMP DL,1
jnz RIGHTNOFOE
;;;;;;;;;;;;;

MOV AX,WORD PTR X
MOV CX,WORD PTR Y
INC AX
INC CX
GETARINDEX AX,CX  ;GET NEW INDEX
;;;;;;;;;;;;;
MOV BYTE PTR cursorState2[BX],1
RIGHTNOFOE:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;LEFT SIDE
CMP DH,1
jnz LEFTNOFOE
;;;;;;;;;;;;;

MOV AX,WORD PTR X
MOV CX,WORD PTR Y
INC AX
DEC CX
GETARINDEX AX,CX  ;GET NEW INDEX
;;;;;;;;;;;;;
MOV BYTE PTR cursorState2[BX],1
LEFTNOFOE:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;MOV FOWARD
;;;;;;;;;;;;;;;;;

MOV AX,WORD PTR X
MOV CX,WORD PTR Y
INC AX
;;;;;;;;;;;;;;;;;
INSIDEGRID AX,CX
CMP BX,1
jz ITSVALID3
JMP far ptr CANTMOVE
ITSVALID3:
;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;

MOV AX,WORD PTR X
MOV CX,WORD PTR Y
INC AX
;;;;;;;;;;;;;;;;;
ISWHITE2 AX,CX
CMP BX,0
jnz ITSVALID4
JMP far ptr CANTMOVE
ITSVALID4:
;;;;;;;;;;;;;;;;;

MOV AX,WORD PTR X
MOV CX,WORD PTR Y
INC AX
;;;;;;;;;;;;;;;;;
ISEMPTY AL,CL
CMP BX,1
jz CANMOVEFORWARD
;;ELSE
JMP far ptr CANTMOVE
CANMOVEFORWARD:
MOV AX,WORD PTR X
MOV CX,WORD PTR Y
GETARINDEX AX,CX 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;CHECK FIRST ROW;;;;;;;;;;;;;;;
CMP BX,16
JC SEEIFGREATERTAHN47OREQUAL
JMP far ptr ONEMOVEONLY

SEEIFGREATERTAHN47OREQUAL:
CMP BX,8
JNC TWOMOVES
JMP far ptr ONEMOVEONLY

TWOMOVES:
;;;;;;;;;;;;;;;;;SEE IF TWO IS EMPTY
MOV AX,WORD PTR X
MOV CX,WORD PTR Y
INC AX
INC AX
ISEMPTY AL,CL
CMP BX,1
jz ITSVALID5
JMP far ptr ONEMOVEONLY
ITSVALID5:
;;;;;;;;;;;;;;;;;;;;;;FRONT TO THE PAW BY TWO
MOV AX,WORD PTR X
MOV CX,WORD PTR Y
INC AX
INC AX
GETARINDEX AX,CX 
MOV BYTE PTR cursorState2[BX],1
;;;;;;;;;;;;;;;;;;;;;;FRONT TO THE PAW BY ONE
ONEMOVEONLY:
MOV AX,WORD PTR X
MOV CX,WORD PTR Y
INC AX
GETARINDEX AX,CX
MOV BYTE PTR cursorState2[BX],1
CANTMOVE:


ENDM HANDLEPAWN2

HANDLEKING MACRO X,Y ;This Macro is Responsible for handling White KING Movements Logic
LOCAL CANMOVE1
LOCAL CHKIFWHITE1
LOCAL CANTMOVE1
LOCAL CANMOVE2
LOCAL CHKIFWHITE2
LOCAL CANTMOVE2
LOCAL CANMOVE3
LOCAL CHKIFWHITE3
LOCAL CANTMOVE3
LOCAL CANMOVE4
LOCAL CHKIFWHITE4
LOCAL CANTMOVE4
LOCAL CANMOVE5
LOCAL CHKIFWHITE5
LOCAL CANTMOVE5
LOCAL CANMOVE6
LOCAL CHKIFWHITE6
LOCAL CANTMOVE6
LOCAL CANMOVE7
LOCAL CHKIFWHITE7
LOCAL CANTMOVE7
LOCAL CANMOVE8
LOCAL CHKIFWHITE8
LOCAL CANTMOVE8
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
MOV AX,WORD PTR X
MOV CX,WORD PTR Y
DEC AX;;;;;;;;;;;;;;;FRONT STEP
INSIDEGRID AX,CX ; WHITE 1 IF NOT WHITE
CMP BX,0
Jz far ptr CANTMOVE1
;;;;;;;;;;;;;
MOV AX,X
MOV CX,Y
DEC AX;;;;;;;;;;;;;;;FRONT STEP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;TO EAT FRONT 
ISEMPTY AL,CL ; EMPTY 1 IF NOT EMPETY 0
CMP BX,1
jnz CHKIFWHITE1
JMP far ptr CANMOVE1
;;;;;;;;;;;;;;;;
CHKIFWHITE1:
MOV AX,X
MOV CX,Y
DEC AX;;;;;;;;;;;;;;;FRONT STEP
ISWHITE AX,CX ; WHITE 1 IF NOT WHITE
CMP BX,1
Jz  CANTMOVE1
;;;;;;;;;;;;;;;
CANMOVE1:
MOV AX,X
MOV CX,Y
DEC AX;;;;;;;;;;;;;;;FRONTRIGHT STEP
GETARINDEX AX,CX
MOV cursorState[BX],1
CANTMOVE1:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
MOV AX,X
MOV CX,Y
DEC AX;;;;;;;;;;;;;;;FRONT STEP
INC CX
INSIDEGRID AX,CX ; WHITE 1 IF NOT WHITE
CMP BX,0
Jz far ptr CANTMOVE2
;;;;;;;;;;;;;
MOV AX,X
MOV CX,Y
DEC AX;;;;;;;;;;;;;;;FRONTRIGHT STEP
INC CX
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;TO EAT FRONT RIGHT
ISEMPTY AL,CL ; EMPTY 1 IF NOT EMPETY 0
CMP BX,1
jnz CHKIFWHITE2
JMP far ptr CANMOVE2
;;;;;;;;;;;;;;;;
CHKIFWHITE2:
MOV AX,X
MOV CX,Y
DEC AX;;;;;;;;;;;;;;;FRONTRIGHT STEP
INC CX
ISWHITE AX,CX ; WHITE 1 IF NOT WHITE
CMP BX,1
Jz  CANTMOVE2
;;;;;;;;;;;;;;;
CANMOVE2:
MOV AX,X
MOV CX,Y
DEC AX;;;;;;;;;;;;;;;FRONTRIGHT STEP
INC CX
GETARINDEX AX,CX
MOV cursorState[BX],1
CANTMOVE2:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
MOV AX,X
MOV CX,Y
DEC AX;;;;;;;;;;;;;;;FRONT STEP
DEC CX
INSIDEGRID AX,CX ; WHITE 1 IF NOT WHITE
CMP BX,0
Jz far ptr CANTMOVE3
;;;;;;;;;;;;;
MOV AX,X
MOV CX,Y
DEC AX;;;;;;;;;;;;;;;FRONTLEFT STEP
DEC CX
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;TO EAT FRONT LEFT
ISEMPTY AL,CL ; EMPTY 1 IF NOT EMPETY 0
CMP BX,1
jnz CHKIFWHITE3
JMP far ptr CANMOVE3
;;;;;;;;;;;;;;;;
CHKIFWHITE3:
MOV AX,X
MOV CX,Y
DEC AX;;;;;;;;;;;;;;;FRONTLEFT STEP
DEC CX
ISWHITE AX,CX ; WHITE 1 IF NOT WHITE
CMP BX,1
Jz  CANTMOVE3
;;;;;;;;;;;;;;;
CANMOVE3:
MOV AX,X
MOV CX,Y
DEC AX;;;;;;;;;;;;;;;FRONTLEFT STEP
DEC CX
GETARINDEX AX,CX
MOV cursorState[BX],1
CANTMOVE3:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
MOV AX,X
MOV CX,Y
INC CX
INSIDEGRID AX,CX ; WHITE 1 IF NOT WHITE
CMP BX,0
Jz far ptr CANTMOVE4
;;;;;;;;;;;;;
MOV AX,X
MOV CX,Y
INC CX
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;TO EAT FRONT LEFT
ISEMPTY AL,CL ; EMPTY 1 IF NOT EMPETY 0
CMP BX,1
jnz CHKIFWHITE4
JMP far ptr CANMOVE4
;;;;;;;;;;;;;;;;
CHKIFWHITE4:
MOV AX,X
MOV CX,Y
INC CX
ISWHITE AX,CX ; WHITE 1 IF NOT WHITE
CMP BX,1
Jz CANTMOVE4
;;;;;;;;;;;;;;;
CANMOVE4:
MOV AX,X
MOV CX,Y
INC CX
GETARINDEX AX,CX
MOV cursorState[BX],1
CANTMOVE4:



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
MOV AX,X
MOV CX,Y
INC AX;;;;;;;;;;;;;;;FRONT STEP
INC CX
INSIDEGRID AX,CX ; WHITE 1 IF NOT WHITE
CMP BX,0
Jz far ptr CANTMOVE5
;;;;;;;;;;;;;
MOV AX,X
MOV CX,Y
INC AX;;;;;;;;;;;;;;;FRONT STEP
INC CX
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;TO EAT FRONT 
ISEMPTY AL,CL ; EMPTY 1 IF NOT EMPETY 0
CMP BX,1
jnz CHKIFWHITE5
JMP far ptr CANMOVE5
;;;;;;;;;;;;;;;;
CHKIFWHITE5:
MOV AX,X
MOV CX,Y
INC AX;;;;;;;;;;;;;;;FRONT STEP
INC CX
ISWHITE AX,CX ; WHITE 1 IF NOT WHITE
CMP BX,1
Jz  CANTMOVE5
;;;;;;;;;;;;;;;
CANMOVE5:
MOV AX,X
MOV CX,Y
INC AX;;;;;;;;;;;;;;;FRONT STEP
INC CX
GETARINDEX AX,CX
MOV cursorState[BX],1
CANTMOVE5:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
MOV AX,X
MOV CX,Y
INC AX;;;;;;;;;;;;;;;FRONT STEP
INSIDEGRID AX,CX ; WHITE 1 IF NOT WHITE
CMP BX,0
Jz far ptr CANTMOVE6
;;;;;;;;;;;;;
MOV AX,X
MOV CX,Y
INC AX
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;TO EAT FRONT RIGHT
ISEMPTY AL,CL ; EMPTY 1 IF NOT EMPETY 0
CMP BX,1
jnz CHKIFWHITE6
JMP far ptr CANMOVE6
;;;;;;;;;;;;;;;;
CHKIFWHITE6:
MOV AX,X
MOV CX,Y
INC AX
ISWHITE AX,CX ; WHITE 1 IF NOT WHITE
CMP BX,1
Jz  CANTMOVE6
;;;;;;;;;;;;;;;
CANMOVE6:
MOV AX,X
MOV CX,Y
INC AX
GETARINDEX AX,CX
MOV cursorState[BX],1
CANTMOVE6:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
MOV AX,X
MOV CX,Y
INC AX;;;;;;;;;;;;;;;FRONT STEP
DEC CX
INSIDEGRID AX,CX ; WHITE 1 IF NOT WHITE
CMP BX,0
Jz far ptr CANTMOVE7
;;;;;;;;;;;;;
MOV AX,X
MOV CX,Y
INC AX;;;;;;;;;;;;;;;FRONT STEP
DEC CX
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;TO EAT FRONT LEFT
ISEMPTY AL,CL ; EMPTY 1 IF NOT EMPETY 0
CMP BX,1
jnz CHKIFWHITE7
JMP far ptr CANMOVE7
;;;;;;;;;;;;;;;;
CHKIFWHITE7:
MOV AX,X
MOV CX,Y
INC AX;;;;;;;;;;;;;;;FRONT STEP
DEC CX
ISWHITE AX,CX ; WHITE 1 IF NOT WHITE
CMP BX,1
Jz CANTMOVE7
;;;;;;;;;;;;;;;
CANMOVE7:
MOV AX,X
MOV CX,Y
INC AX;;;;;;;;;;;;;;;FRONT STEP
DEC CX
GETARINDEX AX,CX
MOV cursorState[BX],1
CANTMOVE7:


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
MOV AX,X
MOV CX,Y
DEC CX
INSIDEGRID AX,CX ; WHITE 1 IF NOT WHITE
CMP BX,0
Jz far ptr CANTMOVE8
;;;;;;;;;;;;;
MOV AX,X
MOV CX,Y
DEC CX
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;TO EAT FRONT LEFT
ISEMPTY AL,CL ; EMPTY 1 IF NOT EMPETY 0
CMP BX,1
jnz CHKIFWHITE8
JMP far ptr CANMOVE8
;;;;;;;;;;;;;;;;
CHKIFWHITE8:
MOV AX,X
MOV CX,Y
DEC CX
ISWHITE AX,CX ; WHITE 1 IF NOT WHITE
CMP BX,1
Jz  CANTMOVE8
;;;;;;;;;;;;;;;
CANMOVE8:
MOV AX,X
MOV CX,Y
DEC CX
GETARINDEX AX,CX
MOV cursorState[BX],1
CANTMOVE8:

ENDM HANDLEKING

HANDLEKING2 MACRO X,Y ;This Macro is Responsible for handling Black KING Movements Logic
LOCAL CANMOVE1
LOCAL CHKIFWHITE1
LOCAL CANTMOVE1
LOCAL CANMOVE2
LOCAL CHKIFWHITE2
LOCAL CANTMOVE2
LOCAL CANMOVE3
LOCAL CHKIFWHITE3
LOCAL CANTMOVE3
LOCAL CANMOVE4
LOCAL CHKIFWHITE4
LOCAL CANTMOVE4
LOCAL CANMOVE5
LOCAL CHKIFWHITE5
LOCAL CANTMOVE5
LOCAL CANMOVE6
LOCAL CHKIFWHITE6
LOCAL CANTMOVE6
LOCAL CANMOVE7
LOCAL CHKIFWHITE7
LOCAL CANTMOVE7
LOCAL CANMOVE8
LOCAL CHKIFWHITE8
LOCAL CANTMOVE8
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
MOV AX,WORD PTR X
MOV CX,WORD PTR Y
DEC AX;;;;;;;;;;;;;;;FRONT STEP
INSIDEGRID AX,CX ; WHITE 1 IF NOT WHITE
CMP BX,0
Jz far ptr CANTMOVE1
;;;;;;;;;;;;;
MOV AX,WORD PTR X
MOV CX,WORD PTR Y
DEC AX;;;;;;;;;;;;;;;FRONT STEP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;TO EAT FRONT 
ISEMPTY AL,CL ; EMPTY 1 IF NOT EMPETY 0
CMP BX,1
jnz CHKIFWHITE1
JMP far ptr CANMOVE1
;;;;;;;;;;;;;;;;
CHKIFWHITE1:
MOV AX,WORD PTR X
MOV CX,WORD PTR Y
DEC AX;;;;;;;;;;;;;;;FRONT STEP
ISWHITE2 AX,CX ; WHITE 1 IF NOT WHITE
CMP BX,0
Jz  CANTMOVE1
;;;;;;;;;;;;;;;
CANMOVE1:
MOV AX,X
MOV CX,Y
DEC AX;;;;;;;;;;;;;;;FRONTRIGHT STEP
GETARINDEX AX,CX
MOV BYTE PTR cursorState2[BX],1
CANTMOVE1:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
MOV AX,WORD PTR X
MOV CX,WORD PTR Y
DEC AX;;;;;;;;;;;;;;;FRONT STEP
INC CX
INSIDEGRID AX,CX ; WHITE 1 IF NOT WHITE
CMP BX,0
Jz far ptr CANTMOVE2
;;;;;;;;;;;;;
MOV AX,WORD PTR X
MOV CX,WORD PTR Y
DEC AX;;;;;;;;;;;;;;;FRONTRIGHT STEP
INC CX
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;TO EAT FRONT RIGHT
ISEMPTY AL,CL ; EMPTY 1 IF NOT EMPETY 0
CMP BX,1
jnz CHKIFWHITE2
JMP far ptr CANMOVE2
;;;;;;;;;;;;;;;;
CHKIFWHITE2:
MOV AX,WORD PTR X
MOV CX,WORD PTR Y
DEC AX;;;;;;;;;;;;;;;FRONTRIGHT STEP
INC CX
ISWHITE2 AX,CX ; WHITE 1 IF NOT WHITE
CMP BX,0
Jz  CANTMOVE2
;;;;;;;;;;;;;;;
CANMOVE2:
MOV AX,WORD PTR X
MOV CX,WORD PTR Y
DEC AX;;;;;;;;;;;;;;;FRONTRIGHT STEP
INC CX
GETARINDEX AX,CX
MOV BYTE PTR cursorState2[BX],1
CANTMOVE2:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
MOV AX,X
MOV CX,Y
DEC AX;;;;;;;;;;;;;;;FRONT STEP
DEC CX
INSIDEGRID AX,CX ; WHITE 1 IF NOT WHITE
CMP BX,0
Jz far ptr CANTMOVE3
;;;;;;;;;;;;;
MOV AX,X
MOV CX,Y
DEC AX;;;;;;;;;;;;;;;FRONTLEFT STEP
DEC CX
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;TO EAT FRONT LEFT
ISEMPTY AL,CL ; EMPTY 1 IF NOT EMPETY 0
CMP BX,1
jnz CHKIFWHITE3
JMP far ptr CANMOVE3
;;;;;;;;;;;;;;;;
CHKIFWHITE3:
MOV AX,X
MOV CX,Y
DEC AX;;;;;;;;;;;;;;;FRONTLEFT STEP
DEC CX
ISWHITE2 AX,CX ; WHITE 1 IF NOT WHITE
CMP BX,0
Jz  CANTMOVE3
;;;;;;;;;;;;;;;
CANMOVE3:
MOV AX,X
MOV CX,Y
DEC AX;;;;;;;;;;;;;;;FRONTLEFT STEP
DEC CX
GETARINDEX AX,CX
MOV cursorState2[BX],1
CANTMOVE3:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
MOV AX,X
MOV CX,Y
INC CX
INSIDEGRID AX,CX ; WHITE 1 IF NOT WHITE
CMP BX,0
Jz far ptr CANTMOVE4
;;;;;;;;;;;;;
MOV AX,X
MOV CX,Y
INC CX
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;TO EAT FRONT LEFT
ISEMPTY AL,CL ; EMPTY 1 IF NOT EMPETY 0
CMP BX,1
jnz CHKIFWHITE4
JMP far ptr CANMOVE4
;;;;;;;;;;;;;;;;
CHKIFWHITE4:
MOV AX,X
MOV CX,Y
INC CX
ISWHITE2 AX,CX ; WHITE 1 IF NOT WHITE
CMP BX,0
Jz  CANTMOVE4
;;;;;;;;;;;;;;;
CANMOVE4:
MOV AX,X
MOV CX,Y
INC CX
GETARINDEX AX,CX
MOV cursorState2[BX],1
CANTMOVE4:



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
MOV AX,X
MOV CX,Y
INC AX;;;;;;;;;;;;;;;FRONT STEP
INC CX
INSIDEGRID AX,CX ; WHITE 1 IF NOT WHITE
CMP BX,0
Jz far ptr CANTMOVE5
;;;;;;;;;;;;;
MOV AX,X
MOV CX,Y
INC AX;;;;;;;;;;;;;;;FRONT STEP
INC CX
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;TO EAT FRONT 
ISEMPTY AL,CL ; EMPTY 1 IF NOT EMPETY 0
CMP BX,1
jnz CHKIFWHITE5
JMP far ptr CANMOVE5
;;;;;;;;;;;;;;;;
CHKIFWHITE5:
MOV AX,X
MOV CX,Y
INC AX;;;;;;;;;;;;;;;FRONT STEP
INC CX
ISWHITE2 AX,CX ; WHITE 1 IF NOT WHITE
CMP BX,0
Jz far ptr CANTMOVE5
;;;;;;;;;;;;;;;
CANMOVE5:
MOV AX,X
MOV CX,Y
INC AX;;;;;;;;;;;;;;;FRONT STEP
INC CX
GETARINDEX AX,CX
MOV cursorState2[BX],1
CANTMOVE5:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
MOV AX,X
MOV CX,Y
INC AX;;;;;;;;;;;;;;;FRONT STEP
INSIDEGRID AX,CX ; WHITE 1 IF NOT WHITE
CMP BX,0
Jz far ptr CANTMOVE6
;;;;;;;;;;;;;
MOV AX,X
MOV CX,Y
INC AX
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;TO EAT FRONT RIGHT
ISEMPTY AL,CL ; EMPTY 1 IF NOT EMPETY 0
CMP BX,1
jnz CHKIFWHITE6
JMP far ptr CANMOVE6
;;;;;;;;;;;;;;;;
CHKIFWHITE6:
MOV AX,X
MOV CX,Y
INC AX
ISWHITE2 AX,CX ; WHITE 1 IF NOT WHITE
CMP BX,0
Jz CANTMOVE6
;;;;;;;;;;;;;;;
CANMOVE6:
MOV AX,X
MOV CX,Y
INC AX
GETARINDEX AX,CX
MOV cursorState2[BX],1
CANTMOVE6:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
MOV AX,X
MOV CX,Y
INC AX;;;;;;;;;;;;;;;FRONT STEP
DEC CX
INSIDEGRID AX,CX ; WHITE 1 IF NOT WHITE
CMP BX,0
Jz far ptr CANTMOVE7
;;;;;;;;;;;;;
MOV AX,X
MOV CX,Y
INC AX;;;;;;;;;;;;;;;FRONT STEP
DEC CX
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;TO EAT FRONT LEFT
ISEMPTY AL,CL ; EMPTY 1 IF NOT EMPETY 0
CMP BX,1
jnz CHKIFWHITE7
JMP far ptr CANMOVE7
;;;;;;;;;;;;;;;;
CHKIFWHITE7:
MOV AX,X
MOV CX,Y
INC AX;;;;;;;;;;;;;;;FRONT STEP
DEC CX
ISWHITE2 AX,CX ; WHITE 1 IF NOT WHITE
CMP BX,0
Jz  CANTMOVE7
;;;;;;;;;;;;;;;
CANMOVE7:
MOV AX,X
MOV CX,Y
INC AX;;;;;;;;;;;;;;;FRONT STEP
DEC CX
GETARINDEX AX,CX
MOV cursorState2[BX],1
CANTMOVE7:


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
MOV AX,X
MOV CX,Y
DEC CX
INSIDEGRID AX,CX ; WHITE 1 IF NOT WHITE
CMP BX,0
Jz far ptr CANTMOVE8
;;;;;;;;;;;;;
MOV AX,X
MOV CX,Y
DEC CX
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;TO EAT FRONT LEFT
ISEMPTY AL,CL ; EMPTY 1 IF NOT EMPETY 0
CMP BX,1
jnz CHKIFWHITE8
JMP  CANMOVE8
;;;;;;;;;;;;;;;;
CHKIFWHITE8:
MOV AX,X
MOV CX,Y
DEC CX
ISWHITE2 AX,CX ; WHITE 1 IF NOT WHITE
CMP BX,0
Jz  CANTMOVE8
;;;;;;;;;;;;;;;
CANMOVE8:
MOV AX,X
MOV CX,Y
DEC CX
GETARINDEX AX,CX
MOV cursorState2[BX],1
CANTMOVE8:

ENDM HANDLEKING2

GETARINDEXBYBYTE MACRO X,Y ;This Macro is another version from GETARINDEX but by byte
    MOV AL,X 
    MOV BL , 8D
    MUL BL 
    ADD AL , Y
    MOV BL,AL
    MOV BH,0
ENDM GETARINDEXBYBYTE

SECONDQHANDLE MACRO ;This Macro is Responsible for handling when first player press his second Q to move a selected piece to another cell
            LOCAL SKIP
            local blabla
            local blabla2
            local blabla3
            local er
            pusha
        movecursor 2d,31d
        ShowMessage clear
        popa

                  GETARINDEX startRowCursor,startColCursor

                  MOV cl,BYTE PTR cellColorState
                  mov BYTE PTR colorState[bx],cl
                  mov cl,BYTE PTR curRowCursor
                  mov ch,BYTE PTR curColCursor
                  mov BYTE PTR endRowCursor,cl
                  mov BYTE PTR endColCursor,ch
                  mov cx,bx ;START INDEX

                  GETARINDEX endRowCursor,endColCursor
                  
                  CMP BYTE PTR cursorState[BX],0
                  jne blabla
                  JMP far ptr SKIP
                blabla:

                cmp BYTE PTR gridState[bx],0
                jne blabla2
                  JMP far ptr blabla3
                blabla2:
              ;---------------------------
              Pusha
                getDrawPosition 30d,0d,whiterow,whitecol
                DRAWCELL        cx,dx,0fh
                GETIMGDATA      endRowCursor,endColCursor
                DRAWWITHSOURCE  [bx],60D,60D,whiterow,whitecol,30D,0D
                mov             al,BYTE PTR whiterow
                inc             al
                mov            BYTE PTR  whiterow,al
                cmp al,8d
                jne er
                mov al,0d
                mov ah,1h
                mov            BYTE PTR  whiterow,al
                mov            BYTE PTR  whitecol,ah
                er:
              popa
              ;-----------------------------
            blabla3:

                  PUSHA
                  mov si,cx ;START INDEX
                  mov dh,BYTE PTR gridState[si] ;DATA OF FIRST INDEX

                  mov gridState[si],0 ;CLEAR START
                  mov si,bx ;END INDEX
                  mov gridState[si],dh ; MOVE START TO END

                  
                ;count down start
                  PUSHA
                  GETTIME
                  mov ax,si
                  mov cl,2D
                  mul cl
                  mov si,ax
                  mov word ptr timeState[si],BX
                  POPA
                ;count down end

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
                  
                  FIRSTQHANDLE2M
                  
                  checkqhandle
                  checkqhandle2
                  CHECKMATE
                  
ENDM SECONDQHANDLE

SECONDQHANDLE2 MACRO ;This Macro is Responsible for handling when second player press his second ENTER to move a selected piece to another cell
            LOCAL SKIP
            local blabla
            local blabla2
            local blabla3
            local er
      pusha
        movecursor 60d,31d
        ShowMessage clear
        popa

                  GETARINDEX startRowCursor2,startColCursor2

                  MOV cl,BYTE PTR cellColorState2
                  mov BYTE PTR colorState[bx],cl
                  mov cl,BYTE PTR curRowCursor2
                  mov ch,BYTE PTR curColCursor2
                  mov BYTE PTR endRowCursor2,cl
                  mov BYTE PTR endColCursor2,ch
                  mov cx,bx ;START INDEX

                  GETARINDEX endRowCursor2,endColCursor2

                  CMP cursorState2[BX],0
                  jne blabla
                  JMP far ptr SKIP
                blabla:

                cmp gridState[bx],0
                jne blabla2
                  JMP far ptr blabla3
                blabla2:
              ;---------------------------
              Pusha
                getDrawPosition 30d,0d,blackrow,blackcol
                DRAWCELL        cx,dx,0fh
                GETIMGDATA      endRowCursor2,endColCursor2
                DRAWWITHSOURCE  [bx],60D,60D,blackrow,blackcol,30D,0D
                mov             al,blackrow
                inc             al
                mov             blackrow,al
                cmp al,8d
                jne er
                mov al,0d
                mov ah,11D
                mov             blackrow,al
                mov             blackcol,ah
                er:
              popa
              ;-----------------------------
            blabla3:

                  PUSHA
                  mov si,cx ;START INDEX
                  mov dh,BYTE PTR gridState[si] ;DATA OF FIRST INDEX
                  mov gridState[si],0 ;CLEAR START
                  mov si,bx ;END INDEX
                  mov gridState[si],dh ; MOVE START TO END

                  
                ;count down start
                  PUSHA
                  GETTIME
                  mov ax,si
                  mov cl,2D
                  mul cl
                  mov si,ax
                  mov word ptr timeState[si],BX
                  POPA
                ;count down end

                  POPA
                SKIP:
                  pusha
                  UPDATECELL     startRowCursor2,startColCursor2,150D,0D
                  popa

                  pusha
                  UPDATECELL     endRowCursor2,endColCursor2,150D,0D
                  popa

                  PUSHA
                  CLEAR_AVAILABLE_PLACES2
                  POPA

                  DRAWWITHSOURCE       border2data,borderwidth,borderheight,endRowCursor2,endColCursor2,150D,0D    ; col,row

                  mov bl,BYTE PTR stateOfQ2
                  dec bl
                  mov BYTE PTR stateOfQ2,bl

                  FIRSTQHANDLEM

                  checkqhandle2   
                  ; checkqhandle     
                  CHECKMATE       
ENDM SECONDQHANDLE2

CLEAR_AVAILABLE_PLACES MACRO ;This Macro is Responsible for remove the marks that we made on the cells that the selected white piece can move to 
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
                  mov                  BYTE PTR  dummyData1,al
                  mov                  BYTE PTR  dummyData2,ah
                  pusha
                  GETARINDEXBYBYTE      dummyData1,dummyData2
                  cmp                  BYTE PTR  cursorState[bx],0
                  jne                   TMP
                  JMP far ptr                   Nbreak6
    TMP:          
                  MOV                  BYTE PTR  cursorState[bx],0
                  popa
                  MOV                  BYTE PTR  dummyData3,0D
                  MOV                  BYTE PTR  dummyData4,0D
                  ADD                  BYTE PTR  dummyData3,al
                  ADD                  BYTE PTR  dummyData4,ah
                  pusha
                  UPDATECELL            dummyData3,dummyData4,150D,0D
                  
    Nbreak6:      
                  popa
                  inc                   al
                  cmp                   al,8
                  je                   TMP2
                  JMP far ptr                   Nloop10
    TMP2:         
                  inc                   ah
                  cmp                   ah,8
                  je                   TMP3
                  JMP far ptr                   Nloop9
    TMP3:

                  DRAWWITHSOURCE borderdata,borderwidth,borderheight,curRowCursor,curColCursor,150D,0D
                  DRAWWITHSOURCE border2data,borderwidth,borderheight,curRowCursor2,curColCursor2,150D,0D
                  DRAW_AVAILABLE_PLACES2

    ENDM CLEAR_AVAILABLE_PLACES

CLEAR_AVAILABLE_PLACES2 MACRO ;This Macro is Responsible for remove the marks that we made on the cells that the selected black piece can move to 
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
                  mov                  BYTE PTR  dummyData1,al
                  mov                  BYTE PTR  dummyData2,ah
                  pusha
                  GETARINDEXBYBYTE      dummyData1,dummyData2
                  cmp                  BYTE PTR  cursorState2[bx],0
                  jne                   TMP
                  JMP far ptr                   Nbreak6
    TMP:          
                  MOV                  BYTE PTR  cursorState2[bx],0
                  popa
                  MOV                  BYTE PTR  dummyData3,0D
                  MOV                  BYTE PTR  dummyData4,0D
                  ADD                  BYTE PTR  dummyData3,al
                  ADD                 BYTE PTR   dummyData4,ah
                  pusha
                  UPDATECELL            dummyData3,dummyData4,150D,0D
                  
    Nbreak6:      
                  popa
                  inc                   al
                  cmp                   al,8
                  je                   TMP2
                  JMP far ptr                   Nloop10
    TMP2:         
                  inc                   ah
                  cmp                   ah,8
                  je                   TMP3
                  JMP far ptr                   Nloop9
    TMP3:
                  DRAWWITHSOURCE borderdata,borderwidth,borderheight,curRowCursor,curColCursor,150D,0D
                  DRAWWITHSOURCE border2data,borderwidth,borderheight,curRowCursor2,curColCursor2,150D,0D
                  DRAW_AVAILABLE_PLACES

    ENDM CLEAR_AVAILABLE_PLACES2

CHECKMATE MACRO ;This Macro is Responsible for printing to two players when the king is about to die
LOCAL LOOPAGAIN1
LOCAL CONTINUOUEGAME1
LOCAL LOOPAGAIN2
LOCAL CONTINUOUEGAME2
LOCAL not4f
LOCAL not4f2
MOV BX, 64
LOOPAGAIN1:
DEC BX
CMP gridState[BX],5 ; BLACK KING
jz CONTINUOUEGAME1
CMP BX,0
jnz LOOPAGAIN1

MOV AL,1
MOV WINNER,AL ;;;;;;;;;;;if white wins print it as winner and wait for f4 key
  movecursor 37,30
  ShowMessage WINNERISWHITE
  not4f:
  mov ah,0
  int 16h
  cmp ah,3Eh;f4
  jnz not4f
JMP far ptr faraway
CONTINUOUEGAME1:

MOV BX, 64
LOOPAGAIN2:
DEC BX
CMP gridState[BX],12 ; WHITE KING
jz CONTINUOUEGAME2
CMP BX,0
jnz LOOPAGAIN2
MOV AL,2
MOV WINNER,AL
  movecursor 37,30
  ShowMessage WINNERISBLACK
  not4f2:
  mov ah,0
  int 16h
  cmp ah,3Eh;f4
  jnz not4f2
JMP far ptr faraway
CONTINUOUEGAME2:

ENDM CHECKMATE

connect MACRO
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
  ENDM connect

CURSORMOV MACRO ;This Macro is Responsible for Game Logic When any player move the cursor or press Q or ENTER or other thing
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
  LOCAL temp24
  LOCAL tmplabel102
  LOCAL tmplabel202
  LOCAL qpressed2
  LOCAL firsrQ2
  LOCAL label62
  LOCAL label72
  LOCAL label82
  LOCAL label92
  LOCAL up2
  LOCAL left2
  LOCAL right2
  LOCAL down2
  LOCAL label52
  LOCAL skip12
  LOCAL temp202
  LOCAL label42
  LOCAL skip22
  LOCAL temp222
  LOCAL label22
  LOCAL skip32
  LOCAL label102
  LOCAL label12
  LOCAL skip42
  LOCAL label112
  local skip1e
  LOCAL skip12e
  LOCAL skip2e
  LOCAL skip22e
  LOCAL skip3e
  LOCAL skip32e
  LOCAL skip4e
  LOCAL skip42e
  LOCAL skip42e

                  connect   
cursorLoop:

                  PRINTCURRTIMER

                  mov         ah,01
                  int         16h
                  jnz temp24
                  JMP far ptr          curs
                temp24:
                  mov         ah,0
                  int         16h

                  ;if f4 is pressed return to main screen  
                  cmp ah,3Eh
                  jnz dontexit
                  JMP far ptr faraway  
                  dontexit: 

                  cmp ah,10h
                  jnz             tmplabel10
                  JMP far ptr qpressed
tmplabel10:

 cmp ah,1ch
                  jnz             tmplabel102
                  JMP far ptr qpressed2
tmplabel102:
                ; handle chat f6 click
                  cmp             ah,40h
                  jnz             temp23
                  cmp f6,0
                  je set
                  mov f6,0
                  JMP far ptr temp23
                  set:
                  mov f6,1
                temp23:
                ;end handle chat f6 click

                ;   pusha
                ;   GETARINDEXBYBYTE curRowCursor,startColCursor
                ;   popa
                 ; mov firstIndex,bx

                  cmp             ah,11h
                  jnz             label6
                  JMP far ptr             up
    label6:

                  cmp             ah,1eh
                  jnz             label7
                  JMP far ptr             left
    label7:

                  cmp             ah,20h
                  jnz             label8
                  JMP far ptr             right
    label8:

                  cmp             ah,1fh
                  jnz             label9
                  JMP far ptr             down
    label9:

       cmp             ah,48h
                  jnz             label62
                  JMP far ptr             up2
    label62:

                  cmp             ah,4bh
                  jnz             label72
                  JMP far ptr             left2
    label72:

                  cmp             ah,4dh
                  jnz             label82
                  JMP far ptr             right2
    label82:

                  cmp             ah,50h
                  jnz             label92
                  JMP far ptr             down2
    label92:

           

                  JMP far ptr             cursorLoop                  

    left:
                  mov             dx,curColCursor
                  cmp             dx,0D
                  jnz             temp20

                  JMP far ptr             cursorLoop
    temp20:
 pusha
                  UPDATECELL     curRowCursor,curColCursor,150D,0D
                  popa
  pusha
                  DRAWWITHSOURCE       border2data,borderwidth,borderheight,curRowCursor2,curColCursor2,150D,0D    ; col,row
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

                  cmp cursorState2[bx],0
                  je skip1e
                  pusha
                  DRAWWITHSOURCE       select2data,borderwidth,borderheight,curRowCursor,curColCursor,150D,0D    ; col,row
                  popa
                  skip1e:

                  sub             dx,1D

                  mov             curColCursor,dx
                  DRAWWITHSOURCE       borderdata,borderwidth,borderheight,curRowCursor,curColCursor,150D,0D    ; col,row
                  cmp             ah,11h
                  jz             label5

                  JMP far ptr             cursorLoop
    label5:

 left2:
                  mov             dx,curColCursor2
                  cmp             dx,0D
                  jnz             temp202

                  JMP far ptr             cursorLoop
    temp202:
 pusha
                  UPDATECELL     curRowCursor2,curColCursor2,150D,0D
                  popa
 pusha
                  DRAWWITHSOURCE       borderdata,borderwidth,borderheight,curRowCursor,curColCursor,150D,0D    ; col,row
                  popa
                  pusha
                  GETARINDEXBYBYTE curRowCursor2,curColCursor2
                  mov firstIndex2,bx
                  popa
                  mov bx,firstIndex2
                  cmp cursorState2[bx],0
                  je skip12
                  pusha
                  DRAWWITHSOURCE       select2data,borderwidth,borderheight,curRowCursor2,curColCursor2,150D,0D    ; col,row
                  popa
                  skip12:

                  cmp cursorState[bx],0
                  je skip12e
                  pusha
                  DRAWWITHSOURCE       selectdata,borderwidth,borderheight,curRowCursor2,curColCursor2,150D,0D    ; col,row
                  popa
                  skip12e:
                  
                  sub             dx,1D

                  mov             curColCursor2,dx
                  DRAWWITHSOURCE       border2data,borderwidth,borderheight,curRowCursor2,curColCursor2,150D,0D    ; col,row
                  cmp             ah,4bh
                  jz              label52

                  JMP far ptr             cursorLoop
    label52:

    right:
                  mov             dx,curColCursor
                  cmp             dx,7d
                  jnz             temp22
                  JMP far ptr             cursorLoop
    temp22:
  pusha
                  UPDATECELL     curRowCursor,curColCursor,150D,0D
                  popa
 pusha
                  DRAWWITHSOURCE       border2data,borderwidth,borderheight,curRowCursor2,curColCursor2,150D,0D    ; col,row
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

                  cmp cursorState2[bx],0
                  je skip2e
                  pusha
                  DRAWWITHSOURCE       select2data,borderwidth,borderheight,curRowCursor,curColCursor,150D,0D    ; col,row
                  popa
                  skip2e:

                  add             dx,1
                  mov             curColCursor,dx
                  DRAWWITHSOURCE       borderdata,borderwidth,borderheight,curRowCursor,curColCursor,150D,0D    ; col,row
                  cmp             ah,20h
                  jz              label4
                  JMP far ptr             cursorLoop
    label4:

    right2:
                  mov             dx,curColCursor2
                  cmp             dx,7d
                  jnz             temp222
                  JMP far ptr             cursorLoop
    temp222:
  pusha
                  UPDATECELL     curRowCursor2,curColCursor2,150D,0D
                  popa
 pusha
                  DRAWWITHSOURCE       borderdata,borderwidth,borderheight,curRowCursor,curColCursor,150D,0D    ; col,row
                  popa
                  pusha
                  GETARINDEXBYBYTE curRowCursor2,curColCursor2
                  mov firstIndex2,bx
                  popa
                  mov bx,firstIndex2
                  cmp cursorState2[bx],0
                  je skip22
                  pusha
                  DRAWWITHSOURCE       select2data,borderwidth,borderheight,curRowCursor2,curColCursor2,150D,0D    ; col,row
                  popa
                  skip22:

                  cmp cursorState[bx],0
                  je skip22e
                  pusha
                  DRAWWITHSOURCE       selectdata,borderwidth,borderheight,curRowCursor2,curColCursor2,150D,0D    ; col,row
                  popa
                  skip22e:

                  add             dx,1
                  mov             curColCursor2,dx
                  DRAWWITHSOURCE       border2data,borderwidth,borderheight,curRowCursor2,curColCursor2,150D,0D    ; col,row
                  cmp             ah,4dh
                  jz              label42
                  JMP far ptr             cursorLoop
    label42:

    up:
                  mov             dx,curRowCursor
                  cmp             dx,0D
                  jnz             label10
                  JMP far ptr             cursorLoop
    label10:

  pusha
                  UPDATECELL     curRowCursor,curColCursor,150D,0D
                  popa
 pusha
                  DRAWWITHSOURCE       border2data,borderwidth,borderheight,curRowCursor2,curColCursor2,150D,0D    ; col,row
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

                  cmp cursorState2[bx],0
                  je skip3e
                  pusha
                  DRAWWITHSOURCE       select2data,borderwidth,borderheight,curRowCursor,curColCursor,150D,0D    ; col,row
                  popa
                  skip3e:

                  sub             dx,1D
                  mov             curRowCursor,dx
                  DRAWWITHSOURCE       borderdata,borderwidth,borderheight,curRowCursor,curColCursor,150D,0D    ; col,row
                  cmp             ah,11h
                  jz              label2

                  JMP far ptr             cursorLoop
    label2:

    up2:
                  mov             dx,curRowCursor2
                  cmp             dx,0D
                  jnz             label102
                  JMP far ptr             cursorLoop
    label102:

  pusha
                  UPDATECELL     curRowCursor2,curColCursor2,150D,0D
                  popa
 pusha
                  DRAWWITHSOURCE       borderdata,borderwidth,borderheight,curRowCursor,curColCursor,150D,0D    ; col,row
                  popa
                  pusha
                  GETARINDEXBYBYTE curRowCursor2,curColCursor2
                  mov firstIndex2,bx
                  popa
                  mov bx,firstIndex2
                  cmp cursorState2[bx],0
                  je skip32
                  pusha
                  DRAWWITHSOURCE       select2data,borderwidth,borderheight,curRowCursor2,curColCursor2,150D,0D    ; col,row
                  popa
                  skip32:

                  cmp cursorState[bx],0
                  je skip32e
                  pusha
                  DRAWWITHSOURCE       selectdata,borderwidth,borderheight,curRowCursor2,curColCursor2,150D,0D    ; col,row
                  popa
                  skip32e:

                  sub             dx,1D

                  mov             curRowCursor2,dx
                  DRAWWITHSOURCE       border2data,borderwidth,borderheight,curRowCursor2,curColCursor2,150D,0D    ; col,row
                  cmp             ah,48h
                  jz              label22

                  JMP far ptr             cursorLoop
    label22:




    down:
                  mov             dx,curRowCursor
                  cmp             dx,7D
                  jnz             label11
                  JMP far ptr             cursorLoop
    label11:
                  pusha
                  UPDATECELL     curRowCursor,curColCursor,150D,0D
                  popa
 pusha
                  DRAWWITHSOURCE       border2data,borderwidth,borderheight,curRowCursor2,curColCursor2,150D,0D    ; col,row
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

                  cmp cursorState2[bx],0
                  je skip4e
                  pusha
                  DRAWWITHSOURCE       select2data,borderwidth,borderheight,curRowCursor,curColCursor,150D,0D    ; col,row
                  popa
                  skip4e:

                  add             dx,1
                  mov             curRowCursor,dx
                  DRAWWITHSOURCE       borderdata,borderwidth,borderheight,curRowCursor,curColCursor,150D,0D    ; col,row
                  cmp             ah,1fh
                  jz              label1
                  JMP far ptr             cursorLoop
    label1:

    down2:
                  mov             dx,curRowCursor2
                  cmp             dx,7D
                  jnz             label112
                  JMP far ptr             cursorLoop
    label112:
                  pusha
                  UPDATECELL     curRowCursor2,curColCursor2,150D,0D
                  popa
 pusha
                  DRAWWITHSOURCE       borderdata,borderwidth,borderheight,curRowCursor,curColCursor,150D,0D    ; col,row
                  popa
                  pusha
                  GETARINDEXBYBYTE curRowCursor2,curColCursor2
                  mov firstIndex2,bx
                  popa
                  mov bx,firstIndex2
                  cmp cursorState2[bx],0
                  je skip42
                  pusha
                  DRAWWITHSOURCE       select2data,borderwidth,borderheight,curRowCursor2,curColCursor2,150D,0D    ; col,row
                  popa
                  skip42:

                  cmp cursorState[bx],0
                  je skip42e
                  pusha
                  DRAWWITHSOURCE       selectdata,borderwidth,borderheight,curRowCursor2,curColCursor2,150D,0D    ; col,row
                  popa
                  skip42e:

                  add             dx,1
                  mov             curRowCursor2,dx
                  DRAWWITHSOURCE       border2data,borderwidth,borderheight,curRowCursor2,curColCursor2,150D,0D    ; col,row
                  cmp             ah,50h
                  jz              label12
                  JMP far ptr             cursorLoop
    label12:


    qpressed:
    mov bl,stateOfQ
    cmp bl,0
    jnz tmplabel20
    JMP far ptr firsrQ
    tmplabel20:

    SECONDQHANDLE
    JMP far ptr   cursorLoop   

    firsrQ:
    FIRSTQHANDLE
                 JMP far ptr             cursorLoop   
;--------------------------------------------------
    qpressed2:
    mov bl,stateOfQ2
    cmp bl,0
    jnz tmplabel202
    JMP far ptr firsrQ2
    tmplabel202:

    SECONDQHANDLE2
    JMP far ptr   cursorLoop   

    firsrQ2:
    FIRSTQHANDLE2
                 JMP far ptr             cursorLoop   
;--------------------------------------------------


ENDM CURSORMOV

DRAW_AVAILABLE_PLACES MACRO ;This Macro is Responsible for draw marks on the cells that the selected white piece can move to 
LOCAL loop9
LOCAL loop10
LOCAL break6
LOCAL BREAK7
LOCAL BREAK8
local beforeloop9
local beforeloop10
mov al,0
mov ah,0
loop9:
mov al,0
loop10:
 mov BYTE PTR dummyData1,al
 mov BYTE PTR dummyData2,ah
 pusha
 GETARINDEXBYBYTE dummyData1,dummyData2
 cmp BYTE PTR cursorState[bx],0
 je BREAK7
 popa
 mov BYTE PTR dummyData1,al
 mov BYTE PTR dummyData2,ah
 pusha
DRAWWITHSOURCE       selectdata,selectwidth,selectheight,dummyData1,dummyData2,150D,0D 
popa
PUSHA
 GETARINDEXBYBYTE dummyData1,dummyData2
CMP BYTE PTR gridState[BX],5D
je BREAK8
BREAK7:
JMP  BREAK6
BREAK8:
         pusha
        movecursor 2d,31d
        ShowMessage CHECKSTRING
        popa
break6:
popa
inc al
cmp al,8
je beforeloop10
JMP  loop10
beforeloop10:
inc ah
cmp ah,8
je beforeloop9
JMP  loop9
beforeloop9:


ENDM DRAW_AVAILABLE_PLACES

DRAW_AVAILABLE_PLACES2 MACRO ;This Macro is Responsible for draw marks on the cells that the selected black piece can move to 
LOCAL loop9
LOCAL loop10
LOCAL break6
LOCAL BREAK7
LOCAL BREAK8
local beforeloop9
local beforeloop10
mov al,0
mov ah,0
loop9:
mov al,0
loop10:
 mov BYTE PTR dummyData1,al
 mov BYTE PTR dummyData2,ah
 pusha
 GETARINDEXBYBYTE dummyData1,dummyData2
 cmp BYTE PTR cursorState2[bx],0
 je BREAK7
 popa
 mov BYTE PTR dummyData1,al
 mov BYTE PTR dummyData2,ah
 pusha
DRAWWITHSOURCE       select2data,select2width,select2height,dummyData1,dummyData2,150D,0D 
popa
PUSHA
 GETARINDEXBYBYTE dummyData1,dummyData2
CMP BYTE PTR gridState[BX],12D
je BREAK8
BREAK7:
JMP far ptr break6
BREAK8:
         pusha
        movecursor 80d,31d
        ShowMessage CHECKSTRING
        popa
break6:
popa
inc al
cmp al,8
je beforeloop10
JMP far ptr loop10
beforeloop10:
inc ah
cmp ah,8
je beforeloop9
JMP far ptr loop9
beforeloop9:


ENDM DRAW_AVAILABLE_PLACES2

check_AVAILABLE_PLACES MACRO ;This Macro is Responsible for draw marks on the cells that the selected white piece can move to 
LOCAL loop9
LOCAL loop10
LOCAL break6
LOCAL BREAK7
LOCAL BREAK8
local beforeloop9
local beforeloop10
mov al,0
mov ah,0
loop9:
mov al,0
loop10:
 mov BYTE PTR dummyData1,al
 mov BYTE PTR dummyData2,ah
 pusha
 GETARINDEXBYBYTE dummyData1,dummyData2
 cmp BYTE PTR cursorState[bx],0
 je BREAK7
 popa
 mov BYTE PTR dummyData1,al
 mov BYTE PTR dummyData2,ah

PUSHA
 GETARINDEXBYBYTE dummyData1,dummyData2
CMP BYTE PTR gridState[BX],5D
je BREAK8
BREAK7:
JMP far ptr BREAK6
BREAK8:
         pusha
        movecursor 2d,31d
        ShowMessage CHECKSTRING
        popa
break6:
popa
inc al
cmp al,8
je beforeloop10
JMP far ptr loop10
beforeloop10:
inc ah
cmp ah,8
je beforeloop9
JMP far ptr loop9
beforeloop9:

ENDM check_AVAILABLE_PLACES

check_AVAILABLE_PLACES2 MACRO ;This Macro is Responsible for draw marks on the cells that the selected black piece can move to 
LOCAL loop9
LOCAL loop10
LOCAL break6
LOCAL BREAK7
LOCAL BREAK8
local beforeloop9
local beforeloop10
mov al,0
mov ah,0
loop9:
mov al,0
loop10:
 mov BYTE PTR dummyData1,al
 mov BYTE PTR dummyData2,ah
 pusha
 GETARINDEXBYBYTE dummyData1,dummyData2
 cmp BYTE PTR cursorState2[bx],0
 je BREAK7
 popa
 mov BYTE PTR dummyData1,al
 mov BYTE PTR dummyData2,ah

PUSHA
 GETARINDEXBYBYTE dummyData1,dummyData2
CMP BYTE PTR gridState[BX],12D
je BREAK8
BREAK7:
JMP far ptr break6
BREAK8:
         pusha
        movecursor 80d,31d
        ShowMessage CHECKSTRING
        popa
break6:
popa
inc al
cmp al,8
je beforeloop10
JMP far ptr loop10
beforeloop10:
inc ah
cmp ah,8
je beforeloop9
JMP far ptr loop9
beforeloop9:

ENDM check_AVAILABLE_PLACES2

DrawPiecies MACRO A,B ;This Macro is Responsible for drawing the pieces when the game starts
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

INITIALIZEGRID MACRO A,B ;This Macro is Responsible for initializig the first state of the grid as the initial location of pieces and the color of the cells (A,B)

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

ISEMPTY MACRO x,y ;This Macro is of return type boolean and Responsible for checking if the cell is empty cell or contains piece
LOCAL break5
LOCAL empty
pusha
GETARINDEXBYBYTE x,y
cmp  gridState[bx],0
je empty
popa
mov bx,0
JMP far ptr break5
empty:
popa
mov bx,1
break5:
ENDM ISEMPTY

validateName MACRO entermsg,name,strFailed ;This Macro is Responsible for validating the name entered by players
    LOCAL repeatt
    LOCAL biggerthana
    LOCAL outOfTheValidation
    LOCAL fistcheck


    movecursor  17H,05H
    ShowMessage entermsg
    movecursor  17H,06H
    cin         name
    ;movecursor  17H,0AH
    JMP far ptr fistcheck
;---------fist check with enter message-----------;
    repeatt:

    CALL far ptr CLS

    movecursor  17H,05H
    ShowMessage strFailed
    movecursor  17H,06H
    cin         name

    fistcheck:
;---------other checks with error message-----------;
    mov bx,offset name + 2
    mov al,[bx]
    mov bl,122;;== z 
    cmp bl,al;;if greater than z JMP far ptr
    jc repeatt
    cmp al,65;;== A
    jc repeatt;;if less than A JMP far ptr
    mov bl,90;;== Z 
    cmp bl,al;;if greater than Z JMP far ptr
    jc biggerthana
    JMP far ptr outOfTheValidation
    biggerthana:
    cmp al,97;;== a
    jc repeatt;;if less than a JMP far ptr
    outOfTheValidation:
ENDM validateName

movecursorWithPageNumber MACRO x,y,p  ;This Macro is Responsible for moving the cursor
                mov         ah,2
                mov         bh,BYTE PTR p
                mov cl,x
                mov ch,y
                mov         dh,ch
                mov         dl,cl
                int         10h
ENDM        movecursorWithPageNumber

MAINMAIN MACRO player1Name,player2Name ;This Macro is Responsible for moving between screens when player press f2 or esc
    LOCAL check_for_anotherkey
    LOCAL check_for_f2
    LOCAL check_for_esc

    check_for_anotherkey:
    mov ah,0
    int 16h 
    cmp ah,3bh;f1 scan code
    jz skipf2chk;;;;;;;;;;;;;;;;;;;;;;;;;;;
    JMP far ptr check_for_f2
    skipf2chk:
    ;open chat
    OPENCHAT player1Name+2,player2Name+2
    check_for_f2:
    cmp ah,3ch;f2 scane code
    jnz check_for_esc
    ;open game
    JMP far ptr play

    check_for_esc:
    cmp al,01Bh;esc ascii
    jz skipescchk
    JMP far ptr check_for_anotherkey;;;;;;;;;;;;;;;;;;
    skipescchk:
    ;exist game
    MOV AH, 4CH
    MOV AL, 01 ;your return code.
    INT 21H

ENDM MAINMAIN

STATUSLINE MACRO ;This Macro is Responsible for the status bar that appears to players
  LOCAL LOOPXXXX
  LOCAL WHITEDIDNTWIN
  MOV BL,10
  ;;;;;;;;;;;;;;;;; --------- lined
  LOOPXXXX:
  movecursorWithPageNumber BL,18,0
  mov ah,2
  MOV DL,'-' 
  int 21h
  INC BL
  CMP BL,70
  jnz LOOPXXXX
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  movecursorWithPageNumber 30,20,0

  CMP WINNER,1
  jnz WHITEDIDNTWIN
  MOV AL,0
  MOV WINNER,AL
  ShowMessage WINNERISWHITE

  JMP far ptr BLACKIDNTWIN
  WHITEDIDNTWIN:

  CMP WINNER,2
  jnz BLACKIDNTWIN
  MOV AL,0
  MOV WINNER,AL
  ShowMessage WINNERISBLACK
  
  BLACKIDNTWIN:
ENDM STATUSLINE

OPENCHAT MACRO player1Name,player2Name ;This Macro is Responsible for handling Game Chat
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
                  jz         AGAIN             ;jump untill it is empty

    ;If empty put the VALUE in Transmit data register
                  mov         dx , 3F8H         ; Transmit data register
                  mov         al,bl
                  out         dx , al
                  JMP far ptr         AGAIN
           
  
    deadmid:      
                  JMP far ptr         dead

                
    AGAIN:     
      ;Receiving a value     
    ;Check that Data Ready
                  mov         dx , 3FDH         ; Line Status Register
          
                  in          al , dx
                  AND         al , 00000001b
                  jz          CHK               ;jump untill it recive data

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

                  JMP far ptr         mainloop
    dead:  
                  mov  al, 00h   ; select display page 0
                  mov  ah, 05h   ; function 05h: select active display page
                  int  10h

ENDM OPENCHAT
                
GETARINDEX MACRO X,Y  ;This Macro is Responsible for Getting the cell number in 1-d array of size (64) from the row and coloum numbers of the cell and OUTPUT IN BX
    MOV AX,X 
    MOV BL , 8D
    MUL BL 
    ADD AX , Y
    MOV BX,AX
ENDM GETARINDEX

GETARINDEXBYTE MACRO X,Y ;This Macro is another version from GETARINDEX but by byte
    MOV AL,byte ptr X 
    MOV BL , 8D
    MUL BL 
    MOV CL , byte ptr Y 
    MOV CH , 0 
    ADD AX , CX
    MOV BX,AX
ENDM GETARINDEX

UPDATECELL MACRO X,Y,A,B ;This Macro is Responsible for updating the cell by current state (piece inside it and color of the cell) 
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

GETIMGDATA MACRO X,Y  ;This Macro is Responsible for getting image data and store it in colored pixels to draw 
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
    MOV AX,word ptr X 
    MOV BL , 8D
    MUL BL 
    ADD AX ,word ptr  Y 

    ; GETTING THE STATE OF THE GRID AT (X,Y) WHICH IS 0 --> 12
    LEA SI,gridState
    ADD SI,AX
    MOV AL,BYTE PTR [SI]
    MOV AH,0H

    CMP AX,0
    je EMPTY2
    
    ; DEC AX 
    ; MOV BX,360D
    ; MUL BX

    ; ; LOADING THE IMG DATA 
    ; LEA SI,BROCKDATA
    ; ADD SI,AX

    ; MOV BX,SI
    ; JMP far ptr RETURN 
    
    CMP AX,1D
    je B1

   CMP AX,2D
   je B2
   CMP AX,3D
   je B3
   CMP AX,4D
   je B4
   CMP AX,5D
   je B5
   CMP AX,6D
   je B6
   CMP AX,7D
   je B7
   CMP AX,8D
   je B8
   CMP AX,9D
   je B9
   CMP AX,10D
   je B10
   CMP AX,11D
   je B11
   CMP AX,12D
   je B12

   EMPTY2: JMP far ptr EMPTY
       

    B1: LEA BX,brockdata
    JMP far ptr RETURN
        B2: LEA BX,bknightdata       
    JMP far ptr RETURN
        B3: LEA BX,bbishopdata       
    JMP far ptr RETURN
        B4: LEA BX,bqueendata        
    JMP far ptr RETURN
        B5: LEA BX,bkingdata         
    JMP far ptr RETURN
        B6: LEA BX,bpawndata         
    JMP far ptr RETURN
        B7: LEA BX,wpawndata         
    JMP far ptr RETURN
        B8: LEA BX,wrockdata         
    JMP far ptr RETURN
        B9: LEA BX,wknightdata       
    JMP far ptr RETURN
        B10: LEA BX,wbishopdata       
    JMP far ptr RETURN
        B11: LEA BX,wqueendata        
    JMP far ptr RETURN
        B12: LEA BX,wkingdata         
    JMP far ptr RETURN

    EMPTY:
    MOV BX,0

    RETURN:

ENDM GETIMGDATA

ISWHITE MACRO X,Y ;This Macro is of return type boolean and Responsible for checking if the selected piece white or black
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
jg WHITE
CMP CH , 7
jl BLACK 

WHITE:
MOV BX,1 
JMP far ptr RETURN
BLACK: 
MOV BX, 0
RETURN: 


ENDM ISWHITE

ISWHITE2 MACRO X,Y ;This Macro is another version of ISWHITE that that handles some corner cases when piece is black 
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
jg WHITE
CMP CH , 0 
jg BLACK 

WHITE:
MOV BX,1 
JMP far ptr RETURN
BLACK: 
MOV BX, 0
RETURN: 


ENDM ISWHITE2

ISWHITEBYTE MACRO X,Y ;This Macro is another version from ISWHITE but by byte
LOCAL WHITE 
LOCAL BLACK 
LOCAL RETURN 

MOV AX ,0 
MOV AL,byte ptr X
MOV CL,byte ptr Y
MOV CH,0

LEA SI,gridState 
GETARINDEX AX,CX 
ADD SI,BX
MOV CH,BYTE PTR [SI]
CMP CH,6 
jg WHITE
CMP CH , 7 
jl BLACK 

WHITE:
MOV BX,1 
JMP far ptr RETURN
BLACK: 
MOV BX, 0
RETURN: 


ENDM ISWHITEBYTE

ISWHITEBYTE2 MACRO X,Y ;This Macro is another version from ISWHITE2 but by byte
LOCAL WHITE 
LOCAL BLACK 
LOCAL RETURN 

MOV AX ,0 
MOV AL,byte ptr X
MOV CL,byte ptr Y
MOV CH,0

LEA SI,gridState 
GETARINDEX AX,CX 
ADD SI,BX
MOV CH,BYTE PTR [SI]
CMP CH,6 
jg WHITE
CMP CH , 0 
jg BLACK 

WHITE:
MOV BX,1 
JMP far ptr RETURN
BLACK: 
MOV BX, 0
RETURN: 


ENDM ISWHITEBYTE2

INSIDEGRID MACRO X , Y ;This Macro is Responsible for checking if the row number and column number inside the grid or not 
LOCAL NOTVALID 
LOCAL VALID 
LOCAL RETURN 

MOV AL , X 
MOV AH ,Y

CMP AL , 7 
jg NOTVALID
CMP AL , 0 
jl NOTVALID
CMP AH , 7 
jg NOTVALID
CMP AH , 0 
jl NOTVALID


VALID: 
MOV BX,1 
JMP far ptr RETURN
NOTVALID:
MOV BX, 0
RETURN: 

ENDM INSIDEGRID

ENTERGAMECHAT MACRO player1Name,player2Name ;This Macro is Responsible for entering the game chat
                        LOCAL dead
            LOCAL mainloop
            LOCAL afterenter
            local afterenter2
            LOCAL deadmid
            local midh
            local CHK
            local AGAIN
            local enter
            local temp88
            local set2
            local bla

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

                  mov         dh,00H
                  mov         dl,23H
                  push        dx
                  mov         dh,00H
                  mov         dl,20H
                  push        dx

                  movecursorWithPageNumber  00H,20H,1D
                
    ;program starts here
    mainloop:     
                  
                cmp f6,1
                je enter
                JMP far ptr AGAIN
                enter:
                  mov         ah,01
                  int         16h
                  jnz bla
                  JMP far ptr          AGAIN
                bla:

                  mov         ah,0
                  int         16h
                  
                  cmp             ah,40h
                  jnz             temp88
                  cmp f6,0
                  je set2
                  mov f6,0
                  JMP far ptr temp88
                  set2:
                  mov f6,1
                  temp88:

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
                  jz          AGAIN             ;jump untill it is empty

    ;If empty put the VALUE in Transmit data register
                  mov         dx , 3F8H         ; Transmit data register
                  mov         al,bl
                  out         dx , al
                  JMP far ptr         AGAIN
           
    ;Receiving a value
    deadmid:      
                  JMP far ptr         dead
                  
    AGAIN:        
    ;Check that Data Ready
                  mov         dx , 3FDH         ; Line Status Register
          
                  in          al , dx
                  AND         al , 00000001b
                  jz         CHK               ;jump if no recive data

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
                
                ;   JMP far ptr         mainloop
    dead:  
                  mov  al, 00h   ; select display page 0
                  mov  ah, 05h   ; function 05h: select active display page
                  int  10h

ENDM ENTERGAMECHAT

GETTIME MACRO ;This Macro is Responsible for handling the time (3 sec) for each piece and OUTPUT IN BX AND TIME DATA

    MOV TIME,0
    MOV AH,2CH
    INT 21H ; SEC -> DH ;MIN -> CL ;HRS ->CH

    ; MOV AX,0
    ; MOV AL,CH ;HOURS
    ; MOV BL,3600D
    ; MUL BL

    ; ADD TIME,AX

    MOV AX,0
    MOV AL,CL ;MINUTES
    MOV BL,60
    MUL BL

    ADD TIME,AX

    MOV AX,0 
    MOV AL,dh ;SECONDS

    ADD TIME,AX

    MOV BX,WORD PTR TIME

ENDM GETTIME

INITIALIZETIME MACRO ;This Macro is Responsible for initializing the time when game starts
                  GETTIME
                  MOV     WORD PTR STARTTIME,BX
ENDM INITIALIZETIME

PRINTCURRTIMER MACRO ;This Macro is Responsible for printing the time on the screen
    LOCAL NSP 
                  movecursor  0,0
                  GETTIME
                  MOV         AX,BX
                  MOV         BX,WORD PTR STARTTIME
                  SUB         AX,BX
                  MOV         CL,60D
                  DIV         CL
                  PUSHA
                  MOV         AH,0
                  TOSTRING    MIN
                  ShowMessage MIN
                  ShowMessage COLN
                  POPA
                  MOV         AL,AH
                  MOV         AH,0
                  TOSTRING    SEC
                  ShowMessage SEC

                  CMP         SEC[1],'$'
                  jne         NSP
                  
                  ShowMessage SPACE
    NSP:          
                  MOV         SEC[1],'$'
                  MOV         MIN[1],'$'

ENDM PRINTCURRTIMER

.MODEL LARGE
.286
; .STACK 64
stack segment para stack
        db 128 dup ( ' ' )
stack ends

data segment para

  nameq             db  'Please enter your name:','$'
  erroname          db  'Please write a valid name :','$'
  clear             db  '                                                                                                    ','$'
  line              db  '---------------------------------------------------','$'
  WINNERISBLACK     db  'winner is black','$'
  WINNERISWHITE     db  'winner is white','$'

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
  
  king_dx           db  -1,-1,0,1,1,1,0,-1
  king_dy           db  0,1,1,1,0,-1,-1,-1

  WINNER            db  0
  thename           db  16,?,16 dup('$')                                                                                            ; max size 15 char last digit for $
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
  ;--------------
  select2width      equ 60D
  select2height     equ 60D
  select2filename   db  'select2.bin',0
  select2filehandle DW  ?
  select2data       db  select2width*select2height dup(0)
  ;---------------

  borderwidth       equ 60D
  borderheight      equ 60D
  borderfilename    db  'border.bin',0
  borderfilehandle  DW  ?
  borderdata        db  borderwidth*borderheight dup(0)
  ;---------------

  border2width      equ 60D
  border2height     equ 60D
  border2filename   db  'border2.bin',0
  border2filehandle DW  ?
  border2data       db  border2width*border2height dup(0)

  gridState         db  64  dup(0)
  colorState        db  64  dup(0)
  cursorState       db  64  dup(0)                                                                                                  ; 0 for not cursor 1 for cursor
  cursorState2      db  64  dup(0)
  timeState         dW  64  dup(0)

  curRowCursor      dw  0
  curColCursor      dw  0

  startRowCursor    dw  0
  startColCursor    dw  0

  endRowCursor      dw  0
  endColCursor      dw  0

  cellColorState    db  0

  stateOfQ          db  0

  curRowCursor2     dw  0
  curColCursor2     dw  0

  startRowCursor2   dw  0
  startColCursor2   dw  0

  endRowCursor2     dw  0
  endColCursor2     dw  0

  cellColorState2   db  0

  stateOfQ2         db  0

  dummyData1        db  0
  dummyData2        db  0

  dummyData3        DW  0
  dummyData4        dW  0

  DUMMYX            DB  5
  DUMMYY            DB  5

  firstIndex        db  0

  firstIndex2       db  0
    
  SEC               db  60 DUP('$')
  MIN               db  60 DUP('$')
  SPACE             DB  ' ','$'
  TIME              DW  0
  STARTTIME         DW  0
  COLN              DB  ':','$'

  f6                db  0

  whiterow          db  0
  whitecol          db  0

  blackrow          db  0D
  blackcol          db  10D
  
  CHECKSTRING       DB  'WARNING!!! :CHECK','$'
  ;---------------------------------------------------------------------------------------------------
data ends

code segment
          ASSUME         CS:code, DS:data, SS:stack

MAIN PROC FAR

  ;INITIALIZING
          call           far ptr    GETDATA
          call           far ptr    GETSTACK
          CALL           far ptr   CLS
  ;OPENING AND READING AND CLOSING BIN FILES
          OpenFile       bbishopfilename, bbishopfilehandle
          ReadData       bbishopfilehandle ,bbishopwidth,bbishopheight,bbishopdata
          CloseFile      bbishopfilehandle

          OpenFile       bkingfilename, bkingfilehandle
          ReadData       bkingfilehandle ,bkingwidth,bkingheight,bkingdata
          CloseFile      bkingfilehandle

          OpenFile       bknightfilename, bknightfilehandle
          ReadData       bknightfilehandle ,bknightwidth,bknightheight,bknightdata
          CloseFile      bknightfilehandle

          OpenFile       bpawnfilename, bpawnfilehandle
          ReadData       bpawnfilehandle ,bpawnwidth,bpawnheight,bpawndata
          CloseFile      bpawnfilehandle

          OpenFile       bqueenfilename, bqueenfilehandle
          ReadData       bqueenfilehandle ,bqueenwidth,bqueenheight,bqueendata
          CloseFile      bqueenfilehandle

          OpenFile       brockfilename, brockfilehandle
          ReadData       brockfilehandle ,brockwidth,brockheight,brockdata
          CloseFile      brockfilehandle

  ;--white piecies----
          OpenFile       wbishopfilename, wbishopfilehandle
          ReadData       wbishopfilehandle ,wbishopwidth,bbishopheight,wbishopdata
          CloseFile      wbishopfilehandle

          OpenFile       wkingfilename, wkingfilehandle
          ReadData       wkingfilehandle ,wkingwidth,wkingheight,wkingdata
          CloseFile      wkingfilehandle

          OpenFile       wknightfilename, wknightfilehandle
          ReadData       wknightfilehandle ,wknightwidth,wknightheight,wknightdata
          CloseFile      wknightfilehandle

          OpenFile       wpawnfilename, wpawnfilehandle
          ReadData       wpawnfilehandle ,wpawnwidth,wpawnheight,wpawndata
          CloseFile      wpawnfilehandle

          OpenFile       wqueenfilename, wqueenfilehandle
          ReadData       wqueenfilehandle ,wqueenwidth,wqueenheight,wqueendata
          CloseFile      wqueenfilehandle

          OpenFile       wrockfilename, wrockfilehandle
          ReadData       wrockfilehandle ,wrockwidth,wrockheight,wrockdata
          CloseFile      wrockfilehandle

  ;--border-----
          OpenFile       borderfilename, borderfilehandle
          ReadData       borderfilehandle ,borderwidth,borderheight,borderdata
          CloseFile      borderfilehandle

          OpenFile       border2filename, border2filehandle
          ReadData       border2filehandle ,border2width,border2height,border2data
          CloseFile      border2filehandle

          OpenFile       selectfilename, selectfilehandle
          ReadData       selectfilehandle ,selectwidth,selectheight,selectdata
          CloseFile      selectfilehandle

          OpenFile       select2filename, select2filehandle
          ReadData       select2filehandle ,select2width,select2height,select2data
          CloseFile      select2filehandle
  ;------------------------------------------------------------------------------------------------
  ;------------------------------------------------------------------------------------------------
  ;------------------------------------------------------------------------------------------------
  ;------------------------------------------------------------------------------------------------

  ;START MENU
          validateName   nameq,thename,erroname                                                    ;Veryyyyyyyyyyyyyyyy STABLE
          movecursor     17H,0AH
          ShowMessage    proceed
          call           far ptr    waitkey
  ;CHOICE MENU
  faraway:

          call           far ptr     CLS
          movecursor     17H,03H
          ShowMessage    op1
          movecursor     17H,08H
          ShowMessage    op2
          movecursor     17H,0DH
          ShowMessage    op3
          STATUSLINE
          MAINMAIN       thename,thename
  ;GAME SCREEN
  play:   
          CALL           far ptr      EnterGraphics
          mov            curColCursor,00h
          mov            curRowCursor,07h
          mov            curColCursor2,00h
          mov            curRowCursor2,00h
          mov            whiterow,0D
          mov            whitecol,0D
          mov            blackrow,0D
          mov            blackcol,10D
          INITIALIZEGRID 42H,06H                                                                   ;0FH,08H
          DrawGrid       150D,0D,colorState[1],colorState[0]
          DrawPiecies    150D,0D

          DRAWWITHSOURCE borderdata,borderwidth,borderheight,curRowCursor,curColCursor,150D,0D
          DRAWWITHSOURCE border2data,borderwidth,borderheight,curRowCursor2,curColCursor2,150D,0D

  ;----------------------
  ; getDrawPosition 30d,0d,whiterow,whitecol
  ; DRAWCELL        cx,dx,0fh
  ; GETIMGDATA      0,0
  ; DRAWWITHSOURCE  [bx],borderwidth,borderheight,whiterow,whitecol,30D,0D
  ; mov             al,whitecol
  ; inc             al
  ; mov             whitecol,al

  ; getDrawPosition 30d,0d,blackrow,blackcol
  ; DRAWCELL        cx,dx,0fh
  ; GETIMGDATA      0,0
  ; DRAWWITHSOURCE  [bx],borderwidth,borderheight,blackrow,blackcol,30D,0D
  ; mov             al,blackcol
  ; inc             al
  ; mov             blackcol,al
  ;---------------------
          INITIALIZETIME
          JMP            far ptr            helpme
code ends

code2 SEGMENT
                ASSUME    CS:code2
  helpme:       
  curs:         
                CURSORMOV

                JMP       far ptr       curs

                EXT
MAIN ENDP

  ;----------------------------------------------------------------------------------------------------------------


  ;--------------------------------------------------Functions---------------------------------------------------------
GETDATA PROC  FAR                             ;GET DATA
                MOV       AX,data
                MOV       DS,AX
                retf
GETDATA ENDP

GETSTACK PROC  FAR                            ;GET DATA
                MOV       AX,stack
                MOV       SS,AX
                retf
GETSTACK ENDP

CLS PROC  FAR                                 ;CLEAR SCREEN
                MOV       AX,0003H
                INT       10H
                retf
CLS ENDP

EnterText PROC          FAR                   ;ENTER TEXT MODE
                MOV       AX,3H
                INT       10H
                retf
EnterText ENDP

EnterGraphics PROC   FAR                      ;ENTER GRAPHICS MODE
                MOV       AX,4F02H
                MOV       BX,103H             ;(800x600) pixel ;grid =480*480; char=60*60
                INT       10H
                retf
EnterGraphics ENDP

waitkey PROC  FAR                             ;wait for key
                MOV       AH , 0
                INT       16h
                retf
waitkey ENDP
code2 ends
END MAIN

;http://www.wagemakers.be/english/doc/vga