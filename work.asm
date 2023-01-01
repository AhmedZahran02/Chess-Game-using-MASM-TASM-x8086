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
                      CMP SI,WORD ptr imgwidth
                      JNE innerloop
                      SUB CX,SI
                      INC DX
                      INC DI
                      CMP DI,WORD ptr imgheight
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
                      CMP SI,WORD PTR imgwidth
                      JNE innerloop
                      SUB CX,SI
                      INC DX
                      INC DI
                      CMP DI,WORD PTR imgheight
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
    local AGAIN
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
                  JLE temp151
                  jmp break80
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
                call far ptr DRAW_AVAILABLE_PLACES
                POPA

                ;Sending a value

    ;Check that Transmitter Holding Register is Empty
    AGAIN:
                  mov         dx , 3FDH         ; Line Status Register

                  In          al , dx           ;Read Line Status
                  AND         al , 00100000b
                  JZ          AGAIN             ;jump untill it is empty

    ;If empty put the VALUE in Transmit data register
                  MOV AH,0
                  GETARINDEXBYBYTE startRowCursor,startColCursor ; out in bx
                  mov         dx , 3F8H         ; Transmit data register
                  mov         al,bl               ;send start cursor
   
                  out         dx , al
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
                jne skpp
                jmp break80
                skpp:

                call far ptr CLEAR_AVAILABLE_PLACES

                  GETARINDEX curRowCursor,curColCursor

                  pusha
                  ISEMPTY startRowCursor,startColCursor
                    cmp bx,1
                    jne temp100
                    jmp break80
                    temp100:
                    popa

                  pusha
                  ISWHITEBYTE startRowCursor,startColCursor
                  cmp bx,0
                  jne temp150
                  jmp break80
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
                  HANDLEPAWN startRowCursor,startColCursor
                  JMP NOACTION
                ROOK:
                  HANDLEROOK startRowCursor,startColCursor
                  JMP NOACTION
                KNIGHT:
                  HANDLEKNIGHT startRowCursor,startColCursor
                  JMP NOACTION
                BISHOP:
                  HANDLEBISHOP startRowCursor,startColCursor
                  JMP NOACTION
                QUEEN:
                  HANDLEQUEEN startRowCursor,startColCursor
                  JMP NOACTION
                KING:
                  HANDLEKING startRowCursor,startColCursor
                  JMP NOACTION
                NOACTION:
                PUSHA
                call far ptr DRAW_AVAILABLE_PLACES
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
                    jne temp100
                    jmp break80
                    temp100:
                    popa

                  pusha
                  ISWHITEBYTE curRowCursor2,curColCursor2
                  cmp bx,1
                  jne temp150
                  jmp break80
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
                  JLE temp151
                  jmp break80
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
                  ; DRAWWITHSOURCE       border2data,borderwidth,borderheight,curRowCursor2,curColCursor2,150D,0D 
                  popa
                  mov bl,BYTE PTR stateOfQ2

                  inc bl
                  mov BYTE PTR stateOfQ2,bl

                ;------------------------
                  GETARINDEX curRowCursor2,curColCursor2
                  CMP BYTE PTR gridState[BX],6d
                  JNE PAWNTEMP
                  JMP PAWN
                  PAWNTEMP:
                  CMP BYTE PTR gridState[BX],1d
                  JNE ROOKTEMP
                  JMP ROOK
                  ROOKTEMP:
                  CMP BYTE PTR gridState[BX],2d
                  JNE KNIGHTTEMP
                  JMP KNIGHT
                  KNIGHTTEMP:
                  CMP BYTE PTR gridState[BX],3d
                  JNE BISHOPTEMP
                  JMP BISHOP
                  BISHOPTEMP:
                  CMP BYTE PTR gridState[BX],4d
                  JNE QUEENTEMP
                  JMP QUEEN
                  QUEENTEMP:
                  CMP BYTE PTR gridState[BX],5d
                  JNE KINGTEMP
                  JMP KING
                  KINGTEMP:
                  JMP NOACTION
                PAWN:
                  HANDLEPAWN2 curRowCursor2,curColCursor2
                  JMP NOACTION
                ROOK:
                  HANDLEROOK2 curRowCursor2,curColCursor2
                  JMP NOACTION
                KNIGHT:
                  HANDLEKNIGHT2 curRowCursor2,curColCursor2
                  JMP NOACTION
                BISHOP:
                  HANDLEBISHOP2 curRowCursor2,curColCursor2
                  JMP NOACTION
                QUEEN:
                  HANDLEQUEEN2 curRowCursor2,curColCursor2
                  JMP NOACTION
                KING:
                  HANDLEKING2 curRowCursor2,curColCursor2
                  JMP NOACTION
                NOACTION:
                PUSHA
                call far ptr DRAW_AVAILABLE_PLACES2
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
                 
                  mov cl,BYTE PTR curRowCursor
                  mov ch,BYTE PTR curColCursor
                  mov BYTE PTR curRowCursor,cl
                  mov BYTE PTR curColCursor,ch

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
                check_AVAILABLE_PLACES
                call far ptr CLEAR_AVAILABLE_PLACES
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
                    jne temp100
                    popa
                    jmp break80
                    temp100:
                    popa

                  pusha
                  ISWHITEBYTE curRowCursor2,curColCursor2
                  cmp bx,1
                  jne temp150
                  popa
                  jmp break80
                  temp150:
                  popa
             
                ;------------------------
                  GETARINDEX curRowCursor2,curColCursor2
                  CMP BYTE PTR gridState[BX],6d
                  JNE PAWNTEMP
                  JMP PAWN
                  PAWNTEMP:
                  CMP BYTE PTR gridState[BX],1d
                  JNE ROOKTEMP
                  JMP ROOK
                  ROOKTEMP:
                  CMP BYTE PTR gridState[BX],2d
                  JNE KNIGHTTEMP
                  JMP KNIGHT
                  KNIGHTTEMP:
                  CMP BYTE PTR gridState[BX],3d
                  JNE BISHOPTEMP
                  JMP BISHOP
                  BISHOPTEMP:
                  CMP BYTE PTR gridState[BX],4d
                  JNE QUEENTEMP
                  JMP QUEEN
                  QUEENTEMP:
                  CMP BYTE PTR gridState[BX],5d
                  JNE KINGTEMP
                  JMP KING
                  KINGTEMP:
                  JMP NOACTION


                PAWN:
                  HANDLEPAWN2 curRowCursor2,curColCursor2
                  JMP NOACTION
                ROOK:
                  HANDLEROOK2 curRowCursor2,curColCursor2
                  JMP NOACTION
                KNIGHT:
                  HANDLEKNIGHT2 curRowCursor2,curColCursor2
                  JMP NOACTION
                BISHOP:
                  HANDLEBISHOP2 curRowCursor2,curColCursor2
                  JMP NOACTION
                QUEEN:
                  HANDLEQUEEN2 curRowCursor2,curColCursor2
                  JMP NOACTION
                KING:
                  HANDLEKING2 curRowCursor2,curColCursor2
                  JMP NOACTION
                NOACTION:
                ;---------------------------------------------------
                check_AVAILABLE_PLACES2
                call far ptr CLEAR_AVAILABLE_PLACES2
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
                jmp break80
                skpp:

    call far ptr CLEAR_AVAILABLE_PLACES2

                  GETARINDEX startRowCursor2,startColCursor2

                  pusha
                  ISEMPTY startRowCursor2,startColCursor2
                    cmp bx,1
                    jne temp100
                    popa
                    jmp break80
                    temp100:
                    popa

                  pusha
                  ISWHITEBYTE startRowCursor2,startColCursor2
                  cmp bx,1
                  jne temp150
                  popa
                  jmp break80
                  temp150:
                  popa
             
                ;------------------------
                  GETARINDEX startRowCursor2,startColCursor2
                  CMP BYTE PTR gridState[BX],6d
                  JNE PAWNTEMP
                  JMP PAWN
                  PAWNTEMP:
                  CMP BYTE PTR gridState[BX],1d
                  JNE ROOKTEMP
                  JMP ROOK
                  ROOKTEMP:
                  CMP BYTE PTR gridState[BX],2d
                  JNE KNIGHTTEMP
                  JMP KNIGHT
                  KNIGHTTEMP:
                  CMP BYTE PTR gridState[BX],3d
                  JNE BISHOPTEMP
                  JMP BISHOP
                  BISHOPTEMP:
                  CMP BYTE PTR gridState[BX],4d
                  JNE QUEENTEMP
                  JMP QUEEN
                  QUEENTEMP:
                  CMP BYTE PTR gridState[BX],5d
                  JNE KINGTEMP
                  JMP KING
                  KINGTEMP:
                  JMP NOACTION


                PAWN:
                  HANDLEPAWN2 startRowCursor2,startColCursor2
                  JMP NOACTION
                ROOK:
                  HANDLEROOK2 startRowCursor2,startColCursor2
                  JMP NOACTION
                KNIGHT:
                  HANDLEKNIGHT2 startRowCursor2,startColCursor2
                  JMP NOACTION
                BISHOP:
                  HANDLEBISHOP2 startRowCursor2,startColCursor2
                  JMP NOACTION
                QUEEN:
                  HANDLEQUEEN2 startRowCursor2,startColCursor2
                  JMP NOACTION
                KING:
                  HANDLEKING2 startRowCursor2,startColCursor2
                  JMP NOACTION
                NOACTION:
                ;---------------------------------------------------
                PUSHA
                call far ptr DRAW_AVAILABLE_PLACES2
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
mov byte ptr cursorState[bx],1
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
mov cursorState2[bx],1
jmp First_Loop
break1:
ISWHITEBYTE dummyData1,dummyData2
cmp bx,0
jne temp26
jmp break12
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
mov cursorState2[bx],1
jmp Second_Loop
break2:
ISWHITEBYTE dummyData1,dummyData2
cmp bx,0
jne temp27
jmp break22
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
mov cursorState2[bx],1
jmp Third_Loop
break3:
ISWHITEBYTE dummyData1,dummyData2
cmp bx,0
jne temp28
jmp break32
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
mov cursorState2[bx],1
jmp Fourth_Loop
break4:
ISWHITEBYTE dummyData1,dummyData2
cmp bx,0
jne temp29
jmp break42
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
mov cursorState2[bx],1
jmp First_Loop
break1:
ISWHITEBYTE dummyData1,dummyData2
cmp bx,0
jne temp26
jmp break12
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
mov cursorState2[bx],1
jmp Second_Loop
break2:
ISWHITEBYTE dummyData1,dummyData2
cmp bx,0
jne temp27
jmp break22
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
mov cursorState2[bx],1
jmp Third_Loop
break3:
ISWHITEBYTE dummyData1,dummyData2
cmp bx,0
jne temp28
jmp break32
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
mov cursorState2[bx],1
jmp Fourth_Loop
break4:
ISWHITEBYTE dummyData1,dummyData2
cmp bx,0
jne temp29
jmp break42
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
                        JE NOTVALID 

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
                        jne skp  ; jmp if black
                        popa

                        mov cursorState2[bx],1
                        jmp NOTVALID
                        
                        skp:
                          POPA
                          mov cursorState2[bx],0

                        NOTVALID:
                          CMP CX,0
                          JE RETURN
                          DEC CX
                          JMP LOOP1
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
JZ ITSVALID1
MOV DL,0
JMP CONTINUOUECHECK
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
JNZ CONTINUOUECHECK
;;;;;;;;;;;;;

MOV AX,WORD PTR X
MOV CX,WORD PTR Y
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

MOV AX,WORD PTR X
MOV CX,WORD PTR Y
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

MOV AX,WORD PTR X
MOV CX,WORD PTR Y
DEC AX
DEC CX
;;;;;;;;;;;;;
ISEMPTY AL,CL ;NOT EMPTY
MOV DH,0
CMP BX,0
JNZ CONTINUOUECHECK2
;;;;;;;;;;;;;;;;;;

MOV AX,WORD PTR X
MOV CX,WORD PTR Y
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
JNZ LEFTNOFOE
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
JZ ITSVALID3
JMP CANTMOVE
ITSVALID3:
;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;

MOV AX,WORD PTR X
MOV CX,WORD PTR Y
DEC AX
;;;;;;;;;;;;;;;;;
ISWHITE AX,CX
CMP BX,1
JNZ ITSVALID4
JMP CANTMOVE
ITSVALID4:
;;;;;;;;;;;;;;;;;

MOV AX,WORD PTR X
MOV CX,WORD PTR Y
DEC AX
;;;;;;;;;;;;;;;;;
ISEMPTY AL,CL
CMP BX,1
JZ CANMOVEFORWARD
;;ELSE
JMP CANTMOVE
CANMOVEFORWARD:
MOV AX,WORD PTR X
MOV CX,WORD PTR Y
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
MOV AX,WORD PTR X
MOV CX,WORD PTR Y
DEC AX
DEC AX
ISEMPTY AL,CL
CMP BX,1
JZ ITSVALID5
JMP ONEMOVEONLY
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
JZ ITSVALID1
MOV DL,0
JMP CONTINUOUECHECK
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
JNZ CONTINUOUECHECK
;;;;;;;;;;;;;

MOV AX,WORD PTR X
MOV CX,WORD PTR Y
INC AX
INC CX
;;;;;;;;;;;;;;
ISWHITE2 AX,CX
CMP BX,0
JZ CONTINUOUECHECK
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
JZ ITSVALID2
MOV DH,0
JMP CONTINUOUECHECK2
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
JNZ CONTINUOUECHECK2
;;;;;;;;;;;;;;;;;;

MOV AX,WORD PTR X
MOV CX,WORD PTR Y
INC AX
DEC CX
;;;;;;;;;;;;;;;;;;
ISWHITE2 AX,CX
CMP BX,0
JZ CONTINUOUECHECK2
MOV DH,1 ;LEFT HAS FOE
CONTINUOUECHECK2:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;RIGHT SIDE
CMP DL,1
JNZ RIGHTNOFOE
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
JNZ LEFTNOFOE
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
JZ ITSVALID3
JMP CANTMOVE
ITSVALID3:
;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;

MOV AX,WORD PTR X
MOV CX,WORD PTR Y
INC AX
;;;;;;;;;;;;;;;;;
ISWHITE2 AX,CX
CMP BX,0
JNZ ITSVALID4
JMP CANTMOVE
ITSVALID4:
;;;;;;;;;;;;;;;;;

MOV AX,WORD PTR X
MOV CX,WORD PTR Y
INC AX
;;;;;;;;;;;;;;;;;
ISEMPTY AL,CL
CMP BX,1
JZ CANMOVEFORWARD
;;ELSE
JMP CANTMOVE
CANMOVEFORWARD:
MOV AX,WORD PTR X
MOV CX,WORD PTR Y
GETARINDEX AX,CX 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;CHECK FIRST ROW;;;;;;;;;;;;;;;
CMP BX,16
JC SEEIFGREATERTAHN47OREQUAL
JMP ONEMOVEONLY

SEEIFGREATERTAHN47OREQUAL:
CMP BX,8
JNC TWOMOVES
JMP ONEMOVEONLY

TWOMOVES:
;;;;;;;;;;;;;;;;;SEE IF TWO IS EMPTY
MOV AX,WORD PTR X
MOV CX,WORD PTR Y
INC AX
INC AX
ISEMPTY AL,CL
CMP BX,1
JZ ITSVALID5
JMP ONEMOVEONLY
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
JZ CANTMOVE1
;;;;;;;;;;;;;
MOV AX,X
MOV CX,Y
DEC AX;;;;;;;;;;;;;;;FRONT STEP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;TO EAT FRONT 
ISEMPTY AL,CL ; EMPTY 1 IF NOT EMPETY 0
CMP BX,1
JNZ CHKIFWHITE1
JMP CANMOVE1
;;;;;;;;;;;;;;;;
CHKIFWHITE1:
MOV AX,X
MOV CX,Y
DEC AX;;;;;;;;;;;;;;;FRONT STEP
ISWHITE AX,CX ; WHITE 1 IF NOT WHITE
CMP BX,1
JZ CANTMOVE1
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
JZ CANTMOVE2
;;;;;;;;;;;;;
MOV AX,X
MOV CX,Y
DEC AX;;;;;;;;;;;;;;;FRONTRIGHT STEP
INC CX
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;TO EAT FRONT RIGHT
ISEMPTY AL,CL ; EMPTY 1 IF NOT EMPETY 0
CMP BX,1
JNZ CHKIFWHITE2
JMP CANMOVE2
;;;;;;;;;;;;;;;;
CHKIFWHITE2:
MOV AX,X
MOV CX,Y
DEC AX;;;;;;;;;;;;;;;FRONTRIGHT STEP
INC CX
ISWHITE AX,CX ; WHITE 1 IF NOT WHITE
CMP BX,1
JZ CANTMOVE2
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
JZ CANTMOVE3
;;;;;;;;;;;;;
MOV AX,X
MOV CX,Y
DEC AX;;;;;;;;;;;;;;;FRONTLEFT STEP
DEC CX
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;TO EAT FRONT LEFT
ISEMPTY AL,CL ; EMPTY 1 IF NOT EMPETY 0
CMP BX,1
JNZ CHKIFWHITE3
JMP CANMOVE3
;;;;;;;;;;;;;;;;
CHKIFWHITE3:
MOV AX,X
MOV CX,Y
DEC AX;;;;;;;;;;;;;;;FRONTLEFT STEP
DEC CX
ISWHITE AX,CX ; WHITE 1 IF NOT WHITE
CMP BX,1
JZ CANTMOVE3
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
JZ CANTMOVE4
;;;;;;;;;;;;;
MOV AX,X
MOV CX,Y
INC CX
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;TO EAT FRONT LEFT
ISEMPTY AL,CL ; EMPTY 1 IF NOT EMPETY 0
CMP BX,1
JNZ CHKIFWHITE4
JMP CANMOVE4
;;;;;;;;;;;;;;;;
CHKIFWHITE4:
MOV AX,X
MOV CX,Y
INC CX
ISWHITE AX,CX ; WHITE 1 IF NOT WHITE
CMP BX,1
JZ CANTMOVE4
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
JZ CANTMOVE5
;;;;;;;;;;;;;
MOV AX,X
MOV CX,Y
INC AX;;;;;;;;;;;;;;;FRONT STEP
INC CX
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;TO EAT FRONT 
ISEMPTY AL,CL ; EMPTY 1 IF NOT EMPETY 0
CMP BX,1
JNZ CHKIFWHITE5
JMP CANMOVE5
;;;;;;;;;;;;;;;;
CHKIFWHITE5:
MOV AX,X
MOV CX,Y
INC AX;;;;;;;;;;;;;;;FRONT STEP
INC CX
ISWHITE AX,CX ; WHITE 1 IF NOT WHITE
CMP BX,1
JZ CANTMOVE5
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
JZ CANTMOVE6
;;;;;;;;;;;;;
MOV AX,X
MOV CX,Y
INC AX
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;TO EAT FRONT RIGHT
ISEMPTY AL,CL ; EMPTY 1 IF NOT EMPETY 0
CMP BX,1
JNZ CHKIFWHITE6
JMP CANMOVE6
;;;;;;;;;;;;;;;;
CHKIFWHITE6:
MOV AX,X
MOV CX,Y
INC AX
ISWHITE AX,CX ; WHITE 1 IF NOT WHITE
CMP BX,1
JZ CANTMOVE6
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
JZ CANTMOVE7
;;;;;;;;;;;;;
MOV AX,X
MOV CX,Y
INC AX;;;;;;;;;;;;;;;FRONT STEP
DEC CX
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;TO EAT FRONT LEFT
ISEMPTY AL,CL ; EMPTY 1 IF NOT EMPETY 0
CMP BX,1
JNZ CHKIFWHITE7
JMP CANMOVE7
;;;;;;;;;;;;;;;;
CHKIFWHITE7:
MOV AX,X
MOV CX,Y
INC AX;;;;;;;;;;;;;;;FRONT STEP
DEC CX
ISWHITE AX,CX ; WHITE 1 IF NOT WHITE
CMP BX,1
JZ CANTMOVE7
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
JZ CANTMOVE8
;;;;;;;;;;;;;
MOV AX,X
MOV CX,Y
DEC CX
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;TO EAT FRONT LEFT
ISEMPTY AL,CL ; EMPTY 1 IF NOT EMPETY 0
CMP BX,1
JNZ CHKIFWHITE8
JMP CANMOVE8
;;;;;;;;;;;;;;;;
CHKIFWHITE8:
MOV AX,X
MOV CX,Y
DEC CX
ISWHITE AX,CX ; WHITE 1 IF NOT WHITE
CMP BX,1
JZ CANTMOVE8
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
JZ CANTMOVE1
;;;;;;;;;;;;;
MOV AX,WORD PTR X
MOV CX,WORD PTR Y
DEC AX;;;;;;;;;;;;;;;FRONT STEP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;TO EAT FRONT 
ISEMPTY AL,CL ; EMPTY 1 IF NOT EMPETY 0
CMP BX,1
JNZ CHKIFWHITE1
JMP CANMOVE1
;;;;;;;;;;;;;;;;
CHKIFWHITE1:
MOV AX,WORD PTR X
MOV CX,WORD PTR Y
DEC AX;;;;;;;;;;;;;;;FRONT STEP
ISWHITE2 AX,CX ; WHITE 1 IF NOT WHITE
CMP BX,0
JZ CANTMOVE1
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
JZ CANTMOVE2
;;;;;;;;;;;;;
MOV AX,WORD PTR X
MOV CX,WORD PTR Y
DEC AX;;;;;;;;;;;;;;;FRONTRIGHT STEP
INC CX
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;TO EAT FRONT RIGHT
ISEMPTY AL,CL ; EMPTY 1 IF NOT EMPETY 0
CMP BX,1
JNZ CHKIFWHITE2
JMP CANMOVE2
;;;;;;;;;;;;;;;;
CHKIFWHITE2:
MOV AX,WORD PTR X
MOV CX,WORD PTR Y
DEC AX;;;;;;;;;;;;;;;FRONTRIGHT STEP
INC CX
ISWHITE2 AX,CX ; WHITE 1 IF NOT WHITE
CMP BX,0
JZ CANTMOVE2
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
JZ CANTMOVE3
;;;;;;;;;;;;;
MOV AX,X
MOV CX,Y
DEC AX;;;;;;;;;;;;;;;FRONTLEFT STEP
DEC CX
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;TO EAT FRONT LEFT
ISEMPTY AL,CL ; EMPTY 1 IF NOT EMPETY 0
CMP BX,1
JNZ CHKIFWHITE3
JMP CANMOVE3
;;;;;;;;;;;;;;;;
CHKIFWHITE3:
MOV AX,X
MOV CX,Y
DEC AX;;;;;;;;;;;;;;;FRONTLEFT STEP
DEC CX
ISWHITE2 AX,CX ; WHITE 1 IF NOT WHITE
CMP BX,0
JZ CANTMOVE3
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
JZ CANTMOVE4
;;;;;;;;;;;;;
MOV AX,X
MOV CX,Y
INC CX
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;TO EAT FRONT LEFT
ISEMPTY AL,CL ; EMPTY 1 IF NOT EMPETY 0
CMP BX,1
JNZ CHKIFWHITE4
JMP CANMOVE4
;;;;;;;;;;;;;;;;
CHKIFWHITE4:
MOV AX,X
MOV CX,Y
INC CX
ISWHITE2 AX,CX ; WHITE 1 IF NOT WHITE
CMP BX,0
JZ CANTMOVE4
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
JZ CANTMOVE5
;;;;;;;;;;;;;
MOV AX,X
MOV CX,Y
INC AX;;;;;;;;;;;;;;;FRONT STEP
INC CX
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;TO EAT FRONT 
ISEMPTY AL,CL ; EMPTY 1 IF NOT EMPETY 0
CMP BX,1
JNZ CHKIFWHITE5
JMP CANMOVE5
;;;;;;;;;;;;;;;;
CHKIFWHITE5:
MOV AX,X
MOV CX,Y
INC AX;;;;;;;;;;;;;;;FRONT STEP
INC CX
ISWHITE2 AX,CX ; WHITE 1 IF NOT WHITE
CMP BX,0
JZ CANTMOVE5
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
JZ CANTMOVE6
;;;;;;;;;;;;;
MOV AX,X
MOV CX,Y
INC AX
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;TO EAT FRONT RIGHT
ISEMPTY AL,CL ; EMPTY 1 IF NOT EMPETY 0
CMP BX,1
JNZ CHKIFWHITE6
JMP CANMOVE6
;;;;;;;;;;;;;;;;
CHKIFWHITE6:
MOV AX,X
MOV CX,Y
INC AX
ISWHITE2 AX,CX ; WHITE 1 IF NOT WHITE
CMP BX,0
JZ CANTMOVE6
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
JZ CANTMOVE7
;;;;;;;;;;;;;
MOV AX,X
MOV CX,Y
INC AX;;;;;;;;;;;;;;;FRONT STEP
DEC CX
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;TO EAT FRONT LEFT
ISEMPTY AL,CL ; EMPTY 1 IF NOT EMPETY 0
CMP BX,1
JNZ CHKIFWHITE7
JMP CANMOVE7
;;;;;;;;;;;;;;;;
CHKIFWHITE7:
MOV AX,X
MOV CX,Y
INC AX;;;;;;;;;;;;;;;FRONT STEP
DEC CX
ISWHITE2 AX,CX ; WHITE 1 IF NOT WHITE
CMP BX,0
JZ CANTMOVE7
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
JZ CANTMOVE8
;;;;;;;;;;;;;
MOV AX,X
MOV CX,Y
DEC CX
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;TO EAT FRONT LEFT
ISEMPTY AL,CL ; EMPTY 1 IF NOT EMPETY 0
CMP BX,1
JNZ CHKIFWHITE8
JMP CANMOVE8
;;;;;;;;;;;;;;;;
CHKIFWHITE8:
MOV AX,X
MOV CX,Y
DEC CX
ISWHITE2 AX,CX ; WHITE 1 IF NOT WHITE
CMP BX,0
JZ CANTMOVE8
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
            local sendright
            local sendwrong

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
                  jmp SKIP
                blabla:

                cmp BYTE PTR gridState[bx],0
                jne blabla2
                  jmp blabla3
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
                JNE er
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

                  ;Sending a value

                  ;Check that Transmitter Holding Register is Empty
                  AGAIN:
                                mov         dx , 3FDH         ; Line Status Register

                                In          al , dx           ;Read Line Status
                                AND         al , 00100000b
                                JZ          AGAIN             ;jump untill it is empty

                  ;If empty put the VALUE in Transmit data register
                                GETARINDEX endRowCursor,endColCursor
                                CMP BYTE PTR cursorState[BX],0
                                jne sendright
                                mov         dx , 3F8H         ; Transmit data register
                                mov         al,127D               ;send wrong
                                out         dx , al
                                jmp sendwrong
                                sendright:
                        
                                mov         dx , 3F8H         ; Transmit data register
                                mov         al,bl               ;send right
                                out         dx , al
                                  
                                sendwrong:
                              ;------------------------------

                  PUSHA
                  call far ptr CLEAR_AVAILABLE_PLACES
                  POPA

                  DRAWWITHSOURCE       borderdata,borderwidth,borderheight,endRowCursor,endColCursor,150D,0D    ; col,row

                  mov bl,BYTE PTR stateOfQ
                  dec bl
                  mov BYTE PTR stateOfQ,bl
                  
                  FIRSTQHANDLE2M
                  
                  checkqhandle
                  CHECKMATE


ENDM SECONDQHANDLE

SECONDQHANDLE2 MACRO ;This Macro is Responsible for handling when second player press his second ENTER to move a selected piece to another cell
            LOCAL SKIP
            local blabla
            local blabla2
            local blabla3
            local er

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
                  jmp SKIP
                blabla:

                cmp gridState[bx],0
                jne blabla2
                  jmp blabla3
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
                JNE er
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
                  call far ptr CLEAR_AVAILABLE_PLACES2
                  POPA

                  ; DRAWWITHSOURCE       border2data,borderwidth,borderheight,endRowCursor2,endColCursor2,150D,0D    ; col,row

                  mov bl,BYTE PTR stateOfQ2
                  dec bl
                  mov BYTE PTR stateOfQ2,bl

                  FIRSTQHANDLEM

                  checkqhandle2   
                  CHECKMATE            
ENDM SECONDQHANDLE2

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
JZ CONTINUOUEGAME1
CMP BX,0
JNZ LOOPAGAIN1

MOV AL,1
MOV WINNER,AL ;;;;;;;;;;;if white wins print it as winner and wait for f4 key
  movecursor 37,30
  ShowMessage WINNERISWHITE
  not4f:
  mov ah,0
  int 16h
  cmp ah,3Eh;f4
  jnz not4f
JMP faraway
CONTINUOUEGAME1:

MOV BX, 64
LOOPAGAIN2:
DEC BX
CMP gridState[BX],12 ; WHITE KING
JZ CONTINUOUEGAME2
CMP BX,0
JNZ LOOPAGAIN2
MOV AL,2
MOV WINNER,AL
  movecursor 37,30
  ShowMessage WINNERISBLACK
  not4f2:
  mov ah,0
  int 16h
  cmp ah,3Eh;f4
  jnz not4f2
JMP faraway
CONTINUOUEGAME2:

ENDM CHECKMATE

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
  local chat
  local label102
  local receive

                  connect
                  INITIALIZEGRID   42H,06H                                                                ;0FH,08H
                  DrawGrid         150D,0D,colorState[1],colorState[0]
                  DrawPiecies      150D,0D
                  DRAWWITHSOURCE   borderdata,borderwidth,borderheight,curRowCursor,curColCursor,150D,0D
                  INITIALIZEGAMECHAT thename,thename
    cursorLoop:
                  PRINTCURRTIMER
                  CALL FAR PTR FREEZEPROC
                  mov         ah,01
                  int         16h
                  JNZ temp24
                  jmp          receive                 ;need to be changed to jump to recieve
                temp24:
                  mov         ah,0
                  int         16h

                  SEEIFRECIVING22:
                  ;if f4 is pressed return to main screen  
                  cmp ah,3Eh
                  jnz dontexit

                  ;Check that Transmitter Holding Register is Empty
                  mov dx , 3FDH         ; Line Status Register
                  In  al , dx           ;Read Line Status 
                  AND al , 00100000b
                  jz SEEIFRECIVING22             ;jump untill it is empty
                  ;If empty put the VALUE in Transmit data register
                  mov dx , 3F8H         ; Transmit data register
                  add ah,100d
                  mov al,AH   ; al,VALUE
                  out dx , al
                  jmp faraway  
                  dontexit: 

                  cmp ah,4FH ;enter key
                  jnz             tmplabel102
                  jmp qpressed
                  tmplabel102:

       cmp             ah,48h                              ;up
                  jnz             label62
                  jmp             up
    label62:

                  cmp             ah,4bh                              ;left
                  jnz             label72
                  jmp             left
    label72:

                  cmp             ah,4dh                              ;right
                  jnz             label82
                  jmp             right
    label82:

                  cmp             ah,50h                              ;down
                  jnz             label92
                  jmp             down
    label92:

    cmp             ah,127d                              ;chat
                  JG             label102
                  jmp             chat
    label102:      

                  jmp             receive                  

    left:
                  mov             dx,curColCursor
                  cmp             dx,0D
                  jnz             temp20

                  jmp             receive
    temp20:
 pusha
                  UPDATECELL     curRowCursor,curColCursor,150D,0D
                  popa
  pusha
                  ; DRAWWITHSOURCE       border2data,borderwidth,borderheight,curRowCursor2,curColCursor2,150D,0D    ; col,row
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
                  jz              label5

                  jmp             receive
    label5:

    right:
                  mov             dx,curColCursor
                  cmp             dx,7d
                  jnz             temp22
                  jmp             receive
    temp22:
  pusha
                  UPDATECELL     curRowCursor,curColCursor,150D,0D
                  popa
 pusha
                  ; DRAWWITHSOURCE       border2data,borderwidth,borderheight,curRowCursor2,curColCursor2,150D,0D    ; col,row
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
                  jmp             receive
    label4:

    up:
                  mov             dx,curRowCursor
                  cmp             dx,0D
                  jnz             label10
                  jmp             receive
    label10:

  pusha
                  UPDATECELL     curRowCursor,curColCursor,150D,0D
                  popa
 pusha
                  ; DRAWWITHSOURCE       border2data,borderwidth,borderheight,curRowCursor2,curColCursor2,150D,0D    ; col,row
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

                  jmp             receive
    label2:

    down:
                  mov             dx,curRowCursor
                  cmp             dx,7D
                  jnz             label11
                  jmp             receive
    label11:
                  pusha
                  UPDATECELL     curRowCursor,curColCursor,150D,0D
                  popa
 pusha
                  ; DRAWWITHSOURCE       border2data,borderwidth,borderheight,curRowCursor2,curColCursor2,150D,0D    ; col,row
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
                  jmp             receive
    label1:

    qpressed:
    mov bl,stateOfQ
    cmp bl,0
    jnz tmplabel20
    jmp firsrQ
    tmplabel20:

    SECONDQHANDLE
    jmp   receive   

    firsrQ:
    FIRSTQHANDLE
                 jmp             receive   
;--------------------------------------------------
    chat:
    ;TODO make chat send
        sendyou
    jmp             receive   
;--------------------------------------------------
receive:
;TODO make recieve chat and game
handlereceive
;--------------------------------------------------
jmp cursorLoop

ENDM CURSORMOV

handlereceive MACRO
  local quit
  local handleq
  local faultinsert
  local insertinq1
  local temp1
  local temp2
  local temp3
  local temp4
  LOCAL dontexit2
      ;Receiving a value       
                  ;Check that Data Ready
                  mov         dx , 3FDH         ; Line Status Register
          
                  in          al , dx
                  AND         al , 00000001b
                  jnz temp4
                  Jmp          quit               ;jump if no recive data
                  temp4:
    ;If Ready read the VALUE in Receive data register
                  mov         dx , 03F8H
                  in          al , dx
                  MOV              AH,0

                  ;if f4 is pressed return to main screen  2E
                  cmp al,0A2h ; +100
                  jnz dontexit2
                  jmp faraway  
                  dontexit2: 


                  
                  mov bl , al
                  and bl,10000000b
                  jnz temp3
                  jmp handleq
                  temp3:
                  ;print char TODO                  ;char recived then print it
                        pusha
                        movecursorWithPageNumber  p2cx,p2cy,1D    
                        popa

                        and al,01111111B
                        mov char,al
                        mov         ah,2       
                        mov dl,char
                        int         21h

                        mov al,p2cx
                        inc al
                        mov p2cx,al

                        mov         bl,char
                        cmp         bl,13
                        jne         afterenter

                        mov al,p2cy
                        inc al
                        mov p2cy,al

                        cmp         p2cy,24H          ;CURSOR CHECK
                        jne         afterenter

                        MOV         p2cy,23H
                        mov         p2cx,0H
                        pusha
                        movecursorWithPageNumber  p2cx,p2cy,1
                        popa
                        ShowMessage clear                 
                        afterenter:  
                  jmp quit
                  
                  handleq:
                  cmp received1, 127d
                  jne temp2
                  jmp insertinq1
                  temp2:
                  cmp al,127D
                  jne temp1
                  jmp faultinsert
                  temp1:
                  mov received2,al                    ;2 q recievied take action

                  mov al,63D                    ; convert (63 - recieved 1) to x and y
                  sub al,received1
                  
                  

             

                  MOV              CL , 8D
                  DIV              CL
                  mov curRowCursor2,al              ; mov them to current index 2
                  mov curColCursor2,ah
                  mov startRowCursor2,al              ; mov them to start index 2
                  mov startColCursor2,ah
                  FIRSTQHANDLE2                       ; call firstqhandle2

                  mov al,63D                          ; convert (63 - recieved 2) to x and y
                  sub al,received2
                  
                  MOV              AH,0
                  MOV              CL , 8D
                  DIV              CL
                  mov curRowCursor2,al                ; mov them to current index 2
                  mov curColCursor2,ah
                  SECONDQHANDLE2                      ; call sencondqhandle2
                  mov received1,127D                  ; reset received1 and received2 to 127D
                  mov received2,127D

                  faultinsert:
                  mov received1,127d                  ;q2 recievied fault then reset
                  mov received2,127d
                  jmp quit
                  insertinq1:
                  mov received1,al                  ;q1 recievied then store it
                  quit:
ENDM handlereceive

INITIALIZEGAMECHAT MACRO player1Name,player2Name
                  mov  al, 01h   ; select display page 1
                  mov  ah, 05h   ; function 05h: select active display page
                  int  10h

                  movecursorWithPageNumber  00,21H,1D
                  ShowMessage line

                  movecursorWithPageNumber  00,1FH,1D
                  ShowMessage player1Name+2

                  movecursorWithPageNumber  00,22H,1D
                  ShowMessage player2Name+2

                  mov         p2cx,00H
                  mov         p2cy,23H
                  mov         p1cx,00H
                  mov         p1cy,20H

                  movecursorWithPageNumber  00H,20H,1D
ENDM INITIALIZEGAMECHAT

sendyou MACRO
  local iwillsend
    local afterenter

    mov char,al

        ;Check that Transmitter Holding Register is Empty
    iwillsend:
                  mov         dx , 3FDH         ; Line Status Register

                  In          al , dx           ;Read Line Status
                  AND         al , 00100000b
                  JZ          iwillsend             ;jump untill it is empty

    ;If empty put the VALUE in Transmit data register
                  mov         dx , 3F8H         ; Transmit data register
                  mov cl,char
                  or  cl,10000000b
                  mov         al,cl
                  out         dx , al

                  pusha
                  movecursorWithPageNumber  p1cx,p1cy,1D    
                  popa

                  mov         ah,2       
                  mov dl,char
                  int         21h

                  mov al,p1cx
                  inc al
                  mov p1cx,al

                  mov         bl,char
                  cmp         bl,13
                  jne         afterenter

                  mov al,p1cy
                  inc al
                  mov p1cy,al

                  cmp         p1cy,21H          ;CURSOR CHECK
                  jne         afterenter

                  MOV         p1cy,20H
                  mov         p1cx,0H
                  pusha
                  movecursorWithPageNumber  p1cx,p1cy,1
                  popa
                  ShowMessage clear                 
                  afterenter:   
ENDM sendyou

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
JE BREAK8
BREAK7:
JMP BREAK6
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
jmp loop10
beforeloop10:
inc ah
cmp ah,8
je beforeloop9
jmp loop9
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
JE BREAK8
BREAK7:
JMP break6
BREAK8:
         pusha
        movecursorWithPageNumber 80d,31d,1D
        ShowMessage CHECKSTRING
        popa
break6:
popa
inc al
cmp al,8
je beforeloop10
jmp loop10
beforeloop10:
inc ah
cmp ah,8
je beforeloop9
jmp loop9
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

local white
local black
local black2
local white2

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

cmp senttf2,1         ;if player 0 11 -> 12 else 12->11
jne black2
mov gridState[3],4 ;white queen
mov gridState[4],5 ;white king
jmp white2
black2:
mov gridState[3],5 ;white king
mov gridState[4],4 ;white queen
white2:

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
cmp senttf2,1         ;if player 0 11 -> 12 else 12->11
jne black
mov gridState[59],11 ;white queen
mov gridState[60],12 ;white king
jmp white
black:
mov gridState[59],12 ;white king
mov gridState[60],11 ;white queen
white:
mov gridState[61],10 ;white bishop
mov gridState[62],9  ;white knight
mov gridState[63],8  ;white rook

ENDM INITIALIZEGRID

ISEMPTY MACRO x,y ;This Macro is of return type boolean and Responsible for checking if the cell is empty cell or contains piece
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
    JMP fistcheck
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

MAINMAINSERIAL MACRO player1Name,player2Name ;This Macro is Responsible for moving between screens when player press f2 or esc
    LOCAL check_for_anotherkey
    LOCAL check_for_f2
    LOCAL check_for_esc
    LOCAL LEAVERECIVING
    LOCAL NOTF1

    mov dx,3fbh           ; Line Control Register
    mov al,10000000b      ;Set Divisor Latch Access Bit
    out dx,al


    ;000c => 9600 baud rate
    ;Set LSB byte of the Baud Rate Divisor Latch
    mov dx,3f8h
    mov al,0ch
    out dx,al


    ;Set MSB byte of the Baud Rate Divisor Latch register.
    mov dx,3f9h
    mov al,00h
    out dx,al


    ;Set port configuration
    mov dx,3fbh
    mov al,00011011b      ;011=> even parity 0=> one stop bit 11=> 8bits
    out dx,al


    
    check_for_anotherkey:


;;;;;;;;;;;;;;;;;;;; GET KEY
    mov ah,1
    int 16h
    jz  SEEIFRECIVING ;IF YES
    mov ah,0
    int 16h ;STORE THE PRESSED KEY AT key-AH:scancode , AL:ASCII)
;;;;;;;;;;;;;;;;;;;; GET KEY

    cmp al,01Bh;esc ascii
    jz skipescchk
    JMP far ptr skippxx;;;;;;;;;;;;;;;;;;
    skipescchk:
    ;exist game
    MOV AH, 4CH
    MOV AL, 01 ;your return code.
    INT 21H
    skippxx:
    ;Sending KEY VALUE a value

    ;Check that Transmitter Holding Register is Empty
    mov dx , 3FDH         ; Line Status Register

    In  al , dx           ;Read Line Status 
    AND al , 00100000b
    jz SEEIFRECIVING             ;jump untill it is empty

    ;If empty put the VALUE in Transmit data register
    mov dx , 3F8H         ; Transmit data register
    mov al,AH   ; al,VALUE
    out dx , al
    ;JMP far ptr         check_for_anotherkey

SEEIFRECIVING:



    cmp ah,3bh;f1 scan code
    jz skipf2chk;;;;;;;;;;;;;;;;;;;;;;;;;;;
    JMP far ptr check_for_f2
    skipf2chk:
    ;open chat

    mov al,INVITE
    cmp al, 3
    jz addthree1
    mov INVITE , 3
    mov senttf1 ,1
    mov recivedf1,0
    JMP far ptr  movthree1
    addthree1:
    mov al ,recivedf1
    cmp al,1
    jnz movthree1
    add INVITE , 3
    movthree1:

    ;OPENCHAT player1Name+2,player2Name+2
    check_for_f2:


    cmp ah,3ch;f2 scane code
    jnz check_for_esc
    ;open game

    mov al,INVITE
    cmp al, 2
    jz addtwo1
    mov INVITE , 2
    mov senttf2,1
    mov recivedf2,0
    JMP far ptr  movtwo1
    addtwo1:
    mov al,recivedf2
    cmp al,1
    jnz movtwo1
    add INVITE , 2
    movtwo1:

    ;JMP far ptr play

    check_for_esc:

    mov al,INVITE
    cmp al,6
    jz chattt
    jmp seeifplay
    chattt:
    OPENCHAT player1Name+2,player2Name+2
    seeifplay:

    cmp al,4
    jz playyy
    jmp seeifout
    playyy:
    JMP far ptr play
    seeifout:

          ;Receiving a value     
    ;Check that Data Ready
    mov dx , 3FDH         ; Line Status Register
          
    in  al , dx
    AND al , 00000001b
    jnz llllll
    jmp far ptr LEAVERECIVING               ;jump IF NOT  it recive data
    llllll:
    ;If Ready read the VALUE in Receive data register
    mov dx , 03F8H
    in  al , dx
    mov AH , al

    ; SEE WHAT HAVE YOU RECIVED

    ;IF F1 PRINT INVATATION
    cmp ah,3bh;f1 scan code
    jz llx;;;;;;;;;;;;;;;;;;;;;;;;;;;
    JMP far ptr  NOTF1
    llx:

    mov al,INVITE
    cmp al, 3
    jz addthree
    mov INVITE , 3
    mov recivedf1,1
    mov senttf1,0
    JMP far ptr  movthree
    addthree:
    mov al,senttf1
    cmp al,1
    jnz movthree
    add INVITE , 3
    movthree:

    ;;;;;;;;;print message
    mov ah, 9
    mov dx, offset F1_INVITATION
    int 21h
;;;;;;;;;;;;;;;;;;;;;; FOR SCROLLING 
    mov ah, 6               
    mov al, 1               ; number of lines to scroll
    mov bh, 07               ; attribute
    mov ch, 18               ; row top
    mov cl, 0               ; col left
    mov dh, 25              ; row bottom
    mov dl, 80              ; col right
    int 10h
;;;;;;;;;;;;;;;;;;;;;; FOR SCROLLING 
    movecursorWithPageNumber 20,20,0
    MOV AH,0 
    NOTF1:


    ;IF F2 PRINT p1 WANTS TO PLAY
    cmp ah,3ch;f2 scan code
    jz lllxx;;;;;;;;;;;;;;;;;;;;;;;;;;;
    jmp far ptr NOTF2
    lllxx:

    mov al,INVITE
    cmp al, 2
    jz addtwo
    mov INVITE , 2
    mov recivedf2 ,1
    mov senttf2,0
    JMP far ptr  movtwo
    addtwo:
    mov al, senttf2
    cmp al, 1
    jnz movtwo
    add INVITE , 2
    movtwo:

    ;;;;;;;;;print message
    mov ah, 9
    mov dx, offset F2_INVITATION
    int 21h
;;;;;;;;;;;;;;;;;;;;;; FOR SCROLLING 
    mov ah, 6               
    mov al, 1               ; number of lines to scroll
    mov bh, 07               ; attribute
    mov ch, 18               ; row top
    mov cl, 0               ; col left
    mov dh, 25              ; row bottom
    mov dl, 80              ; col right
    int 10h
;;;;;;;;;;;;;;;;;;;;;; FOR SCROLLING 
    movecursorWithPageNumber 20,20,0
    MOV AH,0 
    NOTF2:
    LEAVERECIVING:

JMP far ptr         check_for_anotherkey

ENDM MAINMAINSERIAL

RESETMAINFLAGS MACRO F1,F2,F3,F4,F5
MOV F1,0
MOV F3,0
MOV F4,0
MOV F5,0
MOV F6,0
ENDM RESETMAINFLAGS

STATUSLINE MACRO ;This Macro is Responsible for the status bar that appears to players
  LOCAL LOOPXXXX
  LOCAL WHITEDIDNTWIN
  MOV BL,10
  ;;;;;;;;;;;;;;;;; --------- lined
  LOOPXXXX:
  movecursorWithPageNumber BL,17,0
  mov ah,2
  MOV DL,'-' 
  int 21h
  INC BL
  CMP BL,70
  JNZ LOOPXXXX
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  movecursorWithPageNumber 20,20,0

  CMP WINNER,1
  JNZ WHITEDIDNTWIN
  MOV AL,0
  MOV WINNER,AL
  ShowMessage WINNERISWHITE

  JMP BLACKIDNTWIN
  WHITEDIDNTWIN:

  CMP WINNER,2
  JNZ BLACKIDNTWIN
  MOV AL,0
  MOV WINNER,AL
  ShowMessage WINNERISBLACK
  
  BLACKIDNTWIN:
ENDM STATUSLINE

RECIVENAME MACRO PLAYERNAME1,PLAYERNAME2

            LOCAL dead2
            LOCAL mainloop2
            LOCAL afterenter
            local afterenter22
            LOCAL deadmid2
            local CHK2
            local AGAINx
            local skip1111x
            LOCAL skippppppppppppppppppppppppppppppp
            LOCAL outOfTheValidation
            LOCAL biggerthana
            LOCAL seeifbackspace
            LOCAL deletchartransimited
            LOCAL deletchar
            local skip111x
            LOCAL skip111xx
            LOCAL skip111xxx
            LOCAL skip111xxxxx
            LOCAL skip111xxxxxx
            LOCAL skip111xxxxxxx


    mov dx,3fbh           ; Line Control Register
    mov al,10000000b      ;Set Divisor Latch Access Bit
    out dx,al


    ;000c => 9600 baud rate
    ;Set LSB byte of the Baud Rate Divisor Latch
    mov dx,3f8h
    mov al,0ch
    out dx,al


    ;Set MSB byte of the Baud Rate Divisor Latch register.
    mov dx,3f9h
    mov al,00h
    out dx,al


    ;Set port configuration
    mov dx,3fbh
    mov al,00011011b      ;011=> even parity 0=> one stop bit 11=> 8bits
    out dx,al

    mov dh ,30
    mov dl,10
    push dx
    mov si,0
    mov di,0
    mainloop2:    
                  mov bl,NAMEFLAG
                  cmp bl,2
                  jnz skip111xxxxxxx
                  JMP far ptr   dead2
                  skip111xxxxxxx:
                


                  mov         ah,01
                  int         16h
                  jnz          skippppppppppppppppppppppppppppppp
                  JMP far ptr         AGAINx
                  skippppppppppppppppppppppppppppppp:
                  mov         ah,0
                  int         16h


                  mov         bl,al

                  ; ;;;;;;;;;;;;;;;;;;;;;;;; see if si greater than one
                  cmp         si,0
                  jnz         seeifbackspace
                  cmp         bl,08
                  jz mainloop2
                  seeifbackspace:


                  cmp         bl,08
                  jnz         skip111xxxx
                  jmp  far ptr deletchar
                  skip111xxxx:

                  ;;;;;;;;;;;;;;;;;;;;;;;; see if si is 15 stop adding name length
                  cmp         si,15
                  jc         skip111xxx
                  jmp  far ptr mainloop2
                  skip111xxx:


                  ;;;;;;;;;;;;;;;;;;;;;;;;; see if enter pressed;;;;;;;;;;;;
                  cmp         bl,13
                  jne         skip111xx
                  cmp si,0
                  jz skip111xx
                  jmp  far ptr afterenterx
                  skip111xx:


                  mov cl,122;;== z 
                  cmp cl,al;;if greater than z jmp
                  jc mainloop2
                  cmp al,65;;== A
                  jc mainloop2;;if less than A jmp
                  mov cl,90;;== Z 
                  cmp cl,al;;if greater than Z jmp
                  jc biggerthana
                  jmp outOfTheValidation
                  biggerthana:
                  cmp al,97;;== a
                  jc mainloop2;;if less than a jmp
                  outOfTheValidation:

                  pop         dx
                  push        dx
                  push        ax
                  movecursorWithPageNumber  dh,dl,0
                  pop         ax
                  pop         dx
                  inc         dh
                  push        dx

                  push        ax

                  mov         ah,2
                  mov         dl,al
                  int         21h

                  pop         ax
                  JMP far ptr  afterenterx

                  deletchar:
                  pop         dx
                  dec dh
                  push        dx
                  push        ax

                  movecursorWithPageNumber  dh,dl,0
                  pop         ax
                  pop         dx

                  push        dx
                  push        ax

                  mov         ah,2
                  mov         dl,0
                  int         21h

                  pop         ax


    afterenterx:   
    ;Sending a value

    ;Check that Transmitter Holding Register is Empty
                  mov         dx , 3FDH         ; Line Status Register

                  In          al , dx           ;Read Line Status
                  AND         al , 00100000b
                  JZ          AGAINx             ;jump untill it is empty

    ;If empty put the VALUE in Transmit data register

                  cmp         ah,13            ; enter key
                  jne          skip111x
                  jmp deadmid2
                  skip111x:

                  mov         dx , 3F8H         ; Transmit data register
                  mov         al,bl
                  out         dx , al
                  
                  cmp al,13
                  jnz skip111xxxxx
                  inc NAMEFLAG
                  skip111xxxxx:

                  cmp al,08
                  jz deletchartransimited
                  
                  mov PLAYERNAME1[si + 2],al
                  inc si
                  jmp         AGAINx

                  deletchartransimited:
                  dec si
                  mov PLAYERNAME1[si + 2],0
                  jmp         AGAINx
           
    ;Receiving a value
    deadmid2:      
                  jmp         dead2
                  
    AGAINx:        
    ;Check that Data Ready
                  mov         dx , 3FDH         ; Line Status Register
          
                  in          al , dx
                  AND         al , 00000001b
                  JZ          CHK2              ;jump untill it recive data

    ;If Ready read the VALUE in Receive data register
                  mov         dx , 03F8H
                  in          al , dx

                  cmp al,13
                  jnz skip111xxxxxx
                  inc NAMEFLAG
                  skip111xxxxxx:

                  cmp al,08
                  jz deletcharafterrecive
                  mov PLAYERNAME2[di + 2], al
                  inc di
                  JMP far ptr CHK2
                  deletcharafterrecive:

                  dec di
                  mov PLAYERNAME2[di + 2], 0

                  
    CHK2:          

                  jmp         mainloop2
    dead2:  

ENDM RECIVENAME

OPENCHAT MACRO player1Name,player2Name ;This Macro is Responsible for handling Game Chat
            LOCAL dead
            LOCAL mainloop
            LOCAL afterenter
            local afterenter2
            LOCAL deadmid
            local midh
            local CHK
            local AGAIN
            local skip1111
            local skip111
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

                  cmp         ah,3dh            ; F3 key
                  jne          skip111

                  mov         dx , 3F8H         ; Transmit data register
                  mov         al,ah
                  out         dx , al
                  jmp deadmid
                  skip111:

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
                  

                  cmp         al,3dh            ; F3 key
                  jne          skip1111
                  JMP far ptr  dead
                  skip1111:

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
                  RESETMAINFLAGS INVITE,senttf1,recivedf1,senttf2,recivedf2
                  ;;;;;;;;;;;;;;;;;;;;;; FOR SCROLLING 
                  mov ah, 6               
                  mov al, 0               ; number of lines to scroll
                  mov bh, 07               ; attribute
                  mov ch, 18               ; row top
                  mov cl, 0               ; col left
                  mov dh, 25              ; row bottom
                  mov dl, 80              ; col right
                  int 10h
                  ;;;;;;;;;;;;;;;;;;;;;; FOR SCROLLING 

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
    local white
    local black
    local temp1
    local temp2
    local temp3
    local temp4
    local temp5
    local temp6
    local temp7
    local temp8
    local temp9
    local temp10
    local temp11
    local temp12
    local temp13
    local temp14
    local temp15
    local temp16
    local temp17
    local temp18
    local temp19
    local temp20
    local temp21
    local temp22
    local temp23
    local temp24
    local temp25
    

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
    jne temp1
    jmp EMPTY2
    temp1:
    
    ; DEC AX 
    ; MOV BX,360D
    ; MUL BX

    ; ; LOADING THE IMG DATA 
    ; LEA SI,BROCKDATA
    ; ADD SI,AX

    ; MOV BX,SI
    ; JMP RETURN 

cmp senttf2,1  
je white
jmp black
white:
   CMP AX,1D
   jne temp2
   jmp B1
   temp2:
   CMP AX,2D
   jne temp3
   jmp B2
   temp3:
   CMP AX,3D
   jne temp4
   jmp B3
   temp4:
   CMP AX,4D
   jne temp5
   jmp B4
   temp5:
   CMP AX,5D
   jne temp6
   jmp B5
   temp6:
   CMP AX,6D
   jne temp7
   jmp B6
   temp7:
   CMP AX,7D
   jne temp8
   jmp B7
   temp8:
   CMP AX,8D
   jne temp9
   jmp B8
   temp9:
   CMP AX,9D
   jne temp10
   jmp B9
   temp10:
   CMP AX,10D
   jne temp11
   jmp B10
   temp11:
   CMP AX,11D
   jne temp12
   jmp B11
   temp12:
   CMP AX,12D
   jne temp13
   jmp B12
   temp13:
jmp EMPTY2

black:
   CMP AX,8D
   jne temp14
   jmp B1
   temp14:
   CMP AX,9D
   jne temp15
   jmp B2
   temp15:
   CMP AX,10D
   jne temp16
   jmp B3
   temp16:
   CMP AX,11D
   jne temp17
   jmp B4
   temp17:
   CMP AX,12D
   jne temp18
   jmp B5
   temp18:
   CMP AX,7D
   jne temp19
   jmp B6
   temp19:
   CMP AX,6D
   jne temp20
   jmp B7
   temp20:
   CMP AX,1D
   jne temp21
   jmp B8
   temp21:
   CMP AX,2D
   jne temp22
   jmp B9
   temp22:
   CMP AX,3D
   jne temp23
   jmp B10
   temp23:
   CMP AX,4D
   jne temp24
   jmp B11
   temp24:
   CMP AX,5D
   jne temp25
   jmp B12
   temp25:

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
JG WHITE
CMP CH , 0 
JG BLACK 

WHITE:
MOV BX,1 
JMP RETURN
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
JG WHITE
CMP CH , 0 
JG BLACK 

WHITE:
MOV BX,1 
JMP RETURN
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
                  movecursorWithPageNumber 0,0,1
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
                  JNE         NSP
                  
                  ShowMessage SPACE
    NSP:          
                  MOV         SEC[1],'$'
                  MOV         MIN[1],'$'

ENDM PRINTCURRTIMER

connect MACRO
  local notyet
  local temp4
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
    
                  notyet:
                  mov         dx , 3FDH         ; Line Status Register
                  in          al , dx
                  AND         al , 00000001b
                  jnz temp4
                  Jmp          notyet               ;jump if no recive data
                  temp4:
                  mov         dx , 03F8H
                  in          al , dx
  ENDM connect

.MODEL SMALL
.286
.STACK 64
;-----------
.Data
  nameq             db  'Please enter your name:','$'
  erroname          db  'Please write a valid name :','$'
  clear             db  '                                                                                                    ','$'
  line              db  '---------------------------------------------------','$'
  WINNERISBLACK     db  'winner is black','$'
  WINNERISWHITE     db  'winner is white','$'

  VAR               DB  '             ','$'

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
  thename           db  16,?,16 dup('$')
  theOthername      db  16,?,16 dup('$')                                                                                            ; max size 15 char last digit for $
  proceed           db  'Please Enter key to continue','$'
  op1               db  'To start chatting press F1','$'
  op2               db  'To start the game press F2','$'
  op3               db  'To end the program press ESC','$'
  F1_INVITATION     DB  'SENT YOU A CHAT INVITATION PRESS F1 TO PROCCED','$'
  F2_INVITATION     DB  'SENT YOU A GAME INVITATION PRESS F2 TO PROCCED','$'
  NAMEFLAG          DB  0
  INVITE            DB  0
  senttf1           DB  0
  recivedf1         DB  0
  senttf2           DB  0
  recivedf2         DB  0
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

  freezewidth       equ 60D
  freezeheight      equ 60D
  freezefilename    db  'freeze.bin',0
  freezefilehandle  DW  ?
  freezedata        db  freezewidth*freezeheight dup(0)

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

  received1         db  127d
  received2         db  127d
  
  CHECKSTRING       DB  'WARNING!!! :CHECK','$'

  p1cx              db  00H
  p1cy              db  20H
  p2cx              db  00H
  p2cy              db  23H

  char              db  ?
  ;---------------------------------------------------------------------------------------------------
 


.CODE
MAIN PROC FAR
  ;INITIALIZING
                          call                     far ptr       GETDATA
                          CALL                     far ptr      CLS
  ;OPENING AND READING AND CLOSING BIN FILES
                          OpenFile                 bbishopfilename, bbishopfilehandle
                          ReadData                 bbishopfilehandle ,bbishopwidth,bbishopheight,bbishopdata
                          CloseFile                bbishopfilehandle

                          OpenFile                 bkingfilename, bkingfilehandle
                          ReadData                 bkingfilehandle ,bkingwidth,bkingheight,bkingdata
                          CloseFile                bkingfilehandle

                          OpenFile                 bknightfilename, bknightfilehandle
                          ReadData                 bknightfilehandle ,bknightwidth,bknightheight,bknightdata
                          CloseFile                bknightfilehandle

                          OpenFile                 bpawnfilename, bpawnfilehandle
                          ReadData                 bpawnfilehandle ,bpawnwidth,bpawnheight,bpawndata
                          CloseFile                bpawnfilehandle

                          OpenFile                 bqueenfilename, bqueenfilehandle
                          ReadData                 bqueenfilehandle ,bqueenwidth,bqueenheight,bqueendata
                          CloseFile                bqueenfilehandle

                          OpenFile                 brockfilename, brockfilehandle
                          ReadData                 brockfilehandle ,brockwidth,brockheight,brockdata
                          CloseFile                brockfilehandle

  ;--white piecies----
                          OpenFile                 wbishopfilename, wbishopfilehandle
                          ReadData                 wbishopfilehandle ,wbishopwidth,bbishopheight,wbishopdata
                          CloseFile                wbishopfilehandle

                          OpenFile                 wkingfilename, wkingfilehandle
                          ReadData                 wkingfilehandle ,wkingwidth,wkingheight,wkingdata
                          CloseFile                wkingfilehandle

                          OpenFile                 wknightfilename, wknightfilehandle
                          ReadData                 wknightfilehandle ,wknightwidth,wknightheight,wknightdata
                          CloseFile                wknightfilehandle

                          OpenFile                 wpawnfilename, wpawnfilehandle
                          ReadData                 wpawnfilehandle ,wpawnwidth,wpawnheight,wpawndata
                          CloseFile                wpawnfilehandle

                          OpenFile                 wqueenfilename, wqueenfilehandle
                          ReadData                 wqueenfilehandle ,wqueenwidth,wqueenheight,wqueendata
                          CloseFile                wqueenfilehandle

                          OpenFile                 wrockfilename, wrockfilehandle
                          ReadData                 wrockfilehandle ,wrockwidth,wrockheight,wrockdata
                          CloseFile                wrockfilehandle

  ;--border-----
                          OpenFile                 borderfilename, borderfilehandle
                          ReadData                 borderfilehandle ,borderwidth,borderheight,borderdata
                          CloseFile                borderfilehandle

                          OpenFile                 border2filename, border2filehandle
                          ReadData                 border2filehandle ,border2width,border2height,border2data
                          CloseFile                border2filehandle

                          OpenFile                 selectfilename, selectfilehandle
                          ReadData                 selectfilehandle ,selectwidth,selectheight,selectdata
                          CloseFile                selectfilehandle

                          OpenFile                 select2filename, select2filehandle
                          ReadData                 select2filehandle ,select2width,select2height,select2data
                          CloseFile                select2filehandle

                          OpenFile                 freezefilename, freezefilehandle
                          ReadData                 freezefilehandle ,freezewidth,freezeheight,freezedata
                          CloseFile                freezefilehandle
  ;------------------------------------------------------------------------------------------------
  ;------------------------------------------------------------------------------------------------
  ;------------------------------------------------------------------------------------------------
  ;------------------------------------------------------------------------------------------------

  ;START MENU
                          movecursor               30d,7d
                          ShowMessage              nameq
                          movecursor               30d,10d
                          RECIVENAME               thename,theOthername
  ;validateName             nameq,thename,erroname
                          movecursor               30d,11d
                          ShowMessage              proceed
                          call                     far ptr       waitkey
                
  ;CHOICE MENU
  faraway:                

                          call                     far ptr     CLS
                          movecursor               17H,03H
                          ShowMessage              op1
                          movecursor               17H,08H
                          ShowMessage              op2
                          movecursor               17H,0DH
                          ShowMessage              op3
                          STATUSLINE
  ;MAINMAIN       thename,thename
  
                          RESETMAINFLAGS           INVITE,senttf1,recivedf1,senttf2,recivedf2
                          MAINMAINSERIAL           thename,theOthername
  ;GAME SCREEN
  play:                   
                          CALL                     far ptr       EnterGraphics
                          mov                      curColCursor,00h
                          mov                      curRowCursor,07h
                          mov                      curColCursor2,00h
                          mov                      curRowCursor2,00h
                          mov                      whiterow,0D
                          mov                      whitecol,0D
                          mov                      blackrow,0D
                          mov                      blackcol,10D

  ;---------------------
                          INITIALIZETIME
  curs:                   
                          CURSORMOV

                          JMP                      curs

                          EXT
MAIN ENDP
  ;----------------------------------------------------------------------------------------------------------------


  ;--------------------------------------------------Functions---------------------------------------------------------
GETDATA PROC    far                                                                                                       ;GET DATA
                          MOV                      AX,@DATA
                          MOV                      DS,AX
                          retf
GETDATA ENDP

CLS PROC far
                          MOV                      AX,0003H
                          INT                      10H
                          retf
CLS ENDP

EnterText PROC far                                                                                                        ;ENTER TEXT MODE
                          MOV                      AX,3H
                          INT                      10H
                          retf
EnterText ENDP

EnterGraphics PROC far                                                                                                    ;ENTER GRAPHICS MODE
                          MOV                      AX,4F02H
                          MOV                      BX,103H                                                                ;(800x600) pixel ;grid =480*480; char=60*60
                          INT                      10H
                          retf
EnterGraphics ENDP

waitkey PROC    far                                                                                                       ;wait for key
                          MOV                      AH , 0
                          INT                      16h
                          retf
waitkey ENDP

FREEZEPROC PROC   FAR                                                                                                     ;CHECKING THE FREEZE OF 3 SECONDS
       
                          mov                      al,0
                          mov                      ah,0
  Nloop9:                 
                          mov                      AL,0
  Nloop10:                
                          mov                      BYTE PTR  dummyData1,al
                          mov                      BYTE PTR  dummyData2,ah
                          pusha
                          GETARINDEXBYBYTE         dummyData1,dummyData2
                
  ; Code for checking the freeze  ----------------------------------------------------------
                          mov                      SI,BX
                 
                          GETTIME
                          mov                      ax,si
                          mov                      cl,2D
                          mul                      cl
                          mov                      si,ax
                          CMP                      word ptr timeState[si],0
                          JE                       LEAVEIT
                          dec                      BX
                          dec                      BX
                          dec                      BX
                          CMP                      word ptr timeState[si],BX
                 
                          Jle                      temp151
  ; U STILL IN FREEZE DUDE
                          pusha
                          DRAWWITHSOURCE           freezedata,borderwidth,borderheight,dummyData1,dummyData2,150D,0D
                          popa
  LEAVEIT:                
                          JMP                      nbreak6
  temp151:                
                          mov                      word ptr timeState[si],0D
                          popa
  ; Converting byte to word coz dummydata 1 , 2 are bytes
          
                          MOV                      BYTE PTR  dummyData3,0D
                          MOV                      BYTE PTR  dummyData4,0D
                          ADD                      BYTE PTR  dummyData3,al
                          ADD                      BYTE PTR  dummyData4,ah

                          PUSHA
                          PUSHA
                          UPDATECELL               dummyData3,dummyData4,150D,0D
                          POPA
                 
  ; ---------------------------------------------------------------------------------------------
  Nbreak6:                
                          popa
                          inc                      al
                          cmp                      al,8
                          je                       TMP2
                          JMP                      far ptr                   Nloop10
  TMP2:                   
                          inc                      ah
                          cmp                      ah,8
                          je                       TMP3
                          JMP                      far ptr          Nloop9
  TMP3:                   
  BREAK80:                
                          DRAWWITHSOURCE           borderdata,borderwidth,borderheight,curRowCursor,curColCursor,150D,0D
                          retf
FREEZEPROC ENDP

CLEAR_AVAILABLE_PLACES PROC FAR

                          mov                      al,0
                          mov                      ah,0
  Nloop91011:             
                          mov                      AL,0
  Nloop101112:            
                          mov                      BYTE PTR  dummyData1,al
                          mov                      BYTE PTR  dummyData2,ah
                          pusha
                          GETARINDEXBYBYTE         dummyData1,dummyData2
                          cmp                      BYTE PTR  cursorState[bx],0
                          JNE                      TMP123
                          JMP                      Nbreak678
  TMP123:                 
                          MOV                      BYTE PTR  cursorState[bx],0
                          popa
                          MOV                      BYTE PTR  dummyData3,0D
                          MOV                      BYTE PTR  dummyData4,0D
                          ADD                      BYTE PTR  dummyData3,al
                          ADD                      BYTE PTR  dummyData4,ah
                          pusha
                          UPDATECELL               dummyData3,dummyData4,150D,0D
                  
  Nbreak678:              
                          popa
                          inc                      al
                          cmp                      al,8
                          JE                       TMP245
                          JMP                      Nloop101112
  TMP245:                 
                          inc                      ah
                          cmp                      ah,8
                          JE                       TMP345
                          JMP                      Nloop91011
  TMP345:                 

                          DRAWWITHSOURCE           borderdata,borderwidth,borderheight,curRowCursor,curColCursor,150D,0D
  ; DRAWWITHSOURCE           border2data,borderwidth,borderheight,curRowCursor2,curColCursor2,150D,0D
                          call                     far ptr DRAW_AVAILABLE_PLACES2
                          retf
CLEAR_AVAILABLE_PLACES ENDP

CLEAR_AVAILABLE_PLACES2 PROC far
                          mov                      al,0
                          mov                      ah,0
  Nloop9zz:               
                          mov                      AL,0
  Nloop10zz:              
                          mov                      BYTE PTR  dummyData1,al
                          mov                      BYTE PTR  dummyData2,ah
                          pusha
                          GETARINDEXBYBYTE         dummyData1,dummyData2
                          cmp                      BYTE PTR  cursorState2[bx],0
                          JNE                      TMP
                          JMP                      Nbreak6zz
  TMP:                    
                          MOV                      BYTE PTR  cursorState2[bx],0
                          popa
                          MOV                      BYTE PTR  dummyData3,0D
                          MOV                      BYTE PTR  dummyData4,0D
                          ADD                      BYTE PTR  dummyData3,al
                          ADD                      BYTE PTR   dummyData4,ah
                          pusha
                          UPDATECELL               dummyData3,dummyData4,150D,0D
                  
  Nbreak6zz:              
                          popa
                          inc                      al
                          cmp                      al,8
                          JE                       TMP2zz
                          JMP                      Nloop10zz
  TMP2zz:                 
                          inc                      ah
                          cmp                      ah,8
                          JE                       TMP3zz
                          JMP                      Nloop9zz
  TMP3zz:                 
                          DRAWWITHSOURCE           borderdata,borderwidth,borderheight,curRowCursor,curColCursor,150D,0D
  ; DRAWWITHSOURCE           border2data,borderwidth,borderheight,curRowCursor2,curColCursor2,150D,0D
                          call                     far ptr DRAW_AVAILABLE_PLACES
                          retf
CLEAR_AVAILABLE_PLACES2 ENDP

DRAW_AVAILABLE_PLACES PROC far

                          mov                      al,0
                          mov                      ah,0
  loop9dd:                
                          mov                      al,0
  loop10dd:               
                          mov                      BYTE PTR dummyData1,al
                          mov                      BYTE PTR dummyData2,ah
                          pusha
                          GETARINDEXBYBYTE         dummyData1,dummyData2
                          cmp                      BYTE PTR cursorState[bx],0
                          je                       BREAK7dd
                          popa
                          mov                      BYTE PTR dummyData1,al
                          mov                      BYTE PTR dummyData2,ah
                          pusha
                          DRAWWITHSOURCE           selectdata,selectwidth,selectheight,dummyData1,dummyData2,150D,0D
                          popa
                          PUSHA
                          GETARINDEXBYBYTE         dummyData1,dummyData2
                          CMP                      BYTE PTR gridState[BX],5D
                          JE                       BREAK8dd
  BREAK7dd:               
                          JMP                      BREAK6dd
  BREAK8dd:               
                          pusha
                          movecursorWithPageNumber 2d,31d,1D
                          ShowMessage              CHECKSTRING
                          popa
  break6dd:               
                          popa
                          inc                      al
                          cmp                      al,8
                          je                       beforeloop10dd
                          jmp                      loop10dd
  beforeloop10dd:         
                          inc                      ah
                          cmp                      ah,8
                          je                       beforeloop9dd
                          jmp                      loop9dd
  beforeloop9dd:          
                          retf
DRAW_AVAILABLE_PLACES ENDP

DRAW_AVAILABLE_PLACES2 PROC far

                          mov                      al,0
                          mov                      ah,0
  loop9ee:                
                          mov                      al,0
  loop10ee:               
                          mov                      BYTE PTR dummyData1,al
                          mov                      BYTE PTR dummyData2,ah
                          pusha
                          GETARINDEXBYBYTE         dummyData1,dummyData2
                          cmp                      BYTE PTR cursorState2[bx],0
                          je                       BREAK7ee
                          popa
                          mov                      BYTE PTR dummyData1,al
                          mov                      BYTE PTR dummyData2,ah
                          pusha
                          DRAWWITHSOURCE           select2data,select2width,select2height,dummyData1,dummyData2,150D,0D
                          popa
                          PUSHA
                          GETARINDEXBYBYTE         dummyData1,dummyData2
                          CMP                      BYTE PTR gridState[BX],12D
                          JE                       BREAK8ee
  BREAK7ee:               
                          JMP                      break6ee
  BREAK8ee:               
                          pusha
                          movecursorWithPageNumber 80d,31d,1D
                          ShowMessage              CHECKSTRING
                          popa
  break6ee:               
                          popa
                          inc                      al
                          cmp                      al,8
                          je                       beforeloop10ee
                          jmp                      loop10ee
  beforeloop10ee:         
                          inc                      ah
                          cmp                      ah,8
                          je                       beforeloop9ee
                          jmp                      loop9ee
  beforeloop9ee:          

                          retf
DRAW_AVAILABLE_PLACES2 ENDP



END MAIN

;http://www.wagemakers.be/english/doc/vga