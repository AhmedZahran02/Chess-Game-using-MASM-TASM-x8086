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

                  GETARINDEX curRowCursor,curColCursor
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

ENDM FIRSTQHANDLE

SECONDQHANDLE MACRO

                  GETARINDEX startRowCursor,startColCursor

                  MOV cl,BYTE PTR cellColorState
                  mov BYTE PTR colorState[bx],cl
                  mov cl,BYTE PTR curRowCursor
                  mov ch,BYTE PTR curColCursor
                  mov BYTE PTR endRowCursor,cl
                  mov BYTE PTR endColCursor,ch
                  mov cx,bx
                  GETARINDEX endRowCursor,endColCursor
                  mov si,cx
                  mov dh,BYTE PTR gridState[si]
                  mov gridState[si],0
                  mov si,bx
                  mov gridState[si],dh
                  pusha
                  UPDATECELL     startRowCursor,startColCursor,150D,0D
                  popa

                  pusha
                  UPDATECELL     endRowCursor,endColCursor,150D,0D
                  popa
                  DRAWWITHSOURCE       borderdata,borderwidth,borderheight,curRowCursor,curColCursor,150D,0D    ; col,row


                  mov bl,BYTE PTR stateOfQ
                  dec bl
                  mov BYTE PTR stateOfQ,bl

ENDM SECONDQHANDLE

CURSORMOV MACRO 
  LOCAL cursorLoop
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
  

cursorLoop:


                  mov             ah,0
                  int             16h

                  cmp ah,10h
                  jnz             tmplabel10
                  jmp qpressed
tmplabel10:
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

ENDM CURSORMOV

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

mov gridState[8],6 ;black pawn
mov gridState[9],6 ;black pawn
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

    call CLS


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

GETARINDEX MACRO X,Y ;OUTPUT IN BX
    MOV AX,X 
    MOV BL , 8D
    MUL BL 
    ADD AX , Y
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

.MODEL SMALL
.286
.STACK 64
;-----------
.Data
    nameq             db  'Please enter your name:','$'
    erroname          db  'Please write a valid name :','$'
    
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
    
    
    

    thename           db  16,?,16 dup('$')                      ; max size 15 char last digit for $
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
    borderwidth       equ 60D
    borderheight      equ 60D
    borderfilename    db  'border.bin',0
    borderfilehandle  DW  ?
    borderdata        db  borderwidth*borderheight dup(0)

    gridState         db  64  dup(0)
    colorState        db  64  dup(0)
    
    curRowCursor      dw  0
    curColCursor      dw  0

    startRowCursor    dw  0
    startColCursor    dw  0

    endRowCursor      dw  0
    endColCursor      dw  0

    cellColorState    db  0

    stateOfQ          db  0
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
    ;------------------------------------------------------------------------------------------------
    ;--------INITIAL GRID-----------------
    start:        
                  INITIALIZEGRID 0FH,08H
    ;------------------------------------------------------------------------------------------------
    ;------------------------------------------------------------------------------------------------
    ;------------------------------------------------------------------------------------------------

    ;START MENU
                  validateName   nameq,thename,erroname                                       ;Veryyyyyyyyyyyyyyyy STABLE
                  movecursor     17H,0AH
                  ShowMessage    proceed
                  call           waitkey
    ;CHOICE MENU
                  call           CLS
                  movecursor     17H,03H
                  ShowMessage    op1
                  movecursor     17H,08H
                  ShowMessage    op2
                  movecursor     17H,0DH
                  ShowMessage    op3
                  call           waitkey
    ;GAME SCREEN
                  CALL           EnterGraphics
                  DrawGrid       150D,0D,colorState[1],colorState[0]
                  DrawPiecies    150D,0D

                  DRAWWITHSOURCE borderdata,borderwidth,borderheight,0D,0D,150D,0D            ; col,row

                  CURSORMOV
   
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
GETDATA PROC                                                                                  ;GET DATA
                  MOV            AX,@DATA
                  MOV            DS,AX
                  ret
GETDATA ENDP

CLS PROC                                                                                      ;CLEAR SCREEN
                  MOV            AX,0003H                                                     ;;ah == 0 set to graph mod the al = 3 return to text mode
                  INT            10H
                  ret
CLS ENDP

EnterText PROC                                                                                ;ENTER TEXT MODE
                  MOV            AX,3H
                  INT            10H
                  ret
EnterText ENDP

EnterGraphics PROC                                                                            ;ENTER GRAPHICS MODE
                  MOV            AX,4F02H
                  MOV            BX,103H                                                      ;(800x600) pixel ;grid =480*480; char=60*60
                  INT            10H
                  ret
EnterGraphics ENDP

waitkey PROC                                                                                  ;wait for key
                  MOV            AH , 0
                  INT            16h
                  ret
waitkey ENDP

END MAIN

;http://www.wagemakers.be/english/doc/vga
