;-----------------------------------------MACROS-------------------------------------------------------------
EXT MACRO ;PRESS ANY KEY TO EXIT APPLICATION
                MOV  AH , 0
                INT  16h
                MOV         AH,4CH
                INT         21H
                ENDM        EXT

ShowMessage MACRO MyMessage ;PRINT STRING
                MOV         AH,9H
                MOV         DX,offset MyMessage
                INT         21H
                ENDM        ShowMessage

ShowCMessage MACRO MyMessage ;X ;PRINT STRING WITH COLOR X
                local clp 
                LOCAL otc
                mov SI,offset MyMessage
                
           clp: 
                MOV AL,[SI]
                cmp al,'$'
                je otc
                mov ah,9 ;Display
                mov bh,0 ;Page 0
                mov cx,1h ;1 times
                ; mov bl,X ;Green (A) on white(F) background
                int 10h
                inc SI
                
                cmp [si],'$'
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

DRAW MACRO        ;DRAW IMAGE
                LOCAL drawLoop
                LOCAL innerloop
                
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

.MODEL SMALL
.STACK 64
;-----------
.Data
        imgwidth      equ 26D
        imgheight     equ 25D

        imgfilename   db  'myking.bin',0
        imgfilehandle DW  ?
        imgdata       db  imgwidth*imgheight dup(0)
        ;-----------------------------


.CODE
MAIN PROC FAR
                      call GETDATA
                      CALL CLS
                      CALL OpenFile
                      CALL ReadData
                      CALL EnterGraphics
        ;-----------------------------------------
                      MOV  CX,0D                        ;COL
                      MOV  DX,0D                        ;ROW
                      DRAW

        ;------------------------------------------------------------------------------
                      call CloseFile
                      EXT
MAIN ENDP


        ;--------------------------------------------------Functions---------------------------------------------------------
GETDATA PROC                                            ;GET DATA
                      MOV  AX,@DATA
                      MOV  DS,AX
                      ret
GETDATA ENDP

CLS PROC                                                ;CLEAR SCREEN
                      MOV  AX,0003H
                      INT  10H
                      ret
CLS ENDP

EnterGraphics PROC                                      ;ENTER GRAPHICS MODE
                      MOV  AX,4F02H
                      MOV  BX,107H                      ;105(1024*768) pixel
                      INT  10H
                      ret
EnterGraphics ENDP

OpenFile PROC                                           ;OPEN FILE
                      MOV  AH, 3Dh
                      MOV  AL, 0                        ; read only
                      LEA  DX, imgfilename              ;GET NAME
                      INT  21h
                      MOV  [imgfilehandle], AX          ;GET HANDLE OF THE FILE
                      RET
OpenFile ENDP

ReadData PROC                                           ;READ FILE CONTENT
                      MOV  AH,3Fh
                      MOV  BX, [imgfilehandle]
                      MOV  CX,imgwidth*imgheight        ; number of bytes to read
                      LEA  DX, imgdata                  ;PUT DATA IN IMGDATA
                      INT  21h
                      RET
ReadData ENDP

CloseFile PROC                                          ;CLOSE FILE
                      MOV  AH, 3Eh
                      MOV  BX, [imgfilehandle]
                      INT  21h
                      RET
CloseFile ENDP

END MAIN