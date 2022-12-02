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

DRAW MACRO        ;DRAW IMAGE
                ADD  DI,-64D ; dump fixing
                MOV AX,0A000H
                MOV ES,AX
                MOV  DX,imgheight
                
                CMP CX,0
                JE SKIP
                XLP:
                ADD DI,320D
                DEC CX
                JNZ XLP
                SKIP:

                LEA BX,imgdata
        ; Drawing loop
        drawLoop:     
                      MOV  CX,imgwidth
                      
                      innerloop:
                      MOV  AL,0FH;[BX]
                      STOSB       
                      INC BX
                      DEC CX
                      JNZ innerloop
                      
                      ADD DI,320D  
                      SUB DI,imgwidth
                      DEC DX
                      JNZ  drawLoop
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
                      CALL EnterGraphics
                      CALL OpenFile
                      CALL ReadData
        ;-----------------------------------------
                      MOV  CX,20D                       ;ROW
                      ADD  DI,20D                       ;COL
                      DRAW

        ;-----------------------------------------
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
                      MOV  AX,13H                       ;(320*200) pixel
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