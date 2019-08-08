100 INPUT "NUMBER TO BE FACTORED: ";N
110 PRINT
120 IF N = 0 THEN GOTO 999
130 IF N = 2 THEN 250
140 LET Z = 0
150 LET F = 2
160 PUSH N,0 : GOSUB _SQRT : LET O = POP()
180 GOSUB 300
185 IF N = 1 THEN 500
190 FOR F = 3 TO O+1 STEP 2
200     GOSUB 300
205     IF N = 1 THEN 500
210 NEXT F
220 IF Z = 0 THEN 250
230 PRINT N;
240 GOTO 500
250 PRINT N; " IS PRIME."
260 GOTO 500
300 REM TESTS F AS A FACTOR.
310 IF (N%F) # 0 THEN 400
330 IF Z > 0 THEN GOTO 370
340 PRINT N; " HAS THE FACTORS: "
350 PRINT "      "
360 LET Z = 1
370 PRINT F,
380 LET N = N/F
390 IF N # 1 THEN 310
400 RETURN
500 REM ALL DONE.
510 PRINT
520 PRINT
530 PRINT
540 GOTO 100
999 END

_SQRT                                  ' This is an integer SQR subroutine.
  Local (1)

  Push ((10^(Pop()*2)) * Pop())        ' Output is scaled by 10^(TOS()).
  a@ = Tos()

  Do
    Push (a@ + (Tos()/a@))/2

    If Abs(a@ - Tos()) < 2 Then
       a@ = Pop()
       If Pop() Then
          Push a@
          Break
       EndIf
    EndIf

    a@ = Pop()
  Loop

Return
