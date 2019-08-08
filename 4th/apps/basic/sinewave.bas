     REM Original program by David Ahl, Creative Computing, 1978
     REM uBasic version by J.L. Bezemer, 2011,2014
     REM Based on an integer 4tH version
     REM ***
     LET Z=0 : FOR X=0 TO 160
     FOR Y=1 TO ((FUNC(_SIN(X*2500))*25)+260000)/10000 : PRINT " "; : NEXT Y
     IF Z THEN PRINT "J.L. Bezemer"
     IF Z=0 THEN PRINT "4tH compiler"
     LET Z=Z=0 : NEXT X : END
_SIN PARAM(1) : PUSH A@ : LET A@=TOS()<0 : PUSH ABS(POP()%62832)
     IF TOS()>31416 THEN A@=A@=0 : PUSH POP()-31416
     IF TOS()>15708 THEN PUSH 31416-POP()
     PUSH (TOS()*TOS())/10000 : PUSH 10000+((10000*-(TOS()/72))/10000)
     PUSH 10000+((POP()*-(TOS()/42))/10000) : PUSH 10000+((POP()*-(TOS()/20))/10000)
     PUSH 10000+((POP()*-(POP()/6))/10000)  : PUSH (POP()*POP())/10000 
     IF A@ THEN PUSH -POP()
     RETURN
     REM ** This is an integer SIN subroutine. Input and output are scaled by 10K.
_COS PARAM(1) : PUSH ABS(A@%62832) : IF TOS()>31416 THEN PUSH 62832-POP()
     LET A@=TOS()>15708 : IF A@ THEN PUSH 31416-POP()
     PUSH TOS() : PUSH (POP()*POP())/10000 : PUSH 10000+((10000*-(TOS()/56))/10000)
     PUSH 10000+((POP()*-(TOS()/30))/10000): PUSH 10000+((POP()*-(TOS()/12))/10000)
     PUSH 10000+((POP()*-(POP()/2))/10000) : IF A@ THEN PUSH -POP()
     RETURN
     REM ** This is an integer COS subroutine. Input and output are scaled by 10K.
_TAN PARAM(1) : RETURN ((FUNC(_SIN(A@))*10000)/FUNC(_COS(A@)))
     REM ** This is an integer TAN subroutine. Input and output are scaled by 10K.


