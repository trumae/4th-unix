5 REM "AWARI"
7 REM "CREATIVE COMPUTING  MORRISTOWN, NEW JERSEY"
10 REM How to play: http://awari.cs.vu.nl/awari/rules.html
15 N = 0 : REM DIM B(13),G(13),F(50)
20 PRINT:PRINT:E=0
25 FOR I=0 TO 12:@(I)=3:NEXT I
30 C=0:@(N+40)=0:@(13)=0:@(6)=0
35 GOSUB 500
40 GOSUB 110
45 IF E=0 THEN GOTO 80
50 IF M=H THEN GOSUB 100
55 IF E=0 THEN GOTO 80
60 PRINT "MY MOVE IS ";:GOSUB 800
65 IF E=0 THEN GOTO 80
70 IF M=H THEN PRINT ",";:GOSUB 800
75 IF E>0 THEN PRINT: GOTO 35
80 PRINT:PRINT "GAME OVER"
85 D=@(6)-@(13):IF D<0 THEN PRINT "I WIN BY ";-D;" POINTS":GOTO 20
90 N=N+1:IF D=0 THEN PRINT "DRAWN GAME":GOTO 20
95 PRINT "YOU WIN BY ";D;" POINTS":GOTO 20
100 PRINT "AGAIN, ";
110 INPUT "YOUR MOVE ";M:IF M<7 THEN IF M>0 THEN M=M-1:GOTO 130
120 PRINT "ILLEGAL MOVE":GOTO 100
130 IF @(M)=0 THEN GOTO 120
140 H=6:GOSUB 200
150 GOTO 500
200 K=M:GOSUB 600
205 E=0:IF K>6 THEN K=K-7
210 C=C+1:IF C<9 THEN @(N+40)=@(N+40)*6+K
212 I=0
215 IF @(I)#0 THEN GOTO 230
220 I=I+1 : IF I<6 GOTO 215
225 RETURN
230 I=7
233 IF @(I)#0 THEN E=1 : RETURN
235 I=I+1 : IF I<13 THEN GOTO 233
240 RETURN
500 PRINT:PRINT "   ";
505 I=12
507 GOSUB 580
510 I=I-1 : IF I>6 GOTO 507 
515 PRINT:I=13:GOSUB 580
520 PRINT "                       ";:PRINT @(6):PRINT "   ";
525 FOR I=0 TO 5:GOSUB 580
530 NEXT I
535 PRINT:PRINT:RETURN
580 IF @(I)<10 THEN PRINT " ";
585 PRINT @(I);:RETURN
600 P=@(M):@(M)=0
605 FOR P=P TO 1 STEP -1:M=M+1:IF M>13 THEN M=M-14
610 @(M)=@(M)+1:NEXT P
615 IF @(M)=1 THEN IF M#6 THEN IF M#13 THEN IF @(12-M)#0 THEN GOTO 625
620 RETURN
625 @(H)=@(H)+@(12-M)+1:@(M)=0:@(12-M)=0:RETURN
800 D=-99:H=13
805 FOR I=0 TO 13:@(I+20)=@(I):NEXT I
810 FOR J=7 TO 12:IF @(J)=0 THEN GOTO 885
815 G=0:M=J:GOSUB 600
820 FOR I=0 TO 5:IF @(I)=0 THEN GOTO 845
825 L=@(I)+I:R=0
830 IF L>13 THEN L=L-14:R=1:GOTO 830
835 IF @(L)=0 THEN IF L#6 THEN IF L#13 THEN R=@(12-L)+R
840 IF R>Q THEN Q=R
845 NEXT I
850 Q=@(13)-@(6)-Q:IF C>8 THEN GOTO 875
855 K=J:IF K>6 THEN K=K-7
860 FOR I=0 TO N-1: IF @(N+40)*6+K=((@(I+40)*10)/6^(7-C)+1)/10 THEN Q=Q-2
870 NEXT I
875 FOR I=0 TO 13:@(I)=@(I+20):NEXT I
880 IF Q+1>D THEN A=J:D=Q
885 NEXT J
890 M=A:PRINT M-6;:GOTO 200
900 FOR I=0 TO N-1:PRINT @(I):NEXT I
999 END