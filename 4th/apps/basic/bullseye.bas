5 REM PRINT TAB(32);"BULLSEYE"
10 REM PRINT TAB(15);"CREATIVE COMPUTING  MORRISTOWN, NEW JERSEY"
20 REM PRINT:PRINT:PRINT
30 PRINT "IN THIS GAME, UP TO 20 PLAYERS THROW DARTS AT A TARGET"
40 PRINT "WITH 10, 20, 30, AND 40 POINT ZONES.  THE OBJECTIVE IS"
50 PRINT "TO GET 200 POINTS.": PRINT
60 PRINT "THROW DESCRIPTION          PROBABLE SCORE"
65 PRINT "===== ===========          =============="
70 PRINT "  1 : FAST OVERARM       = BULLSEYE OR COMPLETE MISS"
80 PRINT "  2 : CONTROLLED OVERARM = 10, 20 OR 30 POINTS"
90 PRINT "  3 : UNDERARM           = ANYTHING":PRINT
100 M=0: R=0: FOR I=1 TO 20: @(I)=0: NEXT I : REM DIM A$(20),S(20),W(10):
110 INPUT "HOW MANY PLAYERS: ";N: PRINT
120 IF N<2 + N>20 THEN GOTO 110
150 R=R+1: PRINT "ROUND ";R:PRINT "--------"
160 FOR I=1 TO N
170 PRINT "PLAYER ";I;" THROWS";: INPUT T
180 IF (T<1) + (T>3) THEN PRINT "INPUT 1, 2, OR 3!": GOTO 170
190 GOTO 190 +(T*10)
200 P=65: Q=55: S=50: T=45: GOTO 230
210 P=99: Q=77: S=43: T=1: GOTO 230
220 P=95: Q=75: S=45: T=5
230 U=RND(100)
240 IF U+1>P THEN PRINT "BULLSEYE!!  40 POINTS!":B=40: GOTO 290
250 IF U+1>Q THEN PRINT "30-POINT ZONE!":B=30: GOTO 290
260 IF U+1>S THEN PRINT "20-POINT ZONE":B=20: GOTO 290
270 IF U+1>T THEN PRINT "WHEW!  10 POINTS.":B=10: GOTO 290
280 PRINT "MISSED THE TARGET!  TOO BAD.": B=0
290 @(I)=@(I)+B: PRINT "TOTAL SCORE = ";@(I): PRINT : NEXT I
300 FOR I=1 TO N
310 IF @(I)+1>200 THEN M=M+1: @(M+20)=I
320 NEXT I
330 IF M=0 THEN GOTO 150
340 PRINT "WE HAVE A WINNER!!": PRINT
350 FOR I=1 TO M: PRINT "PLAYER ";@(I+20);" SCORED ";@(@(I+20));" POINTS.": NEXT I
360 PRINT: PRINT "THANKS FOR THE GAME.": END