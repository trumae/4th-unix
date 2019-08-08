REM
REM Demo implementing Zeller's Congruence
REM http://en.wikipedia.org/wiki/Zeller_congruence
REM
REM March 2009 - PvE.
REM Revised december 2009.
REM Revised may 2010.

INPUT "Enter day of the month (1-31): ", q
IF (q > 31) + (q < 1) THEN
    PRINT "Day must be within 1-31!"
    END
ENDIF

INPUT "Enter month (1-12): ", m
IF (m > 12) + (m < 1) THEN
    PRINT "Month must be within 1-12!"
    END
ENDIF

INPUT "Enter year: ", y

REM the LET statement is optional
LET J = y / 100
LET K = y % 100

IF (m = 1) + (m = 2) THEN
    m = m + 12
    K = k - 1
ENDIF

REM Here the LET is omitted
h = (q + ((m+1)*26)/10 + K + (K/4) + (J/4) + 5*J)

d = (h % 7)

IF d = 0 THEN
        PRINT "That is a Saturday."
ELSE IF d = 1 THEN
        PRINT "That is a Sunday."
ELSE IF d = 2 THEN
        PRINT "That is a Monday."
ELSE IF d = 3 THEN
        PRINT "That is a Tuesday."
ELSE IF d = 4 THEN
        PRINT "That is a Wednesday."
ELSE IF d = 5 THEN
        PRINT "That is a Thursday."
ELSE IF d = 6 THEN
        PRINT "That is a Friday."
ENDIF ENDIF ENDIF ENDIF ENDIF ENDIF ENDIF