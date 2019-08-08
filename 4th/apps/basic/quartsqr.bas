10 REM Table of Quarter Squares
30 PRINT "QUARTER SQUARES" : PRINT
40 PRINT "Before the discovery of logarithms, tables of quartersquares were used"
50 PRINT "for multiplication. The quartersquare of an integer is the integer part"
60 PRINT "of one quarter of its square." : PRINT
70 PRINT "To multiply the two numbers x and y it is simply necessary to calculate"
80 PRINT "xy = quartersquare(x+y) - quartersquare(x-y)" : PRINT
110 PRINT "",
120 FOR U = 0 TO 9
130    PRINT U,
140 NEXT U
150 PRINT :PRINT
160 FOR T = 0 TO 190 STEP 10
170    PRINT T,
190    FOR N = T TO T+9
200       PRINT (N^2/4),
210    NEXT N
220    PRINT
230 NEXT T
240 END