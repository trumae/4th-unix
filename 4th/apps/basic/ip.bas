'
' Simple IP netmask calculator - needs BaCon 1.0 build 11 or higher.
'
' Calculate network IPv4 netmask from CIDR notation.
' See also: http://en.wikipedia.org/wiki/CIDR_notation
'
' PvE March 2010 - GPL
'------------------------------------------------------------------------

INPUT "Usage: IP where value is in the range 1-32: "; m

b = 0

PRINT "Netmask is: ";

DO WHILE m > 8
    PRINT "255.";
    b = b + 1
    m = m - 8
LOOP

IF m > 0 THEN
    PRINT 256 - (2 ^ (8-m));
    b = b + 1
ENDIF

DO WHILE b < 4
    PRINT ".0";
    b = b + 1
LOOP

PRINT