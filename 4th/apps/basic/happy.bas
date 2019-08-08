' ************************************************************
' PROGRAM:      happynum.bas
' PURPOSE:      generate happy numbers
' AUTHOR:       vovchik (Puppy Linux forum)
' COMMENTS:     ported from Rosetta code examples   
' DEPENDS:      gcc, bacon
' PLATFORM:     Puppy Linux (actually, any *nix)
' DATE:         06-12-2011
' NOTES:        see http://en.wikipedia.org/wiki/Happy_number
' A happy number is defined by the following process:
' Starting with any positive integer, replace the number
' by the sum of the squares of its digits, and repeat
' the process until the number equals 1 (where it will
' stay), or it loops endlessly in a cycle which does not
' include 1. Those numbers for which this process ends
' in 1 are happy numbers :), while those that do not end in
' 1 are unhappy numbers. :(
' ************************************************************

' ************************
' MAIN
' ************************

PROC _PRINT_HAPPY(20)
END

' ************************
' END MAIN
' ************************

' ************************
' SUBS & FUNCTIONS
' ************************

' --------------------
_is_happy PARAM(1)
' --------------------
LOCAL (5)
  f@ = 100
  c@ = a@
  b@ = 0

  DO WHILE b@ < f@
    e@ = 0

    DO WHILE c@
      d@ = c@ % 10
      c@ = c@ / 10
      e@ = e@ + (d@ * d@)
    LOOP

  UNTIL e@ = 1
    c@ = e@
    b@ = b@ + 1
  LOOP

RETURN(b@ < f@)

' --------------------
_PRINT_HAPPY PARAM(1)
' --------------------
LOCAL (2)
  b@ = 1
  c@ = 0

  DO

    IF FUNC (_is_happy(b@)) THEN
       c@ = c@ + 1
       PRINT b@
    ENDIF

    b@ = b@ + 1
    UNTIL c@ + 1 > a@
  LOOP

RETURN

' ************************
' END SUBS & FUNCTIONS
' ************************
