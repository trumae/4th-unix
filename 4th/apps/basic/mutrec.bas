' Two functions are said to be mutually recursive if the first calls the
' second, and in turn the second calls the first. Write two mutually recursive
' functions that compute members of the Hofstadter Female and Male sequences.
' https://en.wikipedia.org/wiki/Hofstadter_sequence#Hofstadter_Female_and_Male_sequences

LOCAL(1)                               ' main uses locals as well

FOR a@ = 0 TO 200                      ' set the array
  @(a@) = -1
NEXT

PRINT "F sequence:"                    ' print the F-sequence
FOR a@ = 0 TO 73
  PRINT FUNC(_f(a@));" ";
NEXT
PRINT

PRINT "M sequence:"                    ' print the M-sequence
FOR a@ = 0 TO 73
  PRINT FUNC(_m(a@));" ";
NEXT
PRINT

END


_f PARAM(1)                            ' F-function
  IF a@ = 0 THEN RETURN (1)            ' memoize the solution
  IF @(a@) < 0 THEN @(a@) = a@ - FUNC(_m(FUNC(_f(a@ - 1))))
RETURN (@(a@))                         ' return array element


_m PARAM(1)                            ' M-function
  IF a@ = 0 THEN RETURN (0)            ' memoize the solution
  IF @(a@+100) < 0 THEN @(a@+100) = a@ - FUNC(_f(FUNC(_m(a@ - 1))))
RETURN (@(a@+100))                     ' return array element
