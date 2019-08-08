' PROGRAM Armstrong Numbers

PRINT "ARMSTRONG NUMBERS" : PRINT
PRINT "Program computes all Armstrong numbers in the range of 0 - 999."
PRINT "An Armstrong number is a number such that the sum of its digits"
PRINT "raised to the third power is equal to the number itself." : PRINT

'   A, B, C                            the three digits
'   S, T                               the number and its cubic sum
'   TOS                                a counter

Push 1                                 'initialise

For A = 0 To 9                         'for the left most digit
  For B = 0 To 9                       'for the middle digit
    For C = 0 To 9                     'for the right most digit
      S = A*100 + B*10 + C             'the number
      T = A^3 + B^3 + C^3              'the sum of cubes
      If S = T Then Gosub _Display     'if they are equal
    Next
  Next
Next

If Pop() Then End                      'clean up the stack
                                       'display results subroutine
_Display
  Print "Armstrong number "; Tos();": "; S
  Push Pop() + 1                       'count the Armstrongs
Return
