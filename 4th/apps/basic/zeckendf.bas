' "Zeckendorf numbers" for uBasic, J.L. Bezemer 2014

' Just as numbers can be represented in a positional notation as sums of
'  multiples of the powers of ten (decimal) or two (binary); all the
' positive integers can be represented as the sum of one or zero times the
' distinct members of the Fibonacci series.

' For a true Zeckendorf number there is the added restriction that no two
' consecutive Fibonacci numbers can be used which leads to the former unique
' solution.

' The task is to generate and show here a table of the Zeckendorf number
' representations of the decimal numbers zero to twenty, in order.
' See OEIS A014417 for the the sequence of required results.

For x = 0 to 20                        ' Print Zeckendorf numbers 0 - 20
  Print x,
  Proc _Zeckendorf (x)                 ' get Zeckendorf number repres.
Next

End

_Fibonacci Param (1)
  Local (2)

  b@ = 0                               ' This function returns the
  c@ = 1                               ' Fibonacci number which is smaller
                                       ' or equal to A@
  Do While c@ < a@ + 1
     Push c@
     c@ = b@ + c@                      ' Get next Fibonacci number
     b@ = Pop()
  Loop                                 ' Loop if not exceeded A@

Return (b@)                            ' Return Fibonacci number

_Zeckendorf Param (1)
  Local (1)

  b@ = Func (_Fibonacci (a@))          ' This function breaks A@ up
  Print b@;                            ' into its Zeckendorf components
  a@ = a@ - b@                         ' First digit is always there
                                       ' Any remainder to resolve
  Do While a@                          ' Now go for the next digits
    b@ = Func (_Fibonacci (a@))
    Print " + ";b@;                    ' Print the next digit
    a@ = a@ - b@                       ' Update A@
  Loop

  Print                                ' Terminate the line
Return                                 ' and return
