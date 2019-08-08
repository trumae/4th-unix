' "Roman numerals/Encode" for uBasic, Copyright J.L. Bezemer 2014
' You can redistribute this file and/or modify it under
' the terms of the GNU General Public License

Push 1, 4, 5, 9, 10, 40, 50, 90, 100, 400, 500, 900, 1000
                                       ' Initialize array
For i = 12 To 0 Step -1
  @(i) = Pop()
Next
                                       ' Calculate and print numbers
Print 1999, : Proc _FNroman (1999)
Print 2014, : Proc _FNroman (2014)
Print 1666, : Proc _FNroman (1666)
Print 3888, : Proc _FNroman (3888)

End

_FNroman Param (1)                     ' ( n --)
  Local (1)                            ' Define b@
                                       ' Try all numbers in array
  For b@ = 12 To 0 Step -1
    Do While a@ > @(b@) - 1            ' Several occurrences of same number?
      GoSub ((b@ + 1) * 10)            ' Print roman digit
      a@ = a@ - @(b@)                  ' Decrement number
    Loop
  Next

  Print                                ' Terminate line
Return
                                       ' Print roman digits
 10 Print "I";  : Return
 20 Print "IV"; : Return
 30 Print "V";  : Return
 40 Print "IX"; : Return
 50 Print "X";  : Return
 60 Print "XL"; : Return
 70 Print "L";  : Return
 80 Print "XC"; : Return
 90 Print "C";  : Return
100 Print "CD"; : Return
110 Print "D";  : Return
120 Print "CM"; : Return
130 Print "M";  : Return
