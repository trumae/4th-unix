' The Harshad or Niven numbers are positive integers >= 1 that are divisible
' by the sum of their digits.

' For example, 42 is a Harshad number as 42 is divisible by (4+2) without
' remainder. Assume that the series is defined as the numbers in increasing
' order.

' The task is to create a function/method/procedure to generate successive
' members of the Harshad sequence. Use it to list the first twenty members of
' the sequence and list the first Harshad number greater than 1000.

C=0

For I = 1 Step 1 Until C = 20          ' First 20 Harshad numbers
  If FUNC(_FNHarshad(I)) Then Print I;" "; : C = C + 1
Next

For I = 1001 Step 1                    ' First Harshad greater than 1000
  If FUNC(_FNHarshad(I)) Then Print I;" " : Break
Next

End

_FNHarshad Param(1)
  Local(2)

  c@ = a@
  b@ = 0
  Do While (c@ > 0)
     b@ = b@ + (c@ % 10)
     c@ = c@ / 10
  Loop

Return ((a@ % b@) = 0)

