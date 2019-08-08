' In this task, the goal is to take a combined set of elements and apply a
' function to each element. Here we use both a SQRT() and COS() function.

S = 5                                  ' Size of the array

For x = 0 To S - 1                     ' Initialize array
  @(x) = x + 1
Next

Proc _MapArray (_SquareRoot, S)        ' Call mapping procedure

For x = 0 To S - 1                     ' Print results
  Print "SQRT(";x+1;") = ";Using "#.####";@(x)
Next

For x = 0 To S - 1                     ' Reinitialize array
  @(x) = x + 1
Next

Proc _MapArray (_Cosine, S)            ' Call mapping procedure

Print : For x = 0 To S - 1             ' Print results
  Print "COS(";x+1;") = ";Using "#.####";@(x)
Next

End


_MapArray Param(2)                     ' Param(1) = function
  Local (1)                            ' Param(2) = array size

  For c@ = 0 To b@ - 1
    @(c@) = FUNC(a@(@(c@)))
  Next
Return


_SquareRoot Param (1)                  ' This is an integer SQR subroutine
  Local (2)

  b@ = (10^(4*2)) * a@                 ' Output is scaled by 10^4
  a@ = b@

  Do
    c@ = (a@ + (b@ / a@))/2
  Until (Abs(a@ - c@) < 2)
    a@ = c@
  Loop

Return (c@)


_Cosine Param(1)                       ' This is an integer COS subroutine
  Push Abs((a@*10000)%62832)           ' Output is scaled by 10^4
  If Tos()>31416 Then Push 62832-Pop()
  Let a@=Tos()>15708
  If a@ Then Push 31416-Pop()
  Push Tos()
  Push (Pop()*Pop())/10000
  Push 10000+((10000*-(Tos()/56))/10000)
  Push 10000+((Pop()*-(Tos()/30))/10000)
  Push 10000+((Pop()*-(Tos()/12))/10000)
  Push 10000+((Pop()*-(Pop()/2))/10000)
  If a@ Then Push -Pop()               ' Result is directly transferred
Return                                 ' through the stack