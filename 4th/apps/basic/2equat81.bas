' Not Only 30 Programs For The Sinclair ZX81 1K
' Structured uBasic version, J.L. Bezemer 2014

' Note: this solution is based on the Cramer's rule
' There was a serious bug in the original version!

Print "Solutions To 2 Equations:"
Print "A*X + B*Y + C =0"

For I=1 To 6
  Gosub 100 + 10*(I%3)
  Input @(I)
Next

D = @(1)*@(5)-@(2)*@(4)

If D = 0 Then
   Print "Degenerate: no solutions"
Else
  A = (@(3)*@(5)-@(2)*@(6))/D
  B = (@(1)*@(6)-@(3)*@(4))/D
  Print "Solutions are:","","X=";A,"","Y=";B
Endif

Stop

100 Print "C"; : Return
110 Print "A"; : Return
120 Print "B"; : Return
