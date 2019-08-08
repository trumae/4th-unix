' Hofstadter Q-sequence:
'   a(1) = a(2) = 1; a(n) = a(n-a(n-1)) + a(n-a(n-2)) for n > 2

' https://oeis.org/A005185
' https://en.wikipedia.org/wiki/Hofstadter_sequence#Hofstadter_Q_sequence

Print "First 10 terms of Q = " ;
For i = 1 To 10 : Print FUNC(_q(i));" "; : Next : Print
Print "256th term = ";FUNC(_q(256))

End

_q Param(1)
  Local(2)

  If a@ < 3 Then Return (1)
  If a@ = 3 Then Return (2)

  @(0) = 1 : @(1) = 1 : @(2) = 2
  c@ = 0

  For b@ = 3 To a@-1
    @(b@) = @(b@ - @(b@-1)) + @(b@ - @(b@-2))
    If @(b@) < @(b@-1) THEN c@ = c@ + 1
  Next

Return (@(a@-1))