' In modular arithmetic, the modular multiplicative inverse of an integer
' a modulo m is an integer x such that ax = 1 + km
' It can be shown that such an inverse exists only if a and m are coprime

Print FUNC(_MulInv(42, 2017))
End

_MulInv Param(2)
  Local(5)

  c@ = b@
  f@ = 0
  g@ = 1

  If b@ = 1 Then Return

  Do While a@ > 1
    e@ = a@ / b@
    d@ = b@
    b@ = a@ % b@
    a@ = d@

    d@ = f@
    f@ = g@ - e@ * f@
    g@ = d@
  Loop

  If g@ < 0 Then g@ = g@ + c@
Return (g@)
