' The multiplicative inverse of a modulo m exists if and only if a and m are
' coprime (i.e., if gcd(a, m) = 1).[1] If the modular multiplicative inverse
' of a modulo m exists, the operation of division by a modulo m can be defined
' as multiplying by the inverse of a, which is in essence the same concept as
' division in the field of reals.

Print FUNC(_mul_inv(42, 2017))
Print FUNC(_mul_inv(40, 1))
Print FUNC(_mul_inv(52, -217))
Print FUNC(_mul_inv(-486, 217))
Print FUNC(_mul_inv(40, 2018))

End

_mul_inv Param(2)
  Local(6)

  If (b@ < 0) b@ = -b@
  If (a@ < 0) a@ = b@ - (-a@ % b@)
  c@ = 0 : d@ = 1 :  e@ = b@ :  f@ = a@ % b@

  Do Until (f@ = 0)
    g@ = e@/f@
    h@ = d@ :  d@ = c@ - g@*d@ :  c@ = h@
    h@ = f@ :  f@ = e@ - g@*f@ :  e@ = h@
  Loop

  If (e@ > 1) Return (-1)  ' No inverse
  If (c@ < 0) c@ = c@ + b@
Return (c@)
