' A Mersenne number is a number in the form of (2^P)-1.

' If P is prime, the Mersenne number may be a Mersenne prime (if P is not
' prime, the Mersenne number is also not prime).

' In the search for Mersenne prime numbers it is advantageous to eliminate
' exponents by finding a small factor before starting a, potentially lengthy,
' Lucas-Lehmer test.

' There are very efficient algorithms for determining if a number divides
' (2^P)-1 (or equivalently, if 2P mod (the number) = 1). Some languages
' already have built-in implementations of this exponent-and-mod operation
' (called modPow or similar).

Print "A factor of M929 is "; FUNC(_FNmersenne_factor(929))
Print "A factor of M937 is "; FUNC(_FNmersenne_factor(937))

End

_FNmersenne_factor Param(1)
  Local(2)

  If (FUNC(_FNisprime(a@)) = 0) Then Return (-1)

  For b@ = 1 TO 99999
    c@ = (2*a@*b@) + 1
    If (FUNC(_FNisprime(c@))) Then
      If (AND (c@, 7) = 1) + (AND (c@, 7) = 7) Then
        If FUNC(_ModPow2 (a@, c@)) = 1 Then
          Unloop : Return (c@)
        EndIf
      EndIf
    EndIf
  Next

Return (0)


_FNisprime Param(1)
  Local (1)

  If ((a@ % 2) = 0) Then Return (a@ = 2)
  If ((a@ % 3) = 0) Then Return (a@ = 3)

  b@ = 5

  Do Until ((b@ * b@) > a@) + ((a@ % b@) = 0)
    b@ = b@ + 2
  Until (a@ % b@) = 0
    b@ = b@ + 4
  Loop

Return ((b@ * b@) > a@)


_ModPow2 Param(2)
  Local(2)

  d@ = 1
  For c@ = 30 To 0 Step -1
    If ((a@+1) > SHL(1,c@)) Then
       d@ = d@ * d@
       If AND (a@, SHL(1,c@)) Then
          d@ = d@ * 2
       EndIf
       d@ = d@ % b@
    EndIf
  Next

Return (d@)