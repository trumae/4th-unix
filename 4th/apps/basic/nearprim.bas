' A k-Almost-prime is a natural number n that is the product of k (possibly
' identical) primes. So, for example, 1-almost-primes, where k = 1, are the
' prime numbers themselves; 2-almost-primes are the semiprimes. 

Local(3)

For c@ = 1 To 5
  Print "k = ";c@;": ";

  b@=0

  For a@ = 2 Step 1 While b@ < 10
    If FUNC(_kprime (a@,c@)) Then
       b@ = b@ + 1
       Print " ";a@;
    EndIf
  Next

  Print
Next

End

_kprime Param(2)
  Local(2)

  d@ = 0
  For c@ = 2 Step 1 While (d@ < b@) * ((c@ * c@) < (a@ + 1))
    Do While (a@ % c@) = 0
      a@ = a@ / c@
      d@ = d@ + 1
    Loop
  Next
Return (b@ = (d@ + (a@ > 1)))