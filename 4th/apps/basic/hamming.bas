' Hamming numbers - uBasic version J.L. Bezemer 2014

' Hamming numbers are also known as ugly numbers and also 5-smooth numbers
' (numbers whose prime divisors are less or equal to 5). Generate the sequence
' of Hamming numbers, in increasing order.

' In particular: Show the first twenty Hamming numbers.

For H = 1 To 20
  Print "H("; H; ") = "; Func (_FnHamming (H))
Next

End

_FnHamming Param (1)
  Local (8)
  @(0) = 1

  b@ = 2 : c@ = 3 : d@ = 5
  e@ = 0 : f@ = 0 : g@ = 0

  For h@ = 1 To a@ - 1
    i@ = b@
    If i@ > c@ Then i@ = c@
    If i@ > d@ Then i@ = d@
    @(h@) = i@

    If i@ = b@ Then e@ = e@ + 1 : b@ = 2 * @(e@)
    If i@ = c@ Then f@ = f@ + 1 : c@ = 3 * @(f@)
    If i@ = d@ Then g@ = g@ + 1 : d@ = 5 * @(g@)
  Next

Return (@(a@-1))