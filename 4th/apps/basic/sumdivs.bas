' These define three classifications of positive integers based on their
' proper divisors.

' Let P(n) be the sum of the proper divisors of n, where the proper divisors
' of n are all positive divisors of n other than n itself.

'   if P(n) < n then n is classed as deficient (OEIS A005100).
'   if P(n) == n then n is classed as perfect (OEIS A000396).
'   if P(n) > n then n is classed as abundant (OEIS A005101).

' Example: 6 has proper divisors 1, 2, and 3. 1 + 2 + 3 = 6 so 6 is classed as
' a perfect number. 

P = 0 : D = 0 : A = 0

For n= 1 to 2000
  s = FUNC(_SumDivisors(n))-n
  If s = n Then P = P + 1
  If s < n Then D = D + 1
  If s > n Then A = A + 1
Next

Print "Perfect: ";P;" Deficient: ";D;" Abundant: ";A
End

' Return the least power of a@ that does not divide b@

_LeastPower Param(2)
  Local(1)

  c@ = a@
  Do While (b@ % c@) = 0
    c@ = c@ * a@
  Loop

Return (c@)


' Return the sum of the proper divisors of a@

_SumDivisors Param(1)
  Local(4)

  b@ = a@
  c@ = 1

  ' Handle two specially

  d@ = FUNC(_LeastPower (2,b@))
  c@ = c@ * (d@ - 1)
  b@ = b@ / (d@ / 2)

  ' Handle odd factors

  For e@ = 3 Step 2 While (e@*e@) < (b@+1)
    d@ = FUNC(_LeastPower (e@,b@))
    c@ = c@ * ((d@ - 1) / (e@ - 1))
    b@ = b@ / (d@ / e@)
  Loop

  ' At this point, t must be one or prime

  If (b@ > 1) c@ = c@ * (b@+1)
Return (c@)
