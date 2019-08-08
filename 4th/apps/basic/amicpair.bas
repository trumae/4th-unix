' Amicable pairs

' 1184 and 1210 are an amicable pair, with proper divisors:
'   1, 2, 4, 8, 16, 32, 37, 74, 148, 296, 592     ( = 1210) and 
'   1, 2, 5, 10, 11, 22, 55, 110, 121, 242, 605   ( = 1184) respectively.

Input "Limit: ";l
Print "Amicable pairs < ";l

For n = 1 To l
  m = FUNC(_SumDivisors (n))-n
  If m = 0 Then Continue               ' No division by zero, please
  p = FUNC(_SumDivisors (m))-m
  If (n=p) * (n<m) Then Print n;" and ";m
Next

End

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
