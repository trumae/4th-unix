' ** Taken from Rosetta code examples
' ** http://rosettacode.org

c = 0
for p       = 0 to 100
  for n     = 0 to 20
    for d   = 0 to 10
      for q = 0 to 4
       if p + n * 5 + d * 10 + q * 25 = 100 then
         print p;" pennies ";n;" nickels "; d;" dimes ";q;" quarters"
         c = c + 1
       endif
      next q
    next d
  next n
next p
print c;" ways to make a buck"