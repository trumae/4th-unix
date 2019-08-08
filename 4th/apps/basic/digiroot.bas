' The digital root, X , of a number, n , is calculated:
' find X as the sum of the digits of n
' find a new X by summing the digits of X ,
' repeating until X has only one digit.
' The additive persistence is the number of summations required to obtain
' the single digit.

PRINT "Digital root of 627615 is "; FUNC(_FNdigitalroot(627615, 10)) ;
PRINT " (additive persistence " ; Pop(); ")"

PRINT "Digital root of 39390 is "; FUNC(_FNdigitalroot(39390, 10)) ;
PRINT " (additive persistence " ; Pop(); ")"

PRINT "Digital root of 588225 is "; FUNC(_FNdigitalroot(588225, 10)) ;
PRINT " (additive persistence " ; Pop(); ")"

PRINT "Digital root of 9992 is "; FUNC(_FNdigitalroot(9992, 10)) ;
PRINT " (additive persistence " ; Pop(); ")"
END


_FNdigitalroot Param(2)
  Local (1)
  c@ = 0
  Do Until a@ < b@
    c@ = c@ + 1
    a@ = FUNC(_FNdigitsum (a@, b@))
  Loop
  Push (c@)                            ' That's how uBasic handles an extra
Return (a@)                            ' return value: on the stack

_FNdigitsum Param (2)
  Local (2)
  d@ =0
  Do While a@ # 0
    c@ = a@ / b@
    d@ = d@ + a@ - (c@ * b@)
    a@ = c@
  Loop
Return (d@)