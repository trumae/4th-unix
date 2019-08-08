' This task requires the finding of the greatest common divisor of 2 integers
' and to compute the least common multiple of two integers.

Print "GCD of 18 : 12 = "; FUNC(_GCD_Iterative_Euclid(18,12))
Print "GCD of 1071 : 1029 = "; FUNC(_GCD_Iterative_Euclid(1071,1029))
Print "GCD of 3528 : 3780 = "; FUNC(_GCD_Iterative_Euclid(3528,3780))
Print "LCM of 12 : 18 = "; FUNC(_LCM(12,18))

End


_GCD_Iterative_Euclid Param(2)
  Local (1)
  Do While b@
    c@ = a@
    a@ = b@
    b@ = c@ % b@
  Loop
Return (ABS(a@))


_LCM Param(2)
If a@*b@
  Return (ABS(a@*b@)/FUNC(_GCD_Iterative_Euclid(a@,b@)))
Else
  Return (0)
EndIf
