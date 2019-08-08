' "Chinese Remainder" for uBasic, Copyright J.L. Bezemer 2014
' You can redistribute this file and/or modify it under
' the terms of the GNU General Public License

' Your task is to write a program to solve a system of linear congruences by
' applying the Chinese Remainder Theorem. If the system of equations cannot be
' solved, your program must somehow indicate this. (It may throw an exception
' or return a special false value.) Since there are infinitely many solutions,
' the program should return the unique solution s.

' See also: http://en.wikipedia.org/wiki/Chinese_Remainder_Theorem

' Show the functionality of this program by printing the result such that the
' n's are [3,5,7] and the a's are [2,3,2].

@(000) = 3 : @(001) = 5 : @(002) = 7
@(100) = 2 : @(101) = 3 : @(102) = 2

Print Func (_Chinese_Remainder (3))

' -------------------------------------

@(000) = 11 : @(001) = 12 : @(002) = 13
@(100) = 10 : @(101) = 04 : @(102) = 12

Print Func (_Chinese_Remainder (3))

' -------------------------------------

End

                                       ' returns x where (a * x) % b == 1
_Mul_Inv Param (2)                     ' ( a b -- n)
  Local (4)

  c@ = b@
  d@ = 0
  e@ = 1

  If b@ = 1 Then Return (1)

  Do While a@ > 1
     f@ = a@ / b@
     Push b@ : b@ = a@ % b@ : a@ = Pop()
     Push d@ : d@ = e@ - f@ * d@ : e@ = Pop()
  Loop

  If e@ < 0 Then e@ = e@ + c@

Return (e@)


_Chinese_Remainder Param (1)           ' ( len -- n)
  Local (5)

  b@ = 1
  c@ = 0

  For d@ = 0 Step 1 While d@ < a@
    b@ = b@ * @(d@)
  Next

  For d@ = 0 Step 1 While d@ < a@
    e@ = b@ / @(d@)
    c@ = c@ + (@(100 + d@) * Func (_Mul_Inv (e@, @(d@))) * e@)
  Next

Return (c@ % b@)
