' "AKS prime test" for uBasic, Copyright J.L. Bezemer 2014
' You can redistribute this file and/or modify it under
' the terms of the GNU General Public License

' The AKS algorithm for testing whether a number is prime is a polynomial-time
' algorithm based on an elementary theorem about Pascal triangles.

' See: http://www.cse.iitk.ac.in/users/manindra/algebra/primality_v6.pdf

' The task:

' 1. Create a function/subroutine/method that given p generates the
'    coefficients of the expanded polynomial representation of (x − 1)p.
' 2. Use the function to show here the polynomial expansions of (x − 1)p
'    for p in the range 0 to at least 7, inclusive.
' 3. Use the previous function in creating another function that when given p
'    returns whether p is prime using the theorem.
' 4. Use your test to generate a list of all primes under 35.

For n = 0 To 9
  Push n : Gosub _coef : Gosub _drop
  Print "(x-1)^";n;" = ";
  Push n : Gosub _show
  Print
Next

Print
Print "primes (never mind the 1):";

For n = 1 To 34
  Push n : Gosub _isprime
  If Pop() Then Print " ";n;
Next

Print
End

                                       ' show polynomial expansions
_show                                  ' ( n --)
  Do
    If @(Tos()) > -1 Then Print "+";
    Print @(Tos());"x^";Tos();
  While (Tos())
    Push Pop() - 1
  Loop

  Gosub _drop
Return

                                       ' test whether number is a prime
_isprime                               ' ( n --)
  Gosub _coef

  i = Tos()
  @(0) = @(0) + 1
  @(i) = @(i) - 1


  Do While (i) * ((@(i) % Tos()) = 0)
    i = i - 1
  Loop

  Gosub _drop
  Push (i = 0)
Return

                                       ' generate coefficients
_coef                                  ' ( n -- n)
  If (Tos() < 0) + (Tos() > 34) Then End
                                       ' gracefully deal with range issue
  i = 0
  @(i) = 1

  Do While i < Tos()
    j = i
    @(j+1) = 1

    Do While j > 0
      @(j) = @(j-1) - @(j)
      j = j - 1
    Loop

    @(0) = -@(0)
    i = i + 1
  Loop
Return

                                       ' drop a value from the stack
_drop                                  ' ( n --)
  If Pop() Endif
Return
