' The factorial of a number, written as n! , is defined as:
' n! = n (n − 1) (n − 2)...(2) (1) .

' Multifactorials generalize factorials as follows:

'    n!     = n (n − 1) (n − 2) ... (2) (1)
'    n!!    = n (n − 2) (n − 4)...
'    n!!!   = n (n − 3) (n − 6)...
'    n!!!!  = n (n − 4) (n − 8)...
'    n!!!!! = n (n − 5) (n − 10)...

' In all cases, the terms in the products are positive integers.

' If we define the degree of the multifactorial as the difference in
' successive terms that are multiplied together for a multifactorial
' (the number of exclamation marks), then the task is twofold:

' Write a function that given n and the degree, calculates the multifactorial.
' Use the function to generate and display here a table of the first ten
' members (1 to 10) of the first five degrees of multifactorial.

print "Degree  |           Multifactorials 1 to 10"
for x = 1 to 53 : print "-"; : next : print
for d = 1 to 5
  print d;"       ";"| ";
  for n = 1 to 10
    print FUNC(_multiFact(n, d));" ";
  next
  print
next

end

_multiFact param (2)
  local (2)
  c@ = 1
  for d@ = a@ to 2 step -b@
    c@ = c@ * d@
  next
return (c@)
