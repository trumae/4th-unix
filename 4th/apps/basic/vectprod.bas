' A vector is defined as having three dimensions as being represented by an
' ordered collection of three numbers:   (X, Y, Z).

' If you imagine a graph with the x and y axis being at right angles to each
' other and having a third, z axis coming out of the page, then a triplet of
' numbers, (X, Y, Z) would represent a point in the region, and a vector from
' the origin to the point.

' Given the vectors: 
'        A = (a1,  a2,  a3) 
'        B = (b1,  b2,  b3) 
'        C = (c1,  c2,  c3) 

' then the following common vector products are defined: 
' The dot product            (a scalar quantity)
'     A • B = a1b1 + a2b2 + a3b3
' The cross product          (a vector quantity)
'     A x B = (a2b3 - a3b2, a3b1 - a1b3, a1b2 - a2b1)
' The scalar triple product  (a scalar quantity)
'     A • (B x C)
' The vector triple product  (a vector quantity)
'     A x (B x C)

a = 0                                  ' use variables for vector addresses
b = a + 3
c = b + 3
d = c + 3

Proc _Vector (a, 3, 4, 5)              ' initialize the vectors
Proc _Vector (b, 4, 3, 5)
Proc _Vector (c, -5, -12, -13)

Print "a . b = "; FUNC(_FNdot(a, b))
Proc _Cross (a, b, d)
Print "a x b = (";@(d+0);", ";@(d+1);", ";@(d+2);")"
Print "a . (b x c) = "; FUNC(_FNscalarTriple(a, b, c))
Proc _VectorTriple (a, b, c, d)
Print "a x (b x c) = (";@(d+0);", ";@(d+1);", ";@(d+2);")"
End

_FNdot Param (2)
Return ((@(a@+0)*@(b@+0))+(@(a@+1)*@(b@+1))+(@(a@+2)*@(b@+2)))

_Vector Param (4)                      ' initialize a vector
  @(a@ + 0) = b@
  @(a@ + 1) = c@
  @(a@ + 2) = d@
Return

_Cross Param (3)
  @(c@+0) = @(a@ + 1) * @(b@ + 2) - @(a@ + 2) * @(b@ + 1)
  @(c@+1) = @(a@ + 2) * @(b@ + 0) - @(a@ + 0) * @(b@ + 2)
  @(c@+2) = @(a@ + 0) * @(b@ + 1) - @(a@ + 1) * @(b@ + 0)
Return

_FNscalarTriple Param (3)
  Local (1)                            ' a "local" vector
  d@ = d + 3                           ' (best effort) ;-)
  Proc _Cross(b@, c@, d@)
Return (FUNC(_FNdot(a@, d@)))

_VectorTriple Param(4)
  Local (1)                            ' a "local" vector
  e@ = d + 3                           ' (best effort) ;-)
  Proc _Cross (b@, c@, e@)
  Proc _Cross (a@, e@, d@)
Return
