Rem Produce a graphical or ASCII-art representation of a Sierpinski triangle
Rem of order N.
Rem uBasic version, 2013,2015 J.L. Bezemer

Input "Triangle order: ";n
n = 2^n

For y = n - 1 To 0 Step -1

  For i = 0 To y
    Print " ";
  Next

  x = 0

  For x = 0 Step 1 While ((x + y) < n)
    If AND (x,y) Then
       Print "  ";
    Else
       Print "* ";
    EndIf
  Next

  Print
Next
End
