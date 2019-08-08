' A spiral array is a square arrangement of the first N2 natural numbers,
' where the numbers increase sequentially as you go around the edges of the
' array spiralling inwards.

' For example, given 5, produce this array:

'  0  1  2  3  4
' 15 16 17 18  5
' 14 23 24 19  6
' 13 22 21 20  7
' 12 11 10  9  8

Input "Width:  ";w
Input "Height: ";h
Print

For i = 0 To h-1
  For j = 0 To w-1
    Print Using "__#"; FUNC(_Spiral(w,h,j,i));
  Next
  Print
Next
End


_Spiral Param(4)
If d@ Then
  Return (a@ + FUNC(_Spiral(b@-1, a@, d@ - 1, a@ - c@ - 1)))
Else
  Return (c@)
EndIf
