' Produce a zig-zag array. A zig-zag array is a square arrangement of the
' first N^2 integers, where the numbers increase sequentially as you zig-zag
' along the anti-diagonals of the array.

S = 5

i = 1
j = 1

For e = 0 To (S*S)-1
  @((i-1) * S + (j-1)) = e

  If (i + j) % 2 = 0 Then

    If j < S Then
      j = j + 1
    Else
      i = i + 2
    EndIf

    If i > 1 Then
      i = i - 1
    EndIf
  Else

    If i < S
      i = i + 1
    Else
      j = j + 2
    EndIf

    If j > 1
      j = j - 1
    EndIf
  EndIf
Next

For r = 0 To S-1
  For c = 0 To S-1
    Print Using "___#";@(r * S + c);
  Next
  Print
Next
