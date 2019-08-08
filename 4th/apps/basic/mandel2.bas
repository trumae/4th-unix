' Copyright (C) 2002, 2003 Volker Poplawski <volker@poplawski.de>
'                          Stefan Reinauer
' This example even fits in a signature ;-)

' uBasic version, J.L. Bezemer 2014

For A = -18022 To 18021 Step 1502
  For B = -32768 To 16383 Step 616
    D = 42                             ' Start off with a star
    Gosub _Reset                       ' Reset all values

    For I = 0 To 29

      Push E                           ' Save original E
      E = G - H + B                    ' Calculate the new E
      F = ((Pop() * F)/8192) + A       ' Calculate the new F
      G = E * E / 16384                ' Calculate the new G
      H = F * F / 16384                ' Calculate the new H

      If G+H > 65536 Then              ' If outside the range
         D = 32                        ' Reset and print a space
         Gosub _Reset                  ' Quit the loop
         Break
      Endif

    Next
    Gosub D                            ' Print the character
  Next
  Print                                ' Print a newline
Next

End

_Reset                                 ' Reset all values
  E = 0
  F = 0
  G = 0
  H = 0
Return

32 Print " "; : Return                 ' Print a space
42 Print "*"; : Return                 ' Print a star