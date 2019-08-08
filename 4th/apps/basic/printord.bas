' "N'th" for uBasic, Copyright J.L. Bezemer 2014
' You can redistribute this file and/or modify it under
' the terms of the GNU General Public License

' Write a subroutine that when given an integer greater than or equal to zero
' returns a string of the number followed by an apostrophe then the ordinal
' suffix. Example returns would include 1'st 2'nd 3'rd 11'th 111'th 1001'st

' Use your routine to show here the output for at least the following
' (inclusive) ranges of integer inputs: 0..25, 250..265, 1000..1025

For x = 0 to 25                        ' Test range 0..25
  Push x : GoSub _PrintOrdinal
Next x  : Print

For x = 250 to 265                     ' Test range 250..265
  Push x : GoSub _PrintOrdinal
Next x : Print

For x = 1000 to 1025                   ' Test range 1000..1025
  Push x : GoSub _PrintOrdinal
Next x : Print

End                                    ' End test program
                                       ' ( n --)
_PrintOrdinal                          ' Ordinal subroutine
  If Tos() > -1 Then                   ' If within range then
    Print Using "____#";Tos();"'";     ' Print the number
                                       ' Take care of 11, 12 and 13
    If (Tos()%100 > 10) * (Tos()%100 < 14) Then
       Gosub (Pop() * 0) + 100         ' Clear stack and print "th"
    Else
      Push Pop() % 10                  ' Calculate n mod 10
      GoSub 100 + 10 * ((Tos()>0) + (Tos()>1) + (Tos()>2) - (3 * (Pop()>3)))
    EndIf
  Else                                 ' And decide which ordinal to use
    Print Pop();" is zero or less"     ' Otherwise, it is an error
  EndIf

Return
                                       ' Select and print proper ordinal
100 Print "th"; : Return
110 Print "st"; : Return
120 Print "nd"; : Return
130 Print "rd"; : Return
