' "Stem-and-leaf plot" for uBasic, Copyright J.L. Bezemer 2014
' You can redistribute this file and/or modify it under
' the terms of the GNU General Public License

' Create a well-formatted stem-and-leaf plot from the following data set,
' where the leaves are the last digits. The primary intent of this task is
' the presentation of information.

Push  12, 127,  28,  42,  39, 113,  42,  18,  44, 118,  44,  37, 113, 124
Push  0, 13 : Gosub _Read              ' read 1st line of data

Push  37,  48, 127,  36,  29,  31, 125, 139, 131, 115, 105, 132, 104, 123
Push  14, 27 : Gosub _Read             ' read 2nd line of data

Push  35, 113, 122,  42, 117, 119,  58, 109,  23, 105,  63,  27,  44, 105
Push  28, 41 : Gosub _Read             ' read 3rd line of data

Push  99,  41, 128, 121, 116, 125,  32,  61,  37, 127,  29, 113, 121,  58
Push  42, 55 : Gosub _Read             ' read 4tH line of data

Push 114, 126,  53, 114,  96,  25, 109,   7,  31, 141,  46,  13,  27,  43
Push  56, 69 : Gosub _Read             ' read 5th line of data

Push 117, 116,  27,   7,  68,  40,  31, 115, 124,  42, 128,  52,  71, 118
Push  70, 83 : Gosub _Read             ' read 6th line of data

Push 117,  38,  27, 106,  33, 117, 116, 111,  40, 119,  47, 105,  57, 122
Push  84, 97 : Gosub _Read             ' read 7th line of data

Push 109, 124, 115,  43, 120,  43,  27,  27,  18,  28,  48, 125, 107, 114
Push  98, 111 : Gosub _Read            ' read 8th line of data

Push  34, 133,  45, 120,  30, 127,  31, 116, 146
Push 112, 120 : Gosub _Read            ' read last line of data

Push 121 : Gosub _SimpleSort           ' now sort 121 elements

i = @(0) / 10 - 1
For j = 0 To Pop() - 1                 ' note array size was still on stack
  d = @(j) / 10
  Do While d > i
    If j Print
    i = i + 1
    If i < 10 Print " ";               ' align stem number
    Print i;" |";                      ' print stem number
  Loop
  Print @(j) % 10;" ";                 ' print leaf number
Next
Print                                  ' print final LF

End

                                       ' simplest sorting algorithm
_SimpleSort                            ' ( n -- n)
  For x = 0 To Tos() - 1
    For y = x+1 To Tos() - 1
      If @(x) > @ (y) Then             ' if larger, switch elements
         Push @(y)
         @(y) = @(x)
         @(x) = Pop()
      Endif
    Next
  Next

Return

                                       ' read a line of data backwards
_Read                                  ' (.. n1 n2 -- ..)
  For x = Pop() To Pop() Step -1       ' loop from n2 to n1
    @(x) = Pop()                       ' get element from stack
  Next
Return