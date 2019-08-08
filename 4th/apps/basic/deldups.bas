' "Remove duplicate elements" for uBasic, J.L. Bezemer 2014
' Given an Array, derive a sequence of elements in which all duplicates are
' removed.

' Go through the list, and for each element, check the rest of the list to see
' if it appears again, and discard it if it does. The complexity is O(n^2).
' The up-shot is that this always works on any type (provided that you can
' test for equality).

n = 16                                 ' Define the number of labels and
                                       ' define the labels themselves.
Push _Now, _is, _the, _time, _for, _all, _good, _men
Push _to, _come, _to, _the, _aid, _of, _the, _party.
                                       ' Push all labels like numbers on the
For x = n-1 To 0 Step -1               ' stack and read them back in reverse.
  @(x) = Pop()                         ' This is the way you emulate a DATA
Next                                   ' statement.

GoSub _RemoveDuplicates                ' Now call the subroutine to eliminate
                                       ' all the duplicates.
For x = 0 To Pop() - 1                 ' Print all the labels. They point to
  GoSub @(x)                           ' a unique line number of a subroutine.
Next                                   ' The subroutine returned the number
                                       ' of labels left after elimination.
Print                                  ' Terminate the line and the program.
End

_RemoveDuplicates                      ' Remove the duplicates subroutine.
  y = 0

  For i = 0 To n-1
    s = @(i)
    For j = 0 To i-1                   ' Look for a duplicate.
      Until s = @(j)                   ' Exit the loop in a controlled way.
    Next

    If j+1 > i Then                    ' If found, delete the duplicate.
       @(y) = s
       y = y + 1
    EndIf
  Next
  Push y                               ' Return size of reduced array.
Return
                                       ' Link labels to strings.
_Now    Print "Now ";    : Return
_is     Print "is ";     : Return
_the    Print "the ";    : Return
_time   Print "time ";   : Return
_for    Print "for ";    : Return
_all    Print "all ";    : Return
_good   Print "good ";   : Return
_men    Print "men ";    : Return
_to     Print "to ";     : Return
_come   Print "come ";   : Return
_aid    Print "aid ";    : Return
_of     Print "of ";     : Return
_party. Print "party. "; : Return