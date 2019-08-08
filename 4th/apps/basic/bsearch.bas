' A binary search divides a range of values into halves, and continues to
' narrow down the field of search until the unknown value is found. It is
' the classic example of a "divide and conquer" algorithm.

For i = 1 To 100                       ' Fill array with some values
  @(i-1) = i
Next

Print FUNC(_binarySearch(50,0,99))     ' Now find value '50'
End                                    ' and print its index


_binarySearch Param(3)                 ' value, start index, end index
  Local(1)                             ' The middle of the array

If c@ < b@ Then                        ' Ok, signal we didn't find it
  Return (-1)
Else
  d@ = SHL(b@ + c@, -1)                ' Prevent overflow (LOL!)
  If a@ < @(d@) Then Return (FUNC(_binarySearch (a@, b@, d@-1)))
  If a@ > @(d@) Then Return (FUNC(_binarySearch (a@, d@+1, c@)))
  If a@ = @(d@) Then Return (d@)       ' We found it, return index!
EndIf
