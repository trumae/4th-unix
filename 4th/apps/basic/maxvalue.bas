' Create a function that returns the maximum value in a provided set of
' values, where the number of values may not be known until runtime.

Push 13, 0, -6, 2, 37, -10, 12         ' Push values on the stack
Print "Maximum value = " ; FUNC(_FNmax(7))
End                                    ' We pushed seven values

_FNmax Param(1)
  Local(3)

  d@ = -(2^31)                         ' Set maximum to a tiny value

  For b@ = 1 To a@                     ' Get all values from the stack
    c@ = Pop()
    If c@ > d@ THEN d@ = c@            ' Change maximum if required
  Next
Return (d@)                            ' Return the maximum
