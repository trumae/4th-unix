' Create a function which takes in a variable number of arguments and prints
' each one on its own line. Also show, if possible in your language, how to
' call the function on a list of arguments constructed at runtime.

' Functions of this type are also known as Variadic Functions.


Push _Mary, _had, _a, _little, _lamb   ' Push the hashes
Proc _PrintStrings (5)                 ' Print the string

Push 1, 4, 5, 19, 12, 3                ' Push the numbers
Print "Maximum is: ";FUNC(_Max(6))     ' Call the function

End


_PrintStrings Param(1)                 ' Print a variadic number of strings
  Local(1)

  For b@ = a@-1 To 0 Step -1           ' Reverse the hashes, load in array
    @(b@) = Pop()
  Next

  For b@ = 0 To a@-1                   ' Now call the appropriate subroutines
    Proc @(b@)
  Until b@ = a@-1
    Print " ";                         ' Print a space
  Next                                 ' unless it is the last word

  Print                                ' Terminate the string
Return


_Max Param(1)                          ' Calculate the maximum value
  Local(3)

  d@ = -(2^31)                         ' Set maximum to a tiny value

  For b@ = 1 To a@                     ' Get all values from the stack
    c@ = Pop()
    If c@ > d@ THEN d@ = c@            ' Change maximum if required
  Next
Return (d@)                            ' Return the maximum

                                       ' Hashed labels
_Mary   Print "Mary"; : Return
_had    Print "had"; : Return
_a      Print "a"; : Return
_little Print "little"; : Return
_lamb   Print "lamb"; : Return