' Hofstadter Figure-Figure sequences
' These two sequences of positive integers are defined as:

'  R(1)=1
'  S(1)=2
'  R(n)=R(n-1)+S(n-1)

' The sequence S(n) is further defined as the sequence of positive integers
' not present in R(n).

' Sequence R starts: 1, 3, 7, 12, 18, ...
' Sequence S starts: 2, 4, 5, 6, 8, ...

' Task:
' 1. Create two functions named that when given n return R(n) or S(n)
'    respectively.
' 2. (Note that R(1) = 1 and S(1) = 2 to avoid off-by-one errors).
' 3. Calculate and show that the first ten values of R are: 1, 3, 7, 12, 18,
'    26, 35, 45, 56, and 69
' 4. Calculate and show that the first 40 values of ffr plus the first 960
'    values of ffs include all the integers from 1 to 1000 exactly once.

' Note that uBasic/4tH has no dynamic memory facilities and only one single
' array of 256 elements. So the only way to cram over a 1000 values there is
' to use a bitmap. This bitmap consists of an R range and an S range. In each
' range, a bit represents a positional value (bit 0 = "1", bit 1 = "2", etc.).

' The R(x) and S(x) functions simply count the number of bits set they
' encountered. To determine whether all integers between 1 and 1000 are
' complementary, both ranges are XORed, which would result in a value other
' than (2^31)-1 if there were any discrepancies present.


Proc _SetBitR(1)                       ' Set the first R value
Proc _SetBitS(2)                       ' Set the first S value

Print "Creating bitmap, wait.."        ' Create the bitmap
Proc _MakeBitMap
Print

Print "R(1 .. 10):";                   ' Print first 10 R-values

For x = 1 To 10
  Print " ";FUNC(_Rx(x));
Next

Print : Print "S(1 .. 10):";           ' Print first 10 S-values

For x = 1 To 10
  Print " ";FUNC(_Sx(x));
Next

Print : Print                          ' Terminate and skip line

For x = 0 To (1000/31)                 ' Check the first 1000 values
  Print "Checking ";(x*31)+1;" to ";(x*31)+31;":\t";
  If XOR(@(x), @(x+64)) = 2147483647 Then
     Print "OK"                        ' XOR R() and S() ranges
  Else                                 ' should deliver MAX-N
     Print "Fail!"                     ' or we did have an error
  EndIf
Next

For x = 1 to 40                        ' Prove there are only 40 R(x) values
  If FUNC(_Rx(x)) > 1000 Then Print "R(";x;") value greater than 1000"
Next                                   ' below 1000

If FUNC(_Rx(x)) < 1001 Then Print "R(";x;") value also below 1000"
End


_MakeBitMap                            ' Create the bitmap
  Local (4)

  a@ = 1                               ' Previous R(x) level
  b@ = 1                               ' Previous R(x) value

  Do Until b@ > (1000/31)*32           ' Fill up an entire array element
                                       ' calculate R(x+1) level
    c@ = FUNC(_Rx(a@)) + FUNC(_Sx(a@))
    Proc _SetBitR (c@)                 ' Set R(x+1) in the bitmap

    For d@ = b@ + 1 To c@ - 1          ' Set all intermediate S() values
      Proc _SetBitS (d@)               ' between R(x) and R(x+1)
    Next

    Proc _SetBitS (c@+1)               ' Number after R(x) is always S()
    b@ = c@                            ' R(x+1) now becomes R(x)
    a@ = a@ + 1                        ' Increment level
  Loop                                 ' Now do it again
Return


_Rx Param(1)                           ' Return value R(x)
  Local(2)

  b@ = 0                               ' No value found so far

  For c@ = 1 To (64*31)-1              ' Check the entire bitmap
    If (FUNC(_GetBitR(c@))) Then b@ = b@ + 1
    Until b@ = a@                      ' If a value found, increment counter
  Next                                 ' Until the required level is reached
Return (c@)                            ' Return position in bitmap


_Sx Param(1)                           ' Return value S(x)
  Local(2)

  b@ = 0

  For c@ = 1 To (64*31)-1              ' No value found so far
    If (FUNC(_GetBitS(c@))) Then b@ = b@ + 1
    Until b@ = a@                      ' If a value found, increment counter
  Next                                 ' Until the required level is reached
Return (c@)                            ' Return position in bitmap


_SetBitR Param(1)                      ' Set bit n-1 in R-bitmap
  a@ = a@ - 1
  @(a@/31) = OR(@(a@/31), SHL(1,a@%31))
Return

_GetBitR Param(1)                      ' Return bit n-1 in R-bitmap
  a@ = a@ - 1
Return (AND(@(a@/31), SHL(1,a@%31))#0)

_SetBitS Param(1)                      ' Set bit n-1 in S-bitmap
  a@ = a@ - 1
  @(64+a@/31) = OR(@(64+a@/31), SHL(1,a@%31))
Return

_GetBitS Param(1)                      ' Return bit n-1 in S-bitmap
  a@ = a@ - 1
Return (AND(@(64+a@/31), SHL(1,a@%31))#0)