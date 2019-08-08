' In mathematics, the Thue–Morse sequence, or Prouhet–Thue–Morse sequence,
' is the binary sequence (an infinite sequence of 0s and 1s) obtained by
' starting with 0 and successively appending the Boolean complement of the
' sequence obtained thus far.

' The first few steps of this procedure yield the strings 0 then 01, 0110,
' 01101001, 0110100110010110, and so on, which are prefixes of the Thue–Morse
' sequence  (sequence A010060 in the OEIS).

For x = 0 to 6                         ' sequence loop
  Print Using "_#";x;": ";             ' print sequence
  For y = 0 To (2^x)-1                 ' element loop
    Print AND(FUNC(_Parity(y)),1);     ' print element
  Next                                 ' next element
  Print                                ' terminate elements line
Next                                   ' next sequence

End

_Parity Param (1)                      ' parity function
  Local (1)                            ' number of bits set
  b@ = 0                               ' no bits set yet
  Do While a@ # 0                      ' until all bits are counted
    If AND(a@, 1) Then b@ = b@ + 1     ' bit set? increment count
    a@ = SHL(a@, -1)                   ' shift the number
  Loop
Return (b@)                            ' return number of bits set