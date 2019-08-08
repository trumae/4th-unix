' ------=< MAIN >=------

m = 0
Proc _hailstone_print(27)
Print

For x = 1 To 1000
    n = Func(_hailstone(x))
    If n > m Then
        t = x
        m = n
    EndIf
Next

Print  "The longest sequence is for "; t; ", it has a sequence length of "; m

End

_hailstone_print Param (1)
    ' print the number and sequence

    Local (1)
    b@ = 1

    Print "sequence for number "; a@
    Print Using "________"; a@;   'starting number

    Do While a@ # 1
        If (a@ % 2 ) = 1 Then
            a@ = a@ * 3 + 1   ' n * 3 + 1
        Else
            a@ = a@ / 2       ' n / 2
        EndIf

        b@ = b@ + 1
        Print Using "________"; a@;

        If (b@ % 10) = 0 Then Print
    Loop

    Print : Print
    Print "sequence length = "; b@
    Print

    For b@ = 0 To 79
      Print "-";
    Next

    Print
Return
 
_hailstone Param (1)
    ' normal version
    ' only counts the sequence

    Local (1)
    b@ = 1

    Do While a@ # 1
        If (a@ % 2) = 1 Then
            a@ = a@ * 3 + 1  ' n * 3 + 1
        Else
            a@ = a@ / 2      ' divide number by 2
        EndIf

        b@ = b@ + 1
    Loop

Return (b@)
