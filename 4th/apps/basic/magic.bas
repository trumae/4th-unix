' ------=< MAIN >=------
 
Proc _magicsq(5)
Proc _magicsq(11)
End

_magicsq Param (1) Local (4)

    ' reset the array
    For b@ = 0 to 255
        @(b@) = 0
    Next

    If  ((a@ % 2) = 0) + (a@ < 3) + (a@ > 15) Then
        Print "error: size is not odd or size is smaller then 3 or bigger than 15"
        Return
    EndIf

    ' start in the middle of the first row
    b@ = 1
    c@ = a@ - (a@ / 2)
    d@ = 1
    e@ = a@ * a@

    ' main loop for creating magic square
    Do
        If @(c@*a@+d@) = 0 Then
            @(c@*a@+d@) = b@
            If (b@ % a@) = 0 Then
                d@ = d@ + 1
            Else
                c@ = c@ + 1
                d@ = d@ - 1
            EndIf
            b@ = b@ + 1
        EndIf
        If c@ > a@ Then
            c@ = 1
            Do While @(c@*a@+d@) # 0
                c@ = c@ + 1
            Loop
        EndIf
        If d@ < 1 Then
            d@ = a@
            Do While @(c@*a@+d@) # 0
                d@ = d@ - 1
            Loop
        EndIf
    Until b@ > e@
    Loop

    Print "Odd magic square size: "; a@; " * "; a@
    Print "The magic sum = "; ((e@+1) / 2) * a@
    Print

    For d@ = 1 To a@
        For c@ = 1 To a@
            Print Using "____"; @(c@*a@+d@);
        Next
        Print
    Next
    Print
Return
