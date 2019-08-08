' "Pascals Triangle" for uBasic, Copyright J.L. Bezemer 2014
' You can redistribute this file and/or modify it under
' the terms of the GNU General Public License

Input "Number Of Rows: "; N
@(1) = 1
Print Tab((N+1)*3);"1"

For R = 2 To N
    Print Tab((N-R)*3+1);
    For I = R To 1 Step -1
        @(I) = @(I) + @(I-1)
        Print Using "______";@(i);
    Next
Next

Print
End
