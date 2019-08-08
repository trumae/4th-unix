' ------=< MAIN >=------
' only months with 31 day can have five weekends
' these months are: January, March, May, July, August, October, December
' in nr: 1, 3, 5, 7, 8, 10, 12
' the 1e day needs to be on a friday (= 5)

For y = 1900 To 2100  ' Gregorian calendar
    a = 0
    For m = 1 To 12 Step 2
        If m = 9 Then m = 8
        If Func(_wd(m , 1 , y)) = 5 Then
           If a Then
              Print ", ";
           Else
              Print y; " | ";
              a = 1
           EndIf
           GoSub m*10
           t = t + 1
        EndIf
    Next
    If a Then
       Print
    Else
       i = i + 1
       @(i) = y
    EndIf
Next

Print
Print "Number of months from 1900 to 2100 that have five weekends: ";t
Print
Print i;" years don't have months with five weekends:"
Print

For j = 1 To i
    Print @(j); " ";
    If (j % 8) = 0 Then Print
Next

Print
End


_wd Param(3)
    ' Zellerish
    ' 0 = Sunday, 1 = Monday, 2 = Tuesday, 3 = Wednesday
    ' 4 = Thursday, 5 = Friday, 6 = Saturday

    If a@ < 3 Then        ' If a@ = 1 Or a@ = 2 Then
        a@ = a@ + 12
        c@ = c@ - 1
    EndIf
Return ((c@ + (c@ / 4) - (c@ / 100) + (c@ / 400) + b@ + ((153 * a@ + 8) / 5)) % 7)


 10 Print "January"; : Return
 20 Print "February"; : Return
 30 Print "March"; : Return
 40 Print "April"; : Return
 50 Print "May"; : Return
 60 Print "June"; : Return
 70 Print "July"; : Return
 80 Print "August"; : Return
 90 Print "September"; : Return
100 Print "October"; : Return
110 Print "November"; : Return
120 Print "December"; : Return
