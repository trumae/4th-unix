' "Calendar" for uBasic, Copyright J.L. Bezemer 2014
' You can redistribute this file and/or modify it under
' the terms of the GNU General Public License

Input "Year to print: "; Y             ' input required year

Push 63 : Gosub _Space                 ' center year
Print Y

For M = 1 To 7 Step 6                  ' two rows of six months
  Print

  For C = 0 To 5                       ' print months
    Push 1, M + C, Y : Gosub _FnMjd : @(C+6) = Pop()

    Push 5 : Gosub _Space              ' print month name
    Gosub (M + C)

    If C = 5 Then                      ' is it the last month?
      Print                            ' if so, terminate line
    Else                               ' if not, print eight spaces
      Push 8 : Gosub _Space
    EndIf
  Next

  For C = 0 To 5                       ' print days header
    Print "Su Mo Tu We Th Fr Sa";

    If C = 5 Then                      ' is it the last month?
      Print                            ' if so, terminate line
    Else                               ' if not, print two spaces
      Push 2 : Gosub _Space
    EndIf
                                       ' get days/month
    Push M + C, Y : Gosub _FnDim : @(C+12) = Pop()
    @(C) = 1                           ' initialize array while we're at it
  Next

  C =  0                               ' no columns printed yet
  Q =  0                               ' keep track of position

  Do                                   ' get day of the week
    Push @(C+6) : Gosub _FnDow : Z = Pop ()

    If @(C) - 1 < @(C + 12) Then
                                       ' if a position requested BEFORE
       If (Z*3 + C*22) < Q Then        ' the current position
          Print                        ' then terminate the line
          Q = 0                        ' and reset the counter
       Endif
                                       ' goto required position on screen
       Push (Z*3 + C*22) - Q : Gosub _Space
       Q = Z*3 + C*22                  ' set counter to that position

       If @(C) < 10 Then               ' if only one digit date
          Print " ";                   ' then compensate
       Endif

       Print @(C);                     ' print the date
       Q = Q + 2                       ' and update the position

       @(C) = @(C) + 1
       @(C+6) = @(C+6) + 1
    Endif
                                       ' increment columns,
    If (Z = 6) + (@(C)>@(C+12)) Then   ' wrap around if required
       C = (C + 1) % 6
    EndIf
                                       ' loop until all dates are printed
    Until (@(0)>@(0+12))*(@(1)>@(1+12))*(@(2)>@(2+12))*(@(3)>@(3+12))*(@(4)>@(4+12))*(@(5)>@(5+12))
  Loop

  Print                                ' terminate row with LF
Next

End

                                       ' convert D/M/Y to Julian date
_FnMjd                                 ' (day month year -- julian date)
  R = Pop()                            ' pop year
  S = Pop() - 3                        ' pop month
  T = Pop()                            ' pop day

  If S < 0 Then                        ' if month negative
    S = S + 12                         ' add 12 months
    R = R - 1                          ' and subtract a year
  Endif

  Push T + (153*S+2)/5 + R*365 + R/4 - R/100 + R/400 - 678882
Return

                                       ' calculate day of the week
_FnDow                                 ' (julian -- day-of-week)
  Push (Pop()+2400002)%7               ' convert julian date to day of week
Return

                                       ' calculate days in month
_FnDim                                 ' (month year -- #days)
  R = Pop()                            ' pop year
  S = Pop()                            ' pop month

  If S = 2 Then                        ' if February, get no. days
    Push 28 + ((R%4)=0) - ((R%100)=0) + ((R%400)=0)
  Else                                 ' otherwise, either 30 or 31
    If (S = 4) + (S = 6) + (S = 9) + (S = 11) Then
      Push 30
    Else
      Push 31
    Endif
  Endif
Return

                                       ' print a number of spaces
_Space                                 ' (#spaces --)
  For X = 1 To Pop()                   ' print n spaces
    Print " ";
  Next
Return

                                       ' month name subroutines
001 Print " January "; : Return        ' all have a fixed length
002 Print " February"; : Return        ' which simplifies things
003 Print "  March  "; : Return
004 Print "  April  "; : Return
005 Print "   May   "; : Return
006 Print "  June   "; : Return
007 Print "  July   "; : Return
008 Print " August  "; : Return
009 Print "September"; : Return
010 Print " October "; : Return
011 Print " November"; : Return
012 Print " December"; : Return