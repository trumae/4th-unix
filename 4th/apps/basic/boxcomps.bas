' "Box the compass" for uBasic, Copyright J.L. Bezemer 2014
' You can redistribute this file and/or modify it under
' the terms of the GNU General Public License

' Create a function that takes a heading in degrees and returns the correct
' 32-point compass heading. Use the function to print and display a table of
' Index, Compass point, and Degree; rather like the corresponding columns
' from, the first table of the wikipedia article, but use only the following
' 33 headings as input:

Push 0, 1687, 1688, 3375, 5062, 5063, 6750, 8437, 8438, 10125, 11812, 11813
Push 13500, 15187, 15188, 16875, 18562, 18563, 20250, 21937, 21938, 23625
Push 25312, 25313, 27000, 28687, 28688, 30375, 32062, 32063, 33750, 35437
Push 35438                             ' Use the stack as a DATA statement

For x = 32 To 0 Step -1                ' Now read the values, but reverse
  @(x) = Pop()                         ' Since the last value is on top
Next                                   ' of the data stack

For x = 0 To 32                        ' Here comes the payload

  j = ((@(x) * 32 / 3600) + 5) / 10    ' Scale by ten, then correct

  Print Using "_#";(j % 32) + 1;"  ";  ' Print heading
  GoSub 100 + ((j % 32) * 10)          ' Now get the compass point
  Print Using "__#.##"; @(x)           ' Finally, print the angle
                                       ' which is scaled by 100
Next

End
                                       ' All compass points
100 Print "North                 "; : Return
110 Print "North by east         "; : Return
120 Print "North-northeast       "; : Return
130 Print "Northeast by north    "; : Return
140 Print "Northeast             "; : Return
150 Print "Northeast by east     "; : Return
160 Print "East-northeast        "; : Return
170 Print "East by north         "; : Return
180 Print "East                  "; : Return
190 Print "East by south         "; : Return
200 Print "East-southeast        "; : Return
210 Print "Southeast by east     "; : Return
220 Print "Southeast             "; : Return
230 Print "Southeast by south    "; : Return
240 Print "South-southeast       "; : Return
250 Print "South by east         "; : Return
260 Print "South                 "; : Return
270 Print "South by west         "; : Return
280 Print "South-southwest       "; : Return
290 Print "Southwest by south    "; : Return
300 Print "Southwest             "; : Return
310 Print "Southwest by west     "; : Return
320 Print "West-southwest        "; : Return
330 Print "West by south         "; : Return
340 Print "West                  "; : Return
350 Print "West by north         "; : Return
360 Print "West-northwest        "; : Return
370 Print "Northwest by west     "; : Return
380 Print "Northwest             "; : Return
390 Print "Northwest by north    "; : Return
400 Print "North-northwest       "; : Return
410 Print "North by west         "; : Return
420 Print "North                 "; : Return
