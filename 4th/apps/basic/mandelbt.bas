' Mandelbrot set - uBasic version J.L. Bezemer 2014
' uBasic does not support floating point calculations, so fixed point
' arithmetic is used, with Value 10000 representing 1.0. The Mandelbrot image
' is drawn using ASCII characters 1-9 to show number of iterations. Iteration
' count 10 or more is represented with '@'. To compensate the aspect ratio of
' the font, step sizes in x and y directions are different.

A =-21000                              ' Left Edge = -2.1
B = 15000                              ' Right Edge = 1.5
C = 15000                              ' Top Edge = 1.5
D =-15000                              ' Bottom Edge = -1.5
E = 200                                ' Max Iteration Depth
F = 350                                ' X Step Size
G = 750                                ' Y Step Size

For L = C To D Step -G                 ' Y0
    For K = A To B-1 Step F            ' X0
        V = 0                          ' Y
        U = 0                          ' X
        I = 32                         ' Char To Be Displayed
        For O = 0 To E-1               ' Iteration
            X = (U/10 * U) / 1000      ' X*X
            Y = (V/10 * V) / 1000      ' Y*Y
            If (X + Y > 40000)
                I = 48 + O             ' Print Digit 0...9
                If (O > 9)             ' If Iteration Count > 9,
                    I = 64             '  Print '@'
                Endif
                Break
            Endif
            Z = X - Y + K              ' Temp = X*X - Y*Y + X0
            V = (U/10 * V) / 500 + L   ' Y = 2*X*Y + Y0
            U = Z                      ' X = Temp
        Next
        Gosub I                        '  Ins_char(I)
    Next
    Print
Next

End
                                       ' Translate number to ASCII
32 Print " "; : Return
48 Print "0"; : Return
49 Print "1"; : Return
50 Print "2"; : Return
51 Print "3"; : Return
52 Print "4"; : Return
53 Print "5"; : Return
54 Print "6"; : Return
55 Print "7"; : Return
56 Print "8"; : Return
57 Print "9"; : Return
64 Print "@"; : Return