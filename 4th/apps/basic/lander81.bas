' Not Only 30 Programs For The Sinclair ZX81 1K
' Structured uBasic version, J.L. Bezemer 2014

V = Rnd(500)
H = 2000
R = 6000

Do
  Print "","Moon Lander"
  Print
  If H < 1 Then H = 0
  Print "","Speed ";V
  Print "","Dist ";H
  Print "","Fuel ";R
  Print

  If H > 0 Then
     Print "Thrust (0-99)";
     Input F
     Print "Time (1-6)   ";
     Input T

     If F*T > R/10 Then
        F = R/(10*T)
     Endif

     R = R-F*T*10
     A = F-32
     H = A*T^2+V*T+H
     V = 2*A*T+V
   Else
     Print "Score=";100+V

     If V < -100 Then
        Print "Crash"
     Else
        Print "Landed"
     Endif

     End
   Endif
Loop

