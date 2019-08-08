' Hamlet - the fencing match

Print "Shakespeare's 'Hamlet' - the fencing match."
Print
Print "   Claudius bets that Laertes cannot exceed Hamlet by 3 hits in 12 passes."
Print "   Because Laertes is nominally a better fencer than Hamlet, Claudius has"
Print "   laid on odds of 12 for 9. Is this a good bet?" :
Print

Print "H-wins", "L-wins", "H-profit%"
P = 12                     'the maximum number of plays in a game
A = 0
B = 0

For T = 1 To 10
   R = 0
   S = 0

   For C = 1 To 1000
      H = 0
      L = 0
      N = 0

      For I = 1 To P
         X = Rnd(24) + 1

         If X < 10 Then
            H = H + 1
         Else
            L = L + 1
         Endif

         Until (H=5) + (L=8)
      Next

      If H=5 Then R = R + 1 : A = A + 1
      If L=8 Then S = S + 1 : B = B + 1
   Next

   Print R,S
Next

Print "----", "----"
Print A, B
Print "* 12","* 9"
Print A*12, B*9,
Z = (A*12 - B*9) * 10000  / (A*12 + B*9)
Print Z/100;".";Z%100
Print
Print Z/100;".";Z%100;

If Z > 0 Then
  Print "% profit shows that Claudius has made a good bet."
Else
  Print "% loss shows that Claudius has made a bad bet."
Endif