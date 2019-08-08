' Weasel program - Copyright 2015 J.L Bezemer
' You can redistribute this file and/or modify it under
' the terms of the GNU General Public License

' The accurate way to describe Richard Dawkins’s "weasel" program is this way:
' 1. Use a set of characters that includes the upper case alphabet and
'    a space.
' 2. Initialize a population of n 28-character strings with random
'    assignments of characters from our character set.
' 3. Identify the string or strings closest to the target string in the
'    population.
' 4. If a string matches the target, terminate.
' 5. Base a new generation population of size n upon copies of the closest
'    matching string or strings, where each position has a chance of randomly
'    mutating, based upon a set mutation rate.
' 6. Go to step 3.

T = 0                                  ' Address of target
L = 28                                 ' Length of string
P = T + L                              ' Address of parent
R = 6                                  ' Mutation rate in percent
C = 7                                  ' Number of children
B = 0                                  ' Best rate so far

Proc _Initialize                       ' Initialize

Do                                     ' Now start mutating
  I = 0                                ' Nothing does it better so far

  For x = 2 To C+1                     ' Addresses of children
    Proc _MutateDNA (x, P, R)          ' Now mutate their DNA
    F = FUNC(_Fitness (x, T))          ' Check for fitness
    If F > B Then B = F : I = x        ' If fitness of child is better
  Next                                 ' Make it the best score

  If I Then                            ' If a better child was found
    Proc _MakeParent (P, I)            ' Make the child the parent
    Proc _PrintParent (P)              ' Print the new parent
  EndIf

  Until B = L                          ' Until top score equals length
Loop

End


_MutateDNA Param(3)                    ' Mutate an entire DNA
  Local(1)

  For d@ = 0 To L-1                    ' For the entire string
    If c@ > RND(100) Then              ' If mutation rate is met
       @(a@*L+d@) = ORD("A") + RND(27) ' Mutate the gene
    Else
       @(a@*L+d@) = @(b@+d@)           ' Otherwise copy it from the parent
    EndIf
  Next
Return


_Fitness Param(2)                      ' Check for fitness
  Local(2)

  c@ = 0                               ' Fitness is zero
  For d@ = 0 To L-1                    ' For the entire string
    If @(a@*L+d@) = @(b@+d@) Then c@ = c@ + 1
  Next                                 ' If string matches, increment score
Return (c@)                            ' Return the fitness


_MakeParent Param(2)                   ' Make a child into a parent
  Local(1)

  For c@ = 0 To L-1                    ' For the entire string
    @(a@+c@) = @(b@*L+c@)              ' Copy the DNA gene by gene
  Next
Return


_PrintParent Param(1)                  ' Print the parent
  Local(1)

  For b@ = 0 To L-1                    ' For the entire string
    If (@(a@+b@)) > ORD("Z") Then      ' Space is CHAR(Z+1)
      Print " ";                       ' So translate it for printing
    Else
      Print CHR(@(a@+b@));             ' Print a gene
    EndIf
  Next

  Print                                ' Issue a linefeed
Return


_Initialize                            ' Initialize target and parent
  @(0)=ORD("M")                        ' Initialize target (long!)
  @(1)=ORD("E")                        ' Character by character
  @(2)=ORD("T")
  @(3)=ORD("H")
  @(4)=ORD("I")
  @(5)=ORD("N")
  @(6)=ORD("K")
  @(7)=ORD("S")
  @(8)=ORD("Z")+1
  @(9)=ORD("I")
  @(10)=ORD("T")
  @(11)=ORD("Z")+1
  @(12)=ORD("I")
  @(13)=ORD("S")
  @(14)=ORD("Z")+1
  @(15)=ORD("L")
  @(16)=ORD("I")
  @(17)=ORD("K")
  @(18)=ORD("E")
  @(19)=ORD("Z")+1
  @(20)=ORD("A")
  @(21)=ORD("Z")+1
  @(22)=ORD("W")
  @(23)=ORD("E")
  @(24)=ORD("A")
  @(25)=ORD("S")
  @(26)=ORD("E")
  @(27)=ORD("L")

  Proc _MutateDNA (P/L, P, 100)          ' Now mutate the parent DNA
Return