' uBasic equation solver - Copyright 2014 J.L. Bezemer
' You can redistribute this file and/or modify it under
' the terms of the GNU General Public License

' ****************
' ** Sample run **
' ****************

' X (1) or Number (2)?      1
' Enter a non-zero number:  5
' - (1), + (2) or = (3)?    2
' X (1) or Number (2)?      2
' Enter a non-zero number:  -20
' - (1), + (2) or = (3)?    3
' X (1) or Number (2)?      2
' Enter a non-zero number:  25
' - (1), + (2) or STOP (3)? 1
' X (1) or Number (2)?      1
' Enter a non-zero number:  10
' - (1), + (2) or STOP (3)? 3

' 5x + -20 = 25 - 10x => 15x = 45 => x = 3


C = 0                                  ' Bytecode counter
S = 1                                  ' Left or right side of equation
P = 1                                  ' Effective sign

X = 0                                  ' Accumulator for X-es
Y = 0                                  ' Accumulator for numbers

Do
  GoSub _Operand                       ' Get an operand
  @(C) = Pop()                         ' If it is a variable, signal it
  If @(C) = 0 Then C = C + 1 : V = 1   ' Compile "variable" marker
  @(C) = Pop()                         ' Compile operand

  If V Then                            ' If it is a variable
     X = X + (P * S * @(C )) : V = 0   ' Then update X accumulator
  Else                                 ' Otherwise
     Y = Y + (P * -S * @(C))           ' Update the Y acccumulator
  EndIf

  C = C + 1                            ' Increment counter

  GoSub _Operator                      ' Get an operator
  @(C) = Tos() : C = C + 1             ' And compile it
  P = -(Tos() = 1) + (Tos() # 1)       ' Get the effective sign
  If (S = 1) * (Tos() = 3) Then S = -1 : Push Pop() - 3
                                       ' Did we pass to the other side?
  Until (S = -1) * (Pop() = 3)         ' Stop when on the right side
Loop

@(C - 1) = 4 : Print                   ' Terminate bytecode

GoSub _PrintEquation                   ' And print the equation
Print " => ";X;"x = ";Y;" => x = ";Y/X ' Solve the equation
If Y%X Then Print: Print "(remainder = ";Y%X;")"
                                       ' Warn for any remainder
End


_PrintEquation                         ' Print the equation
  Local (1)
  a@ = 0                               ' Reset the bytecode counter

  Do
    If @(a@) = 0 Then                  ' If it is a variable
       a@ = a@ + 1                     ' Increment the counter
       Print @(a@);"x";                ' And print the "X"
    Else
       Print @(a@);                    ' Otherwise print the number
    EndIf

    a@ = a@ + 1                        ' Increment the counter

    If @(a@) = 1 Then Print " - ";     ' Print the appropriate
    If @(a@) = 2 Then Print " + ";     ' Operator
    If @(a@) = 3 Then Print " = ";     ' If we detect the terminator
    If @(a@) = 4 Then Break            ' Then break out of the loop

    a@ = a@ + 1                        ' Increment the counter
  Loop
Return


_Operand                               ' Get an operand
  Local (2)
  Do                                   ' Get the operand type
    Input "X (1) or Number (2)?      ";a@
    Until (a@ = 1) + (a@ = 2)          ' If not appropriate, loop
  Loop

  Do                                   ' Get the operand itself
    Input "Enter a non-zero number:  ";b@
    Until b@ # 0                       ' If not appropriate, loop
  Loop

  Push b@, a@ - 1                      ' Push operand and type
Return


_Operator                              ' Get an operator
  Local (1)
  Do                                   ' If we're on the right side
    If S = -1 Then                     ' Of the equation, allow to stop
      Input "- (1), + (2) or STOP (3)? ";a@
    Else                               ' Otherwise allow and equals sign
      Input "- (1), + (2) or = (3)?    ";a@
    EndIf

    Until (a@ > 0) + (a@ < 4)          ' If not appropriate, loop
  Loop

  Push (a@)                            ' Push operator and return
Return
