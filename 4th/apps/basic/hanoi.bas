' In this task, the goal is to solve the Towers of Hanoi problem
' with recursion.

Proc  _Move(4, 1,2,3)                  ' 4 disks, 3 poles
End

_Move Param(4)
  If (a@ > 0) Then
    Proc _Move (a@ - 1, b@, d@, c@)
    Print "Move disk from pole ";b@;" to pole ";c@
    Proc _Move (a@ - 1, d@, c@, b@)
  EndIf
Return