' Implement the Drossel and Schwabl definition of the forest-fire model.

' It is basically a 2D cellular automaton where each cell can be in three
' distinct states (empty, tree and burning) and evolves according to the
' following rules (as given by Wikipedia)

' 1. A burning cell turns into an empty cell
' 2. A tree will burn if at least one neighbor is burning
' 3. A tree ignites with probability f even if no neighbor is burning
' 4. An empty space fills with a tree with probability p


B = 1                                  ' A burning tree
E = 16                                 ' An empty space
T = 256                                ' A living tree

Input "%Chance a tree will burn: ";F   ' Enter chance of combustion
Input "%Chance a tree will grow: ";P   ' Enter chance of a new tree

Proc _CreateForest                     ' Now create a new forest

Do
  Proc _PrintForest                    ' Print the current forest
  Input "Press '1' to continue, '0' to quit: ";A
While A                                ' Until the user has had enough
  Proc _BurnForest                     ' See what happens
  Proc _UpdateForest                   ' Update from buffer
Loop

End


_CreateForest                          ' Create an entire new forest
  Local(1)

  For a@ = 0 to 120                    ' For each main cell determine
    If RND(100) < P Then               ' if a tree will grow here
      @(a@) = T                        ' Ok, we got a tree
    Else                               ' Otherwise it remains empty
      @(a@) = E
    EndIf
  Next
Return


_BurnForest                            ' Now the forest starts to burn
  Local(2)

  For a@ = 0 To 10                     ' Loop vertical
    For b@ = 0 To 10                   ' Loop horizontal
      If @((a@ * 11) + b@) = B Then @((a@ * 11) + b@ + 121) = E
                                       ' A tree has been burned flat
      If @((a@ * 11) + b@) = E Then    ' For each open space determine
        If RND(100) < P Then           ' if a tree will grow here
          @((a@ * 11) + b@ + 121) = T
        Else                           ' Otherwise it remains an empty space
          @((a@ * 11) + b@ + 121) = E
        EndIf
      EndIf

      If @((a@ * 11) + b@) = T Then    ' A tree grows here
        If RND(100) < F Then           ' See if it will spontaneously combust
          @((a@ * 11) + b@ + 121) = B
        Else                           ' No, then see if it got any burning
          @((a@ * 11) + b@ + 121) = FUNC(_BurningTrees(a@, b@))
        EndIf                          ' neighbors that will set it ablaze
      EndIf

    Next
  Next
Return


_UpdateForest                          ' Update the main buffer
  Local(1)

  For a@ = 0 To 120                    ' Move from temporary buffer to main
    @(a@) = @(a@+121)
  Next
Return


_PrintForest                           ' Print the forest on screen
  Local(2)
  Print                                ' Let's make a little space

  For a@ = 0 To 10                     ' Loop vertical
    For b@ = 0 To 10                   ' Loop horizontal
      If @((a@ * 11) + b@) = B Then    ' This is a burning tree
        Print " *";
      Else                             ' Otherwise..
        If @((a@ * 11) + b@) = E Then  ' It may be an empty space
          Print "  ";
        Else                           ' Otherwise
          Print " @";                  ' It has to be a tree
        EndIf
      EndIf
    Next
    Print                              ' Terminate row
  Next

  Print                                ' Terminate map
Return


_BurningTrees Param(2)                 ' Check the trees environment
  Local(2)

  For c@ = a@-1 To a@+1                ' Loop vertical -1/+1
    If c@ < 0 Then Continue            ' Skip top edge
  Until c@ > 10                        ' End at bottom edge
    For d@ = b@-1 To b@+1              ' Loop horizontal -1/+1
      If d@ < 0 Then Continue          ' Skip left edge
    Until d@ > 10                      ' End at right edge
      If @((c@ * 11) + d@) = B Then Unloop : Unloop : Return (B)
    Next                               ' We found a burning tree, exit!
  Next                                 ' Try next row

Return (T)                             ' No burning trees found