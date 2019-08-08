' "Not One" for uBasic, J.L. Bezemer 2013,2014

Print "The game of 'Notone' is played with two players and a pair of dice."
Print "There are ten rounds in the game. One round consisting of one turn"
Print "for each player. Players (yourself and the computer) add the score"
Print "they attain on each round. And the player with the highest score"
Print "after ten rounds is the winner."
Print
Print "On each turn the player may roll the two dice from 1 to n times. If"
Print "T1 is the total of dice on the Ith roll, then the players score for"
Print "the turn is T(1)+T(2)+T(3)+..T(N)."
Print
Print "However, and here's the catch. If any T(I)is equal to T(1) then the"
Print "turn is over and his score for that round is zero. After each roll"
Print "that doesn't equal T(1), the player can decide whether to roll again"
Print "or stop and score the number of points already obtained."
Print

Local (3)

b@ = 0 : c@ = 0                       ' set up the scores

For a@ = 1 To 10                      ' for 10 rounds
  Print "Round ";a@ : Print
  b@ = Func (_Player (b@))            ' Your turn
  c@ = Func (_Computer (b@, c@))      ' The computers turn
Next

Print "Final score","You","Computer"  ' Print final score
Print "Total points:", b@, c@

End


_Again?
  Local (1)

  Do                                   ' get confirmation
    Input "Roll again? (Yes=1, No=0): "; a@
    Until (a@ = 0) + (a@ = 1)
  Loop
Return (a@)


_RollDice
Return ((Rnd(6)+1)+(Rnd(6)+1))         ' Roll two dice


_Player Param (1)
  Local (3)

  Print "Your turn:"                   ' Players turn
  b@ = Func (_RollDice) : c@ = b@ : Print b@
                                       ' Set up initial result
  Do                                   ' Ask for another one
    Until Func (_Again?) = 0           ' Not another roll

    d@ = Func (_RollDice)              ' Roll the dice
    Print d@

    If d@ = b@ Then                    ' If failed ..
      c@ = 0                           ' Set result to zero
      Print "You get a zero for this round."
      Break                            ' We failed..
    Else                               ' Otherwise
      c@ = c@ + d@                     ' Add to the result
    Endif

  Loop

  Print "You got ";a@ + c@;" points."  ' Print the score
  Print
Return (a@ + c@)


_Computer Param (2)
  Local (5)

  Print "Computers turn:"              ' Computers turn
  c@ = Func (_RollDice) : d@ = c@ : e@ = 1
  Print "Computer's roll ";e@;" : ";c@
                                       ' determine stategy
  f@ = 5 + 2 * ((b@ + 36) < a@) + Rnd(2) + 2 * ((c@ < 4) + (c@ > 10))

  Do Until e@ = f@                     ' Now apply the "strategy"

    e@ = e@ + 1 : g@ = Func (_RollDice)
    Print "Computer's roll ";e@;" : ";g@
                                       ' Roll the dice
    If g@ = c@ Then                    ' If failed ..
      d@ = 0                           ' Set result to zero
      Print "The computer gets a zero for this round."
      Break                            ' We failed ,,
    Else                               ' Otherwise
      d@ = d@ + g@                     ' Add to the result
    Endif

  Loop

  Print "The computer got ";b@ + d@;" points."
  Print                                ' Print the score
Return (b@ + d@)