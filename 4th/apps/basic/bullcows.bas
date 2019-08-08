' This is an old game played with pencil and paper that was later implemented
' on computer.

' The task is for the program to create a four digit random number from the
' digits 1 to 9, without duplication. The program should ask for guesses to
' this number, reject guesses that are malformed, then print the score for the
' guess.

' The score is computed as:

' 1. The player wins if the guess is the same as the randomly chosen number,
'    and the program ends.
' 2. A score of one bull is accumulated for each digit in the guess that
'    equals the corresponding digit in the randomly chosen initial number.
' 3. A score of one cow is accumulated for each digit in the guess that also
'    appears in the randomly chosen number, but in the wrong position.


Local(2)                               ' Let's use no globals

Proc _Initialize                       ' Get our secret number

Do                                     ' Repeat until it's guessed
  Do
    Input "Enter your guess: ";a@      ' Enter your guess
    While FUNC(_Invalid(a@))           ' but make sure it's a valid guess
  Loop

  a@ = FUNC(_Bulls)                    ' Count the number of bulls
  b@ = FUNC(_Cows)                     ' Count the number of cows
                                       ' Now give some feedback
  Print : Print "\tThere were ";a@;" bulls and ";b@;" cows." : Print
  Until a@ = 4                         ' Until the secret is guessed
Loop

Print "You win!"                       ' Yes, you guessed it

End


_Initialize                            ' Make a secret
  Local (1)

  Do
    a@ = 1234 + RND(8643)              ' Get a valid number
    While FUNC(_Invalid(a@))           ' and accept it unless invalid
  Loop

  For a@ = 0 to 3                      ' Now save it at the proper place
    @(a@+4) = @(a@)
  Next
Return


_Invalid Param(1)                      ' Check whether a number is valid
  Local(2)
                                       ' Ok, these can be right at all
  If (a@ < 1234) + (a@ > 9876) Then Return (1)
                                       ' Now break 'em up in different digits
  For b@ = 3 To 0 Step -1
    @(b@) = a@ % 10                    ' A digit of zero can't be right
    If @(b@) = 0 Then Unloop : Return (1)
    a@ = a@ / 10
  Next

  For b@ = 0 To 2                      ' Now compare all digits
    For c@ = b@ + 1 To 3               ' The others were already compared
      If @(b@) = @(c@) Then Unloop : Unloop : Return (1)
    Next                               ' Wrong, we found similar digits
  Next
Return (0)                             ' All digits are different


_Bulls                                 ' Count the number of valid guesses
  Local (2)

  b@ = 0                               ' Start with zero

  For a@ = 0 to 3                      ' Increment with each valid guess
    If @(a@) = @(a@+4) Then b@ = b@ + 1
  Next
Return (b@)                            ' Return number of valid guesses


_Cows
  Local (3)                            ' Count the number of proper digits

  c@ = 0                               ' Start with zero

  For a@ = 0 To 3                      ' All the players guesses
    For b@ = 4 To 7                    ' All the computers secrets
      If (a@+4) = b@ Then Continue     ' Skip the bulls
      If @(a@) = @(b@) Then c@ = c@ + 1
    Next                               ' Increment with valid digits
  Next
Return (c@)                            ' Return number of valid digits