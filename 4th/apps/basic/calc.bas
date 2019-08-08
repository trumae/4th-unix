rem Calculator Application (CALC.BAS)
rem A simple calculator application.
rem Version 2.1.0
rem Made by Joshua Beck
rem Released under the GNU General Public Licence version 3
rem Send any bugs, ideas or comments to zerokelvinkeyboard@gmail.com

_MAIN
  REM main menu
  PRINT
  PRINT "> Calculator <"
  PRINT
  PRINT "1| Simple Calculations 2| Advanced Maths 3| About 4| Exit"
  DO
    INPUT "Your choice: ";V
    UNTIL (V > 0) * (V < 5)
  LOOP
  IF V = 1 THEN GOSUB _BASEMATH
  IF V = 2 THEN GOSUB _ADVMATH
  IF V = 3 THEN GOSUB _ABOUT
  IF V = 4 THEN END
GOTO _MAIN

_BASEMATH
  REM start the menu loop
  DO
    REM se the menu title
    PRINT
    PRINT "> Simple Calculations <"
    PRINT
    REM set items in the menu
    PRINT "1| Addition 2| Subtraction 3| Multiplication 4| Division 5| Back"
    REM call a menu
    DO
      INPUT "Your choice: ";V
      UNTIL (V > 0) * (V < 6)
    LOOP
    REM find out what they selected and gosub there
    IF V = 1 THEN GOSUB _ADD
    IF V = 2 THEN GOSUB _SUB
    IF V = 3 THEN GOSUB _MUL
    IF V = 4 THEN GOSUB _DIV
  REM present the menu again unless 'back' was selected
    UNTIL V = 5
  LOOP 
  V = 0
RETURN

_ADD
  REM set the title
  PRINT
  PRINT "> Addition <"
  REM first input prompt
  INPUT "Input first number... ";a
  REM second input prompt
  INPUT "Input second number... ";b
  REM do the actual calculation
  REM the first input is A and the second is B
  a = a + b
  REM prompt above first number
  PRINT "Answer is: ";a
  REM back to main menu
RETURN

_SUB
  PRINT
  PRINT "> Subtraction <"
  INPUT "Input number to subtract from... ";a
  INPUT "Input number to subtract... ";b
  A = A - B
  PRINT "Answer is: ";a
RETURN

_MUL
  PRINT
  PRINT "> Multiplication <"
  INPUT "Input first number... ";a
  INPUT "Input second number... ";b
  A = A * B
  PRINT "Answer is: ";a
RETURN

_DIV
  PRINT
  PRINT "> Division <"
  INPUT "Input number to be divided... ";a
  INPUT "Input number to divide by... ";b
  REM define error message
  REM if the divisor is zero then present this error
  IF B = 0 THEN PRINT "Attempted to divide by zero!"
  IF B = 0 THEN RETURN
  D = A / B
  E = A % B
  A = D
  B = E
  PRINT "Answer is: ";a
  PRINT "Reminder is: ";b
RETURN

_ADVMATH
  DO
    PRINT
    PRINT "> Advanced Maths <"
    PRINT
    PRINT "1| Square/Cube Number 2| Power 3| Mass Addition 4| Mass Subtraction 5| Back"
    DO
      INPUT "Your choice: ";V
      UNTIL (V > 0) * (V < 6)
    LOOP
    IF V = 1 THEN GOSUB _SQUARE
    IF V = 2 THEN GOSUB _POWER
    IF V = 3 THEN GOSUB _MASSADD
    IF V = 4 THEN GOSUB _MASSTAKE
    UNTIL V = 5
  LOOP
RETURN

_SQUARE
  PRINT
  PRINT "> Square/Cube Number <"
  INPUT "Input a number to square and cube: ";a
  D = A
  A = A * D
  B = A * D
  PRINT "Number Squared is: ";a
  PRINT "Number Cubed is: ";b
RETURN

_POWER
  PRINT
  PRINT "> Power <"
  INPUT "Input a number: ";a
  INPUT "Input power to raise to: ";b
  D = A
  IF B = 0 THEN A = 1
  IF B = 0 THEN GOTO _POWERSKIP
  IF B = 1 THEN GOTO _POWERSKIP
  DO
    A = A * D
    B = B - 1
    UNTIL B = 1
  LOOP
  _POWERSKIP
  PRINT "Answer is: ";a
RETURN

_MASSADD
  PRINT
  PRINT "> Mass Add <"
  INPUT "Enter the base number: ";a
  INPUT "Enter the first number to add: ";b
  N = A
  N = N + B
_ADDMORE
  INPUT "Enter another number to add (or zero to finish the sum): ";v
  N = N + V
  IF V > 0 THEN GOTO _ADDMORE
  B = N
  PRINT "The base number was: ";a
  PRINT "The total was: ";b
RETURN

_MASSTAKE
  PRINT
  PRINT "> Mass Subtract <"
  INPUT "Enter the base number: ";a
  INPUT "Enter the first number to take: ";b
  N = A
  N = N - B
_TAKEMORE
  INPUT "Enter another number to take (or zero to finish the sum): ";v
  N = N - V
  IF V > 0 THEN GOTO _TAKEMORE
  B = N
  PRINT "The base number was: ";a
  PRINT "The total was: ";b
RETURN

_ABOUT
  REM About message (strings have an 128 character limit)
  PRINT
  PRINT "            > About <"
  PRINT
  PRINT "    Calculator, version 2.1.0"
  PRINT "An advanced calculator application"
  PRINT "  Released under the GNU GPL v3"
  PRINT "     Written in MikeOS BASIC"
  PRINT "      Ported to uBasic/4tH"
RETURN
