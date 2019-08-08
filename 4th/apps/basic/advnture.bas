
REM ------------------------------------------------
REM ADVENTURE 2.3 -- By Justin Tokarchuk
REM For MikeOS and MikeBasic Derivatives
REM ------------------------------------------------

REM VARS:
REM $1 = Player Name
REM a = room number
REM b = got note
REM c = got candle
REM d = opened treasure chest
REM e = got key
REM g = command

a = 1
b = 0
c = 0
d = 0
e = 0
g = 0


_LOGO
  PRINT "          _______     __                     __                     __ "
  PRINT "         |   _   |.--|  |.--.--.-----.-----.|  |_.--.--.----.-----.|  |"
  PRINT "         |       ||  _  ||  |  |  -__|     ||   _|  |  |   _|  -__||__|"
  PRINT "         |___|___||_____| \\___/|_____|__|__||____|_____|__| |_____||__|"
  PRINT " "
  PRINT "         --------------------------------------------------------------"
  PRINT "                        |  A Text-Based Adventure Game  |              "
  PRINT "                         -------------------------------               "
  PRINT ""
  PRINT ""

_GAMEINTRO
  PRINT ""
  PRINT " HALLOWEEN NIGHT. The spookiest night of the year! Not very spooky"
  PRINT " in your cruddy room, though. So what if toilet paper takes hours"
  PRINT " to clean off of MR. RAUL's house. Why did I have to get grounded?"
  PRINT " SCREW IT, I am mighty! I will not be held down by parents!"
  PRINT " I'm going to sneak out, and prove to everyone that MR. RAUL's house"
  PRINT " is not haunted!"
  PRINT
  PRINT " You sneak down the stairs from your room, and notice your mother is"
  PRINT " fast asleep on the couch! Opportunity is knocking! You dart through"
  PRINT " your front door and across the street to MR. RAUL's house."
  PRINT " You notice the door is cracked open, you push the door open and walk"
  PRINT " inside."
  PRINT
  PRINT " -- SLAM!! -- Oh no! The door swings shut behind you! -- CLICK! --"
  PRINT " You examine the door to find a padlock holding it shut!"
  GOSUB _MOVEROOM
  GOTO _PARSER 
 
_MOVEROOM
  IF a = 1 THEN GOSUB _R1
  IF a = 2 THEN GOSUB _R2
  IF a = 3 THEN GOSUB _R3
  IF a = 4 THEN GOSUB _R4
  IF a = 5 THEN GOSUB _R5
  IF a = 6 THEN GOSUB _R6
  RETURN

_R1
  PRINT ""
  PRINT " -- The House Entrance -- "
  PRINT " The entrance of the house."
  PRINT " There is a large padlock behind you, barring your freedom." 
  RETURN

_R2
  PRINT ""
  PRINT " -- The Dining Room -- "
  PRINT " There is a large table in the middle of the room. There are multiple"
  PRINT " doors going out of this room. You see a large painting."
  RETURN

_R3
  PRINT ""
  PRINT " -- The Kitchen -- "
  PRINT " There is a doorway in this room, with the door ripped off of the"
  PRINT " hinges. Odd, you think. The rest of the kitchen seems immaculate."
  RETURN

_R4
  PRINT ""
  PRINT " -- The Bathroom -- "
  PRINT " Odd, for being a bathroom there are no windows or methods of"
  PRINT " ventilation.";
  IF c = 0 THEN PRINT " There is a candle sitting atop the sink."
  IF c = 1 THEN PRINT
  RETURN

_R5
  PRINT ""
  PRINT " -- The Bedroom -- "
  PRINT " A door leads back to the dining room." 
  RETURN

_R6
  PRINT ""
  PRINT " -- The Basement -- "
  IF c = 1 THEN PRINT " + You light your candle."
  IF c = 1 THEN PRINT " There is a treasure Chest on the floor."
  IF c = 0 THEN PRINT " It is too dark to see anything in here."
  RETURN

_PARSER
  x = 0

  PRINT
  PRINT " (1) North (2) South (3) West  (4) East"
  PRINT " (5) Look  (6) List  (7) Take  (8) Examine"
  PRINT " (9) Use  (10) Open (11) Help (12) Unlock"
  PRINT "(13) Read (14) Exit" : PRINT
  INPUT "Command: ";g

  IF g = 1 THEN GOSUB _NORTH
  IF g = 2 THEN GOSUB _SOUTH
  IF g = 3 THEN GOSUB _WEST
  IF g = 4 THEN GOSUB _EAST
  IF g = 5 THEN GOSUB _MOVEROOM
  IF g = 6 THEN GOSUB _INVENTORY
  IF (g = 7) * (a = 2) * (b = 0) THEN b = 1
  IF (g = 7) * (a = 2) * (b = 1) THEN PRINT " You've taken the note."
  IF (g = 7) * (a = 4) * (c = 0) THEN GOSUB _CANDLE
  IF (g = 8) * (a = 1) THEN PRINT " A worn-out, stained old rug."
  IF (g = 8) * (a = 2) THEN PRINT " A grandiose hardwood table."
  IF (g = 8) * (a = 2) * (b = 0) THEN PRINT " A note sits atop it."
  IF (g = 8) * (a = 2) THEN PRINT " There is a picture of MR. RAUL"
  IF g = 9 THEN GOSUB _NOTE
  IF g = 10 THEN GOSUB _TREASURECHEST
  IF g = 11 THEN GOSUB _HELP
  IF (g = 12) * (a = 1) * (e = 1) THEN GOTO _GAMEEND
  IF g = 13 THEN GOSUB _NOTE
  IF g = 14 THEN END
  IF (g < 0) + (g > 14) THEN PRINT " Confused? Need a hand? Type 11 for a list of commands!"
  GOTO _PARSER

_CANDLE
  PRINT " + You take the candle from the sink."
  c = 1
  RETURN

_TREASURECHEST
  IF e = 1 THEN PRINT " You already have the treasure!"
  IF e = 1 THEN RETURN
  IF (a = 6) * (c = 1) THEN e = 1
  IF e = 1 THEN PRINT " + You open the treasure chest and take a KEY out of it."
  RETURN

_NOTE
  IF b = 1 THEN PRINT " The note reads:"
  IF b = 1 THEN PRINT " The secret to your freedom lies in a box!"
  RETURN

_GAMEEND
  PRINT " You unlock the door and rush outside as you gasp the free air!"
  PRINT " Nightfall is close and you almost had to spend the night! You "
  PRINT " decide that it would be wise to run home before mom wakes."
  PRINT " GAME OVER! Thanks for playing!"
  END

_INVENTORY
  PRINT ""
  IF (b = 0) * (c = 0) * (e = 0) THEN GOSUB _EMPTY
  IF b = 1 THEN PRINT " NOTE"   
  IF c = 1 THEN PRINT " CANDLE"  
  IF e = 1 THEN PRINT " KEY"
  RETURN

_EMPTY
  LET x = RND(5) + 1
  IF X = 1 THEN PRINT " Nothing. Not even so much as a fly out of your packsack."
  IF X = 2 THEN PRINT " You wonder why your packsack is so light, it's empty."
  IF X = 3 THEN PRINT " Your packsack has a surprising emptyness."
  IF X = 4 THEN PRINT " Apart from several dead flies in your packsack, it's empty."
  IF X = 5 THEN PRINT " You're packsack is full of loot!"
  IF X = 5 THEN PRINT " Not really, it's empty."
  RETURN

_NODIR
  LET x = RND(3) + 1
  IF x = 1 THEN PRINT " ..So that's how the wall feels on my face. Excellent."
  IF x = 2 THEN PRINT " You cannot go that way."
  IF x = 3 THEN PRINT " You win!"
  IF x = 3 THEN PRINT " .... Just kidding." 
  RETURN

_NORTH
  REM -- ENTRANCE TO DINING ROOM --
  IF a = 1 THEN x = 1
  IF a = 1 THEN a = 2
  IF x = 1 THEN GOSUB _MOVEROOM
  IF x = 1 THEN RETURN
  REM -- DINING ROOM TO KITCHEN --
  IF a = 2 THEN x = 2
  IF a = 2 THEN a = 3
  IF x = 2 THEN GOSUB _MOVEROOM
  IF x = 2 THEN RETURN
  GOSUB _NODIR  
  RETURN

_WEST
  REM -- ENTRANCE TO BEDROOM --
  IF a = 1 THEN x = 1
  IF a = 1 THEN a = 5
  IF x = 1 THEN GOSUB _MOVEROOM
  IF x = 1 THEN RETURN
  REM -- DINING ROOM TO BASEMENT --
  IF a = 2 THEN x = 2
  IF a = 2 THEN a = 6
  IF x = 2 THEN GOSUB _MOVEROOM
  IF x = 2 THEN RETURN
  REM -- KITCHEN TO BATHROOM
  IF a = 3 THEN x = 3
  IF a = 3 THEN a = 4
  IF x = 3 THEN GOSUB _MOVEROOM
  IF x = 3 THEN RETURN
  GOSUB _NODIR
  RETURN

_SOUTH
  REM -- DINING ROOM TO ENTRANCE
  IF a = 2 THEN x = 2
  IF a = 2 THEN a = 1
  IF x = 2 THEN GOSUB _MOVEROOM
  IF x = 2 THEN RETURN
  REM -- KITCHEN TO DINING ROOM --
  IF a = 3 THEN x = 3
  IF a = 3 THEN a = 2
  IF x = 3 THEN GOSUB _MOVEROOM
  IF x = 3 THEN RETURN
  GOSUB _NODIR
  RETURN

_EAST
  REM -- BATHROOM TO KITCHEN --
  IF a = 4 THEN x = 4
  IF a = 4 THEN a = 3
  IF x = 4 THEN GOSUB _MOVEROOM
  IF x = 4 THEN RETURN
  REM -- BEDROOM TO ENTRANCE --
  IF a = 5 THEN x = 5
  IF a = 5 THEN a = 1
  IF x = 5 THEN GOSUB _MOVEROOM
  IF x = 5 THEN RETURN
  REM -- BASEMENT TO DINING ROOM --
  IF a = 6 THEN x = 6
  IF a = 6 THEN a = 2
  IF x = 6 THEN GOSUB _MOVEROOM
  IF x = 6 THEN RETURN
  GOSUB _NODIR
  RETURN

_HELP
  PRINT ""
  PRINT " The following are valid commands ingame:"
  PRINT ""
  PRINT " NORTH, WEST, SOUTH, EAST  - MOVES TO INPUTTED DIRECTION"
  PRINT " LOOK                      - REPRINTS ROOM DESCRIPTION"
  PRINT " LIST                      - VIEW WHAT YOU ARE CARRYING"
  PRINT " TAKE (OBJECT)             - TAKES AN OBJECT"
  PRINT " EXAMINE (OBJECT)          - EXAMINES AN OBJECT"
  PRINT " USE (OBJECT)              - USES AN OBJECT"
  PRINT " OPEN (OBJECT)             - OPENS A CONTAINER"
  PRINT " HELP                      - VIEW THESE COMMANDS AGAIN"
  PRINT " UNLOCK (OBJECT)           - ATTEMPTS TO UNLOCK AN OBJECT"
  PRINT " EXIT                      - QUITS THE GAME"
  PRINT
  RETURN
