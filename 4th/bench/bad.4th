\ Copyright 1997, Chris Jakeman

\   0       harmless, no need to detect action.
\  -2      prevents action, reports cause and location, continues at Forth prompt.
\  -3      prevents action, reports cause only, continues at Forth prompt.
\  -4      prevents action, reports "Error found", continues at Forth prompt.
\  -6      executes action, but values returned are incorrect.
\  -7      prevents action, reports cause only, empties dictionary, continues at Forth prompt.
\ -10     prevents action, reports cause and location, exits Forth.
\ -11     prevents action, reports cause only, exits Forth.
\ -12     prevents action, reports "Error found", exits Forth.
\ -14     no message, exits Forth.
\ -16     no message, hangs Forth but computer need not be re-booted.
\ -18     no message, hangs Forth and computer must be re-booted.
\ -20     no message, corrupts Forth which hangs at some later time.

[pragma] ignorenumbers
include lib/mixed.4th                  \ for UM/MOD
include lib/interprt.4th               \ for INTERPRET

\ Find a value not likely to be an execution token.
1 ALIGNED CONSTANT NotAnXT 
\ Find an address likely to be outside the data space.
-4 ALIGNED CONSTANT NotADataAddress 
\ Find an address which is likely to be unaligned.
HERE ALIGNED 1+ CONSTANT NotAligned
\ Create a : word which cannot be a macro.
: _DROP DROP ;

: BadExecute ( -- ) NotAnXT EXECUTE ;
: BadAddress ( -- ) NotADataAddress @ DROP ;
: BadAlign ( -- ) NotAligned @ DROP ;
: BadReturn ( -- ) NotAnXT >R ;
: BadCode ( -- ) ['] _DROP 20 0 FILL 0 ['] _DROP EXECUTE ; \ Try executing it
: BadRes1 ( -- n1 n2 ) 9 0 /MOD ;      \ Divide by zero
: BadRes2 ( -- ) 0 1 1 UM/MOD ;        \ Out of range
: BadBreak ( -- u1 u2 ) BEGIN AGAIN ;  \ Try to interrupt loop
: BadNumb ( -- ) 1 1 BASE ! . ;        \ Loops forever?

create BadWords
  ," BadExecute" ' BadExecute ,
  ," BadAddress" ' BadAddress ,
  ," BadAlign"   ' BadAlign   ,
  ," BadReturn"  ' BadReturn  ,
  ," BadCode"    ' BadCode    ,
  ," BadRes1"    ' BadRes1    ,
  ," BadRes2"    ' BadRes2    ,
  ," BadBreak"   ' BadBreak   ,
  ," BadNumb"    ' BadNumb    ,
  NULL ,

BadWords to dictionary

\ The interpreter itself
: Bad
  begin                                \ show the prompt and get a command
    ." OK" cr refill drop              \ interpret and issue oops when needed
    ['] interpret catch dup if decimal ." Signal " . cr else drop then
  again                                \ repeat command loop eternally
;

Bad