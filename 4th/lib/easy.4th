\ Easy4tH  V1.5.18            A 4tH to ANS Forth interface

\ Typical usage:
\    4096 constant /string-space
\    s" easy.4th" included

\ This is an ANS Forth program requiring:
\      1. The word SLITERAL in the String word set
\      2. The word D>S in the Double word set
\      3. The word TIME&DATE in the Facility Ext. word set
\      4. The words CS-ROLL, [IF], [ELSE] and [THEN] in the
\         Tools Ext. word set.

\ (c) Copyright 1997, 2015 Wil Baden, Hans Bezemer.
\ Contributions by George Hubert, Stephen Pelc, Anton Ertl,
\ Elizabeth Rather, Federico de Ceballos, Michael L. Gassanenko and others.
\ Permission is granted by the authors to use this software
\ for any application provided this copyright notice is preserved.

DECIMAL

\ *************
\ * DATATYPES *
\ *************

0 CONSTANT STRUCT                      \ gForth

: END-STRUCT CONSTANT ;                \ gForth
: EQUATES CONSTANT ;
: ARRAY CREATE CELLS ALLOT ;
: STRING CREATE CHARS ALLOT ;
: OFFSET CREATE DOES> SWAP CHARS + C@ ;
: TAG ' >BODY 1 CELLS - CREATE , DOES> DUP @ - ;
: (C,) PARSE 0 ?DO I CHARS OVER + C@ C, LOOP DROP ;
: C" [CHAR] " (C,) ;                   \ This redefines the standard C"
: C| [CHAR] | (C,) ;
: TABLE CREATE ;
: +CONSTANT CREATE , DOES> @ + ;
: *CONSTANT CREATE , DOES> @ * ;
: /CONSTANT CREATE , DOES> @ / ;
: /FIELD MAX ;
: ENUM DUP CONSTANT 1+ ;               \ Swift Forth
: [PRAGMA] TRUE CONSTANT ;
: [IGNORE] CREATE DOES> DROP ;
: -> ; IMMEDIATE

\ ************* 
\ * CONSTANTS *
\ *************

S" MAX-N" ENVIRONMENT?                 \ query environment
[IF]                                   \ if successful
NEGATE 1- CONSTANT (ERROR)             \ create constant (ERROR)
[ELSE]
.( Warning: ) CHAR ( EMIT .( ERROR) CHAR ) EMIT .(  undefined) CR
[THEN]

S" MAX-N" ENVIRONMENT?                 \ query environment
[IF]                                   \ if successful
CONSTANT MAX-N                         \ create constant MAX-N
[ELSE]
.( Warning: MAX-N undefined) CR
[THEN]

S" STACK-CELLS" ENVIRONMENT?           \ query environment
[IF]                                   \ if successful
CONSTANT STACK-CELLS                   \ create constant STACK-CELLS
[ELSE]
.( Warning: STACK-CELLS undefined) CR
[THEN]

S" /PAD" ENVIRONMENT?                  \ query environment
[IF]                                   \ if successful
CONSTANT /PAD                          \ create constant /PAD
[ELSE]
.( Warning: /PAD undefined) CR
[THEN]

S" /HOLD" ENVIRONMENT?                 \ query environment
[IF]                                   \ if successful
CONSTANT /HOLD                         \ create constant /HOLD
[ELSE]
.( Warning: /HOLD undefined) CR
[THEN]

S" MAX-CHAR" ENVIRONMENT?              \ query environment
[IF]                                   \ if successful
CONSTANT MAX-CHAR                      \ create constant MAX-CHAR
[ELSE]
.( Warning: MAX-CHAR undefined) CR
[THEN]

S" ADDRESS-UNIT-BITS" ENVIRONMENT?     \ query environment
[IF]                                   \ if successful
1 chars * CONSTANT CHAR-BITS           \ create constant CHAR-BITS
[ELSE]
.( Warning: CHAR-BITS undefined) CR
[THEN]

\ *************
\ * COMPILING *
\ *************

: [NOT] 0= ; IMMEDIATE                 \ used for conditional compilation
: [MAX] MAX ;                          \ also used in colon definitions
: [HEX] HEX ; IMMEDIATE                \ works at compile time
: [DECIMAL] DECIMAL ; IMMEDIATE        \ works at compile time
: [BINARY] 2 BASE ! ; IMMEDIATE        \ works at compile time
: [OCTAL] 8 BASE ! ; IMMEDIATE         \ works at compile time
: (FIND) BL WORD FIND SWAP ;           \ (factored word)
: [ABORT] TRUE ABORT" Compilation aborted" ; IMMEDIATE
: :REDO >IN @ >R : R> >IN ! ;          \ portable DOES> replacement
: DONE 1 CS-ROLL POSTPONE ELSE 1 CS-ROLL ; IMMEDIATE

\ **************
\ * COMMON USE *
\ **************

: @C @ ;                               \ CROSS EXT
: OCTAL 8 BASE ! ;                     \ 4TH
: CHOP 1- SWAP CHAR+ SWAP ;            \ 4TH
: >STRING OVER PLACE ;                 \ 4TH
: SMOVE CELLS MOVE ;                   \ 4TH
: ERROR? (ERROR) OVER = ;              \ 4TH
: NOT 0= ;                             \ COMUS

(FIND) [UNDEFINED] DROP 0= [IF]        \ COMUS
: [UNDEFINED] (FIND) DROP 0= ; IMMEDIATE
[THEN]

[UNDEFINED] [DEFINED] [IF]             \ SEARCH EXT
: [DEFINED] (FIND) DROP 0<> ; IMMEDIATE
[THEN]
                                       \ SEARCH EXT
[UNDEFINED] ALIAS [IF]
: ALIAS CREATE , DOES> @ EXECUTE ;
[THEN]
                                       \ COMUS
[UNDEFINED] AKA [IF]
: AKA (FIND) OVER 0= ABORT" Undefined word" ALIAS 0> IF IMMEDIATE THEN ;
[THEN]
                                       \ COMUS
[UNDEFINED] TH [IF]
: TH CELLS + ;
[THEN]
                                       \ COMUS
[UNDEFINED] BOUNDS [IF]
: BOUNDS OVER + SWAP ;
[THEN]
                                       \ COMUS
[UNDEFINED] -ROT [IF]
: -ROT ROT ROT ;
[THEN]
                                       \ COMUS
[UNDEFINED] PLACE [IF]
: PLACE OVER >R ROT OVER CHAR+ R> MOVE C! ; ;
[THEN]
                                       \ COMUS
[UNDEFINED] +PLACE [IF]
: +PLACE
  DUP >R OVER >R COUNT DUP >R CHARS + SWAP MOVE R> R> + R> C! ;
[THEN]
                                       \ STRING
[UNDEFINED] /STRING [IF]
: /STRING SWAP OVER - >R + R> ;
[THEN]
                                       \ FACILITY EXT
[UNDEFINED] +FIELD [IF]
: +FIELD CREATE OVER , + DOES> @ + ;
: FIELD: ALIGNED 1 CELLS +FIELD ;
: CFIELD: 1 CHARS +FIELD ;
[THEN]

[UNDEFINED] BUFFER: [IF]               \ CORE EXT
: BUFFER: CREATE ALLOT ;
[THEN]
                                       \ DOUBLE
[UNDEFINED] D>S [IF]
AKA DROP D>S
.( Warning: D>S undefined, guessing) CR
[THEN]
                                       \ COMUS
[UNDEFINED] SKIP [IF]
: SKIP >R BEGIN DUP WHILE OVER C@ R@ = WHILE CHOP REPEAT THEN R> DROP ;
[THEN]

[UNDEFINED] CELL [IF]
1 CELLS CONSTANT CELL
[THEN]

[UNDEFINED] CELL- [IF]
: CELL- CELL - ;
[THEN]

[UNDEFINED] CHAR- [IF]
: CHAR- 1- ;
[THEN]

\ ***********
\ * PARSING *
\ ***********
                                       \ 4TH
: OMIT 
  >R SOURCE >IN @ OVER MIN /STRING OVER SWAP R> SKIP DROP SWAP - >IN +! ; 
                                       \ 4TH
: PARSE-WORD DUP OMIT PARSE ;

\ **************
\ * CONVERSION *
\ **************
                                       \ 4TH
[DEFINED] (ERROR) [IF]
: NUMBER                               ( a n1 -- n2)
  0. 2SWAP OVER C@ [CHAR] - = DUP >R
  IF CHOP THEN >NUMBER SWAP DROP 0=
  IF D>S R> IF NEGATE THEN ELSE 2DROP (ERROR) R> DROP THEN
;
[ELSE]
.( Warning: NUMBER undefined, needs ) CHAR ( EMIT .( ERROR) CHAR ) EMIT CR
[THEN]

\ *****************
\ * STRING TABLES *
\ *****************
                                       \ COMUS
: SCONSTANT CREATE HERE OVER 1+ CHARS ALLOT PLACE DOES> COUNT ;
                                       \ 4TH
[DEFINED] SLITERAL [IF]
: (S|) [CHAR] | PARSE STATE @ ;
: S| (S|) IF POSTPONE SLITERAL THEN ; IMMEDIATE
: .| (S|) IF POSTPONE SLITERAL POSTPONE TYPE ELSE TYPE THEN ; IMMEDIATE
[ELSE]
.( Warning: S| undefined, needs SLITERAL) CR
[THEN]

[DEFINED] /STRING-SPACE [IF]
( Reserve STRING-SPACE in data-space )
CREATE STRING-SPACE   /STRING-SPACE CHARS ALLOT
VARIABLE NEXT-STRING  0 NEXT-STRING !
 
( " ccc" -- caddr )
: (S,) PARSE
  DUP 1+ NEXT-STRING @ + /STRING-SPACE >
        ABORT" String space exhausted"
  STRING-SPACE NEXT-STRING @ CHARS + >R
  DUP 1+ NEXT-STRING +!
  R@ PLACE
  R> ,
;

: ," [CHAR] " (S,) ;                   \ 4TH
: ,| [CHAR] | (S,) ;                   \ 4TH
[ELSE]
.( Warning: ," undefined, needs /STRING-SPACE) CR
[THEN]

\ ******************
\ * FLOATING POINT *
\ ******************

[DEFINED] F+ [IF]                      \ size expressed in CELLS
1 FLOATS CELL /MOD SWAP [IF] 1+ [THEN] CONSTANT FLOAT
                                       \ oversize FLOAT when needed
[UNDEFINED] S>F [IF]
: S>F S>D D>F ;
[THEN]

[UNDEFINED] F>S [IF]
: F>S F>D D>S ;
[THEN]

[UNDEFINED] S>FLOAT [IF]
: S>FLOAT >FLOAT 0= ABORT" BAD FLOAT" ;
[THEN]
[ELSE]
.( Warning: FLOATING undefined, section skipped) CR
[THEN]

\ ********
\ * TIME *
\ ********

[DEFINED] TIME&DATE [IF]
: >JD
  >R 3 - DUP 0< IF 12 + R> 1- >R THEN
  306 * 5 + 10 / + R@ 1461 4 */ + 1721116 +
  DUP 2299169 > IF 3 +  R@ 100 / -  R@ 400 / + THEN R> DROP
;

: >TIME >JD 2440588 - 86400 * >R 3600 * SWAP 60 * + + R> + ;
: TIME TIME&DATE >TIME ;
[THEN]

\ ***************
\ * UNSUPPORTED *
\ ***************

: [UNSUPPORTED] TRUE ABORT" Not supported" ; IMMEDIATE

AKA [UNSUPPORTED] USE                  \ all the following words will make it
AKA [UNSUPPORTED] OPEN                 \ impossible to port a 4tH program to
AKA [UNSUPPORTED] CLOSE                \ ANS Forth without at least some 
AKA [UNSUPPORTED] HI                   \ modifications. So instead of making
AKA [UNSUPPORTED] LO                   \ compilation simply fail, we issue a
AKA [UNSUPPORTED] VARS                 \ message that the program uses words
AKA [UNSUPPORTED] FIRST                \ which are not supported by this
AKA [UNSUPPORTED] LAST                 \ compiler.
AKA [UNSUPPORTED] SEEK
AKA [UNSUPPORTED] TELL
AKA [UNSUPPORTED] HIDE
AKA [UNSUPPORTED] PAUSE
AKA [UNSUPPORTED] ARGS
AKA [UNSUPPORTED] :TOKEN
AKA [UNSUPPORTED] ARGN

S" ezneeds.4th" INCLUDED               \ chainload INCLUDE and [NEEDS