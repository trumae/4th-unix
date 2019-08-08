\ 4tH library - COMPARE (STRING) - Copyright 2003,2010 J.L. Bezemer
\ You can redistribute this file and/or modify it under
\ the terms of the GNU General Public License

[UNDEFINED] compare [IF]               \ this routine compares two strings
: compare                              ( a1 n1 a2 n2 -- f )
  rot over over swap - >r              ( a1 a2 n2 n1)
  min 0 tuck                           ( a1 a2 0 n 0)
  ?do                                  ( a1 a2 f)
    drop                               ( a1 a2)
    over i + c@                        ( a1 a2 c1)
[UNDEFINED] casesensitive [IF]
    dup [char] A - max-n and 26 < if bl or then
[THEN]
    over i + c@                        ( a1 a2 c1 c2)
[UNDEFINED] casesensitive [IF]
    dup [char] A - max-n and 26 < if bl or then
[THEN]
    - dup                              ( a1 a2 f f)
    if leave then                      ( a1 a2 f)
  loop
  >r drop drop r> r> swap dup          ( f1 f2 f2)
  if swap then drop                    ( f)
;
[THEN]

