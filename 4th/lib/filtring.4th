\ 4tH library - FILTERING - Copyright 2015 J.L. Bezemer
\ You can redistribute this file and/or modify it under
\ the terms of the GNU General Public License

[UNDEFINED] filtering [IF]
[UNDEFINED] 2over     [IF] include lib/anscore.4th [THEN]
[UNDEFINED] break?    [IF] include lib/breakq.4th  [THEN]

: filtering                          ( a1 n1 a2 n2 -- a1 n3)
  bounds 2swap over >r chars over + >r dup
  begin                              ( a2+n2 a2 a1 a1 r: a1 a1+n1)
    over r@ <                        \ read pointer at the end?
  while                              \ if not, check if filtered character
    2over 2>r over c@ dup true 2r> break?
    if over c! char+ else drop then swap char+ swap
  repeat r> drop >r drop 2drop r> r> tuck -
;                                    \ clean up, calculate length
[THEN]

\ s" She was a soul stripper. She took my heart!" s" aei" filtering type cr
