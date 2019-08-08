\ 4tH library - Hashing - Copyright 2009,2014 J.L. Bezemer
\ You can redistribute this file and/or modify it under
\ the terms of the GNU General Public License
\ ---
\ See: http://www.cse.yorku.ca/~oz/hash.html

[UNDEFINED] (hash) [IF]

/cell 4 [=] [IF]                       \ 32 bit FNV constants
  -2128831035 constant (FNVoffset)
     16777619 constant (FNVprime)
[ELSE]                                 \ 64 bit FNV constants
-3750763034362895579 constant (FNVoffset)
       1099511628211 constant (FNVprime)
[THEN]

: (djb2) dup 5 lshift + + ;            ( c n1 -- n2)
: (sdbm) dup >r 6 lshift r@ 16 lshift + + r> - ;
: (fnv1a) xor (FNVprime) * ;
: (hash) 2swap bounds ?do over i c@ -rot execute loop nip ;
: djb2 ['] (djb2) 5381 (hash) ;        ( a n -- n2)
: sdbm ['] (sdbm) 0 (hash) ;           ( a n -- n2)
: fnv1a ['] (fnv1a) (FNVoffset) (hash) ;

[DEFINED] 4th# [IF]
  hide (djb2)
  hide (sdbm)
  hide (fnv1a)
  hide (FNVprime)
  hide (FNVoffset)
[THEN]
[THEN]
