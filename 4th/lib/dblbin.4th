\ DXForth MISC.SCR, Copyright Ed, 2011-03-25
\ Albert van der Horst, Thursday 09 April 2015 19:57:13, c.l.f

[UNDEFINED] dand      [IF]
[UNDEFINED] cell-bits [IF] include lib/ansdbl.4th [THEN]

\ Logical
: DAND     ( xd1 xd2 -- xd3 )  rot and >r and r> ;
: DOR      ( xd1 xd2 -- xd3 )  rot or >r or r> ;
: DXOR     ( xd1 xd2 -- xd3 )  rot xor >r xor r> ;
: DINVERT  ( xd1 -- xd2 )      invert swap invert swap ;

\ Shift
: DLSHIFT >r swap dup r@ lshift swap cell-bits r@ - rshift rot r> lshift or ;
: DRSHIFT >r dup r@ rshift swap cell-bits r@ - lshift rot r> rshift or swap ;
           ( xd1 n -- xd2 )
[THEN]