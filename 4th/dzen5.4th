\ Floating point from FORTH TOOLS & APPLICATIONS by      09jul85jap 
\ Fiebrbach & Thomas. 
\ It does not use a separate floating point stack. 
\ Source: ftp://ftp.taygeta.com/pub/Forth/Archive/ibm/float.arc (John A Peters) 
\ Converstion to 4tH below (drj, 10/2016) 
 
\ include lib/anstools.4th 
include lib/3dup3rot.4th
include lib/todbl.4th        \ s>double 

\ *** System setup **** 
 
: get-xsize ( -- n )  -1 max-n begin  swap 1+ swap 10 / dup 0= until drop 2* ; 
: get-ssize ( -- n )   1 max-n begin  swap 1+ swap 2 / dup 0= until drop ; 
 
variable xdigits
get-xsize xdigits ! does> @ ;
 
: fpsize_ ( -- d )  1  0 get-xsize 0 do 10 1 m*/ loop ; 
  2 cells array fpsize  fpsize_ fpsize 2! does> 2@ ; 
 
variable N2shifts 
get-ssize 2*  N2shifts ! does> @ ; 
 
variable xbits 
max-n get-ssize / 1+ negate xbits ! does> @ ;
 
2 cells array limit/10 
     max-n 10 / 2*  dup >r  max-n - r> 1+ 
     limit/10 2! does> 2@ ; 
 
\ ******************** 
 
3 cells constant float 
float +constant float+ 
float *constant floats 

: fdup  3dup ; 
: fdrop 3drop ; 
: fswap 3swap ; 
: fover 3over ; 
: frot  3rot ; 
: fnip  3swap 3drop ; 
 
: f@ ( a -- f ) dup cell+ 2@ rot @ ; 
: f! ( f a -- )  swap over ! cell+ 2! ; 
: fnegate ( f -- -f ) >r dnegate r> ; 
: f0< ( f -- bool ) drop 0 0 d< ; 
: f0> ( f -- bool)  fnegate f0< ; 
: f0= ( f -- bool ) drop d0= ; 
: fabs ( f1 -- f2 ) >r dabs r> ; 
 
: xover ( d n -- d n d )  >r 2dup r> rot rot ;  \ useful utility words 
: d0>  ( d -- bool ) dnegate d0< ; 
: d>f  ( d -- f )  0 ; 
: s>f  ( n -- f ) dup 0< if -1 else 0 then  0 ; 
 
: ?notnear  ( n --- n 1/0  1 if n has high bits all 0 or all 1 )
      dup  xbits and  dup xbits =   swap 0=   or ; 

: sm/10 ( d1 --- d1/10 )          ( Signed double division by 10 ) 
  dup 0< >R  dabs  ( save sign ) 
  10 mu/mod  rot drop  r> if dnegate then ; 
 

variable LEXP  ( for 0 representation ) 
 
: fnl ( f --- f  normalized left ) 
  fdup f0= if drop lexp @ exit then              ( zero case ) 
  >r  ?notnear if begin ?notnear while 
                    r> 1- >r 10 mu* repeat
               else 2dup sm/10 ?notnear  not 
                 if 2swap r> 1+ >r then 2drop 
               then r> ; 
 
( the top six bits are guard bits.  x is shifted left until   ) 
( one bit is different from the rest in the guard bits.       ) 
( x could be shifted one digit to the right.                  ) 

 : q/10          ( q --- q/10 )         ( divide quad word by 10 ) 
  10 mu/mod   rot >r limit/10 r@ mu* 
  r> 6 * 9 + 10 /   0                  ( round off correction ) 
  d+  >r >r    2swap 10 mu/mod rot drop 
  r> r> d+   2swap    ; 
 
: fsign ( f --- +or-f sign  1=neg, 0=pos ) 
  over 0< >r fabs r> ; 
 
: fnr ( f --- f  normalize right ) 
  dup 0< 
  if fsign >r >r 
        begin   2dup 10 mu/mod 2drop   0=   r@ 0= not  and 
        while   10 mu/mod rot drop   r> 1+ >r 
        repeat   r> r> if fnegate then 
  then  ; 
 
( shifts to the right until either the remainder is nonzero  ) 
( or the exponent is incremented to zero.                    ) 
 
: fen  ( f1 n --- f2 )       \  lower f1 exponent by n 
   swap over + >r                         ( save new exponent ) 
   0 do sm/10 loop  r> ; 
 
: f+  ( f1 f2 --- f3 ) 
  >r >r over r> r> rot            ( 3 pick )
  over min lexp !                 ( for possible zero )
  fnl fswap fnl fswap
  >r >r over r> r> rot            ( 3 pick )
  over - ( ?dup ) dup if dup then
  if dup 0<    if >r fswap r> abs    then    fen    then
  >r rot drop d+ r> fnr ;
 
: f- ( f1 f2 --- f3 ) 
  fnegate f+ ;
 
: f<  ( f1 f2 --- bool )  f- f0< ; 
: f=  ( f1 f2 --- bool )  f- f0= ; 
: f>  ( f1 f2 --- bool )  fswap f< ; 
: f<> ( f1 f2 --- bool )  f= not ; 
: f<= ( f1 f2 --- bool )  f> not ; 
: f>= ( f1 f2 --- bool )  f< not ; 
 
: dc+  ( d1 d2 --- d1+d2 flag, 1 if both are <0 ) 
  dup 0<  >r 2over 0< r> and >r drop d+ r> ; 
 
: q*  ( d1 d2 --- q1 ) 
  rot    >r r@ swap >r r@ um*                           ( hi ) 
  2swap  >r r@ swap >r r@ um*                           ( lo ) 
  r> r> r> r> 
  rot um*  2swap um*  dc+  ( mid )  >r            ( save carry1 ) 
  0 rot rot  dc+  r>  + >r >r 2swap         ( qlo save under ) 
  r> r>   ( bring back carry )  d+  ; 
 
: fsxor  ( f1 f2 --- +or-f2  +or-f1  xored-sign ) 
  fsign  >r fswap fsign r> xor ; 
 
: f*  ( f1 f2 --- f3 ) 
  fsxor  >r >r rot >r q*  0 >r 
  begin  2dup d0=  not 
         >r >r over r> r> rot   ( 3 pick ) 
         max-n 1+ and or while 
                   ( while not less than signed double number ) 
        r> 1+ >r q/10 repeat 
  2drop  r> r> r> + + r> if fnegate then fnl fnr  ; 

 
: d+over        ( d1 d2 --- d1+d2 carry ) 
        >r rot                               ( get low halves ) 
        0 rot 0 d+                           ( extend and add ) 
        rot 0 r> 0 d+              ( do same with high halves ) 
        rot 0 d+  ;                     ( add carry from lows ) 
 
( q/ utility                            mkg     pt    84jul21 ) 
 
: div-over-chk  ( d1 d2 --- d1 d2 flag,   0 if d1<d2 unsigned ) 
        2over 2over du< 0= ; 
 
: arith-div-shift     ( d q --- d 2q d carry, q twice-size bits ) 
        2dup d+over >r              ( shift highs, save carry ) 
        2swap 2dup d+over            ( shift lows, with carry ) 
        0 2rot d+                ( extend carry, add to highs ) 
        >r >r 2over r> r> 2swap     ( d1 d2 d3 -- d1 d2 d3 d1 ) 
        r> ; 
 
: subt&quot-set       ( d1 d2 d3 --- d1+1 d2-d3 ) 
                           ( subtract, set bit 31 of quotient ) 
        d- >r >r               ( subtract, save high dividend ) 
        1 0 d+                 ( set bit 31 of low ) 
        r> r> ;                ( recover high half ) 
 
: q/                    ( q d --- q/d           N-bit result ) 
     2rot 2rot                              ( dividend to top ) 
     N2shifts 0 do                          ( needs N shifts ) 
        arith-div-shift            ( sets up for subtractions ) 
        if  ( carry )  subt&quot-set 
        else  div-over-chk 
           if                   ( high dividend not > divisor ) 
             subt&quot-set 
           else 2drop                        ( remove divisor ) 
           then 
        then 
     loop  2drop 2swap 2drop ;    ( remove divisor, remainder ) 
 
 
: f/  ( f1 f2 --- f3 ) 
  fdup f0= if fdrop exit then            ( division by 0 test ) 
  fswap fsxor  >r >r >r >r                          ( f2 on r ) 
  fnl >r fpsize q* r> xdigits - 
  r> r> r> fnl >r rot r> - >r 
  q/ r> r> if fnegate then fnl fnr  ; 
 
 
\ **** simplest I/O ***** 
 include lib/dbldot.4th 
:  ff.   ( f -- )  \  print as I or E format: simple & fast 
     rot rot  0  d.r   dup if dup then 
     if   ." .E" 0 .r  then space ; 
 
: ss>float ( a n -- float ) s>double  d>f ; 

\ ***************************** 
\ *** Setup to use fpin.4th *** 
\ ***************************** 
  100 constant maxdigits 
  xdigits value precision 
: set-precision xdigits  min to precision ; 
 
 
\ ***************************** 
\ *** Setup to use fpout.4th *** 
\ ***************************** 
 
: floor ( r1 -- r2 ) 
    dup 0< if fdup 
    abs 0 ?do sm/10 loop 2>r 
    xover d0< 2r@ rot >r d>f f- f0= 0= r> and 2r> rot 
    if 1 0 d-   then d>f 
    then ; 
 
: fceil ( r1 -- r2 ) fnegate floor fnegate ; 
: ftrunc ( r1 -- r2 ) fdup f0< if fceil else floor then ; 
 
: D10*  ( d -- 10*d )  d2* 2dup d2* d+ ; 
: f>d ( f -- d) 
   dup 0< if abs 0 ?do sm/10 loop else 0 ?do D10* loop then ; 
 
: f>s ( f -- n )  f>d drop ; 
 
: fround ( r1 -- r2 ) 
   fdup f0< >r fabs  5 0 -1 ( f0.5 )  f+ r> if fnegate then 
   ftrunc ; 
 
 
\ ********************************* 
\ ***  Example: 32-bit stetup  *** 
\ ********************************* 

include lib/fpin.4th 
include lib/fpout.4th 
 
\ from zentaylr.4th 
: >taylor fdup f* fover ;              \ setup for Taylor series 
: (taylor) 2>r fover f* frot fover 2r> d>f f/ ; 
: +taylor (taylor) f+ frot frot ;      \ add Taylor iteration 
: -taylor (taylor) f- frot frot ;      \ subtract Taylor iteration 
 
create (a)  \ Use doubles so can modify to 16-bit implementations 
  705230784 , 0 , 
  422820123 , 0 , 
   92705272 , 0 , 
    1520143 , 0 , 
    2765672 , 0 , 
     430638 , 0 , 
does> swap 2* + dup  @c swap cell+ @c -10 ; 
 
 
\ from: zenferf.4th 
: ferf 
  fdup fabs 1378 0 -3 f< 
  if 
    fdup >taylor 
                 3 0 -taylor 
                10 0 +taylor 
                42 0 -taylor 
               216 0 +taylor 
              1320 0 -taylor 
              9360 0 +taylor 
             75600 0 -taylor 
            685440 0 +taylor 
           6894720 0 -taylor 
          76204800 0 +taylor 
         918086400 0 -taylor 
    fdrop fdrop 
    112837916 0 -8  f* 
  else 
    xover d0> >r fabs fdup 1 s>f 
    6 0 do fover i (a) f* f+ frot frot fover f* frot loop 
    fdup f* fdup f* fdup f* fdup f* 1 s>f fswap f/ -1 s>f f+ 
    fswap fdrop fswap fdrop r> if fnegate then 
  then 
; 
 
8 set-precision 
 ." erf(.2) = "   s" .2"  s>float ferf f. cr 
 ." erf(.5) = "   s" .5"  s>float ferf f. cr 
 ." erf(.9) = "   s" .9"  s>float ferf f. cr 
 ." erf(1.3)= "   s" 1.3" s>float ferf f. cr 
 ." erf(2.5)= "   s" 2.5" s>float ferf f. cr 
 
\ ******************* 
\ *** More tests **** 
\ ******************* 
 
 cr 
." System size is " get-ssize . cr 
." This results in a precision of " xdigits . cr cr 
 
 fpsize d>f ff. cr         \ simplest i/o 
 s" 1234" ss>float ff. cr cr 
 
 get-xsize set-precision   \ fpin and fpout 
 1 0 0  3 0 0  f/  f. cr 
 s" -123.45e-7" s>float f. cr 
 s" 0.4895e26" s>float g. cr 
