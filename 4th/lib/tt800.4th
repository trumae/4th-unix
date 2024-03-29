\ by M. Matsumoto, email: matumoto@math.keio.ac.jp
\ tt800 generates one pseudorandom number with double precision
\ which is uniformly distributed on [0,1]-interval for each call.
\ One may choose any initial 25 seeds except all zeros.

\ See: ACM Transactions on Modelling and Computer Simulation,
\ Vol. 4, No. 3, 1994, pages 254-266.

\ 4tH version by H.Bezemer 2017
\ include lib/fp1.4th
[UNDEFINED] TT800  [IF]
[UNDEFINED] FCLEAR [IF] [ABORT] [THEN]
[UNDEFINED] UL+    [IF] include lib/constant.4th [THEN]

25 constant #seed                       \ number of seeds
#seed array seed does> swap cells + ;   \ seeds array
                                        ( n -- x[n])
7 constant (m)
0 value (k)

[HEX]
create (seeds)                         \ initial seeds
  15f24dab +UL , 0b685215     , 676ccae7 +UL , 2f3ec239 +UL , 715fad23 ,
  24a590ad     , 69e4b5ef     , 3f456141 +UL , 16bc1b7b +UL , 27bdf825 +UL ,
  41de75b7 +UL , 0858a9c9 +UL , 2da87693     , 3657f9dd +UL , 7fdc8a9f +UL ,
  0121da71 +UL , 0b823ecb +UL , 085d05f5 +UL , 4e20cd47     , 5a9ad5d9 ,
  512c0c03     , 6a857ccd +UL , 4cc1d30f     , 0891a8a1 +UL , 26b7aadb +UL ,
does> swap cells + @c ;
                                       \ initialize (x) with seeds
#seed 0 ?do i (seeds) i seed ! loop fclear
                                       \ magic vectors, don't change
create (mag01) 0 , 0ebfd028 +UL , does> swap cells + @c ;

: seed!                                ( i offset --)
  over + swap seed dup @ dup 1 rshift swap 1 and (mag01) xor rot seed @ xor
  swap !
;
                                       \ TT800 randomizer
: tt800                                ( -- f)
  (k) #seed = if
    #seed (m) - 0 ?do i (m) seed! loop
    #seed dup (m) - ?do i (m) #seed - seed! loop 0 to (k)
  then

  (k) seed @                           \ magic vectors, don't change
  dup 07 lshift 2b5b2500 and xor 
  dup 0F lshift 5b8b0000 +UL and xor 7FFFFFFF +UL and
  dup 10 rshift xor (k) 1+ to (k)
  u>d d>f 7FFFFFFF +UL u>d d>f f/      \ make a floating point number
;

[DECIMAL]
[DEFINED] 4TH# [IF]
  hide (k)
  hide (m)
  hide (seeds)
  hide (mag01)
[THEN]
[THEN]

\ 50 0 ?do tt800 f. i 8 mod 7 = if cr then loop depth .