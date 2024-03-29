\ 4tH library - GAMMA - Copyright 2015 J.L. Bezemer
\ You can redistribute this file and/or modify it under
\ the terms of the GNU General Public License

\ Taken from: "Ramanujan’s estimate for the gamma function via monotonicity
\ arguments", The Ramanujan Journal, June 2011, Volume 25, Issue 2, pp 149-154
\ by Cristinel Mortici. The formula was taken from Ramanujan's lost notebook:

\ SQRT(pi)* (n/e)^n * (8n^3 + 4n^2 + n + 1/30)^(1/6)

\ This is the manuscript in which the Indian mathematician Srinivasa Ramanujan
\ recorded the mathematical discoveries of the last year (1919–1920) of his
\ life. Its whereabouts was unknown to all but a few mathematicians until it
\ was rediscovered by George Andrews in 1976, in a box of effects of G. N.
\ Watson stored at the Wren Library at Trinity College, Cambridge.

\ Based on "The Ramamujan asymptotic formula for the Euler Gamma function"
\ by E.A. Karatsuba, 24 July 2008. Since the d(x) function requires a gamma
\ function, it is emulated here by two symmetrical sigmoidals.

\ First domain:
\ y = 0.03187831 + ((0.00482388-0.03187831)/ (1 + POWER((A6/2.280802),1.428045)))

\ Second domain:
\ y = d + (a-d)/(1 + (x/c)^b)
\ a=0.003935717, b=1.052779, c=1.841693, d=0.03330828

\ Third domain:
\ 1/30

\ It is the most accurate Zen gamma function, but it is slow
\ Doesn't require tuning. Rel. error between 1E-7 to 1E-8 (32 bits)
\ include lib/fp2.4th

[UNDEFINED] gamma [IF]
[UNDEFINED] f**   [IF] include lib/zenfalog.4th [THEN]

2 constant (gamma-shift)               \ don't change this
                                       \ an approximation of the d(x) function
: ~d(x)                                ( f1 -- f2)
  2dup 10 s>f f<                       \ use first symmetrical sigmoidal
  if                                   \ for range 1-10
    -2705443 -8 2swap 2280802 -6 f/ 1428045 -6 f** 1 s>f f+ f/ 3187831 -8 f+
  else                                 \ use second symmetrical sigmoidal
    -29372563 -9 2swap 1841693 -6 f/ 1052779 -6 f** 1 s>f f+ f/ 3330828 -8 f+
  then 333333333 -10 2over f< if 2drop 1 s>f 30 s>f f/ then
;                                      \ perform some sane clipping to infinity

: (ramanujan)                          ( f1 -- f2)
  2dup 2dup f* 4 s>f f*                ( n 4n2)
  2over 2over f* 2dup f+ f+ 2over f+   ( n 8n3+4n2+n)
  2over ~d(x) f+                       ( n 8n3+4n2+n+d[x])
  1 s>f 6 s>f f/ f**                   ( n 8n3+4n2+n+d[x]^1/6)
  2swap 2dup 271828183 -8 f/           ( 8n3+4n2+n+d[x]^1/6 n n/e)
  2swap f** f* 1772453851 -9 f*        ( f)
;

: gamma                                ( f1 -- f2)
  2dup f0< >r 2dup f0= r> or abort" Gamma less or equal to zero"
  2dup (gamma-shift) 1- s>f f+ (ramanujan) 2swap
  1 s>f (gamma-shift) 0 do 2over i s>f f+ f* loop 2nip f/
;

[DEFINED] 4TH# [IF]
  hide (ramanujan)
  hide (gamma-shift)
  hide ~d(x)
[THEN]
[THEN]

\ s" 0.5" s>float gamma f. cr
\ 11 1 ?do i s>f gamma f. cr loop
\ 1006 995 ?do i . i -2 gamma f. cr loop depth .