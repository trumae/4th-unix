( This is an ANS Standard Program implementing the function RAN4 as   )
( described in the second edition of "Numerical Recipes in C" by      )
( Press Teukolsky Vetterling and Flannery {ISBN 0-521-43108-5}        )

( Forth Scientific Library Algorithm #24                              )
( ran4.seq	1.1 12:10:28 1/9/95  GC                               )

( Note that this code is based on the description given therein, in   )
( the sense that the authors use the term in the chapter of that      )
( book entitled Legal Matters, so there is no conflict of copyright.  )

( This program requires the following to be present;                  )
(  The ANS Standard Core words.                                       )
(  The Double-Number word set.                                        )
  
( This program assumes a two's complement environment.                )
( Alternate versions of the word PseudoDes are given for 32 bit and   )
( 16 bit Forths. When the former is commented out the program assumes )
( a 16 bit environment, when the latter is commented out a 32 bit     )
( environment is assumed.                                             )

( To extend the code to other cell widths, the literal numbers given  )
( in PseudoDes should be chosen randomly and contain an equal number  )
( of bits that are set and bits that are not set. {ie in a 24 bit     )
( system each of the numbers in PseudoDes should contain 24 1's when  )
( printed as an unsigned double binary number.}                       )

( RAN4 returns a double length random number {in the sense that most  )
( programmers understand "random"}. All the bits of this number are   )
( 'good' so no special technique need be applied to reduce the range. )
( This is in contrast to, say, a linear congruential generator, where )
( the low bits are "more random" than the high bits, so range         )
( reduction by MOD is counterindicted.                                )

( RAN4 is capable of generating 2^32 different random sequences {in a )
( 16 bit Forth} all of which have a cycle of 2^32 numbers.            )

( START-SEQUENCE initialises RAN4. It takes 2 double length arguments.)
( The top of stack specifies which sequence to generate numbers from. )
( The second on stack specifies the starting position within that     )
( sequence.                                                           )

( The algorithm is described in Numerical Recipes and the word names  )
( given here are in accordance with that description. Names which     )
( include lower case letters should be considered private to RAN4.    )

( RAN4 is a slow generator, but is believed by Press et al to be one  )
( of the best available, in terms of the quality of the sequences     )
( generated. Nonetheless I take no liability for the effects of using )
( this code.                                                          )

[UNDEFINED] RAN4 [IF]
[UNDEFINED] UM*  [IF] include lib/mixed.4th  [THEN]
[UNDEFINED] DXOR [IF] include lib/dblbin.4th [THEN]

[HEX]
: FuncG ( d dc1 dc2--d) 
        >R >R DXOR  2DUP UM*  2SWAP DUP UM*  DINVERT
        ROT DUP UM*  D+  SWAP R> R> DXOR  D+ ;

: PseudoDes ( d d--d d)  ( 32 bit version)
        2TUCK  634C383B +UL 3AA96887 +UL 3D02B5F8     4B0F3B58     FuncG DXOR
        2TUCK  39F74033     1E17D32C     1226BF1A +UL 6874F0C3 +UL FuncG DXOR
        2TUCK  60B43DA7     03BCDC3C     1D38CD47     6955C5A6     FuncG DXOR
        2TUCK  65E9215B     0F33D1B2     7358B432 +UL 55A7CA46     FuncG DXOR ;
 
( : PseudoDes ( d d--d d)  ( 16 bit version)
(             2SWAP 2OVER  BAA96887. 4BOF3B58. FuncG DXOR             )
(             2SWAP 2OVER  1E17D32C. E874F0C3. FuncG DXOR             )
(             2SWAP 2OVER  03BCDC3C. 6955C5A6. FuncG DXOR             )
(             2SWAP 2OVER  0F33D1B2. 55A7CA46. FuncG DXOR ;           )

2 ARRAY Counter
2 ARRAY Sequence#

: START-SEQUENCE  ( dcounter dseq#)  Sequence# 2!  Counter 2! ;

: RAN4 ( --d)  Sequence# 2@  Counter 2@  PseudoDes
               2NIP
               Counter 2@ 1 U>D D+  Counter 2! ;

( The code presented here is placed in the public domain.             )
( Gordon Charlton {gordon@charlton.demon.co.uk} 10th September 1994   )
[DECIMAL]

[DEFINED] 4TH# [IF]
  hide FuncG
  hide PseudoDes
  hide Counter
  hide Sequence#
[THEN]
[THEN]
