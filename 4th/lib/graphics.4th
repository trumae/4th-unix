\ Graphics using portable bit maps (ppm/pgm); 
\ 4tH library file, D Johnson (Aug 9, 2010) 
\ Updated May 13, 2012 (drj): 
\   Added check of header line length in GET_NWORDS 
\   Added width and height check in GET_HEADER 
\   Added CROP, INTENSITY! INTENSITY@ 
\   Behaver COLOR@ COLOR! now symmetric, 
\   "Height" spelling corrected 
\   Increased default "screen size" 
\ 6/2/2015 (drj): use CELLS with gstack for ANS-Forth compatibility 
 
[UNDEFINED] line [IF] 
[UNDEFINED] 2over      [IF] include lib/anscore.4th  [THEN] 
[UNDEFINED] char>lower [IF] include lib/ulcase.4th   [THEN] 
[UNDEFINED] s>         [IF] include lib/stsstack.4th [THEN] 
[UNDEFINED] 10K        [IF] include lib/constant.4th [THEN] 
[UNDEFINED] stack      [IF] include lib/stack.4th    [THEN] 

512 constant gstack_len            \ Allow space for odd header lines 
gstack_len string gstack$          \ Space for the string stack 
gstack$  gstack_len string-stack 
gstack_len 4 /  constant max_header_len
 
6 constant gstkmax         \ space for the application stack 
gstkmax array gstack       \ Graphics application stack 
gstack stack               \ w/ depth for 3 points or 2 pixels 
 
: gdepth ( -- n ) gstack @ gstack -  1 cells / ; 
: >g     ( n -- ) gdepth gstkmax > abort" gstack overflow!" 
                  gstack >a ; 
: g>     ( -- n ) gdepth 0= abort" gstack is empty!" 
                  gstack a> ; 
: g@     ( -- n ) gstack a@ ; 
: g'@    ( -- n ) gstack @  1 cells -  @ ; 

81 string image_comment$ 
s" #4tH portable bitmap file"  image_comment$ place 

char # constant hcomment      \ Defines a *.ppm header comment

10 constant EOL               \ End of line for just one white space
 
\ ============================================================= 
   800 constant ppm_width     \ "Screen Size" or maximum 
   600 constant ppm_height    \  possible image size. 
   255 constant ppm_nmax      \  maximum intensity of byte 
     3 constant (c/pixel)     \  colors per pixel 
\ ============================================================= 
 
                              \ Define space for the "BitMap" 
   ppm_width ppm_height *  (c/pixel) *  string bitmap
 
       \ Actual image size.  Must be defined in the program 
       \ or read from the *.ppm bitmap file! 
variable  pic_intensity    \ typical value is 255 
variable  pic_width 
variable  pic_height 
                                          \ Other screen modes 
defer n>color               ( n -- r g b )  
defer color>n               ( r g b -- ) 
 
: color>gray  ( red green blue -- gray )  \ assume 24 bit pixel 
        5 *  swap 16 *  rot 11 *  + +  32 / ; 
 
: gray>color ( gray -- red green blue )  dup dup ; 
 
 
\ Image to use 
(c/pixel) value c/pixel 
 
: to_color  ( -- )   (c/pixel) to c/pixel 
        ' color>gray  is color>n 
        ' gray>color  is n>color  ; 
 
: color_image ( -- )  to_color 
        s" 4tH portable bitmap: true color" 
        image_comment$ place 
        ppm_nmax pic_intensity ! ; 
 
: to_gray ( -- )  1 to c/pixel 
        ' color>gray  is color>n 
        ' gray>color  is n>color ; 
 
: grayscale_image ( -- ) to_gray 
        s" 4tH portable bitmap: grayscale" 
        image_comment$ place 
        ppm_nmax pic_intensity ! ; 
 
\ Netpbm magic number 2,3 gray/color text; 5,6 gray/color binary 
: magic# ( n -- flag )       \ true=binary; false=text 
       s" Image loaded from a " image_comment$ place 
       dup 2 = if cr to_gray   false >r 
               s" P2 grayscale file"  image_comment$ +place 
               else 
       dup 3 = if cr to_color  false >r 
               s" P3 color file" image_comment$ +place 
               else 
       dup 5 = if cr to_gray   true  >r 
               s" P5 grayscale file" image_comment$ +place 
               else 
       dup 6 = if cr to_color  true  >r 
               s" P6 color file" image_comment$ +place 
               else abort" Not a PGM/PPM file" 
   then then then then 
   drop r> ; 
 
\ Load image data from the *.ppm header. 
                            \ Data previously placed in string stack. 
: get_header ( -- flag )    \ True=binary;  False=text 
  gstack$ s>  dup >g        \ keep length of last word in header 
              number error? abort" Error: intensity?" 
                 ppm_nmax min pic_intensity ! 
  gstack$ s>  number error? abort" Error: height?" 
                 dup ppm_height > abort" Image height too large"
                 pic_height ! 
  gstack$ s>  number error? abort" Error: width?" 
                 dup ppm_width > abort" Image width too large"
                 pic_width ! 
  gstack$ s>  drop dup c@ char>lower [char] p <> 
                 abort" Not PPM/PGM type file!" 
  1+ c@ char 0 -  magic#    \ check if P2, P3, P5, or P6 file 
  ; 
 
 
    \ Offset to readjust file pointer based on TIB size and 
    \ length of the last word read from the file header. 
: nreadjust ( -- n ) source nip g> - char- char- ; 
 
\ Parse specified number of words from file to string stack. 
\ Can span multiple file lines while skipping comments. 
 
: get_nwords ( n  -- )  \ Results are placed in string stack!! 
 false to do_palette? 
 1 begin refill while 
   begin 
     bl parse-word 
     over c@ hcomment <>     \ skip over comments 
     over 0<>    and         \ and check the string count 
   while 
     dup max_header_len >  abort" Error in PPM file header"
     gstack$ >s              \ move the word to string stack 
     1+  2dup  < if  2drop exit then   \ check number parsed 
   repeat 
   2drop 
 repeat ; 
 
: get_image     ( addr n -- ) \ Load portable bitmap into memory 
   input open error? \ value for file 
   abort" File could not be opened" 
 
   dup use >r 
   4 get_nwords                \ Read PPM header data to sstack 
 
   get_header if               \ read binary data 
      r@ tell  dup  source nip > 
        if  nreadjust - r@ seek abort" Can't read file" 
        else drop then 
      bitmap  pic_width @  pic_height @  *  c/pixel * 
      accept drop 
   else                          \ read text data 
         0     \ offset for bitmap 
         begin  refill while 
            begin  32 parse-word  dup 0<> 
               while  number over bitmap +  c! char+ 
            repeat 
            drop drop 
         repeat 
         drop     \ the offset  for bitmap 
   then 
   r> close ; 
 
 
: save_image ( addr n -- ) 
  output open error?        \ value for file 
  abort" File could not be opened"    \ save handle 
 
  dup use                   \ redirect input to file 
  c/pixel (c/pixel) =  if ." P6" else ." P5" then  cr 
  ." # " image_comment$ count type cr 
  pic_width @ .  pic_height @ . cr 
  pic_intensity @ <# EOL hold #s #> type   \ only 1 whitespace! 
  bitmap pic_width @  pic_height @   *  c/pixel *  type 
  close ; 
 
(c/pixel) string pixel_color    \ Define "current" color 
 
: element ( r c -- pixel_bitmap_offset ) 
    swap pic_width @  *  +  c/pixel *  ; 
 
: intensity@ ( -- r b g ) 
   c/pixel 0 do   pixel_color i + c@   loop ; 
 
: color@ ( -- r g b ) 
    intensity@ 
    c/pixel 1 = if n>color then ; 
 
: intensity! ( r g b -- )  \ set current color/grayscale 
     c/pixel 0 do   pixel_color c/pixel char- i -  + c!  loop ; 
 
: color! ( r g b -- ) \ set current color; must be r g b format 
    c/pixel 1 = if color>n then 
    intensity! ; 
 
: set_pixel ( rx cy -- )    \ using the "current" color 
    element bitmap + 
    c/pixel 0 do 
          dup pixel_color i + c@ swap c!   char+ 
    loop drop ; 
 
: pixel! ( red green blue rx cy -- )  \ color the pixel 
    element bitmap + 
    c/pixel 0 do  over over c/pixel i - char- + 
    c! nip loop    drop ; 
 
: background ( -- )   \ set background to current color 
    pic_width @ 0 do 
      pic_height @ 0 do 
         intensity@ i j pixel! 
      loop 
    loop ; 
 
                      \ color screen between white (255) to 
: whiteout ( n -- )   \ to varying levels of gray to black (0) 
     pic_intensity @ min 
     bitmap pic_width @  pic_height @   *  c/pixel * 
     rot fill ; 
 
: pixel@ ( rx cy -- red blue green ) \ get pixel color 
    element bitmap + 
    c/pixel 0 do 
        dup c@   swap char+ 
     loop  drop ; 
 
 
: crop ( addr n rx1 cy1 rx2 cy2 -- )  \ save cropped area to file 
  >g >g   >g >g 
  output open error?        \ value for file 
  abort" File could not be opened"    \ save handle 
 
  dup use                   \ redirect input to file 
  c/pixel (c/pixel) =  if ." P6" else ." P5" then  cr 
  ." # " image_comment$ count type cr 
  g> g> swap                       ( cy1 rx1; gstack: cy2 rx2 ) 
  over g'@ swap  - .   dup g@ swap - . cr 
  pic_intensity @ <# EOL hold #s #> type   \ only 1 whitespace! 
  g@ over do 
    over g'@ swap do 
      c/pixel 1 = if j i pixel@ emit 
                else j i  pixel@ rot rot swap emit emit emit 
                then 
    loop 
  loop  g> g>  drop drop drop drop  close ; 
 
 
: pnts<>  ( rx1 cy1 rx2 cy2 -- f ) \ don't want identical pnts 
    rot = if <> else true 2drop then ; 
 
: hline ( rx1 cy1 rx2 cy2 -- ) 
    2over drop                   \ rx1 
    >r over r> swap              \ rx1 rx2 
    > if  2swap  then            \ need to change point order? 
    2over 2over                  \ 4dup 
    rot -                        \ find dy 
    swap rot  -                  \ find dx 
    swap 10K * swap / >g 
    drop swap 
    10K *  swap rot 
    do dup 10K /  i  swap set_pixel  g@  + loop 
    g> drop ; 
 
: vline ( rx1 cy1 rx2 cy2  --) 
    >r over r> swap              \ cy1 
    over                         \ cy1 cy2 
    > if 2swap then              \ need to change point order? 
    2over 2over                  \ 4dup 
    rot -                        \ dy 
    rot rot swap -               \ dx 
    10K * swap /   >g 
    nip rot 10K * swap rot 
    do dup 10K /  i  set_pixel  g@  + loop 
    g> drop ; 
 
: line ( rx1 cy1 rx2 cy2 -- )  \ using the "current" color 
    2over 2over pnts<> if 
      2over 2over 
      rot - abs  ( positive dy ) 
      swap rot - abs ( positive dx ) 
      < if hline else vline then drop 
    else 2drop set_pixel then ; 
 
 
\ Simple colors (0-7) 
: black ( -- )    0  0  0                     color! ; 
: blue ( -- )     0  0  pic_intensity @       color! ; 
: green ( -- )    0  pic_intensity @  0       color! ; 
: cyan ( -- )     0  pic_intensity @  dup     color! ; 
: red ( -- )      pic_intensity @  0  0       color! ; 
: magenta ( -- )  pic_intensity @  0  over    color! ; 
: yellow ( -- )   pic_intensity @  dup  0     color! ; 
: white ( -- )    pic_intensity @  dup  dup   color! ; 
 
\ Define the default setup 
    ppm_width   pic_width ! 
    ppm_height  pic_height ! 
    ppm_nmax    pic_intensity ! 
    color_image 
    white 
 
[DEFINED] 4TH# [IF] 
  hide gstack_len 
  hide max_header_len 
  hide gstkmax 
  hide gstack 
  hide hline 
  hide vline 
  hide pnts<> 
  hide element 
  hide gstack$ 
  hide hcomment 
  hide EOL 
  hide (c/pixel) 
  hide get_header 
  hide magic# 
[THEN] 
[THEN] 
