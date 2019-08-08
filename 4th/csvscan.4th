\ 4tH CSVSCAN - Copyright 2009,2017 J.L. Bezemer
\ You can redistribute this file and/or modify it under
\ the terms of the GNU General Public License

\ This program analyzes a CSV file, lists the number of lines and columns
\ and lists all columns with the maximum length of the fields found.
\ The first line of the CSV file MUST contain a list of all fields involved.

include lib/parsing.4th                \ for PARSE-CSV?
include lib/padding.4th                \ for .PADDING
include lib/getopts.4th                \ for GET-OPTIONS
include lib/argopen.4th                \ for ARG-OPEN

255 constant max-fields                \ maximum number of fields
 32 constant max-length                \ maximum length of field
max-fields max-length * constant /buffer

char ; value delimiter                 \ the delimiter (ASCII value)
0 value #fields                        \ number of fields
false value verbose                    \ verbose flag

max-fields array length-fields         \ array with field lengths
max-fields array filled-fields         \ array with filled fields
/buffer string field-names             \ array with field names
/buffer string bigTIB                  \ alternate TIB
                                       \ buffer with field names
: Preprocess                           ( --)
  bigTIB /buffer source!               \ initialize big TIB
  refill 0= abort" Cannot read header" \ get header line
  0 field-names >r                     \ initialize variables
  begin                                \ start scanning field names
    delimiter parse-csv?               \ parse the header
  while                                \ if not EOL
    r@ place r> count 1+ chars + >r 1+ \ save column name, increment counter
  repeat                               \ next field
  2drop r> drop dup to #fields         \ clean up and save number of columns
  0 ?do 0 dup filled-fields i th ! length-fields i th ! loop
;                                      \ initialize column length 

: scan-fields                          ( n -- n)
  #fields 0 do                         \ scan all fields
    delimiter parse-csv? 0= verbose and
    if ." Warning: record " over 1+ . ." contains " i . ." fields!" cr then
    dup length-fields i th dup @ rot max swap !
    -trailing if 1 filled-fields i th +! then drop
  loop                                 \ next field
;
                                       ( -- n)
: Process 0 begin refill while scan-fields 1+ repeat ;

: PostProcess                          ( h n --)
  option-index args type ." : " #fields . ." columns, " dup . ." rows" cr cr
  swap close                           \ close the file
  field-names #fields 0 do             \ list all fields
    ." [" i 1+ 3 .r ." ]  "            \ print field number
    dup count max-length .padding swap \ show name of field
    ." : " length-fields i th @ 4 .r   \ print statistics
    ."   ("  filled-fields i th @ 1000 * over /
    <# # [char] . hold #s #> type ." %)" cr
    swap count 1+ chars +              \ get next name
  loop drop drop                       \ drop buffer address and #rows
;
                                       \ get a delimiter
: get-delimiter                        ( --)
  get-argument drop c@ to delimiter    \ use first character
;
                                       \ get ASCII code delimiter
: get-code                             ( --)
  get-argument number error? abort" Invalid ASCII code" to delimiter
;
                                       \ set verbose flag
: set-verbose true to verbose ;        ( --)

create options
  char d , ' get-delimiter ,
  char c , ' get-code ,
  char v , ' set-verbose ,
  NULL ,

: OpenFile                             ( -- h)
  options get-options input option-index dup 1+ argn >
  abort" Usage: csvscan -d delimiter -c code -v csv-file"
  arg-open
;

: csvscan                              ( --)
  OpenFile                             \ open file
  Preprocess                           \ scan the header
  Process                              \ process the file
  PostProcess                          \ show results
;

csvscan