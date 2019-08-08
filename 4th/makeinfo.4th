\ 4tH MAKEINFO - Copyright 2014 J.L. Bezemer
\ You can redistribute this file and/or modify it under
\ the terms of the GNU General Public License

\ This program will create an information program out of the ASCII version
\ of the 4tH manual. It works a lot like the Unix equivalent "man".
\ Note that due to the vast amount of information it collects, it requires
\ more than 64K of memory to compile and run correctly.

include lib/stmstack.4th               \ for >S, S>
include lib/leading.4th                \ for -LEADING
include lib/charat.4th                 \ for CHARIN
include lib/compare.4th                \ for COMPARE
include lib/argopen.4th                \ for ARG-OPEN

char " constant (")                    \ character "
char | constant (|)                    \ character |
                                       \ get the next line
: next-line refill 0= abort" Unexpected end of file" ;
: get-tib tib count -leading -trailing ;
: "-or-| 2dup (") charin if ." |" (|) else .| "| (") then >r type r> emit ;
                                       \ select C" or C|
: c"-or-c|                             ( a n --)
  2dup (") charin if ." c| " (|) else .| c" | (") then >r type r> emit
;                                      \ embed the string issued
                                       \ select ," or ,|
: ,"-or-,|                             ( a n --)
  2dup (") charin if ." ,| " (|) else .| ," | (") then >r type r> emit
;                                      \ embed the string issued
                                       \ print a line from a section
: print-line                           ( -- n)
  get-tib 2dup s" Editor manual" compare
  if                                   \ have we found the end of the chapter?
    dup if 4 spaces c"-or-c| ."  10 c," cr drop 0 else 2drop 1+ then next-line
  else                                 \ if not, deal with empty lines
    2drop drop max-n                   \ we're at the end of the chapter
  then                                 \ so signal it
;
                                       \ print an entire section
: print-section                        ( a n -- f)
  ."   TAG info-4th " "-or-| cr        \ start off with the tag
  0 begin dup 3 < while print-line repeat ."     0 c," cr
  begin get-tib 0= while drop next-line repeat drop max-n =
;                                      \ make sure we don't have an empty line
                                       \ process the entire file
: process                              ( --)
  ." \ Generated by MAKEINFO.4TH - Copyright J.L. Bezemer 2014" cr cr
  ." include lib/info.4th" cr cr       \ include the library
  ." width string search-key" cr cr    \ needed to prevent clobber
  ." offset info-4th" cr               \ start off with the sections
  begin bl parse -leading -trailing 2dup >s print-section until
;                                      \ print each section and save title
                                       \ create the entry table
: make-entries                         ( --)
  cr ." create .info-4th" cr           \ make the header
  begin
    s> s.error 0=                      \ have we processed all strings?
  while                                \ if not, create an entry point
    2 spaces 2dup ,"-or-,| space "-or-| ."  ," cr
  repeat                               \ get rid of string, terminate table
  2drop ."   NULL ," cr                \ now finish the program
  ." does> [: info-4th ;] is info-entry info ;" cr cr
  .| argn 2 < abort" Usage: info-4th keyword"| cr
  .| 1 args search-key place search-key count .info-4th| cr
;
                                       \ get to the proper section and start
: start-info? tib count s" ! CORE" compare 0= dup if process then ;
: create-info s.clear begin next-line start-info? until make-entries ;
                                       \ main program
: make-info                            ( --)
  argn 3 < abort" Usage: makeinfo text-file 4tH-source"
  input 1 arg-open output 2 arg-open over over use use create-info close close
;                                      \ open files, process, close files

make-info