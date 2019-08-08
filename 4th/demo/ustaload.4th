\ 4tH demo - US state DB loader - Copyright 2016 J.L. Bezemer
\ You can redistribute this file and/or modify it under
\ the terms of the GNU General Public License

\ This DB uses data from:
\ https://en.wikipedia.org/wiki/List_of_states_and_territories_of_the_United_States

\ The resulting CSV file is very plain, so we don't need the overhead of
\ loading extensive CSV support. It can be found in the "/apps/data" directory.

include lib/dbms.4th                   \ include database engine

struct
  32 +field State                      \ US state name
   4 +field Abbr                       \ US state abbreviation
  16 +field Capital                    \ US state capital
  16 +field Population                 \ US state population
   8 +field Area                       \ US state area (km*km)
   8 +field Seats                      \ US state House seats
end-struct /UState                     \ total size of buffer

/UState buffer: UState                 \ allocate room for buffer

: Field> [char] ; parse ;              \ helper word to read the fields

." Creating US states database.." cr

s" ustates.dbm" db.create              \ create the DB file
UState /UState  s" ustates.dbm" db.declare to US
US db.use                              \ declare and use it

128 {char} db.key State idx.init abort" Cannot create 'State' index"
to US.State

128 {char} db.key Abbr idx.init abort" Cannot create 'Abbr' index"
to US.Abbr

128 {char} db.key Capital idx.init abort" Cannot create 'Capital' index"
to US.Capital

US.State US.Abbr US.Capital 3 dbs.bind abort" Cannot bind indexes"

s" ustates.csv" input open             \ open the CSV file and abort on error
error? abort" Cannot open 'ustates.csv'"
dup use                                \ everything OK, now use it
                                       \ skip the field description
refill 0= abort" Cannot read 'ustates.csv'"

begin
  refill                               \ read next CSV record
while
  db.clear                             \ clear the DB buffer
    Field> db.buffer -> State      place
    Field> db.buffer -> Abbr       place
    Field> db.buffer -> Capital    place
    Field> db.buffer -> Population place
    Field> db.buffer -> Area       place
    Field> db.buffer -> Seats      place
  dbs.insert                           \ read fields and insert record
repeat

close                                  \ close the CSV file
                                       \ save 'state' index
US.State dup s" usstate.idx"  idx.save
idx.close abort" Cannot close 'State' index"
                                       \ save 'abbr' index
US.Abbr dup s" usabbr.idx" idx.save
idx.close  abort" Cannot close 'Abbr' index"
                                       \ save 'capital' index
US.Capital  dup s" uscapitl.idx" idx.save
idx.close  abort" Cannot close 'Capital' index"
                                       \ release indexes
dbs.release abort" Cannot release indexes"
db.close                               \ close dictionary

." US states database successfully created.." cr
