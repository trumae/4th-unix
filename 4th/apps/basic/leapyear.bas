' Determine whether a given year is a leap year in the Gregorian calendar. 

DO
  INPUT "Enter a year: "; y
  IF FUNC(_FNleap(y)) THEN
    PRINT y; " is a leap year"
  ELSE
    PRINT y; " is not a leap year"
  ENDIF
LOOP
END

_FNleap Param (1)
RETURN ((a@ % 4 = 0) * ((a@ % 400 = 0) + (a@ % 100 # 0)))