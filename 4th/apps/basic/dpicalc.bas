' ***********************************************************
' PROGRAM:      dpicalc.bas
' PURPOSE:      to calculate dpi for a given screen resolution
'                       and size
' AUTHOR:               vovchik (Puppy Linux forum)
' COMMENTS:     ported from an Arch Linux python script
' DEPENDS:      bash, bacon
' PLATFORM:     Puppy Linux (actually, any *nix)
' DATE:         22-01-2010
' VERSION:      0.1
' ***********************************************************

' *****************
' MAIN
' *****************

PROC _GetArgs
m = FUNC (_CalcDPI(h, v, d))
PROC _ShowResults
END

' *****************
' END MAIN
' *****************

' *****************
' FUNCTIONs
' *****************

_SQR PARAM (2)                         ' This is an integer SQR subroutine.
  LOCAL (1)                            ' A@ holds 10, B@ holds 4, now comes C@

  b@ = (10^(b@*2)) * a@                ' Output is scaled by 10^B@).
  a@ = b@

  DO
    c@ = (a@ + (b@ / a@))/2
  UNTIL (Abs(a@ - c@) < 2)
    a@ = c@
  LOOP

RETURN (c@)


_CalcDPI PARAM (3)                     ' Some minor loss of precision may occur
RETURN (10*FUNC(_SQR((a@*a@) + (b@*b@),1))/c@)


_GetArgs
  PRINT "uBasic DPI Calculator"
  PRINT "Where"
  INPUT "  hres = horizontal resolution in pixels: ";h
  INPUT "  vres = vertical resolution in pixels  : ";v
  INPUT "  size = diag. size of screen in inches : ",d
RETURN


_ShowResults
   PRINT "Calculated DPI for screen resolution of "
   PRINT h;"x";v;" pixels and ";d;"\q in diagonal = ";m/100;".";m%100;" dpi"
RETURN

' *****************
' END FUNCTIONS
' *****************
