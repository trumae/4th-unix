' Write a function or program which:
' 1. takes a positive integer representing a duration in seconds as input
'    (e.g., 100), and
' 2. returns a string which shows the same duration decomposed into weeks,
'    days, hours, minutes, and seconds as detailed below (e.g., "1 min,
'    40 sec").


Proc _CompoundDuration(7259)
Proc _CompoundDuration(86400)
Proc _CompoundDuration(6000000)

End


_CompoundDuration Param(1)             ' Print compound seconds
  a@ = FUNC(_Compound(a@, 604800, _wk))
  a@ = FUNC(_Compound(a@, 86400, _d))
  a@ = FUNC(_Compound(a@, 3600, _hr))
  a@ = FUNC(_Compound(a@, 60, _min))

  If a@ > 0 Then Print a@;" sec";      ' Still seconds left to print?
  Print
Return


_Compound Param(3)
  Local(1)

  d@ = a@/b@                           ' Get main component
  a@ = a@%b@                           ' Leave the rest

  If d@ > 0 Then                       ' Print the component
    Print d@;
    Proc c@

    If a@ > 0 Then
      Print ", ";                      ' If something follows, take
    EndIf                              ' care of the comma
  EndIf
Return (a@)


_wk  Print " wk";  : Return
_d   Print " d";   : Return
_hr  Print " hr";  : Return
_min Print " min"; : Return
