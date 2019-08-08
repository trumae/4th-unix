' ** Several sorting routines
' ** Converted to uBasic, J.L. Bezemer - 2013,2015
' **
' ** Taken from various Rosetta code examples
' ** http://rosettacode.org

PRINT "Shell sort:"
  n = FUNC (_InitArray)
  PROC _ShowArray (n)
  PROC _Shellsort (n)
  PROC _ShowArray (n)
PRINT

PRINT "Comb sort:"
  n = FUNC (_InitArray)
  PROC _ShowArray (n)
  PROC _Combsort (n)
  PROC _ShowArray (n)
PRINT

PRINT "Insertion sort:"
  n = FUNC (_InitArray)
  PROC _ShowArray (n)
  PROC _Insertionsort (n)
  PROC _ShowArray (n)
PRINT

PRINT "Selection sort:"
  n = FUNC (_InitArray)
  PROC _ShowArray (n)
  PROC _Selectionsort (n)
  PROC _ShowArray (n)
PRINT

PRINT "Bubble sort:"
  n = FUNC (_InitArray)
  PROC _ShowArray (n)
  PROC _Bubblesort (n)
  PROC _ShowArray (n)
PRINT

PRINT "Gnome sort:"
  n = FUNC (_InitArray)
  PROC _ShowArray (n)
  PROC _Gnomesort (n)
  PROC _ShowArray (n)
PRINT

PRINT "Cocktail sort:"
  n = FUNC (_InitArray)
  PROC _ShowArray (n)
  PROC _Cocktailsort (n)
  PROC _ShowArray (n)
PRINT

PRINT "Circle sort:"
  n = FUNC (_InitArray)
  PROC _ShowArray (n)
  PROC _Circlesort (n)
  PROC _ShowArray (n)
PRINT

PRINT "Quick sort:"
  n = FUNC (_InitArray)
  PROC _ShowArray (n)
  PROC _Quicksort (n)
  PROC _ShowArray (n)
PRINT

PRINT "Heap sort:"
  n = FUNC (_InitArray)
  PROC _ShowArray (n)
  PROC _Heapsort (n)
  PROC _ShowArray (n)
PRINT

PRINT "Stooge sort:"
  n = FUNC (_InitArray)
  PROC _ShowArray (n)
  PROC _Stoogesort (n)
  PROC _ShowArray (n)
PRINT

PRINT "Pancake sort:"
  n = FUNC (_InitArray)
  PROC _ShowArray (n)
  PROC _Pancakesort (n)
  PROC _ShowArray (n)
PRINT

PRINT "Simple sort:"
  n = FUNC (_InitArray)
  PROC _ShowArray (n)
  PROC _Simplesort (n)
  PROC _ShowArray (n)
PRINT

END


_Shellsort PARAM (1)                   ' Shellsort subroutine
  LOCAL (4)
  b@ = a@

  DO WHILE b@
    b@ = b@ / 2
    FOR c@ = b@ TO a@ - 1
      e@ = @(c@)
      d@ = c@
      DO WHILE (d@ > b@-1) * (e@ < @(ABS(d@ - b@)))
        @(d@) = @(d@ - b@)
        d@ = d@ - b@
      LOOP
      @(d@) = e@
    NEXT
  LOOP
RETURN


_Combsort PARAM (1)                    ' Combsort subroutine
  LOCAL(4)
  b@ = a@
  c@ = 1

  DO WHILE (b@ > 1) + c@

    b@ = (b@ * 10) / 13

    IF (b@ = 9) + (b@ = 10) THEN b@ = 11
    IF b@ < 1 THEN b@ = 1

    c@ = 0
    d@ = 0
    e@ = b@

    DO WHILE e@ < a@
      IF @(d@) > @(e@) THEN PROC _Swap (d@, e@) : c@ = 1
      d@ = d@ + 1
      e@ = e@ + 1
    LOOP
  LOOP
RETURN


_Insertionsort PARAM (1)               ' Insertion sort
  LOCAL (3)

  FOR b@ = 1 TO a@-1
    c@ = @(b@)
    d@ = b@
    DO WHILE (d@>0) * (c@ < @(ABS(d@-1)))
        @(d@) = @(d@-1)
        d@ = d@ - 1
    LOOP
    @(d@) = c@
  NEXT
RETURN


_Selectionsort PARAM (1)               ' Selection sort
  LOCAL (3)

  FOR b@ = 0 TO a@-1
    c@ = b@

    FOR d@ = b@ TO a@-1
      IF @(d@) < @(c@) THEN c@ = d@
    NEXT

    IF b@ # c@ THEN PROC _Swap (b@, c@)
  NEXT
RETURN


_Bubblesort PARAM(1)                   ' Bubble sort
  LOCAL (2)

  DO
    b@ = 0
    FOR c@ = 1 TO a@-1
      IF @(c@-1) > @(c@) THEN PROC _Swap (c@, c@-1) : b@ = c@
    NEXT
    a@ = b@
    UNTIL b@ = 0
  LOOP

RETURN


_Gnomesort PARAM (1)                   ' Gnome sort
  LOCAL (2)
  b@=1
  c@=2

  DO WHILE b@ < a@
    IF @(b@-1) > @(b@) THEN
      PROC _Swap (b@, b@-1)
      b@ = b@ - 1
      IF b@ THEN
        CONTINUE
      ENDIF
    ENDIF
    b@ = c@
    c@ = c@ + 1
  LOOP
RETURN


_Cocktailsort PARAM (1)                ' Cocktail sort
  LOCAL (2)
  b@ = 0

  DO WHILE b@ = 0
    b@ = 1
    FOR c@=1 TO a@-1
      IF @(c@) < @(c@-1) THEN PROC _Swap (c@, c@-1) : b@ = 0
    NEXT
  UNTIL b@
    b@ = 1
    FOR c@=a@-1 TO 1 STEP -1
      IF @(c@) < @(c@-1) THEN PROC _Swap (c@, c@-1) : b@ = 0
    NEXT
  LOOP
RETURN


_InnerCircle PARAM (2)
  LOCAL (3)
  c@ = a@
  d@ = b@
  e@ = 0

  IF c@ = d@ THEN RETURN (0)

  DO WHILE c@ < d@
    IF @(c@) > @(d@) THEN PROC _Swap (c@, d@) : e@ = e@ + 1
    c@ = c@ + 1
    d@ = d@ - 1
  LOOP

  e@ = e@ + FUNC (_InnerCircle (a@, d@))
  e@ = e@ + FUNC (_InnerCircle (c@, b@))
RETURN (e@)


_Circlesort PARAM(1)                   ' Circle sort
  DO WHILE FUNC (_InnerCircle (0, a@-1))
  LOOP
RETURN


_InnerQuick PARAM(2)
  LOCAL(4)

  IF b@ < 2 THEN RETURN
  f@ = a@ + b@ - 1
  c@ = a@
  e@ = f@
  d@ = @((c@ + e@) / 2)

  DO
    DO WHILE @(c@) < d@
      c@ = c@ + 1
    LOOP

    DO WHILE @(e@) > d@
      e@ = e@ - 1
    LOOP

    IF c@ - 1 < e@ THEN
      PROC _Swap (c@, e@)
      c@ = c@ + 1
      e@ = e@ - 1
    ENDIF

    UNTIL c@ > e@
  LOOP

  IF a@ < e@ THEN PROC _InnerQuick (a@, e@ - a@ + 1)
  IF c@ < f@ THEN PROC _InnerQuick (c@, f@ - c@ + 1)
RETURN


_Quicksort PARAM(1)                   ' Quick sort
  PROC _InnerQuick (0, a@)
RETURN


_Heapify PARAM(1)
  LOCAL(1)

  b@ = (a@ - 2) / 2
  DO WHILE b@ > -1
     PROC _Siftdown (b@, a@)
     b@ = b@ - 1
  LOOP
RETURN


_Siftdown PARAM(2)
  LOCAL(2)
  c@ = a@

  DO WHILE ((c@ * 2) + 1) < (b@)
    d@ = c@ * 2 + 1
    IF d@+1 < b@ IF @(d@) < @(d@+1) THEN d@ = d@ + 1
  WHILE @(c@) < @(d@)
    PROC _Swap (d@, c@)
    c@ = d@
  LOOP

RETURN


_Heapsort PARAM(1)                     ' Heapsort
  LOCAL(1)
  PROC _Heapify (a@)

  b@ = a@ - 1
  DO WHILE b@ > 0
     PROC _Swap (b@, 0)
     PROC _Siftdown (0, b@)
     b@ = b@ - 1
  LOOP
RETURN


_InnerStooge PARAM(2)                  ' Stoogesort
  LOCAL(1)

  IF @(b@) < @(a@) Then Proc _Swap (a@, b@)
  IF b@ - a@ > 1 THEN
    c@ = (b@ - a@ + 1)/3
    PROC _InnerStooge (a@, b@-c@)
    PROC _InnerStooge (a@+c@, b@)
    PROC _InnerStooge (a@, b@-c@)
  ENDIF
RETURN


_Stoogesort PARAM(1)
  PROC _InnerStooge (0, a@ -  1)
RETURN


_Flip PARAM(1)
  LOCAL(1)

  b@ = 0

  DO WHILE b@ < a@
    PROC _Swap (b@, a@)
    b@ = b@ + 1
    a@ = a@ - 1
  LOOP
RETURN


_Pancakesort PARAM (1)                 ' Pancakesort
  LOCAL(3)

  IF a@ < 2 THEN RETURN

  FOR b@ = a@ TO 2 STEP -1

    c@  = 0

    FOR d@ = 0 TO b@ - 1
      IF @(d@) > @(c@) THEN c@ = d@
    NEXT

    IF c@ = b@ - 1 THEN CONTINUE
    IF c@ THEN PROC _Flip (c@)
    PROC _Flip (b@ - 1)

  NEXT
RETURN


_Simplesort PARAM(1)                   ' Simplesort
  LOCAL(2)

  FOR b@ = 0 TO a@ - 1
    FOR c@ = b@+1 TO a@ - 1
      IF @(b@) > @ (c@) THEN PROC _Swap (b@, c@)
    NEXT
  NEXT
RETURN


_Swap PARAM(2)                        ' Swap two array elements
  PUSH @(a@)
  @(a@) = @(b@)
  @(b@) = POP()
RETURN


_InitArray                             ' Init example array
  PUSH 4, 65, 2, -31, 0, 99, 2, 83, 782, 1

  FOR i = 0 TO 9
    @(i) = POP()
  NEXT

RETURN (i)


_ShowArray PARAM (1)                   ' Show array subroutine
  FOR i = 0 TO a@-1
    PRINT @(i),
  NEXT

  PRINT
RETURN
