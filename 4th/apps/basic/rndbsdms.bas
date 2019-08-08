' "Linear Congruential Generator" for uBasic, Copyright J.L. Bezemer 2014
' You can redistribute this file and/or modify it under
' the terms of the GNU General Public License

' The task is to replicate two historic random number generators. One is the
' rand() function from BSD libc, and the other is the rand() function from the
' Microsoft C Runtime (MSCVRT.DLL). Each replica must yield the same sequence
' of integers as the original generator, when starting from the same seed.

' NOTE: This implementation only works for the 32 bit version!!
                                       ' This is the 32 bit version
w = 32                                 ' Change for different integer size
b = 0                                  ' Initial BSD seed
m = 0                                  ' Initial MS seed

Print "BSD"                            ' Get the first 10 numbers from BSD
For i = 1 To 10
    GoSub _randBSD
    Print Pop()
Next i

Print

Print "Microsoft"                      ' Get the first 10 numbers from MS
For i = 1 To 10
    GoSub _randMS
    Print Pop()
Next i

End


_randBSD                               ' ( n1 -- n2)
    Push (1103515245 * b + 12345)      ' Compensate for the sign bit
    If Tos() < 0 Then Push (Pop() - (2 ^ (w-1)))
    b = Pop() % (2 ^ 31)               ' Now we got a number less than 2^31
    Push b                             ' So we can complete the operation
Return


_randMS                                ' ( n1 -- n2)
    Push (214013 * m + 2531011)        ' Compensate for the sign bit
    If Tos() < 0 Then Push (Pop() - (2 ^ (w-1)))
    m =  Pop() % (2 ^ 31)              ' Now we got a number less than 2^31
    Push m / (2 ^ 16)                  ' So we can complete the operation
Return
