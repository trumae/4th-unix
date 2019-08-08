' Lucas-Lehmer Test: for p an odd prime, the Mersenne number 2^p − 1 is prime
' if and only if 2^p − 1 divides S(p − 1)
' where S(n + 1) = (S(n))^2 − 2, and S(1) = 4.

m = 15
n = 1
For j = 2 To m
    If j = 2 Then
        s = 0
    Else
        s = 4
    EndIf
    n = (n + 1) * 2 - 1
    For i = 1 To j - 2
        s = (s * s - 2) % n
    Next i
    If s = 0 Then Print "M";j
Next
