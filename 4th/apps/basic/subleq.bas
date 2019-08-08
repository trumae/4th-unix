' Subleq is an example of a One-Instruction Set Computer (OISC). It is named
' after its only instruction, which is SUbtract and Branch if Less than or
' EQual to zero.

' The machine's memory consists of an array of signed integers. Execution
' begins with the instruction pointer aimed at the first word, which is
' address 0. It proceeds as follows:

' - Let A, B, and C be the value stored in the three consecutive words in
'   memory starting at the instruction pointer.
' - Advance the instruction pointer 3 words to point at the address after
'   the one containing C.
' - If A is -1, then a character is read from standard input and its code
'   point stored in the address given by B. C is unused.
' - If B is -1, then the number contained in the address given by A is
'   interpreted as a code point and the corresponding character output. C is
'   again unused.
' - Otherwise, both A and B are treated as the addresses of memory locations.
'   The number contained in the address given by A is subtracted from the
'   number at the address given by B (and the result stored back in addr B).
'   If the result is zero or negative, the value C becomes the new instruction
'   pointer.
' - If the instruction pointer becomes negative, execution halts.

GoSub _Initialize                      ' Initialize memory

i = 0                                  ' Reset instruction counter

Do While i > -1                        ' While IP is not negative
  A = @(i)                             ' Fill the registers with
  B = @(i+1)                           ' opcodes and operands
  C = @(i+2)

  i = i + 3                            ' Increment instruction counter
                                       ' A<0 = Input, B<0 = Output
  If B < 0 Then Print CHR(@(A)); : Continue
  If A < 0 Then Input "Enter: ";@(B) : Continue
  @(B) = @(B) - @(A) : If @(B) < 1 Then i = C
Loop                                   ' Change memory contents
                                       ' And optionally the IP
End
                                       ' Corresponds to assembler language:
_Initialize                            ' start:
  @(0) = 15                            '   zero, message, -1
  @(1) = 17
  @(2) = -1
  @(3) = 17                            '   message, -1, -1
  @(4) = -1
  @(5) = -1
  @(6) = 16                            '   neg1, start+1, -1
  @(7) = 1
  @(8) = -1
  @(9) = 16                            '   neg1, start+3, -1
  @(10) = 3
  @(11) = -1
  @(12) = 15                           '   zero, zero, start
  @(13) = 15
  @(14) = 0
  @(15) = 0                            ' zero: 0
  @(16) = -1                           ' neg1: -1
  @(17) = 72                           ' message: "Hello, world!\n\0"
  @(18) = 101
  @(19) = 108
  @(20) = 108
  @(21) = 111
  @(22) = 44
  @(23) = 32
  @(24) = 119
  @(25) = 111
  @(26) = 114
  @(27) = 108
  @(28) = 100
  @(29) = 33                           ' Works only with ASCII
  @(30) = 10                           ' Replace with ORD(c) when required
  @(31) = 0
Return