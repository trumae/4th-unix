40 r=4*13
50 for i=1 to 13:@(27+i)=4:next
60 for i=1 to 9
70 gosub 1250
80 @(27+k)=@(27+k)-1
90 @(13+k)=@(13+k)+1
100 gosub 1250
110 @(27+k)=@(27+k)-1
120 @(k)=@(k)+1
130 next i
140 print "Go Fish"
150 print "======="
160 print
180 ' PRINT_HAND
190 print "Score: Player: ";@(84),"Computer: ";@(85),
200 print r;" cards remaining" : print
210 print "Your hand: ";
220 for i=1 to 13
230 if @(i)=0 then 280
240 for j=1 to @(i)
250 gosub 2000 + (i*10) : print " ";
260 next j
270 ' NEXT_CARD
280 next i
290 print
300 ' ASK_CARD
310 gosub 1100
320 t=1400
330 input "Which card do you ask for (1 = Ace, 11 = Jack, 12 = Queen, 13 = King): ";z
360 if (z<1) + (z>13) then print "Sorry, that is not a valid choice.":goto 310
370 if @(z)=0 then print "You do not have that card!":goto 310
380 @(41+z)=1
390 if @(13+z)=0 then gosub t: print ", go fish!": gosub t : print " draws a ";:gosub 800:gosub 920:goto 470
400 v=@(13+z)
410 @(13+z)=0
420 @(z)=@(z)+v
430 gosub t: print " gets ";v;" more cards."
440 gosub 920
450 goto 190
460 ' COMPUTER_TURN_1
470 t=1500
480 for i=1 to 13
490 @(69+i)=0
500 next
510 ' COMPUTER_TURN_2
520 gosub 1100
530 p=0
540 for i=1 to 13
550 if (@(13+i)>0) * (@(41+i)>0) then @(55+i)=1:p=p+1
560 next
570 if p=0 then 650
580 ' DRAW_GUESS
590 k=rnd(13)+1
600 if @(55+k)=0 then 590
610 @(41+k)=0
620 @(69+k)=1
630 goto 680
640 ' DRAW_RAND
650 k=rnd(13)+1
660 if (@(13+k)=0) + (@(69+k)#0) then 650
670 ' MAKE_TURN
680 gosub t: print " wants your ";: gosub 2000 + (k*10) : print "'s."
690 @(69+k)=1
700 if @(k)=0 then gosub t: print ", go fish!": gosub t: print " draws a ";:gosub 860:gosub 1010:goto 190
710 v=@(k)
720 @(k)=0
730 @(13+k)=@(13+k)+v
740 gosub t: print " gets ";v;" more cards."
750 gosub 1010
760 goto 520
770 goto 190
780 end
790 ' DRAW_CARD_P
800 gosub 1250
810 gosub 2000 + (k*10) : print "."
820 @(27+k)=@(27+k)-1
830 @(k)=@(k)+1
840 return
850 ' DRAW_CARD_C
860 gosub 1250
870 print "card."
880 @(27+k)=@(27+k)-1
890 @(13+k)=@(13+k)+1
900 return
910 ' CHECK_BOOK_P
920 for i=1 to 13
930 if @(i)#4 then 980
940 gosub t: print " completes book of ";: gosub 2000 + (i*10) : print "'s."
950 @(i)=0
960 @(84)=@(84)+1
970 ' NEXT_LOOP_P
980 next i
990 return
1000 ' CHECK_BOOK_C
1010 for i=1 to 13
1020 if @(13+i)#4 then 1070
1030 gosub t: print " completes book of ";: gosub 2000 + (i*10) : print "'s."
1040 @(13+i)=0
1050 @(85)=@(85)+1
1060 ' NEXT_LOOP_C
1070 next i
1080 return
1090 ' CHECK_END
1100 q=0:c=0
1110 for i=1 to 13
1120 q=q+@(i)
1130 c=c+@(13+i)
1140 next i
1150 if (r=0) + (q=0) + (c=0) then 1180
1160 return
1170 ' END_GAME
1180 print
1190 print "*** Game over! ***"
1200 print
1210 if @(84)>@(85) then print "Player has won.":end
1220 if @(84)<@(85) then print "The computer has won.":end
1230 print "It's a tie!":end
1240 ' DEAL_CARD
1250 r=r-1
1260 s=rnd(r)+1
1270 for k=1 to 13
1280 s=s-@(27+k)
1290 if s<1 then unloop : return
1300 next
1400 Print "Player"; : Return
1500 Print "Computer"; : Return
2010 Print "Ace"; : Return
2020 Print "2"; : Return
2030 Print "3"; : Return
2040 Print "4"; : Return
2050 Print "5"; : Return
2060 Print "6"; : Return
2070 Print "7"; : Return
2080 Print "8"; : Return
2090 Print "9"; : Return
2100 Print "10"; : Return
2110 Print "Jack"; : Return
2120 Print "Queen"; : Return
2130 Print "King"; : Return
