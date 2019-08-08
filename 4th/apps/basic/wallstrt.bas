' Allegedly written by Richard Altwasser for the ZX Spectrum, 1983
' Modified by Anonymous, 1984
' Rewritten for uBasic by Hans Bezemer, 2014, 2015

' @() Mapping
' ===========
' 000 - 099 : 20-day statistics on 5 companies : P(5,20)
' 100 - 104 : Good fortune per company         : D(5)
' 105 - 109 : Economy effects per company      : E(5)
' 110 - 114 : Shares per company owned         : Q(5)
' 115 - 119 : Current companies                : C$(5), D$(5)

                                       ' Initialize companies
@(115) = 1000 : @(116) = 1100 : @(117) = 1200
@(118) = 1300 : @(119) = 1400 : R = 1500
                                       ' Initialize share prices
For x = 0 To 4 : @(x * 20)= 100 + (Rnd(20) - Rnd(20)) : Next

D = 1                                  ' This is "day 1"
C = 10000                              ' Starting capital

Do                                     ' Main loop
  Proc _PrintTable                     ' Print stock table
  Proc _ChangePrices                   ' Change the prices
  Proc _ChangeEconomy                  ' See what the economy does
  Proc _DisplayGraphs                  ' Examine graphs per company
Loop

End

                                       ' Print table and allow trading
_PrintTable                            ' ( --)
  Local (4)

  a@ = (D+18) % 20                     ' Represents yesterday
  b@ = (D+19) % 20                     ' Represents today

  Do
    Print "TABLE OF STOCKS HELD"       ' Print the stock table
    Print
    Print "STOCK  PRICE  CHANGE  QTY   VALUE"
    Print "-----  -----  ------  ---   -----"

    For c@ = 0 To 4                    ' Now show all companies
      d@ = (@(c@ * 20 + b@) - @(c@ * 20 + a@)) * (D>1)
      GoSub @(115+c@)                  ' Print company name
      Print Tab(9);Using "__#";@(c@ * 20 + b@);Tab(14);
                                       ' Print the stock price
      If d@ < 0 Then Print d@;         ' Now evaluate the changes
      If d@ = 0 Then Print "NIL";
      If d@ > 0 Then Print "+";d@;
      Print Tab(22);

      If @(110 + c@) =0 Then           ' Print number of stock owned
         Print "NIL";
      Else
         Print Using "__#";@(110 + c@);
      EndIf
                                       ' And the value of your stock
      Print Using "_______#";@(110 + c@) * @(c@ * 20 + b@)
    Next

    Print
    Print "CASH IN HAND ";C            ' That's your working capital
    Print
    Print "1= BUYING"
    Print "2= SELLING"
    Print "3= NO FURTHER TRADING"
    Print                              ' You may trade now

    Do                                 ' Enter 1-3
      Input "ENTER YOUR OPTION 1-3 ";c@
      Until (c@ > 0) * (c@ < 4)
    Loop                               ' Loop until input is valid
                                       ' Optional stock trading
    If c@ = 1 Then Proc _TradeShares (1)
    If c@ = 2 Then Proc _TradeShares (-1)
    Until c@ = 3                       ' "3" exits
  Loop
Return

                                       ' Trade shares
_TradeShares Param(1)                  ' ( n --)
  Local(2)                             ' Buying or selling?

  Print
  For b@=0 To 4 : Print b@+1;"= "; : GoSub @(115+b@) : Print : Next
  Print

  Do                                   ' Enter 1-5
    Input "ENTER YOUR CHOICE : ";b@
    Until (b@ > 0) * (b@ < 6)
  Loop                                 ' Loop until input is valid

  b@ = b@ - 1                          ' Adjust company number entered
  Print "HOW MANY "; : GoSub @(115+b@) : Print " SHARES DO YOU WISH TO ";
                                       ' Now pop up the question
  If a@ = 1 Then                       ' Differentiate between buying
     Input "BUY ? ";c@                 ' And selling
  Else
     Input "SELL ? ";c@
  EndIf

  Do
    c@ = c@ * a@                       ' Buying or selling?

    If c@ < -@(110+b@) Then            ' Do we have that shares?
      Print "YOU ONLY HAVE ";@(110+b@);" SHARES TO SELL, ";
      Input "TRY AGAIN ";c@            ' If not, try again
    Else                               ' Not acquiring too many stock?
      If c@ + @(110+b@) > 999 Then     ' If so, try again
        Input "YOU MAY ONLY HOLD 999 SHARES OF ANY STOCK, TRY AGAIN ";c@
      Else                             ' Can we afford the buy?
        If (c@ * @(b@*20 + ((D+19) % 20))) > C Then
          Input "YOU CAN'T AFFORD IT. TRY AGAIN ";c@
        Else                           ' Everything OK, let's close the deal
          @(110+b@) = @(110+b@) + c@
          C = C - c@ * @(b@*20 + ((D+19) % 20))
          Break                        ' Break out of the loop, we're done
        EndIf
      EndIf
    EndIf

  Loop                                 ' Loop until you get it right
Return

                                       ' Prompt for graphs
_DisplayGraphs                         ' ( --)
  Local (1)

  Do
    Print "DISPLAY PRICE GRAPHS"       ' Print headers
    Print
    Print "1-5 DISPLAY A GRAPH"
    Print                              ' Show current companies
    For a@ = 0 To 4: PRINT a@+1;"= "; : GoSub @(115+a@) : Print : Next
    Print                              ' Or exit this menu
    Print "6= SHOW STOCK TABLE"
    Print

    Do                                 ' Enter 1-6
      Input "ENTER 1 TO 6 ";a@
      Until (a@ > 0) * (a@ < 7)
    Loop                               ' Loop until input is valid
                                       ' Return to stock table
    a@ = a@ - 1
  Until a@ = 5
    Proc _ShowGraph (D, a@)            ' Show the graph
  Loop

  Print
Return

                                       ' Print the ordinal day
_PrintDate Param(1)                    ' ( n --)
                                       ' Ensure 11, 12 are OK
  If ((a@ > 3) * (a@ < 21)) + ((a@ % 10 > 3) + (a@ % 10 = 0))
     Print a@;"th";
  Else                                 ' Ends in 1, then "First"
     If (a@ % 10 = 1) Then
        Print a@;"st";
     Else                              ' Ends in 2, then "Second"
        If (a@ % 10 = 2) Then
           Print a@;"nd";
       Else                            ' Ends in 3, then "Third"
          If (a@ % 10 = 3) Then
             Print a@;"rd";
          EndIf
       EndIf
     EndIf
  EndIf

Return

                                       ' Calculate next trading day, show news
_ChangePrices                          ' ( --)
  Local(4)

  Print
  Print "WALLSTREET NEWS   ";          ' Print header
  Proc _PrintDate ((D % 365) + (D/365 > 0))
  Print " day ";D/365+1984
  Print

  D = D + 1                            ' Increment day
  a@ = (D+18) % 20                     ' Represents yesterday
  b@ = (D+19) % 20                     ' Represents today

  For c@ = 0 To 4                      ' Loop through all companies
    @(100 + c@) = @(100 + c@) - 1
    @(c@ * 20 + b@) = @(c@ * 20 + a@) + (@(105 + c@) * (7 + Rnd(6)))/10
                                       ' Calculate new share price
    If @(100 + c@) < 1 Then            ' Issue a company specific new item
       @(100 + c@) = 1 + Rnd(10)
       Proc _PriceTrends (c@)
    EndIf

    If @(c@ * 20 + b@) < 20 Then       ' This one is going bankrupt
       Push @(115 + c@) : @(115 + c@) = R : R = Pop()
       For d@ = 0 To 20 : @(c@ * 20 + d@) = 100 : Next
       @(100 + c@) = 0  : @(105 + c@) = 0 :  @(110 + c@) = 0
       GoSub R : Print " has presented a bankruptcy petition. ";
       GoSub @(115 + c@) : Print " is back in "; : GoSub @(115 + c@) + 50
       Print "!" : Print
    EndIf                              ' Default the new companies data
                                       ' Limit the growth of a company
    If @(c@ * 20 + b@) > 340 Then @(c@ * 20 + b@) = 340
  Next                                 ' Evaluate next company
Return

                                       ' Calculate trends per company
_PriceTrends Param(1)                  ' ( n --)
  Local (1)                            ' Get company

  b@ = Sgn(Rnd(10) - Rnd(10))          ' Which way is the economy going

  @(105 + a@) = (Rnd(10) + 2) * b@     ' Determine effects per company
  GoSub 1600 + 10*(Rnd(7)) (a@, b@)
Return                                 ' Generate news reel per company

                                       ' Calculate general economy, show news
_ChangeEconomy                         ' ( --)
  If Rnd(10) < 6 Then Return
  Local (3)

  a@ = (D+19) % 20                     ' Represents today
  b@ = Sgn(Rnd(10) - Rnd(10))          ' Which way is the economy going

  For c@ = 0 To 4                      ' Loop through all companies
    If Sgn (@(c@ * 20 + a@) - 180) # b@ Then
      @(c@ * 20 + a@) = @(c@ * 20 + a@) + (b@ * (5 + Rnd(6)))
      @(105 + c@) = @(105 + c@) + b@
    EndIf
  Next

  GoSub 1700 + 50*(b@>0) + (10*Rnd(5))
Return

                                       ' Show graph for company on specific day
_ShowGraph Param (2)                   ' (n1 n2 --)
  Local(2)

  Print
  Print "SHARE PRICE MOVEMENTS "; : GoSub @(115 + b@) : Print
  Print                                ' Get the company and the day

  b@ = b@ * 20

  For c@ = 340 To 0 Step -20           ' Print the graph top down
    Print Using "__#";c@;"| ";         ' Print the vertical scale
    For d@ = Abs(a@ - 20) * (a@ < 20) To 19
      If (@(b@ + ((a@ + d@) % 20)) > (c@ - 1)) * (@(b@ + ((a@ + d@) % 20)) < (c@ + 20)) Then
         Print Tab((d@ - Abs(a@ - 20) * (a@ < 20)) * 3 + 5);"*";
      EndIf                            ' Plot the graph, compensate for day<20
    Next                               ' Test next horizontal point
    Print                              ' Finish the line
  Next                                 ' Next vertical line
                                       ' Print horizontal scale
  Print "   +------------------------------------------------------------"
  Print "      1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20"
  Print
Return

                                       ' Names and businesses of companies
1000 Print "NAM";         : Return
1050 Print "Minerals";    : Return
1100 Print "HAK";         : Return
1150 Print "Preserves";   : Return
1200 Print "JVC";         : Return
1250 Print "Electronics"; : Return
1300 Print "BMW";         : Return
1350 Print "Motor-cars";  : Return
1400 Print "C&A";         : Return
1450 Print "Clothing";    : Return
1500 Print "IBM";         : Return
1550 Print "Computers";   : Return
                                       ' Headlines for news reel per company
1600 Param (2)
     Print "Major "; : GoSub @(115+a@)+50 : Print " industry strike ";
     If b@>0 Then
        Print "averted"
     Else
        Print "planned"
     EndIf
     Print
Return

1610 Param (2)
     GoSub @(115+a@)+50 : Print " industry affected by import duty ";
     If b@>0 Then
        Print "reduction"
     Else
        Print "increase"
     EndIf
     Print
Return

1620 Param (2)
     Print "World trade conference predicts "; : GoSub @(115+a@)+50
     If b@>0 Then
        Print " boom"
     Else
        Print " slum"
     EndIf
     Print
Return

1630 Param (2)
     Print "Senior "; : GoSub @(115+a@) : Print " manager ";
     If b@>0 Then
        Print "wins award for efficiency"
     Else
        Print "named in fraud case"
     EndIf
     Print
Return

1640 Param (2)
     Print "Scientists announce discovery that could make "; : GoSub @(115+a@)+50 : Print " industry ";
     If b@>0 Then
        Print "more profitable"
     Else
        Print "redundant"
     EndIf
     Print
Return

1650 Param (2)
     Print "Government announce new "; : GoSub @(115+a@)+50
     If b@>0 Then
        Print " subsidy"
     Else
        Print " tax"
     EndIf
     Print
Return

1660 Param (2)
     GoSub @(115+a@) : Print " chairman predicts ";
     If b@>0 Then
        Print "higher profits"
     Else
        Print "more layoffs"
     EndIf
     Print
Return
                                       ' Headlines for newsreel economy
1700 Print "Chancellor promises less tax": Print : Return
1710 Print "Trade secretary announces major deals with Arab states": Print : Return
1720 Print "\qFall in interest rates soon\q says Treasury spokesman": Print : Return
1730 Print "New trade links with China": Print : Return
1740 Print "Tory popularity grows as unemployment falls": Print : Return
1750 Print "Employment secretary predicts gloomy future": Print : Return
1760 Print "Interest rates rise by 3/4 %": Print : Return
1770 Print "CBI chairman has little hope for industrial growth": Print : Return
1780 Print "TUC talk of general strike soon": Print : Return
1790 Print "Labour party win key by-election": Print : Return