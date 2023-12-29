9000 REM
9010 REM ******************************
9020 REM ** SETUP VT102 ESCAPE CODES **
9030 REM ** G.DANDRIDGE - NOV 2022   **
9040 REM ******************************
9050 REM
9060 ES$=CHR$(27)   : REM ESCAPE
9070 CC$=ES$+"[m"   : REM CLEAR CHARACTER ATTRIBUTES
9080 CS$=ES$+"[2J"  : REM CLEAR SCREEN
9090 BD$=ES$+"[1m"  : REM BOLD
9100 UL$=ES$+"[4m"  : REM UNDERLINE
9110 BK$=ES$+"[5m"  : REM BLINK
9120 RV$=ES$+"[7m"  : REM REVERSE