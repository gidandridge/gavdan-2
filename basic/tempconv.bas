10 CLS$=CHR$(12)
20 CR$=CHR$(10)
30 LF$=CHR$(13)
40 PRINT CLS$;"Temperature converter"
50 PRINT CR$;LF$;"Which do you want..."
60 PRINT CR$;LF$;"(1) Celsius to Fahrenheit"
70 PRINT "(2) Farenheit to Celsius"
80 PRINT : INPUT Z : PRINT
90 IF Z=1 THEN GOTO 200
100 IF Z=2 THEN GOTO 300
110 END
200 INPUT "Enter degrees Celsius";CELSIUS
210 FARENHEIT=CELSIUS*9/5+32
220 PRINT "...is ";FARENHEIT;" degrees Farenhiet"
230 END
300 INPUT "Enter degrees Farenhiet";FARENHIET
310 CELSIUS=(FARENHIET-32)*5/9
320 PRINT "...is ";CELSIUS;" degrees Celsius"
330 END
