10 PRINT : PRINT
20 PRINT "COWBOY SHOOTOUT -"
30 PRINT "YOU ARE BACK TO BACK"
40 PRINT "TAKE 10 PACES..."
50 FOR I=1 TO 10
60 PRINT I;"..";
65 FOR D=1 TO 500
66 NEXT D
70 NEXT I
80 PRINT
90 FOR I=1 TO INT(RND(0)*5000)
100 NEXT I
110 GET K$
115 IF K$<>"" THEN GOTO 160
120 PRINT "HE DRAWS..... ";
130 FOR I=1 TO 100
140 GET K$
145 IF K$<>"" THEN GOTO 190
150 NEXT I
160 PRINT "AND SHOOTS."
170 PRINT "YOU ARE DEAD."
180 GOTO 210
190 PRINT "BUT YOU SHOOT FIRST."
200 PRINT "YOU KILLED HIM."
210 END

