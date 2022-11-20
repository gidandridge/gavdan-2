10 REM *** Tunnel Trek ***
15 REM Adapted from "Dynamic games for your Electron" book by Neal Cavalier-Smith
16 REM I take no resposiblility for the structure of this code! G.Dandridge.
30 GOSUB 1000 : REM PROCsetup
40 DO
50 GOSUB 2000 : REM PROCstartgame
60 LOOP UNTIL A$="N"
70 STOP
1000 REM PROCsetup
1005 A$="Y"
1020 PRINT "***TUNNEL TREK***"
1030 FIGHTS=0:GOLD=30:POS=1
1040 DIM D$(10)
1050 PRINT : PRINT : PRINT "You are at the start of a system of"
1060 PRINT "tunnels and caves. You have THIRTY"
1070 PRINT" pices of gold."
1080 PRINT : PRINT "When you reach the end of the tunnels"
1090 PRINT "you must have at least TWENTY peices of"
1100 PRINT "gold to pay the toll colelctor."
1110 PRINT : PRINT : PRINT"PRESS ANY KEY"
1120 DO : GET K$ : LOOP UNTIL K$<>""
1130 DATA GIANT RAT,ZOMBIE,WEREWOLF,AN AUSTRIALIAN CABBAGE MONSTER
1140 DATA TROLL,POISNONED SPEAR,FLAMING SWORD,BOW AND ARROW
1150 DATA EGG,CROSSBOW
1160 FOR COUNT=1 TO 10
1170 READ D$(COUNT)
1180 NEXT COUNT
1190 RETURN
2000 REM PROCstartgame
2010 IF POS<1 THEN POS=INT(RND(0)*7)+1
2020 PRINT : PRINT "THIS IS A TUNNEL MAZE"
2030 IF POS>9 THEN GOSUB 4000 : REM PROCexit
2040 PRINT : PRINT "THIS IS CAVE NUMBER ";POS
2050 PRINT "CAVE NUMBER 10 IS THE EXIT"
2060 FIGHTS=FIGHTS+1
2070 PRINT "THIS IS CHALLENGE NUMBER ";FIGHTS
2080 IF GOLD<1 THEN LET GOLD=3
2090 TUN=INT(RND(0)*4)+2
2100 PRINT : PRINT "YOU ARE NOW FACING ";TUN"; TUNNELS"
2110 INPUT "WHICH ONE WILL YOU TRY ";A
2120 IF RND(0)<.1 THEN GOSUB 3000 : REM PROCchoose
2130 IF A=tun THEN GOSUB 3000 : REM PROCchoose
2140 E$=D$(INT(RND(0)*4)+1)
2150 PRINT "THE TUNNEL IS BLOCKED BY A ";E$
2160 PRINT "ARMED WITH A ";D$(INT(RND(0)*4)+6)
2170 PRINT : PRINT "****WEAPONS ARE>>****"
2180 PRINT "(1) A pointed stick"
2190 PRINT "(2) Bare hands"
2200 PRINT "(3) A small rock"
2210 PRINT "(4) A nailed club"
2220 PRINT : INPUT "WHICH WEAPON DO YOU CHOOSE ";B
2230 IF B=4 THEN B=3
2240 C=INT(RND(0)*2)+1
2250 IF B=C THEN GOSUB 6000 ELSE GOSUB 5000
2260 RETURN
3000 REM PROCchoose
3010 K=INT(RND(0)*3)+1
3020 ON K GOSUB 8000,8200,8400,8600
3030 RETURN
4000 REM PROCexit
4010 IF POS<>10 THEN RETURN
4020 PRINT : PRINT : PRINT "YOU ARE AT THE EXIT."
4030 PRINT "DO YOU HAVE ENOUGH GOLD TO PAY THE TOLL?"
4040 PRINT "PRESS A KEY TO FIND OUT"
4050 DO : GET K$ : LOOP UNTIL K$<>""
4060 IF GOLD>19 THEN GOTO 4100
4070 PRINT : PRINT "NOPE, THE GUARD HAS KILLED YOU."
4080 PRINT "YOU COULD NOT PAY THE TOLL."
4090 GOSUB 7000
4095 GOTO 4120
4100 PRINT "CONGRATULATIONS, YOU CAN PAY THE TOLL"
4110 PRINT "AND MAY KEEP THE REMAINING ";GOLD-20;" GOLD PIECES."
4120 PRINT : INPUT "WOULD YOU LIKE ANOTHER GAME (Y/N)";A$
4130 IF A$="N" OR A$="n" THEN STOP ELSE RUN
4140 RETURN
5000 REM PROCtrashed
5010 GOSUB 7000 : REM RROCmarch
5020 PRINT : PRINT "THE ";E$;" BEAT YOU AND LEFT YOU ";
5030 GOLD=GOLD-INT(RND(0)*3)
5040 IF GOLD<0 THEN GOLD=0
5050 PRINT "WITH ";GOLD;" GOLD PIECES"
5060 POS=(INT(RND(0)*7)+1)-5+POS
5070 IF POS<1 THEN LET POS=1
5080 PRINT "AND SENT YOU TO ";POS
5090 RETURN
6000 REM PROCwin
6010 PRINT : PRINT "YOU HAVE FOUGHT YOUR WAY PAST THE ";E$
6020 GOLD=GOLD+INT(RND(0)*2)+1
6030 POS=POS+INT(RND(0)*2)+1
6040 PRINT "AND HAVE ";GOLD;" GOLD PIECES"
6050 PRINT : PRINT "YOU ARE APPROACHING CAVE ";POS
6060 RETURN
7000 REM PROCmarch
7010 PRINT CHR$(7)
7020 RETURN
8000 REM SUB1
8010 PRINT : PRINT "YOU HAVE FALLEN THROUGH A TRAP DOOR"
8020 POS=POS-1
8030 GOLD=GOLD-(INT(RND(0)*1)+1)-2
8040 RETURN
8200 REM SUB2
8210 PRINT : PRINT "A FLOOD WASHES YOU DOWN A SIDE TUNNEL"
8220 POS=POS+1
8230 GOLD=GOLD-(INT(RND(0)*2)+1)-2
8240 RETURN
8400 REM SUB3
8410 PRINT : PRINT "YOU HAVE BEEN HELPED BY A PASSING GNOME"
8420 GOLD=GOLD+INT(RND(0)*4)+1
8430 POS=POS+INT(RND(0)*2)+1
8440 RETRUN
8600 REM SUB4
8610 PRINT : PRINT "WHOPEE!!! A HORD OF GOLD"
8620 PRINT "YOU MAY TAKE UP TO FIVE PIECES, BUT BE"
8630 PRINT "CAREFULL! THE MORE YOU TAKE THE MORE IT"
8640 PRINT "COST YOU ...."
8650 PRINT "HOW MANY WOULD YOU LIKE TO TAKE ",D
8660 IF D>5 THEN GOTO 8610
8670 GOLD=GOLD+D
8680 POS=POS-INT(D/2)
8690 RETURN