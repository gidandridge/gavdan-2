
5  ? TAB(15);"***************************"
10 ? TAB(26);"Eliza"
15 ? TAB(20);"Creative Computing"
20 ? TAB(18);"Morristown, New Jersey"
25 ?
30 ? TAB(18);"Adapted for EhBASIC by"
35 ? TAB(23);"Lee Davison"
40 ? TAB(16);"(leeedavison@lycos.co.uk)"
45 ?
50 ? TAB(17);"Please don't use commas"
55 ? TAB(16);"or periods in your inputs"
60 ?
65 ? TAB(15);"***************************"
70 ?
75 ?
80 REM ********************
85 REM ** INITIALIZATION **
90 REM ********************
100 DIM S(36),R(36),N(36)
105 DIM kw$(36),win$(7),wout$(7),rp$(112)
110 N1=36:N2=14:N3=112:P$="":C$=P$
112 FOR X=1 TO N1:READ kw$(X):NEXT X
114 FOR X=1 TO N2/2:READ win$(X):READ wout$(X): NEXT X
116 FOR X=1 TO N3:READ rp$(X):NEXT X
130 FOR X=1 TO N1
140 READ S(X),L:R(X)=S(X):N(X)=S(X)+L-1
150 NEXT X

160 ? "Hi! I'm Eliza. what's your problem?"

170 REM ************************
180 REM ** USER INPUT SECTION **
190 REM ************************
200 INPUT I$
205 I$="  "+LCASE$(I$)+"  "
210 REM CHECK FOR 'SHUT UP'
240 IF 9>LEN(I$) THEN 250
241 IF MID$(I$,3,7)<>"shut up" THEN 250
245 ? "Ok. If you feel that way I'll shut up....":END

250 IF I$=P$ THEN ? "please don't repeat yourself!":GOTO 200

260 REM ************************
270 REM ** FIND KEYWORD IN I$ **
280 REM ************************
290 K=0:L=0
295 il=LEN(I$)
300 FOR kk=1 TO N1
305 kl=LEN(kw$(kk))
310 FOR ll=1 TO il-kl+1
315 IF MID$(I$,ll,kl)<>kw$(kk) THEN 350
320 IF kk<>13 THEN 330
325 IF MID$(I$,ll,LEN(kw$(29)))=kw$(29) THEN kk=29
330 F$=kw$(kk)
340 K=kk : L=ll : ll=il-kl+1 : kk=N1 : REM finish loops
350 NEXT ll
360 NEXT kk
365 IF K THEN 430:REM WE DID FIND A KEYWORD
370 K=36:GOTO 570:REM WE DIDN'T FIND ANY KEYWORDS

380 REM *******************************************
390 REM ** TAKE PART OF STRING AND CONJUGATE IT ***
400 REM * USING THE LIST OF STRINGS TO BE SWAPPED *
410 REM *******************************************
430 C$=" "+RIGHT$(I$,LEN(I$)-LEN(F$)-L+1)+" "
440 FOR X=1 TO N2/2
460 FOR L=1 TO LEN(C$)
470 IF L+LEN(win$(X))>LEN(C$) THEN 510
480 IF MID$(C$,L,LEN(win$(X)))<>win$(X) THEN 510
490 C$=LEFT$(C$,L-1)+wout$(X)+RIGHT$(C$,LEN(C$)-L-LEN(win$(X))+1)
495 L=L+LEN(wout$(X))
500 GOTO 540

510 IF L+LEN(wout$(X))>LEN(C$)THEN 540
520 IF MID$(C$,L,LEN(wout$(X)))<>wout$(X) THEN 540
530 C$=LEFT$(C$,L-1)+win$(X)+RIGHT$(C$,LEN(C$)-L-LEN(wout$(X))+1)
535 L=L+LEN(win$(X))
540 NEXT L
550 NEXT X
555 IF MID$(C$,2,1)=" "THEN C$=RIGHT$(C$,LEN(C$)-1):REM ONLY 1 SPACE
556 FOR L=1 TO LEN(C$)
557 IF MID$(C$,L,1)="!" THEN C$=LEFT$(C$,L-1)+RIGHT$(C$,LEN(C$)-L):GOTO 557
558 NEXT L

560 REM ************************************************
570 REM ** NOW USING THE KEYWORD NUMBER (K) GET REPLY **
580 REM ************************************************
600 F$=rp$(R(K))
610 R(K)=R(K)+1:IF R(K)>N(K) THEN R(K)=S(K)
620 IF RIGHT$(F$,1)<>"*" THEN ? F$:P$=I$:GOTO 200
625 IF C$<>"   " THEN 630
626 ? "you will have to elaborate more for me to help you"
627 GOTO 200

630 ? LEFT$(F$,LEN(F$)-1);C$
640 P$=I$:GOTO 200

1000 REM **************************
1010 REM ** PROGRAM DATA FOLLOWS **
1020 REM **************************
1030 REM ****** KEYWORDS **********
1049 REM **************************
1050 DATA "can you ","can i ","you are ","you're ","i don't ","i feel "
1060 DATA "why don't you ","why can't i ","are you ","i can't ","i am ","i'm "
1070 DATA "you ","i want ","what ","how ","who ","where ","when ","why "
1080 DATA "name ","cause ","sorry ","dream ","hello ","hi ","maybe "
1090 DATA "no","your ","always ","think ","alike ","yes ","friend "
1100 DATA "computer", "nokeyfound"
1200 REM **********************************
1210 REM ** STRING DATA FOR CONJUGATIONS **
1220 REM **********************************
1230 DATA " are "," am "," were "," was "," you "," I "," your"," my "
1235 DATA " I've "," you've "," I'm "," you're "
1240 DATA " me "," you "
1300 REM *************
1310 REM ** REPLIES **
1320 REM *************
1330 DATA "Don't you believe that I can*"
1340 DATA "Perhaps you would like to be like me*"
1350 DATA "You want me to be able to*"
1360 DATA "Perhaps you don't want to*"
1365 DATA "Do you want to be able to*"
1370 DATA "What makes you think I am*"
1380 DATA "Does it please you to believe I am*"
1390 DATA "Perhaps you would like to be*"
1400 DATA "Do you sometimes wish you were*"
1410 DATA "Don't you really*"
1420 DATA "Why don't you*"
1430 DATA "Do you wish to be able to*"
1440 DATA "Does that trouble you*"
1450 DATA "Do you often feel*"
1460 DATA "Do you often feel*"
1470 DATA "Do you enjoy feeling*"
1480 DATA "Do you really believe I don't*"
1490 DATA "Perhaps in good time I will*"
1500 DATA "Do you want me to*"
1510 DATA "Do you think you should be able to*"
1520 DATA "Why can't you*"
1530 DATA "Why are you interested in whether or not I am*"
1540 DATA "Would you prefer if I were not*"
1550 DATA "Perhaps in your fantasies I am*"
1560 DATA "How do you know you can't*"
1570 DATA "have you tried?"
1580 DATA "Perhaps you can now*"
1590 DATA "Did you come to me because you are*"
1600 DATA "How long have you been*"
1610 DATA "Do you believe it is normal to be*"
1620 DATA "Do you enjoy being*"
1630 DATA "We were discussing you, not me."
1640 DATA "Oh, I*"
1650 DATA "you're not really talking about me, are you?"
1660 DATA "what would it mean to you if you got*"
1670 DATA "Why do you want*"
1680 DATA "Suppose you soon got*"
1690 DATA "What if you never got*"
1700 DATA "I sometimes also want*"
1710 DATA "Why do you ask?"
1720 DATA "Does that question interest you?"
1730 DATA "What answer would please you the most?"
1740 DATA "What do you think?"
1750 DATA "Are such questions on your mind often?"
1760 DATA "What is it that you really want to know?"
1770 DATA "Have you asked anyone else?"
1780 DATA "Have you asked such questions before?"
1790 DATA "What else comes to mind when you ask that?"
1800 DATA "Names don't interest me."
1810 DATA "I don't care about names, please go on."
1820 DATA "Is that the real reason?"
1830 DATA "Don't any other reasons come to mind?"
1840 DATA "Does that reason explain anything else?"
1850 DATA "What other reasons might There be?"
1860 DATA "Please don't apologise!"
1870 DATA "Apologies are not necessary."
1880 DATA "What feelings do you have when you apologise?"
1890 DATA "Don't be so defensive!"
1900 DATA "what does that dream suggest to you?"
1910 DATA "Do you dream often?"
1920 DATA "What persons appear in your dreams?"
1930 DATA "Are you disturbed by your dreams?"
1940 DATA "How do you do ...please state your problem."
1950 DATA "You don't seem quite certain."
1960 DATA "Why the uncertain tone?"
1970 DATA "Can't you be more positive?"
1980 DATA "You aren't sure?"
1990 DATA "Don't you know?"
2000 DATA "Are you saying no just to be negative?"
2010 DATA "You are being a bit negative."
2020 DATA "Why not?"
2030 DATA "Are you sure?"
2040 DATA "Why no?"
2050 DATA "Why are you concerned about my*"
2060 DATA "What about your own*"
2070 DATA "Can you think of a specific example?"
2080 DATA "When?"
2090 DATA "What are you thinking of?"
2100 DATA "Really, always?"
2110 DATA "Do you really think so?"
2120 DATA "But you are not sure you*"
2130 DATA "Do you doubt you*"
2140 DATA "In what way?"
2150 DATA "What resemblance do you see?"
2160 DATA "What does the similarity suggest to you?"
2170 DATA "What other connections do you see?"
2180 DATA "Could there really be some connection?"
2190 DATA "How?"
2200 DATA "You seem quite positive."
2210 DATA "Are you sure?"
2220 DATA "I see."
2230 DATA "I understand."
2240 DATA "Why do you bring up the topic of friends?"
2250 DATA "Do your friends worry you?"
2260 DATA "Do your friends pick on you?"
2270 DATA "Are you sure you have any friends?"
2280 DATA "Do you impose on your friends?"
2290 DATA "Perhaps your love for friends worries you."
2300 DATA "Do computers worry you?"
2310 DATA "Are you talking about me in particular?"
2320 DATA "Are you frightened by machines?"
2330 DATA "Why do you mention computers?"
2340 DATA "What do you think machines have to do with your problem?"
2350 DATA "Don't you think computers Can help people?"
2360 DATA "What is it about machines that worries you?"
2370 DATA "Say, do you have any psychological problems?"
2380 DATA "What does that suggest to you?"
2390 DATA "I see."
2400 DATA "I'm not sure I understand you fully."
2410 DATA "Come come elucidate your thoughts."
2420 DATA "Can you elaborate on that?"
2430 DATA "That is quite interesting."
2500 REM ************************************
2510 REM ** DATA FOR FINDING RIGHT REPLIES **
2520 REM ************************************
2530 DATA 1,3,4,2,6,4,6,4,10,4,14,3,17,3,20,2,22,3,25,3
2540 DATA 28,4,28,4,32,3,35,5,40,9,40,9,40,9,40,9,40,9,40,9
2550 DATA 49,2,51,4,55,4,59,4,63,1,63,1,64,5,69,5,74,2,76,4
2560 DATA 80,3,83,7,90,3,93,6,99,7,106,6