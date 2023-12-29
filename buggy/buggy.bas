100 REM ** SETUP USER PORT **
110 DB=$BFF3 : PB=$BFF1
120 POKE DB,$FF
130 REM ** BRANCH TO SETUP MACHINE CODE **
140 GOTO 700
200 REM ** DISPLAY INSTRUCTIONS **
210 PRINT CHR$(12)
220 PRINT "MOTOR CONTROL"
230 PRINT "-------------"
240 PRINT
250 PRINT "90 DEGREES IS 128 STEPS" : PRINT
300 REM ** CONTROL MOTOR $6000=FWD $601B=BCK **
310 INPUT "ENTER STEPS ";ST
320 FOR C=1 TO ST
330 CALL $6000
340 NEXT C
350 GOTO 300
700 REM ** LOAD MACHINE CODE INTO RAM **
710 RESTORE
720 FOR MC=0 TO 64
730 READ C : POKE $6000+MC,C
740 NEXT MC
750 GOTO 200
800 REM ** MACHINE CODE - BOTH MOTORS FORWARDS **
810 DATA $A9,$11
815 DATA $8D,$F1,$BF
820 DATA $20,$36,$60
825 DATA $4E,$F1,$BF
830 DATA $20,$36,$60
835 DATA $4E,$F1,$BF
840 DATA $20,$36,$60
845 DATA $4E,$F1,$BF
850 DATA $20,$36,$60
855 DATA $60
865 DATA $A9,$88
870 DATA $8D,$F1,$BF
875 DATA $20,$36,$60
880 DATA $0E,$F1,$BF
885 DATA $20,$36,$60
890 DATA $0E,$F1,$BF
895 DATA $20,$36,$60
900 DATA $0E,$F1,$BF
905 DATA $20,$36,$60
910 DATA $60
915 DATA $A2,$05
920 DATA $A0,$60
925 DATA $88
930 DATA $D0,$FD
935 DATA $CA
940 DATA $D0,$F8
945 DATA $60
