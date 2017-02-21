       IDENTIFICATION DIVISION.                                         00010000
       PROGRAM-ID. CIOCCS05.                                            00020033
      ***************************************************************** 00030000
      * CIOCCIMN - CLIENT PROGRAM                                       00040000
      *                                                                 00050000
      * CREDIT ISSUANCE MAIN MENU                                       00060000
      *                                                                 00070000
      ***************************************************************** 00080000
      *                         VERSION HISTORY                         00090000
      *---------------------------------------------------------------- 00100000
      *DATE/TIME ³   AUTHOR   ³ DESCRIPTION                             00110000
      *---------------------------------------------------------------- 00120000
      *2015-02-03    KRIS      INITIAL VERSION                          00130000
      ***************************************************************** 00140000
       ENVIRONMENT DIVISION.                                            00150000
       DATA DIVISION.                                                   00160000
       WORKING-STORAGE SECTION.                                         00170000
      *                                                                 00180000
       77 WS-BEGIN              PIC X(17) VALUE 'CIOCCS05 WS BEGIN'.    00190033
       01 WS-VAR.                                                       00200000
          05 WS-GETTIME         PIC X(20).                              00210000
          05 WS-DATEOUT         PIC X(10).                              00220000
          05 WS-TIMEOUT         PIC X(8).                               00230000
          05 WS-RESP-CODE       PIC S9(8) COMP.                         00240000
          05 WS-MESSAGE         PIC X(40).                              00250000
          05 WS-ENTER-FLAG      PIC X(1).                               00260000
          05 WS-TRANSID         PIC X(4).                               00270000
       01 WS-MAP-OPTION         PIC X(1).                               00280000
          88 WS-MAP-ERASE       VALUE '0'.                              00290000
          88 WS-MAP-DATAONLY    VALUE '1'.                              00300000
      *                                                                 00310000
      *SCREEN HANDLER                                                   00320000
       COPY SD11WS.                                                     00330000
      * SYMBOLIC MAP                                                    00340000
       COPY CICP04.                                                     00350015
      *MAP CONTROL                                                      00360000
       COPY DFHBMSCA.                                                   00370000
      *CICS FUNCTION KEYS                                               00380000
       COPY DFHAID.                                                     00390000
      *CIMENU                                                           00400000
       COPY CIMENU.                                                     00410000
      *                                                                 00420000
       COPY CIC0015I.                                                   00430015
       COPY CIC0015O.                                                   00440015
      *                                                                 00450000
       COPY CIC0014I.                                                   00460015
       COPY CIC0014O.                                                   00470015
      *                                                                 00480000
       01 WS-SRV-COMMAREA.                                              00490000
      *SERVICE REQUEST/RESPONSE COMMAREA                                00500000
       COPY SD01WS.                                                     00510000
      *                                                                 00520000
       01 WS-COMMAREA.                                                  00530000
          05 WS-FIRST-SEND      PIC X(1).                               00540000
          05 WS-OPTION          PIC 9(3).                               00550027
          05 WS-CARD-NUMB       PIC 9(16).                              00560027
          05 WS-CUST-ID-NUMB    PIC 9(18).                              00560132
      *                                                                 00561025
       01 WS-COMMAREA-CS04.                                             00562033
          05 WS-FIRST-SEND      PIC X(1).                               00563025
          05 WS-OPTION          PIC 9(3).                               00564025
          05 WS-CUST-ID-NUMB    PIC 9(18).                              00565030
      *                                                                 00566025
       01 WS-COMMAREA-CS06.                                             00567033
          05 WS-FIRST-SEND      PIC X(1).                               00568033
          05 WS-OPTION          PIC 9(3).                               00569033
          05 WS-CARD-NUMB       PIC 9(16).                              00569142
          05 WS-CUST-ID-NUMB    PIC 9(18).                              00569233
      *                                                                 00569333
       77 WS-END                PIC X(17) VALUE 'CIOCCS05 WS END'.      00570033
      *                                                                 00580000
       LINKAGE SECTION.                                                 00590000
       01 DFHCOMMAREA.                                                  00600000
          05 LK-FIRST-SEND      PIC X(1).                               00610000
          05 LK-OPTION          PIC 9(3).                               00620027
          05 LK-CARD-NUMB       PIC 9(16).                              00630027
      *   05 LK-CUST-ID-NUMB    PIC 9(18).                              00631035
      *                                                                 00640000
       PROCEDURE DIVISION.                                              00650000
       0000-MAINLINE.                                                   00660000
      *                                                                 00670000
            PERFORM 1000-INIT                                           00680000
               THRU 1000-INIT-EXIT                                      00690000
      *                                                                 00700000
            PERFORM 2000-PRE-PROCESSING                                 00710000
               THRU 2000-PRE-PROCESSING-EXIT                            00720000
      *                                                                 00730000
            PERFORM 3000-MAIN-PROCESS                                   00740000
               THRU 3000-MAIN-PROCESS-EXIT                              00750000
      *                                                                 00760000
            PERFORM 4000-POST-PROCESSING                                00770000
               THRU 4000-POST-PROCESSING-EXIT                           00780000
      *                                                                 00790000
            PERFORM 5000-CLEAN-UP                                       00800000
               THRU 5000-CLEAN-UP-EXIT                                  00810000
            .                                                           00820000
      *                                                                 00830000
       0000-EXIT.                                                       00840000
            EXIT.                                                       00850000
      *                                                                 00860000
       1000-INIT.                                                       00870000
            IF LK-FIRST-SEND = 'Y'                                      00880000
               MOVE LOW-VALUES TO CICP04O                               00890015
               PERFORM 1020-GET-EXISTING-CARD                           00900017
                  THRU 1020-GET-EXISTING-CARD-EXIT                      00910017
               SET WS-MAP-ERASE TO TRUE                                 00920000
               PERFORM 3030-SEND-MAP                                    00930000
                  THRU 3030-SEND-MAP-EXIT                               00940000
      * NOT FIRST SHOW                                                  00950000
            ELSE                                                        00960000
                  MOVE LOW-VALUES TO CICP04I                            00970015
                  EXEC CICS RECEIVE MAP('CICP04')                       00980015
                                   MAPSET('CICP04')                     00990015
                                   INTO(CICP04I)                        01000015
                                   RESP(WS-RESP-CODE)                   01010000
                  END-EXEC                                              01020000
            END-IF                                                      01030000
            .                                                           01040000
       1000-INIT-EXIT.                                                  01050000
            EXIT.                                                       01060000
      *                                                                 01070000
       1010-ASK-TIME-DATE.                                              01080000
      *                                                                 01090000
            EXEC CICS                                                   01100000
                 ASKTIME                                                01110000
                 ABSTIME(WS-GETTIME)                                    01120000
            END-EXEC                                                    01130000
            EXEC CICS                                                   01140000
                 FORMATTIME                                             01150000
                 ABSTIME(WS-GETTIME)                                    01160000
                 DATESEP('/')                                           01170000
                 YYYYMMDD(WS-DATEOUT)                                   01180000
            END-EXEC                                                    01190000
            EXEC CICS                                                   01200000
                 FORMATTIME                                             01210000
                 ABSTIME(WS-GETTIME)                                    01220000
                 TIMESEP                                                01230000
                 TIME(WS-TIMEOUT)                                       01240000
            END-EXEC                                                    01250000
            MOVE WS-DATEOUT TO SYSDO                                    01260000
            MOVE WS-TIMEOUT TO SYSTO                                    01270000
            .                                                           01280000
      *                                                                 01290000
       1010-ASK-TIME-DATE-EXIT.                                         01300000
            EXIT.                                                       01310000
      *                                                                 01320000
       1020-GET-EXISTING-CARD.                                          01330017
            INITIALIZE SDCA-SERVICE-COMMAREA                            01340000
            MOVE 'VBS.CI.CREDCARD.INQ' TO SD-SRV-NAME                   01350015
            INITIALIZE CIC0015I-REC                                     01360015
            MOVE LK-CARD-NUMB TO CIC0015I-NUMB                          01370027
            MOVE CIC0015I-REC TO SD-SRV-INPUT-DATA                      01380015
            EXEC CICS                                                   01390000
                 LINK                                                   01400000
                 PROGRAM(WS-PGM-SRV-DRIVER)                             01410000
                 COMMAREA(WS-SRV-COMMAREA)                              01420000
                 RESP(WS-RESP-CODE)                                     01430000
            END-EXEC                                                    01440000
            EVALUATE WS-RESP-CODE                                       01450000
                WHEN DFHRESP(NORMAL)                                    01460000
                     IF SD-RESP-CODE EQUAL ZEROS                        01470000
                        PERFORM 1030-POPULATE-CARD                      01480017
                           THRU 1030-POPULATE-CARD-EXIT                 01490017
                     END-IF                                             01500000
            END-EVALUATE                                                01510000
            .                                                           01520000
      *                                                                 01530000
       1020-GET-EXISTING-CARD-EXIT.                                     01540017
            EXIT.                                                       01550000
      *                                                                 01560000
       1030-POPULATE-CARD.                                              01570017
            INITIALIZE CIC0015O-REC                                     01580015
            MOVE SD-SRV-OUTPUT-DATA       TO CIC0015O-REC               01590015
            MOVE CIC0015O-PROD-TYPE       TO PTYPEO                     01600015
            MOVE CIC0015O-NUMB            TO CANUMO                     01610015
            MOVE CIC0015O-STATUS          TO CASTAO                     01620021
            MOVE CIC0015O-ACCT-NUMB       TO ACNUMO                     01630021
            MOVE CIC0015O-CUST-NUMB       TO CUNUMO                     01640022
            MOVE CIC0015O-LAST-DATE       TO LUDATO                     01650015
            MOVE CIC0015O-OPEN-DATE       TO OPDATO                     01660015
            MOVE CIC0015O-DISP-IND        TO DIINDO                     01670015
            MOVE CIC0015O-DISP-SUBDATE    TO DISUBO                     01680015
            MOVE CIC0015O-DISP-DATE       TO DIDATO                     01690015
            MOVE CIC0015O-TYPE            TO CATYPO                     01700015
            MOVE CIC0015O-PRISEC-IND      TO PSINDO                     01710015
            MOVE CIC0015O-LOCK-IND        TO LOINDO                     01720015
            MOVE CIC0015O-LOCK-DATE       TO LODATO                     01730015
            MOVE CIC0015O-CURR-EXPIRY-DATE TO EXDATO                    01740015
            MOVE CIC0015O-ACTIVATE-IND    TO ACINDO                     01750015
            MOVE CIC0015O-ACTIVATE-DATE   TO AVDATO                     01760015
            MOVE CIC0015O-CURR-ACTION     TO CUACTO                     01770015
            MOVE CIC0015O-LAST-ACTION     TO LAACTO                     01780015
            MOVE CIC0015O-CVV             TO CACVVO                     01790015
            MOVE CIC0015O-CVV2            TO CACVV2O                    01800015
            MOVE CIC0015O-CUST-NAME       TO CUNAMO                     01810015
            MOVE CIC0015O-TRNPWD-IND      TO NAPASO                     01820015
            MOVE CIC0015O-TRNPWD-WRGCNT    TO PWCOUO                    01840015
            MOVE CIC0015O-TRNPWD-WRGDATE   TO PWDATO                    01850015
            MOVE CIC0015O-TRNPWD-WRGTIME   TO PWTIMO                    01860015
            MOVE CIC0015O-TRNPWD-LAST-DATE TO PUDATO                    01870015
            .                                                           01900000
      *                                                                 01910000
       1030-POPULATE-CARD-EXIT.                                         01920017
            EXIT.                                                       01930000
       2000-PRE-PROCESSING.                                             01940000
      *                                                                 01950000
       2000-PRE-PROCESSING-EXIT.                                        01960000
            EXIT.                                                       01970000
      *                                                                 01980000
       3000-MAIN-PROCESS.                                               01990000
            EVALUATE EIBAID ALSO TRUE                                   02000000
                WHEN ANY ALSO LK-OPTION = 005                           02001047
                     EXEC CICS                                          02002047
                          XCTL PROGRAM('CIOCCS00')                      02003047
                               RESP(WS-RESP-CODE)                       02004047
                     END-EXEC                                           02005047
                     IF WS-RESP-CODE NOT = DFHRESP(NORMAL)              02006047
                        MOVE 'PROGRAM CIOCCS00 IS NOT AVAILABLE'        02007047
                                TO MSGO                                 02008047
                        SET WS-MAP-DATAONLY TO TRUE                     02009047
                        PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT   02009147
                     END-IF                                             02009247
                WHEN DFHPF1 ALSO ANY                                    02010000
                     EXEC CICS                                          02020000
                          XCTL PROGRAM('CIOCCS04')                      02030033
                               RESP(WS-RESP-CODE)                       02040000
                     END-EXEC                                           02050000
                     IF WS-RESP-CODE NOT = DFHRESP(NORMAL)              02060000
                        MOVE 'PROGRAM CIOCCS04 IS NOT AVAILABLE'        02070042
                                TO MSGO                                 02080000
                        SET WS-MAP-DATAONLY TO TRUE                     02090000
                        PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT   02100000
                     END-IF                                             02110000
                WHEN DFHENTER ALSO LK-OPTION = 001                      02111042
                     MOVE 'Y' TO WS-FIRST-SEND OF WS-COMMAREA-CS06      02112042
                     MOVE 001 TO WS-OPTION OF WS-COMMAREA-CS06          02113046
                     MOVE LK-CARD-NUMB TO WS-CARD-NUMB OF               02113142
                          WS-COMMAREA-CS06                              02113242
                     MOVE CUNUMI TO WS-CUST-ID-NUMB OF                  02114042
                          WS-COMMAREA-CS06                              02115042
                     EXEC CICS                                          02116033
                          XCTL PROGRAM('CIOCCS06')                      02117033
                           COMMAREA(WS-COMMAREA-CS06)                   02118033
                               RESP(WS-RESP-CODE)                       02119033
                     END-EXEC                                           02119133
                     IF WS-RESP-CODE NOT = DFHRESP(NORMAL)              02119233
                        MOVE 'PROGRAM CIOCCIMN IS NOT AVAILABLE'        02119333
                                TO MSGO                                 02119433
                        SET WS-MAP-DATAONLY TO TRUE                     02119533
                        PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT   02119633
                     END-IF                                             02119733
                WHEN DFHENTER ALSO LK-OPTION = 002                      02119845
                     MOVE 'Y' TO WS-FIRST-SEND OF WS-COMMAREA-CS06      02119945
                     MOVE 001 TO WS-OPTION OF WS-COMMAREA-CS06          02120046
                     MOVE LK-CARD-NUMB TO WS-CARD-NUMB OF               02120145
                          WS-COMMAREA-CS06                              02120245
                     MOVE CUNUMI TO WS-CUST-ID-NUMB OF                  02120345
                          WS-COMMAREA-CS06                              02120445
                     EXEC CICS                                          02120545
                          XCTL PROGRAM('CIOCCS08')                      02120645
                           COMMAREA(WS-COMMAREA-CS06)                   02120745
                               RESP(WS-RESP-CODE)                       02120845
                     END-EXEC                                           02120945
                     IF WS-RESP-CODE NOT = DFHRESP(NORMAL)              02121045
                        MOVE 'PROGRAM CIOCCIMN IS NOT AVAILABLE'        02121145
                                TO MSGO                                 02121245
                        SET WS-MAP-DATAONLY TO TRUE                     02121345
                        PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT   02121445
                     END-IF                                             02121545
                WHEN DFHENTER ALSO LK-OPTION = 003                      02121646
                     MOVE 'Y' TO WS-FIRST-SEND OF WS-COMMAREA-CS06      02121746
                     MOVE 001 TO WS-OPTION OF WS-COMMAREA-CS06          02121846
                     MOVE LK-CARD-NUMB TO WS-CARD-NUMB OF               02121946
                          WS-COMMAREA-CS06                              02122046
                     MOVE CUNUMI TO WS-CUST-ID-NUMB OF                  02122146
                          WS-COMMAREA-CS06                              02122246
                     EXEC CICS                                          02122346
                          XCTL PROGRAM('CIOCCS09')                      02122446
                           COMMAREA(WS-COMMAREA-CS06)                   02122546
                               RESP(WS-RESP-CODE)                       02122646
                     END-EXEC                                           02122746
                     IF WS-RESP-CODE NOT = DFHRESP(NORMAL)              02122846
                        MOVE 'PROGRAM CIOCCIMN IS NOT AVAILABLE'        02122946
                                TO MSGO                                 02123046
                        SET WS-MAP-DATAONLY TO TRUE                     02123146
                        PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT   02123246
                     END-IF                                             02123346
                WHEN DFHENTER ALSO LK-OPTION = 004                      02123447
                     PERFORM 1020-GET-EXISTING-CARD                     02123548
                        THRU 1020-GET-EXISTING-CARD-EXIT                02123648
                     INITIALIZE SDCA-SERVICE-COMMAREA                   02123747
                     MOVE 'VBS.CI.CREDCARD.UPD' TO SD-SRV-NAME          02123847
                     INITIALIZE CIC0014I-REC                            02123947
                     MOVE CIC0015O-REC TO CIC0014I-REC                  02124047
                     MOVE 002 TO CIC0014I-STATUS                        02124149
                     MOVE 01 TO CIC0014I-ACTIVATE-IND                   02124247
                          PERFORM 1010-ASK-TIME-DATE                    02125047
                          THRU 1010-ASK-TIME-DATE-EXIT                  02125147
                     MOVE WS-DATEOUT TO CIC0014I-ACTIVATE-DATE          02125247
                     MOVE CIC0014I-REC  TO SD-SRV-INPUT-DATA            02125347
                     EXEC CICS                                          02125447
                          LINK                                          02125547
                          PROGRAM(WS-PGM-SRV-DRIVER)                    02125647
                          COMMAREA(WS-SRV-COMMAREA)                     02125747
                          RESP(WS-RESP-CODE)                            02125847
                     END-EXEC                                           02125947
                     EVALUATE WS-RESP-CODE                              02126047
                         WHEN DFHRESP(NORMAL)                           02126147
                              IF SD-RESP-CODE EQUAL ZEROS               02126247
                                MOVE 'CARD ACTIVATED SUCCESSFULLY'      02126347
                                   TO MSGO                              02126447
                                 MOVE 005 TO LK-OPTION                  02126547
                              ELSE                                      02126647
                                 MOVE SD-RESP-ADDITIONAL TO MSGO        02126747
                              END-IF                                    02126847
                         WHEN OTHER                                     02126947
                              MOVE 'SERVICE PROGRAM ERROR' TO MSGO      02127047
                     END-EVALUATE                                       02127147
                     PERFORM 1020-GET-EXISTING-CARD                     02127249
                        THRU 1020-GET-EXISTING-CARD-EXIT                02127349
                     SET WS-MAP-DATAONLY TO TRUE                        02127447
                     PERFORM 3030-SEND-MAP                              02127547
                        THRU 3030-SEND-MAP-EXIT                         02127647
                WHEN DFHPF3 ALSO ANY                                    02128047
                     MOVE 'THANK YOU FOR USING THE SYSTEM' TO WS-MESSAGE02130000
                     EXEC CICS                                          02140000
                          SEND CONTROL                                  02150000
                          CURSOR                                        02160000
                          ERASE                                         02170000
                          FREEKB                                        02180000
                          ALARM                                         02190000
                     END-EXEC                                           02200000
                     EXEC CICS                                          02210000
                          SEND FROM(WS-MESSAGE)                         02220000
                     END-EXEC                                           02230000
                     PERFORM 5010-RETURN THRU 5010-RETURN-EXIT          02240000
                WHEN DFHCLEAR ALSO ANY                                  02250000
                     EXEC CICS                                          02260000
                           SEND CONTROL                                 02270000
                           CURSOR                                       02280000
                           ERASE                                        02290000
                           FREEKB                                       02300000
                           ALARM                                        02310000
                     END-EXEC                                           02320000
                     PERFORM 5010-RETURN THRU 5010-RETURN-EXIT          02330000
      *          WHEN DFHENTER ALSO LK-OPTION = 002                     02550041
      *               IF WS-RESP-CODE NOT EQUAL DFHRESP(NORMAL)         02560041
      *                  MOVE 'INVALID REQUEST, CHECK YOUR INPUT.'      02570041
      *                       TO MSGO                                   02580041
      *                  SET WS-MAP-DATAONLY TO TRUE                    02590041
      *                  PERFORM 3030-SEND-MAP                          02600041
      *                     THRU 3030-SEND-MAP-EXIT                     02610041
      *               ELSE                                              02620041
      *                  PERFORM 3010-CHECK-INPUT                       02630041
      *                     THRU 3010-CHECK-INPUT-EXIT                  02640041
      *               END-IF                                            02670043
                 WHEN OTHER                                             02680000
                      MOVE 'INVALID KEY PRESSED!' TO MSGO               02690000
                      SET WS-MAP-DATAONLY TO TRUE                       02700000
                      PERFORM 3030-SEND-MAP                             02710000
                         THRU 3030-SEND-MAP-EXIT                        02720000
            END-EVALUATE                                                02730000
            .                                                           02740000
       3000-MAIN-PROCESS-EXIT.                                          02750000
            EXIT.                                                       02760000
      *                                                                 02770000
       3010-CHECK-INPUT.                                                02780000
            IF COMMUL NOT = 0                                           02790000
               INITIALIZE CIMENU-REC                                    02800000
               MOVE COMMUI TO CIMENU-TRANSID                            02810000
               EXEC CICS READ                                           02820000
                    FILE('CIMENU')                                      02830000
                    INTO(CIMENU-REC)                                    02840000
                    RIDFLD(CIMENU-TRANSID)                              02850000
                    RESP(WS-RESP-CODE)                                  02860000
               END-EXEC                                                 02870000
               EVALUATE WS-RESP-CODE                                    02880000
                   WHEN DFHRESP(NORMAL)                                 02890000
                        EXEC CICS                                       02900000
                             XCTL PROGRAM(CIMENU-PGM)                   02910000
                             COMMAREA(WS-COMMAREA)                      02920000
                             RESP(WS-RESP-CODE)                         02930000
                        END-EXEC                                        02940000
                        IF WS-RESP-CODE NOT = DFHRESP(NORMAL)           02950000
                        STRING 'PROGRAM ' DELIMITED BY SIZE             02960000
                               CIMENU-PGM DELIMITED BY SPACE            02970000
                               ' IS NOT AVAILABLE' DELIMITED BY SIZE    02980000
                               INTO MSGO                                02990000
                           SET WS-MAP-DATAONLY TO TRUE                  03000000
                           PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT03010000
                        END-IF                                          03020000
                   WHEN DFHRESP(NOTFND)                                 03030000
                        MOVE 'INVALID TRANSATION ID!' TO MSGO           03040000
                        SET WS-MAP-DATAONLY TO TRUE                     03050000
                        PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT   03060000
                   WHEN OTHER                                           03070000
                        MOVE 'CIMENU FILE ERROR!' TO MSGO               03080000
                        SET WS-MAP-DATAONLY TO TRUE                     03090000
                        PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT   03100000
               END-EVALUATE                                             03110000
            END-IF                                                      03120000
            .                                                           03536039
      *                                                                 03540000
       3010-CHECK-INPUT-EXIT.                                           03550000
            EXIT.                                                       03560000
      *                                                                 03570000
      *                                                                 03890000
       3030-SEND-MAP.                                                   03900000
            PERFORM 3040-SET-PROTECT                                    03901035
                  THRU 3040-SET-PROTECT-EXIT                            03902035
            EVALUATE LK-OPTION                                          03903035
                WHEN 001                                                03903135
                     MOVE 'PF1=RETURN PF3=EXIT ENTER=CUSTOMER INFORMATIO03903235
      -                   'N' TO KEYO                                   03903339
                WHEN 002                                                03903435
                     MOVE 'PF1=RETURN PF3=EXIT ENTER=SET TRANSACTION PAS03903535
      -                   'SWORD' TO KEYO                               03903638
                WHEN 003                                                03903735
                     MOVE 'PF1=RETURN PF3=EXIT ENTER=SET PASSWORD NEED' 03903835
                           TO KEYO                                      03903935
                WHEN 004                                                03904035
                     MOVE 'PRESS ENTER TO CONFIRM ACTIVATION' TO MSGO   03904147
                     MOVE 'PF1=RETURN PF3=EXIT ENTER=CARD ACTIVATION' TO03904235
                           KEYO                                         03904335
                WHEN 005                                                03904435
                     MOVE 'PRESS ANY KEY TO RETURN' TO KEYO             03904535
                WHEN OTHER                                              03904635
                     CONTINUE                                           03904735
            END-EVALUATE                                                03904835
            PERFORM 1010-ASK-TIME-DATE                                  04040000
               THRU 1010-ASK-TIME-DATE-EXIT                             04050000
            EVALUATE TRUE                                               04060000
                WHEN WS-MAP-ERASE                                       04070000
                     EXEC CICS SEND                                     04080000
                          MAP('CICP04')                                 04090015
                          MAPSET('CICP04')                              04100015
                          FROM(CICP04O)                                 04110015
                          ERASE                                         04120000
                     END-EXEC                                           04130000
                WHEN WS-MAP-DATAONLY                                    04140000
                     EXEC CICS SEND                                     04150000
                          MAP('CICP04')                                 04160015
                          MAPSET('CICP04')                              04170015
                          FROM(CICP04O)                                 04180015
                          DATAONLY                                      04190000
                     END-EXEC                                           04200000
            END-EVALUATE                                                04210000
      *     MOVE '1' TO WS-ENTER-FLAG                                   04220000
            PERFORM 5020-RETURN-TRANS THRU 5020-RETURN-TRANS-EXIT       04230000
            .                                                           04240000
      *                                                                 04250000
       3030-SEND-MAP-EXIT.                                              04260000
            EXIT.                                                       04270000
      *                                                                 04280000
       3040-SET-PROTECT.                                                04290000
            MOVE ATTR-PROT-MDT          TO PTYPEA                       04300015
            MOVE ATTR-PROT-MDT          TO CANUMA                       04310015
            MOVE ATTR-PROT-MDT          TO CUNUMA                       04320015
            MOVE ATTR-PROT-MDT          TO CASTAA                       04330015
            MOVE ATTR-PROT-MDT          TO ACNUMA                       04340015
            MOVE ATTR-PROT-MDT          TO LUDATA                       04350015
            MOVE ATTR-PROT-MDT          TO OPDATA                       04360015
            MOVE ATTR-PROT-MDT          TO DIINDA                       04370015
            MOVE ATTR-PROT-MDT          TO DISUBA                       04380015
            MOVE ATTR-PROT-MDT          TO DIDATA                       04390015
            MOVE ATTR-PROT-MDT          TO CATYPA                       04400015
            MOVE ATTR-PROT-MDT          TO PSINDA                       04410015
            MOVE ATTR-PROT-MDT          TO LOINDA                       04420015
            MOVE ATTR-PROT-MDT          TO LODATA                       04430015
            MOVE ATTR-PROT-MDT          TO EXDATA                       04440015
            MOVE ATTR-PROT-MDT          TO ACINDA                       04450015
            MOVE ATTR-PROT-MDT          TO AVDATA                       04460015
            MOVE ATTR-PROT-MDT          TO CUACTA                       04470015
            MOVE ATTR-PROT-MDT          TO LAACTA                       04480015
            MOVE ATTR-PROT-MDT          TO CACVVA                       04490015
            MOVE ATTR-PROT-MDT          TO CACVV2A                      04500015
            MOVE ATTR-PROT-MDT          TO CUNAMA                       04510015
            MOVE ATTR-PROT-MDT          TO NAPASA                       04520015
            MOVE ATTR-PROT-MDT          TO PWCOUA                       04530015
            MOVE ATTR-PROT-MDT          TO PWDATA                       04540015
            MOVE ATTR-PROT-MDT          TO PWTIMA                       04550015
            MOVE ATTR-PROT-MDT          TO PUDATA                       04560015
            .                                                           04600000
       3040-SET-PROTECT-EXIT.                                           04610000
            EXIT.                                                       04620000
       4000-POST-PROCESSING.                                            05280000
      *                                                                 05290000
       4000-POST-PROCESSING-EXIT.                                       05300000
            EXIT.                                                       05310000
      *                                                                 05320000
       5000-CLEAN-UP.                                                   05330000
            PERFORM 5010-RETURN                                         05340000
               THRU 5010-RETURN-EXIT                                    05350000
            .                                                           05360000
      *                                                                 05370000
       5000-CLEAN-UP-EXIT.                                              05380000
            EXIT.                                                       05390000
      *                                                                 05400000
       5010-RETURN.                                                     05410000
            EXEC CICS RETURN END-EXEC                                   05420000
            .                                                           05430000
       5010-RETURN-EXIT.                                                05440000
            EXIT.                                                       05450000
      *                                                                 05460000
       5020-RETURN-TRANS.                                               05470000
            INITIALIZE WS-COMMAREA                                      05480000
            MOVE 'N' TO WS-FIRST-SEND OF WS-COMMAREA                    05490026
            MOVE LK-OPTION TO WS-OPTION OF WS-COMMAREA                  05500027
            MOVE LK-CARD-NUMB TO WS-CARD-NUMB OF WS-COMMAREA            05510027
      *     MOVE LK-CUST-ID-NUMB TO WS-CUST-ID-NUMB OF WS-COMMAREA      05511039
            EXEC CICS RETURN TRANSID('CIB5')                            05520033
                      COMMAREA(WS-COMMAREA)                             05530000
            END-EXEC                                                    05540000
            .                                                           05550000
       5020-RETURN-TRANS-EXIT.                                          05560000
            EXIT.                                                       05570000
