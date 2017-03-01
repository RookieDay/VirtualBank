       IDENTIFICATION DIVISION.                                         00010000
       PROGRAM-ID. CIOCCS07.                                            00020000
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
       77 WS-BEGIN              PIC X(17) VALUE 'CIOCCS07 WS BEGIN'.    00190000
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
       COPY CICS07.                                                     00350000
      *MAP CONTROL                                                      00360000
       COPY DFHBMSCA.                                                   00370000
      *CICS FUNCTION KEYS                                               00380000
       COPY DFHAID.                                                     00390000
      *CIMENU                                                           00400000
       COPY CIMENU.                                                     00410000
      *                                                                 00420000
       COPY CIC0002I.                                                   00430003
       COPY CIC0002O.                                                   00440003
      *                                                                 00441009
       COPY CIC0003I.                                                   00450009
       COPY CIC0003O.                                                   00460009
      *                                                                 00480000
       01 WS-SRV-COMMAREA.                                              00490000
      *SERVICE REQUEST/RESPONSE COMMAREA                                00500000
       COPY SD01WS.                                                     00510000
      *                                                                 00520000
       01 WS-COMMAREA.                                                  00530000
          05 WS-FIRST-SEND      PIC X(1).                               00540000
          05 WS-OPTION          PIC 9(3).                               00550000
          05 WS-NUMB            PIC 9(16).                              00560000
          05 WS-CUST-ID         PIC 9(18).                              00570000
      *                                                                 00580000
       01 WS-COMMAREA-CS06.                                             00590000
          05 WS-FIRST-SEND      PIC X(1).                               00600000
          05 WS-OPTION          PIC 9(3).                               00610000
          05 WS-NUMB            PIC 9(16).                              00620000
          05 WS-CUST-ID         PIC 9(18).                              00630000
      *                                                                 00640000
       01 WS-COMMAREA-CS08.                                             00650005
          05 WS-FIRST-SEND      PIC X(1).                               00660000
          05 WS-OPTION          PIC 9(3).                               00670000
          05 WS-NUMB            PIC 9(16).                              00680000
          05 WS-CUST-ID         PIC 9(18).                              00690000
       01 WS-COMMAREA-CS05.                                             00691018
          05 WS-FIRST-SEND      PIC X(1).                               00692018
          05 WS-OPTION          PIC 9(3).                               00693018
          05 WS-NUMB            PIC 9(16).                              00694018
       77 WS-END                PIC X(17) VALUE 'CIOCCS07 WS END'.      00700000
      *                                                                 00710000
       LINKAGE SECTION.                                                 00720000
       01 DFHCOMMAREA.                                                  00730000
          05 LK-FIRST-SEND      PIC X(1).                               00740000
          05 LK-OPTION          PIC 9(3).                               00750004
          05 LK-NUMB            PIC 9(16).                              00760000
          05 LK-CUST-ID         PIC 9(18).                              00770000
      *                                                                 00780000
       PROCEDURE DIVISION.                                              00790000
       0000-MAINLINE.                                                   00800000
      *                                                                 00810000
            PERFORM 1000-INIT                                           00820000
               THRU 1000-INIT-EXIT                                      00830000
      *                                                                 00840000
            PERFORM 2000-PRE-PROCESSING                                 00850000
               THRU 2000-PRE-PROCESSING-EXIT                            00860000
      *                                                                 00870000
            PERFORM 3000-MAIN-PROCESS                                   00880000
               THRU 3000-MAIN-PROCESS-EXIT                              00890000
      *                                                                 00900000
            PERFORM 4000-POST-PROCESSING                                00910000
               THRU 4000-POST-PROCESSING-EXIT                           00920000
      *                                                                 00930000
            PERFORM 5000-CLEAN-UP                                       00940000
               THRU 5000-CLEAN-UP-EXIT                                  00950000
            .                                                           00960000
      *                                                                 00970000
       0000-EXIT.                                                       00980000
            EXIT.                                                       00990000
      *                                                                 01000000
       1000-INIT.                                                       01010000
            IF LK-FIRST-SEND = 'Y'                                      01020000
               MOVE LOW-VALUES TO CICS07O                               01030000
               SET WS-MAP-ERASE TO TRUE                                 01060000
               PERFORM 3030-SEND-MAP                                    01070000
                  THRU 3030-SEND-MAP-EXIT                               01080000
      * NOT FIRST SHOW                                                  01090000
            ELSE                                                        01100000
                  MOVE LOW-VALUES TO CICS07I                            01110000
                  EXEC CICS RECEIVE MAP('CICS07')                       01120000
                                   MAPSET('CICS07')                     01130000
                                   INTO(CICS07I)                        01140000
                                   RESP(WS-RESP-CODE)                   01150000
                  END-EXEC                                              01160000
            END-IF                                                      01170000
            .                                                           01180000
       1000-INIT-EXIT.                                                  01190000
            EXIT.                                                       01200000
      *                                                                 01210000
       1010-ASK-TIME-DATE.                                              01220000
      *                                                                 01230000
            EXEC CICS                                                   01240000
                 ASKTIME                                                01250000
                 ABSTIME(WS-GETTIME)                                    01260000
            END-EXEC                                                    01270000
            EXEC CICS                                                   01280000
                 FORMATTIME                                             01290000
                 ABSTIME(WS-GETTIME)                                    01300000
                 DATESEP('/')                                           01310000
                 YYYYMMDD(WS-DATEOUT)                                   01320000
            END-EXEC                                                    01330000
            EXEC CICS                                                   01340000
                 FORMATTIME                                             01350000
                 ABSTIME(WS-GETTIME)                                    01360000
                 TIMESEP                                                01370000
                 TIME(WS-TIMEOUT)                                       01380000
            END-EXEC                                                    01390000
            MOVE WS-DATEOUT TO SYSDO                                    01400000
            MOVE WS-TIMEOUT TO SYSTO                                    01410000
            .                                                           01420000
      *                                                                 01430000
       1010-ASK-TIME-DATE-EXIT.                                         01440000
            EXIT.                                                       01450000
      *                                                                 01460000
       1020-GET-EXISTING-CUST.                                          01470000
            INITIALIZE SDCA-SERVICE-COMMAREA                            01480000
            MOVE 'VBS.CI.CUSTOMER.INQ' TO SD-SRV-NAME                   01490009
            INITIALIZE CIC0003I-REC                                     01500009
            MOVE LK-CUST-ID TO CIC0003I-ID                              01510011
            MOVE CIC0003I-REC TO SD-SRV-INPUT-DATA                      01520009
            EXEC CICS                                                   01530000
                 LINK                                                   01540000
                 PROGRAM(WS-PGM-SRV-DRIVER)                             01550000
                 COMMAREA(WS-SRV-COMMAREA)                              01560000
                 RESP(WS-RESP-CODE)                                     01570000
            END-EXEC                                                    01580000
            EVALUATE WS-RESP-CODE                                       01590000
                WHEN DFHRESP(NORMAL)                                    01600000
                     IF SD-RESP-CODE EQUAL ZEROS                        01610000
                        INITIALIZE CIC0003O-REC                         01620009
                        MOVE SD-SRV-OUTPUT-DATA TO CIC0003O-REC         01630009
                     ELSE                                               01630109
                        MOVE 'CUSTOMER READ ERROR' TO MSGO              01630209
                        SET WS-MAP-DATAONLY TO TRUE                     01630309
                        PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT   01630409
                     END-IF                                             01630509
                WHEN OTHER                                              01630609
                     MOVE 'SERVICE PROGRAM ERROR' TO MSGO               01630709
                     SET WS-MAP-DATAONLY TO TRUE                        01630809
                     PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT      01630909
            END-EVALUATE                                                01631009
                PERFORM 1030-UPDATE-CUST                                01633009
                        THRU 1030-UPDATE-CUST-EXIT                      01634009
            .                                                           01660012
      *                                                                 01670000
       1020-GET-EXISTING-CUST-EXIT.                                     01680000
            EXIT.                                                       01690000
      *                                                                 01700000
       1030-UPDATE-CUST.                                                01701009
            INITIALIZE SDCA-SERVICE-COMMAREA                            01710009
            MOVE 'VBS.CI.CUSTOMER.UPD' TO SD-SRV-NAME                   01720009
            INITIALIZE CIC0002I-REC                                     01730009
            MOVE CIC0003O-REC TO CIC0002I-REC                           01740009
            MOVE PWD1I TO CIC0002I-QURPWD                               01750009
            MOVE CIC0002I-REC  TO SD-SRV-INPUT-DATA                     01770009
            EXEC CICS                                                   01780009
                 LINK                                                   01790009
                 PROGRAM(WS-PGM-SRV-DRIVER)                             01800009
                 COMMAREA(WS-SRV-COMMAREA)                              01810009
                 RESP(WS-RESP-CODE)                                     01820009
            END-EXEC                                                    01830009
            EVALUATE WS-RESP-CODE                                       01840009
                WHEN DFHRESP(NORMAL)                                    01850009
                     IF SD-RESP-CODE EQUAL ZEROS                        01860009
                        MOVE 'INQUERY PASSWORD SET SUCCESSFULLY'        01870009
                          TO MSGO                                       01880009
                        MOVE 002 TO LK-OPTION                           01890017
                     ELSE                                               01910009
                        MOVE SD-RESP-ADDITIONAL TO MSGO                 01920009
                     END-IF                                             01930009
                WHEN OTHER                                              01940009
                     MOVE 'SERVICE PROGRAM ERROR' TO MSGO               01950009
            END-EVALUATE                                                01960009
            SET WS-MAP-DATAONLY TO TRUE                                 01970009
            PERFORM 3030-SEND-MAP                                       01980009
               THRU 3030-SEND-MAP-EXIT                                  01990009
            .                                                           02000013
      *                                                                 02010009
       1030-UPDATE-CUST-EXIT.                                           02020009
                  EXIT.                                                 02030009
      *                                                                 02040009
       2000-PRE-PROCESSING.                                             02080000
      *                                                                 02090000
       2000-PRE-PROCESSING-EXIT.                                        02100000
            EXIT.                                                       02110000
      *                                                                 02120000
       3000-MAIN-PROCESS.                                               02130000
            EVALUATE EIBAID ALSO TRUE                                   02140000
                WHEN DFHPF1 ALSO LK-OPTION = 001                        02150018
                MOVE 'Y' TO WS-FIRST-SEND OF WS-COMMAREA-CS06           02160002
                MOVE 001 TO WS-OPTION OF WS-COMMAREA-CS06               02170005
                MOVE LK-NUMB TO WS-NUMB OF WS-COMMAREA-CS06             02180002
                MOVE LK-CUST-ID TO WS-CUST-ID OF WS-COMMAREA-CS06       02190002
                     EXEC CICS                                          02200000
                          XCTL PROGRAM('CIOCCS06')                      02210002
                           COMMAREA(WS-COMMAREA-CS06)                   02220002
                               RESP(WS-RESP-CODE)                       02230000
                     END-EXEC                                           02240000
                     IF WS-RESP-CODE NOT = DFHRESP(NORMAL)              02250000
                        MOVE 'PROGRAM CIOCCS06 IS NOT AVAILABLE'        02260005
                                TO MSGO                                 02270000
                        SET WS-MAP-DATAONLY TO TRUE                     02280000
                        PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT   02290000
                     END-IF                                             02300000
                WHEN DFHPF1 ALSO LK-OPTION = 002                        02300118
                MOVE 'Y' TO WS-FIRST-SEND OF WS-COMMAREA-CS05           02300218
                MOVE 002 TO WS-OPTION OF WS-COMMAREA-CS05               02300318
                MOVE LK-NUMB TO WS-NUMB OF WS-COMMAREA-CS05             02300418
                     EXEC CICS                                          02300618
                          XCTL PROGRAM('CIOCCS05')                      02300718
                           COMMAREA(WS-COMMAREA-CS05)                   02300818
                               RESP(WS-RESP-CODE)                       02300918
                     END-EXEC                                           02301018
                     IF WS-RESP-CODE NOT = DFHRESP(NORMAL)              02301118
                        MOVE 'PROGRAM CIOCCS06 IS NOT AVAILABLE'        02301218
                                TO MSGO                                 02301318
                        SET WS-MAP-DATAONLY TO TRUE                     02301418
                        PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT   02301518
                     END-IF                                             02301618
                WHEN DFHPF3 ALSO ANY                                    02310000
                     MOVE 'THANK YOU FOR USING THE SYSTEM' TO WS-MESSAGE02320000
                     EXEC CICS                                          02330000
                          SEND CONTROL                                  02340000
                          CURSOR                                        02350000
                          ERASE                                         02360000
                          FREEKB                                        02370000
                          ALARM                                         02380000
                     END-EXEC                                           02390000
                     EXEC CICS                                          02400000
                          SEND FROM(WS-MESSAGE)                         02410000
                     END-EXEC                                           02420000
                     PERFORM 5010-RETURN THRU 5010-RETURN-EXIT          02430000
                WHEN DFHCLEAR ALSO ANY                                  02440000
                     EXEC CICS                                          02450000
                           SEND CONTROL                                 02460000
                           CURSOR                                       02470000
                           ERASE                                        02480000
                           FREEKB                                       02490000
                           ALARM                                        02500000
                     END-EXEC                                           02510000
                     PERFORM 5010-RETURN THRU 5010-RETURN-EXIT          02520000
                 WHEN DFHENTER ALSO LK-OPTION = 001                     02521005
                      IF WS-RESP-CODE NOT EQUAL DFHRESP(NORMAL)         02522005
                         MOVE 'INVALID REQUEST, CHECK YOUR INPUT.'      02523005
                              TO MSGO                                   02524005
                         SET WS-MAP-DATAONLY TO TRUE                    02525005
                         PERFORM 3030-SEND-MAP                          02526005
                            THRU 3030-SEND-MAP-EXIT                     02527005
                      ELSE                                              02528005
                         PERFORM 3010-CHECK-INPUT                       02529005
                            THRU 3010-CHECK-INPUT-EXIT                  02529105
                        PERFORM 1020-GET-EXISTING-CUST                  02529215
                           THRU 1020-GET-EXISTING-CUST-EXIT             02529315
                      END-IF                                            02529405
                WHEN DFHENTER ALSO LK-OPTION = 002                      02530005
                     INITIALIZE WS-COMMAREA-CS08                        02540010
                     MOVE 'Y' TO WS-FIRST-SEND OF WS-COMMAREA-CS08      02550005
                     MOVE '001' TO WS-OPTION OF WS-COMMAREA-CS08        02560005
                     MOVE LK-NUMB TO WS-NUMB OF WS-COMMAREA-CS08        02570005
                     MOVE LK-CUST-ID TO WS-CUST-ID OF                   02580000
                          WS-COMMAREA-CS08                              02590005
                     EXEC CICS                                          02600000
                          XCTL PROGRAM('CIOCCS08')                      02610005
                               RESP(WS-RESP-CODE)                       02620000
                               COMMAREA(WS-COMMAREA-CS08)               02630005
                     END-EXEC                                           02640000
                     IF WS-RESP-CODE NOT = DFHRESP(NORMAL)              02650000
                        MOVE 'PROGRAM CIOCCS08 IS NOT AVAILABLE'        02660005
                                TO MSGO                                 02670000
                        SET WS-MAP-DATAONLY TO TRUE                     02680000
                        PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT   02690000
                     END-IF                                             02700000
                 WHEN OTHER                                             02840000
                      MOVE 'INVALID KEY PRESSED!' TO MSGO               02850000
                      SET WS-MAP-DATAONLY TO TRUE                       02860000
                      PERFORM 3030-SEND-MAP                             02870000
                         THRU 3030-SEND-MAP-EXIT                        02880000
            END-EVALUATE                                                02890000
            .                                                           02900000
       3000-MAIN-PROCESS-EXIT.                                          02910000
            EXIT.                                                       02920000
      *                                                                 02930000
       3010-CHECK-INPUT.                                                02940000
            IF COMMUL NOT = 0                                           02950000
               INITIALIZE CIMENU-REC                                    02960000
               MOVE COMMUI TO CIMENU-TRANSID                            02970000
               EXEC CICS READ                                           02980000
                    FILE('CIMENU')                                      02990000
                    INTO(CIMENU-REC)                                    03000000
                    RIDFLD(CIMENU-TRANSID)                              03010000
                    RESP(WS-RESP-CODE)                                  03020000
               END-EXEC                                                 03030000
               EVALUATE WS-RESP-CODE                                    03040000
                   WHEN DFHRESP(NORMAL)                                 03050000
                        EXEC CICS                                       03060000
                             XCTL PROGRAM(CIMENU-PGM)                   03070000
                             COMMAREA(WS-COMMAREA)                      03080000
                             RESP(WS-RESP-CODE)                         03090000
                        END-EXEC                                        03100000
                        IF WS-RESP-CODE NOT = DFHRESP(NORMAL)           03110000
                        STRING 'PROGRAM ' DELIMITED BY SIZE             03120000
                               CIMENU-PGM DELIMITED BY SPACE            03130000
                               ' IS NOT AVAILABLE' DELIMITED BY SIZE    03140000
                               INTO MSGO                                03150000
                           SET WS-MAP-DATAONLY TO TRUE                  03160000
                           PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT03170000
                        END-IF                                          03180000
                   WHEN DFHRESP(NOTFND)                                 03190000
                        MOVE 'INVALID TRANSATION ID!' TO MSGO           03200000
                        SET WS-MAP-DATAONLY TO TRUE                     03210000
                        PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT   03220000
                   WHEN OTHER                                           03230000
                        MOVE 'CIMENU FILE ERROR!' TO MSGO               03240000
                        SET WS-MAP-DATAONLY TO TRUE                     03250000
                        PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT   03260000
               END-EVALUATE                                             03270000
            END-IF                                                      03280000
                IF (PWD1L NOT = 6 OR PWD2L NOT = 6)                     03280108
                   MOVE 'THE PASSWORD LENGTH MUST BE 6' TO MSGO         03280201
                   SET WS-MAP-DATAONLY TO TRUE                          03280401
                   PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT        03280501
                END-IF                                                  03280601
                IF (PWD1I IS NOT NUMERIC OR PWD2I IS NOT NUMERIC)       03280707
                   MOVE 'THE PASSWORD MUST BE NUMBER!' TO MSGO          03280807
                   SET WS-MAP-DATAONLY TO TRUE                          03280907
                   PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT        03281007
                END-IF                                                  03281107
                IF (PWD1I NOT = PWD2I)                                  03282001
                   MOVE 'THE TWO INPUT PASSWORD DOES NOT MATCH' TO MSGO 03284001
                   SET WS-MAP-DATAONLY TO TRUE                          03285001
                   PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT        03286001
                END-IF                                                  03287001
            .                                                           03290000
      *                                                                 03300000
       3010-CHECK-INPUT-EXIT.                                           03310000
            EXIT.                                                       03320000
      *                                                                 03340000
       3030-SEND-MAP.                                                   03350000
            IF LK-OPTION = 001                                          03360005
               MOVE 'PF1=RETURN PF3=EXIT ENTER=PROCESS' TO KEYO         03390001
            ELSE                                                        03400005
               MOVE 'PF1=RETURN PF3=EXIT ENTER=SET TRANSCATION PASSWORD'03401005
                    TO KEYO                                             03402005
            END-IF                                                      03410005
            PERFORM 1010-ASK-TIME-DATE                                  03420000
               THRU 1010-ASK-TIME-DATE-EXIT                             03430000
            EVALUATE TRUE                                               03440000
                WHEN WS-MAP-ERASE                                       03450000
                     EXEC CICS SEND                                     03460000
                          MAP('CICS07')                                 03470001
                          MAPSET('CICS07')                              03480001
                          FROM(CICS07O)                                 03490001
                          ERASE                                         03500000
                     END-EXEC                                           03510000
                WHEN WS-MAP-DATAONLY                                    03520000
                     EXEC CICS SEND                                     03530000
                          MAP('CICS07')                                 03540001
                          MAPSET('CICS07')                              03550001
                          FROM(CICS07O)                                 03560001
                          DATAONLY                                      03570000
                     END-EXEC                                           03580000
            END-EVALUATE                                                03590000
      *     MOVE '1' TO WS-ENTER-FLAG                                   03600000
            PERFORM 5020-RETURN-TRANS THRU 5020-RETURN-TRANS-EXIT       03610000
            .                                                           03620000
      *                                                                 03630000
       3030-SEND-MAP-EXIT.                                              03640000
            EXIT.                                                       03650000
      *                                                                 03660000
       4000-POST-PROCESSING.                                            04020000
      *                                                                 04030000
       4000-POST-PROCESSING-EXIT.                                       04040000
            EXIT.                                                       04050000
      *                                                                 04060000
       5000-CLEAN-UP.                                                   04070000
            PERFORM 5010-RETURN                                         04080000
               THRU 5010-RETURN-EXIT                                    04090000
            .                                                           04100000
      *                                                                 04110000
       5000-CLEAN-UP-EXIT.                                              04120000
            EXIT.                                                       04130000
      *                                                                 04140000
       5010-RETURN.                                                     04150000
            EXEC CICS RETURN END-EXEC                                   04160000
            .                                                           04170000
       5010-RETURN-EXIT.                                                04180000
            EXIT.                                                       04190000
      *                                                                 04200000
       5020-RETURN-TRANS.                                               04210000
            INITIALIZE WS-COMMAREA                                      04220000
            MOVE 'N' TO WS-FIRST-SEND OF WS-COMMAREA                    04230000
      *     IF LK-FIRST-SEND = 'T'                                      04231017
      *        MOVE 002 TO WS-OPTION OF WS-COMMAREA                     04232017
      *     ELSE                                                        04233017
      *        MOVE 001 TO WS-OPTION OF WS-COMMAREA                     04234017
      *     END-IF                                                      04235017
            MOVE LK-OPTION TO WS-OPTION OF WS-COMMAREA                  04240017
            MOVE LK-NUMB TO WS-NUMB OF WS-COMMAREA                      04250000
            MOVE LK-CUST-ID TO WS-CUST-ID OF WS-COMMAREA                04260000
            EXEC CICS RETURN TRANSID('CIB7')                            04270001
                      COMMAREA(WS-COMMAREA)                             04280000
            END-EXEC                                                    04290000
            .                                                           04300000
       5020-RETURN-TRANS-EXIT.                                          04310000
            EXIT.                                                       04320000
