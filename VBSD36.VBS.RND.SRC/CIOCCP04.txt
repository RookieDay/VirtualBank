       IDENTIFICATION DIVISION.                                         00010000
       PROGRAM-ID. CIOCCP04.                                            00020015
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
       77 WS-BEGIN              PIC X(17) VALUE 'CIOCCP04 WS BEGIN'.    00190015
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
          05 WS-STEP            PIC 9(3).                               00550000
          05 WS-CARD-NUMBER     PIC 9(16).                              00560020
       77 WS-END                PIC X(17) VALUE 'CIOCCP04 WS END'.      00570015
      *                                                                 00580000
       LINKAGE SECTION.                                                 00590000
       01 DFHCOMMAREA.                                                  00600000
          05 LK-FIRST-SEND      PIC X(1).                               00610000
          05 LK-STEP            PIC 9(3).                               00620000
          05 LK-CARD-NUMBER     PIC 9(16).                              00630020
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
            MOVE LK-CARD-NUMBER TO CIC0015I-NUMB                        01370017
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
                WHEN DFHPF1 ALSO ANY                                    02010000
                     EXEC CICS                                          02020000
                          XCTL PROGRAM('CIOCCP00')                      02030000
                               RESP(WS-RESP-CODE)                       02040000
                     END-EXEC                                           02050000
                     IF WS-RESP-CODE NOT = DFHRESP(NORMAL)              02060000
                        MOVE 'PROGRAM CIOCCIMN IS NOT AVAILABLE'        02070000
                                TO MSGO                                 02080000
                        SET WS-MAP-DATAONLY TO TRUE                     02090000
                        PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT   02100000
                     END-IF                                             02110000
                WHEN DFHPF3 ALSO ANY                                    02120000
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
                 WHEN DFHENTER ALSO LK-STEP = 002                       02550012
                      IF WS-RESP-CODE NOT EQUAL DFHRESP(NORMAL)         02560000
                         MOVE 'INVALID REQUEST, CHECK YOUR INPUT.'      02570000
                              TO MSGO                                   02580000
                         SET WS-MAP-DATAONLY TO TRUE                    02590000
                         PERFORM 3030-SEND-MAP                          02600000
                            THRU 3030-SEND-MAP-EXIT                     02610000
                      ELSE                                              02620000
                         PERFORM 3010-CHECK-INPUT                       02630000
                            THRU 3010-CHECK-INPUT-EXIT                  02640000
                         PERFORM 3020-CARD-UPDATE                       02650017
                            THRU 3020-CARD-UPDATE-EXIT                  02660017
                      END-IF                                            02670000
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
            IF (PTYPEL = 0 OR CANUML = 0 OR CUNUML = 0                  03130015
                 OR CASTAL = 0 OR ACNUML = 0 OR LUDATL = 0              03140015
                 OR OPDATL = 0 OR DIINDL = 0 OR DISUBL = 0              03150015
                 OR DIDATL = 0 OR CATYPL = 0 OR PSINDL = 0              03160015
                 OR LOINDL = 0 OR LODATL = 0 OR EXDATL = 0              03170015
                 OR ACINDL = 0 OR AVDATL = 0 OR CUACTL = 0              03180015
                 OR LAACTL = 0 OR CACVVL = 0 OR CACVV2L = 0             03190015
                 OR CUNAML = 0 OR NAPASL = 0 OR PWCOUL = 0              03200015
                 OR PWDATL = 0 OR PWTIML = 0 OR PUDATL = 0)             03210015
               MOVE 'FIELDS CAN NOT BE EMPTY' TO MSGO                   03220000
               SET WS-MAP-DATAONLY TO TRUE                              03230000
               PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT            03240000
            END-IF                                                      03250000
            IF (PTYPEI NOT = 001 AND PTYPEI NOT = 002                   03260018
                AND PTYPEI NOT = 003)                                   03261018
               MOVE 'INVAILD PRODUCT TYPE' TO MSGO                      03270015
               SET WS-MAP-DATAONLY TO TRUE                              03280000
               PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT            03290000
            END-IF                                                      03300000
            IF (CASTAI NOT = 001 AND CASTAI NOT = 002 AND               03310018
                CASTAI NOT = 003 AND CASTAI NOT = 004)                  03311018
               MOVE 'INVAILD CARD STATUS' TO MSGO                       03320015
               SET WS-MAP-DATAONLY TO TRUE                              03330000
               PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT            03340000
            END-IF                                                      03350000
            IF (DIINDI NOT = 01 AND DIINDI NOT = 02)                    03360015
               MOVE 'INVAILD DISPOSE IND' TO MSGO                       03370015
               SET WS-MAP-DATAONLY TO TRUE                              03380000
               PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT            03390000
            END-IF                                                      03400000
            IF (CATYPI NOT = 01 AND CATYPI NOT = 02)                    03410015
               MOVE 'INVAILD CARD TYPE' TO MSGO                         03430015
               SET WS-MAP-DATAONLY TO TRUE                              03440000
               PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT            03450000
            END-IF                                                      03460000
            IF (PSINDI NOT = 01 AND PSINDI NOT = 02)                    03470015
               MOVE 'INVAILD PRI/SEC IND' TO MSGO                       03490015
               SET WS-MAP-DATAONLY TO TRUE                              03500000
               PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT            03510000
            END-IF                                                      03520000
            IF (LOINDI NOT = 01 AND LOINDI NOT = 02 AND LOINDI NOT = 03 03521015
                AND LOINDI NOT = 04 AND LOINDI NOT = 05                 03521118
                AND LOINDI NOT = 06 AND LOINDI NOT = 07                 03521218
                AND LOINDI NOT = 08)                                    03521318
               MOVE 'INVAILD LOCK IND' TO MSGO                          03522015
               SET WS-MAP-DATAONLY TO TRUE                              03523015
               PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT            03524015
            END-IF                                                      03524115
            IF (ACINDI NOT = 01 AND ACINDI NOT = 02)                    03525015
               MOVE 'INVAILD ACTIVATE IND' TO MSGO                      03526015
               SET WS-MAP-DATAONLY TO TRUE                              03527015
               PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT            03528015
            END-IF                                                      03529015
            IF (CUACTI NOT = 01 AND CUACTI NOT = 02 AND CUACTI NOT = 03)03530015
               MOVE 'INVAILD CURR ACTION' TO MSGO                       03531015
               SET WS-MAP-DATAONLY TO TRUE                              03532015
               PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT            03533015
            END-IF                                                      03534015
            IF (LAACTI NOT = 01 AND LAACTI NOT = 02 AND LAACTI NOT = 03)03534115
               MOVE 'INVAILD LAST ACTION' TO MSGO                       03534215
               SET WS-MAP-DATAONLY TO TRUE                              03534315
               PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT            03534415
            END-IF                                                      03534515
            IF (NAPASI NOT = 01 AND NAPASI NOT = 02)                    03534615
               MOVE 'INVAILD NEED TRANSACTION PASSWORD' TO MSGO         03534715
               SET WS-MAP-DATAONLY TO TRUE                              03534815
               PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT            03534915
            END-IF                                                      03535015
            .                                                           03536015
      *                                                                 03540000
       3010-CHECK-INPUT-EXIT.                                           03550000
            EXIT.                                                       03560000
      *                                                                 03570000
       3020-CARD-UPDATE.                                                03580017
            INITIALIZE SDCA-SERVICE-COMMAREA                            03590000
            MOVE 'VBS.CI.CREDCARD.UPD' TO SD-SRV-NAME                   03600017
            INITIALIZE CIC0014I-REC                                     03610015
      *     MOVE CIC0014O-REC TO CIC0014I-REC                           03620015
            PERFORM 3060-POPULATE-CICARD                                03630017
               THRU 3060-POPULATE-CICARD-EXIT                           03640017
            MOVE CIC0014I-REC  TO SD-SRV-INPUT-DATA                     03650015
            EXEC CICS                                                   03660000
                 LINK                                                   03670000
                 PROGRAM(WS-PGM-SRV-DRIVER)                             03680000
                 COMMAREA(WS-SRV-COMMAREA)                              03690000
                 RESP(WS-RESP-CODE)                                     03700000
            END-EXEC                                                    03710000
            EVALUATE WS-RESP-CODE                                       03720000
                WHEN DFHRESP(NORMAL)                                    03730000
                     IF SD-RESP-CODE EQUAL ZEROS                        03740000
                        MOVE 'CREDCARD UPDATE SUCCESSFULLY' TO MSGO     03750017
                     ELSE                                               03760000
                        MOVE SD-RESP-ADDITIONAL TO MSGO                 03770000
                     END-IF                                             03780000
                WHEN OTHER                                              03790000
                     MOVE 'SERVICE PROGRAM ERROR' TO MSGO               03800000
            END-EVALUATE                                                03810000
            SET WS-MAP-DATAONLY TO TRUE                                 03820000
            PERFORM 3030-SEND-MAP                                       03830000
               THRU 3030-SEND-MAP-EXIT                                  03840000
            .                                                           03850000
      *                                                                 03860000
       3020-CARD-UPDATE-EXIT.                                           03870017
            EXIT.                                                       03880000
      *                                                                 03890000
       3030-SEND-MAP.                                                   03900000
            IF LK-STEP = 001                                            03910000
               PERFORM 3040-SET-PROTECT                                 03920000
                  THRU 3040-SET-PROTECT-EXIT                            03930000
               MOVE 'PF1=RETURN PF3=EXIT' TO KEYO                       03940017
      *        MOVE 'PRESS ANY KEY TO RETURN ' TO KEYO                  03950010
            ELSE                                                        03960000
               PERFORM 3050-SET-UNPROTECT                               03970000
                  THRU 3050-SET-UNPROTECT-EXIT                          03980000
               MOVE 'PF1=RETURN PF3=EXIT ENTER=UPDATE' TO KEYO          03990017
            END-IF                                                      04000000
      *        IF WS-FIRST-SEND = 'T'                                   04010000
      *        MOVE 'PRESS ANY KEY TO RETURN ' TO KEYO                  04020000
      *        END-IF                                                   04030000
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
       3050-SET-UNPROTECT.                                              04630000
            MOVE ATTR-UNPROT-MDT          TO PTYPEA                     04631015
            MOVE ATTR-PROT-MDT            TO CANUMA                     04640017
            MOVE ATTR-UNPROT-MDT          TO CUNUMA                     04650015
            MOVE ATTR-UNPROT-MDT          TO CASTAA                     04660015
            MOVE ATTR-UNPROT-MDT          TO ACNUMA                     04670015
            MOVE ATTR-UNPROT-MDT          TO LUDATA                     04680015
            MOVE ATTR-UNPROT-MDT          TO OPDATA                     04681015
            MOVE ATTR-UNPROT-MDT          TO DIINDA                     04682015
            MOVE ATTR-UNPROT-MDT          TO DISUBA                     04690015
            MOVE ATTR-UNPROT-MDT          TO DIDATA                     04700015
            MOVE ATTR-UNPROT-MDT          TO CATYPA                     04710015
            MOVE ATTR-UNPROT-MDT          TO PSINDA                     04720015
            MOVE ATTR-UNPROT-MDT          TO LOINDA                     04730015
            MOVE ATTR-UNPROT-MDT          TO LODATA                     04740015
            MOVE ATTR-UNPROT-MDT          TO EXDATA                     04750015
            MOVE ATTR-UNPROT-MDT          TO ACINDA                     04760015
            MOVE ATTR-UNPROT-MDT          TO AVDATA                     04770015
            MOVE ATTR-UNPROT-MDT          TO CUACTA                     04780015
            MOVE ATTR-UNPROT-MDT          TO LAACTA                     04790015
            MOVE ATTR-UNPROT-MDT          TO CACVVA                     04800015
            MOVE ATTR-UNPROT-MDT          TO CACVV2A                    04810015
            MOVE ATTR-UNPROT-MDT          TO CUNAMA                     04820015
            MOVE ATTR-UNPROT-MDT          TO NAPASA                     04830015
            MOVE ATTR-UNPROT-MDT          TO PWCOUA                     04840015
            MOVE ATTR-UNPROT-MDT          TO PWDATA                     04850015
            MOVE ATTR-UNPROT-MDT          TO PWTIMA                     04860015
            MOVE ATTR-UNPROT-MDT          TO PUDATA                     04870015
            .                                                           04910000
       3050-SET-UNPROTECT-EXIT.                                         04920000
            EXIT.                                                       04930000
       3060-POPULATE-CICARD.                                            04940017
            MOVE PTYPEI    TO  CIC0014I-PROD-TYPE                       04950019
            MOVE CANUMI    TO  CIC0014I-NUMB                            04960019
            MOVE CASTAI    TO  CIC0014I-STATUS                          04970023
            MOVE ACNUMI    TO  CIC0014I-ACCT-NUMB                       04980023
            MOVE CUNUMI    TO  CIC0014I-CUST-NUMB                       04990023
            MOVE LUDATI    TO  CIC0014I-LAST-DATE                       05000019
            MOVE OPDATI    TO  CIC0014I-OPEN-DATE                       05010019
            MOVE DIINDI    TO  CIC0014I-DISP-IND                        05020019
            MOVE DISUBI    TO  CIC0014I-DISP-SUBDATE                    05030019
            MOVE DIDATI    TO  CIC0014I-DISP-DATE                       05040019
            MOVE CATYPI    TO  CIC0014I-TYPE                            05050019
            MOVE PSINDI    TO  CIC0014I-PRISEC-IND                      05060019
            MOVE LOINDI    TO  CIC0014I-LOCK-IND                        05070019
            MOVE LODATI    TO  CIC0014I-LOCK-DATE                       05080019
            MOVE EXDATI    TO  CIC0014I-CURR-EXPIRY-DATE                05090019
            MOVE ACINDI    TO  CIC0014I-ACTIVATE-IND                    05100019
            MOVE AVDATI    TO  CIC0014I-ACTIVATE-DATE                   05110019
            MOVE CUACTI    TO  CIC0014I-CURR-ACTION                     05120019
            MOVE LAACTI    TO  CIC0014I-LAST-ACTION                     05130019
            MOVE CACVVI    TO  CIC0014I-CVV                             05140019
            MOVE CACVV2I   TO  CIC0014I-CVV2                            05150019
            MOVE CUNAMI    TO  CIC0014I-CUST-NAME                       05160019
            MOVE NAPASI    TO  CIC0014I-TRNPWD-IND                      05170019
            MOVE PWCOUI    TO  CIC0014I-TRNPWD-WRGCNT                   05190019
            MOVE PWDATI    TO  CIC0014I-TRNPWD-WRGDATE                  05200019
            MOVE PWTIMI    TO  CIC0014I-TRNPWD-WRGTIME                  05210019
            MOVE PUDATI    TO  CIC0014I-TRNPWD-LAST-DATE                05220019
            .                                                           05250000
       3060-POPULATE-CICARD-EXIT.                                       05260017
            EXIT.                                                       05270000
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
            MOVE 'N' TO WS-FIRST-SEND                                   05490000
            MOVE LK-STEP TO WS-STEP                                     05500000
            MOVE LK-CARD-NUMBER TO WS-CARD-NUMBER                       05510017
            EXEC CICS RETURN TRANSID('CIC4')                            05520015
                      COMMAREA(WS-COMMAREA)                             05530000
            END-EXEC                                                    05540000
            .                                                           05550000
       5020-RETURN-TRANS-EXIT.                                          05560000
            EXIT.                                                       05570000
