       IDENTIFICATION DIVISION.                                         00010000
       PROGRAM-ID. CIOCTA05.                                            00020000
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
       77 WS-BEGIN              PIC X(17) VALUE 'CIOCTA05 WS BEGIN'.    00190000
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
       COPY CICP04.                                                     00350000
      *MAP CONTROL                                                      00360000
       COPY DFHBMSCA.                                                   00370000
      *CICS FUNCTION KEYS                                               00380000
       COPY DFHAID.                                                     00390000
      *CIMENU                                                           00400000
       COPY CIMENU.                                                     00410000
      *                                                                 00420000
       COPY CIC0015I.                                                   00430000
       COPY CIC0015O.                                                   00440000
      *                                                                 00450000
       COPY CIC0014I.                                                   00460000
       COPY CIC0014O.                                                   00470000
      *                                                                 00480000
       01 WS-SRV-COMMAREA.                                              00490000
      *SERVICE REQUEST/RESPONSE COMMAREA                                00500000
       COPY SD01WS.                                                     00510000
      *                                                                 00520000
       01 WS-COMMAREA.                                                  00530000
          05 WS-FIRST-SEND      PIC X(1).                               00540000
      *   05 WS-OPTION          PIC 9(3).                               00550000
          05 WS-CARD-NUMB       PIC 9(16).                              00560000
      *   05 WS-CUST-ID-NUMB    PIC 9(18).                              00570000
      *                                                                 00580000
       01 WS-COMMAREA-TA05.                                             00590001
          05 WS-FIRST-SEND      PIC X(1).                               00600000
      *   05 WS-OPTION          PIC 9(3).                               00610000
          05 WS-CARD-NUMB       PIC 9(16).                              00620000
      *                                                                 00630000
       77 WS-END                PIC X(17) VALUE 'CIOCTA05 WS END'.      00640000
      *                                                                 00650000
       LINKAGE SECTION.                                                 00660000
       01 DFHCOMMAREA.                                                  00670000
          05 LK-FIRST-SEND      PIC X(1).                               00680000
          05 LK-CARD-NUMB       PIC 9(16).                              00700000
      *                                                                 00720000
       PROCEDURE DIVISION.                                              00730000
       0000-MAINLINE.                                                   00740000
      *                                                                 00750000
            PERFORM 1000-INIT                                           00760000
               THRU 1000-INIT-EXIT                                      00770000
      *                                                                 00780000
            PERFORM 2000-PRE-PROCESSING                                 00790000
               THRU 2000-PRE-PROCESSING-EXIT                            00800000
      *                                                                 00810000
            PERFORM 3000-MAIN-PROCESS                                   00820000
               THRU 3000-MAIN-PROCESS-EXIT                              00830000
      *                                                                 00840000
            PERFORM 4000-POST-PROCESSING                                00850000
               THRU 4000-POST-PROCESSING-EXIT                           00860000
      *                                                                 00870000
            PERFORM 5000-CLEAN-UP                                       00880000
               THRU 5000-CLEAN-UP-EXIT                                  00890000
            .                                                           00900000
      *                                                                 00910000
       0000-EXIT.                                                       00920000
            EXIT.                                                       00930000
      *                                                                 00940000
       1000-INIT.                                                       00950000
            IF LK-FIRST-SEND = 'Y'                                      00960000
               MOVE LOW-VALUES TO CICP04O                               00970000
               PERFORM 1020-GET-EXISTING-CARD                           00980000
                  THRU 1020-GET-EXISTING-CARD-EXIT                      00990000
               SET WS-MAP-ERASE TO TRUE                                 01000000
               PERFORM 3030-SEND-MAP                                    01010000
                  THRU 3030-SEND-MAP-EXIT                               01020000
      * NOT FIRST SHOW                                                  01030000
            ELSE                                                        01040000
                  MOVE LOW-VALUES TO CICP04I                            01050000
                  EXEC CICS RECEIVE MAP('CICP04')                       01060000
                                   MAPSET('CICP04')                     01070000
                                   INTO(CICP04I)                        01080000
                                   RESP(WS-RESP-CODE)                   01090000
                  END-EXEC                                              01100000
            END-IF                                                      01110000
            .                                                           01120000
       1000-INIT-EXIT.                                                  01130000
            EXIT.                                                       01140000
      *                                                                 01150000
       1010-ASK-TIME-DATE.                                              01160000
      *                                                                 01170000
            EXEC CICS                                                   01180000
                 ASKTIME                                                01190000
                 ABSTIME(WS-GETTIME)                                    01200000
            END-EXEC                                                    01210000
            EXEC CICS                                                   01220000
                 FORMATTIME                                             01230000
                 ABSTIME(WS-GETTIME)                                    01240000
                 DATESEP('/')                                           01250000
                 YYYYMMDD(WS-DATEOUT)                                   01260000
            END-EXEC                                                    01270000
            EXEC CICS                                                   01280000
                 FORMATTIME                                             01290000
                 ABSTIME(WS-GETTIME)                                    01300000
                 TIMESEP                                                01310000
                 TIME(WS-TIMEOUT)                                       01320000
            END-EXEC                                                    01330000
            MOVE WS-DATEOUT TO SYSDO                                    01340000
            MOVE WS-TIMEOUT TO SYSTO                                    01350000
            .                                                           01360000
      *                                                                 01370000
       1010-ASK-TIME-DATE-EXIT.                                         01380000
            EXIT.                                                       01390000
      *                                                                 01400000
       1020-GET-EXISTING-CARD.                                          01410000
            INITIALIZE SDCA-SERVICE-COMMAREA                            01420000
            MOVE 'VBS.CI.CREDCARD.INQ' TO SD-SRV-NAME                   01430000
            INITIALIZE CIC0015I-REC                                     01440000
            MOVE LK-CARD-NUMB TO CIC0015I-NUMB                          01450000
            MOVE CIC0015I-REC TO SD-SRV-INPUT-DATA                      01460000
            EXEC CICS                                                   01470000
                 LINK                                                   01480000
                 PROGRAM(WS-PGM-SRV-DRIVER)                             01490000
                 COMMAREA(WS-SRV-COMMAREA)                              01500000
                 RESP(WS-RESP-CODE)                                     01510000
            END-EXEC                                                    01520000
            EVALUATE WS-RESP-CODE                                       01530000
                WHEN DFHRESP(NORMAL)                                    01540000
                     IF SD-RESP-CODE EQUAL ZEROS                        01550000
                        PERFORM 1030-POPULATE-CARD                      01560000
                           THRU 1030-POPULATE-CARD-EXIT                 01570000
                     END-IF                                             01580000
            END-EVALUATE                                                01590000
            .                                                           01600000
      *                                                                 01610000
       1020-GET-EXISTING-CARD-EXIT.                                     01620000
            EXIT.                                                       01630000
      *                                                                 01640000
       1030-POPULATE-CARD.                                              01650000
            INITIALIZE CIC0015O-REC                                     01660000
            MOVE SD-SRV-OUTPUT-DATA       TO CIC0015O-REC               01670000
            MOVE CIC0015O-PROD-TYPE       TO PTYPEO                     01680000
            MOVE CIC0015O-NUMB            TO CANUMO                     01690000
            MOVE CIC0015O-STATUS          TO CASTAO                     01700000
            MOVE CIC0015O-ACCT-NUMB       TO ACNUMO                     01710000
            MOVE CIC0015O-CUST-NUMB       TO CUNUMO                     01720000
            MOVE CIC0015O-LAST-DATE       TO LUDATO                     01730000
            MOVE CIC0015O-OPEN-DATE       TO OPDATO                     01740000
            MOVE CIC0015O-DISP-IND        TO DIINDO                     01750000
            MOVE CIC0015O-DISP-SUBDATE    TO DISUBO                     01760000
            MOVE CIC0015O-DISP-DATE       TO DIDATO                     01770000
            MOVE CIC0015O-TYPE            TO CATYPO                     01780000
            MOVE CIC0015O-PRISEC-IND      TO PSINDO                     01790000
            MOVE CIC0015O-LOCK-IND        TO LOINDO                     01800000
            MOVE CIC0015O-LOCK-DATE       TO LODATO                     01810000
            MOVE CIC0015O-CURR-EXPIRY-DATE TO EXDATO                    01820000
            MOVE CIC0015O-ACTIVATE-IND    TO ACINDO                     01830000
            MOVE CIC0015O-ACTIVATE-DATE   TO AVDATO                     01840000
            MOVE CIC0015O-CURR-ACTION     TO CUACTO                     01850000
            MOVE CIC0015O-LAST-ACTION     TO LAACTO                     01860000
            MOVE CIC0015O-CVV             TO CACVVO                     01870000
            MOVE CIC0015O-CVV2            TO CACVV2O                    01880000
            MOVE CIC0015O-CUST-NAME       TO CUNAMO                     01890000
            MOVE CIC0015O-TRNPWD-IND      TO NAPASO                     01900000
            MOVE CIC0015O-TRNPWD-WRGCNT    TO PWCOUO                    01910000
            MOVE CIC0015O-TRNPWD-WRGDATE   TO PWDATO                    01920000
            MOVE CIC0015O-TRNPWD-WRGTIME   TO PWTIMO                    01930000
            MOVE CIC0015O-TRNPWD-LAST-DATE TO PUDATO                    01940000
            .                                                           01950000
      *                                                                 01960000
       1030-POPULATE-CARD-EXIT.                                         01970000
            EXIT.                                                       01980000
       2000-PRE-PROCESSING.                                             01990000
      *                                                                 02000000
       2000-PRE-PROCESSING-EXIT.                                        02010000
            EXIT.                                                       02020000
      *                                                                 02030000
       3000-MAIN-PROCESS.                                               02040000
            EVALUATE EIBAID                                             02050001
                WHEN DFHPF1                                             02060001
                MOVE 'Y' TO WS-FIRST-SEND OF WS-COMMAREA-TA05           02070001
                MOVE LK-CARD-NUMB TO WS-CARD-NUMB OF                    02090000
                                               WS-COMMAREA-TA05         02100001
                     EXEC CICS                                          02110000
                          XCTL PROGRAM('CIOCTA03')                      02120000
                           COMMAREA(WS-COMMAREA-TA05)                   02130001
                               RESP(WS-RESP-CODE)                       02140000
                     END-EXEC                                           02150000
                     IF WS-RESP-CODE NOT = DFHRESP(NORMAL)              02160000
                        MOVE 'PROGRAM CIOCCIMN IS NOT AVAILABLE'        02170000
                                TO MSGO                                 02180000
                        SET WS-MAP-DATAONLY TO TRUE                     02190000
                        PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT   02200000
                     END-IF                                             02210000
                WHEN DFHPF3                                             02220001
                     MOVE 'THANK YOU FOR USING THE SYSTEM' TO WS-MESSAGE02230000
                     EXEC CICS                                          02240000
                          SEND CONTROL                                  02250000
                          CURSOR                                        02260000
                          ERASE                                         02270000
                          FREEKB                                        02280000
                          ALARM                                         02290000
                     END-EXEC                                           02300000
                     EXEC CICS                                          02310000
                          SEND FROM(WS-MESSAGE)                         02320000
                     END-EXEC                                           02330000
                     PERFORM 5010-RETURN THRU 5010-RETURN-EXIT          02340000
                WHEN DFHCLEAR                                           02350001
                     EXEC CICS                                          02360000
                           SEND CONTROL                                 02370000
                           CURSOR                                       02380000
                           ERASE                                        02390000
                           FREEKB                                       02400000
                           ALARM                                        02410000
                     END-EXEC                                           02420000
                     PERFORM 5010-RETURN THRU 5010-RETURN-EXIT          02430000
                 WHEN OTHER                                             02570000
                      MOVE 'INVALID KEY PRESSED!' TO MSGO               02580000
                      SET WS-MAP-DATAONLY TO TRUE                       02590000
                      PERFORM 3030-SEND-MAP                             02600000
                         THRU 3030-SEND-MAP-EXIT                        02610000
            END-EVALUATE                                                02620000
            .                                                           02630000
       3000-MAIN-PROCESS-EXIT.                                          02640000
            EXIT.                                                       02650000
      *                                                                 02660000
       3010-CHECK-INPUT.                                                02670000
            IF COMMUL NOT = 0                                           02680000
               INITIALIZE CIMENU-REC                                    02690000
               MOVE COMMUI TO CIMENU-TRANSID                            02700000
               EXEC CICS READ                                           02710000
                    FILE('CIMENU')                                      02720000
                    INTO(CIMENU-REC)                                    02730000
                    RIDFLD(CIMENU-TRANSID)                              02740000
                    RESP(WS-RESP-CODE)                                  02750000
               END-EXEC                                                 02760000
               EVALUATE WS-RESP-CODE                                    02770000
                   WHEN DFHRESP(NORMAL)                                 02780000
                        EXEC CICS                                       02790000
                             XCTL PROGRAM(CIMENU-PGM)                   02800000
                             COMMAREA(WS-COMMAREA)                      02810000
                             RESP(WS-RESP-CODE)                         02820000
                        END-EXEC                                        02830000
                        IF WS-RESP-CODE NOT = DFHRESP(NORMAL)           02840000
                        STRING 'PROGRAM ' DELIMITED BY SIZE             02850000
                               CIMENU-PGM DELIMITED BY SPACE            02860000
                               ' IS NOT AVAILABLE' DELIMITED BY SIZE    02870000
                               INTO MSGO                                02880000
                           SET WS-MAP-DATAONLY TO TRUE                  02890000
                           PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT02900000
                        END-IF                                          02910000
                   WHEN DFHRESP(NOTFND)                                 02920000
                        MOVE 'INVALID TRANSATION ID!' TO MSGO           02930000
                        SET WS-MAP-DATAONLY TO TRUE                     02940000
                        PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT   02950000
                   WHEN OTHER                                           02960000
                        MOVE 'CIMENU FILE ERROR!' TO MSGO               02970000
                        SET WS-MAP-DATAONLY TO TRUE                     02980000
                        PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT   02990000
               END-EVALUATE                                             03000000
            END-IF                                                      03010000
            IF (PTYPEL = 0 OR CANUML = 0 OR CUNUML = 0                  03020000
                 OR CASTAL = 0 OR ACNUML = 0 OR LUDATL = 0              03030000
                 OR OPDATL = 0 OR DIINDL = 0 OR DISUBL = 0              03040000
                 OR DIDATL = 0 OR CATYPL = 0 OR PSINDL = 0              03050000
                 OR LOINDL = 0 OR LODATL = 0 OR EXDATL = 0              03060000
                 OR ACINDL = 0 OR AVDATL = 0 OR CUACTL = 0              03070000
                 OR LAACTL = 0 OR CACVVL = 0 OR CACVV2L = 0             03080000
                 OR CUNAML = 0 OR NAPASL = 0 OR PWCOUL = 0              03090000
                 OR PWDATL = 0 OR PWTIML = 0 OR PUDATL = 0)             03100000
               MOVE 'FIELDS CAN NOT BE EMPTY' TO MSGO                   03110000
               SET WS-MAP-DATAONLY TO TRUE                              03120000
               PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT            03130000
            END-IF                                                      03140000
            IF (PTYPEI NOT = 001 AND PTYPEI NOT = 002                   03150000
                AND PTYPEI NOT = 003)                                   03160000
               MOVE 'INVAILD PRODUCT TYPE' TO MSGO                      03170000
               SET WS-MAP-DATAONLY TO TRUE                              03180000
               PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT            03190000
            END-IF                                                      03200000
            IF (CASTAI NOT = 001 AND CASTAI NOT = 002 AND               03210000
                CASTAI NOT = 003 AND CASTAI NOT = 004)                  03220000
               MOVE 'INVAILD CARD STATUS' TO MSGO                       03230000
               SET WS-MAP-DATAONLY TO TRUE                              03240000
               PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT            03250000
            END-IF                                                      03260000
            IF (DIINDI NOT = 01 AND DIINDI NOT = 02)                    03270000
               MOVE 'INVAILD DISPOSE IND' TO MSGO                       03280000
               SET WS-MAP-DATAONLY TO TRUE                              03290000
               PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT            03300000
            END-IF                                                      03310000
            IF (CATYPI NOT = 01 AND CATYPI NOT = 02)                    03320000
               MOVE 'INVAILD CARD TYPE' TO MSGO                         03330000
               SET WS-MAP-DATAONLY TO TRUE                              03340000
               PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT            03350000
            END-IF                                                      03360000
            IF (PSINDI NOT = 01 AND PSINDI NOT = 02)                    03370000
               MOVE 'INVAILD PRI/SEC IND' TO MSGO                       03380000
               SET WS-MAP-DATAONLY TO TRUE                              03390000
               PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT            03400000
            END-IF                                                      03410000
            IF (LOINDI NOT = 01 AND LOINDI NOT = 02 AND LOINDI NOT = 03 03420000
                AND LOINDI NOT = 04 AND LOINDI NOT = 05                 03430000
                AND LOINDI NOT = 06 AND LOINDI NOT = 07                 03440000
                AND LOINDI NOT = 08)                                    03450000
               MOVE 'INVAILD LOCK IND' TO MSGO                          03460000
               SET WS-MAP-DATAONLY TO TRUE                              03470000
               PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT            03480000
            END-IF                                                      03490000
            IF (ACINDI NOT = 01 AND ACINDI NOT = 02)                    03500000
               MOVE 'INVAILD ACTIVATE IND' TO MSGO                      03510000
               SET WS-MAP-DATAONLY TO TRUE                              03520000
               PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT            03530000
            END-IF                                                      03540000
            IF (CUACTI NOT = 01 AND CUACTI NOT = 02 AND CUACTI NOT = 03)03550000
               MOVE 'INVAILD CURR ACTION' TO MSGO                       03560000
               SET WS-MAP-DATAONLY TO TRUE                              03570000
               PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT            03580000
            END-IF                                                      03590000
            IF (LAACTI NOT = 01 AND LAACTI NOT = 02 AND LAACTI NOT = 03)03600000
               MOVE 'INVAILD LAST ACTION' TO MSGO                       03610000
               SET WS-MAP-DATAONLY TO TRUE                              03620000
               PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT            03630000
            END-IF                                                      03640000
            IF (NAPASI NOT = 01 AND NAPASI NOT = 02)                    03650000
               MOVE 'INVAILD NEED TRANSACTION PASSWORD' TO MSGO         03660000
               SET WS-MAP-DATAONLY TO TRUE                              03670000
               PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT            03680000
            END-IF                                                      03690000
            .                                                           03700000
      *                                                                 03710000
       3010-CHECK-INPUT-EXIT.                                           03720000
            EXIT.                                                       03730000
      *                                                                 03740000
      *                                                                 04060000
       3030-SEND-MAP.                                                   04070000
               PERFORM 3040-SET-PROTECT                                 04090000
                  THRU 3040-SET-PROTECT-EXIT                            04100000
               MOVE 'PF1=RETURN PF3=EXIT' TO KEYO                       04110000
            PERFORM 1010-ASK-TIME-DATE                                  04210000
               THRU 1010-ASK-TIME-DATE-EXIT                             04220000
            EVALUATE TRUE                                               04230000
                WHEN WS-MAP-ERASE                                       04240000
                     EXEC CICS SEND                                     04250000
                          MAP('CICP04')                                 04260000
                          MAPSET('CICP04')                              04270000
                          FROM(CICP04O)                                 04280000
                          ERASE                                         04290000
                     END-EXEC                                           04300000
                WHEN WS-MAP-DATAONLY                                    04310000
                     EXEC CICS SEND                                     04320000
                          MAP('CICP04')                                 04330000
                          MAPSET('CICP04')                              04340000
                          FROM(CICP04O)                                 04350000
                          DATAONLY                                      04360000
                     END-EXEC                                           04370000
            END-EVALUATE                                                04380000
            PERFORM 5020-RETURN-TRANS THRU 5020-RETURN-TRANS-EXIT       04400000
            .                                                           04410000
      *                                                                 04420000
       3030-SEND-MAP-EXIT.                                              04430000
            EXIT.                                                       04440000
      *                                                                 04450000
       3040-SET-PROTECT.                                                04460000
            MOVE ATTR-PROT-MDT          TO PTYPEA                       04470000
            MOVE ATTR-PROT-MDT          TO CANUMA                       04480000
            MOVE ATTR-PROT-MDT          TO CUNUMA                       04490000
            MOVE ATTR-PROT-MDT          TO CASTAA                       04500000
            MOVE ATTR-PROT-MDT          TO ACNUMA                       04510000
            MOVE ATTR-PROT-MDT          TO LUDATA                       04520000
            MOVE ATTR-PROT-MDT          TO OPDATA                       04530000
            MOVE ATTR-PROT-MDT          TO DIINDA                       04540000
            MOVE ATTR-PROT-MDT          TO DISUBA                       04550000
            MOVE ATTR-PROT-MDT          TO DIDATA                       04560000
            MOVE ATTR-PROT-MDT          TO CATYPA                       04570000
            MOVE ATTR-PROT-MDT          TO PSINDA                       04580000
            MOVE ATTR-PROT-MDT          TO LOINDA                       04590000
            MOVE ATTR-PROT-MDT          TO LODATA                       04600000
            MOVE ATTR-PROT-MDT          TO EXDATA                       04610000
            MOVE ATTR-PROT-MDT          TO ACINDA                       04620000
            MOVE ATTR-PROT-MDT          TO AVDATA                       04630000
            MOVE ATTR-PROT-MDT          TO CUACTA                       04640000
            MOVE ATTR-PROT-MDT          TO LAACTA                       04650000
            MOVE ATTR-PROT-MDT          TO CACVVA                       04660000
            MOVE ATTR-PROT-MDT          TO CACVV2A                      04670000
            MOVE ATTR-PROT-MDT          TO CUNAMA                       04680000
            MOVE ATTR-PROT-MDT          TO NAPASA                       04690000
            MOVE ATTR-PROT-MDT          TO PWCOUA                       04700000
            MOVE ATTR-PROT-MDT          TO PWDATA                       04710000
            MOVE ATTR-PROT-MDT          TO PWTIMA                       04720000
            MOVE ATTR-PROT-MDT          TO PUDATA                       04730000
            .                                                           04740000
       3040-SET-PROTECT-EXIT.                                           04750000
            EXIT.                                                       04760000
       4000-POST-PROCESSING.                                            05390000
      *                                                                 05400000
       4000-POST-PROCESSING-EXIT.                                       05410000
            EXIT.                                                       05420000
      *                                                                 05430000
       5000-CLEAN-UP.                                                   05440000
            PERFORM 5010-RETURN                                         05450000
               THRU 5010-RETURN-EXIT                                    05460000
            .                                                           05470000
      *                                                                 05480000
       5000-CLEAN-UP-EXIT.                                              05490000
            EXIT.                                                       05500000
      *                                                                 05510000
       5010-RETURN.                                                     05520000
            EXEC CICS RETURN END-EXEC                                   05530000
            .                                                           05540000
       5010-RETURN-EXIT.                                                05550000
            EXIT.                                                       05560000
      *                                                                 05570000
       5020-RETURN-TRANS.                                               05580000
            INITIALIZE WS-COMMAREA                                      05590000
            MOVE 'N' TO WS-FIRST-SEND OF WS-COMMAREA                    05600000
      *     MOVE LK-OPTION TO WS-OPTION OF WS-COMMAREA                  05610000
            MOVE LK-CARD-NUMB TO WS-CARD-NUMB OF WS-COMMAREA            05620000
      *     MOVE LK-CUST-ID-NUMB TO WS-CUST-ID-NUMB OF WS-COMMAREA      05630000
            EXEC CICS RETURN TRANSID('CID5')                            05640000
                      COMMAREA(WS-COMMAREA)                             05650000
            END-EXEC                                                    05660000
            .                                                           05670000
       5020-RETURN-TRANS-EXIT.                                          05680000
            EXIT.                                                       05690000
