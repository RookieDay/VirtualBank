       IDENTIFICATION DIVISION.                                         00010000
       PROGRAM-ID. CIOCCS09.                                            00020045
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
       77 WS-BEGIN              PIC X(17) VALUE 'CIOCCS09 WS BEGIN'.    00190045
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
       COPY CICS09.                                                     00350045
      *MAP CONTROL                                                      00360000
       COPY DFHBMSCA.                                                   00370000
      *CICS FUNCTION KEYS                                               00380000
       COPY DFHAID.                                                     00390000
      *CIMENU                                                           00400000
       COPY CIMENU.                                                     00410000
      *                                                                 00420000
       COPY CIC0015I.                                                   00430015
       COPY CIC0015O.                                                   00440015
      *                                                                 00441054
       COPY CIC0014I.                                                   00442054
       COPY CIC0014O.                                                   00443054
      *                                                                 00450000
       01 WS-SRV-COMMAREA.                                              00490000
      *SERVICE REQUEST/RESPONSE COMMAREA                                00500000
       COPY SD01WS.                                                     00510000
      *                                                                 00520000
       01 WS-COMMAREA.                                                  00530000
          05 WS-FIRST-SEND      PIC X(1).                               00540000
          05 WS-OPTION          PIC 9(3).                               00550027
          05 WS-CARD-NUMB       PIC 9(16).                              00560027
          05 WS-CUST-ID-NUMB    PIC 9(18).                              00560154
      *                                                                 00561025
       01 WS-COMMAREA-CS05.                                             00562045
          05 WS-FIRST-SEND      PIC X(1).                               00563025
          05 WS-OPTION          PIC 9(3).                               00564025
          05 WS-CARD-NUMB       PIC 9(16).                              00564145
      *                                                                 00569333
       01 WS-COMMAREA-CS08.                                             00569454
          05 WS-FIRST-SEND      PIC X(1).                               00569554
          05 WS-OPTION          PIC 9(3).                               00569654
          05 WS-CARD-NUMB       PIC 9(16).                              00569756
          05 WS-CUST-ID-NUMB    PIC 9(18).                              00569856
       77 WS-END                PIC X(17) VALUE 'CIOCCS09 WS END'.      00570045
      *                                                                 00580000
       LINKAGE SECTION.                                                 00590000
       01 DFHCOMMAREA.                                                  00600000
          05 LK-FIRST-SEND      PIC X(1).                               00610000
          05 LK-OPTION          PIC 9(3).                               00620027
          05 LK-CARD-NUMB       PIC 9(16).                              00630027
          05 LK-CUST-ID-NUMB    PIC 9(18).                              00631054
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
               MOVE LOW-VALUES TO CICS09O                               00890045
      *        PERFORM 1020-GET-EXISTING-CARD                           00900061
      *           THRU 1020-GET-EXISTING-CARD-EXIT                      00910061
               SET WS-MAP-ERASE TO TRUE                                 00920000
               PERFORM 3030-SEND-MAP                                    00930000
                  THRU 3030-SEND-MAP-EXIT                               00940000
      * NOT FIRST SHOW                                                  00950000
            ELSE                                                        00960000
                  MOVE LOW-VALUES TO CICS09I                            00970045
                  EXEC CICS RECEIVE MAP('CICS09')                       00980045
                                   MAPSET('CICS09')                     00990045
                                   INTO(CICS09I)                        01000045
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
            MOVE 'VBS.CI.CREDCARD.INQ' TO SD-SRV-NAME                   01350054
            INITIALIZE CIC0015I-REC                                     01360015
            MOVE  LK-CARD-NUMB TO CIC0015I-NUMB                         01370058
            MOVE CIC0015I-REC TO SD-SRV-INPUT-DATA                      01380015
            EXEC CICS                                                   01390000
                 LINK                                                   01400000
                 PROGRAM(WS-PGM-SRV-DRIVER)                             01410000
                 COMMAREA(WS-SRV-COMMAREA)                              01420000
                 RESP(WS-RESP-CODE)                                     01430000
            END-EXEC                                                    01440000
            EVALUATE WS-RESP-CODE                                       01441054
                WHEN DFHRESP(NORMAL)                                    01442054
                     IF SD-RESP-CODE EQUAL ZEROS                        01443054
                        INITIALIZE CIC0015O-REC                         01444054
                        MOVE SD-SRV-OUTPUT-DATA TO CIC0015O-REC         01445054
                     ELSE                                               01446054
                        MOVE 'CARDOMER READ ERROR' TO MSGO              01447054
                        SET WS-MAP-DATAONLY TO TRUE                     01448054
                        PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT   01449054
                     END-IF                                             01449154
                WHEN OTHER                                              01449254
                     MOVE 'SERVICE PROGRAM ERROR' TO MSGO               01449354
                     SET WS-MAP-DATAONLY TO TRUE                        01449454
                     PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT      01449554
            END-EVALUATE                                                01449654
                PERFORM 1030-UPDATE-CARD                                01449754
                        THRU 1030-UPDATE-CARD-EXIT                      01449854
            .                                                           01449954
      *     EVALUATE WS-RESP-CODE                                       01450053
      *         WHEN DFHRESP(NORMAL)                                    01460053
      *              IF SD-RESP-CODE EQUAL ZEROS                        01470053
      *                 PERFORM 1030-POPULATE-CARD                      01480053
      *                    THRU 1030-POPULATE-CARD-EXIT                 01490053
      *              END-IF                                             01500053
      *     END-EVALUATE                                                01510053
      *                                                                 01530000
       1020-GET-EXISTING-CARD-EXIT.                                     01540017
            EXIT.                                                       01550000
      *                                                                 01560000
       1030-UPDATE-CARD.                                                01570054
            INITIALIZE SDCA-SERVICE-COMMAREA                            01580054
            MOVE 'VBS.CI.CREDCARD.UPD' TO SD-SRV-NAME                   01590054
            INITIALIZE CIC0014I-REC                                     01600054
            MOVE CIC0015O-REC TO CIC0014I-REC                           01610054
            IF NTPASI = 'Y'                                             01630062
               MOVE 01 TO CIC0014I-TRNPWD-IND                           01631062
            ELSE                                                        01632062
               MOVE 02 TO CIC0014I-TRNPWD-IND                           01633062
            END-IF                                                      01634062
            MOVE CIC0014I-REC  TO SD-SRV-INPUT-DATA                     01640054
            EXEC CICS                                                   01650054
                 LINK                                                   01660054
                 PROGRAM(WS-PGM-SRV-DRIVER)                             01670054
                 COMMAREA(WS-SRV-COMMAREA)                              01680054
                 RESP(WS-RESP-CODE)                                     01690054
            END-EXEC                                                    01700054
            EVALUATE WS-RESP-CODE                                       01701058
                WHEN DFHRESP(NORMAL)                                    01710054
                     IF SD-RESP-CODE EQUAL ZEROS                        01720054
                       MOVE 'NEED TRANSACTION PASSWORD SET SUCCESSFULLY'01730061
                          TO MSGO                                       01740054
                        MOVE 002 TO LK-OPTION                           01750054
                     ELSE                                               01760054
                        MOVE SD-RESP-ADDITIONAL TO MSGO                 01770054
                     END-IF                                             01780054
                WHEN OTHER                                              01790054
                     MOVE 'SERVICE PROGRAM ERROR' TO MSGO               01800054
            END-EVALUATE                                                01810054
            SET WS-MAP-DATAONLY TO TRUE                                 01820054
            PERFORM 3030-SEND-MAP                                       01830054
               THRU 3030-SEND-MAP-EXIT                                  01840054
            .                                                           01850054
      *                                                                 01860054
       1030-UPDATE-CARD-EXIT.                                           01870054
                  EXIT.                                                 01880054
      *                                                                 01890054
       2000-PRE-PROCESSING.                                             01940000
      *                                                                 01950000
       2000-PRE-PROCESSING-EXIT.                                        01960000
            EXIT.                                                       01970000
      *                                                                 01980000
       3000-MAIN-PROCESS.                                               01990000
            EVALUATE EIBAID ALSO TRUE                                   02000000
                WHEN DFHPF1 ALSO LK-OPTION = 001                        02010054
                     MOVE 'Y' TO WS-FIRST-SEND OF WS-COMMAREA-CS08      02011054
                     MOVE 001 TO WS-OPTION OF WS-COMMAREA-CS08          02012054
                   MOVE LK-CARD-NUMB TO WS-CARD-NUMB OF WS-COMMAREA-CS0802013054
                     MOVE LK-CUST-ID-NUMB TO WS-CUST-ID-NUMB OF         02014054
                          WS-COMMAREA-CS08                              02015054
                     EXEC CICS                                          02020000
                          XCTL PROGRAM('CIOCCS08')                      02030054
                          COMMAREA(WS-COMMAREA-CS08)                    02031054
                               RESP(WS-RESP-CODE)                       02040000
                     END-EXEC                                           02050000
                     IF WS-RESP-CODE NOT = DFHRESP(NORMAL)              02060000
                        MOVE 'PROGRAM CIOCCS08 IS NOT AVAILABLE'        02070054
                                TO MSGO                                 02080000
                        SET WS-MAP-DATAONLY TO TRUE                     02090000
                        PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT   02100000
                     END-IF                                             02110000
                WHEN DFHPF1 ALSO LK-OPTION = 002                        02111054
                      INITIALIZE WS-COMMAREA-CS05                       02112063
                      MOVE 'Y' TO WS-FIRST-SEND OF WS-COMMAREA-CS05     02113063
                      MOVE '004' TO WS-OPTION OF WS-COMMAREA-CS05       02114063
                   MOVE LK-CARD-NUMB TO WS-CARD-NUMB OF WS-COMMAREA-CS0502115063
                      EXEC CICS                                         02116063
                           XCTL PROGRAM('CIOCCS05')                     02117063
                                RESP(WS-RESP-CODE)                      02118063
                                COMMAREA(WS-COMMAREA-CS05)              02119063
                      END-EXEC                                          02119163
                      IF WS-RESP-CODE NOT = DFHRESP(NORMAL)             02119263
                         MOVE 'PROGRAM CIOCCS05 IS NOT AVAILABLE'       02119363
                                 TO MSGO                                02119463
                         SET WS-MAP-DATAONLY TO TRUE                    02119563
                         PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT  02119663
                      END-IF                                            02119763
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
                WHEN DFHENTER ALSO LK-OPTION = 001                      02340054
                     IF WS-RESP-CODE NOT EQUAL DFHRESP(NORMAL)          02350054
                        MOVE 'INVALID REQUEST, CHECK YOUR INPUT.'       02360054
                             TO MSGO                                    02370054
                        SET WS-MAP-DATAONLY TO TRUE                     02380054
                        PERFORM 3030-SEND-MAP                           02390054
                           THRU 3030-SEND-MAP-EXIT                      02400054
                     ELSE                                               02410054
                        PERFORM 3010-CHECK-INPUT                        02420054
                           THRU 3010-CHECK-INPUT-EXIT                   02430054
                       PERFORM 1020-GET-EXISTING-CARD                   02440059
                          THRU 1020-GET-EXISTING-CARD-EXIT              02450054
                     END-IF                                             02460054
                 WHEN DFHENTER ALSO LK-OPTION = 002                     02470054
                      INITIALIZE WS-COMMAREA-CS05                       02480061
                      MOVE 'Y' TO WS-FIRST-SEND OF WS-COMMAREA-CS05     02490061
                      MOVE '004' TO WS-OPTION OF WS-COMMAREA-CS05       02500061
                   MOVE LK-CARD-NUMB TO WS-CARD-NUMB OF WS-COMMAREA-CS0502510061
                      EXEC CICS                                         02540054
                           XCTL PROGRAM('CIOCCS05')                     02550061
                                RESP(WS-RESP-CODE)                      02560054
                                COMMAREA(WS-COMMAREA-CS05)              02570061
                      END-EXEC                                          02580054
                      IF WS-RESP-CODE NOT = DFHRESP(NORMAL)             02590054
                         MOVE 'PROGRAM CIOCCS05 IS NOT AVAILABLE'       02600063
                                 TO MSGO                                02610054
                         SET WS-MAP-DATAONLY TO TRUE                    02620054
                         PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT  02630054
                      END-IF                                            02640054
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
            END-IF                                                      03120051
                IF (NTPASI NOT = 'Y' AND NTPASI NOT = 'N')              03130051
                   MOVE 'NEED TRANSACTION PASSWORD MUST BE Y/N' TO MSGO 03140045
                   SET WS-MAP-DATAONLY TO TRUE                          03150045
                   PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT        03160045
                END-IF                                                  03170045
            .                                                           03536039
      *                                                                 03540000
       3010-CHECK-INPUT-EXIT.                                           03550000
            EXIT.                                                       03560000
      *                                                                 03570000
      *                                                                 03890000
       3030-SEND-MAP.                                                   03900000
            IF LK-OPTION = 001                                          03901054
               MOVE 'PF1=RETURN PF3=EXIT ENTER=PROCESS' TO KEYO         03903254
            ELSE                                                        03903354
               MOVE 'PF1=RETURN PF3=EXIT ENTER=ACTIVATE CARD' TO KEYO   03903454
            END-IF                                                      03903654
            PERFORM 1010-ASK-TIME-DATE                                  04040000
               THRU 1010-ASK-TIME-DATE-EXIT                             04050000
            EVALUATE TRUE                                               04060000
                WHEN WS-MAP-ERASE                                       04070000
                     EXEC CICS SEND                                     04080000
                          MAP('CICS09')                                 04090045
                          MAPSET('CICS09')                              04100045
                          FROM(CICS09O)                                 04110045
                          ERASE                                         04120000
                     END-EXEC                                           04130000
                WHEN WS-MAP-DATAONLY                                    04140000
                     EXEC CICS SEND                                     04150000
                          MAP('CICS09')                                 04160045
                          MAPSET('CICS09')                              04170045
                          FROM(CICS09O)                                 04180045
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
            MOVE LK-CUST-ID-NUMB TO WS-CUST-ID-NUMB OF WS-COMMAREA      05511054
            EXEC CICS RETURN TRANSID('CIB9')                            05520045
                      COMMAREA(WS-COMMAREA)                             05530000
            END-EXEC                                                    05540000
            .                                                           05550000
       5020-RETURN-TRANS-EXIT.                                          05560000
            EXIT.                                                       05570000
