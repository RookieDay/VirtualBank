       IDENTIFICATION DIVISION.                                         00010001
       PROGRAM-ID. CIOCCA08.                                            00020032
      ***************************************************************** 00030001
      * CIOCCIMN - CLIENT PROGRAM                                       00040001
      *                                                                 00050001
      * CREDIT ISSUANCE MAIN MENU                                       00060001
      *                                                                 00070001
      ***************************************************************** 00080001
      *                         VERSION HISTORY                         00090001
      *---------------------------------------------------------------- 00100001
      *DATE/TIME ³   AUTHOR   ³ DESCRIPTION                             00110001
      *---------------------------------------------------------------- 00120001
      *2015-01-06    KEVIN      INITIAL VERSION                         00130001
      ***************************************************************** 00140001
       ENVIRONMENT DIVISION.                                            00150001
       DATA DIVISION.                                                   00160001
       WORKING-STORAGE SECTION.                                         00170001
      *                                                                 00180001
       77 WS-BEGIN              PIC X(17) VALUE 'CIOCCA08 WS BEGIN'.    00190029
       01 WS-VAR.                                                       00200001
          05 WS-GETTIME         PIC X(20).                              00210001
          05 WS-DATEOUT         PIC X(10).                              00220001
          05 WS-TIMEOUT         PIC X(8).                               00230001
          05 WS-RESP-CODE       PIC S9(8) COMP.                         00240001
          05 WS-MESSAGE         PIC X(40).                              00250001
          05 WS-ENTER-FLAG      PIC X(1).                               00260001
          05 WS-TRANSID         PIC X(4).                               00270001
       01 WS-MAP-OPTION         PIC X(1).                               00280001
          88 WS-MAP-ERASE       VALUE '0'.                              00290001
          88 WS-MAP-DATAONLY    VALUE '1'.                              00300001
      *                                                                 00310001
      *SCREEN HANDLER                                                   00320001
       COPY SD11WS.                                                     00330001
      * SYMBOLIC MAP                                                    00340001
       COPY CICA08.                                                     00350029
      *MAP CONTROL                                                      00360001
       COPY DFHBMSCA.                                                   00370001
      *CICS FUNCTION KEYS                                               00380001
       COPY DFHAID.                                                     00390001
      *CIMENU                                                           00400001
       COPY CIMENU.                                                     00410001
      *                                                                 00420001
       COPY CIC0007I.                                                   00421014
       COPY CIC0007O.                                                   00422014
      *                                                                 00423014
       COPY CIC0006I.                                                   00424014
       COPY CIC0006O.                                                   00425014
      *                                                                 00426014
       01 WS-SRV-COMMAREA.                                              00430001
      *SERVICE REQUEST/RESPONSE COMMAREA                                00440001
       COPY SD01WS.                                                     00450001
      *                                                                 00460001
       01 WS-COMMAREA.                                                  00470001
          05 WS-FIRST-SEND      PIC X(1).                               00480001
          05 WS-OPTION          PIC 9(3).                               00490001
          05 WS-APPL-ID         PIC 9(13).                              00491010
       77 WS-END                PIC X(15) VALUE 'CIOCCA08 WS END'.      00500029
      *                                                                 00510001
       LINKAGE SECTION.                                                 00520001
       01 DFHCOMMAREA.                                                  00530001
          05 LK-FIRST-SEND      PIC X(1).                               00532003
          05 LK-OPTION          PIC 9(3).                               00533007
          05 LK-APPL-ID         PIC 9(13).                              00534010
      *COMMON CICS SCREEN HANDLE VARIABLES                              00540001
       COPY SD00WS.                                                     00550001
      *                                                                 00560001
       PROCEDURE DIVISION.                                              00570001
       0000-MAINLINE.                                                   00580001
      *                                                                 00590001
            PERFORM 1000-INIT                                           00600001
               THRU 1000-INIT-EXIT                                      00610001
      *                                                                 00620001
            PERFORM 2000-PRE-PROCESSING                                 00630001
               THRU 2000-PRE-PROCESSING-EXIT                            00640001
      *                                                                 00650001
            PERFORM 3000-MAIN-PROCESS                                   00660001
               THRU 3000-MAIN-PROCESS-EXIT                              00670001
      *                                                                 00680001
            PERFORM 4000-POST-PROCESSING                                00690001
               THRU 4000-POST-PROCESSING-EXIT                           00700001
      *                                                                 00710001
            PERFORM 5000-CLEAN-UP                                       00720001
               THRU 5000-CLEAN-UP-EXIT                                  00730001
            .                                                           00740001
      *                                                                 00750001
       0000-EXIT.                                                       00760001
            EXIT.                                                       00770001
      *                                                                 00780001
       1000-INIT.                                                       00790001
            IF LK-FIRST-SEND = 'Y'                                      00800003
               MOVE LOW-VALUES TO CICA08O                               00810029
               EVALUATE LK-OPTION                                       00811019
                   WHEN 004                                             00812035
                   WHEN 003                                             00813035
                        PERFORM 1020-GET-EXISTING-APPL                  00820020
                           THRU 1020-GET-EXISTING-APPL-EXIT             00830020
                        PERFORM 1040-ATTR-PROTECT                       00831020
                           THRU 1040-ATTR-PROTECT-EXIT                  00832020
                   WHEN OTHER                                           00844020
                        CONTINUE                                        00845020
               END-EVALUATE                                             00848020
               SET WS-MAP-ERASE TO TRUE                                 00849020
               PERFORM 3030-SEND-MAP                                    00849120
                  THRU 3030-SEND-MAP-EXIT                               00849220
      * NOT FIRST SHOW                                                  00850001
            ELSE                                                        00860001
                  MOVE LOW-VALUES TO CICA08I                            00880029
                  EXEC CICS RECEIVE MAP('CICA08')                       00890029
                                   MAPSET('CICA08')                     00900029
                                   INTO(CICA08I)                        00910029
                                   RESP(WS-RESP-CODE)                   00920001
                  END-EXEC                                              00930001
            END-IF                                                      00950001
            .                                                           00960001
       1000-INIT-EXIT.                                                  00970001
            EXIT.                                                       00980001
      *                                                                 00990001
       1010-ASK-TIME-DATE.                                              01000001
      *                                                                 01010001
            EXEC CICS                                                   01020001
                 ASKTIME                                                01030001
                 ABSTIME(WS-GETTIME)                                    01040001
            END-EXEC                                                    01050001
            EXEC CICS                                                   01060001
                 FORMATTIME                                             01070001
                 ABSTIME(WS-GETTIME)                                    01080001
                 DATESEP('/')                                           01090001
                 YYYYMMDD(WS-DATEOUT)                                   01100001
            END-EXEC                                                    01110001
            EXEC CICS                                                   01120001
                 FORMATTIME                                             01130001
                 ABSTIME(WS-GETTIME)                                    01140001
                 TIMESEP                                                01150001
                 TIME(WS-TIMEOUT)                                       01160001
            END-EXEC                                                    01170001
            MOVE WS-DATEOUT TO SYSDO                                    01180001
            MOVE WS-TIMEOUT TO SYSTO                                    01190001
            .                                                           01200001
      *                                                                 01210001
       1010-ASK-TIME-DATE-EXIT.                                         01220001
            EXIT.                                                       01230001
      *                                                                 01240001
       1020-GET-EXISTING-APPL.                                          01241009
            INITIALIZE SDCA-SERVICE-COMMAREA                            01242009
            MOVE 'VBS.CI.APPLICAN.INQ' TO SD-SRV-NAME                   01243009
            INITIALIZE CIC0007I-REC                                     01244009
            MOVE LK-APPL-ID TO CIC0007I-ID                              01245012
            MOVE CIC0007I-REC TO SD-SRV-INPUT-DATA                      01246020
            EXEC CICS                                                   01247009
                 LINK                                                   01248009
                 PROGRAM(WS-PGM-SRV-DRIVER)                             01249009
                 COMMAREA(WS-SRV-COMMAREA)                              01249109
                 RESP(WS-RESP-CODE)                                     01249209
            END-EXEC                                                    01249309
            EVALUATE WS-RESP-CODE                                       01249409
                WHEN DFHRESP(NORMAL)                                    01249509
                     IF SD-RESP-CODE EQUAL ZEROS                        01249609
                        PERFORM 1030-POPULATE-APPL                      01249709
                           THRU 1030-POPULATE-APPL-EXIT                 01249809
                     END-IF                                             01249909
            END-EVALUATE                                                01250009
            .                                                           01250109
      *                                                                 01250209
       1020-GET-EXISTING-APPL-EXIT.                                     01250309
            EXIT.                                                       01250409
      *                                                                 01250509
       1030-POPULATE-APPL.                                              01250617
            INITIALIZE CIC0007O-REC                                     01250716
            MOVE SD-SRV-OUTPUT-DATA            TO CIC0007O-REC          01250813
            MOVE CIC0007O-REVIEW-ID            TO REVIDO                01257229
            MOVE CIC0007O-REVIEW-DATE          TO REVDTO                01257329
            MOVE CIC0007O-REVIEW-RESULT        TO REVRTO                01257429
            MOVE CIC0007O-REVIEW-REFUSE-REASON TO REVRRO                01257529
            MOVE CIC0007O-REVIEW-COMMENT       TO COMMTO                01257629
            .                                                           01258909
      *                                                                 01259009
       1030-POPULATE-APPL-EXIT.                                         01259109
            EXIT.                                                       01259209
      *                                                                 01259310
       1040-ATTR-PROTECT.                                               01260017
            MOVE ATTR-PROT-SKIP-MDT       TO REVIDA                     01260730
            MOVE ATTR-PROT-SKIP-MDT       TO REVDTA                     01260830
            MOVE ATTR-PROT-SKIP-MDT       TO REVRTA                     01260930
            MOVE ATTR-PROT-SKIP-MDT       TO REVRRA                     01261030
            MOVE ATTR-PROT-SKIP-MDT       TO COMMTA                     01261118
            .                                                           01263710
      *                                                                 01263810
       1040-ATTR-PROTECT-EXIT.                                          01263911
            EXIT.                                                       01264010
      *                                                                 01264110
       2000-PRE-PROCESSING.                                             01264201
      *                                                                 01265001
       2000-PRE-PROCESSING-EXIT.                                        01270001
            EXIT.                                                       01280001
      *                                                                 01290001
       3000-MAIN-PROCESS.                                               01300001
            EVALUATE EIBAID ALSO TRUE                                   01310020
                WHEN DFHPF1 ALSO ANY                                    01320020
                     EXEC CICS                                          01330001
                          XCTL PROGRAM('CIOCCIMN')                      01340001
                               RESP(WS-RESP-CODE)                       01350001
                     END-EXEC                                           01360001
                     IF WS-RESP-CODE NOT = DFHRESP(NORMAL)              01370001
                        MOVE 'PROGRAM CIOCCIMN IS NOT AVAILABLE'        01380001
                                TO MSGO                                 01390001
                        SET WS-MAP-DATAONLY TO TRUE                     01400001
                        PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT   01410001
                     END-IF                                             01420001
                WHEN DFHPF3 ALSO ANY                                    01430020
                     MOVE 'THANK YOU FOR USING THE SYSTEM' TO WS-MESSAGE01440001
                     EXEC CICS                                          01450001
                          SEND CONTROL                                  01460001
                          CURSOR                                        01470001
                          ERASE                                         01480001
                          FREEKB                                        01490001
                          ALARM                                         01500001
                     END-EXEC                                           01510001
                     EXEC CICS                                          01520001
                          SEND FROM(WS-MESSAGE)                         01530001
                     END-EXEC                                           01540001
                     PERFORM 5010-RETURN THRU 5010-RETURN-EXIT          01550001
                WHEN DFHCLEAR ALSO ANY                                  01560020
                     EXEC CICS                                          01570001
                           SEND CONTROL                                 01580001
                           CURSOR                                       01590001
                           ERASE                                        01600001
                           FREEKB                                       01610001
                           ALARM                                        01620001
                     END-EXEC                                           01630001
                     PERFORM 5010-RETURN THRU 5010-RETURN-EXIT          01640001
                WHEN DFHPF7 ALSO ANY                                    01641020
                     INITIALIZE WS-COMMAREA                             01641121
                     MOVE 'Y' TO WS-FIRST-SEND                          01641221
                     MOVE LK-OPTION TO WS-OPTION                        01641321
                     MOVE LK-APPL-ID TO WS-APPL-ID                      01641421
                     EXEC CICS                                          01642020
                          XCTL PROGRAM('CIOCCA04')                      01643020
                               RESP(WS-RESP-CODE)                       01644020
                               COMMAREA(WS-COMMAREA)                    01644121
                     END-EXEC                                           01645020
                     IF WS-RESP-CODE NOT = DFHRESP(NORMAL)              01646020
                        MOVE 'PROGRAM CIOCCA04 IS NOT AVAILABLE'        01647020
                                TO MSGO                                 01648020
                        SET WS-MAP-DATAONLY TO TRUE                     01649020
                        PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT   01649120
                     END-IF                                             01649220
                WHEN DFHPF8 ALSO LK-OPTION = 004 OR 003                 01649334
                     INITIALIZE WS-COMMAREA                             01649421
                     MOVE 'Y' TO WS-FIRST-SEND                          01649521
                     MOVE LK-OPTION TO WS-OPTION                        01649621
                     MOVE LK-APPL-ID TO WS-APPL-ID                      01649721
                     EXEC CICS                                          01649820
                          XCTL PROGRAM('CIOCCA05')                      01649930
                               RESP(WS-RESP-CODE)                       01650020
                               COMMAREA(WS-COMMAREA)                    01650121
                     END-EXEC                                           01650220
                     IF WS-RESP-CODE NOT = DFHRESP(NORMAL)              01650320
                        MOVE 'PROGRAM CIOCCA05 IS NOT AVAILABLE'        01650445
                                TO MSGO                                 01650520
                        SET WS-MAP-DATAONLY TO TRUE                     01650620
                        PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT   01650720
                     END-IF                                             01650820
                WHEN DFHPF9 ALSO LK-OPTION = 002                        01651030
                     MOVE LOW-VALUES TO CICA08O                         01660032
                     SET WS-MAP-ERASE TO TRUE                           01670001
                     PERFORM 3030-SEND-MAP                              01680001
                        THRU 3030-SEND-MAP-EXIT                         01690001
                WHEN DFHENTER ALSO LK-OPTION = 002                      01700030
                     IF WS-RESP-CODE NOT EQUAL DFHRESP(NORMAL)          01710008
                        MOVE 'INVALID REQUEST, CHECK YOUR INPUT.'       01720008
                             TO MSGO                                    01730008
                        SET WS-MAP-DATAONLY TO TRUE                     01740008
                        PERFORM 3030-SEND-MAP                           01750008
                           THRU 3030-SEND-MAP-EXIT                      01760008
                     ELSE                                               01770008
                        PERFORM 3010-CHECK-INPUT                        01780008
                           THRU 3010-CHECK-INPUT-EXIT                   01790008
                        PERFORM 3020-APPLUPDATE                         01800021
                           THRU 3020-APPLUPDATE-EXIT                    01810021
                     END-IF                                             01820008
                WHEN OTHER                                              01830008
                     MOVE 'INVALID KEY PRESSED!' TO MSGO                01840008
                     SET WS-MAP-DATAONLY TO TRUE                        01850008
                     PERFORM 3030-SEND-MAP                              01860008
                        THRU 3030-SEND-MAP-EXIT                         01870008
            END-EVALUATE                                                01880001
            .                                                           01890001
      *                                                                 01891017
       3000-MAIN-PROCESS-EXIT.                                          01900001
            EXIT.                                                       01910001
      *                                                                 01920001
       3010-CHECK-INPUT.                                                01930001
            IF COMMUL NOT = 0                                           01940011
               INITIALIZE CIMENU-REC                                    01950011
               MOVE COMMUI TO CIMENU-TRANSID                            01960011
               EXEC CICS READ                                           01970011
                    FILE('CIMENU')                                      01980011
                    INTO(CIMENU-REC)                                    01990011
                    RIDFLD(CIMENU-TRANSID)                              02000011
                    RESP(WS-RESP-CODE)                                  02010011
               END-EXEC                                                 02020011
               EVALUATE WS-RESP-CODE                                    02030011
                   WHEN DFHRESP(NORMAL)                                 02040011
                        EXEC CICS                                       02050011
                             XCTL PROGRAM(CIMENU-PGM)                   02060011
                             COMMAREA(CIMENU-TRANSID)                   02070050
                             RESP(WS-RESP-CODE)                         02080011
                        END-EXEC                                        02090011
                        IF WS-RESP-CODE NOT = DFHRESP(NORMAL)           02100011
                        STRING 'PROGRAM ' DELIMITED BY SIZE             02110011
                               CIMENU-PGM DELIMITED BY SPACE            02120011
                               ' IS NOT AVAILABLE' DELIMITED BY SIZE    02130011
                               INTO MSGO                                02140011
                           SET WS-MAP-DATAONLY TO TRUE                  02141011
                           PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT02142011
                        END-IF                                          02143011
                   WHEN DFHRESP(NOTFND)                                 02144011
                        MOVE 'INVALID TRANSATION ID!' TO MSGO           02145011
                        SET WS-MAP-DATAONLY TO TRUE                     02146011
                        PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT   02147011
                   WHEN OTHER                                           02148011
                        MOVE 'CIMENU FILE ERROR!' TO MSGO               02149011
                        SET WS-MAP-DATAONLY TO TRUE                     02149111
                        PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT   02149211
               END-EVALUATE                                             02149311
            END-IF                                                      02149411
                IF (REVIDL = 0 OR REVDTL = 0 OR REVRTL = 0              02149731
                     OR REVRRL = 0 OR COMMTL = 0)                       02149831
                   MOVE 'FIELDS CAN NOT BE EMPTY' TO MSGO               02150419
                   SET WS-MAP-DATAONLY TO TRUE                          02150519
                   PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT        02150619
                END-IF                                                  02150719
                IF (REVRTI NOT = 001 AND REVRTI NOT = 002               02150846
                    AND REVRTI NOT = 003)                               02150946
                   MOVE 'INVAILD REVIEW RESULT' TO MSGO                 02151031
                   SET WS-MAP-DATAONLY TO TRUE                          02151119
                   PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT        02151219
                END-IF                                                  02151319
                IF (REVRTI = 001 AND REVRRI NOT = 001)                  02151448
                   MOVE 'WRONG REFUSE REASON,REFUSE REASON MUST BE 001' 02151549
                       TO MSGO                                          02151649
                   SET WS-MAP-DATAONLY TO TRUE                          02151748
                   PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT        02151848
                END-IF                                                  02151948
                IF (REVRTI NOT = 001 AND REVRRI = 001)                  02152048
                   MOVE 'WRONG REFUSE REASON,REFUSE REASON CAN NOT BE 0 02152149
      -                   '01' TO MSGO                                  02152249
                   SET WS-MAP-DATAONLY TO TRUE                          02152348
                   PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT        02152448
                END-IF                                                  02152548
                IF (REVRRI NOT = 001 AND REVRRI NOT = 002 AND           02152633
                    REVRRI NOT = 003 AND REVRRI NOT = 004 AND           02152733
                    REVRRI NOT = 005 AND REVRRI NOT = 006)              02152832
                   MOVE 'INVAILD REFUSE REASON' TO MSGO                 02152931
                   SET WS-MAP-DATAONLY TO TRUE                          02153019
                   PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT        02153119
                END-IF                                                  02154019
            .                                                           02155001
      *                                                                 02160001
       3010-CHECK-INPUT-EXIT.                                           02170001
            EXIT.                                                       02180001
      *                                                                 02190001
       3020-APPLUPDATE.                                                 02200021
            INITIALIZE SDCA-SERVICE-COMMAREA                            02210026
            MOVE 'VBS.CI.APPLICAN.INQ' TO SD-SRV-NAME                   02220026
            INITIALIZE CIC0007I-REC                                     02230026
            MOVE LK-APPL-ID TO CIC0007I-ID                              02240026
            MOVE CIC0007I-REC TO SD-SRV-INPUT-DATA                      02250026
            EXEC CICS                                                   02260026
                 LINK                                                   02270026
                 PROGRAM(WS-PGM-SRV-DRIVER)                             02280026
                 COMMAREA(WS-SRV-COMMAREA)                              02290026
                 RESP(WS-RESP-CODE)                                     02300026
            END-EXEC                                                    02310026
            EVALUATE WS-RESP-CODE                                       02320026
                WHEN DFHRESP(NORMAL)                                    02330026
                     IF SD-RESP-CODE EQUAL ZEROS                        02340026
                        INITIALIZE CIC0007O-REC                         02350026
                        MOVE SD-SRV-OUTPUT-DATA TO CIC0007O-REC         02360026
                     ELSE                                               02361026
                        MOVE 'APPLICATION READ ERROR' TO MSGO           02362026
                        SET WS-MAP-DATAONLY TO TRUE                     02363026
                        PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT   02364026
                     END-IF                                             02370026
                WHEN OTHER                                              02371026
                     MOVE 'SERVICE PROGRAM ERROR' TO MSGO               02372026
                     SET WS-MAP-DATAONLY TO TRUE                        02373026
                     PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT      02374026
            END-EVALUATE                                                02380026
      *                                                                 02390026
            INITIALIZE SDCA-SERVICE-COMMAREA                            02490026
            MOVE 'VBS.CI.APPLICAN.UPD' TO SD-SRV-NAME                   02500026
            INITIALIZE CIC0006I-REC                                     02510026
            MOVE CIC0007O-REC TO CIC0006I-REC                           02511026
            PERFORM 3040-CHANGE-REVIEW                                  02520030
               THRU 3040-CHANGE-REVIEW-EXIT                             02530030
            MOVE CIC0006I-REC  TO SD-SRV-INPUT-DATA                     02540026
            EXEC CICS                                                   02550026
                 LINK                                                   02560026
                 PROGRAM(WS-PGM-SRV-DRIVER)                             02570026
                 COMMAREA(WS-SRV-COMMAREA)                              02580026
                 RESP(WS-RESP-CODE)                                     02590026
            END-EXEC                                                    02600026
            EVALUATE WS-RESP-CODE                                       02610026
                WHEN DFHRESP(NORMAL)                                    02620026
                     IF SD-RESP-CODE EQUAL ZEROS                        02630026
                        MOVE 'APPLICATION REVIEW SUCCESSFUL'            02640036
                          TO MSGO                                       02640126
                        MOVE 'T' TO WS-FIRST-SEND                       02640227
                        MOVE 'PRESS ANY KEY TO RETURN' TO KEYO          02640328
                     ELSE                                               02641026
                        MOVE SD-RESP-ADDITIONAL TO MSGO                 02642026
                     END-IF                                             02643026
                WHEN OTHER                                              02644026
                     MOVE 'SERVICE PROGRAM ERROR' TO MSGO               02645026
            END-EVALUATE                                                02646026
            SET WS-MAP-DATAONLY TO TRUE                                 02647026
            PERFORM 3030-SEND-MAP                                       02648026
               THRU 3030-SEND-MAP-EXIT                                  02649026
            .                                                           02650001
      *                                                                 02660001
       3020-APPLUPDATE-EXIT.                                            02670021
            EXIT.                                                       02680001
      *                                                                 02690001
       3030-SEND-MAP.                                                   02700001
            PERFORM 1010-ASK-TIME-DATE                                  02710001
               THRU 1010-ASK-TIME-DATE-EXIT                             02720001
               EVALUATE LK-OPTION                                       02720137
                    WHEN 002                                            02720237
                    MOVE 'PF1=MAIN MENU PF3=EXIT PF7=PREV PF9=REFRESH   02720337
      -                   'ENTER=PROCESS' TO KEYO                       02720437
                    MOVE WS-DATEOUT TO REVDTO                           02720540
                    MOVE ATTR-PROT-SKIP-MDT TO REVDTA                   02720640
                    WHEN 003                                            02720737
                    WHEN 004                                            02720837
                    MOVE 'PF1=MAIN MENU PF3=EXIT PF7=PREV PF8=NEXT'     02720937
                    TO KEYO                                             02721037
               END-EVALUATE                                             02721137
               IF WS-FIRST-SEND = 'T'                                   02721237
               MOVE 'PRESS ANY KEY TO RETURN ' TO KEYO                  02721337
               END-IF                                                   02721437
            EVALUATE TRUE                                               02721537
                WHEN WS-MAP-ERASE                                       02740001
                     EXEC CICS SEND                                     02750001
                          MAP('CICA08')                                 02760030
                          MAPSET('CICA08')                              02770030
                          FROM(CICA08O)                                 02780030
                          ERASE                                         02790001
                     END-EXEC                                           02800001
                WHEN WS-MAP-DATAONLY                                    02810001
                     EXEC CICS SEND                                     02820001
                          MAP('CICA08')                                 02830030
                          MAPSET('CICA08')                              02840030
                          FROM(CICA08O)                                 02850030
                          DATAONLY                                      02860001
                     END-EXEC                                           02870001
            END-EVALUATE                                                02880001
            PERFORM 5020-RETURN-TRANS THRU 5020-RETURN-TRANS-EXIT       02900001
            .                                                           02910001
      *                                                                 02920001
       3030-SEND-MAP-EXIT.                                              02930001
            EXIT.                                                       02940001
      *                                                                 02950001
       3040-CHANGE-REVIEW.                                              02951030
            EVALUATE REVRTI                                             02951144
                WHEN 001                                                02951241
                     MOVE '003'   TO   CIC0006I-STATUS                  02951347
                WHEN 002                                                02951441
                     MOVE '013'   TO   CIC0006I-STATUS                  02951547
                WHEN 003                                                02951641
                     MOVE '010'   TO   CIC0006I-STATUS                  02951747
            END-EVALUATE                                                02951841
            MOVE REVIDI    TO        CIC0006I-REVIEW-ID                 02952030
            MOVE REVDTI    TO        CIC0006I-REVIEW-DATE               02953030
            MOVE REVRTI    TO        CIC0006I-REVIEW-RESULT             02954030
            MOVE REVRRI    TO        CIC0006I-REVIEW-REFUSE-REASON      02955030
            MOVE COMMTI    TO        CIC0006I-REVIEW-COMMENT            02956030
            .                                                           02961313
      *                                                                 02961413
       3040-CHANGE-REVIEW-EXIT.                                         02961530
            EXIT.                                                       02961613
      *                                                                 02961713
       4000-POST-PROCESSING.                                            02962001
      *                                                                 02970001
       4000-POST-PROCESSING-EXIT.                                       02980001
            EXIT.                                                       02990001
      *                                                                 03000001
       5000-CLEAN-UP.                                                   03010001
            PERFORM 5010-RETURN                                         03020001
               THRU 5010-RETURN-EXIT                                    03030001
            .                                                           03040001
      *                                                                 03050001
       5000-CLEAN-UP-EXIT.                                              03060001
            EXIT.                                                       03070001
      *                                                                 03080001
       5010-RETURN.                                                     03090001
            EXEC CICS RETURN END-EXEC                                   03100001
            .                                                           03110001
       5010-RETURN-EXIT.                                                03120001
            EXIT.                                                       03130001
      *                                                                 03140001
       5020-RETURN-TRANS.                                               03150001
            IF WS-FIRST-SEND = 'T'                                      03150127
               EXEC CICS RETURN TRANSID('CICA')                         03150227
               END-EXEC                                                 03150327
            END-IF                                                      03150427
            INITIALIZE WS-COMMAREA                                      03151013
            MOVE 'N' TO WS-FIRST-SEND                                   03152013
            MOVE LK-OPTION TO WS-OPTION                                 03153013
            MOVE LK-APPL-ID TO WS-APPL-ID                               03154022
            EXEC CICS RETURN TRANSID('CIA8')                            03160030
                      COMMAREA(WS-COMMAREA)                             03170013
            END-EXEC                                                    03180001
            .                                                           03190001
       5020-RETURN-TRANS-EXIT.                                          03200001
            EXIT.                                                       03210001
