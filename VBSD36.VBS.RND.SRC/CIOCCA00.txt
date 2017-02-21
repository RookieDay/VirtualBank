       IDENTIFICATION DIVISION.                                         00010001
       PROGRAM-ID. CIOCCA00.                                            00020010
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
       77 WS-BEGIN              PIC X(17) VALUE 'CIOCCAMN WS BEGIN'.    00190002
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
       COPY CICA00.                                                     00350002
      *MAP CONTROL                                                      00360001
       COPY DFHBMSCA.                                                   00370001
      *CICS FUNCTION KEYS                                               00380001
       COPY DFHAID.                                                     00390001
      *CIMENU                                                           00400001
       COPY CIMENU.                                                     00410001
      *                                                                 00420001
       01 WS-SRV-COMMAREA.                                              00430001
      *SERVICE REQUEST/RESPONSE COMMAREA                                00440001
       COPY SD01WS.                                                     00450001
      *                                                                 00451007
       01 WS-COMMAREA.                                                  00452007
          05 WS-FIRST-SEND      PIC X(1).                               00453007
          05 WS-OPTION          PIC 9(3).                               00454007
       77 WS-END                PIC X(15) VALUE 'CIOCCAMN WS END'.      00460002
      *                                                                 00470001
       LINKAGE SECTION.                                                 00480001
       01 DFHCOMMAREA.                                                  00490001
      *COMMON CICS SCREEN HANDLE VARIABLES                              00500001
       COPY SD00WS.                                                     00510001
      *                                                                 00520001
       PROCEDURE DIVISION.                                              00530001
       0000-MAINLINE.                                                   00540001
      *                                                                 00550001
            PERFORM 1000-INIT                                           00560001
               THRU 1000-INIT-EXIT                                      00570001
      *                                                                 00580001
            PERFORM 2000-PRE-PROCESSING                                 00590001
               THRU 2000-PRE-PROCESSING-EXIT                            00600001
      *                                                                 00610001
            PERFORM 3000-MAIN-PROCESS                                   00620001
               THRU 3000-MAIN-PROCESS-EXIT                              00630001
      *                                                                 00640001
            PERFORM 4000-POST-PROCESSING                                00650001
               THRU 4000-POST-PROCESSING-EXIT                           00660001
      *                                                                 00670001
            PERFORM 5000-CLEAN-UP                                       00680001
               THRU 5000-CLEAN-UP-EXIT                                  00690001
            .                                                           00700001
      *                                                                 00710001
       0000-EXIT.                                                       00720001
            EXIT.                                                       00730001
      *                                                                 00740001
       1000-INIT.                                                       00750001
            IF EIBCALEN = 0                                             00760001
               MOVE LOW-VALUES TO CICA00O                               00770002
               SET WS-MAP-ERASE TO TRUE                                 00780001
               PERFORM 3030-SEND-MAP                                    00790001
                  THRU 3030-SEND-MAP-EXIT                               00800001
      * NOT FIRST SHOW                                                  00810001
            ELSE                                                        00820001
               IF SDCA-CICS-SECONDENTER                                 00830001
                  MOVE LOW-VALUES TO CICA00I                            00840002
                  EXEC CICS RECEIVE MAP('CICA00')                       00850002
                                   MAPSET('CICA00')                     00860002
                                   INTO(CICA00I)                        00870004
                                   RESP(WS-RESP-CODE)                   00880001
                  END-EXEC                                              00890001
               END-IF                                                   00900001
            END-IF                                                      00910001
            .                                                           00920001
       1000-INIT-EXIT.                                                  00930001
            EXIT.                                                       00940001
      *                                                                 00950001
       1010-ASK-TIME-DATE.                                              00960001
      *                                                                 00970001
            EXEC CICS                                                   00980001
                 ASKTIME                                                00990001
                 ABSTIME(WS-GETTIME)                                    01000001
            END-EXEC                                                    01010001
            EXEC CICS                                                   01020001
                 FORMATTIME                                             01030001
                 ABSTIME(WS-GETTIME)                                    01040001
                 DATESEP('/')                                           01050001
                 YYYYMMDD(WS-DATEOUT)                                   01060001
            END-EXEC                                                    01070001
            EXEC CICS                                                   01080001
                 FORMATTIME                                             01090001
                 ABSTIME(WS-GETTIME)                                    01100001
                 TIMESEP                                                01110001
                 TIME(WS-TIMEOUT)                                       01120001
            END-EXEC                                                    01130001
            MOVE WS-DATEOUT TO SYSDO                                    01140001
            MOVE WS-TIMEOUT TO SYSTO                                    01150001
            .                                                           01160001
      *                                                                 01170001
       1010-ASK-TIME-DATE-EXIT.                                         01180001
            EXIT.                                                       01190001
      *                                                                 01200001
       2000-PRE-PROCESSING.                                             01210001
      *                                                                 01220001
       2000-PRE-PROCESSING-EXIT.                                        01230001
            EXIT.                                                       01240001
      *                                                                 01250001
       3000-MAIN-PROCESS.                                               01260001
            EVALUATE EIBAID                                             01270001
                WHEN DFHPF1                                             01271005
                     EXEC CICS                                          01272005
                          XCTL PROGRAM('CIOCCIMN')                      01273011
                               RESP(WS-RESP-CODE)                       01274005
                     END-EXEC                                           01275005
                     IF WS-RESP-CODE NOT = DFHRESP(NORMAL)              01276005
                        MOVE 'PROGRAM CIOCCIMN IS NOT AVAILABLE'        01277012
                                TO MSGO                                 01279105
                        SET WS-MAP-DATAONLY TO TRUE                     01279205
                        PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT   01279305
                     END-IF                                             01279405
                WHEN DFHPF3                                             01280001
                     MOVE 'THANK YOU FOR USING THE SYSTEM' TO WS-MESSAGE01290001
                     EXEC CICS                                          01300001
                          SEND CONTROL                                  01310001
                          CURSOR                                        01320001
                          ERASE                                         01330001
                          FREEKB                                        01340001
                          ALARM                                         01350001
                     END-EXEC                                           01360001
                     EXEC CICS                                          01370001
                          SEND FROM(WS-MESSAGE)                         01380001
                     END-EXEC                                           01390001
                     PERFORM 5010-RETURN THRU 5010-RETURN-EXIT          01400001
                WHEN DFHCLEAR                                           01410001
                     EXEC CICS                                          01420001
                           SEND CONTROL                                 01430001
                           CURSOR                                       01440001
                           ERASE                                        01450001
                           FREEKB                                       01460001
                           ALARM                                        01470001
                     END-EXEC                                           01480001
                     PERFORM 5010-RETURN THRU 5010-RETURN-EXIT          01490001
                WHEN DFHPF9                                             01500001
                     MOVE LOW-VALUES TO CICA00O                         01510002
                     SET WS-MAP-ERASE TO TRUE                           01520001
                     PERFORM 3030-SEND-MAP                              01530001
                        THRU 3030-SEND-MAP-EXIT                         01540001
                 WHEN DFHENTER                                          01550001
                      IF WS-RESP-CODE NOT EQUAL DFHRESP(NORMAL)         01560001
                         MOVE 'INVALID REQUEST, CHECK YOUR INPUT.'      01570001
                              TO MSGO                                   01580001
                         SET WS-MAP-DATAONLY TO TRUE                    01590001
                         PERFORM 3030-SEND-MAP                          01600001
                            THRU 3030-SEND-MAP-EXIT                     01610001
                      ELSE                                              01620001
                         PERFORM 3010-CHECK-INPUT                       01630001
                            THRU 3010-CHECK-INPUT-EXIT                  01640001
                         PERFORM 3020-XCTL                              01650001
                            THRU 3020-XCTL-EXIT                         01660001
                      END-IF                                            01670001
                 WHEN OTHER                                             01680001
                      MOVE 'INVALID KEY PRESSED!' TO MSGO               01690001
                      SET WS-MAP-DATAONLY TO TRUE                       01700001
                      PERFORM 3030-SEND-MAP                             01710001
                         THRU 3030-SEND-MAP-EXIT                        01720001
            END-EVALUATE                                                01730001
            .                                                           01740001
       3000-MAIN-PROCESS-EXIT.                                          01750001
            EXIT.                                                       01760001
      *                                                                 01770001
       3010-CHECK-INPUT.                                                01780001
            INITIALIZE CIMENU-REC                                       01790001
            EVALUATE TRUE                                               01800001
                WHEN (COMMUL NOT = 0)                                   01810001
                     MOVE COMMUI TO CIMENU-TRANSID                      01820001
                WHEN (MENUL  NOT = 0)                                   01830001
                     MOVE MENUI  TO CIMENU-TRANSID                      01840001
                WHEN (OPT1L  NOT = 0 AND OPT1I = 'S')                   01850001
                     MOVE 'CI01' TO CIMENU-TRANSID                      01860010
                WHEN (OPT2L  NOT = 0 AND OPT2I = 'S')                   01870001
                     MOVE 'CI02' TO CIMENU-TRANSID                      01880010
                WHEN (OPT3L  NOT = 0 AND OPT3I = 'S')                   01881003
                     MOVE 'CI03' TO CIMENU-TRANSID                      01882010
                WHEN (OPT4L  NOT = 0 AND OPT4I = 'S')                   01883003
                     MOVE 'CI04' TO CIMENU-TRANSID                      01884010
                WHEN (OPT5L  NOT = 0 AND OPT5I = 'S')                   01885003
                     MOVE 'CI05' TO CIMENU-TRANSID                      01886010
                WHEN OTHER                                              01890001
                     MOVE 'INVALID INPUT' TO MSGO                       01900001
                     SET WS-MAP-DATAONLY TO TRUE                        01910001
                     PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT      01920001
            END-EVALUATE                                                01930001
            .                                                           01940001
      *                                                                 01950001
       3010-CHECK-INPUT-EXIT.                                           01960001
            EXIT.                                                       01970001
      *                                                                 01980001
       3020-XCTL.                                                       01990001
      *     INITIALIZE WS-COMMAREA                                      01990113
      *     MOVE 'Y' TO WS-FIRST-SEND                                   01990213
      *     EVALUATE CIMENU-TRANSID                                     01991013
      *         WHEN 'CI01'                                             01991113
      *              MOVE 0 TO WS-OPTION                                01991213
      *         WHEN 'CI02'                                             01991413
      *              MOVE 1 TO WS-OPTION                                01991513
      *         WHEN 'CI03'                                             01991613
      *              MOVE 2 TO WS-OPTION                                01991713
      *         WHEN 'CI04'                                             01991813
      *              MOVE 3 TO WS-OPTION                                01991913
      *         WHEN 'CI05'                                             01992013
      *              MOVE 4 TO WS-OPTION                                01992113
      *     END-EVALUATE                                                01993013
            EXEC CICS READ                                              02000001
                 FILE('CIMENU')                                         02010001
                 INTO(CIMENU-REC)                                       02020001
                 RIDFLD(CIMENU-TRANSID)                                 02030001
                 RESP(WS-RESP-CODE)                                     02040001
            END-EXEC                                                    02050001
            EVALUATE WS-RESP-CODE                                       02060001
                WHEN DFHRESP(NORMAL)                                    02070001
                     EXEC CICS                                          02080001
                          XCTL PROGRAM(CIMENU-PGM)                      02090001
                          COMMAREA(CIMENU-TRANSID)                      02090113
                          RESP(WS-RESP-CODE)                            02091006
                     END-EXEC                                           02100001
                     IF WS-RESP-CODE NOT = DFHRESP(NORMAL)              02110006
                        STRING 'PROGRAM ' DELIMITED BY SIZE             02111006
                               CIMENU-PGM DELIMITED BY SPACE            02112006
                               ' IS NOT AVAILABLE' DELIMITED BY SIZE    02113006
                               INTO MSGO                                02114006
                        SET WS-MAP-DATAONLY TO TRUE                     02115006
                        PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT   02116006
                     END-IF                                             02117006
                WHEN DFHRESP(NOTFND)                                    02118006
                     MOVE 'INVALID TRANSATION ID!' TO MSGO              02120001
                     SET WS-MAP-DATAONLY TO TRUE                        02130001
                     PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT      02140001
                WHEN OTHER                                              02150001
                     MOVE 'CIMENU FILE ERROR!' TO MSGO                  02160001
                     SET WS-MAP-DATAONLY TO TRUE                        02170001
                     PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT      02180001
            END-EVALUATE                                                02190001
            .                                                           02200001
      *                                                                 02210001
       3020-XCTL-EXIT.                                                  02220001
            EXIT.                                                       02230001
      *                                                                 02240001
       3030-SEND-MAP.                                                   02250001
            PERFORM 1010-ASK-TIME-DATE                                  02260001
               THRU 1010-ASK-TIME-DATE-EXIT                             02270001
            EVALUATE TRUE                                               02280001
                WHEN WS-MAP-ERASE                                       02290001
                     EXEC CICS SEND                                     02300001
                          MAP('CICA00')                                 02310002
                          MAPSET('CICA00')                              02320002
                          FROM(CICA00O)                                 02330002
                          ERASE                                         02340001
                     END-EXEC                                           02350001
                WHEN WS-MAP-DATAONLY                                    02360001
                     EXEC CICS SEND                                     02370001
                          MAP('CICA00')                                 02380002
                          MAPSET('CICA00')                              02390002
                          FROM(CICA00O)                                 02400004
                          DATAONLY                                      02410001
                     END-EXEC                                           02420001
            END-EVALUATE                                                02430001
            MOVE '1' TO WS-ENTER-FLAG                                   02440001
            PERFORM 5020-RETURN-TRANS THRU 5020-RETURN-TRANS-EXIT       02450001
            .                                                           02460001
      *                                                                 02470001
       3030-SEND-MAP-EXIT.                                              02480001
            EXIT.                                                       02490001
      *                                                                 02500001
       4000-POST-PROCESSING.                                            02510001
      *                                                                 02520001
       4000-POST-PROCESSING-EXIT.                                       02530001
            EXIT.                                                       02540001
      *                                                                 02550001
       5000-CLEAN-UP.                                                   02560001
            PERFORM 5010-RETURN                                         02570001
               THRU 5010-RETURN-EXIT                                    02580001
            .                                                           02590001
      *                                                                 02600001
       5000-CLEAN-UP-EXIT.                                              02610001
            EXIT.                                                       02620001
      *                                                                 02630001
       5010-RETURN.                                                     02640001
            EXEC CICS RETURN END-EXEC                                   02650001
            .                                                           02660001
       5010-RETURN-EXIT.                                                02670001
            EXIT.                                                       02680001
      *                                                                 02690001
       5020-RETURN-TRANS.                                               02700001
            EXEC CICS RETURN TRANSID('CICA')                            02710002
                      COMMAREA(WS-ENTER-FLAG)                           02720001
            END-EXEC                                                    02730001
            .                                                           02740001
       5020-RETURN-TRANS-EXIT.                                          02750001
            EXIT.                                                       02760001
