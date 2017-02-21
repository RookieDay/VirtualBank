       IDENTIFICATION DIVISION.                                         00010000
       PROGRAM-ID. CIOCCS00.                                            00020001
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
      *2015-01-06    KEVIN      INITIAL VERSION                         00130000
      ***************************************************************** 00140000
       ENVIRONMENT DIVISION.                                            00150000
       DATA DIVISION.                                                   00160000
       WORKING-STORAGE SECTION.                                         00170000
      *                                                                 00180000
       77 WS-BEGIN              PIC X(17) VALUE 'CIOCCS00 WS BEGIN'.    00190001
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
       COPY CICS00.                                                     00350002
      *MAP CONTROL                                                      00360000
       COPY DFHBMSCA.                                                   00370000
      *CICS FUNCTION KEYS                                               00380000
       COPY DFHAID.                                                     00390000
      *CIMENU                                                           00400000
       COPY CIMENU.                                                     00410000
      *                                                                 00420000
       01 WS-SRV-COMMAREA.                                              00430000
      *SERVICE REQUEST/RESPONSE COMMAREA                                00440000
       COPY SD01WS.                                                     00450000
      *                                                                 00451000
       01 WS-COMMAREA.                                                  00452000
          05 WS-FIRST-SEND      PIC X(1).                               00453000
          05 WS-OPTION          PIC 9(3).                               00454000
       77 WS-END                PIC X(15) VALUE 'CIOCCS00 WS END'.      00460001
      *                                                                 00470000
       LINKAGE SECTION.                                                 00480000
       01 DFHCOMMAREA.                                                  00490000
      *COMMON CICS SCREEN HANDLE VARIABLES                              00500000
       COPY SD00WS.                                                     00510000
      *                                                                 00520000
       PROCEDURE DIVISION.                                              00530000
       0000-MAINLINE.                                                   00540000
      *                                                                 00550000
            PERFORM 1000-INIT                                           00560000
               THRU 1000-INIT-EXIT                                      00570000
      *                                                                 00580000
            PERFORM 2000-PRE-PROCESSING                                 00590000
               THRU 2000-PRE-PROCESSING-EXIT                            00600000
      *                                                                 00610000
            PERFORM 3000-MAIN-PROCESS                                   00620000
               THRU 3000-MAIN-PROCESS-EXIT                              00630000
      *                                                                 00640000
            PERFORM 4000-POST-PROCESSING                                00650000
               THRU 4000-POST-PROCESSING-EXIT                           00660000
      *                                                                 00670000
            PERFORM 5000-CLEAN-UP                                       00680000
               THRU 5000-CLEAN-UP-EXIT                                  00690000
            .                                                           00700000
      *                                                                 00710000
       0000-EXIT.                                                       00720000
            EXIT.                                                       00730000
      *                                                                 00740000
       1000-INIT.                                                       00750000
            IF EIBCALEN = 0                                             00760000
               MOVE LOW-VALUES TO CICS00O                               00770001
               SET WS-MAP-ERASE TO TRUE                                 00780000
               PERFORM 3030-SEND-MAP                                    00790000
                  THRU 3030-SEND-MAP-EXIT                               00800000
      * NOT FIRST SHOW                                                  00810000
            ELSE                                                        00820000
               IF SDCA-CICS-SECONDENTER                                 00830000
                  MOVE LOW-VALUES TO CICS00I                            00840001
                  EXEC CICS RECEIVE MAP('CICS00')                       00850001
                                   MAPSET('CICS00')                     00860001
                                   INTO(CICS00I)                        00870001
                                   RESP(WS-RESP-CODE)                   00880000
                  END-EXEC                                              00890000
               END-IF                                                   00900000
            END-IF                                                      00910000
            .                                                           00920000
       1000-INIT-EXIT.                                                  00930000
            EXIT.                                                       00940000
      *                                                                 00950000
       1010-ASK-TIME-DATE.                                              00960000
      *                                                                 00970000
            EXEC CICS                                                   00980000
                 ASKTIME                                                00990000
                 ABSTIME(WS-GETTIME)                                    01000000
            END-EXEC                                                    01010000
            EXEC CICS                                                   01020000
                 FORMATTIME                                             01030000
                 ABSTIME(WS-GETTIME)                                    01040000
                 DATESEP('/')                                           01050000
                 YYYYMMDD(WS-DATEOUT)                                   01060000
            END-EXEC                                                    01070000
            EXEC CICS                                                   01080000
                 FORMATTIME                                             01090000
                 ABSTIME(WS-GETTIME)                                    01100000
                 TIMESEP                                                01110000
                 TIME(WS-TIMEOUT)                                       01120000
            END-EXEC                                                    01130000
            MOVE WS-DATEOUT TO SYSDO                                    01140000
            MOVE WS-TIMEOUT TO SYSTO                                    01150000
            .                                                           01160000
      *                                                                 01170000
       1010-ASK-TIME-DATE-EXIT.                                         01180000
            EXIT.                                                       01190000
      *                                                                 01200000
       2000-PRE-PROCESSING.                                             01210000
      *                                                                 01220000
       2000-PRE-PROCESSING-EXIT.                                        01230000
            EXIT.                                                       01240000
      *                                                                 01250000
       3000-MAIN-PROCESS.                                               01260000
            EVALUATE EIBAID                                             01270000
                WHEN DFHPF1                                             01271000
                     EXEC CICS                                          01272000
                          XCTL PROGRAM('CIOCCIMN')                      01273000
                               RESP(WS-RESP-CODE)                       01274000
                     END-EXEC                                           01275000
                     IF WS-RESP-CODE NOT = DFHRESP(NORMAL)              01276000
                        MOVE 'PROGRAM CIOCCIMN IS NOT AVAILABLE'        01277000
                                TO MSGO                                 01279100
                        SET WS-MAP-DATAONLY TO TRUE                     01279200
                        PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT   01279300
                     END-IF                                             01279400
                WHEN DFHPF3                                             01280000
                     MOVE 'THANK YOU FOR USING THE SYSTEM' TO WS-MESSAGE01290000
                     EXEC CICS                                          01300000
                          SEND CONTROL                                  01310000
                          CURSOR                                        01320000
                          ERASE                                         01330000
                          FREEKB                                        01340000
                          ALARM                                         01350000
                     END-EXEC                                           01360000
                     EXEC CICS                                          01370000
                          SEND FROM(WS-MESSAGE)                         01380000
                     END-EXEC                                           01390000
                     PERFORM 5010-RETURN THRU 5010-RETURN-EXIT          01400000
                WHEN DFHCLEAR                                           01410000
                     EXEC CICS                                          01420000
                           SEND CONTROL                                 01430000
                           CURSOR                                       01440000
                           ERASE                                        01450000
                           FREEKB                                       01460000
                           ALARM                                        01470000
                     END-EXEC                                           01480000
                     PERFORM 5010-RETURN THRU 5010-RETURN-EXIT          01490000
                WHEN DFHPF9                                             01500000
                     MOVE LOW-VALUES TO CICS00O                         01510002
                     SET WS-MAP-ERASE TO TRUE                           01520000
                     PERFORM 3030-SEND-MAP                              01530000
                        THRU 3030-SEND-MAP-EXIT                         01540000
                 WHEN DFHENTER                                          01550000
                      IF WS-RESP-CODE NOT EQUAL DFHRESP(NORMAL)         01560000
                         MOVE 'INVALID REQUEST, CHECK YOUR INPUT.'      01570000
                              TO MSGO                                   01580000
                         SET WS-MAP-DATAONLY TO TRUE                    01590000
                         PERFORM 3030-SEND-MAP                          01600000
                            THRU 3030-SEND-MAP-EXIT                     01610000
                      ELSE                                              01620000
                         PERFORM 3010-CHECK-INPUT                       01630000
                            THRU 3010-CHECK-INPUT-EXIT                  01640000
                         PERFORM 3020-XCTL                              01650000
                            THRU 3020-XCTL-EXIT                         01660000
                      END-IF                                            01670000
                 WHEN OTHER                                             01680000
                      MOVE 'INVALID KEY PRESSED!' TO MSGO               01690000
                      SET WS-MAP-DATAONLY TO TRUE                       01700000
                      PERFORM 3030-SEND-MAP                             01710000
                         THRU 3030-SEND-MAP-EXIT                        01720000
            END-EVALUATE                                                01730000
            .                                                           01740000
       3000-MAIN-PROCESS-EXIT.                                          01750000
            EXIT.                                                       01760000
      *                                                                 01770000
       3010-CHECK-INPUT.                                                01780000
            INITIALIZE CIMENU-REC                                       01790000
            EVALUATE TRUE                                               01800000
                WHEN (COMMUL NOT = 0)                                   01810000
                     MOVE COMMUI TO CIMENU-TRANSID                      01820000
                WHEN (MENUL  NOT = 0)                                   01830000
                     MOVE MENUI  TO CIMENU-TRANSID                      01840000
                WHEN (OPT1L  NOT = 0 AND OPT1I = 'S')                   01850000
                     MOVE 'CI06' TO CIMENU-TRANSID                      01860002
                WHEN OTHER                                              01890000
                     MOVE 'INVALID INPUT' TO MSGO                       01900000
                     SET WS-MAP-DATAONLY TO TRUE                        01910000
                     PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT      01920000
            END-EVALUATE                                                01930000
            .                                                           01940000
      *                                                                 01950000
       3010-CHECK-INPUT-EXIT.                                           01960000
            EXIT.                                                       01970000
      *                                                                 01980000
       3020-XCTL.                                                       01990000
            EXEC CICS READ                                              02000000
                 FILE('CIMENU')                                         02010000
                 INTO(CIMENU-REC)                                       02020000
                 RIDFLD(CIMENU-TRANSID)                                 02030000
                 RESP(WS-RESP-CODE)                                     02040000
            END-EXEC                                                    02050000
            EVALUATE WS-RESP-CODE                                       02060000
                WHEN DFHRESP(NORMAL)                                    02070000
                     EXEC CICS                                          02080000
                          XCTL PROGRAM(CIMENU-PGM)                      02090000
                               COMMAREA(CIMENU-TRANSID)                 02090104
                          RESP(WS-RESP-CODE)                            02091000
                     END-EXEC                                           02100000
                     IF WS-RESP-CODE NOT = DFHRESP(NORMAL)              02110000
                        STRING 'PROGRAM ' DELIMITED BY SIZE             02111000
                               CIMENU-PGM DELIMITED BY SPACE            02112000
                               ' IS NOT AVAILABLE' DELIMITED BY SIZE    02113000
                               INTO MSGO                                02114000
                        SET WS-MAP-DATAONLY TO TRUE                     02115000
                        PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT   02116000
                     END-IF                                             02117000
                WHEN DFHRESP(NOTFND)                                    02118000
                     MOVE 'INVALID TRANSATION ID!' TO MSGO              02120000
                     SET WS-MAP-DATAONLY TO TRUE                        02130000
                     PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT      02140000
                WHEN OTHER                                              02150000
                     MOVE 'CIMENU FILE ERROR!' TO MSGO                  02160000
                     SET WS-MAP-DATAONLY TO TRUE                        02170000
                     PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT      02180000
            END-EVALUATE                                                02190000
            .                                                           02200000
      *                                                                 02210000
       3020-XCTL-EXIT.                                                  02220000
            EXIT.                                                       02230000
      *                                                                 02240000
       3030-SEND-MAP.                                                   02250000
            PERFORM 1010-ASK-TIME-DATE                                  02260000
               THRU 1010-ASK-TIME-DATE-EXIT                             02270000
            EVALUATE TRUE                                               02280000
                WHEN WS-MAP-ERASE                                       02290000
                     EXEC CICS SEND                                     02300000
                          MAP('CICS00')                                 02310002
                          MAPSET('CICS00')                              02320002
                          FROM(CICS00O)                                 02330002
                          ERASE                                         02340000
                     END-EXEC                                           02350000
                WHEN WS-MAP-DATAONLY                                    02360000
                     EXEC CICS SEND                                     02370000
                          MAP('CICS00')                                 02380002
                          MAPSET('CICS00')                              02390002
                          FROM(CICS00O)                                 02400002
                          DATAONLY                                      02410000
                     END-EXEC                                           02420000
            END-EVALUATE                                                02430000
            MOVE '1' TO WS-ENTER-FLAG                                   02440000
            PERFORM 5020-RETURN-TRANS THRU 5020-RETURN-TRANS-EXIT       02450000
            .                                                           02460000
      *                                                                 02470000
       3030-SEND-MAP-EXIT.                                              02480000
            EXIT.                                                       02490000
      *                                                                 02500000
       4000-POST-PROCESSING.                                            02510000
      *                                                                 02520000
       4000-POST-PROCESSING-EXIT.                                       02530000
            EXIT.                                                       02540000
      *                                                                 02550000
       5000-CLEAN-UP.                                                   02560000
            PERFORM 5010-RETURN                                         02570000
               THRU 5010-RETURN-EXIT                                    02580000
            .                                                           02590000
      *                                                                 02600000
       5000-CLEAN-UP-EXIT.                                              02610000
            EXIT.                                                       02620000
      *                                                                 02630000
       5010-RETURN.                                                     02640000
            EXEC CICS RETURN END-EXEC                                   02650000
            .                                                           02660000
       5010-RETURN-EXIT.                                                02670000
            EXIT.                                                       02680000
      *                                                                 02690000
       5020-RETURN-TRANS.                                               02700000
            EXEC CICS RETURN TRANSID('CICS')                            02710002
                      COMMAREA(WS-ENTER-FLAG)                           02720000
            END-EXEC                                                    02730000
            .                                                           02740000
       5020-RETURN-TRANS-EXIT.                                          02750000
            EXIT.                                                       02760000
