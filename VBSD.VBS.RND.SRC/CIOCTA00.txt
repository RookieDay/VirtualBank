       IDENTIFICATION DIVISION.                                         00010001
       PROGRAM-ID. CIOCTA00.                                            00020001
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
       77 WS-BEGIN              PIC X(17) VALUE 'CIOCTA00 WS BEGIN'.    00190001
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
       COPY CITA00.                                                     00350001
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
      *                                                                 00451003
       01 WS-COMMAREA.                                                  00452003
          05 WS-FIRST-SEND      PIC X(1).                               00453003
          05 WS-OPTION          PIC 9(3).                               00454003
       77 WS-END                PIC X(15) VALUE 'CIOCCAMN WS END'.      00455003
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
               MOVE LOW-VALUES TO CITA00O                               00770001
               SET WS-MAP-ERASE TO TRUE                                 00780001
               PERFORM 3030-SEND-MAP                                    00790001
                  THRU 3030-SEND-MAP-EXIT                               00800001
      * NOT FIRST SHOW                                                  00810001
            ELSE                                                        00820001
               IF SDCA-CICS-SECONDENTER                                 00830001
                  MOVE LOW-VALUES TO CITA00I                            00840001
                  EXEC CICS RECEIVE MAP('CITA00')                       00850001
                                   MAPSET('CITA00')                     00860001
                                   INTO(CITA00I)                        00870001
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
                WHEN DFHPF1                                             01280001
                     EXEC CICS                                          01290001
                          XCTL PROGRAM('CIOCCIMN')                      01300001
                               RESP(WS-RESP-CODE)                       01310001
                     END-EXEC                                           01320001
                     IF WS-RESP-CODE NOT = DFHRESP(NORMAL)              01330001
                        MOVE 'PROGRAM CIOCCIMN IS NOT AVAILABLE'        01340001
                                TO MSGO                                 01350001
                        SET WS-MAP-DATAONLY TO TRUE                     01360001
                        PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT   01370001
                     END-IF                                             01380001
                WHEN DFHPF3                                             01390001
                     MOVE 'THANK YOU FOR USING THE SYSTEM' TO WS-MESSAGE01400001
                     EXEC CICS                                          01410001
                          SEND CONTROL                                  01420001
                          CURSOR                                        01430001
                          ERASE                                         01440001
                          FREEKB                                        01450001
                          ALARM                                         01460001
                     END-EXEC                                           01470001
                     EXEC CICS                                          01480001
                          SEND FROM(WS-MESSAGE)                         01490001
                     END-EXEC                                           01500001
                     PERFORM 5010-RETURN THRU 5010-RETURN-EXIT          01510001
                WHEN DFHCLEAR                                           01520001
                     EXEC CICS                                          01530001
                           SEND CONTROL                                 01540001
                           CURSOR                                       01550001
                           ERASE                                        01560001
                           FREEKB                                       01570001
                           ALARM                                        01580001
                     END-EXEC                                           01590001
                     PERFORM 5010-RETURN THRU 5010-RETURN-EXIT          01600001
                WHEN DFHPF9                                             01610001
                     MOVE LOW-VALUES TO CITA00O                         01620001
                     SET WS-MAP-ERASE TO TRUE                           01630001
                     PERFORM 3030-SEND-MAP                              01640001
                        THRU 3030-SEND-MAP-EXIT                         01650001
                 WHEN DFHENTER                                          01660001
                      IF WS-RESP-CODE NOT EQUAL DFHRESP(NORMAL)         01670001
                         MOVE 'INVALID REQUEST, CHECK YOUR INPUT.'      01680001
                              TO MSGO                                   01690001
                         SET WS-MAP-DATAONLY TO TRUE                    01700001
                         PERFORM 3030-SEND-MAP                          01710001
                            THRU 3030-SEND-MAP-EXIT                     01720001
                      ELSE                                              01730001
                         PERFORM 3010-CHECK-INPUT                       01740001
                            THRU 3010-CHECK-INPUT-EXIT                  01750001
                         PERFORM 3020-XCTL                              01760001
                            THRU 3020-XCTL-EXIT                         01770001
                      END-IF                                            01780001
                 WHEN OTHER                                             01790001
                      MOVE 'INVALID KEY PRESSED!' TO MSGO               01800001
                      SET WS-MAP-DATAONLY TO TRUE                       01810001
                      PERFORM 3030-SEND-MAP                             01820001
                         THRU 3030-SEND-MAP-EXIT                        01830001
            END-EVALUATE                                                01840001
            .                                                           01850001
       3000-MAIN-PROCESS-EXIT.                                          01860001
            EXIT.                                                       01870001
      *                                                                 01880001
       3010-CHECK-INPUT.                                                01890001
            INITIALIZE CIMENU-REC                                       01900001
            EVALUATE TRUE                                               01910001
                WHEN (COMMUL NOT = 0)                                   01920001
                     MOVE COMMUI TO CIMENU-TRANSID                      01930001
                WHEN (MENUL  NOT = 0)                                   01940001
                     MOVE MENUI  TO CIMENU-TRANSID                      01950001
                WHEN (OPT1L  NOT = 0 AND OPT1I = 'S')                   01960001
                     MOVE 'CI12' TO CIMENU-TRANSID                      01970001
                WHEN (OPT2L  NOT = 0 AND OPT2I = 'S')                   01980001
                     MOVE 'CI13' TO CIMENU-TRANSID                      01990001
                WHEN OTHER                                              02020001
                     MOVE 'INVALID INPUT' TO MSGO                       02030001
                     SET WS-MAP-DATAONLY TO TRUE                        02040001
                     PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT      02050001
            END-EVALUATE                                                02060001
            .                                                           02070001
      *                                                                 02080001
       3010-CHECK-INPUT-EXIT.                                           02090001
            EXIT.                                                       02100001
      *                                                                 02110001
       3020-XCTL.                                                       02120001
            EXEC CICS READ                                              02130001
                 FILE('CIMENU')                                         02140001
                 INTO(CIMENU-REC)                                       02150001
                 RIDFLD(CIMENU-TRANSID)                                 02160001
                 RESP(WS-RESP-CODE)                                     02170001
            END-EXEC                                                    02180001
            EVALUATE WS-RESP-CODE                                       02190001
                WHEN DFHRESP(NORMAL)                                    02200001
                     EXEC CICS                                          02210001
                          XCTL PROGRAM(CIMENU-PGM)                      02220001
                               COMMAREA(CIMENU-TRANSID)                 02230001
                               RESP(WS-RESP-CODE)                       02240001
                     END-EXEC                                           02250001
                     IF WS-RESP-CODE NOT = DFHRESP(NORMAL)              02260001
                        STRING 'PROGRAM ' DELIMITED BY SIZE             02270001
                               CIMENU-PGM DELIMITED BY SPACE            02280001
                               ' IS NOT AVAILABLE' DELIMITED BY SIZE    02290001
                               INTO MSGO                                02300001
                        SET WS-MAP-DATAONLY TO TRUE                     02310001
                        PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT   02320001
                     END-IF                                             02330001
                WHEN DFHRESP(NOTFND)                                    02340001
                     MOVE 'INVALID TRANSATION ID!' TO MSGO              02350001
                     SET WS-MAP-DATAONLY TO TRUE                        02360001
                     PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT      02370001
                WHEN OTHER                                              02380001
                     MOVE 'CIMENU FILE ERROR!' TO MSGO                  02390001
                     SET WS-MAP-DATAONLY TO TRUE                        02400001
                     PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT      02410001
            END-EVALUATE                                                02420001
            .                                                           02430001
      *                                                                 02440001
       3020-XCTL-EXIT.                                                  02450001
            EXIT.                                                       02460001
      *                                                                 02470001
       3030-SEND-MAP.                                                   02480001
            PERFORM 1010-ASK-TIME-DATE                                  02490001
               THRU 1010-ASK-TIME-DATE-EXIT                             02500001
            EVALUATE TRUE                                               02510001
                WHEN WS-MAP-ERASE                                       02520001
                     EXEC CICS SEND                                     02530001
                          MAP('CITA00')                                 02540001
                          MAPSET('CITA00')                              02550001
                          FROM(CITA00O)                                 02560002
                          ERASE                                         02570001
                     END-EXEC                                           02580001
                WHEN WS-MAP-DATAONLY                                    02590001
                     EXEC CICS SEND                                     02600001
                          MAP('CITA00')                                 02610001
                          MAPSET('CITA00')                              02620001
                          FROM(CITA00O)                                 02630001
                          DATAONLY                                      02640001
                     END-EXEC                                           02650001
            END-EVALUATE                                                02660001
            MOVE '1' TO WS-ENTER-FLAG                                   02670001
            PERFORM 5020-RETURN-TRANS THRU 5020-RETURN-TRANS-EXIT       02680001
            .                                                           02690001
      *                                                                 02700001
       3030-SEND-MAP-EXIT.                                              02710001
            EXIT.                                                       02720001
      *                                                                 02730001
       4000-POST-PROCESSING.                                            02740001
      *                                                                 02750001
       4000-POST-PROCESSING-EXIT.                                       02760001
            EXIT.                                                       02770001
      *                                                                 02780001
       5000-CLEAN-UP.                                                   02790001
            PERFORM 5010-RETURN                                         02800001
               THRU 5010-RETURN-EXIT                                    02810001
            .                                                           02820001
      *                                                                 02830001
       5000-CLEAN-UP-EXIT.                                              02840001
            EXIT.                                                       02850001
      *                                                                 02860001
       5010-RETURN.                                                     02870001
            EXEC CICS RETURN END-EXEC                                   02880001
            .                                                           02890001
       5010-RETURN-EXIT.                                                02900001
            EXIT.                                                       02910001
      *                                                                 02920001
       5020-RETURN-TRANS.                                               02930001
            EXEC CICS RETURN TRANSID('CITA')                            02940001
                      COMMAREA(WS-ENTER-FLAG)                           02950001
            END-EXEC                                                    02960001
            .                                                           02970001
       5020-RETURN-TRANS-EXIT.                                          02980001
            EXIT.                                                       02990001
