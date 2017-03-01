       IDENTIFICATION DIVISION.                                         00010000
       PROGRAM-ID. CIOCCP01.                                            00020050
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
       77 WS-BEGIN              PIC X(17) VALUE 'CIOCCP01 WS BEGIN'.    00190050
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
       COPY CICS01.                                                     00350028
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
      *                                                                 00460000
       01 WS-COMMAREA.                                                  00470000
          05 WS-FIRST-SEND      PIC X(1).                               00480000
          05 WS-STEP            PIC 9(3).                               00492055
          05 WS-ID-NUMBER       PIC 9(18).                              00493055
       01 WS-COMMAREA-SELF.                                             00495038
          05 WS-FIRST-SEND      PIC X(1).                               00496038
          05 WS-OPTION          PIC 9(3).                               00497038
       77 WS-END                PIC X(15) VALUE 'CIOCCP01 WS END'.      00500054
      *                                                                 00510000
       LINKAGE SECTION.                                                 00520000
       01 DFHCOMMAREA.                                                  00530034
          05 LK-FIRST-SEND      PIC X(1).                               00550051
          05 LK-OPTION          PIC 9(3).                               00551051
       COPY SD00WS.                                                     00552034
      *                                                                 00560000
       PROCEDURE DIVISION.                                              00570000
       0000-MAINLINE.                                                   00580000
      *                                                                 00590000
            PERFORM 1000-INIT                                           00600000
               THRU 1000-INIT-EXIT                                      00610000
      *                                                                 00620000
            PERFORM 2000-PRE-PROCESSING                                 00630000
               THRU 2000-PRE-PROCESSING-EXIT                            00640000
      *                                                                 00650000
            PERFORM 3000-MAIN-PROCESS                                   00660000
               THRU 3000-MAIN-PROCESS-EXIT                              00670000
      *                                                                 00680000
            PERFORM 4000-POST-PROCESSING                                00690000
               THRU 4000-POST-PROCESSING-EXIT                           00700000
      *                                                                 00710000
            PERFORM 5000-CLEAN-UP                                       00720000
               THRU 5000-CLEAN-UP-EXIT                                  00730000
            .                                                           00740000
      *                                                                 00750000
       0000-EXIT.                                                       00760000
            EXIT.                                                       00770000
      *                                                                 00780000
       1000-INIT.                                                       00790000
            IF LK-FIRST-SEND = 'Y'                                      00800051
               MOVE LOW-VALUES TO CICS01O                               00810051
               SET WS-MAP-ERASE TO TRUE                                 00820051
               PERFORM 3030-SEND-MAP                                    00830051
                  THRU 3030-SEND-MAP-EXIT                               00840051
      * NOT FIRST SHOW                                                  00850052
            ELSE                                                        00860051
                  MOVE LOW-VALUES TO CICS01I                            00880051
                  EXEC CICS RECEIVE MAP('CICS01')                       00890051
                                   MAPSET('CICS01')                     00900051
                                   INTO(CICS01I)                        00910051
                                   RESP(WS-RESP-CODE)                   00920051
                  END-EXEC                                              00930051
            END-IF                                                      00950051
            .                                                           00960051
      *     IF EIBCALEN = 0                                             00961051
      *        MOVE LOW-VALUES TO CICS01O                               00962051
      *        SET WS-MAP-ERASE TO TRUE                                 00963051
      *        PERFORM 3030-SEND-MAP                                    00964051
      *           THRU 3030-SEND-MAP-EXIT                               00965051
      * NOT FIRST SHOW                                                  00966034
      *     ELSE                                                        00967051
      *        IF SDCA-CICS-SECONDENTER                                 00968051
      *           MOVE LOW-VALUES TO CICS01I                            00969051
      *           EXEC CICS RECEIVE MAP('CICS01')                       00969151
      *                            MAPSET('CICS01')                     00969251
      *                            INTO(CICS01I)                        00969351
      *                            RESP(WS-RESP-CODE)                   00969451
      *           END-EXEC                                              00969551
      *        END-IF                                                   00969651
      *     END-IF                                                      00969751
      *     .                                                           00969851
       1000-INIT-EXIT.                                                  00970000
            EXIT.                                                       00980000
      *                                                                 00990000
       1010-ASK-TIME-DATE.                                              01000000
      *                                                                 01010000
            EXEC CICS                                                   01020000
                 ASKTIME                                                01030000
                 ABSTIME(WS-GETTIME)                                    01040000
            END-EXEC                                                    01050000
            EXEC CICS                                                   01060000
                 FORMATTIME                                             01070000
                 ABSTIME(WS-GETTIME)                                    01080000
                 DATESEP('/')                                           01090000
                 YYYYMMDD(WS-DATEOUT)                                   01100000
            END-EXEC                                                    01110000
            EXEC CICS                                                   01120000
                 FORMATTIME                                             01130000
                 ABSTIME(WS-GETTIME)                                    01140000
                 TIMESEP                                                01150000
                 TIME(WS-TIMEOUT)                                       01160000
            END-EXEC                                                    01170000
            MOVE WS-DATEOUT TO SYSDO                                    01180000
            MOVE WS-TIMEOUT TO SYSTO                                    01190000
            .                                                           01200000
      *                                                                 01210000
       1010-ASK-TIME-DATE-EXIT.                                         01220000
            EXIT.                                                       01230000
      *                                                                 01240000
       2000-PRE-PROCESSING.                                             01250000
      *                                                                 01260000
       2000-PRE-PROCESSING-EXIT.                                        01270000
            EXIT.                                                       01280000
      *                                                                 01290000
       3000-MAIN-PROCESS.                                               01300000
            EVALUATE EIBAID                                             01310000
                WHEN DFHPF1                                             01320026
                     EXEC CICS                                          01330026
                          XCTL PROGRAM('CIOCCP00')                      01340050
                               RESP(WS-RESP-CODE)                       01350026
                     END-EXEC                                           01360026
                     IF WS-RESP-CODE NOT = DFHRESP(NORMAL)              01370026
                        MOVE 'PROGRAM CIOCCP00 IS NOT AVAILABLE'        01380050
                                TO MSGO                                 01390026
                        SET WS-MAP-DATAONLY TO TRUE                     01400026
                        PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT   01410026
                     END-IF                                             01420026
                WHEN DFHPF3                                             01430000
                     MOVE 'THANK YOU FOR USING THE SYSTEM' TO WS-MESSAGE01440000
                     EXEC CICS                                          01450000
                          SEND CONTROL                                  01460000
                          CURSOR                                        01470000
                          ERASE                                         01480000
                          FREEKB                                        01490000
                          ALARM                                         01500000
                     END-EXEC                                           01510000
                     EXEC CICS                                          01520000
                          SEND FROM(WS-MESSAGE)                         01530000
                     END-EXEC                                           01540000
                     PERFORM 5010-RETURN THRU 5010-RETURN-EXIT          01550000
                WHEN DFHCLEAR                                           01560000
                     EXEC CICS                                          01570000
                           SEND CONTROL                                 01580000
                           CURSOR                                       01590000
                           ERASE                                        01600000
                           FREEKB                                       01610000
                           ALARM                                        01620000
                     END-EXEC                                           01630000
                     PERFORM 5010-RETURN THRU 5010-RETURN-EXIT          01640000
                WHEN DFHPF9                                             01650000
                     MOVE LOW-VALUES TO CICS01O                         01660028
                     SET WS-MAP-ERASE TO TRUE                           01670000
                     PERFORM 3030-SEND-MAP                              01680000
                        THRU 3030-SEND-MAP-EXIT                         01690000
                 WHEN DFHENTER                                          01700000
                      IF WS-RESP-CODE NOT EQUAL DFHRESP(NORMAL)         01710000
                         MOVE 'INVALID REQUEST, CHECK YOUR INPUT.'      01720000
                              TO MSGO                                   01730000
                         SET WS-MAP-DATAONLY TO TRUE                    01740000
                         PERFORM 3030-SEND-MAP                          01750000
                            THRU 3030-SEND-MAP-EXIT                     01760000
                      ELSE                                              01770000
                         PERFORM 3010-CHECK-INPUT                       01780000
                            THRU 3010-CHECK-INPUT-EXIT                  01790000
                         PERFORM 3020-XCTL                              01800000
                            THRU 3020-XCTL-EXIT                         01810000
                      END-IF                                            01820000
                 WHEN OTHER                                             01830000
                      MOVE 'INVALID KEY PRESSED!' TO MSGO               01840000
                      SET WS-MAP-DATAONLY TO TRUE                       01850000
                      PERFORM 3030-SEND-MAP                             01860000
                         THRU 3030-SEND-MAP-EXIT                        01870000
            END-EVALUATE                                                01880000
            .                                                           01890000
       3000-MAIN-PROCESS-EXIT.                                          01900000
            EXIT.                                                       01910000
      *                                                                 01920000
       3010-CHECK-INPUT.                                                01930000
            IF COMMUL NOT = 0                                           01933032
               INITIALIZE CIMENU-REC                                    01940032
               MOVE COMMUI TO CIMENU-TRANSID                            01940132
               EXEC CICS READ                                           01940212
                    FILE('CIMENU')                                      01940312
                    INTO(CIMENU-REC)                                    01940412
                    RIDFLD(CIMENU-TRANSID)                              01940512
                    RESP(WS-RESP-CODE)                                  01940612
               END-EXEC                                                 01940712
               EVALUATE WS-RESP-CODE                                    01940812
                   WHEN DFHRESP(NORMAL)                                 01940912
                        EXEC CICS                                       01941012
                             XCTL PROGRAM(CIMENU-PGM)                   01941122
                             COMMAREA(CIMENU-TRANSID)                   01941249
                             RESP(WS-RESP-CODE)                         01941312
                        END-EXEC                                        01941412
                        IF WS-RESP-CODE NOT = DFHRESP(NORMAL)           01941512
                        STRING 'PROGRAM ' DELIMITED BY SIZE             01941623
                               CIMENU-PGM DELIMITED BY SPACE            01941723
                               ' IS NOT AVAILABLE' DELIMITED BY SIZE    01941823
                               INTO MSGO                                01941923
                           SET WS-MAP-DATAONLY TO TRUE                  01942012
                           PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT01942112
                        END-IF                                          01942212
                   WHEN DFHRESP(NOTFND)                                 01942312
                        MOVE 'INVALID TRANSATION ID!' TO MSGO           01942412
                        SET WS-MAP-DATAONLY TO TRUE                     01942512
                        PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT   01942612
                   WHEN OTHER                                           01942712
                        MOVE 'CIMENU FILE ERROR!' TO MSGO               01942812
                        SET WS-MAP-DATAONLY TO TRUE                     01942912
                        PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT   01943012
               END-EVALUATE                                             01943112
            END-IF                                                      01944012
            IF (IDTYPEL = 0 OR IDNUML = 0)                              01945029
               MOVE 'FIELDS CAN NOT BE EMPTY' TO MSGO                   01946013
               SET WS-MAP-DATAONLY TO TRUE                              01947013
               PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT            01948013
            END-IF                                                      01949013
            IF (IDTYPEI NOT = 001 AND IDTYPEI NOT = 002)                01950025
               MOVE 'INVAILD ID TYPE' TO MSGO                           01951013
               SET WS-MAP-DATAONLY TO TRUE                              01952013
               PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT            01953013
            END-IF                                                      01954024
            IF (IDNUMI IS NOT NUMERIC)                                  01960024
               MOVE 'IDMUN MUEST BE NUMBER' TO MSGO                     01970024
               SET WS-MAP-DATAONLY TO TRUE                              01980024
               PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT            01990024
            END-IF                                                      02000024
            .                                                           02150000
      *                                                                 02160000
      * IDNUM MAY NOT EXIST, NEED TO VBS.CI.APPLICAN.IN1                02161053
       3010-CHECK-INPUT-EXIT.                                           02170000
            EXIT.                                                       02180000
      *                                                                 02190000
       3020-XCTL.                                                       02200000
            INITIALIZE WS-COMMAREA                                      02210016
            MOVE 'Y' TO WS-FIRST-SEND OF WS-COMMAREA                    02220016
            MOVE LK-OPTION TO WS-STEP                                   02250056
            MOVE IDNUMI TO WS-ID-NUMBER                                 02260016
            .                                                           02650000
            EXEC CICS                                                   02651016
                 XCTL PROGRAM('CIOCCP02')                               02652050
                 COMMAREA(WS-COMMAREA)                                  02653016
                 RESP(WS-RESP-CODE)                                     02654016
            END-EXEC                                                    02655016
            .                                                           02656017
      *                                                                 02660000
       3020-XCTL-EXIT.                                                  02670000
            EXIT.                                                       02680000
      *                                                                 02690000
       3030-SEND-MAP.                                                   02700000
            PERFORM 1010-ASK-TIME-DATE                                  02710000
               THRU 1010-ASK-TIME-DATE-EXIT                             02720000
            EVALUATE TRUE                                               02730000
                WHEN WS-MAP-ERASE                                       02740000
                     EXEC CICS SEND                                     02750000
                          MAP('CICS01')                                 02760028
                          MAPSET('CICS01')                              02770028
                          FROM(CICS01O)                                 02780028
                          ERASE                                         02790000
                     END-EXEC                                           02800000
                WHEN WS-MAP-DATAONLY                                    02810000
                     EXEC CICS SEND                                     02820000
                          MAP('CICS01')                                 02830028
                          MAPSET('CICS01')                              02840028
                          FROM(CICS01O)                                 02850028
                          DATAONLY                                      02860000
                     END-EXEC                                           02870000
            END-EVALUATE                                                02880000
      *     MOVE 'N' TO LK-FIRST-SEND                                   02890018
      *     MOVE '1' TO WS-ENTER-FLAG                                   02891054
            PERFORM 5020-RETURN-TRANS THRU 5020-RETURN-TRANS-EXIT       02900000
            .                                                           02910000
      *                                                                 02920000
       3030-SEND-MAP-EXIT.                                              02930000
            EXIT.                                                       02940000
      *                                                                 02950000
       4000-POST-PROCESSING.                                            02960000
      *                                                                 02970000
       4000-POST-PROCESSING-EXIT.                                       02980000
            EXIT.                                                       02990000
      *                                                                 03000000
       5000-CLEAN-UP.                                                   03010000
            PERFORM 5010-RETURN                                         03020000
               THRU 5010-RETURN-EXIT                                    03030000
            .                                                           03040000
      *                                                                 03050000
       5000-CLEAN-UP-EXIT.                                              03060000
            EXIT.                                                       03070000
      *                                                                 03080000
       5010-RETURN.                                                     03090000
            EXEC CICS RETURN END-EXEC                                   03100000
            .                                                           03110000
       5010-RETURN-EXIT.                                                03120000
            EXIT.                                                       03130000
      *                                                                 03140000
       5020-RETURN-TRANS.                                               03150000
            INITIALIZE WS-COMMAREA-SELF                                 03151054
            MOVE 'N' TO WS-FIRST-SEND OF WS-COMMAREA-SELF               03152054
            MOVE LK-OPTION TO WS-OPTION OF WS-COMMAREA-SELF             03153054
            EXEC CICS RETURN TRANSID('CIC1')                            03160050
                      COMMAREA(WS-COMMAREA-SELF)                        03171054
            END-EXEC                                                    03180000
            .                                                           03190000
       5020-RETURN-TRANS-EXIT.                                          03200000
            EXIT.                                                       03210000
