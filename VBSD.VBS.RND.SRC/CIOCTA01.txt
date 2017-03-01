       IDENTIFICATION DIVISION.                                         00010000
       PROGRAM-ID. CIOCTA01.                                            00020000
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
       77 WS-BEGIN              PIC X(17) VALUE 'CIOCTA01 WS BEGIN'.    00190000
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
       COPY CITA01.                                                     00350000
      *MAP CONTROL                                                      00360000
       COPY DFHBMSCA.                                                   00370000
      *CICS FUNCTION KEYS                                               00380000
       COPY DFHAID.                                                     00390000
      *CIMENU                                                           00400000
       COPY CIMENU.                                                     00410000
      *                                                                 00420000
       COPY CIC0015I.                                                   00421000
       COPY CIC0015O.                                                   00422000
      *                                                                 00423000
       01 WS-SRV-COMMAREA.                                              00430000
      *SERVICE REQUEST/RESPONSE COMMAREA                                00440000
       COPY SD01WS.                                                     00450000
      *                                                                 00460000
       01 WS-COMMAREA.                                                  00470000
          05 WS-FIRST-SEND      PIC X(1).                               00480000
          05 WS-OPTION          PIC 9(3).                               00490000
          05 WS-CARD-NUMB       PIC 9(16).                              00510010
      *   05 WS-START-TIME      PIC 9(14).                              00511011
      *   05 WS-END-TIME        PIC 9(14).                              00512011
       01 WS-COMMAREA-SELF.                                             00520000
          05 WS-FIRST-SEND      PIC X(1).                               00530000
          05 WS-OPTION          PIC 9(3).                               00531001
          05 WS-CARD-NUMB       PIC 9(16).                              00540000
       77 WS-END                PIC X(15) VALUE 'CIOCTA01 WS END'.      00550000
      *                                                                 00560000
       LINKAGE SECTION.                                                 00570000
       01 DFHCOMMAREA.                                                  00580000
          05 LK-FIRST-SEND      PIC X(1).                               00590000
          05 LK-OPTION          PIC 9(3).                               00591001
          05 LK-CARD-NUMB       PIC 9(16).                              00600000
      *                                                                 00610000
       PROCEDURE DIVISION.                                              00620000
       0000-MAINLINE.                                                   00630000
      *                                                                 00640000
            PERFORM 1000-INIT                                           00650000
               THRU 1000-INIT-EXIT                                      00660000
      *                                                                 00670000
            PERFORM 2000-PRE-PROCESSING                                 00680000
               THRU 2000-PRE-PROCESSING-EXIT                            00690000
      *                                                                 00700000
            PERFORM 3000-MAIN-PROCESS                                   00710000
               THRU 3000-MAIN-PROCESS-EXIT                              00720000
      *                                                                 00730000
            PERFORM 4000-POST-PROCESSING                                00740000
               THRU 4000-POST-PROCESSING-EXIT                           00750000
      *                                                                 00760000
            PERFORM 5000-CLEAN-UP                                       00770000
               THRU 5000-CLEAN-UP-EXIT                                  00780000
            .                                                           00790000
      *                                                                 00800000
       0000-EXIT.                                                       00810000
            EXIT.                                                       00820000
      *                                                                 00830000
       1000-INIT.                                                       00840000
            IF LK-FIRST-SEND = 'Y'                                      00850000
               MOVE LOW-VALUES TO CITA01O                               00860000
               SET WS-MAP-ERASE TO TRUE                                 00870000
               PERFORM 3030-SEND-MAP                                    00880000
                  THRU 3030-SEND-MAP-EXIT                               00890000
      * NOT FIRST SHOW                                                  00900000
            ELSE                                                        00910000
                  MOVE LOW-VALUES TO CITA01I                            00920000
                  EXEC CICS RECEIVE MAP('CITA01')                       00930000
                                   MAPSET('CITA01')                     00940000
                                   INTO(CITA01I)                        00950000
                                   RESP(WS-RESP-CODE)                   00960000
                  END-EXEC                                              00970000
            END-IF                                                      00980000
            .                                                           00990000
       1000-INIT-EXIT.                                                  01000000
            EXIT.                                                       01010000
      *                                                                 01020000
       1010-ASK-TIME-DATE.                                              01030000
      *                                                                 01040000
            EXEC CICS                                                   01050000
                 ASKTIME                                                01060000
                 ABSTIME(WS-GETTIME)                                    01070000
            END-EXEC                                                    01080000
            EXEC CICS                                                   01090000
                 FORMATTIME                                             01100000
                 ABSTIME(WS-GETTIME)                                    01110000
                 DATESEP('/')                                           01120000
                 YYYYMMDD(WS-DATEOUT)                                   01130000
            END-EXEC                                                    01140000
            EXEC CICS                                                   01150000
                 FORMATTIME                                             01160000
                 ABSTIME(WS-GETTIME)                                    01170000
                 TIMESEP                                                01180000
                 TIME(WS-TIMEOUT)                                       01190000
            END-EXEC                                                    01200000
            MOVE WS-DATEOUT TO SYSDO                                    01210000
            MOVE WS-TIMEOUT TO SYSTO                                    01220000
            .                                                           01230000
      *                                                                 01240000
       1010-ASK-TIME-DATE-EXIT.                                         01250000
            EXIT.                                                       01260000
      *                                                                 01270000
       2000-PRE-PROCESSING.                                             01280000
      *                                                                 01290000
       2000-PRE-PROCESSING-EXIT.                                        01300000
            EXIT.                                                       01310000
      *                                                                 01320000
       3000-MAIN-PROCESS.                                               01330000
            EVALUATE EIBAID                                             01340000
                WHEN DFHPF1                                             01350000
                     EXEC CICS                                          01360000
                          XCTL PROGRAM('CIOCTA00')                      01370000
                               RESP(WS-RESP-CODE)                       01380000
                     END-EXEC                                           01390000
                     IF WS-RESP-CODE NOT = DFHRESP(NORMAL)              01400000
                        MOVE 'PROGRAM CIOCTA00 IS NOT AVAILABLE'        01410000
                                TO MSGO                                 01420000
                        SET WS-MAP-DATAONLY TO TRUE                     01430000
                        PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT   01440000
                     END-IF                                             01450000
                WHEN DFHPF3                                             01460000
                     MOVE 'THANK YOU FOR USING THE SYSTEM' TO WS-MESSAGE01470000
                     EXEC CICS                                          01480000
                          SEND CONTROL                                  01490000
                          CURSOR                                        01500000
                          ERASE                                         01510000
                          FREEKB                                        01520000
                          ALARM                                         01530000
                     END-EXEC                                           01540000
                     EXEC CICS                                          01550000
                          SEND FROM(WS-MESSAGE)                         01560000
                     END-EXEC                                           01570000
                     PERFORM 5010-RETURN THRU 5010-RETURN-EXIT          01580000
                WHEN DFHCLEAR                                           01590000
                     EXEC CICS                                          01600000
                           SEND CONTROL                                 01610000
                           CURSOR                                       01620000
                           ERASE                                        01630000
                           FREEKB                                       01640000
                           ALARM                                        01650000
                     END-EXEC                                           01660000
                     PERFORM 5010-RETURN THRU 5010-RETURN-EXIT          01670000
                WHEN DFHPF9                                             01680000
                     MOVE LOW-VALUES TO CITA01O                         01690000
                     SET WS-MAP-ERASE TO TRUE                           01700000
                     PERFORM 3030-SEND-MAP                              01710000
                        THRU 3030-SEND-MAP-EXIT                         01720000
                 WHEN DFHENTER                                          01730000
                      IF WS-RESP-CODE NOT EQUAL DFHRESP(NORMAL)         01740000
                         MOVE 'INVALID REQUEST, CHECK YOUR INPUT.'      01750000
                              TO MSGO                                   01760000
                         SET WS-MAP-DATAONLY TO TRUE                    01770000
                         PERFORM 3030-SEND-MAP                          01780000
                            THRU 3030-SEND-MAP-EXIT                     01790000
                      ELSE                                              01800000
                         PERFORM 3010-CHECK-INPUT                       01810000
                            THRU 3010-CHECK-INPUT-EXIT                  01820000
                         PERFORM 3020-XCTL                              01830000
                            THRU 3020-XCTL-EXIT                         01840000
                      END-IF                                            01850000
                 WHEN OTHER                                             01860000
                      MOVE 'INVALID KEY PRESSED!' TO MSGO               01870000
                      SET WS-MAP-DATAONLY TO TRUE                       01880000
                      PERFORM 3030-SEND-MAP                             01890000
                         THRU 3030-SEND-MAP-EXIT                        01900000
            END-EVALUATE                                                01910000
            .                                                           01920000
       3000-MAIN-PROCESS-EXIT.                                          01930000
            EXIT.                                                       01940000
      *                                                                 01950000
       3010-CHECK-INPUT.                                                01960000
            IF COMMUL NOT = 0                                           01970000
               INITIALIZE CIMENU-REC                                    01980000
               MOVE COMMUI TO CIMENU-TRANSID                            01990000
               EXEC CICS READ                                           02000000
                    FILE('CIMENU')                                      02010000
                    INTO(CIMENU-REC)                                    02020000
                    RIDFLD(CIMENU-TRANSID)                              02030000
                    RESP(WS-RESP-CODE)                                  02040000
               END-EXEC                                                 02050000
               EVALUATE WS-RESP-CODE                                    02060000
                   WHEN DFHRESP(NORMAL)                                 02070000
                        EXEC CICS                                       02080000
                             XCTL PROGRAM(CIMENU-PGM)                   02090000
                             COMMAREA(CIMENU-TRANSID)                   02100000
                             RESP(WS-RESP-CODE)                         02110000
                        END-EXEC                                        02120000
                        IF WS-RESP-CODE NOT = DFHRESP(NORMAL)           02130000
                        STRING 'PROGRAM ' DELIMITED BY SIZE             02140000
                               CIMENU-PGM DELIMITED BY SPACE            02150000
                               ' IS NOT AVAILABLE' DELIMITED BY SIZE    02160000
                               INTO MSGO                                02170000
                           SET WS-MAP-DATAONLY TO TRUE                  02180000
                           PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT02190000
                        END-IF                                          02200000
                   WHEN DFHRESP(NOTFND)                                 02210000
                        MOVE 'INVALID TRANSATION ID!' TO MSGO           02220000
                        SET WS-MAP-DATAONLY TO TRUE                     02230000
                        PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT   02240000
                   WHEN OTHER                                           02250000
                        MOVE 'CIMENU FILE ERROR!' TO MSGO               02260000
                        SET WS-MAP-DATAONLY TO TRUE                     02270000
                        PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT   02280000
               END-EVALUATE                                             02290000
            END-IF                                                      02300000
            IF (CDNUML = 0 )                                            02310008
               MOVE 'CARD NUMBER CAN NOT BE EMPTY' TO MSGO              02320008
               SET WS-MAP-DATAONLY TO TRUE                              02330000
               PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT            02340000
            END-IF                                                      02350000
            IF (CDNUMI IS NOT NUMERIC)                                  02410000
               MOVE 'CARDNUMBER MUEST BE NUMBER' TO MSGO                02420000
               SET WS-MAP-DATAONLY TO TRUE                              02430000
               PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT            02440000
            END-IF                                                      02450000
            .                                                           02460000
      *                                                                 02470000
       3010-CHECK-INPUT-EXIT.                                           02480000
            EXIT.                                                       02490000
      *                                                                 02500000
       3020-XCTL.                                                       02510000
            INITIALIZE SDCA-SERVICE-COMMAREA                            02511000
            MOVE 'VBS.CI.CREDCARD.INQ' TO SD-SRV-NAME                   02512000
            INITIALIZE CIC0015I-REC                                     02513000
            MOVE CDNUMI  TO CIC0015I-NUMB                               02514000
            MOVE CIC0015I-REC TO SD-SRV-INPUT-DATA                      02515000
            EXEC CICS                                                   02516000
                 LINK                                                   02517000
                 PROGRAM(WS-PGM-SRV-DRIVER)                             02518000
                 COMMAREA(WS-SRV-COMMAREA)                              02519000
                 RESP(WS-RESP-CODE)                                     02519100
            END-EXEC                                                    02519200
            EVALUATE WS-RESP-CODE                                       02519300
                WHEN DFHRESP(NORMAL)                                    02519400
                     IF SD-RESP-CODE EQUAL ZEROS                        02519500
                        INITIALIZE CIC0015O-REC WS-COMMAREA             02519608
                        MOVE SD-SRV-OUTPUT-DATA TO CIC0015O-REC         02519700
      *                 IF CIC0015O-STATUS NOT = 001                    02519808
      *                    MOVE 'CARD STATUS NOT CORRECT' TO MSGO       02519908
      *                    SET WS-MAP-DATAONLY TO TRUE                  02520008
      *                    PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT02520108
      *                 ELSE                                            02520209
                        MOVE 'Y' TO WS-FIRST-SEND OF WS-COMMAREA        02520401
                        MOVE '001' TO WS-OPTION OF WS-COMMAREA          02520501
                        MOVE CIC0015O-NUMB TO WS-CARD-NUMB              02520602
                                           OF WS-COMMAREA               02520702
                        EXEC CICS                                       02520900
                             XCTL PROGRAM('CIOCTA02')                   02521000
                             COMMAREA(WS-COMMAREA)                      02521100
                             RESP(WS-RESP-CODE)                         02521200
                        END-EXEC                                        02521300
                     ELSE                                               02521500
                        MOVE SD-RESP-ADDITIONAL TO MSGO                 02521600
                        SET WS-MAP-DATAONLY TO TRUE                     02521700
                        PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT   02521800
                     END-IF                                             02521900
                WHEN OTHER                                              02522000
                     MOVE 'SERVICE PROGRAM ERROR' TO MSGO               02522100
                     SET WS-MAP-DATAONLY TO TRUE                        02522200
                     PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT      02522300
            END-EVALUATE                                                02522400
            .                                                           02522500
      *                                                                 02640000
       3020-XCTL-EXIT.                                                  02650000
            EXIT.                                                       02660000
      *                                                                 02670000
       3030-SEND-MAP.                                                   02680000
            PERFORM 1010-ASK-TIME-DATE                                  02690000
               THRU 1010-ASK-TIME-DATE-EXIT                             02700000
            EVALUATE TRUE                                               02710000
                WHEN WS-MAP-ERASE                                       02720000
                     EXEC CICS SEND                                     02730000
                          MAP('CITA01')                                 02740000
                          MAPSET('CITA01')                              02750000
                          FROM(CITA01O)                                 02760000
                          ERASE                                         02770000
                     END-EXEC                                           02780000
                WHEN WS-MAP-DATAONLY                                    02790000
                     EXEC CICS SEND                                     02800000
                          MAP('CITA01')                                 02810000
                          MAPSET('CITA01')                              02820000
                          FROM(CITA01O)                                 02830000
                          DATAONLY                                      02840000
                     END-EXEC                                           02850000
            END-EVALUATE                                                02860000
      *     MOVE 'N' TO LK-FIRST-SEND                                   02870000
            PERFORM 5020-RETURN-TRANS THRU 5020-RETURN-TRANS-EXIT       02880000
            .                                                           02890000
      *                                                                 02900000
       3030-SEND-MAP-EXIT.                                              02910000
            EXIT.                                                       02920000
      *                                                                 02930000
       4000-POST-PROCESSING.                                            02940000
      *                                                                 02950000
       4000-POST-PROCESSING-EXIT.                                       02960000
            EXIT.                                                       02970000
      *                                                                 02980000
       5000-CLEAN-UP.                                                   02990000
            PERFORM 5010-RETURN                                         03000000
               THRU 5010-RETURN-EXIT                                    03010000
            .                                                           03020000
      *                                                                 03030000
       5000-CLEAN-UP-EXIT.                                              03040000
            EXIT.                                                       03050000
      *                                                                 03060000
       5010-RETURN.                                                     03070000
            EXEC CICS RETURN END-EXEC                                   03080000
            .                                                           03090000
       5010-RETURN-EXIT.                                                03100000
            EXIT.                                                       03110000
      *                                                                 03120000
       5020-RETURN-TRANS.                                               03130000
            INITIALIZE WS-COMMAREA-SELF                                 03140000
            MOVE 'N' TO WS-FIRST-SEND OF WS-COMMAREA-SELF               03150000
            MOVE LK-OPTION TO WS-OPTION OF WS-COMMAREA-SELF             03160003
            EXEC CICS RETURN TRANSID('CID1')                            03170000
                      COMMAREA(WS-COMMAREA-SELF)                        03180000
            END-EXEC                                                    03190000
            .                                                           03200000
       5020-RETURN-TRANS-EXIT.                                          03210000
            EXIT.                                                       03220000
