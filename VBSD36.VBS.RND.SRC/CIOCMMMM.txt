       IDENTIFICATION DIVISION.                                         00010000
       PROGRAM-ID. CIOCTA03.                                            00020000
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
       77 WS-BEGIN              PIC X(17) VALUE 'CIOCTA03 WS BEGIN'.    00190000
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
       COPY CITA03.                                                     00350000
      *MAP CONTROL                                                      00360000
       COPY DFHBMSCA.                                                   00370000
      *CICS FUNCTION KEYS                                               00380000
       COPY DFHAID.                                                     00390000
      *CIMENU                                                           00400000
       COPY CIMENU.                                                     00410000
      *                                                                 00420000
      *CIC0015                                                          00420100
       COPY CIC0015I.                                                   00420200
       COPY CIC0015O.                                                   00420300
      *CIC0023                                                          00421000
       COPY CIC0023I.                                                   00422000
       COPY CIC0023O.                                                   00422100
      *                                                                 00423000
       01 WS-SRV-COMMAREA.                                              00430000
      *SERVICE REQUEST/RESPONSE COMMAREA                                00440000
       COPY SD01WS.                                                     00450000
      *                                                                 00460000
       01 WS-COMMAREA.                                                  00470000
          05 WS-FIRST-SEND      PIC X(1).                               00480000
          05 WS-OPTION          PIC 9(03).                              00492000
      *                                                                 00493000
       01 WS-COMMAREA-TA00.                                             00494000
          05 WS-FIRST-SEND      PIC X(1).                               00494100
          05 WS-OPTION          PIC 9(03).                              00494200
      *                                                                 00494300
       01 WS-CURR-EXPIRY-DATE.                                          00494400
          05 WS-YYYY            PIC 9(4).                               00494500
          05 WS-CHAR-YYYY       PIC X(1) VALUE '/'.                     00494600
          05 WS-MONTH           PIC 9(2).                               00494700
          05 WS-CHAR-MON        PIC X(1) VALUE '/'.                     00494800
          05 WS-DAY             PIC 9(2).                               00494900
      *                                                                 00495000
       01 WS-COMMAREA-TA04.                                             00495100
          05 WS-FIRST-SEND      PIC X(1).                               00495200
          05 WS-CARD-NUMB       PIC 9(16).                              00495300
      *                                                                 00495400
       01 WS-COMMAREA-TA05.                                             00495500
          05 WS-FIRST-SEND      PIC X(1).                               00495600
          05 WS-CARD-NUMB       PIC 9(16).                              00495700
       01 WS-COMMAREA-SELF.                                             00495800
          05 WS-FIRST-SEND      PIC X(1).                               00496000
          05 WS-OPTION          PIC 9(03).                              00496100
      * SAVE                                                            00497002
       01 WS-AUTH-CODE          PIC 9(06).                              00498002
       01 WS-SEED               PIC 9(01) COMP.                         00498102
       01 WS-AU-RESP-CODE       PIC 9(03).                              00498202
       01 WS-DATE               PIC 9(08).                              00498302
       01 WS-TIME               PIC 9(06).                              00498402
      *                                                                 00499000
       77 WS-END                PIC X(15) VALUE 'CIOCTA03 WS END'.      00500000
      *                                                                 00510000
       LINKAGE SECTION.                                                 00520000
       01 DFHCOMMAREA.                                                  00530000
          05 LK-FIRST-SEND      PIC X(1).                               00550000
          05 LK-OPTION          PIC 9(3).                               00551000
       COPY SD00WS.                                                     00552000
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
            IF LK-FIRST-SEND = 'Y'                                      00800000
               MOVE LOW-VALUES TO CITA03O                               00810000
               SET WS-MAP-ERASE TO TRUE                                 00820000
               PERFORM 3030-SEND-MAP                                    00830000
                  THRU 3030-SEND-MAP-EXIT                               00840000
      * NOT FIRST SHOW                                                  00850000
            ELSE                                                        00860000
                  MOVE LOW-VALUES TO CITA03I                            00880000
                  EXEC CICS RECEIVE MAP('CITA03')                       00890000
                                   MAPSET('CITA03')                     00900000
                                   INTO(CITA03I)                        00910000
                                   RESP(WS-RESP-CODE)                   00920000
                  END-EXEC                                              00930000
            END-IF                                                      00950000
      *     .                                                           00960000
      *     IF EIBCALEN = 0                                             00961000
      *        MOVE LOW-VALUES TO CITA03O                               00962000
      *        SET WS-MAP-ERASE TO TRUE                                 00963000
      *        PERFORM 3030-SEND-MAP                                    00964000
      *           THRU 3030-SEND-MAP-EXIT                               00965000
      * NOT FIRST SHOW                                                  00966000
      *     ELSE                                                        00967000
      *        IF SDCA-CICS-SECONDENTER                                 00968000
      *           MOVE LOW-VALUES TO CITA03I                            00969000
      *           EXEC CICS RECEIVE MAP('CITA03')                       00969100
      *                            MAPSET('CITA03')                     00969200
      *                            INTO(CITA03I)                        00969300
      *                            RESP(WS-RESP-CODE)                   00969400
      *           END-EXEC                                              00969500
      *        END-IF                                                   00969600
      *     END-IF                                                      00969700
            .                                                           00969800
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
            EVALUATE EIBAID ALSO TRUE                                   01310000
                WHEN ANY ALSO LK-OPTION = 001                           01311000
                     EXEC CICS                                          01312000
                          XCTL PROGRAM('CIOCTA00')                      01313000
                               RESP(WS-RESP-CODE)                       01314000
                     END-EXEC                                           01315000
                     IF WS-RESP-CODE NOT = DFHRESP(NORMAL)              01316000
                        MOVE 'PROGRAM CIOCTA00 IS NOT AVAILABLE'        01317000
                                TO MSGO                                 01318000
                        SET WS-MAP-DATAONLY TO TRUE                     01319000
                        PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT   01319100
                     END-IF                                             01319200
                WHEN DFHPF1 ALSO ANY                                    01320000
                     EXEC CICS                                          01330000
                          XCTL PROGRAM('CIOCTA00')                      01340000
                               RESP(WS-RESP-CODE)                       01350000
                     END-EXEC                                           01360000
                     IF WS-RESP-CODE NOT = DFHRESP(NORMAL)              01370000
                        MOVE 'PROGRAM CIOCTA00 IS NOT AVAILABLE'        01380000
                                TO MSGO                                 01390000
                        SET WS-MAP-DATAONLY TO TRUE                     01400000
                        PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT   01410000
                     END-IF                                             01420000
                WHEN DFHPF3 ALSO ANY                                    01430000
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
                WHEN DFHCLEAR ALSO ANY                                  01560000
                     EXEC CICS                                          01570000
                           SEND CONTROL                                 01580000
                           CURSOR                                       01590000
                           ERASE                                        01600000
                           FREEKB                                       01610000
                           ALARM                                        01620000
                     END-EXEC                                           01630000
                     PERFORM 5010-RETURN THRU 5010-RETURN-EXIT          01640000
                WHEN DFHPF5 ALSO ANY                                    01650000
                  INITIALIZE WS-COMMAREA-TA04                           01650100
                  MOVE 'Y'        TO WS-FIRST-SEND OF WS-COMMAREA-TA04  01650200
                  MOVE CDNUMO     TO WS-CARD-NUMB  OF WS-COMMAREA-TA04  01650400
                     EXEC CICS                                          01650800
                          XCTL PROGRAM('CIOCTA04')                      01650900
                               COMMAREA(WS-COMMAREA-TA04)               01651000
                               RESP(WS-RESP-CODE)                       01651100
                     END-EXEC                                           01651200
                     IF WS-RESP-CODE NOT = DFHRESP(NORMAL)              01651300
                        MOVE 'PROGRAM CIOCTA04 IS NOT AVAILABLE'        01651400
                                TO MSGO                                 01651500
                        SET WS-MAP-DATAONLY TO TRUE                     01651600
                        PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT   01651700
                     END-IF                                             01651800
                WHEN DFHPF6 ALSO ANY                                    01651900
                  INITIALIZE WS-COMMAREA-TA05                           01652000
                  MOVE 'Y'        TO WS-FIRST-SEND OF WS-COMMAREA-TA05  01652100
                  MOVE CDNUMO     TO WS-CARD-NUMB  OF WS-COMMAREA-TA05  01652200
                     EXEC CICS                                          01652300
                          XCTL PROGRAM('CIOCTA05')                      01652400
                               COMMAREA(WS-COMMAREA-TA05)               01652500
                               RESP(WS-RESP-CODE)                       01652600
                     END-EXEC                                           01652700
                     IF WS-RESP-CODE NOT = DFHRESP(NORMAL)              01652800
                        MOVE 'PROGRAM CIOCTA05 IS NOT AVAILABLE'        01652900
                                TO MSGO                                 01653000
                        SET WS-MAP-DATAONLY TO TRUE                     01653100
                        PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT   01653200
                     END-IF                                             01653300
                WHEN DFHPF9 ALSO ANY                                    01654000
                     MOVE LOW-VALUES TO CITA03O                         01660000
                     SET WS-MAP-ERASE TO TRUE                           01670000
                     PERFORM 3030-SEND-MAP                              01680000
                        THRU 3030-SEND-MAP-EXIT                         01690000
                 WHEN DFHENTER ALSO LK-OPTION = 000                     01700000
                      IF WS-RESP-CODE NOT EQUAL DFHRESP(NORMAL)         01710000
                         MOVE 'INVALID REQUEST, CHECK YOUR INPUT.'      01720000
                              TO MSGO                                   01730000
                         SET WS-MAP-DATAONLY TO TRUE                    01740000
                         PERFORM 3030-SEND-MAP                          01750000
                            THRU 3030-SEND-MAP-EXIT                     01760000
                      ELSE                                              01770000
                         PERFORM 3010-CHECK-INPUT                       01780000
                            THRU 3010-CHECK-INPUT-EXIT                  01790000
                         PERFORM 3020-CARD-NUMB-CHECK                   01800000
                            THRU 3020-CARD-NUMB-CHECK-EXIT              01810000
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
            IF COMMUL NOT = 0                                           01933000
               INITIALIZE CIMENU-REC                                    01940000
               MOVE COMMUI TO CIMENU-TRANSID                            01940100
               EXEC CICS READ                                           01940200
                    FILE('CIMENU')                                      01940300
                    INTO(CIMENU-REC)                                    01940400
                    RIDFLD(CIMENU-TRANSID)                              01940500
                    RESP(WS-RESP-CODE)                                  01940600
               END-EXEC                                                 01940700
               EVALUATE WS-RESP-CODE                                    01940800
                   WHEN DFHRESP(NORMAL)                                 01940900
                        EXEC CICS                                       01941000
                             XCTL PROGRAM(CIMENU-PGM)                   01941100
                             COMMAREA(WS-COMMAREA)                      01941200
                             RESP(WS-RESP-CODE)                         01941300
                        END-EXEC                                        01941400
                        IF WS-RESP-CODE NOT = DFHRESP(NORMAL)           01941500
                        STRING 'PROGRAM ' DELIMITED BY SIZE             01941600
                               CIMENU-PGM DELIMITED BY SPACE            01941700
                               ' IS NOT AVAILABLE' DELIMITED BY SIZE    01941800
                               INTO MSGO                                01941900
                           SET WS-MAP-DATAONLY TO TRUE                  01942000
                           PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT01942100
                        END-IF                                          01942200
                   WHEN DFHRESP(NOTFND)                                 01942300
                        MOVE 'INVALID TRANSATION ID!' TO MSGO           01942400
                        SET WS-MAP-DATAONLY TO TRUE                     01942500
                        PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT   01942600
                   WHEN OTHER                                           01942700
                        MOVE 'CIMENU FILE ERROR!' TO MSGO               01942800
                        SET WS-MAP-DATAONLY TO TRUE                     01942900
                        PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT   01943000
               END-EVALUATE                                             01943100
            END-IF                                                      01944000
            IF (CDNUML = 0 OR AUTIDL = 0 OR VILIDL = 0                  01945000
                 OR TRATPL = 0 OR TRDATL = 0 OR CVV2L = 0)              01946000
               MOVE 'FIELDS CAN NOT BE EMPTY' TO MSGO                   01990000
               SET WS-MAP-DATAONLY TO TRUE                              02000000
               PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT            02010000
            END-IF                                                      02020000
            IF (TRATPI NOT = 01 AND TRATPI NOT = 02                     02130000
                                 AND TRATPI NOT = 03)                   02131000
               MOVE 'INVAILD TRANSACTION TYPE' TO MSGO                  02140001
               SET WS-MAP-DATAONLY TO TRUE                              02150000
               PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT            02151000
            END-IF                                                      02152000
            IF (CDNUMI IS NOT NUMERIC OR TRDATI IS NOT NUMERIC OR       02153003
                TRATPI IS NOT NUMERIC OR CVV2I  IS NOT NUMERIC)         02153103
               MOVE 'CARDNUMB OR TRAN AMOUNT MUEST BE NUMBER' TO MSGO   02154001
               SET WS-MAP-DATAONLY TO TRUE                              02155001
               PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT            02156001
            END-IF                                                      02157001
            .                                                           02159600
      *                                                                 02160000
       3010-CHECK-INPUT-EXIT.                                           02170000
            EXIT.                                                       02180000
      *                                                                 02190000
       3020-CARD-NUMB-CHECK.                                            02200000
            INITIALIZE SDCA-SERVICE-COMMAREA                            02260000
            MOVE 'VBS.CI.CREDCARD.INQ' TO SD-SRV-NAME                   02270000
            INITIALIZE CIC0015I-REC                                     02280000
            MOVE CDNUMI       TO CIC0015I-NUMB                          02290000
            MOVE CIC0015I-NUMB TO SD-SRV-INPUT-DATA                     02300000
            EXEC CICS                                                   02310000
                 LINK                                                   02320000
                 PROGRAM(WS-PGM-SRV-DRIVER)                             02330000
                 COMMAREA(WS-SRV-COMMAREA)                              02340000
                 RESP(WS-RESP-CODE)                                     02350000
            END-EXEC                                                    02360000
            EVALUATE WS-RESP-CODE                                       02370000
                WHEN DFHRESP(NORMAL)                                    02380000
                     IF SD-RESP-CODE EQUAL ZEROS                        02390000
                        PERFORM 3021-CHECK-INPUT                        02411100
                                         THRU 3021-CHECK-INPUT-EXIT     02411200
                     ELSE                                               02412000
      *                  IF SD-RESP-CODE NE ZEROS                       02413000
                        MOVE SD-RESP-ADDITIONAL TO MSGO                 02413100
                        SET WS-MAP-DATAONLY TO TRUE                     02413200
                        PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT   02413300
      *                  END-IF                                         02414000
                     END-IF                                             02420000
                WHEN OTHER                                              02421000
                     MOVE 'SERVICE PROGRAM ERROR' TO MSGO               02422000
                     SET WS-MAP-DATAONLY TO TRUE                        02423000
                     PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT      02424000
            END-EVALUATE                                                02430000
            .                                                           02440000
      *                                                                 02660000
       3020-CARD-NUMB-CHECK-EXIT.                                       02670000
            EXIT.                                                       02680000
      *                                                                 02680100
      *                                                                 02680200
       3021-CHECK-INPUT.                                                02680300
            INITIALIZE CIC0015O-REC                                     02680400
            MOVE SD-SRV-OUTPUT-DATA TO CIC0015O-REC                     02680500
            INITIALIZE WS-AU-RESP-CODE                                  02680700
            UNSTRING CIC0015O-CURR-EXPIRY-DATE DELIMITED BY '-'         02680900
                   INTO WS-YYYY WS-MONTH WS-DAY                         02681000
            END-UNSTRING                                                02681100
            IF CIC0015O-STATUS NOT = 002                                02681400
               MOVE 001                  TO WS-AU-RESP-CODE             02681500
            ELSE                                                        02681600
            IF CIC0015O-CVV2   NOT = CVV2I                              02681700
               MOVE 002                TO WS-AU-RESP-CODE               02681800
            ELSE                                                        02681900
            IF WS-CURR-EXPIRY-DATE       NOT = VILIDI                   02682000
               MOVE 003                TO WS-AU-RESP-CODE               02682100
      *        MOVE WS-CURR-EXPIRY-DATE TO VILIDI                       02682200
            ELSE                                                        02682300
               MOVE 000            TO WS-AU-RESP-CODE                   02682400
            END-IF                                                      02682500
               PERFORM 3022-TIMESTAMP   THRU 3022-TIMESTAMP-EXIT        02682600
               PERFORM 3023-AUTHLOG-ADD THRU 3023-AUTHLOG-ADD-EXIT      02682700
            .                                                           02682800
       3021-CHECK-INPUT-EXIT.                                           02682900
            EXIT.                                                       02683000
      *                                                                 02683100
       3022-TIMESTAMP.                                                  02683200
            EXEC CICS                                                   02683500
                 ASKTIME                                                02683600
                 ABSTIME(WS-GETTIME)                                    02683700
            END-EXEC                                                    02683800
            EXEC CICS                                                   02683900
                 FORMATTIME                                             02684000
                 ABSTIME(WS-GETTIME)                                    02684100
                 YYYYMMDD(WS-DATE)                                      02684300
            END-EXEC                                                    02684400
            EXEC CICS                                                   02684500
                 FORMATTIME                                             02684600
                 ABSTIME(WS-GETTIME)                                    02684700
                 TIME(WS-TIME)                                          02684900
            END-EXEC                                                    02685000
            .                                                           02685300
       3022-TIMESTAMP-EXIT.                                             02685700
            EXIT.                                                       02685800
      *                                                                 02685900
       3023-AUTHLOG-ADD.                                                02686000
            INITIALIZE SDCA-SERVICE-COMMAREA                            02686100
            MOVE 'VBS.CI.AUTHLOGS.ADD' TO SD-SRV-NAME                   02686200
            INITIALIZE CIC0023I-REC                                     02686300
            MOVE CIC0015O-NUMB           TO CIC0023I-NUMB               02686400
            MOVE WS-DATE                 TO CIC0023I-DATE               02686500
            MOVE WS-TIME                 TO CIC0023I-TIME               02686600
            INITIALIZE WS-AUTH-CODE                                     02686700
            COMPUTE WS-AUTH-CODE = FUNCTION RANDOM(WS-SEED) * 999999 + 102686802
            MOVE WS-AUTH-CODE            TO CIC0023I-AUTH-CODE          02686900
      *     MOVE WS-AUTH-CODE            TO TRDATI                      02687002
            MOVE WS-AU-RESP-CODE         TO CIC0023I-RESP-CODE          02687102
            MOVE TRATPI                  TO CIC0023I-TRAN-TYPE          02687202
            MOVE TRDATI                  TO CIC0023I-TRAN-AMOUNT        02687302
            MOVE AUTIDI                  TO CIC0023I-AUTH-ID            02687402
            MOVE CIC0023I-REC  TO SD-SRV-INPUT-DATA                     02687502
            EXEC CICS                                                   02687602
                 LINK                                                   02687702
                 PROGRAM(WS-PGM-SRV-DRIVER)                             02687802
                 COMMAREA(WS-SRV-COMMAREA)                              02687902
                 RESP(WS-RESP-CODE)                                     02688002
            END-EXEC                                                    02688102
            EVALUATE WS-RESP-CODE                                       02688202
                WHEN DFHRESP(NORMAL)                                    02688302
                     IF SD-RESP-CODE EQUAL ZEROS                        02688402
                        INITIALIZE WS-COMMAREA                          02688504
                        MOVE 'Y' TO WS-FIRST-SEND OF WS-COMMAREA        02688605
                        MOVE 001 TO LK-OPTION                           02688702
                        EVALUATE WS-AU-RESP-CODE                        02688802
                           WHEN 000                                     02688902
                                MOVE 'AUTHORIZE SUCCESSFULLY'           02689002
                                                      TO MSGO           02689102
                           WHEN 001                                     02689202
                                MOVE 'AUTHORIZE FAIL,CARD NOT ACTIVE'   02689302
                                                       TO MSGO          02689402
                           WHEN 002                                     02689502
                                MOVE 'AUTHORIZE FAIL,CVV2 NOT CORRECT'  02689602
                                                       TO MSGO          02689702
                           WHEN 003                                     02689802
                                MOVE 'AUTHORIZE FAIL,EXPIRY DATE NOT COR02689902
      -                              'RECT'            TO MSGO          02690002
                           WHEN OTHER                                   02690102
                                CONTINUE                                02690202
                        END-EVALUATE                                    02690302
                     ELSE                                               02690402
                        MOVE SD-RESP-ADDITIONAL TO MSGO                 02690502
                     END-IF                                             02690602
                WHEN OTHER                                              02690702
                     MOVE 'SERVICE PROGRAM ERROR' TO MSGO               02690802
            END-EVALUATE                                                02690902
            SET WS-MAP-DATAONLY TO TRUE                                 02691002
            PERFORM 3030-SEND-MAP                                       02691102
               THRU 3030-SEND-MAP-EXIT                                  02691202
            .                                                           02691302
      *                                                                 02691402
       3023-AUTHLOG-ADD-EXIT.                                           02691502
            EXIT.                                                       02692000
      *                                                                 02695000
       3030-SEND-MAP.                                                   02700000
            PERFORM 1010-ASK-TIME-DATE                                  02710000
               THRU 1010-ASK-TIME-DATE-EXIT                             02720000
            IF LK-OPTION = 001                                          02721000
               MOVE 'PRESS ANY KEY TO RETURN' TO KEYO                   02722000
            END-IF                                                      02727000
            EVALUATE TRUE                                               02730000
                WHEN WS-MAP-ERASE                                       02740000
                     EXEC CICS SEND                                     02750000
                          MAP('CITA03')                                 02760000
                          MAPSET('CITA03')                              02770000
                          FROM(CITA03O)                                 02780000
                          ERASE                                         02790000
                     END-EXEC                                           02800000
                WHEN WS-MAP-DATAONLY                                    02810000
                     EXEC CICS SEND                                     02820000
                          MAP('CITA03')                                 02830000
                          MAPSET('CITA03')                              02840000
                          FROM(CITA03O)                                 02850000
                          DATAONLY                                      02860000
                     END-EXEC                                           02870000
            END-EVALUATE                                                02880000
      *     MOVE 'N' TO LK-FIRST-SEND                                   02890000
      *     MOVE '1' TO WS-ENTER-FLAG                                   02891000
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
            INITIALIZE WS-COMMAREA-SELF                                 03151000
            MOVE 'N' TO WS-FIRST-SEND OF WS-COMMAREA-SELF               03152000
            MOVE LK-OPTION TO WS-OPTION OF WS-COMMAREA-SELF             03153000
            EXEC CICS RETURN TRANSID('CID3')                            03160000
                      COMMAREA(WS-COMMAREA-SELF)                        03171000
            END-EXEC                                                    03180000
            .                                                           03190000
       5020-RETURN-TRANS-EXIT.                                          03200000
            EXIT.                                                       03210000
