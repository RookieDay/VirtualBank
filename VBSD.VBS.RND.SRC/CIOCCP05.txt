       IDENTIFICATION DIVISION.                                         00010000
       PROGRAM-ID. CIOCCP05.                                            00020001
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
       77 WS-BEGIN              PIC X(17) VALUE 'CIOCCP05 WS BEGIN'.    00190001
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
       COPY CICP05.                                                     00350006
      *MAP CONTROL                                                      00360000
       COPY DFHBMSCA.                                                   00370000
      *CICS FUNCTION KEYS                                               00380000
       COPY DFHAID.                                                     00390000
      *CIMENU                                                           00400000
       COPY CIMENU.                                                     00410000
      *                                                                 00420000
      *CIC0006I                                                         00420115
       COPY CIC0006I.                                                   00420215
       COPY CIC0006O.                                                   00420315
      *CIC0012                                                          00421015
       COPY CIC0012I.                                                   00422015
       COPY CIC0012O.                                                   00422115
      *                                                                 00423008
      *CIC0013I                                                         00424008
       COPY CIC0013I.                                                   00425008
       COPY CIC0013O.                                                   00426008
      *                                                                 00427008
      *CIC0018I                                                         00428018
       COPY CIC0018I.                                                   00429018
       COPY CIC0018O.                                                   00429118
       01 WS-SRV-COMMAREA.                                              00430000
      *SERVICE REQUEST/RESPONSE COMMAREA                                00440000
       COPY SD01WS.                                                     00450000
      *                                                                 00460000
       01 WS-COMMAREA.                                                  00470000
          05 WS-FIRST-SEND      PIC X(1).                               00480000
          05 WS-APPLID          PIC 9(13).                              00492003
       01 WS-COMMAREA-SELF.                                             00495000
          05 WS-FIRST-SEND      PIC X(1).                               00496000
       77 WS-END                PIC X(15) VALUE 'CIOCCP05 WS END'.      00500001
      *                                                                 00510000
       LINKAGE SECTION.                                                 00520000
       01 DFHCOMMAREA.                                                  00530000
          05 LK-FIRST-SEND      PIC X(1).                               00550008
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
            IF LK-FIRST-SEND = 'Y'                                      00800015
               MOVE LOW-VALUES TO CICP05O                               00810015
               SET WS-MAP-ERASE TO TRUE                                 00820015
               PERFORM 3030-SEND-MAP                                    00830015
                  THRU 3030-SEND-MAP-EXIT                               00840015
      * NOT FIRST SHOW                                                  00850000
            ELSE                                                        00860015
                  MOVE LOW-VALUES TO CICP05I                            00880015
                  EXEC CICS RECEIVE MAP('CICP05')                       00890015
                                   MAPSET('CICP05')                     00900015
                                   INTO(CICP05I)                        00910015
                                   RESP(WS-RESP-CODE)                   00920015
                  END-EXEC                                              00930015
            END-IF                                                      00950015
      *     .                                                           00960015
      *     IF EIBCALEN = 0                                             00961015
      *        MOVE LOW-VALUES TO CICP05O                               00962015
      *        SET WS-MAP-ERASE TO TRUE                                 00963015
      *        PERFORM 3030-SEND-MAP                                    00964015
      *           THRU 3030-SEND-MAP-EXIT                               00965015
      * NOT FIRST SHOW                                                  00966000
      *     ELSE                                                        00967015
      *        IF SDCA-CICS-SECONDENTER                                 00968015
      *           MOVE LOW-VALUES TO CICP05I                            00969015
      *           EXEC CICS RECEIVE MAP('CICP05')                       00969115
      *                            MAPSET('CICP05')                     00969215
      *                            INTO(CICP05I)                        00969315
      *                            RESP(WS-RESP-CODE)                   00969415
      *           END-EXEC                                              00969515
      *        END-IF                                                   00969615
      *     END-IF                                                      00969715
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
            EVALUATE EIBAID                                             01310010
                WHEN DFHPF1                                             01320010
                     EXEC CICS                                          01330000
                          XCTL PROGRAM('CIOCCP00')                      01340002
                               RESP(WS-RESP-CODE)                       01350000
                     END-EXEC                                           01360000
                     IF WS-RESP-CODE NOT = DFHRESP(NORMAL)              01370000
                        MOVE 'PROGRAM CIOCCP00 IS NOT AVAILABLE'        01380002
                                TO MSGO                                 01390000
                        SET WS-MAP-DATAONLY TO TRUE                     01400000
                        PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT   01410000
                     END-IF                                             01420000
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
                WHEN DFHPF6                                             01650016
                     EXEC CICS                                          01650116
                          XCTL PROGRAM('CIOCCP06')                      01650216
                               RESP(WS-RESP-CODE)                       01650316
                     END-EXEC                                           01650416
                     IF WS-RESP-CODE NOT = DFHRESP(NORMAL)              01650516
                        MOVE 'PROGRAM CIOCCP06 IS NOT AVAILABLE'        01650616
                                TO MSGO                                 01650716
                        SET WS-MAP-DATAONLY TO TRUE                     01650816
                        PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT   01650916
                     END-IF                                             01651016
      *                  PERFORM 3020-APPLID-CHECK                      01651112
      *                     THRU 3020-APPLID-CHECK-EXIT                 01651212
                WHEN DFHPF9                                             01652011
                     MOVE LOW-VALUES TO CICP05O                         01660003
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
                         PERFORM 3020-APPLID-CHECK                      01800006
                            THRU 3020-APPLID-CHECK-EXIT                 01810006
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
            IF (APPIDL = 0)                                             01945011
               MOVE 'FIELDS CAN NOT BE EMPTY' TO MSGO                   01946013
               SET WS-MAP-DATAONLY TO TRUE                              01947000
               PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT            01948000
            END-IF                                                      01949000
            IF (APPIDI IS NOT NUMERIC)                                  01960003
               MOVE 'APPLICATION ID MUEST BE NUMBER' TO MSGO            01970003
               SET WS-MAP-DATAONLY TO TRUE                              01980000
               PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT            01990000
            END-IF                                                      02000000
            .                                                           02150000
      *                                                                 02160000
       3010-CHECK-INPUT-EXIT.                                           02170000
            EXIT.                                                       02180000
      *                                                                 02190000
       3020-APPLID-CHECK.                                               02200006
            INITIALIZE SDCA-SERVICE-COMMAREA                            02260004
            MOVE 'VBS.CI.CUSTAPPL.INQ' TO SD-SRV-NAME                   02270015
            INITIALIZE CIC0012I-REC                                     02280015
            MOVE APPIDI       TO CIC0012I-APPL-ID                       02290015
            MOVE CIC0012I-REC TO SD-SRV-INPUT-DATA                      02300015
            EXEC CICS                                                   02310004
                 LINK                                                   02320004
                 PROGRAM(WS-PGM-SRV-DRIVER)                             02330004
                 COMMAREA(WS-SRV-COMMAREA)                              02340004
                 RESP(WS-RESP-CODE)                                     02350004
            END-EXEC                                                    02360004
            EVALUATE WS-RESP-CODE                                       02370004
                WHEN DFHRESP(NORMAL)                                    02380004
                     IF SD-RESP-CODE EQUAL ZEROS                        02390013
                        INITIALIZE CIC0012O-REC                         02400015
                        MOVE SD-SRV-OUTPUT-DATA TO CIC0012O-REC         02410015
                        IF CIC0012O-STATUS NOT = 005                    02410120
                           MOVE 'STATUS NOT CORRECT' TO MSGO            02410315
                           SET WS-MAP-DATAONLY TO TRUE                  02410415
                           PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT02410515
                        END-IF                                          02410615
                        PERFORM 3021-PROD-CARD THRU 3021-PROD-CARD-EXIT 02411119
                        PERFORM 3022-ACCT-ADD  THRU 3022-ACCT-ADD-EXIT  02411219
                     ELSE                                               02412013
      *                  IF SD-RESP-CODE NE ZEROS                       02413013
                        MOVE SD-RESP-ADDITIONAL TO MSGO                 02413113
                        SET WS-MAP-DATAONLY TO TRUE                     02413213
                        PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT   02413313
      *                  END-IF                                         02414013
                     END-IF                                             02420013
                WHEN OTHER                                              02421004
                     MOVE 'SERVICE PROGRAM ERROR' TO MSGO               02422013
                     SET WS-MAP-DATAONLY TO TRUE                        02423004
                     PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT      02424004
            END-EVALUATE                                                02430004
            .                                                           02440004
      *                                                                 02660000
       3020-APPLID-CHECK-EXIT.                                          02670006
            EXIT.                                                       02680000
      *                                                                 02680119
       3021-PROD-CARD.                                                  02680219
            INITIALIZE SDCA-SERVICE-COMMAREA                            02680319
            MOVE 'VBS.CI.CREDCARD.ADD' TO SD-SRV-NAME                   02680419
            INITIALIZE CIC0013I-REC                                     02680519
            MOVE CIC0012O-CUST-ID-NUMBER TO CIC0013I-CUST-NUMB          02680619
            MOVE CIC0012O-NAME      TO CIC0013I-CUST-NAME               02680719
            MOVE CIC0013I-REC  TO SD-SRV-INPUT-DATA                     02680819
            EXEC CICS                                                   02680919
                 LINK                                                   02681019
                 PROGRAM(WS-PGM-SRV-DRIVER)                             02681119
                 COMMAREA(WS-SRV-COMMAREA)                              02681219
                 RESP(WS-RESP-CODE)                                     02681319
            END-EXEC                                                    02681419
            EVALUATE WS-RESP-CODE                                       02681519
                WHEN DFHRESP(NORMAL)                                    02681619
                     IF SD-RESP-CODE EQUAL ZEROS                        02681719
                        MOVE 'CREDIT CARD CREATED SUCCESSFULLY' TO MSGO 02681819
                        INITIALIZE CIC0013O-REC                         02681919
                        MOVE SD-SRV-OUTPUT-DATA TO CIC0013O-REC         02682019
                        MOVE CIC0013O-NUMB TO PRICARDI                  02682119
                        PERFORM 3023-UPDATE-STETUS THRU                 02682219
                                3023-UPDATE-STETUS-EXIT                 02682319
                     ELSE                                               02682419
                        MOVE SD-RESP-ADDITIONAL TO MSGO                 02682519
                     END-IF                                             02682619
                WHEN OTHER                                              02682719
                     MOVE 'SERVICE PROGRAM ERROR' TO MSGO               02682819
            END-EVALUATE                                                02682919
            .                                                           02683319
      *                                                                 02683419
       3021-PROD-CARD-EXIT.                                             02683519
            EXIT.                                                       02683619
      *                                                                 02683706
       3022-ACCT-ADD.                                                   02683819
            INITIALIZE SDCA-SERVICE-COMMAREA                            02683918
            MOVE 'VBS.CI.ACCOUNTS.ADD' TO SD-SRV-NAME                   02684018
            INITIALIZE CIC0018I-REC                                     02684118
            MOVE CIC0013O-ACCT-NUMB      TO CIC0018I-NUMB               02684222
            MOVE CIC0012O-CUST-ID-NUMBER TO CIC0018I-CUST-NUMB          02684318
            MOVE CIC0012O-FINAL-LIMIT    TO CIC0018I-LIMIT              02684418
            MOVE CIC0012O-BILL-DATE      TO CIC0018I-BILL-DATE          02684518
            MOVE CIC0012O-BILL-TYPE      TO CIC0018O-BILL-TYPE          02684618
            MOVE CIC0018I-REC  TO SD-SRV-INPUT-DATA                     02684718
            EXEC CICS                                                   02684818
                 LINK                                                   02684918
                 PROGRAM(WS-PGM-SRV-DRIVER)                             02685018
                 COMMAREA(WS-SRV-COMMAREA)                              02685118
                 RESP(WS-RESP-CODE)                                     02685218
            END-EXEC                                                    02685318
            EVALUATE WS-RESP-CODE                                       02685418
                WHEN DFHRESP(NORMAL)                                    02685518
                     IF SD-RESP-CODE EQUAL ZEROS                        02685618
                        MOVE 'ACCOUNTS CREATED SUCCESSFULLY' TO MSGO    02685718
                     ELSE                                               02685818
                        MOVE SD-RESP-ADDITIONAL TO MSGO                 02685918
                     END-IF                                             02686018
                WHEN OTHER                                              02686118
                     MOVE 'SERVICE PROGRAM ERROR' TO MSGO               02686218
            END-EVALUATE                                                02686318
            SET WS-MAP-DATAONLY TO TRUE                                 02686418
            PERFORM 3030-SEND-MAP                                       02686518
               THRU 3030-SEND-MAP-EXIT                                  02686618
            .                                                           02686718
      *                                                                 02687018
       3022-ACCT-ADD-EXIT.                                              02688219
            EXIT.                                                       02688317
      *                                                                 02692000
       3023-UPDATE-STETUS.                                              02692115
            INITIALIZE SDCA-SERVICE-COMMAREA                            02692215
            MOVE 'VBS.CI.APPLICAN.UPD' TO SD-SRV-NAME                   02692315
            INITIALIZE CIC0006I-REC                                     02692415
            MOVE CIC0012O-APPL-REC TO CIC0006I-REC                      02692515
            MOVE 006               TO CIC0006I-STATUS                   02692615
            MOVE CIC0006I-REC  TO SD-SRV-INPUT-DATA                     02692715
            EXEC CICS                                                   02692815
                 LINK                                                   02692915
                 PROGRAM(WS-PGM-SRV-DRIVER)                             02693015
                 COMMAREA(WS-SRV-COMMAREA)                              02693115
                 RESP(WS-RESP-CODE)                                     02693215
            END-EXEC                                                    02693315
            EVALUATE WS-RESP-CODE                                       02693415
                WHEN DFHRESP(NORMAL)                                    02693515
                     IF SD-RESP-CODE NOT = ZEROS                        02693615
                        MOVE SD-RESP-ADDITIONAL TO MSGO                 02693715
                     END-IF                                             02693815
                WHEN OTHER                                              02693915
                     MOVE 'SERVICE PROGRAM ERROR' TO MSGO               02694015
            END-EVALUATE                                                02694115
            .                                                           02694515
      *                                                                 02694615
       3023-UPDATE-STETUS-EXIT.                                         02694715
            EXIT.                                                       02694815
      *                                                                 02695015
       3030-SEND-MAP.                                                   02700000
            MOVE ATTR-PROT-SKIP-MDT       TO PRICARDA                   02701014
            MOVE ATTR-PROT-SKIP-MDT       TO SECCARDA                   02702014
            PERFORM 1010-ASK-TIME-DATE                                  02710000
               THRU 1010-ASK-TIME-DATE-EXIT                             02720000
            EVALUATE TRUE                                               02730000
                WHEN WS-MAP-ERASE                                       02740000
                     EXEC CICS SEND                                     02750000
                          MAP('CICP05')                                 02760006
                          MAPSET('CICP05')                              02770006
                          FROM(CICP05O)                                 02780008
                          ERASE                                         02790000
                     END-EXEC                                           02800000
                WHEN WS-MAP-DATAONLY                                    02810000
                     EXEC CICS SEND                                     02820000
                          MAP('CICP05')                                 02830006
                          MAPSET('CICP05')                              02840006
                          FROM(CICP05O)                                 02850008
                          DATAONLY                                      02860000
                     END-EXEC                                           02870000
            END-EVALUATE                                                02880000
      *     MOVE 'N' TO LK-FIRST-SEND                                   02890000
      *     MOVE '1' TO WS-ENTER-FLAG                                   02891015
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
            INITIALIZE WS-COMMAREA-SELF                                 03151010
            MOVE 'N' TO WS-FIRST-SEND OF WS-COMMAREA-SELF               03152010
      *     MOVE '1'    TO WS-OPTION OF WS-COMMAREA-SELF                03153015
            EXEC CICS RETURN TRANSID('CIC5')                            03160011
                      COMMAREA(WS-COMMAREA-SELF)                        03171015
            END-EXEC                                                    03180000
            .                                                           03190000
       5020-RETURN-TRANS-EXIT.                                          03200000
            EXIT.                                                       03210000
