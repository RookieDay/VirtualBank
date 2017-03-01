       IDENTIFICATION DIVISION.                                         00010000
       PROGRAM-ID. CIOCCA03.                                            00020001
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
      *2015-01-22    KEVIN     INITIAL VERSION                          00130001
      ***************************************************************** 00140000
       ENVIRONMENT DIVISION.                                            00150000
       DATA DIVISION.                                                   00160000
       WORKING-STORAGE SECTION.                                         00170000
      *                                                                 00180000
       77 WS-BEGIN              PIC X(17) VALUE 'CIOCCA03 WS BEGIN'.    00190001
       01 WS-VAR.                                                       00200000
          05 WS-GETTIME         PIC X(20).                              00210000
          05 WS-DATEOUT         PIC X(10).                              00220000
          05 WS-TIMEOUT         PIC X(8).                               00230000
          05 WS-RESP-CODE       PIC S9(8) COMP.                         00240000
          05 WS-MESSAGE         PIC X(40).                              00250000
          05 WS-ENTER-FLAG      PIC X(1).                               00260000
          05 WS-TRANSID         PIC X(4).                               00270000
          05 IX                 PIC 9(2).                               00280004
       01 WS-MAP-OPTION         PIC X(1).                               00290000
          88 WS-MAP-ERASE       VALUE '0'.                              00300000
          88 WS-MAP-DATAONLY    VALUE '1'.                              00310000
      *                                                                 00320000
      *SCREEN HANDLER                                                   00330000
       COPY SD11WS.                                                     00340000
      * SYMBOLIC MAP                                                    00350000
       COPY CICA03.                                                     00360001
      *MAP CONTROL                                                      00370000
       COPY DFHBMSCA.                                                   00380000
      *CICS FUNCTION KEYS                                               00390000
       COPY DFHAID.                                                     00400000
      *CIMENU                                                           00410000
       COPY CIMENU.                                                     00420000
      *                                                                 00430001
       COPY CIC0009I.                                                   00440005
       COPY CIC0009O.                                                   00450005
      *                                                                 00460004
       COPY CIC0012I.                                                   00470005
       COPY CIC0012O.                                                   00480005
      *                                                                 00490000
       01 WS-SRV-COMMAREA.                                              00500000
      *SERVICE REQUEST/RESPONSE COMMAREA                                00510000
       COPY SD01WS.                                                     00520000
      *                                                                 00530000
       01 WS-COMMAREA-CA07.                                             00563009
          05 WS-FIRST-SEND      PIC X(1).                               00564009
          05 WS-OPTION          PIC 9(3).                               00565009
          05 WS-APPL-ID         PIC 9(13).                              00566009
      *                                                                 00570002
       01 WS-COMMAREA-SELF.                                             00580002
          05 WS-FIRST-SEND      PIC X(1).                               00590002
          05 WS-OPTION          PIC 9(3).                               00600002
          05 WS-APPL-MIN-ID     PIC 9(13).                              00610002
          05 WS-APPL-MAX-ID     PIC 9(13).                              00620002
      *                                                                 00630002
       77 WS-END                PIC X(17) VALUE 'CIOCCA03 WS END'.      00640001
      *                                                                 00650000
       LINKAGE SECTION.                                                 00660000
       01 DFHCOMMAREA.                                                  00670000
          05 LK-FIRST-SEND      PIC X(1).                               00680002
          05 LK-OPTION          PIC 9(3).                               00690002
          05 LK-APPL-MIN-ID     PIC 9(13).                              00700002
          05 LK-APPL-MAX-ID     PIC 9(13).                              00710002
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
      *     IF EIBCALEN = 0                                             00951026
      *        MOVE 'Y' TO LK-FIRST-SEND                                00951226
      *        EVALUATE EIBTRNID                                        00952426
      *            WHEN 'CI02'                                          00952526
      *                 MOVE 1 TO LK-OPTION                             00952626
      *            WHEN 'CI03'                                          00952726
      *                 MOVE 2 TO LK-OPTION                             00952826
      *            WHEN 'CI04'                                          00952926
      *                 MOVE 3 TO LK-OPTION                             00953026
      *            WHEN 'CI05'                                          00953126
      *                 MOVE 4 TO LK-OPTION                             00953226
      *        END-EVALUATE                                             00953326
      *     END-IF                                                      00955026
            IF LK-FIRST-SEND = 'Y'                                      00960000
               INITIALIZE CIC0009I-REC                                  00970002
               MOVE LK-OPTION TO CIC0009I-OPTION                        00980002
               MOVE ZEROS TO CIC0009I-APPL-ID                           00990002
               MOVE 'F8' TO CIC0009I-DIRECTION                          01000002
      *                                                                 01010002
               PERFORM 1020-GET-APPL-LIST                               01040002
                  THRU 1020-GET-APPL-LIST-EXIT                          01050002
      *                                                                 01060013
               MOVE LOW-VALUES TO CICA03O                               01064016
               IF CIC0009O-COUNT = 0                                    01064127
                  MOVE 'NO RECORDS LISTED!' TO MSGO                     01064227
               END-IF                                                   01064327
      *                                                                 01065016
               PERFORM 1030-POPULATE-MAP                                01070002
                  THRU 1030-POPULATE-MAP-EXIT                           01080002
      *                                                                 01090002
               SET WS-MAP-ERASE TO TRUE                                 01100000
               PERFORM 3030-SEND-MAP                                    01110000
                  THRU 3030-SEND-MAP-EXIT                               01120000
      * NOT FIRST SHOW                                                  01130000
            ELSE                                                        01140000
               MOVE LOW-VALUES TO CICA03I                               01150008
               EXEC CICS RECEIVE MAP('CICA03')                          01160008
                         MAPSET('CICA03')                               01170008
                         INTO(CICA03I)                                  01180008
                         RESP(WS-RESP-CODE)                             01190008
               END-EXEC                                                 01200008
            END-IF                                                      01210000
            .                                                           01220000
       1000-INIT-EXIT.                                                  01230000
            EXIT.                                                       01240000
      *                                                                 01250000
       1010-ASK-TIME-DATE.                                              01260000
      *                                                                 01270000
            EXEC CICS                                                   01280000
                 ASKTIME                                                01290000
                 ABSTIME(WS-GETTIME)                                    01300000
            END-EXEC                                                    01310000
            EXEC CICS                                                   01320000
                 FORMATTIME                                             01330000
                 ABSTIME(WS-GETTIME)                                    01340000
                 DATESEP('/')                                           01350000
                 YYYYMMDD(WS-DATEOUT)                                   01360000
            END-EXEC                                                    01370000
            EXEC CICS                                                   01380000
                 FORMATTIME                                             01390000
                 ABSTIME(WS-GETTIME)                                    01400000
                 TIMESEP                                                01410000
                 TIME(WS-TIMEOUT)                                       01420000
            END-EXEC                                                    01430000
            MOVE WS-DATEOUT TO SYSDO                                    01440000
            MOVE WS-TIMEOUT TO SYSTO                                    01450000
            .                                                           01460000
      *                                                                 01470000
       1010-ASK-TIME-DATE-EXIT.                                         01480000
            EXIT.                                                       01490000
      *                                                                 01500000
       1020-GET-APPL-LIST.                                              01510002
            INITIALIZE SDCA-SERVICE-COMMAREA                            01520002
            MOVE 'VBS.CI.APPLICAN.IN2' TO SD-SRV-NAME                   01530002
            MOVE CIC0009I-REC TO SD-SRV-INPUT-DATA                      01540002
            EXEC CICS                                                   01550002
                 LINK                                                   01560002
                 PROGRAM(WS-PGM-SRV-DRIVER)                             01570002
                 COMMAREA(WS-SRV-COMMAREA)                              01580002
                 RESP(WS-RESP-CODE)                                     01590002
            END-EXEC                                                    01600002
            EVALUATE WS-RESP-CODE                                       01610002
                WHEN DFHRESP(NORMAL)                                    01620002
      *              IF SD-RESP-CODE EQUAL ZEROS                        01630018
                        INITIALIZE CIC0009O-REC                         01640002
                        MOVE SD-SRV-OUTPUT-DATA TO CIC0009O-REC         01650002
      *              END-IF                                             01660018
            END-EVALUATE                                                01670002
            .                                                           01680002
      *                                                                 01690000
       1020-GET-APPL-LIST-EXIT.                                         01700002
            EXIT.                                                       01710000
      *                                                                 01720002
       1030-POPULATE-MAP.                                               01730002
            PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 14                01740002
              IF IX <= CIC0009O-COUNT                                   01750002
                 INITIALIZE CIC0012I-REC                                01760002
                 MOVE CIC0009O-APPL-ID(IX) TO CIC0012I-APPL-ID          01770002
                 PERFORM 1040-GET-CUSTAPPL                              01780002
                    THRU 1040-GET-CUSTAPPL-EXIT                         01790002
                 MOVE CIC0012O-ID             TO APPIDO(IX)             01800002
                 MOVE ATTR-PROT-SKIP-MDT      TO APPIDA(IX)             01801011
                 MOVE CIC0012O-NAME           TO NAMEO(IX)              01810002
                 MOVE CIC0012O-CUST-ID-NUMBER TO IDNUMO(IX)             01820002
                 STRING CIC0012O-LAST-DATE ' ' CIC0012O-LAST-TIME       01830002
                        DELIMITED BY SIZE   INTO LASTO(IX)              01840002
                 EVALUATE CIC0012O-STATUS                               01850002
                     WHEN 001                                           01860002
                          MOVE 'NEW'          TO STATO(IX)              01870002
                     WHEN 002                                           01880002
                          MOVE 'CHECKED'      TO STATO(IX)              01890002
                     WHEN 003                                           01900002
                          MOVE 'REVIEWED'     TO STATO(IX)              01910002
                     WHEN 004                                           01920002
                          MOVE 'INVESTIED'    TO STATO(IX)              01930002
                     WHEN 005                                           01940002
                          MOVE 'CREDITED'     TO STATO(IX)              01950002
                     WHEN 010                                           01960002
                          MOVE 'REVRETN'      TO STATO(IX)              01970002
                     WHEN 011                                           01980002
                          MOVE 'INVRETN'      TO STATO(IX)              01990002
                     WHEN 012                                           02000002
                          MOVE 'CHKFAIL'      TO STATO(IX)              02010002
                     WHEN 013                                           02020002
                          MOVE 'REVFAIL'      TO STATO(IX)              02030002
                     WHEN 014                                           02040002
                          MOVE 'INVFAIL'      TO STATO(IX)              02050002
                     WHEN 015                                           02060002
                          MOVE 'CREFAIL'      TO STATO(IX)              02070002
                     WHEN OTHER                                         02080002
                          MOVE 'UNKNOWN'      TO STATO(IX)              02090002
                 END-EVALUATE                                           02100002
              ELSE                                                      02110008
                 MOVE ATTR-PROT-DARK TO OPTA(IX)                        02120008
              END-IF                                                    02130002
            END-PERFORM                                                 02140002
            .                                                           02150002
      *                                                                 02160002
       1030-POPULATE-MAP-EXIT.                                          02170002
            EXIT.                                                       02180002
      *                                                                 02190000
       1040-GET-CUSTAPPL.                                               02200002
            INITIALIZE SDCA-SERVICE-COMMAREA                            02210002
            MOVE 'VBS.CI.CUSTAPPL.INQ' TO SD-SRV-NAME                   02220002
            MOVE CIC0012I-REC TO SD-SRV-INPUT-DATA                      02230002
            EXEC CICS                                                   02240002
                 LINK                                                   02250002
                 PROGRAM(WS-PGM-SRV-DRIVER)                             02260002
                 COMMAREA(WS-SRV-COMMAREA)                              02270002
                 RESP(WS-RESP-CODE)                                     02280002
            END-EXEC                                                    02290002
            EVALUATE WS-RESP-CODE                                       02300002
                WHEN DFHRESP(NORMAL)                                    02310002
                     IF SD-RESP-CODE EQUAL ZEROS                        02320002
                        INITIALIZE CIC0012O-REC                         02330002
                        MOVE SD-SRV-OUTPUT-DATA TO CIC0012O-REC         02340002
                     END-IF                                             02350002
            END-EVALUATE                                                02360002
            .                                                           02370002
       1040-GET-CUSTAPPL-EXIT.                                          02380002
            EXIT.                                                       02390002
      *                                                                 02400002
       2000-PRE-PROCESSING.                                             02410000
      *                                                                 02420000
       2000-PRE-PROCESSING-EXIT.                                        02430000
            EXIT.                                                       02440000
      *                                                                 02450000
       3000-MAIN-PROCESS.                                               02460000
            EVALUATE EIBAID                                             02470000
                WHEN DFHPF1                                             02480000
                     EXEC CICS                                          02482009
                          XCTL PROGRAM('CIOCCA00')                      02483021
                               RESP(WS-RESP-CODE)                       02485009
                     END-EXEC                                           02486009
                     IF WS-RESP-CODE NOT = DFHRESP(NORMAL)              02487009
                        MOVE 'PROGRAM CIOCCA00 IS NOT AVAILABLE'        02488021
                                TO MSGO                                 02489009
                        SET WS-MAP-DATAONLY TO TRUE                     02489109
                        PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT   02489209
                     END-IF                                             02489309
                WHEN DFHPF3                                             02610000
                     MOVE 'THANK YOU FOR USING THE SYSTEM' TO WS-MESSAGE02620000
                     EXEC CICS                                          02630000
                          SEND CONTROL                                  02640000
                          CURSOR                                        02650000
                          ERASE                                         02660000
                          FREEKB                                        02670000
                          ALARM                                         02680000
                     END-EXEC                                           02690000
                     EXEC CICS                                          02700000
                          SEND FROM(WS-MESSAGE)                         02710000
                     END-EXEC                                           02720000
                     PERFORM 5010-RETURN THRU 5010-RETURN-EXIT          02730000
                WHEN DFHCLEAR                                           02740000
                     EXEC CICS                                          02750000
                           SEND CONTROL                                 02760000
                           CURSOR                                       02770000
                           ERASE                                        02780000
                           FREEKB                                       02790000
                           ALARM                                        02800000
                     END-EXEC                                           02810000
                     PERFORM 5010-RETURN THRU 5010-RETURN-EXIT          02820000
                WHEN DFHPF7                                             02830013
                     INITIALIZE CIC0009I-REC                            02840013
                     MOVE LK-OPTION TO CIC0009I-OPTION                  02850013
                     MOVE LK-APPL-MIN-ID TO CIC0009I-APPL-ID            02860013
                     MOVE 'F7' TO CIC0009I-DIRECTION                    02870013
      *                                                                 02870113
                     PERFORM 1020-GET-APPL-LIST                         02870413
                        THRU 1020-GET-APPL-LIST-EXIT                    02870513
      *                                                                 02870616
                     IF CIC0009O-COUNT = ZEROS                          02870719
                        MOVE 'IT IS ALREADY THE TOP!' TO MSGO           02870817
                        SET WS-MAP-DATAONLY TO TRUE                     02870918
                     ELSE                                               02871016
                        MOVE LOW-VALUES TO CICA03O                      02871116
                        PERFORM 1030-POPULATE-MAP                       02871216
                           THRU 1030-POPULATE-MAP-EXIT                  02871316
                        SET WS-MAP-ERASE TO TRUE                        02871418
                     END-IF                                             02871516
      *                                                                 02871613
                     PERFORM 3030-SEND-MAP                              02871813
                        THRU 3030-SEND-MAP-EXIT                         02871913
                WHEN DFHPF8                                             02872013
                     INITIALIZE CIC0009I-REC                            02872113
                     MOVE LK-OPTION TO CIC0009I-OPTION                  02872213
                     MOVE LK-APPL-MAX-ID TO CIC0009I-APPL-ID            02872313
                     MOVE 'F8' TO CIC0009I-DIRECTION                    02872413
      *                                                                 02872513
                     PERFORM 1020-GET-APPL-LIST                         02872713
                        THRU 1020-GET-APPL-LIST-EXIT                    02872813
      *                                                                 02872916
                     IF CIC0009O-COUNT = ZEROS                          02873018
                        MOVE 'IT IS ALREADY THE BOTTOM!' TO MSGO        02873117
                        SET WS-MAP-DATAONLY TO TRUE                     02873218
                     ELSE                                               02873316
                        MOVE LOW-VALUES TO CICA03O                      02873416
                        PERFORM 1030-POPULATE-MAP                       02873516
                           THRU 1030-POPULATE-MAP-EXIT                  02873616
                        SET WS-MAP-ERASE TO TRUE                        02873718
                     END-IF                                             02873816
      *                                                                 02874013
                     PERFORM 3030-SEND-MAP                              02874213
                        THRU 3030-SEND-MAP-EXIT                         02875013
                WHEN DFHPF9                                             02881713
                     INITIALIZE CIC0009I-REC                            02882221
                     MOVE LK-OPTION TO CIC0009I-OPTION                  02882321
                     SUBTRACT 1 FROM LK-APPL-MIN-ID                     02882421
                              GIVING CIC0009I-APPL-ID                   02882521
                     MOVE 'F8' TO CIC0009I-DIRECTION                    02882621
      *                                                                 02882721
                     PERFORM 1020-GET-APPL-LIST                         02882821
                        THRU 1020-GET-APPL-LIST-EXIT                    02882921
      *                                                                 02883021
                     IF CIC0009O-COUNT = 0                              02883121
                        MOVE 'NO RECORDS LISTED!' TO MSGO               02883221
                     END-IF                                             02883321
                     MOVE LOW-VALUES TO CICA03O                         02883421
      *                                                                 02883521
                     PERFORM 1030-POPULATE-MAP                          02883621
                        THRU 1030-POPULATE-MAP-EXIT                     02883721
      *                                                                 02883821
                     SET WS-MAP-ERASE TO TRUE                           02883921
                     PERFORM 3030-SEND-MAP                              02884021
                        THRU 3030-SEND-MAP-EXIT                         02884121
                 WHEN DFHENTER                                          02885013
                      IF WS-RESP-CODE NOT EQUAL DFHRESP(NORMAL)         02890000
                         MOVE 'INVALID REQUEST, CHECK YOUR INPUT.'      02900000
                              TO MSGO                                   02910000
                         SET WS-MAP-DATAONLY TO TRUE                    02920000
                         PERFORM 3030-SEND-MAP                          02930000
                            THRU 3030-SEND-MAP-EXIT                     02940000
                      ELSE                                              02950000
                         PERFORM 3010-CHECK-INPUT                       02960000
                            THRU 3010-CHECK-INPUT-EXIT                  02970000
                         PERFORM 3020-XCTL                              02980000
                            THRU 3020-XCTL-EXIT                         02990000
                      END-IF                                            03000000
                 WHEN OTHER                                             03010000
                      MOVE 'INVALID KEY PRESSED!' TO MSGO               03020000
                      SET WS-MAP-DATAONLY TO TRUE                       03030000
                      PERFORM 3030-SEND-MAP                             03040000
                         THRU 3030-SEND-MAP-EXIT                        03050000
            END-EVALUATE                                                03060000
            .                                                           03070000
       3000-MAIN-PROCESS-EXIT.                                          03080000
            EXIT.                                                       03090000
      *                                                                 03100000
       3010-CHECK-INPUT.                                                03110000
            IF COMMUL NOT = 0                                           03120000
               INITIALIZE CIMENU-REC                                    03130000
               MOVE COMMUI TO CIMENU-TRANSID                            03140000
               EXEC CICS READ                                           03150000
                    FILE('CIMENU')                                      03160000
                    INTO(CIMENU-REC)                                    03170000
                    RIDFLD(CIMENU-TRANSID)                              03180000
                    RESP(WS-RESP-CODE)                                  03190000
               END-EXEC                                                 03200000
               EVALUATE WS-RESP-CODE                                    03210000
                   WHEN DFHRESP(NORMAL)                                 03220000
                        EXEC CICS                                       03230000
                             XCTL PROGRAM(CIMENU-PGM)                   03240000
                             COMMAREA(CIMENU-TRANSID)                   03250028
                             RESP(WS-RESP-CODE)                         03260000
                        END-EXEC                                        03270000
                        IF WS-RESP-CODE NOT = DFHRESP(NORMAL)           03280000
                        STRING 'PROGRAM ' DELIMITED BY SIZE             03290000
                               CIMENU-PGM DELIMITED BY SPACE            03300000
                               ' IS NOT AVAILABLE' DELIMITED BY SIZE    03310000
                               INTO MSGO                                03320000
                           SET WS-MAP-DATAONLY TO TRUE                  03330000
                           PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT03340000
                        END-IF                                          03350000
                   WHEN DFHRESP(NOTFND)                                 03360000
                        MOVE 'INVALID TRANSATION ID!' TO MSGO           03370000
                        SET WS-MAP-DATAONLY TO TRUE                     03380000
                        PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT   03390000
                   WHEN OTHER                                           03400000
                        MOVE 'CIMENU FILE ERROR!' TO MSGO               03410000
                        SET WS-MAP-DATAONLY TO TRUE                     03420000
                        PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT   03430000
               END-EVALUATE                                             03440000
            END-IF                                                      03450000
            .                                                           03460000
      *                                                                 03470000
       3010-CHECK-INPUT-EXIT.                                           03480000
            EXIT.                                                       03490000
      *                                                                 03500000
       3020-XCTL.                                                       03510000
            PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 14                03511012
               IF OPTI(IX) = 'S'                                        03512012
                  INITIALIZE WS-COMMAREA-CA07                           03513012
                  MOVE 'Y' TO WS-FIRST-SEND OF WS-COMMAREA-CA07         03514012
                  MOVE LK-OPTION TO WS-OPTION OF WS-COMMAREA-CA07       03515012
                  MOVE APPIDI(IX) TO WS-APPL-ID OF WS-COMMAREA-CA07     03516012
                  EXEC CICS                                             03517012
                       XCTL PROGRAM('CIOCCA07')                         03518012
                            COMMAREA(WS-COMMAREA-CA07)                  03519012
                            RESP(WS-RESP-CODE)                          03519112
                  END-EXEC                                              03519212
                  IF WS-RESP-CODE NOT = DFHRESP(NORMAL)                 03519312
                     MOVE 'PROGRAM CIOCCA07 IS NOT AVAILABLE'           03519412
                             TO MSGO                                    03519512
                     SET WS-MAP-DATAONLY TO TRUE                        03519612
                     PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT      03519712
                  END-IF                                                03519812
               END-IF                                                   03519912
            END-PERFORM                                                 03520012
            MOVE 'INVALID SELECTION!' TO MSGO                           03520112
            SET WS-MAP-DATAONLY TO TRUE                                 03520212
            PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT               03520312
            .                                                           03520412
      *                                                                 03521000
       3020-XCTL-EXIT.                                                  03530000
            EXIT.                                                       03540000
      *                                                                 03550000
       3030-SEND-MAP.                                                   03560000
            EVALUATE LK-OPTION                                          03570002
                WHEN 001                                                03580002
                     MOVE 'INTEGRITY CHECKING LIST' TO TITLEO           03590002
                WHEN 002                                                03600002
                     MOVE 'APPLICATION REVIEW LIST' TO TITLEO           03610002
                WHEN 003                                                03620002
                     MOVE 'CREDIT INVESTIGATE LIST' TO TITLEO           03630002
                WHEN 004                                                03640002
                     MOVE 'MANUAL  CREDITING  LIST' TO TITLEO           03650002
                WHEN OTHER                                              03660002
                     MOVE 'INVALID   TITLE   SHOWN' TO TITLEO           03670002
            END-EVALUATE                                                03680002
            PERFORM 1010-ASK-TIME-DATE                                  03690000
               THRU 1010-ASK-TIME-DATE-EXIT                             03700000
            EVALUATE TRUE                                               03710000
                WHEN WS-MAP-ERASE                                       03720000
                     EXEC CICS SEND                                     03730000
                          MAP('CICA03')                                 03740002
                          MAPSET('CICA03')                              03750002
                          FROM(CICA03O)                                 03760002
                          ERASE                                         03770000
                     END-EXEC                                           03780000
                WHEN WS-MAP-DATAONLY                                    03790000
                     EXEC CICS SEND                                     03800000
                          MAP('CICA03')                                 03810002
                          MAPSET('CICA03')                              03820002
                          FROM(CICA03O)                                 03830002
                          DATAONLY                                      03840000
                     END-EXEC                                           03850000
            END-EVALUATE                                                03860000
            PERFORM 5020-RETURN-TRANS THRU 5020-RETURN-TRANS-EXIT       03870000
            .                                                           03880000
      *                                                                 03890000
       3030-SEND-MAP-EXIT.                                              03900000
            EXIT.                                                       03910000
      *                                                                 03920000
       4000-POST-PROCESSING.                                            03930000
      *                                                                 03940000
       4000-POST-PROCESSING-EXIT.                                       03950000
            EXIT.                                                       03960000
      *                                                                 03970000
       5000-CLEAN-UP.                                                   03980000
            PERFORM 5010-RETURN                                         03990000
               THRU 5010-RETURN-EXIT                                    04000000
            .                                                           04010000
      *                                                                 04020000
       5000-CLEAN-UP-EXIT.                                              04030000
            EXIT.                                                       04040000
      *                                                                 04050000
       5010-RETURN.                                                     04060000
            EXEC CICS RETURN END-EXEC                                   04070000
            .                                                           04080000
       5010-RETURN-EXIT.                                                04090000
            EXIT.                                                       04100000
      *                                                                 04110000
       5020-RETURN-TRANS.                                               04120000
            INITIALIZE WS-COMMAREA-SELF                                 04130002
            MOVE 'N' TO WS-FIRST-SEND OF WS-COMMAREA-SELF               04140006
            MOVE LK-OPTION TO WS-OPTION OF WS-COMMAREA-SELF             04150008
            IF CIC0009O-COUNT NOT = ZEROS                               04162008
               MOVE CIC0009O-APPL-ID(1) TO WS-APPL-MIN-ID               04162108
               MOVE CIC0009O-APPL-ID(CIC0009O-COUNT) TO WS-APPL-MAX-ID  04162208
            ELSE                                                        04162320
               MOVE LK-APPL-MIN-ID TO WS-APPL-MIN-ID                    04162420
               MOVE LK-APPL-MAX-ID TO WS-APPL-MAX-ID                    04162520
            END-IF                                                      04163008
            EXEC CICS RETURN TRANSID('CIA3')                            04170002
                      COMMAREA(WS-COMMAREA-SELF)                        04180003
            END-EXEC                                                    04190000
            .                                                           04200000
       5020-RETURN-TRANS-EXIT.                                          04210000
            EXIT.                                                       04220000
