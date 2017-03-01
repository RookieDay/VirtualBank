       IDENTIFICATION DIVISION.                                         00010000
       PROGRAM-ID. CIOCCS02.                                            00020001
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
       77 WS-BEGIN              PIC X(17) VALUE 'CIOCCS02 WS BEGIN'.    00190001
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
       01 IX                    PIC 9(3).                               00301005
      *                                                                 00310000
      *SCREEN HANDLER                                                   00320000
       COPY SD11WS.                                                     00330000
      * SYMBOLIC MAP                                                    00340000
       COPY CICS02.                                                     00350001
      *MAP CONTROL                                                      00360000
       COPY DFHBMSCA.                                                   00370000
      *CICS FUNCTION KEYS                                               00380000
       COPY DFHAID.                                                     00390000
      *CIMENU                                                           00400000
       COPY CIMENU.                                                     00410000
      *                                                                 00411000
       COPY CIC0012I.                                                   00412003
       COPY CIC0012O.                                                   00413003
      *                                                                 00414000
       COPY CIC0008I.                                                   00415001
       COPY CIC0008O.                                                   00416001
      *                                                                 00420000
       01 WS-SRV-COMMAREA.                                              00430000
      *SERVICE REQUEST/RESPONSE COMMAREA                                00440000
       COPY SD01WS.                                                     00450000
      *                                                                 00451005
       01 WS-COMMAREA-CIB3.                                             00452009
          05 WS-FIRST-SEND      PIC X(1).                               00453005
          05 WS-APPL-ID         PIC 9(13).                              00455005
      *                                                                 00456005
       01 WS-COMMAREA-SELF.                                             00457011
          05 WS-FIRST-SEND      PIC X(1).                               00457118
          05 WS-ID-NUMBER       PIC 9(18).                              00457218
          05 WS-ID-TYPE         PIC 9(3).                               00457318
      *                                                                 00460000
       01 WS-COMMAREA.                                                  00470000
          05 WS-FIRST-SEND      PIC X(1).                               00480000
          05 WS-ID-NUMBER       PIC 9(18).                              00490000
          05 WS-ID-TYPE         PIC 9(3).                               00500000
      *                                                                 00510018
       01 WS-COMMAREA-CS01      PIC X(4).                               00520122
       77 WS-END                PIC X(17) VALUE 'CIOCCS02 WS END'.      00521001
      *                                                                 00530000
       LINKAGE SECTION.                                                 00540000
       01 DFHCOMMAREA.                                                  00550000
          05 LK-FIRST-SEND      PIC X(1).                               00560000
          05 LK-ID-NUMBER       PIC 9(18).                              00572007
          05 LK-ID-TYPE         PIC 9(3).                               00573013
      *                                                                 00580000
       PROCEDURE DIVISION.                                              00590000
       0000-MAINLINE.                                                   00600000
      *                                                                 00610000
            PERFORM 1000-INIT                                           00620000
               THRU 1000-INIT-EXIT                                      00630000
      *                                                                 00640000
            PERFORM 2000-PRE-PROCESSING                                 00650000
               THRU 2000-PRE-PROCESSING-EXIT                            00660000
      *                                                                 00670000
            PERFORM 3000-MAIN-PROCESS                                   00680000
               THRU 3000-MAIN-PROCESS-EXIT                              00690000
      *                                                                 00700000
            PERFORM 4000-POST-PROCESSING                                00710000
               THRU 4000-POST-PROCESSING-EXIT                           00720000
      *                                                                 00730000
            PERFORM 5000-CLEAN-UP                                       00740000
               THRU 5000-CLEAN-UP-EXIT                                  00750000
            .                                                           00760000
      *                                                                 00770000
       0000-EXIT.                                                       00780000
            EXIT.                                                       00790000
      *                                                                 00800000
       1000-INIT.                                                       00810000
            IF LK-FIRST-SEND = 'Y'                                      00820000
      *                                                                 00820109
               PERFORM 1020-GET-EXISTING-APPL                           00821003
                  THRU 1020-GET-EXISTING-APPL-EXIT                      00822003
      *                                                                 00822118
               MOVE LOW-VALUES TO CICS02O                               00822217
               IF CIC0008O-COUNT = 0                                    00823010
                  MOVE 'NO RECORDS LISTED!' TO MSGO                     00824010
               END-IF                                                   00825018
      *                                                                 00825118
               PERFORM 1030-APPLICAN-CUST-SHOW                          00826018
                  THRU 1030-APPLICAN-CUST-SHOW-EXIT                     00827018
      *                                                                 00828018
               SET WS-MAP-ERASE TO TRUE                                 00840000
               PERFORM 3030-SEND-MAP                                    00850000
                  THRU 3030-SEND-MAP-EXIT                               00860000
      * NOT FIRST SHOW                                                  00870000
            ELSE                                                        00880000
               MOVE LOW-VALUES TO CICS02I                               00890001
               EXEC CICS RECEIVE MAP('CICS02')                          00900001
                                 MAPSET('CICS02')                       00910001
                                 INTO(CICS02I)                          00920001
                                 RESP(WS-RESP-CODE)                     00930000
               END-EXEC                                                 00940000
            END-IF                                                      00950000
            .                                                           00960000
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
      *                                                                 01231000
       1020-GET-EXISTING-APPL.                                          01232003
            INITIALIZE SDCA-SERVICE-COMMAREA                            01232100
            MOVE 'VBS.CI.APPLICAN.IN1' TO SD-SRV-NAME                   01232202
            INITIALIZE CIC0008I-REC                                     01232309
            MOVE LK-ID-NUMBER TO CIC0008I-CUST-ID                       01232409
            MOVE CIC0008I-REC TO SD-SRV-INPUT-DATA                      01232517
            EXEC CICS                                                   01232707
                 LINK                                                   01232807
                 PROGRAM(WS-PGM-SRV-DRIVER)                             01232907
                 COMMAREA(WS-SRV-COMMAREA)                              01233007
                 RESP(WS-RESP-CODE)                                     01233107
            END-EXEC                                                    01233207
            EVALUATE WS-RESP-CODE                                       01233307
                WHEN DFHRESP(NORMAL)                                    01233407
      *              IF SD-RESP-CODE EQUAL ZEROS                        01233518
                        INITIALIZE CIC0008O-REC                         01233618
                        MOVE SD-SRV-OUTPUT-DATA TO CIC0008O-REC         01233718
      *              END-IF                                             01234018
            END-EVALUATE                                                01234300
            .                                                           01234700
      *                                                                 01234800
       1020-GET-EXISTING-APPL-EXIT.                                     01234903
            EXIT.                                                       01235000
      *                                                                 01240000
       1030-APPLICAN-CUST-SHOW.                                         01240103
            PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 14                01247005
              IF IX <= CIC0008O-COUNT                                   01247105
                 INITIALIZE CIC0012I-REC                                01247217
                 MOVE CIC0008O-APPL-ID(IX)    TO CIC0012I-APPL-ID       01247320
                 PERFORM 1040-GET-CUSTAPPL                              01247417
                      THRU 1040-GET-CUSTAPPL-EXIT                       01247520
                 MOVE CIC0012O-ID             TO APPIDO(IX)             01247617
                 MOVE ATTR-PROT-SKIP-MDT      TO APPIDA(IX)             01247717
                 MOVE CIC0012O-NAME           TO NAMEO(IX)              01247817
                 STRING CIC0012O-IN-DATE ' ' CIC0012O-IN-TIME           01248018
                        DELIMITED BY SIZE     INTO APPLO(IX)            01248120
                 STRING CIC0012O-LAST-DATE ' ' CIC0012O-LAST-TIME       01248218
                        DELIMITED BY SIZE     INTO LASTO(IX)            01248320
                 EVALUATE CIC0012O-STATUS                               01248418
                     WHEN 001                                           01248518
                          MOVE 'NEW'          TO STATO(IX)              01248618
                     WHEN 002                                           01248718
                          MOVE 'CHECKED'      TO STATO(IX)              01248818
                     WHEN 003                                           01248918
                          MOVE 'REVIEWED'     TO STATO(IX)              01249018
                     WHEN 004                                           01249118
                          MOVE 'INVESTIED'    TO STATO(IX)              01249218
                     WHEN 005                                           01249318
                          MOVE 'CREDITED'     TO STATO(IX)              01249418
                     WHEN 010                                           01249518
                          MOVE 'REVRETN'      TO STATO(IX)              01249618
                     WHEN 011                                           01249718
                          MOVE 'INVRETN'      TO STATO(IX)              01249818
                     WHEN 012                                           01249918
                          MOVE 'CHKFAIL'      TO STATO(IX)              01250018
                     WHEN 013                                           01250118
                          MOVE 'REVFAIL'      TO STATO(IX)              01250218
                     WHEN 014                                           01250318
                          MOVE 'INVFAIL'      TO STATO(IX)              01250418
                     WHEN 015                                           01250518
                          MOVE 'CREFAIL'      TO STATO(IX)              01250618
                     WHEN OTHER                                         01250718
                          MOVE 'UNKNOWN'      TO STATO(IX)              01250818
                 END-EVALUATE                                           01250918
              ELSE                                                      01251005
                 MOVE ATTR-PROT-DARK TO OPTA(IX)                        01251105
              END-IF                                                    01251205
            END-PERFORM                                                 01251303
            .                                                           01251403
      *                                                                 01251500
       1030-APPLICAN-CUST-SHOW-EXIT.                                    01251603
            EXIT.                                                       01251700
      *                                                                 01251817
       1040-GET-CUSTAPPL.                                               01251917
            INITIALIZE SDCA-SERVICE-COMMAREA                            01252017
            MOVE 'VBS.CI.CUSTAPPL.INQ' TO SD-SRV-NAME                   01252117
            MOVE CIC0012I-REC TO SD-SRV-INPUT-DATA                      01252217
            EXEC CICS                                                   01252317
                 LINK                                                   01252417
                 PROGRAM(WS-PGM-SRV-DRIVER)                             01252517
                 COMMAREA(WS-SRV-COMMAREA)                              01252617
                 RESP(WS-RESP-CODE)                                     01252717
            END-EXEC                                                    01252817
            EVALUATE WS-RESP-CODE                                       01252917
                WHEN DFHRESP(NORMAL)                                    01253017
                     IF SD-RESP-CODE EQUAL ZEROS                        01253117
                        INITIALIZE CIC0012O-REC                         01253217
                        MOVE SD-SRV-OUTPUT-DATA TO CIC0012O-REC         01253317
                     END-IF                                             01253417
            END-EVALUATE                                                01253517
            .                                                           01253617
       1040-GET-CUSTAPPL-EXIT.                                          01253717
            EXIT.                                                       01253817
      *                                                                 01254017
      *                                                                 01256000
       2000-PRE-PROCESSING.                                             01257007
                                                                        01260007
       2000-PRE-PROCESSING-EXIT.                                        01270007
            EXIT.                                                       01280007
      *                                                                 01290000
       3000-MAIN-PROCESS.                                               01300000
            EVALUATE EIBAID                                             01310000
                WHEN DFHPF1                                             01311000
                     MOVE 'Y000' TO WS-COMMAREA-CS01                    01311122
                     EXEC CICS                                          01312000
                          XCTL PROGRAM('CIOCCS01')                      01313005
                               COMMAREA(WS-COMMAREA-CS01)               01313122
                               RESP(WS-RESP-CODE)                       01314000
                     END-EXEC                                           01315000
                     IF WS-RESP-CODE NOT = DFHRESP(NORMAL)              01316000
                        MOVE 'PROGRAM CIOCCS01 IS NOT AVAILABLE'        01317005
                                TO MSGO                                 01318000
                        SET WS-MAP-DATAONLY TO TRUE                     01319000
                        PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT   01319100
                     END-IF                                             01319200
                WHEN DFHPF3                                             01320000
                     MOVE 'THANK YOU FOR USING THE SYSTEM' TO WS-MESSAGE01330000
                     EXEC CICS                                          01340000
                          SEND CONTROL                                  01350000
                          CURSOR                                        01360000
                          ERASE                                         01370000
                          FREEKB                                        01380000
                          ALARM                                         01390000
                     END-EXEC                                           01400000
                     EXEC CICS                                          01410000
                          SEND FROM(WS-MESSAGE)                         01420000
                     END-EXEC                                           01430000
                     PERFORM 5010-RETURN THRU 5010-RETURN-EXIT          01440000
                WHEN DFHCLEAR                                           01450000
                     EXEC CICS                                          01460000
                           SEND CONTROL                                 01470000
                           CURSOR                                       01480000
                           ERASE                                        01490000
                           FREEKB                                       01500000
                           ALARM                                        01510000
                     END-EXEC                                           01520000
                     PERFORM 5010-RETURN THRU 5010-RETURN-EXIT          01530000
                WHEN DFHPF9                                             01540000
                     PERFORM 1020-GET-EXISTING-APPL                     01541020
                        THRU 1020-GET-EXISTING-APPL-EXIT                01542020
      *                                                                 01543020
      *              MOVE LOW-VALUES TO CICS02O                         01544020
                     IF CIC0008O-COUNT = ZEROS                          01545020
                        MOVE 'NO RECORDS LISTED!' TO MSGO               01546020
                     ELSE                                               01547020
                     MOVE LOW-VALUES TO CICS02O                         01548220
                        PERFORM 1030-APPLICAN-CUST-SHOW                 01549020
                           THRU 1030-APPLICAN-CUST-SHOW-EXIT            01549120
                     END-IF                                             01549220
      *                                                                 01549320
                     SET WS-MAP-ERASE TO TRUE                           01549420
                     PERFORM 3030-SEND-MAP                              01570000
                        THRU 3030-SEND-MAP-EXIT                         01580000
                WHEN DFHENTER                                           01590000
                     IF WS-RESP-CODE NOT EQUAL DFHRESP(NORMAL)          01600000
                        MOVE 'INVALID REQUEST, CHECK YOUR INPUT.'       01610000
                             TO MSGO                                    01620000
                        SET WS-MAP-DATAONLY TO TRUE                     01630000
                        PERFORM 3030-SEND-MAP                           01640000
                           THRU 3030-SEND-MAP-EXIT                      01650000
                     ELSE                                               01660000
                        PERFORM 3010-CHECK-INPUT                        01670000
                           THRU 3010-CHECK-INPUT-EXIT                   01680000
                        PERFORM 3020-XCTL                               01690005
                           THRU 3020-XCTL-EXIT                          01700005
                     END-IF                                             01710000
                WHEN OTHER                                              01720000
                     MOVE 'INVALID KEY PRESSED!' TO MSGO                01730000
                     SET WS-MAP-DATAONLY TO TRUE                        01740000
                     PERFORM 3030-SEND-MAP                              01750000
                        THRU 3030-SEND-MAP-EXIT                         01760000
            END-EVALUATE                                                01770000
            .                                                           01780000
       3000-MAIN-PROCESS-EXIT.                                          01790000
            EXIT.                                                       01800000
      *                                                                 01810000
       3010-CHECK-INPUT.                                                01820000
            IF COMMUL NOT = 0                                           01830000
               INITIALIZE CIMENU-REC                                    01840000
               MOVE COMMUI TO CIMENU-TRANSID                            01850000
               EXEC CICS READ                                           01860000
                    FILE('CIMENU')                                      01870000
                    INTO(CIMENU-REC)                                    01880000
                    RIDFLD(CIMENU-TRANSID)                              01890000
                    RESP(WS-RESP-CODE)                                  01900000
               END-EXEC                                                 01910000
               EVALUATE WS-RESP-CODE                                    01911000
                   WHEN DFHRESP(NORMAL)                                 01912000
                        EXEC CICS                                       01913000
                             XCTL PROGRAM(CIMENU-PGM)                   01914000
                             COMMAREA(CIMENU-TRANSID)                   01915021
                             RESP(WS-RESP-CODE)                         01916000
                        END-EXEC                                        01917000
                        IF WS-RESP-CODE NOT = DFHRESP(NORMAL)           01918000
                        STRING 'PROGRAM ' DELIMITED BY SIZE             01919000
                               CIMENU-PGM DELIMITED BY SPACE            01919100
                               ' IS NOT AVAILABLE' DELIMITED BY SIZE    01919200
                               INTO MSGO                                01919300
                           SET WS-MAP-DATAONLY TO TRUE                  01919400
                           PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT01919500
                        END-IF                                          01919600
                   WHEN DFHRESP(NOTFND)                                 01919700
                        MOVE 'INVALID TRANSATION ID!' TO MSGO           01919800
                        SET WS-MAP-DATAONLY TO TRUE                     01919900
                        PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT   01920000
                   WHEN OTHER                                           01920100
                        MOVE 'CIMENU FILE ERROR!' TO MSGO               01920200
                        SET WS-MAP-DATAONLY TO TRUE                     01920300
                        PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT   01920400
               END-EVALUATE                                             01920500
            END-IF                                                      01920600
            .                                                           01927000
      *                                                                 01930000
       3010-CHECK-INPUT-EXIT.                                           01940000
            EXIT.                                                       01950005
      *                                                                 01960000
       3020-XCTL.                                                       01970005
            PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 14                01980005
               IF OPTI(IX) = 'S'                                        01990005
                  INITIALIZE WS-COMMAREA-CIB3                           02000009
                  MOVE 'Y'        TO WS-FIRST-SEND OF WS-COMMAREA-CIB3  02010018
                  MOVE APPIDI(IX) TO WS-APPL-ID    OF WS-COMMAREA-CIB3  02030018
                  EXEC CICS                                             02040005
                       XCTL PROGRAM('CIOCCS03')                         02050005
                            COMMAREA(WS-COMMAREA-CIB3)                  02060009
                            RESP(WS-RESP-CODE)                          02070005
                  END-EXEC                                              02080005
                  IF WS-RESP-CODE NOT = DFHRESP(NORMAL)                 02090005
                     MOVE 'PROGRAM CIOCCS03 IS NOT AVAILABLE'           02100011
                             TO MSGO                                    02110005
                     SET WS-MAP-DATAONLY TO TRUE                        02120005
                     PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT      02130005
                  END-IF                                                02140005
               END-IF                                                   02150005
            END-PERFORM                                                 02160005
            MOVE 'INVALID SELECTION!' TO MSGO                           02170005
            SET WS-MAP-DATAONLY TO TRUE                                 02180005
            PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT               02190005
            .                                                           02200005
      *                                                                 02430000
       3020-XCTL-EXIT.                                                  02440005
            EXIT.                                                       02450000
      *                                                                 02460000
       3030-SEND-MAP.                                                   02470000
            MOVE 'PROCESS INQUIRE LIST' TO TITLEO                       02473009
            PERFORM 1010-ASK-TIME-DATE                                  02480000
               THRU 1010-ASK-TIME-DATE-EXIT                             02490000
            EVALUATE TRUE                                               02500000
                WHEN WS-MAP-ERASE                                       02510000
                     EXEC CICS SEND                                     02520000
                          MAP('CICS02')                                 02530001
                          MAPSET('CICS02')                              02540001
                          FROM(CICS02O)                                 02550001
                          ERASE                                         02560000
                     END-EXEC                                           02570000
                WHEN WS-MAP-DATAONLY                                    02580000
                     EXEC CICS SEND                                     02590000
                          MAP('CICS02')                                 02600001
                          MAPSET('CICS02')                              02610001
                          FROM(CICS02O)                                 02620001
                          DATAONLY                                      02630000
                     END-EXEC                                           02640000
            END-EVALUATE                                                02650000
            PERFORM 5020-RETURN-TRANS THRU 5020-RETURN-TRANS-EXIT       02670000
            .                                                           02680000
      *                                                                 02690000
       3030-SEND-MAP-EXIT.                                              02700000
            EXIT.                                                       02710000
      *                                                                 02710100
      *                                                                 02721600
       3040-POPULATE-CICUS-EXIT.                                        02721700
            EXIT.                                                       02721800
      *                                                                 02761000
       4000-POST-PROCESSING.                                            02762007
                                                                        02763007
       4000-POST-PROCESSING-EXIT.                                       02764007
            EXIT.                                                       02765007
      *                                                                 02770000
       5000-CLEAN-UP.                                                   02780000
            PERFORM 5010-RETURN                                         02790000
               THRU 5010-RETURN-EXIT                                    02800000
            .                                                           02810000
      *                                                                 02820000
       5000-CLEAN-UP-EXIT.                                              02830000
            EXIT.                                                       02840000
      *                                                                 02850000
       5010-RETURN.                                                     02860000
            EXEC CICS RETURN END-EXEC                                   02870000
            .                                                           02880000
       5010-RETURN-EXIT.                                                02890000
            EXIT.                                                       02900000
      *                                                                 02910000
       5020-RETURN-TRANS.                                               02920000
            INITIALIZE WS-COMMAREA-SELF                                 02930018
            MOVE 'N'          TO WS-FIRST-SEND OF WS-COMMAREA-SELF      02940018
            MOVE LK-ID-NUMBER TO WS-ID-NUMBER  OF WS-COMMAREA-SELF      02961018
            MOVE LK-ID-TYPE   TO WS-ID-TYPE    OF WS-COMMAREA-SELF      02962019
            EXEC CICS RETURN TRANSID('CIB2')                            02967005
                      COMMAREA(WS-COMMAREA-SELF)                        02968005
            END-EXEC                                                    02969005
            .                                                           02969105
       5020-RETURN-TRANS-EXIT.                                          02970000
            EXIT.                                                       02980000
