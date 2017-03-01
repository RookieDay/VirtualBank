       IDENTIFICATION DIVISION.                                         00010001
       PROGRAM-ID. CIOCCA06.                                            00020048
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
       77 WS-BEGIN              PIC X(17) VALUE 'CIOCCA06 WS BEGIN'.    00190048
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
       COPY CICA06.                                                     00350048
      *MAP CONTROL                                                      00360001
       COPY DFHBMSCA.                                                   00370001
      *CICS FUNCTION KEYS                                               00380001
       COPY DFHAID.                                                     00390001
      *CIMENU                                                           00400001
       COPY CIMENU.                                                     00410001
      *                                                                 00411032
       COPY CIC0006I.                                                   00412049
       COPY CIC0006O.                                                   00413049
      *                                                                 00413150
       COPY CIC0007I.                                                   00413250
       COPY CIC0007O.                                                   00413350
      *                                                                 00414039
       01 WS-SRV-COMMAREA.                                              00430001
      *SERVICE REQUEST/RESPONSE COMMAREA                                00440001
       COPY SD01WS.                                                     00450001
      *                                                                 00460001
       01 WS-COMMAREA.                                                  00470004
          05 WS-FIRST-SEND      PIC X(1).                               00480004
          05 WS-OPTION          PIC 9(3).                               00490049
          05 WS-APPL-ID         PIC 9(13).                              00500049
       77 WS-END                PIC X(17) VALUE 'CIOCCA06 WS END'.      00521049
      *                                                                 00530001
       LINKAGE SECTION.                                                 00540001
       01 DFHCOMMAREA.                                                  00550001
          05 LK-FIRST-SEND      PIC X(1).                               00560001
          05 LK-OPTION          PIC 9(3).                               00570055
          05 LK-APPL-ID         PIC 9(13).                              00571049
      *                                                                 00580001
       PROCEDURE DIVISION.                                              00590001
       0000-MAINLINE.                                                   00600001
      *                                                                 00610001
            PERFORM 1000-INIT                                           00620001
               THRU 1000-INIT-EXIT                                      00630001
      *                                                                 00640001
            PERFORM 2000-PRE-PROCESSING                                 00650001
               THRU 2000-PRE-PROCESSING-EXIT                            00660001
      *                                                                 00670001
            PERFORM 3000-MAIN-PROCESS                                   00680001
               THRU 3000-MAIN-PROCESS-EXIT                              00690001
      *                                                                 00700001
            PERFORM 4000-POST-PROCESSING                                00710001
               THRU 4000-POST-PROCESSING-EXIT                           00720001
      *                                                                 00730001
            PERFORM 5000-CLEAN-UP                                       00740001
               THRU 5000-CLEAN-UP-EXIT                                  00750001
            .                                                           00760001
      *                                                                 00770001
       0000-EXIT.                                                       00780001
            EXIT.                                                       00790001
      *                                                                 00800001
       1000-INIT.                                                       00810001
            IF LK-FIRST-SEND = 'Y'                                      00820001
               MOVE LOW-VALUES TO CICA06O                               00820152
               SET WS-MAP-ERASE TO TRUE                                 00840001
               PERFORM 3030-SEND-MAP                                    00850001
                  THRU 3030-SEND-MAP-EXIT                               00860001
      * NOT FIRST SHOW                                                  00870001
            ELSE                                                        00880001
               MOVE LOW-VALUES TO CICA06I                               00890048
               EXEC CICS RECEIVE MAP('CICA06')                          00900048
                                 MAPSET('CICA06')                       00910048
                                 INTO(CICA06I)                          00920049
                                 RESP(WS-RESP-CODE)                     00930039
               END-EXEC                                                 00940039
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
      *                                                                 01231023
       2000-PRE-PROCESSING.                                             01250001
      *                                                                 01260001
       2000-PRE-PROCESSING-EXIT.                                        01270001
            EXIT.                                                       01280001
      *                                                                 01290001
       3000-MAIN-PROCESS.                                               01300001
            EVALUATE EIBAID                                             01310001
                WHEN DFHPF1                                             01311024
                     EXEC CICS                                          01312024
                          XCTL PROGRAM('CIOCCAMN')                      01313049
                               RESP(WS-RESP-CODE)                       01314024
                     END-EXEC                                           01315024
                     IF WS-RESP-CODE NOT = DFHRESP(NORMAL)              01316024
                        MOVE 'PROGRAM CIOCCAMN IS NOT AVAILABLE'        01317049
                                TO MSGO                                 01318024
                        SET WS-MAP-DATAONLY TO TRUE                     01319024
                        PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT   01319124
                     END-IF                                             01319224
                WHEN DFHPF3                                             01320001
                     MOVE 'THANK YOU FOR USING THE SYSTEM' TO WS-MESSAGE01330001
                     EXEC CICS                                          01340001
                          SEND CONTROL                                  01350001
                          CURSOR                                        01360001
                          ERASE                                         01370001
                          FREEKB                                        01380001
                          ALARM                                         01390001
                     END-EXEC                                           01400001
                     EXEC CICS                                          01410001
                          SEND FROM(WS-MESSAGE)                         01420001
                     END-EXEC                                           01430001
                     PERFORM 5010-RETURN THRU 5010-RETURN-EXIT          01440001
                WHEN DFHCLEAR                                           01450001
                     EXEC CICS                                          01460001
                           SEND CONTROL                                 01470001
                           CURSOR                                       01480001
                           ERASE                                        01490001
                           FREEKB                                       01500001
                           ALARM                                        01510001
                     END-EXEC                                           01520001
                     PERFORM 5010-RETURN THRU 5010-RETURN-EXIT          01530001
                WHEN DFHPF7                                             01531049
                     INITIALIZE WS-COMMAREA                             01531149
                     MOVE 'Y' TO WS-FIRST-SEND                          01531249
                     MOVE LK-OPTION TO WS-OPTION                        01531349
                     MOVE LK-APPL-ID TO WS-APPL-ID                      01531449
                     EXEC CICS                                          01532049
                          XCTL PROGRAM('CIOCCA05')                      01533049
                               COMMAREA(WS-COMMAREA)                    01534049
                               RESP(WS-RESP-CODE)                       01535049
                     END-EXEC                                           01536049
                     IF WS-RESP-CODE NOT = DFHRESP(NORMAL)              01537049
                        MOVE 'PROGRAM CIOCCA05 IS NOT AVAILABLE'        01538049
                                TO MSGO                                 01539049
                        SET WS-MAP-DATAONLY TO TRUE                     01539149
                        PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT   01539249
                     END-IF                                             01539349
                WHEN DFHPF9                                             01540001
                     MOVE LOW-VALUES TO CICA06O                         01550053
                     SET WS-MAP-ERASE TO TRUE                           01560001
                     PERFORM 3030-SEND-MAP                              01570001
                        THRU 3030-SEND-MAP-EXIT                         01580001
                WHEN DFHENTER                                           01590041
                     IF WS-RESP-CODE NOT EQUAL DFHRESP(NORMAL)          01600041
                        MOVE 'INVALID REQUEST, CHECK YOUR INPUT.'       01610041
                             TO MSGO                                    01620041
                        SET WS-MAP-DATAONLY TO TRUE                     01630041
                        PERFORM 3030-SEND-MAP                           01640041
                           THRU 3030-SEND-MAP-EXIT                      01650041
                     ELSE                                               01660041
                        PERFORM 3010-CHECK-INPUT                        01670041
                           THRU 3010-CHECK-INPUT-EXIT                   01680041
                        PERFORM 3020-UPD-APPLICAN                       01690049
                           THRU 3020-UPD-APPLICAN-EXIT                  01700049
                     END-IF                                             01710041
                WHEN OTHER                                              01720041
                     MOVE 'INVALID KEY PRESSED!' TO MSGO                01730041
                     SET WS-MAP-DATAONLY TO TRUE                        01740041
                     PERFORM 3030-SEND-MAP                              01750041
                        THRU 3030-SEND-MAP-EXIT                         01760041
            END-EVALUATE                                                01770001
            .                                                           01780001
       3000-MAIN-PROCESS-EXIT.                                          01790001
            EXIT.                                                       01800001
      *                                                                 01810001
       3010-CHECK-INPUT.                                                01820001
            IF COMMUL NOT = 0                                           01830009
               INITIALIZE CIMENU-REC                                    01840009
               MOVE COMMUI TO CIMENU-TRANSID                            01850009
               EXEC CICS READ                                           01860009
                    FILE('CIMENU')                                      01870009
                    INTO(CIMENU-REC)                                    01880009
                    RIDFLD(CIMENU-TRANSID)                              01890009
                    RESP(WS-RESP-CODE)                                  01900009
               END-EXEC                                                 01910009
               EVALUATE WS-RESP-CODE                                    01911009
                   WHEN DFHRESP(NORMAL)                                 01912009
                        EXEC CICS                                       01913009
                             XCTL PROGRAM(CIMENU-PGM)                   01914009
                             COMMAREA(CIMENU-TRANSID)                   01915063
                             RESP(WS-RESP-CODE)                         01916009
                        END-EXEC                                        01917009
                        IF WS-RESP-CODE NOT = DFHRESP(NORMAL)           01918009
                        STRING 'PROGRAM ' DELIMITED BY SIZE             01919010
                               CIMENU-PGM DELIMITED BY SPACE            01919110
                               ' IS NOT AVAILABLE' DELIMITED BY SIZE    01919210
                               INTO MSGO                                01919310
                           SET WS-MAP-DATAONLY TO TRUE                  01919409
                           PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT01919509
                        END-IF                                          01919609
                   WHEN DFHRESP(NOTFND)                                 01919709
                        MOVE 'INVALID TRANSATION ID!' TO MSGO           01919809
                        SET WS-MAP-DATAONLY TO TRUE                     01919909
                        PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT   01920009
                   WHEN OTHER                                           01920109
                        MOVE 'CIMENU FILE ERROR!' TO MSGO               01920209
                        SET WS-MAP-DATAONLY TO TRUE                     01920309
                        PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT   01920409
               END-EVALUATE                                             01920509
            END-IF                                                      01920609
            IF  (COMPLL = 0 OR COMPRL = 0 OR REFUR1L = 0                01920749
                 OR CREDIL = 0 OR CREDDL = 0 OR CREDRL = 0              01920849
                 OR REFUR2L = 0 OR COMMTL = 0 OR FINLL = 0)             01920968
               MOVE 'FIELDS CAN NOT BE EMPTY' TO MSGO                   01921609
               SET WS-MAP-DATAONLY TO TRUE                              01921709
               PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT            01921809
            END-IF                                                      01921909
            IF (COMPRI NOT = 001 AND COMPRI NOT = 002)                  01922050
               MOVE 'INVAILD  COMPUTED RESULT' TO MSGO                  01922150
               SET WS-MAP-DATAONLY TO TRUE                              01922218
               PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT            01922318
            END-IF                                                      01922418
            IF (REFUR1I NOT = 001 AND REFUR1I NOT = 002)                01922550
               MOVE 'INVAILD REFUSE REASON' TO MSGO                     01922650
               SET WS-MAP-DATAONLY TO TRUE                              01922718
               PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT            01922818
            END-IF                                                      01922918
            IF (COMPRI = 001 AND REFUR1I NOT = 001)                     01923060
               MOVE 'INVAILD REFUSE REASON MUST = 001 ' TO MSGO         01923160
               SET WS-MAP-DATAONLY TO TRUE                              01923260
               PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT            01923360
            END-IF                                                      01923460
            IF (COMPRI NOT = 001 AND REFUR1I = 001)                     01923560
               MOVE 'INVAILD REFUSE REASON CAN NOT = 001 ' TO MSGO      01923660
               SET WS-MAP-DATAONLY TO TRUE                              01923760
               PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT            01923860
            END-IF                                                      01923960
            IF (CREDRI NOT = 001 AND CREDRI NOT = 002)                  01924050
               MOVE 'INVAILD CREDITING RESULT' TO MSGO                  01924150
               SET WS-MAP-DATAONLY TO TRUE                              01924209
               PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT            01924309
            END-IF                                                      01924409
            IF (REFUR2I NOT = 001 AND REFUR2I NOT = 002 )               01924550
               MOVE 'INVAILD REFUSE REASON' TO MSGO                     01924650
               SET WS-MAP-DATAONLY TO TRUE                              01924713
               PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT            01924813
            END-IF                                                      01925013
            IF (CREDRI = 001 AND REFUR2I NOT = 001)                     01926060
               MOVE 'REFUSE REASON MUST = 001 ' TO MSGO                 01926167
               SET WS-MAP-DATAONLY TO TRUE                              01926260
               PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT            01926360
            END-IF                                                      01926460
            IF (CREDRI NOT = 001 AND REFUR2I = 001)                     01926560
               MOVE 'REFUSE REASON CAN NOT = 001 ' TO MSGO              01926667
               SET WS-MAP-DATAONLY TO TRUE                              01926760
               PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT            01926860
            END-IF                                                      01926968
            .                                                           01928001
      *                                                                 01930001
       3010-CHECK-INPUT-EXIT.                                           01940001
            EXIT.                                                       01950001
      *                                                                 01960001
       3020-UPD-APPLICAN.                                               01970050
            INITIALIZE SDCA-SERVICE-COMMAREA                            01980050
            MOVE 'VBS.CI.APPLICAN.INQ' TO SD-SRV-NAME                   01990050
            INITIALIZE CIC0007I-REC                                     02000050
            MOVE LK-APPL-ID TO CIC0007I-ID                              02010050
            MOVE CIC0007I-REC TO SD-SRV-INPUT-DATA                      02020050
            EXEC CICS                                                   02030050
                 LINK                                                   02040050
                 PROGRAM(WS-PGM-SRV-DRIVER)                             02050050
                 COMMAREA(WS-SRV-COMMAREA)                              02060050
                 RESP(WS-RESP-CODE)                                     02070050
            END-EXEC                                                    02080050
            EVALUATE WS-RESP-CODE                                       02090050
                WHEN DFHRESP(NORMAL)                                    02100050
                     IF SD-RESP-CODE EQUAL ZEROS                        02110050
                        INITIALIZE CIC0007O-REC                         02120050
                        MOVE SD-SRV-OUTPUT-DATA TO CIC0007O-REC         02130050
                     ELSE                                               02140050
                        MOVE 'APPLICATION READ ERROR' TO MSGO           02150050
                        SET WS-MAP-DATAONLY TO TRUE                     02160050
                        PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT   02170050
                     END-IF                                             02180050
                WHEN OTHER                                              02190050
                     MOVE 'SERVICE PROGRAM ERROR' TO MSGO               02200050
                     SET WS-MAP-DATAONLY TO TRUE                        02210050
                     PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT      02220050
            END-EVALUATE                                                02230050
      *                                                                 02240050
            INITIALIZE SDCA-SERVICE-COMMAREA                            02250050
            MOVE 'VBS.CI.APPLICAN.UPD' TO SD-SRV-NAME                   02260050
            INITIALIZE CIC0006I-REC                                     02270050
            MOVE CIC0007O-REC TO CIC0006I-REC                           02280050
            PERFORM 3040-CHANGE-MANCRE                                  02290050
               THRU 3040-CHANGE-MANCRE-EXIT                             02300050
            MOVE CIC0006I-REC  TO SD-SRV-INPUT-DATA                     02310050
            EXEC CICS                                                   02320050
                 LINK                                                   02330050
                 PROGRAM(WS-PGM-SRV-DRIVER)                             02340050
                 COMMAREA(WS-SRV-COMMAREA)                              02350050
                 RESP(WS-RESP-CODE)                                     02360050
            END-EXEC                                                    02370050
            EVALUATE WS-RESP-CODE                                       02380050
                WHEN DFHRESP(NORMAL)                                    02390050
                     IF SD-RESP-CODE EQUAL ZEROS                        02400050
                        MOVE 'CREDIT MANCRE SUCCESSFUL'                 02410050
                          TO MSGO                                       02411050
                        MOVE 'T' TO WS-FIRST-SEND                       02412050
                        MOVE 'PRESS ANY KEY TO RETURN' TO KEYO          02413050
                     ELSE                                               02414050
                        MOVE SD-RESP-ADDITIONAL TO MSGO                 02415050
                     END-IF                                             02416050
                WHEN OTHER                                              02417050
                     MOVE 'SERVICE PROGRAM ERROR' TO MSGO               02418050
            END-EVALUATE                                                02419050
            SET WS-MAP-DATAONLY TO TRUE                                 02419150
            PERFORM 3030-SEND-MAP                                       02419250
               THRU 3030-SEND-MAP-EXIT                                  02419350
            .                                                           02420001
      *                                                                 02430001
       3020-UPD-APPLICAN-EXIT.                                          02440050
            EXIT.                                                       02450001
      *                                                                 02460001
       3030-SEND-MAP.                                                   02470001
            PERFORM 1010-ASK-TIME-DATE                                  02480001
               THRU 1010-ASK-TIME-DATE-EXIT                             02490001
               MOVE WS-DATEOUT TO CREDDO                                02491059
               MOVE ATTR-PROT-SKIP-MDT TO CREDDA                        02492058
               MOVE '10000.00' TO COMPLI                                02493062
               MOVE ATTR-PROT-SKIP-MDT TO COMPLA                        02494061
               MOVE '001' TO COMPRI                                     02495062
               MOVE ATTR-PROT-SKIP-MDT TO COMPRA                        02496062
               MOVE '001' TO REFUR1I                                    02497062
               MOVE ATTR-PROT-SKIP-MDT TO REFUR1A                       02498062
            EVALUATE TRUE                                               02500001
                WHEN WS-MAP-ERASE                                       02510001
                     EXEC CICS SEND                                     02520001
                          MAP('CICA06')                                 02530051
                          MAPSET('CICA06')                              02540051
                          FROM(CICA06O)                                 02550051
                          ERASE                                         02560001
                     END-EXEC                                           02570001
                WHEN WS-MAP-DATAONLY                                    02580001
                     EXEC CICS SEND                                     02590001
                          MAP('CICA06')                                 02600051
                          MAPSET('CICA06')                              02610051
                          FROM(CICA06O)                                 02620051
                          DATAONLY                                      02630001
                     END-EXEC                                           02640001
            END-EVALUATE                                                02650001
            PERFORM 5020-RETURN-TRANS THRU 5020-RETURN-TRANS-EXIT       02670001
            .                                                           02680001
      *                                                                 02690001
       3030-SEND-MAP-EXIT.                                              02700001
            EXIT.                                                       02710001
      *                                                                 02710137
       3040-CHANGE-MANCRE.                                              02711052
            EVALUATE CREDRI                                             02711158
                WHEN 001                                                02711258
                     MOVE 005       TO        CIC0006I-STATUS           02712058
                WHEN 002                                                02713058
                     MOVE 015       TO        CIC0006I-STATUS           02713158
            END-EVALUATE                                                02713258
            MOVE CREDII    TO        CIC0006I-MANCRE-ID                 02714056
            MOVE CREDDI    TO        CIC0006I-MANCRE-DATE               02715056
            MOVE CREDRI    TO        CIC0006I-MANCRE-RESULT             02716056
            MOVE REFUR2I   TO        CIC0006I-MANCRE-REFUSE-REASON      02717056
            MOVE COMMTI    TO        CIC0006I-MANCRE-COMMENT            02718056
            MOVE COMPLI    TO        CIC0006I-COMPUTE-LIMIT             02719056
            MOVE COMPRI    TO        CIC0006I-COMPUTE-RESULT            02719156
            MOVE REFUR1I   TO        CIC0006I-COMPUTE-REFUSE-REASON     02719256
            MOVE FINLI     TO        CIC0006I-FINAL-LIMIT               02719356
            .                                                           02721537
      *                                                                 02721637
       3040-CHANGE-MANCRE-EXIT.                                         02721752
            EXIT.                                                       02721837
      *                                                                 02761038
       4000-POST-PROCESSING.                                            02762038
      *                                                                 02763038
       4000-POST-PROCESSING-EXIT.                                       02764038
            EXIT.                                                       02765038
      *                                                                 02770001
       5000-CLEAN-UP.                                                   02780001
            PERFORM 5010-RETURN                                         02790001
               THRU 5010-RETURN-EXIT                                    02800001
            .                                                           02810001
      *                                                                 02820001
       5000-CLEAN-UP-EXIT.                                              02830001
            EXIT.                                                       02840001
      *                                                                 02850001
       5010-RETURN.                                                     02860001
            EXEC CICS RETURN END-EXEC                                   02870001
            .                                                           02880001
       5010-RETURN-EXIT.                                                02890001
            EXIT.                                                       02900001
      *                                                                 02910001
       5020-RETURN-TRANS.                                               02920001
            IF WS-FIRST-SEND = 'T'                                      02920246
               EXEC CICS RETURN TRANSID('CICA')                         02920347
               END-EXEC                                                 02920546
            END-IF                                                      02920946
            INITIALIZE WS-COMMAREA                                      02921046
            MOVE 'N' TO WS-FIRST-SEND                                   02922046
            MOVE LK-OPTION TO WS-OPTION                                 02923052
            MOVE LK-APPL-ID TO WS-APPL-ID                               02924052
            EXEC CICS RETURN TRANSID('CIA6')                            02930048
                      COMMAREA(WS-COMMAREA)                             02940004
            END-EXEC                                                    02950001
            .                                                           02960001
       5020-RETURN-TRANS-EXIT.                                          02970001
            EXIT.                                                       02980001
