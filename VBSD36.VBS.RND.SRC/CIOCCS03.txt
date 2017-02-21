       IDENTIFICATION DIVISION.                                         00010001
       PROGRAM-ID. CIOCCS03.                                            00020064
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
      *2015-01-06    KRIS      INITIAL VERSION                          00130032
      ***************************************************************** 00140001
       ENVIRONMENT DIVISION.                                            00150001
       DATA DIVISION.                                                   00160001
       WORKING-STORAGE SECTION.                                         00170001
      *                                                                 00180001
       77 WS-BEGIN              PIC X(17) VALUE 'CIOCCS03 WS BEGIN'.    00190064
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
       COPY CICS03.                                                     00350065
      *MAP CONTROL                                                      00360001
       COPY DFHBMSCA.                                                   00370001
      *CICS FUNCTION KEYS                                               00380001
       COPY DFHAID.                                                     00390001
      *CIMENU                                                           00400001
       COPY CIMENU.                                                     00410001
      *                                                                 00420001
       COPY CIC0012I.                                                   00421059
       COPY CIC0012O.                                                   00422059
      *                                                                 00423039
       COPY CIC0002I.                                                   00424059
       COPY CIC0002O.                                                   00425059
      *                                                                 00426059
       01 WS-SRV-COMMAREA.                                              00430001
      *SERVICE REQUEST/RESPONSE COMMAREA                                00440001
       COPY SD01WS.                                                     00450001
      *                                                                 00460001
       01 WS-COMMAREA.                                                  00470004
          05 WS-FIRST-SEND      PIC X(1).                               00480033
      *   05 WS-STEP            PIC 9(3).                               00490065
          05 WS-APPL-ID         PIC 9(13).                              00500033
       01 WS-COMMAREA-CIB2.                                             00510079
          05 WS-FIRST-SEND      PIC X(1).                               00520079
          05 WS-ID-NUMBER       PIC 9(18).                              00520179
          05 WS-ID-TYPE         PIC 9(3).                               00520279
       77 WS-END                PIC X(17) VALUE 'CIOCCS03 WS END'.      00521064
      *                                                                 00530001
       LINKAGE SECTION.                                                 00540001
       01 DFHCOMMAREA.                                                  00550001
          05 LK-FIRST-SEND      PIC X(1).                               00560001
      *   05 LK-STEP            PIC 9(3).                               00570064
          05 LK-APPL-ID         PIC 9(13).                              00571032
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
               MOVE LOW-VALUES TO CICS03O                               00820164
               PERFORM 1020-GET-EXISTING-CUST                           00821031
                  THRU 1020-GET-EXISTING-CUST-EXIT                      00822031
               SET WS-MAP-ERASE TO TRUE                                 00840001
               PERFORM 3030-SEND-MAP                                    00850001
                  THRU 3030-SEND-MAP-EXIT                               00860001
      * NOT FIRST SHOW                                                  00870001
            ELSE                                                        00880001
                  MOVE LOW-VALUES TO CICS03I                            00890065
                  EXEC CICS RECEIVE MAP('CICS03')                       00900064
                                   MAPSET('CICS03')                     00910064
                                   INTO(CICS03I)                        00920065
                                   RESP(WS-RESP-CODE)                   00930001
                  END-EXEC                                              00940001
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
       1020-GET-EXISTING-CUST.                                          01232031
            INITIALIZE SDCA-SERVICE-COMMAREA                            01232135
            MOVE 'VBS.CI.CUSTAPPL.INQ' TO SD-SRV-NAME                   01232248
            INITIALIZE CIC0012I-REC                                     01232360
            MOVE LK-APPL-ID TO CIC0012I-APPL-ID                         01232460
            MOVE CIC0012I-REC TO SD-SRV-INPUT-DATA                      01232560
            EXEC CICS                                                   01232635
                 LINK                                                   01232735
                 PROGRAM(WS-PGM-SRV-DRIVER)                             01232835
                 COMMAREA(WS-SRV-COMMAREA)                              01232935
                 RESP(WS-RESP-CODE)                                     01233035
            END-EXEC                                                    01233135
            EVALUATE WS-RESP-CODE                                       01233235
                WHEN DFHRESP(NORMAL)                                    01233335
                     IF SD-RESP-CODE EQUAL ZEROS                        01233435
                        PERFORM 1030-POPULATE-CUST                      01233535
                           THRU 1030-POPULATE-CUST-EXIT                 01233635
                     END-IF                                             01233735
            END-EVALUATE                                                01233835
            .                                                           01233935
      *                                                                 01234031
       1020-GET-EXISTING-CUST-EXIT.                                     01234131
            EXIT.                                                       01235031
      *                                                                 01240001
       1030-POPULATE-CUST.                                              01241077
            INITIALIZE CIC0012O-REC                                     01242061
            MOVE SD-SRV-OUTPUT-DATA       TO CIC0012O-REC               01243061
            MOVE CIC0012O-NAME            TO CNAMEO                     01244065
            MOVE CIC0012O-CUST-ID-NUMBER  TO IDNUMO                     01244175
            MOVE CIC0012O-MOBILE          TO MOBILEO                    01246075
            MOVE CIC0012O-BIRTH-DATE      TO BIRTHO                     01247060
            MOVE CIC0012O-ID              TO APPLYIDO                   01248075
            MOVE CIC0012O-CUST-ID-TYPE    TO IDTYPEO                    01249160
            MOVE CIC0012O-COMPANY-NAME    TO COMNAMEO                   01249275
            MOVE CIC0012O-COMPANY-ADRESS  TO COMADDO                    01249375
            MOVE CIC0012O-EMAIL           TO EMAILO                     01249875
            EVALUATE CIC0012O-BILL-ADDR                                 01249975
                WHEN 001                                                01250075
                     MOVE CIC0012O-EMAIL TO BILLADDO                    01250176
                WHEN 002                                                01250275
                     MOVE CIC0012O-LIVE-ADDRESS TO BILLADDO             01250375
                WHEN 003                                                01250475
                     MOVE CIC0012O-COMPANY-ADRESS TO BILLADDO           01250575
            END-EVALUATE                                                01251075
            EVALUATE CIC0012O-STATUS                                    01251671
                WHEN 001                                                01251771
                     MOVE 'APPLICATION IS IN PROCESS, WAITTING FOR CHECK01251871
      -                   '.'             TO STATUSO                    01251971
                WHEN 002                                                01252071
                     MOVE 'APPLICATION IS IN PROCESS, WAITTING FOR REVIE01252171
      -                   'W.'            TO STATUSO                    01252271
                WHEN 003                                                01252371
                     MOVE 'APPLICATION IS IN PROCESS, WAITTING FOR INVES01252471
      -                   'TIGATE.'       TO STATUSO                    01252571
                WHEN 004                                                01252671
                     MOVE 'APPLICATION IS IN PROCESS, WAITTING FOR CREDI01252771
      -                   'T.'             TO STATUSO                   01252871
                WHEN 005                                                01252971
                     MOVE 'APPLICATION IS DONE, WAITTING FOR PRODUCE CAR01253071
      -                   'D.'             TO STATUSO                   01253171
                WHEN 010                                                01253271
                     STRING 'RETURN FROM REVIEW. '                      01253383
                            DELIMITED BY SIZE                           01253483
                            CIC0012O-REVIEW-COMMENT DELIMITED BY SIZE   01253583
                            INTO STATUSO                                01253683
                WHEN 011                                                01253971
                     STRING 'RETURN FROM INVESTIGATE. '                 01254083
                            DELIMITED BY SIZE                           01254183
                            CIC0012O-CREINV-COMMENT DELIMITED BY SIZE   01254283
                            INTO STATUSO                                01254383
                WHEN 012                                                01254671
                     STRING 'CHECK FAILED. '                            01254783
                            DELIMITED BY SIZE                           01254871
                            CIC0012O-INTCHK-COMMENT DELIMITED BY SIZE   01254983
                            INTO STATUSO                                01255071
                WHEN 013                                                01255171
                     STRING 'REVIEW FAILED. '                           01255283
                            DELIMITED BY SIZE                           01255371
                            CIC0012O-REVIEW-COMMENT DELIMITED BY SIZE   01255483
                            INTO STATUSO                                01255571
                WHEN 014                                                01255671
                     STRING 'INVESTIGATE FAILED. '                      01255783
                            DELIMITED BY SIZE                           01255871
                            CIC0012O-CREINV-COMMENT DELIMITED BY SIZE   01255983
                            INTO STATUSO                                01256071
                WHEN 015                                                01256171
                     STRING 'CREDIT FAILED. '                           01256283
                            DELIMITED BY SIZE                           01256371
                            CIC0012O-MANCRE-COMMENT DELIMITED BY SIZE   01256483
                            INTO STATUSO                                01256571
                WHEN OTHER                                              01256675
                     MOVE 'STATUS IS UNKNOWN' TO STATUSO                01256775
            END-EVALUATE                                                01256871
            .                                                           01256971
      *                                                                 01257071
       1030-POPULATE-CUST-EXIT.                                         01257171
            EXIT.                                                       01257271
       2000-PRE-PROCESSING.                                             01258071
      *                                                                 01260001
       2000-PRE-PROCESSING-EXIT.                                        01270001
            EXIT.                                                       01280001
      *                                                                 01290001
       3000-MAIN-PROCESS.                                               01300001
            EVALUATE EIBAID ALSO TRUE                                   01310057
                WHEN DFHPF1 ALSO ANY                                    01311054
                     INITIALIZE WS-COMMAREA-CIB2                        01311179
                     MOVE 'Y' TO WS-FIRST-SEND OF WS-COMMAREA-CIB2      01311279
                     MOVE IDNUMI TO WS-ID-NUMBER                        01311380
                     MOVE IDTYPEI TO WS-ID-TYPE                         01311480
                     EXEC CICS                                          01312036
                          XCTL PROGRAM('CIOCCS02')                      01313079
                               RESP(WS-RESP-CODE)                       01314036
                               COMMAREA(WS-COMMAREA-CIB2)               01314179
                     END-EXEC                                           01315036
                     IF WS-RESP-CODE NOT = DFHRESP(NORMAL)              01316024
                        MOVE 'PROGRAM CIOCCIMN IS NOT AVAILABLE'        01317036
                                TO MSGO                                 01318024
                        SET WS-MAP-DATAONLY TO TRUE                     01319024
                        PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT   01319124
                     END-IF                                             01319224
                WHEN DFHPF3 ALSO ANY                                    01320054
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
                WHEN DFHCLEAR ALSO ANY                                  01450054
                     EXEC CICS                                          01460001
                           SEND CONTROL                                 01470001
                           CURSOR                                       01480001
                           ERASE                                        01490001
                           FREEKB                                       01500001
                           ALARM                                        01510001
                     END-EXEC                                           01520001
                     PERFORM 5010-RETURN THRU 5010-RETURN-EXIT          01530001
      *         WHEN DFHPF9                                             01540054
      *              MOVE LOW-VALUES TO CICS03O                         01550064
      *              SET WS-MAP-ERASE TO TRUE                           01560054
      *              PERFORM 3030-SEND-MAP                              01570054
      *                 THRU 3030-SEND-MAP-EXIT                         01580054
                 WHEN DFHENTER ALSO ANY                                 01590066
                      IF WS-RESP-CODE NOT EQUAL DFHRESP(NORMAL)         01600001
                         MOVE 'INVALID REQUEST, CHECK YOUR INPUT.'      01610001
                              TO MSGO                                   01620001
                         SET WS-MAP-DATAONLY TO TRUE                    01630001
                         PERFORM 3030-SEND-MAP                          01640001
                            THRU 3030-SEND-MAP-EXIT                     01650001
                      ELSE                                              01660001
                         PERFORM 3010-CHECK-INPUT                       01670001
                            THRU 3010-CHECK-INPUT-EXIT                  01680001
                         PERFORM 3020-XCTL                              01690064
                            THRU 3020-XCTL-EXIT                         01700064
                      END-IF                                            01710001
                 WHEN OTHER                                             01720001
                      MOVE 'INVALID KEY PRESSED!' TO MSGO               01730001
                      SET WS-MAP-DATAONLY TO TRUE                       01740001
                      PERFORM 3030-SEND-MAP                             01750001
                         THRU 3030-SEND-MAP-EXIT                        01760001
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
                             COMMAREA(CIMENU-TRANSID)                   01915078
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
            .                                                           01927001
      *                                                                 01930001
       3010-CHECK-INPUT-EXIT.                                           01940001
            EXIT.                                                       01950001
      *                                                                 01960001
       3020-XCTL.                                                       01970064
            EXEC CICS READ                                              01980064
                 FILE('CIMENU')                                         01990064
                 INTO(CIMENU-REC)                                       02000064
                 RIDFLD(CIMENU-TRANSID)                                 02010064
                 RESP(WS-RESP-CODE)                                     02020064
            END-EXEC                                                    02030064
            EVALUATE WS-RESP-CODE                                       02040064
                WHEN DFHRESP(NORMAL)                                    02050064
                     EXEC CICS                                          02060064
                          XCTL PROGRAM(CIMENU-PGM)                      02070064
                          COMMAREA(WS-COMMAREA)                         02080064
                          RESP(WS-RESP-CODE)                            02090064
                     END-EXEC                                           02100064
                     IF WS-RESP-CODE NOT = DFHRESP(NORMAL)              02110064
                        STRING 'PROGRAM ' DELIMITED BY SIZE             02120064
                               CIMENU-PGM DELIMITED BY SPACE            02130064
                               ' IS NOT AVAILABLE' DELIMITED BY SIZE    02140064
                               INTO MSGO                                02150064
                        SET WS-MAP-DATAONLY TO TRUE                     02160064
                        PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT   02170064
                     END-IF                                             02180064
                WHEN DFHRESP(NOTFND)                                    02190064
                     MOVE 'INVALID TRANSATION ID!' TO MSGO              02200064
                     SET WS-MAP-DATAONLY TO TRUE                        02210064
                     PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT      02220064
                WHEN OTHER                                              02230064
                     MOVE 'CIMENU FILE ERROR!' TO MSGO                  02240064
                     SET WS-MAP-DATAONLY TO TRUE                        02250064
                     PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT      02260064
            END-EVALUATE                                                02270064
            .                                                           02280064
      *                                                                 02430001
       3020-XCTL-EXIT.                                                  02440064
            EXIT.                                                       02450001
      *                                                                 02460001
       3030-SEND-MAP.                                                   02470001
            PERFORM 1010-ASK-TIME-DATE                                  02480001
               THRU 1010-ASK-TIME-DATE-EXIT                             02490001
            MOVE ATTR-PROT-SKIP-MDT TO IDNUMA                           02491081
            MOVE ATTR-PROT-SKIP-MDT TO IDTYPEA                          02492082
            EVALUATE TRUE                                               02500001
                WHEN WS-MAP-ERASE                                       02510001
                     EXEC CICS SEND                                     02520001
                          MAP('CICS03')                                 02530064
                          MAPSET('CICS03')                              02540064
                          FROM(CICS03O)                                 02550064
                          ERASE                                         02560001
                     END-EXEC                                           02570001
                WHEN WS-MAP-DATAONLY                                    02580001
                     EXEC CICS SEND                                     02590001
                          MAP('CICS03')                                 02600064
                          MAPSET('CICS03')                              02610064
                          FROM(CICS03O)                                 02620064
                          DATAONLY                                      02630001
                     END-EXEC                                           02640001
            END-EVALUATE                                                02650001
      *     MOVE '1' TO WS-ENTER-FLAG                                   02660004
            PERFORM 5020-RETURN-TRANS THRU 5020-RETURN-TRANS-EXIT       02670001
            .                                                           02680001
      *                                                                 02690001
       3030-SEND-MAP-EXIT.                                              02700001
            EXIT.                                                       02710001
      *                                                                 02720001
       4000-POST-PROCESSING.                                            02733001
      *                                                                 02740001
       4000-POST-PROCESSING-EXIT.                                       02750001
            EXIT.                                                       02760001
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
      *     INITIALIZE WS-COMMAREA                                      02920169
      *     MOVE 'N' TO WS-FIRST-SEND                                   02921069
      *     MOVE LK-APPL-ID TO WS-APPL-ID                               02922269
            MOVE '1' TO WS-ENTER-FLAG                                   02922369
            EXEC CICS RETURN TRANSID('CIB3')                            02930064
                      COMMAREA(WS-ENTER-FLAG)                           02940070
            END-EXEC                                                    02950001
            .                                                           02960001
       5020-RETURN-TRANS-EXIT.                                          02970001
            EXIT.                                                       02980001
