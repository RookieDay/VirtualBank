       IDENTIFICATION DIVISION.                                         00010001
       PROGRAM-ID. CIOCCA07.                                            00020032
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
       77 WS-BEGIN              PIC X(17) VALUE 'CIOCCA07 WS BEGIN'.    00190032
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
       COPY CICA02.                                                     00350032
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
          05 WS-STEP            PIC 9(3).                               00490033
          05 WS-APPL-ID         PIC 9(13).                              00500033
       77 WS-END                PIC X(17) VALUE 'CIOCCA07 WS END'.      00521032
      *                                                                 00530001
       LINKAGE SECTION.                                                 00540001
       01 DFHCOMMAREA.                                                  00550001
          05 LK-FIRST-SEND      PIC X(1).                               00560001
          05 LK-STEP            PIC 9(3).                               00570032
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
               MOVE LOW-VALUES TO CICA02O                               00820162
               PERFORM 1020-GET-EXISTING-CUST                           00821031
                  THRU 1020-GET-EXISTING-CUST-EXIT                      00822031
               SET WS-MAP-ERASE TO TRUE                                 00840001
               PERFORM 3030-SEND-MAP                                    00850001
                  THRU 3030-SEND-MAP-EXIT                               00860001
      * NOT FIRST SHOW                                                  00870001
            ELSE                                                        00880001
                  MOVE LOW-VALUES TO CICA02I                            00890002
                  EXEC CICS RECEIVE MAP('CICA02')                       00900032
                                   MAPSET('CICA02')                     00910032
                                   INTO(CICA02I)                        00920032
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
       1030-POPULATE-CUST.                                              01241035
            INITIALIZE CIC0012O-REC                                     01242061
            MOVE SD-SRV-OUTPUT-DATA       TO CIC0012O-REC               01243061
            MOVE CIC0012O-NAME            TO NAMEO                      01244060
            MOVE CIC0012O-ENGLISH-NAME    TO ENAMEO                     01245060
            MOVE CIC0012O-NATIONALITY     TO NATIONO                    01246060
            MOVE CIC0012O-BIRTH-DATE      TO BIRTHO                     01247060
            MOVE CIC0012O-GENDER          TO GENDERO                    01248060
            MOVE CIC0012O-MARITAL-STATUS  TO MASTUSO                    01249060
            MOVE CIC0012O-CUST-ID-TYPE    TO IDTYPEO                    01249160
            MOVE CIC0012O-CUST-ID-NUMBER  TO IDNUMO                     01249260
            MOVE CIC0012O-ANNUAL-SALARY   TO ANSRYO                     01249360
            MOVE CIC0012O-MOBILE          TO MOBILEO                    01249460
            MOVE CIC0012O-EMAIL           TO EMAILO                     01249560
            MOVE CIC0012O-BILL-TYPE       TO BITYPEO                    01249660
            MOVE CIC0012O-BILL-ADDR       TO BIADDRO                    01249760
            MOVE CIC0012O-BILL-DATE       TO BIDATEO                    01249860
            MOVE CIC0012O-APP-DATE        TO APDATEO                    01249960
            MOVE CIC0012O-LIVE-COUNTRY    TO LCTRYO                     01250060
            MOVE CIC0012O-LIVE-PROVINCE   TO LPROVO                     01250160
            MOVE CIC0012O-LIVE-CITY       TO LCITYO                     01250260
            MOVE CIC0012O-LIVE-DISTRICT   TO LDISTO                     01250360
            MOVE CIC0012O-LIVE-ZIP-CODE   TO LZIPCO                     01250460
            MOVE CIC0012O-LIVE-ADDRESS    TO LADDRO                     01250560
            MOVE CIC0012O-LIVE-YEARS      TO LYEARSO                    01250660
            MOVE CIC0012O-COMPANY-NAME    TO CNAMEO                     01250760
            MOVE CIC0012O-COMPANY-COUNTRY TO CCTRYO                     01250860
            MOVE CIC0012O-COMPANY-PROVINCE TO CPROVO                    01250960
            MOVE CIC0012O-COMPANY-CITY    TO CCITYO                     01251060
            MOVE CIC0012O-COMPANY-DISTRICT TO CDISTO                    01251160
            MOVE CIC0012O-COMPANY-ZIP-CODE TO CZIPCO                    01251260
            MOVE CIC0012O-COMPANY-ADRESS  TO CADDRO                     01251360
            MOVE CIC0012O-COMPANY-SERVE-YEAR TO CSRVYO                  01251460
            .                                                           01254235
      *                                                                 01254335
       1030-POPULATE-CUST-EXIT.                                         01254435
            EXIT.                                                       01254535
       2000-PRE-PROCESSING.                                             01255001
      *                                                                 01260001
       2000-PRE-PROCESSING-EXIT.                                        01270001
            EXIT.                                                       01280001
      *                                                                 01290001
       3000-MAIN-PROCESS.                                               01300001
            EVALUATE EIBAID ALSO TRUE                                   01310057
                WHEN DFHPF1 ALSO ANY                                    01311054
                     EXEC CICS                                          01312036
                          XCTL PROGRAM('CIOCCIMN')                      01313036
                               RESP(WS-RESP-CODE)                       01314036
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
      *              MOVE LOW-VALUES TO CICA02O                         01550054
      *              SET WS-MAP-ERASE TO TRUE                           01560054
      *              PERFORM 3030-SEND-MAP                              01570054
      *                 THRU 3030-SEND-MAP-EXIT                         01580054
                WHEN DFHPF8 ALSO ANY                                    01581063
                     INITIALIZE WS-COMMAREA                             01582063
                     MOVE 'Y' TO WS-FIRST-SEND                          01583063
                     MOVE LK-STEP TO WS-STEP                            01584063
                     MOVE LK-APPL-ID TO WS-APPL-ID                      01585063
                     EXEC CICS                                          01586063
                          XCTL PROGRAM('CIOCCA04')                      01587063
                               RESP(WS-RESP-CODE)                       01588063
                               COMMAREA(WS-COMMAREA)                    01589063
                     END-EXEC                                           01589163
                     IF WS-RESP-CODE NOT = DFHRESP(NORMAL)              01589263
                        MOVE 'PROGRAM CIOCCA04 IS NOT AVAILABLE'        01589363
                                TO MSGO                                 01589463
                        SET WS-MAP-DATAONLY TO TRUE                     01589563
                        PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT   01589663
                     END-IF                                             01589763
                 WHEN DFHENTER ALSO LK-STEP = 001                       01590054
                      IF WS-RESP-CODE NOT EQUAL DFHRESP(NORMAL)         01600001
                         MOVE 'INVALID REQUEST, CHECK YOUR INPUT.'      01610001
                              TO MSGO                                   01620001
                         SET WS-MAP-DATAONLY TO TRUE                    01630001
                         PERFORM 3030-SEND-MAP                          01640001
                            THRU 3030-SEND-MAP-EXIT                     01650001
                      ELSE                                              01660001
                         PERFORM 3010-CHECK-INPUT                       01670001
                            THRU 3010-CHECK-INPUT-EXIT                  01680001
                         PERFORM 3020-CUST-UPDATE                       01690055
                            THRU 3020-CUST-UPDATE-EXIT                  01700055
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
                             COMMAREA(CIMENU-TRANSID)                   01915068
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
            IF (EMAILL = 0 OR MOBILEL = 0 OR GENDERL = 0                01920715
                 OR NATIONL = 0 OR BIRTHL = 0 OR MASTUSL = 0            01920813
                 OR ANSRYL = 0 OR APDATEL = 0 OR BITYPEL = 0            01920919
                 OR BIDATEL = 0 OR BIADDRL = 0 OR BIRTHL = 0            01921022
                 OR LCTRYL = 0 OR LPROVL = 0 OR LCITYL = 0              01921122
                 OR LDISTL = 0 OR LZIPCL = 0 OR LYEARSL = 0             01921222
                 OR LADDRL = 0 OR CNAMEL = 0 OR CZIPCL = 0              01921322
                 OR CCTRYL = 0 OR CPROVL = 0 OR CCITYL = 0              01921422
                 OR CDISTL = 0 OR CADDRL = 0 OR CSRVYL = 0)             01921522
               MOVE 'FIELDS CAN NOT BE EMPTY' TO MSGO                   01921609
               SET WS-MAP-DATAONLY TO TRUE                              01921709
               PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT            01921809
            END-IF                                                      01921909
            IF (GENDERI NOT = 'M' AND GENDERI NOT = 'F')                01922018
               MOVE 'INVAILD GENDER' TO MSGO                            01922118
               SET WS-MAP-DATAONLY TO TRUE                              01922218
               PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT            01922318
            END-IF                                                      01922418
            IF (MASTUSI NOT = 'M' AND MASTUSI NOT = 'U')                01922518
               MOVE 'INVAILD MARITAL STATUS' TO MSGO                    01922618
               SET WS-MAP-DATAONLY TO TRUE                              01922718
               PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT            01922818
            END-IF                                                      01922918
            IF (BITYPEI NOT = 001 AND BITYPEI NOT = 002)                01923021
               MOVE 'INVAILD BILL TYPE' TO MSGO                         01923113
               SET WS-MAP-DATAONLY TO TRUE                              01923209
               PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT            01923309
            END-IF                                                      01923409
            IF (BIDATEI NOT = 05 AND BIDATEI NOT = 15 AND BIDATEI       01923518
                 NOT = 25)                                              01923618
               MOVE 'INVAILD BILL DATE' TO MSGO                         01923713
               SET WS-MAP-DATAONLY TO TRUE                              01923813
               PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT            01923913
            END-IF                                                      01924013
            IF (BIADDRI NOT = 001 AND BIADDRI NOT = 002 AND BIADDRI     01924121
                 NOT = 003)                                             01924221
               MOVE 'INVAILD BILL ADDRESS' TO MSGO                      01924313
               SET WS-MAP-DATAONLY TO TRUE                              01924413
               PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT            01924513
            END-IF                                                      01924613
            .                                                           01927001
      *                                                                 01930001
       3010-CHECK-INPUT-EXIT.                                           01940001
            EXIT.                                                       01950001
      *                                                                 01960001
       3020-CUST-UPDATE.                                                01970054
            INITIALIZE SDCA-SERVICE-COMMAREA                            01980054
            MOVE 'VBS.CI.CUSTOMER.UPD' TO SD-SRV-NAME                   01990054
            INITIALIZE CIC0002I-REC                                     02000059
            MOVE CIC0012O-REC TO CIC0002I-REC                           02001064
            PERFORM 3060-POPULATE-CICUS                                 02010054
               THRU 3060-POPULATE-CICUS-EXIT                            02020054
            MOVE CIC0002I-REC  TO SD-SRV-INPUT-DATA                     02030059
            EXEC CICS                                                   02040054
                 LINK                                                   02050054
                 PROGRAM(WS-PGM-SRV-DRIVER)                             02060054
                 COMMAREA(WS-SRV-COMMAREA)                              02070054
                 RESP(WS-RESP-CODE)                                     02080054
            END-EXEC                                                    02090054
            EVALUATE WS-RESP-CODE                                       02100054
                WHEN DFHRESP(NORMAL)                                    02110054
                     IF SD-RESP-CODE EQUAL ZEROS                        02120054
                        MOVE 'CUSTOMER UPDATED SUCCESSFULLY' TO MSGO    02130067
                     ELSE                                               02160054
                        MOVE SD-RESP-ADDITIONAL TO MSGO                 02170054
                     END-IF                                             02180054
                WHEN OTHER                                              02190054
                     MOVE 'SERVICE PROGRAM ERROR' TO MSGO               02200054
            END-EVALUATE                                                02210054
            SET WS-MAP-DATAONLY TO TRUE                                 02220054
            PERFORM 3030-SEND-MAP                                       02230054
               THRU 3030-SEND-MAP-EXIT                                  02240054
            .                                                           02420054
      *                                                                 02430001
       3020-CUST-UPDATE-EXIT.                                           02440054
            EXIT.                                                       02450001
      *                                                                 02460001
       3030-SEND-MAP.                                                   02470001
            IF LK-STEP = 002 OR LK-STEP = 003 OR LK-STEP = 004          02470134
               PERFORM 3040-SET-PROTECT                                 02470232
                  THRU 3040-SET-PROTECT-EXIT                            02470332
               MOVE 'PF1=RETURN PF3=EXIT PF8=NEXT' TO KEYO              02470454
            ELSE                                                        02470552
               PERFORM 3050-SET-UNPROTECT                               02470652
                  THRU 3050-SET-UNPROTECT-EXIT                          02470752
               MOVE 'PF1=RETURN PF3=EXIT PF8=NEXT ENTER=UPDATE' TO KEYO 02470854
            END-IF                                                      02470932
      *                                                                 02471067
            PERFORM 1010-ASK-TIME-DATE                                  02480001
               THRU 1010-ASK-TIME-DATE-EXIT                             02490001
            EVALUATE TRUE                                               02500001
                WHEN WS-MAP-ERASE                                       02510001
                     EXEC CICS SEND                                     02520001
                          MAP('CICA02')                                 02530032
                          MAPSET('CICA02')                              02540032
                          FROM(CICA02O)                                 02550032
                          ERASE                                         02560001
                     END-EXEC                                           02570001
                WHEN WS-MAP-DATAONLY                                    02580001
                     EXEC CICS SEND                                     02590001
                          MAP('CICA02')                                 02600032
                          MAPSET('CICA02')                              02610032
                          FROM(CICA02O)                                 02620032
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
       3040-SET-PROTECT.                                                02721032
            MOVE ATTR-PROT-MDT          TO NAMEA                        02721137
            MOVE ATTR-PROT-MDT          TO ENAMEA                       02721237
            MOVE ATTR-PROT-MDT          TO NATIONA                      02721337
            MOVE ATTR-PROT-MDT          TO BIRTHA                       02721437
            MOVE ATTR-PROT-MDT          TO GENDERA                      02721537
            MOVE ATTR-PROT-MDT          TO MASTUSA                      02721637
            MOVE ATTR-PROT-MDT          TO IDTYPEA                      02721737
            MOVE ATTR-PROT-MDT          TO IDNUMA                       02721851
            MOVE ATTR-PROT-MDT          TO ANSRYA                       02721937
            MOVE ATTR-PROT-MDT          TO MOBILEA                      02722037
            MOVE ATTR-PROT-MDT          TO EMAILA                       02722137
            MOVE ATTR-PROT-MDT          TO BITYPEA                      02722237
            MOVE ATTR-PROT-MDT          TO BIADDRA                      02722337
            MOVE ATTR-PROT-MDT          TO BIDATEA                      02722437
            MOVE ATTR-PROT-MDT          TO APDATEA                      02722537
            MOVE ATTR-PROT-MDT          TO LCTRYA                       02722637
            MOVE ATTR-PROT-MDT          TO LPROVA                       02722737
            MOVE ATTR-PROT-MDT          TO LCITYA                       02722837
            MOVE ATTR-PROT-MDT          TO LDISTA                       02722937
            MOVE ATTR-PROT-MDT          TO LZIPCA                       02723037
            MOVE ATTR-PROT-MDT          TO LADDRA                       02723137
            MOVE ATTR-PROT-MDT          TO LYEARSA                      02723237
            MOVE ATTR-PROT-MDT          TO CNAMEA                       02723337
            MOVE ATTR-PROT-MDT          TO CCTRYA                       02723437
            MOVE ATTR-PROT-MDT           TO CPROVA                      02723537
            MOVE ATTR-PROT-MDT          TO CCITYA                       02723637
            MOVE ATTR-PROT-MDT           TO CDISTA                      02723737
            MOVE ATTR-PROT-MDT           TO CZIPCA                      02723837
            MOVE ATTR-PROT-MDT          TO CADDRA                       02723937
            MOVE ATTR-PROT-MDT           TO CSRVYA                      02724037
            .                                                           02724149
       3040-SET-PROTECT-EXIT.                                           02724232
            EXIT.                                                       02725032
      *                                                                 02725167
       3050-SET-UNPROTECT.                                              02726052
            MOVE ATTR-PROT-SKIP-MDT       TO IDTYPEA                    02726167
            MOVE ATTR-PROT-SKIP-MDT       TO IDNUMA                     02726267
            MOVE ATTR-UNPROT-MDT          TO NAMEA                      02726367
            MOVE ATTR-UNPROT-MDT          TO ENAMEA                     02726467
            MOVE ATTR-UNPROT-MDT          TO NATIONA                    02726567
            MOVE ATTR-UNPROT-MDT          TO BIRTHA                     02726667
            MOVE ATTR-UNPROT-MDT          TO GENDERA                    02726767
            MOVE ATTR-UNPROT-MDT          TO MASTUSA                    02726867
            MOVE ATTR-UNPROT-MDT          TO ANSRYA                     02726967
            MOVE ATTR-UNPROT-MDT          TO MOBILEA                    02727067
            MOVE ATTR-UNPROT-MDT          TO EMAILA                     02727167
            MOVE ATTR-UNPROT-MDT          TO BITYPEA                    02727267
            MOVE ATTR-UNPROT-MDT          TO BIADDRA                    02727367
            MOVE ATTR-UNPROT-MDT          TO BIDATEA                    02727467
            MOVE ATTR-UNPROT-MDT          TO APDATEA                    02727567
            MOVE ATTR-UNPROT-MDT          TO LCTRYA                     02727667
            MOVE ATTR-UNPROT-MDT          TO LPROVA                     02727767
            MOVE ATTR-UNPROT-MDT          TO LCITYA                     02727867
            MOVE ATTR-UNPROT-MDT          TO LDISTA                     02727967
            MOVE ATTR-UNPROT-MDT          TO LZIPCA                     02728067
            MOVE ATTR-UNPROT-MDT          TO LADDRA                     02728167
            MOVE ATTR-UNPROT-MDT          TO LYEARSA                    02728267
            MOVE ATTR-UNPROT-MDT          TO CNAMEA                     02728367
            MOVE ATTR-UNPROT-MDT          TO CCTRYA                     02728467
            MOVE ATTR-UNPROT-MDT          TO CPROVA                     02728567
            MOVE ATTR-UNPROT-MDT          TO CCITYA                     02728667
            MOVE ATTR-UNPROT-MDT          TO CDISTA                     02728767
            MOVE ATTR-UNPROT-MDT          TO CZIPCA                     02728867
            MOVE ATTR-UNPROT-MDT          TO CADDRA                     02728967
            MOVE ATTR-UNPROT-MDT          TO CSRVYA                     02729067
            .                                                           02729167
       3050-SET-UNPROTECT-EXIT.                                         02729267
            EXIT.                                                       02729367
       3060-POPULATE-CICUS.                                             02729467
            MOVE NAMEI     TO        CIC0002I-NAME                      02729567
            MOVE ENAMEI    TO        CIC0002I-ENGLISH-NAME              02729667
            MOVE NATIONI   TO        CIC0002I-NATIONALITY               02729767
            MOVE BIRTHI    TO        CIC0002I-BIRTH-DATE                02729867
            MOVE GENDERI   TO        CIC0002I-GENDER                    02729967
            MOVE MASTUSI   TO        CIC0002I-MARITAL-STATUS            02730067
            MOVE IDTYPEI   TO        CIC0002I-ID-TYPE                   02730167
            MOVE IDNUMI    TO        CIC0002I-ID-NUMBER                 02730267
            MOVE ANSRYI    TO        CIC0002I-ANNUAL-SALARY             02730367
            MOVE MOBILEI   TO        CIC0002I-MOBILE                    02730467
            MOVE EMAILI    TO        CIC0002I-EMAIL                     02730567
            MOVE BITYPEI   TO        CIC0002I-BILL-TYPE                 02730667
            MOVE BIADDRI   TO        CIC0002I-BILL-ADDR                 02730767
            MOVE BIDATEI   TO        CIC0002I-BILL-DATE                 02730867
            MOVE APDATEI   TO        CIC0002I-APP-DATE                  02730967
            MOVE LCTRYI    TO        CIC0002I-LIVE-COUNTRY              02731067
            MOVE LPROVI    TO        CIC0002I-LIVE-PROVINCE             02731167
            MOVE LCITYI    TO        CIC0002I-LIVE-CITY                 02731267
            MOVE LDISTI    TO        CIC0002I-LIVE-DISTRICT             02731367
            MOVE LZIPCI    TO        CIC0002I-LIVE-ZIP-CODE             02731467
            MOVE LADDRI    TO        CIC0002I-LIVE-ADDRESS              02731567
            MOVE LYEARSI   TO        CIC0002I-LIVE-YEARS                02731667
            MOVE CNAMEI    TO        CIC0002I-COMPANY-NAME              02731767
            MOVE CCTRYI    TO        CIC0002I-COMPANY-COUNTRY           02731867
            MOVE CPROVI    TO        CIC0002I-COMPANY-PROVINCE          02731967
            MOVE CCITYI    TO        CIC0002I-COMPANY-CITY              02732067
            MOVE CDISTI    TO        CIC0002I-COMPANY-DISTRICT          02732167
            MOVE CZIPCI    TO        CIC0002I-COMPANY-ZIP-CODE          02732267
            MOVE CADDRI    TO        CIC0002I-COMPANY-ADRESS            02732367
            MOVE CSRVYI    TO        CIC0002I-COMPANY-SERVE-YEAR        02732467
            .                                                           02732567
       3060-POPULATE-CICUS-EXIT.                                        02732667
            EXIT.                                                       02732767
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
            INITIALIZE WS-COMMAREA                                      02920104
            MOVE 'N' TO WS-FIRST-SEND                                   02921004
            MOVE LK-STEP TO WS-STEP                                     02922146
            MOVE LK-APPL-ID TO WS-APPL-ID                               02922246
            EXEC CICS RETURN TRANSID('CIA7')                            02930032
                      COMMAREA(WS-COMMAREA)                             02940004
            END-EXEC                                                    02950001
            .                                                           02960001
       5020-RETURN-TRANS-EXIT.                                          02970001
            EXIT.                                                       02980001
