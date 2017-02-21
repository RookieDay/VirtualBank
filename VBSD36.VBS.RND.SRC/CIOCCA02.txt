       IDENTIFICATION DIVISION.                                         00010001
       PROGRAM-ID. CIOCCA02.                                            00020001
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
       77 WS-BEGIN              PIC X(17) VALUE 'CIOCCA02 WS BEGIN'.    00190001
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
       COPY CICA02.                                                     00350002
      *MAP CONTROL                                                      00360001
       COPY DFHBMSCA.                                                   00370001
      *CICS FUNCTION KEYS                                               00380001
       COPY DFHAID.                                                     00390001
      *CIMENU                                                           00400001
       COPY CIMENU.                                                     00410001
      *                                                                 00411032
       COPY CIC0003I.                                                   00412032
       COPY CIC0003O.                                                   00413032
      *                                                                 00414039
       COPY CIC0011I.                                                   00415039
       COPY CIC0011O.                                                   00416039
      *                                                                 00420001
       01 WS-SRV-COMMAREA.                                              00430001
      *SERVICE REQUEST/RESPONSE COMMAREA                                00440001
       COPY SD01WS.                                                     00450001
      *                                                                 00460001
       01 WS-COMMAREA.                                                  00470004
          05 WS-FIRST-SEND      PIC X(1).                               00480004
          05 WS-ID-NUMBER       PIC 9(18).                              00490004
          05 WS-ID-TYPE         PIC 9(3).                               00500004
          05 WS-CUSTOMER-NAME   PIC X(30).                              00520004
       01 WS-COMMAREA-CA01      PIC X(4).                               00520129
       77 WS-END                PIC X(17) VALUE 'CIOCCA02 WS END'.      00521004
      *                                                                 00530001
       LINKAGE SECTION.                                                 00540001
       01 DFHCOMMAREA.                                                  00550001
          05 LK-FIRST-SEND      PIC X(1).                               00560001
          05 LK-ID-NUMBER       PIC 9(18).                              00570001
          05 LK-ID-TYPE         PIC 9(3).                               00571001
          05 LK-CUSTOMER-NAME   PIC X(30).                              00572004
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
               MOVE LOW-VALUES TO CICA02O                               00820136
               PERFORM 1020-GET-EXISTING-CUST                           00821031
                  THRU 1020-GET-EXISTING-CUST-EXIT                      00822031
               SET WS-MAP-ERASE TO TRUE                                 00840001
               PERFORM 3030-SEND-MAP                                    00850001
                  THRU 3030-SEND-MAP-EXIT                               00860001
      * NOT FIRST SHOW                                                  00870001
            ELSE                                                        00880001
               MOVE LOW-VALUES TO CICA02I                               00890039
               EXEC CICS RECEIVE MAP('CICA02')                          00900039
                                 MAPSET('CICA02')                       00910039
                                 INTO(CICA02I)                          00920039
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
       1020-GET-EXISTING-CUST.                                          01232031
            INITIALIZE SDCA-SERVICE-COMMAREA                            01232132
            MOVE 'VBS.CI.CUSTOMER.INQ' TO SD-SRV-NAME                   01232232
            INITIALIZE CIC0003I-REC                                     01232332
            MOVE LK-ID-NUMBER TO CIC0003I-ID                            01232434
            MOVE CIC0003I-REC TO SD-SRV-INPUT-DATA                      01232545
            EXEC CICS                                                   01232632
                 LINK                                                   01232732
                 PROGRAM(WS-PGM-SRV-DRIVER)                             01232832
                 COMMAREA(WS-SRV-COMMAREA)                              01232932
                 RESP(WS-RESP-CODE)                                     01233032
            END-EXEC                                                    01233132
            EVALUATE WS-RESP-CODE                                       01233232
                WHEN DFHRESP(NORMAL)                                    01233332
                     IF SD-RESP-CODE EQUAL ZEROS                        01233432
                        PERFORM 1030-POPULATE-CUST                      01233533
                           THRU 1030-POPULATE-CUST-EXIT                 01233633
                     END-IF                                             01234032
            END-EVALUATE                                                01234332
            .                                                           01234738
      *                                                                 01234831
       1020-GET-EXISTING-CUST-EXIT.                                     01234931
            EXIT.                                                       01235031
      *                                                                 01240001
       1030-POPULATE-CUST.                                              01240133
            INITIALIZE CIC0003O-REC                                     01240333
            MOVE SD-SRV-OUTPUT-DATA       TO CIC0003O-REC               01240444
      *     MOVE CIC0003O-NAME            TO NAMEO                      01241133
            MOVE CIC0003O-ENGLISH-NAME    TO ENAMEO                     01241233
            MOVE CIC0003O-NATIONALITY     TO NATIONO                    01241333
            MOVE CIC0003O-BIRTH-DATE      TO BIRTHO                     01241433
            MOVE CIC0003O-GENDER          TO GENDERO                    01241533
            MOVE CIC0003O-MARITAL-STATUS  TO MASTUSO                    01241633
      *     MOVE CIC0003O-ID-TYPE         TO IDTYPEO                    01241733
      *     MOVE CIC0003O-ID-NUMBER       TO IDNUMO                     01241833
            MOVE CIC0003O-ANNUAL-SALARY   TO ANSRYO                     01241933
            MOVE CIC0003O-MOBILE          TO MOBILEO                    01242033
            MOVE CIC0003O-EMAIL           TO EMAILO                     01242133
            MOVE CIC0003O-BILL-TYPE       TO BITYPEO                    01242233
            MOVE CIC0003O-BILL-ADDR       TO BIADDRO                    01242333
            MOVE CIC0003O-BILL-DATE       TO BIDATEO                    01242433
            MOVE CIC0003O-APP-DATE        TO APDATEO                    01242533
            MOVE CIC0003O-LIVE-COUNTRY    TO LCTRYO                     01242633
            MOVE CIC0003O-LIVE-PROVINCE   TO LPROVO                     01242733
            MOVE CIC0003O-LIVE-CITY       TO LCITYO                     01242833
            MOVE CIC0003O-LIVE-DISTRICT   TO LDISTO                     01242933
            MOVE CIC0003O-LIVE-ZIP-CODE   TO LZIPCO                     01243033
            MOVE CIC0003O-LIVE-ADDRESS    TO LADDRO                     01243133
            MOVE CIC0003O-LIVE-YEARS      TO LYEARSO                    01243233
            MOVE CIC0003O-COMPANY-NAME    TO CNAMEO                     01243333
            MOVE CIC0003O-COMPANY-COUNTRY TO CCTRYO                     01243433
            MOVE CIC0003O-COMPANY-PROVINCE TO CPROVO                    01243533
            MOVE CIC0003O-COMPANY-CITY    TO CCITYO                     01243633
            MOVE CIC0003O-COMPANY-DISTRICT TO CDISTO                    01243733
            MOVE CIC0003O-COMPANY-ZIP-CODE TO CZIPCO                    01243833
            MOVE CIC0003O-COMPANY-ADRESS  TO CADDRO                     01243933
            MOVE CIC0003O-COMPANY-SERVE-YEAR TO CSRVYO                  01244033
            MOVE ATTR-UNPROT-MDT          TO ENAMEA                     01244243
            MOVE ATTR-UNPROT-MDT          TO NATIONA                    01244343
            MOVE ATTR-UNPROT-MDT          TO BIRTHA                     01244443
            MOVE ATTR-UNPROT-MDT          TO GENDERA                    01244543
            MOVE ATTR-UNPROT-MDT          TO MASTUSA                    01244643
            MOVE ATTR-UNPROT-MDT          TO ANSRYA                     01245043
            MOVE ATTR-UNPROT-MDT          TO MOBILEA                    01245143
            MOVE ATTR-UNPROT-MDT          TO EMAILA                     01245243
            MOVE ATTR-UNPROT-MDT          TO BITYPEA                    01245343
            MOVE ATTR-UNPROT-MDT          TO BIADDRA                    01245443
            MOVE ATTR-UNPROT-MDT          TO BIDATEA                    01245543
            MOVE ATTR-UNPROT-MDT          TO APDATEA                    01245643
            MOVE ATTR-UNPROT-MDT          TO LCTRYA                     01245743
            MOVE ATTR-UNPROT-MDT          TO LPROVA                     01245843
            MOVE ATTR-UNPROT-MDT          TO LCITYA                     01245943
            MOVE ATTR-UNPROT-MDT          TO LDISTA                     01246043
            MOVE ATTR-UNPROT-MDT          TO LZIPCA                     01246143
            MOVE ATTR-UNPROT-MDT          TO LADDRA                     01246243
            MOVE ATTR-UNPROT-MDT          TO LYEARSA                    01246343
            MOVE ATTR-UNPROT-MDT          TO CNAMEA                     01246443
            MOVE ATTR-UNPROT-MDT          TO CCTRYA                     01246543
            MOVE ATTR-UNPROT-MDT          TO CPROVA                     01246643
            MOVE ATTR-UNPROT-MDT          TO CCITYA                     01246743
            MOVE ATTR-UNPROT-MDT          TO CDISTA                     01246843
            MOVE ATTR-UNPROT-MDT          TO CZIPCA                     01246943
            MOVE ATTR-UNPROT-MDT          TO CADDRA                     01247043
            MOVE ATTR-UNPROT-MDT          TO CSRVYA                     01247143
            .                                                           01247242
      *                                                                 01247342
       1030-POPULATE-CUST-EXIT.                                         01247442
            EXIT.                                                       01247542
      *                                                                 01248042
       2000-PRE-PROCESSING.                                             01250001
      *                                                                 01260001
       2000-PRE-PROCESSING-EXIT.                                        01270001
            EXIT.                                                       01280001
      *                                                                 01290001
       3000-MAIN-PROCESS.                                               01300001
            EVALUATE EIBAID                                             01310001
                WHEN DFHPF1                                             01311024
                     MOVE 'Y000' TO WS-COMMAREA-CA01                    01311130
                     EXEC CICS                                          01312024
                          XCTL PROGRAM('CIOCCA01')                      01313024
                               COMMAREA(WS-COMMAREA-CA01)               01313129
                               RESP(WS-RESP-CODE)                       01314024
                     END-EXEC                                           01315024
                     IF WS-RESP-CODE NOT = DFHRESP(NORMAL)              01316024
                        MOVE 'PROGRAM CIOCCA01 IS NOT AVAILABLE'        01317027
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
                WHEN DFHPF9                                             01540001
                     MOVE LOW-VALUES TO CICA02O                         01550002
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
                        PERFORM 3020-ADD-CUSTAPPL                       01690041
                           THRU 3020-ADD-CUSTAPPL-EXIT                  01700041
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
                             COMMAREA(CIMENU-TRANSID)                   01915048
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
       3020-ADD-CUSTAPPL.                                               01970037
            INITIALIZE SDCA-SERVICE-COMMAREA                            01980037
            MOVE 'VBS.CI.CUSTAPPL.ADD' TO SD-SRV-NAME                   01990039
            INITIALIZE CIC0011I-REC                                     02000039
            PERFORM 3040-POPULATE-CICUS                                 02010038
               THRU 3040-POPULATE-CICUS-EXIT                            02011038
            MOVE CIC0011I-REC  TO SD-SRV-INPUT-DATA                     02020039
            EXEC CICS                                                   02030037
                 LINK                                                   02040037
                 PROGRAM(WS-PGM-SRV-DRIVER)                             02050037
                 COMMAREA(WS-SRV-COMMAREA)                              02060037
                 RESP(WS-RESP-CODE)                                     02070037
            END-EXEC                                                    02080037
            EVALUATE WS-RESP-CODE                                       02090037
                WHEN DFHRESP(NORMAL)                                    02100037
                     IF SD-RESP-CODE EQUAL ZEROS                        02101039
                        MOVE 'APPLICATION CREATED SUCCESSFULLY' TO MSGO 02131139
                        MOVE 'T' TO WS-FIRST-SEND                       02131246
                        MOVE 'F1=RETURN' TO KEYO                        02131346
                     ELSE                                               02131439
                        MOVE SD-RESP-ADDITIONAL TO MSGO                 02131539
                     END-IF                                             02140039
                WHEN OTHER                                              02141038
                     MOVE 'SERVICE PROGRAM ERROR' TO MSGO               02142038
            END-EVALUATE                                                02150037
            SET WS-MAP-DATAONLY TO TRUE                                 02160039
            PERFORM 3030-SEND-MAP                                       02170039
               THRU 3030-SEND-MAP-EXIT                                  02180039
            .                                                           02420001
      *                                                                 02430001
       3020-ADD-CUSTAPPL-EXIT.                                          02440037
            EXIT.                                                       02450001
      *                                                                 02460001
       3030-SEND-MAP.                                                   02470001
            MOVE LK-ID-NUMBER             TO IDNUMO                     02471045
            MOVE LK-ID-TYPE               TO IDTYPEO                    02472045
            MOVE LK-CUSTOMER-NAME         TO NAMEO                      02473045
            MOVE ATTR-PROT-SKIP-MDT       TO NAMEA                      02474046
            MOVE ATTR-PROT-SKIP-MDT       TO IDTYPEA                    02475045
            MOVE ATTR-PROT-SKIP-MDT       TO IDNUMA                     02476045
            PERFORM 1010-ASK-TIME-DATE                                  02480001
               THRU 1010-ASK-TIME-DATE-EXIT                             02490001
            EVALUATE TRUE                                               02500001
                WHEN WS-MAP-ERASE                                       02510001
                     EXEC CICS SEND                                     02520001
                          MAP('CICA02')                                 02530002
                          MAPSET('CICA02')                              02540002
                          FROM(CICA02O)                                 02550002
                          ERASE                                         02560001
                     END-EXEC                                           02570001
                WHEN WS-MAP-DATAONLY                                    02580001
                     EXEC CICS SEND                                     02590001
                          MAP('CICA02')                                 02600002
                          MAPSET('CICA02')                              02610002
                          FROM(CICA02O)                                 02620002
                          DATAONLY                                      02630001
                     END-EXEC                                           02640001
            END-EVALUATE                                                02650001
            PERFORM 5020-RETURN-TRANS THRU 5020-RETURN-TRANS-EXIT       02670001
            .                                                           02680001
      *                                                                 02690001
       3030-SEND-MAP-EXIT.                                              02700001
            EXIT.                                                       02710001
      *                                                                 02710137
       3040-POPULATE-CICUS.                                             02711037
            MOVE NAMEI     TO        CIC0011I-NAME                      02714039
            MOVE ENAMEI    TO        CIC0011I-ENGLISH-NAME              02715039
            MOVE NATIONI   TO        CIC0011I-NATIONALITY               02716039
            MOVE BIRTHI    TO        CIC0011I-BIRTH-DATE                02717039
            MOVE GENDERI   TO        CIC0011I-GENDER                    02718039
            MOVE MASTUSI   TO        CIC0011I-MARITAL-STATUS            02719039
            MOVE IDTYPEI   TO        CIC0011I-ID-TYPE                   02719139
            MOVE IDNUMI    TO        CIC0011I-ID-NUMBER                 02719239
            MOVE ANSRYI    TO        CIC0011I-ANNUAL-SALARY             02719339
            MOVE MOBILEI   TO        CIC0011I-MOBILE                    02719439
            MOVE EMAILI    TO        CIC0011I-EMAIL                     02719539
            MOVE BITYPEI   TO        CIC0011I-BILL-TYPE                 02719639
            MOVE BIADDRI   TO        CIC0011I-BILL-ADDR                 02719739
            MOVE BIDATEI   TO        CIC0011I-BILL-DATE                 02719839
            MOVE APDATEI   TO        CIC0011I-APP-DATE                  02719939
            MOVE LCTRYI    TO        CIC0011I-LIVE-COUNTRY              02720039
            MOVE LPROVI    TO        CIC0011I-LIVE-PROVINCE             02720139
            MOVE LCITYI    TO        CIC0011I-LIVE-CITY                 02720239
            MOVE LDISTI    TO        CIC0011I-LIVE-DISTRICT             02720339
            MOVE LZIPCI    TO        CIC0011I-LIVE-ZIP-CODE             02720440
            MOVE LADDRI    TO        CIC0011I-LIVE-ADDRESS              02720539
            MOVE LYEARSI   TO        CIC0011I-LIVE-YEARS                02720639
            MOVE CNAMEI    TO        CIC0011I-COMPANY-NAME              02720739
            MOVE CCTRYI    TO        CIC0011I-COMPANY-COUNTRY           02720839
            MOVE CPROVI    TO        CIC0011I-COMPANY-PROVINCE          02720939
            MOVE CCITYI    TO        CIC0011I-COMPANY-CITY              02721039
            MOVE CDISTI    TO        CIC0011I-COMPANY-DISTRICT          02721139
            MOVE CZIPCI    TO        CIC0011I-COMPANY-ZIP-CODE          02721239
            MOVE CADDRI    TO        CIC0011I-COMPANY-ADRESS            02721339
            MOVE CSRVYI    TO        CIC0011I-COMPANY-SERVE-YEAR        02721439
            .                                                           02721537
      *                                                                 02721637
       3040-POPULATE-CICUS-EXIT.                                        02721737
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
            MOVE LK-ID-NUMBER TO WS-ID-NUMBER                           02923004
            MOVE LK-ID-TYPE TO WS-ID-TYPE                               02924004
            MOVE LK-CUSTOMER-NAME TO WS-CUSTOMER-NAME                   02925004
            EXEC CICS RETURN TRANSID('CIA2')                            02930012
                      COMMAREA(WS-COMMAREA)                             02940004
            END-EXEC                                                    02950001
            .                                                           02960001
       5020-RETURN-TRANS-EXIT.                                          02970001
            EXIT.                                                       02980001
