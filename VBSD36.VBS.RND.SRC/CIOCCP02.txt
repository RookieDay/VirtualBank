       IDENTIFICATION DIVISION.                                         00010000
       PROGRAM-ID. CIOCCP02.                                            00020000
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
      *2015-02-03    KRIS      INITIAL VERSION                          00130000
      ***************************************************************** 00140000
       ENVIRONMENT DIVISION.                                            00150000
       DATA DIVISION.                                                   00160000
       WORKING-STORAGE SECTION.                                         00170000
      *                                                                 00180000
       77 WS-BEGIN              PIC X(17) VALUE 'CIOCCP02 WS BEGIN'.    00190000
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
       COPY CICA02.                                                     00350000
      *MAP CONTROL                                                      00360000
       COPY DFHBMSCA.                                                   00370000
      *CICS FUNCTION KEYS                                               00380000
       COPY DFHAID.                                                     00390000
      *CIMENU                                                           00400000
       COPY CIMENU.                                                     00410000
      *                                                                 00420000
       COPY CIC0003I.                                                   00430000
       COPY CIC0003O.                                                   00440000
      *                                                                 00450000
       COPY CIC0002I.                                                   00460000
       COPY CIC0002O.                                                   00470000
      *                                                                 00480000
       01 WS-SRV-COMMAREA.                                              00490000
      *SERVICE REQUEST/RESPONSE COMMAREA                                00500000
       COPY SD01WS.                                                     00510000
      *                                                                 00520000
       01 WS-COMMAREA.                                                  00530000
          05 WS-FIRST-SEND      PIC X(1).                               00540000
          05 WS-STEP            PIC 9(3).                               00550000
          05 WS-CUST-ID         PIC 9(18).                              00560007
       77 WS-END                PIC X(17) VALUE 'CIOCCP02 WS END'.      00570000
      *                                                                 00580000
       LINKAGE SECTION.                                                 00590000
       01 DFHCOMMAREA.                                                  00600000
          05 LK-FIRST-SEND      PIC X(1).                               00610000
          05 LK-STEP            PIC 9(3).                               00620000
          05 LK-CUST-ID         PIC 9(18).                              00630007
      *                                                                 00640000
       PROCEDURE DIVISION.                                              00650000
       0000-MAINLINE.                                                   00660000
      *                                                                 00670000
            PERFORM 1000-INIT                                           00680000
               THRU 1000-INIT-EXIT                                      00690000
      *                                                                 00700000
            PERFORM 2000-PRE-PROCESSING                                 00710000
               THRU 2000-PRE-PROCESSING-EXIT                            00720000
      *                                                                 00730000
            PERFORM 3000-MAIN-PROCESS                                   00740000
               THRU 3000-MAIN-PROCESS-EXIT                              00750000
      *                                                                 00760000
            PERFORM 4000-POST-PROCESSING                                00770000
               THRU 4000-POST-PROCESSING-EXIT                           00780000
      *                                                                 00790000
            PERFORM 5000-CLEAN-UP                                       00800000
               THRU 5000-CLEAN-UP-EXIT                                  00810000
            .                                                           00820000
      *                                                                 00830000
       0000-EXIT.                                                       00840000
            EXIT.                                                       00850000
      *                                                                 00860000
       1000-INIT.                                                       00870000
            IF LK-FIRST-SEND = 'Y'                                      00880000
               MOVE LOW-VALUES TO CICA02O                               00890000
               PERFORM 1020-GET-EXISTING-CUST                           00900000
                  THRU 1020-GET-EXISTING-CUST-EXIT                      00910000
               SET WS-MAP-ERASE TO TRUE                                 00920000
               PERFORM 3030-SEND-MAP                                    00930000
                  THRU 3030-SEND-MAP-EXIT                               00940000
      * NOT FIRST SHOW                                                  00950000
            ELSE                                                        00960000
                  MOVE LOW-VALUES TO CICA02I                            00970000
                  EXEC CICS RECEIVE MAP('CICA02')                       00980000
                                   MAPSET('CICA02')                     00990000
                                   INTO(CICA02I)                        01000000
                                   RESP(WS-RESP-CODE)                   01010000
                  END-EXEC                                              01020000
            END-IF                                                      01030000
            .                                                           01040000
       1000-INIT-EXIT.                                                  01050000
            EXIT.                                                       01060000
      *                                                                 01070000
       1010-ASK-TIME-DATE.                                              01080000
      *                                                                 01090000
            EXEC CICS                                                   01100000
                 ASKTIME                                                01110000
                 ABSTIME(WS-GETTIME)                                    01120000
            END-EXEC                                                    01130000
            EXEC CICS                                                   01140000
                 FORMATTIME                                             01150000
                 ABSTIME(WS-GETTIME)                                    01160000
                 DATESEP('/')                                           01170000
                 YYYYMMDD(WS-DATEOUT)                                   01180000
            END-EXEC                                                    01190000
            EXEC CICS                                                   01200000
                 FORMATTIME                                             01210000
                 ABSTIME(WS-GETTIME)                                    01220000
                 TIMESEP                                                01230000
                 TIME(WS-TIMEOUT)                                       01240000
            END-EXEC                                                    01250000
            MOVE WS-DATEOUT TO SYSDO                                    01260000
            MOVE WS-TIMEOUT TO SYSTO                                    01270000
            .                                                           01280000
      *                                                                 01290000
       1010-ASK-TIME-DATE-EXIT.                                         01300000
            EXIT.                                                       01310000
      *                                                                 01320000
       1020-GET-EXISTING-CUST.                                          01330000
            INITIALIZE SDCA-SERVICE-COMMAREA                            01340000
            MOVE 'VBS.CI.CUSTOMER.INQ' TO SD-SRV-NAME                   01350000
            INITIALIZE CIC0003I-REC                                     01360000
            MOVE LK-CUST-ID TO CIC0003I-ID                              01370008
            MOVE CIC0003I-REC TO SD-SRV-INPUT-DATA                      01380000
            EXEC CICS                                                   01390000
                 LINK                                                   01400000
                 PROGRAM(WS-PGM-SRV-DRIVER)                             01410000
                 COMMAREA(WS-SRV-COMMAREA)                              01420000
                 RESP(WS-RESP-CODE)                                     01430000
            END-EXEC                                                    01440000
            EVALUATE WS-RESP-CODE                                       01450000
                WHEN DFHRESP(NORMAL)                                    01460000
                     IF SD-RESP-CODE EQUAL ZEROS                        01470000
                        PERFORM 1030-POPULATE-CUST                      01480000
                           THRU 1030-POPULATE-CUST-EXIT                 01490000
                     END-IF                                             01500000
            END-EVALUATE                                                01510000
            .                                                           01520000
      *                                                                 01530000
       1020-GET-EXISTING-CUST-EXIT.                                     01540000
            EXIT.                                                       01550000
      *                                                                 01560000
       1030-POPULATE-CUST.                                              01570000
            INITIALIZE CIC0003O-REC                                     01580000
            MOVE SD-SRV-OUTPUT-DATA       TO CIC0003O-REC               01590000
            MOVE CIC0003O-NAME            TO NAMEO                      01600000
            MOVE CIC0003O-ENGLISH-NAME    TO ENAMEO                     01610000
            MOVE CIC0003O-NATIONALITY     TO NATIONO                    01620000
            MOVE CIC0003O-BIRTH-DATE      TO BIRTHO                     01630000
            MOVE CIC0003O-GENDER          TO GENDERO                    01640000
            MOVE CIC0003O-MARITAL-STATUS  TO MASTUSO                    01650000
            MOVE CIC0003O-ID-TYPE    TO IDTYPEO                         01660007
            MOVE CIC0003O-ID-NUMBER  TO IDNUMO                          01670007
            MOVE CIC0003O-ANNUAL-SALARY   TO ANSRYO                     01680000
            MOVE CIC0003O-MOBILE          TO MOBILEO                    01690000
            MOVE CIC0003O-EMAIL           TO EMAILO                     01700000
            MOVE CIC0003O-BILL-TYPE       TO BITYPEO                    01710000
            MOVE CIC0003O-BILL-ADDR       TO BIADDRO                    01720000
            MOVE CIC0003O-BILL-DATE       TO BIDATEO                    01730000
            MOVE CIC0003O-APP-DATE        TO APDATEO                    01740000
            MOVE CIC0003O-LIVE-COUNTRY    TO LCTRYO                     01750000
            MOVE CIC0003O-LIVE-PROVINCE   TO LPROVO                     01760000
            MOVE CIC0003O-LIVE-CITY       TO LCITYO                     01770000
            MOVE CIC0003O-LIVE-DISTRICT   TO LDISTO                     01780000
            MOVE CIC0003O-LIVE-ZIP-CODE   TO LZIPCO                     01790000
            MOVE CIC0003O-LIVE-ADDRESS    TO LADDRO                     01800000
            MOVE CIC0003O-LIVE-YEARS      TO LYEARSO                    01810000
            MOVE CIC0003O-COMPANY-NAME    TO CNAMEO                     01820000
            MOVE CIC0003O-COMPANY-COUNTRY TO CCTRYO                     01830000
            MOVE CIC0003O-COMPANY-PROVINCE TO CPROVO                    01840000
            MOVE CIC0003O-COMPANY-CITY    TO CCITYO                     01850000
            MOVE CIC0003O-COMPANY-DISTRICT TO CDISTO                    01860000
            MOVE CIC0003O-COMPANY-ZIP-CODE TO CZIPCO                    01870000
            MOVE CIC0003O-COMPANY-ADRESS  TO CADDRO                     01880000
            MOVE CIC0003O-COMPANY-SERVE-YEAR TO CSRVYO                  01890000
            .                                                           01900000
      *                                                                 01910000
       1030-POPULATE-CUST-EXIT.                                         01920000
            EXIT.                                                       01930000
       2000-PRE-PROCESSING.                                             01940000
      *                                                                 01950000
       2000-PRE-PROCESSING-EXIT.                                        01960000
            EXIT.                                                       01970000
      *                                                                 01980000
       3000-MAIN-PROCESS.                                               01990000
            EVALUATE EIBAID ALSO TRUE                                   02000000
                WHEN DFHPF1 ALSO ANY                                    02010000
                     EXEC CICS                                          02020000
                          XCTL PROGRAM('CIOCCP00')                      02030000
                               RESP(WS-RESP-CODE)                       02040000
                     END-EXEC                                           02050000
                     IF WS-RESP-CODE NOT = DFHRESP(NORMAL)              02060000
                        MOVE 'PROGRAM CIOCCIMN IS NOT AVAILABLE'        02070000
                                TO MSGO                                 02080000
                        SET WS-MAP-DATAONLY TO TRUE                     02090000
                        PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT   02100000
                     END-IF                                             02110000
                WHEN DFHPF3 ALSO ANY                                    02120000
                     MOVE 'THANK YOU FOR USING THE SYSTEM' TO WS-MESSAGE02130000
                     EXEC CICS                                          02140000
                          SEND CONTROL                                  02150000
                          CURSOR                                        02160000
                          ERASE                                         02170000
                          FREEKB                                        02180000
                          ALARM                                         02190000
                     END-EXEC                                           02200000
                     EXEC CICS                                          02210000
                          SEND FROM(WS-MESSAGE)                         02220000
                     END-EXEC                                           02230000
                     PERFORM 5010-RETURN THRU 5010-RETURN-EXIT          02240000
                WHEN DFHCLEAR ALSO ANY                                  02250000
                     EXEC CICS                                          02260000
                           SEND CONTROL                                 02270000
                           CURSOR                                       02280000
                           ERASE                                        02290000
                           FREEKB                                       02300000
                           ALARM                                        02310000
                     END-EXEC                                           02320000
                     PERFORM 5010-RETURN THRU 5010-RETURN-EXIT          02330000
                WHEN DFHPF6 ALSO ANY                                    02390010
                     INITIALIZE WS-COMMAREA                             02400000
                     MOVE 'Y' TO WS-FIRST-SEND                          02410000
                     MOVE LK-STEP TO WS-STEP                            02420000
                     MOVE LK-CUST-ID TO WS-CUST-ID                      02430009
                     EXEC CICS                                          02440000
                          XCTL PROGRAM('CIOCCP03')                      02450000
                               RESP(WS-RESP-CODE)                       02460000
                               COMMAREA(WS-COMMAREA)                    02470000
                     END-EXEC                                           02480000
                     IF WS-RESP-CODE NOT = DFHRESP(NORMAL)              02490000
                        MOVE 'PROGRAM CIOCCP03 IS NOT AVAILABLE'        02500000
                                TO MSGO                                 02510000
                        SET WS-MAP-DATAONLY TO TRUE                     02520000
                        PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT   02530000
                     END-IF                                             02540000
                 WHEN DFHENTER ALSO LK-STEP = 002                       02550012
                      IF WS-RESP-CODE NOT EQUAL DFHRESP(NORMAL)         02560000
                         MOVE 'INVALID REQUEST, CHECK YOUR INPUT.'      02570000
                              TO MSGO                                   02580000
                         SET WS-MAP-DATAONLY TO TRUE                    02590000
                         PERFORM 3030-SEND-MAP                          02600000
                            THRU 3030-SEND-MAP-EXIT                     02610000
                      ELSE                                              02620000
                         PERFORM 3010-CHECK-INPUT                       02630000
                            THRU 3010-CHECK-INPUT-EXIT                  02640000
                         PERFORM 3020-CUST-UPDATE                       02650000
                            THRU 3020-CUST-UPDATE-EXIT                  02660000
                      END-IF                                            02670000
                 WHEN OTHER                                             02680000
                      MOVE 'INVALID KEY PRESSED!' TO MSGO               02690000
                      SET WS-MAP-DATAONLY TO TRUE                       02700000
                      PERFORM 3030-SEND-MAP                             02710000
                         THRU 3030-SEND-MAP-EXIT                        02720000
            END-EVALUATE                                                02730000
            .                                                           02740000
       3000-MAIN-PROCESS-EXIT.                                          02750000
            EXIT.                                                       02760000
      *                                                                 02770000
       3010-CHECK-INPUT.                                                02780000
            IF COMMUL NOT = 0                                           02790000
               INITIALIZE CIMENU-REC                                    02800000
               MOVE COMMUI TO CIMENU-TRANSID                            02810000
               EXEC CICS READ                                           02820000
                    FILE('CIMENU')                                      02830000
                    INTO(CIMENU-REC)                                    02840000
                    RIDFLD(CIMENU-TRANSID)                              02850000
                    RESP(WS-RESP-CODE)                                  02860000
               END-EXEC                                                 02870000
               EVALUATE WS-RESP-CODE                                    02880000
                   WHEN DFHRESP(NORMAL)                                 02890000
                        EXEC CICS                                       02900000
                             XCTL PROGRAM(CIMENU-PGM)                   02910000
                             COMMAREA(WS-COMMAREA)                      02920000
                             RESP(WS-RESP-CODE)                         02930000
                        END-EXEC                                        02940000
                        IF WS-RESP-CODE NOT = DFHRESP(NORMAL)           02950000
                        STRING 'PROGRAM ' DELIMITED BY SIZE             02960000
                               CIMENU-PGM DELIMITED BY SPACE            02970000
                               ' IS NOT AVAILABLE' DELIMITED BY SIZE    02980000
                               INTO MSGO                                02990000
                           SET WS-MAP-DATAONLY TO TRUE                  03000000
                           PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT03010000
                        END-IF                                          03020000
                   WHEN DFHRESP(NOTFND)                                 03030000
                        MOVE 'INVALID TRANSATION ID!' TO MSGO           03040000
                        SET WS-MAP-DATAONLY TO TRUE                     03050000
                        PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT   03060000
                   WHEN OTHER                                           03070000
                        MOVE 'CIMENU FILE ERROR!' TO MSGO               03080000
                        SET WS-MAP-DATAONLY TO TRUE                     03090000
                        PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT   03100000
               END-EVALUATE                                             03110000
            END-IF                                                      03120000
            IF (EMAILL = 0 OR MOBILEL = 0 OR GENDERL = 0                03130000
                 OR NATIONL = 0 OR BIRTHL = 0 OR MASTUSL = 0            03140000
                 OR ANSRYL = 0 OR APDATEL = 0 OR BITYPEL = 0            03150000
                 OR BIDATEL = 0 OR BIADDRL = 0 OR BIRTHL = 0            03160000
                 OR LCTRYL = 0 OR LPROVL = 0 OR LCITYL = 0              03170000
                 OR LDISTL = 0 OR LZIPCL = 0 OR LYEARSL = 0             03180000
                 OR LADDRL = 0 OR CNAMEL = 0 OR CZIPCL = 0              03190000
                 OR CCTRYL = 0 OR CPROVL = 0 OR CCITYL = 0              03200000
                 OR CDISTL = 0 OR CADDRL = 0 OR CSRVYL = 0)             03210000
               MOVE 'FIELDS CAN NOT BE EMPTY' TO MSGO                   03220000
               SET WS-MAP-DATAONLY TO TRUE                              03230000
               PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT            03240000
            END-IF                                                      03250000
            IF (GENDERI NOT = 'M' AND GENDERI NOT = 'F')                03260000
               MOVE 'INVAILD GENDER' TO MSGO                            03270000
               SET WS-MAP-DATAONLY TO TRUE                              03280000
               PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT            03290000
            END-IF                                                      03300000
            IF (MASTUSI NOT = 'M' AND MASTUSI NOT = 'U')                03310000
               MOVE 'INVAILD MARITAL STATUS' TO MSGO                    03320000
               SET WS-MAP-DATAONLY TO TRUE                              03330000
               PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT            03340000
            END-IF                                                      03350000
            IF (BITYPEI NOT = 001 AND BITYPEI NOT = 002)                03360000
               MOVE 'INVAILD BILL TYPE' TO MSGO                         03370000
               SET WS-MAP-DATAONLY TO TRUE                              03380000
               PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT            03390000
            END-IF                                                      03400000
            IF (BIDATEI NOT = 05 AND BIDATEI NOT = 15 AND BIDATEI       03410000
                 NOT = 25)                                              03420000
               MOVE 'INVAILD BILL DATE' TO MSGO                         03430000
               SET WS-MAP-DATAONLY TO TRUE                              03440000
               PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT            03450000
            END-IF                                                      03460000
            IF (BIADDRI NOT = 001 AND BIADDRI NOT = 002 AND BIADDRI     03470000
                 NOT = 003)                                             03480000
               MOVE 'INVAILD BILL ADDRESS' TO MSGO                      03490000
               SET WS-MAP-DATAONLY TO TRUE                              03500000
               PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT            03510000
            END-IF                                                      03520000
            .                                                           03530000
      *                                                                 03540000
       3010-CHECK-INPUT-EXIT.                                           03550000
            EXIT.                                                       03560000
      *                                                                 03570000
       3020-CUST-UPDATE.                                                03580000
            INITIALIZE SDCA-SERVICE-COMMAREA                            03590000
            MOVE 'VBS.CI.CUSTOMER.UPD' TO SD-SRV-NAME                   03600000
            INITIALIZE CIC0002I-REC                                     03610000
      *     MOVE CIC0002O-REC TO CIC0002I-REC                           03620013
            PERFORM 3060-POPULATE-CICUS                                 03630000
               THRU 3060-POPULATE-CICUS-EXIT                            03640000
            MOVE CIC0002I-REC  TO SD-SRV-INPUT-DATA                     03650000
            EXEC CICS                                                   03660000
                 LINK                                                   03670000
                 PROGRAM(WS-PGM-SRV-DRIVER)                             03680000
                 COMMAREA(WS-SRV-COMMAREA)                              03690000
                 RESP(WS-RESP-CODE)                                     03700000
            END-EXEC                                                    03710000
            EVALUATE WS-RESP-CODE                                       03720000
                WHEN DFHRESP(NORMAL)                                    03730000
                     IF SD-RESP-CODE EQUAL ZEROS                        03740000
                        MOVE 'CUSTOMER UPDATE SUCCESSFULLY' TO MSGO     03750014
                     ELSE                                               03760000
                        MOVE SD-RESP-ADDITIONAL TO MSGO                 03770000
                     END-IF                                             03780000
                WHEN OTHER                                              03790000
                     MOVE 'SERVICE PROGRAM ERROR' TO MSGO               03800000
            END-EVALUATE                                                03810000
            SET WS-MAP-DATAONLY TO TRUE                                 03820000
            PERFORM 3030-SEND-MAP                                       03830000
               THRU 3030-SEND-MAP-EXIT                                  03840000
            .                                                           03850000
      *                                                                 03860000
       3020-CUST-UPDATE-EXIT.                                           03870000
            EXIT.                                                       03880000
      *                                                                 03890000
       3030-SEND-MAP.                                                   03900000
            IF LK-STEP = 001                                            03910000
               PERFORM 3040-SET-PROTECT                                 03920000
                  THRU 3040-SET-PROTECT-EXIT                            03930000
               MOVE 'PF1=RETURN PF3=EXIT PF6=CARD LIST' TO KEYO         03940010
      *        MOVE 'PRESS ANY KEY TO RETURN ' TO KEYO                  03950010
            ELSE                                                        03960000
               PERFORM 3050-SET-UNPROTECT                               03970000
                  THRU 3050-SET-UNPROTECT-EXIT                          03980000
               MOVE 'PF1=RETURN PF3=EXIT PF6=CARD LIST ENTER=UPDATE'    03990010
               TO KEYO                                                  03991010
            END-IF                                                      04000000
      *        IF WS-FIRST-SEND = 'T'                                   04010000
      *        MOVE 'PRESS ANY KEY TO RETURN ' TO KEYO                  04020000
      *        END-IF                                                   04030000
            PERFORM 1010-ASK-TIME-DATE                                  04040000
               THRU 1010-ASK-TIME-DATE-EXIT                             04050000
            EVALUATE TRUE                                               04060000
                WHEN WS-MAP-ERASE                                       04070000
                     EXEC CICS SEND                                     04080000
                          MAP('CICA02')                                 04090000
                          MAPSET('CICA02')                              04100000
                          FROM(CICA02O)                                 04110000
                          ERASE                                         04120000
                     END-EXEC                                           04130000
                WHEN WS-MAP-DATAONLY                                    04140000
                     EXEC CICS SEND                                     04150000
                          MAP('CICA02')                                 04160000
                          MAPSET('CICA02')                              04170000
                          FROM(CICA02O)                                 04180000
                          DATAONLY                                      04190000
                     END-EXEC                                           04200000
            END-EVALUATE                                                04210000
      *     MOVE '1' TO WS-ENTER-FLAG                                   04220000
            PERFORM 5020-RETURN-TRANS THRU 5020-RETURN-TRANS-EXIT       04230000
            .                                                           04240000
      *                                                                 04250000
       3030-SEND-MAP-EXIT.                                              04260000
            EXIT.                                                       04270000
      *                                                                 04280000
       3040-SET-PROTECT.                                                04290000
            MOVE ATTR-PROT-MDT          TO NAMEA                        04300000
            MOVE ATTR-PROT-MDT          TO ENAMEA                       04310000
            MOVE ATTR-PROT-MDT          TO NATIONA                      04320000
            MOVE ATTR-PROT-MDT          TO BIRTHA                       04330000
            MOVE ATTR-PROT-MDT          TO GENDERA                      04340000
            MOVE ATTR-PROT-MDT          TO MASTUSA                      04350000
            MOVE ATTR-PROT-MDT          TO IDTYPEA                      04360000
            MOVE ATTR-PROT-MDT          TO IDNUMA                       04370000
            MOVE ATTR-PROT-MDT          TO ANSRYA                       04380000
            MOVE ATTR-PROT-MDT          TO MOBILEA                      04390000
            MOVE ATTR-PROT-MDT          TO EMAILA                       04400000
            MOVE ATTR-PROT-MDT          TO BITYPEA                      04410000
            MOVE ATTR-PROT-MDT          TO BIADDRA                      04420000
            MOVE ATTR-PROT-MDT          TO BIDATEA                      04430000
            MOVE ATTR-PROT-MDT          TO APDATEA                      04440000
            MOVE ATTR-PROT-MDT          TO LCTRYA                       04450000
            MOVE ATTR-PROT-MDT          TO LPROVA                       04460000
            MOVE ATTR-PROT-MDT          TO LCITYA                       04470000
            MOVE ATTR-PROT-MDT          TO LDISTA                       04480000
            MOVE ATTR-PROT-MDT          TO LZIPCA                       04490000
            MOVE ATTR-PROT-MDT          TO LADDRA                       04500000
            MOVE ATTR-PROT-MDT          TO LYEARSA                      04510000
            MOVE ATTR-PROT-MDT          TO CNAMEA                       04520000
            MOVE ATTR-PROT-MDT          TO CCTRYA                       04530000
            MOVE ATTR-PROT-MDT           TO CPROVA                      04540000
            MOVE ATTR-PROT-MDT          TO CCITYA                       04550000
            MOVE ATTR-PROT-MDT           TO CDISTA                      04560000
            MOVE ATTR-PROT-MDT           TO CZIPCA                      04570000
            MOVE ATTR-PROT-MDT          TO CADDRA                       04580000
            MOVE ATTR-PROT-MDT           TO CSRVYA                      04590000
            .                                                           04600000
       3040-SET-PROTECT-EXIT.                                           04610000
            EXIT.                                                       04620000
       3050-SET-UNPROTECT.                                              04630000
            MOVE ATTR-UNPROT-MDT          TO NAMEA                      04631013
            MOVE ATTR-UNPROT-MDT          TO ENAMEA                     04640000
            MOVE ATTR-UNPROT-MDT          TO NATIONA                    04650000
            MOVE ATTR-UNPROT-MDT          TO BIRTHA                     04660000
            MOVE ATTR-UNPROT-MDT          TO GENDERA                    04670000
            MOVE ATTR-UNPROT-MDT          TO MASTUSA                    04680000
            MOVE ATTR-PROT-MDT            TO IDTYPEA                    04681013
            MOVE ATTR-PROT-MDT            TO IDNUMA                     04682013
            MOVE ATTR-UNPROT-MDT          TO ANSRYA                     04690000
            MOVE ATTR-UNPROT-MDT          TO MOBILEA                    04700000
            MOVE ATTR-UNPROT-MDT          TO EMAILA                     04710000
            MOVE ATTR-UNPROT-MDT          TO BITYPEA                    04720000
            MOVE ATTR-UNPROT-MDT          TO BIADDRA                    04730000
            MOVE ATTR-UNPROT-MDT          TO BIDATEA                    04740000
            MOVE ATTR-UNPROT-MDT          TO APDATEA                    04750000
            MOVE ATTR-UNPROT-MDT          TO LCTRYA                     04760000
            MOVE ATTR-UNPROT-MDT          TO LPROVA                     04770000
            MOVE ATTR-UNPROT-MDT          TO LCITYA                     04780000
            MOVE ATTR-UNPROT-MDT          TO LDISTA                     04790000
            MOVE ATTR-UNPROT-MDT          TO LZIPCA                     04800000
            MOVE ATTR-UNPROT-MDT          TO LADDRA                     04810000
            MOVE ATTR-UNPROT-MDT          TO LYEARSA                    04820000
            MOVE ATTR-UNPROT-MDT          TO CNAMEA                     04830000
            MOVE ATTR-UNPROT-MDT          TO CCTRYA                     04840000
            MOVE ATTR-UNPROT-MDT          TO CPROVA                     04850000
            MOVE ATTR-UNPROT-MDT          TO CCITYA                     04860000
            MOVE ATTR-UNPROT-MDT          TO CDISTA                     04870000
            MOVE ATTR-UNPROT-MDT          TO CZIPCA                     04880000
            MOVE ATTR-UNPROT-MDT          TO CADDRA                     04890000
            MOVE ATTR-UNPROT-MDT          TO CSRVYA                     04900000
            .                                                           04910000
       3050-SET-UNPROTECT-EXIT.                                         04920000
            EXIT.                                                       04930000
       3060-POPULATE-CICUS.                                             04940000
            MOVE NAMEI     TO        CIC0002I-NAME                      04950000
            MOVE ENAMEI    TO        CIC0002I-ENGLISH-NAME              04960000
            MOVE NATIONI   TO        CIC0002I-NATIONALITY               04970000
            MOVE BIRTHI    TO        CIC0002I-BIRTH-DATE                04980000
            MOVE GENDERI   TO        CIC0002I-GENDER                    04990000
            MOVE MASTUSI   TO        CIC0002I-MARITAL-STATUS            05000000
            MOVE IDTYPEI   TO        CIC0002I-ID-TYPE                   05010000
            MOVE IDNUMI    TO        CIC0002I-ID-NUMBER                 05020000
            MOVE ANSRYI    TO        CIC0002I-ANNUAL-SALARY             05030000
            MOVE MOBILEI   TO        CIC0002I-MOBILE                    05040000
            MOVE EMAILI    TO        CIC0002I-EMAIL                     05050000
            MOVE BITYPEI   TO        CIC0002I-BILL-TYPE                 05060000
            MOVE BIADDRI   TO        CIC0002I-BILL-ADDR                 05070000
            MOVE BIDATEI   TO        CIC0002I-BILL-DATE                 05080000
            MOVE APDATEI   TO        CIC0002I-APP-DATE                  05090000
            MOVE LCTRYI    TO        CIC0002I-LIVE-COUNTRY              05100000
            MOVE LPROVI    TO        CIC0002I-LIVE-PROVINCE             05110000
            MOVE LCITYI    TO        CIC0002I-LIVE-CITY                 05120000
            MOVE LDISTI    TO        CIC0002I-LIVE-DISTRICT             05130000
            MOVE LZIPCI    TO        CIC0002I-LIVE-ZIP-CODE             05140000
            MOVE LADDRI    TO        CIC0002I-LIVE-ADDRESS              05150000
            MOVE LYEARSI   TO        CIC0002I-LIVE-YEARS                05160000
            MOVE CNAMEI    TO        CIC0002I-COMPANY-NAME              05170000
            MOVE CCTRYI    TO        CIC0002I-COMPANY-COUNTRY           05180000
            MOVE CPROVI    TO        CIC0002I-COMPANY-PROVINCE          05190000
            MOVE CCITYI    TO        CIC0002I-COMPANY-CITY              05200000
            MOVE CDISTI    TO        CIC0002I-COMPANY-DISTRICT          05210000
            MOVE CZIPCI    TO        CIC0002I-COMPANY-ZIP-CODE          05220000
            MOVE CADDRI    TO        CIC0002I-COMPANY-ADRESS            05230000
            MOVE CSRVYI    TO        CIC0002I-COMPANY-SERVE-YEAR        05240000
            .                                                           05250000
       3060-POPULATE-CICUS-EXIT.                                        05260000
            EXIT.                                                       05270000
       4000-POST-PROCESSING.                                            05280000
      *                                                                 05290000
       4000-POST-PROCESSING-EXIT.                                       05300000
            EXIT.                                                       05310000
      *                                                                 05320000
       5000-CLEAN-UP.                                                   05330000
            PERFORM 5010-RETURN                                         05340000
               THRU 5010-RETURN-EXIT                                    05350000
            .                                                           05360000
      *                                                                 05370000
       5000-CLEAN-UP-EXIT.                                              05380000
            EXIT.                                                       05390000
      *                                                                 05400000
       5010-RETURN.                                                     05410000
            EXEC CICS RETURN END-EXEC                                   05420000
            .                                                           05430000
       5010-RETURN-EXIT.                                                05440000
            EXIT.                                                       05450000
      *                                                                 05460000
       5020-RETURN-TRANS.                                               05470000
            INITIALIZE WS-COMMAREA                                      05480000
            MOVE 'N' TO WS-FIRST-SEND                                   05490000
            MOVE LK-STEP TO WS-STEP                                     05500000
            MOVE LK-CUST-ID TO WS-CUST-ID                               05510007
            EXEC CICS RETURN TRANSID('CIC2')                            05520000
                      COMMAREA(WS-COMMAREA)                             05530000
            END-EXEC                                                    05540000
            .                                                           05550000
       5020-RETURN-TRANS-EXIT.                                          05560000
            EXIT.                                                       05570000
