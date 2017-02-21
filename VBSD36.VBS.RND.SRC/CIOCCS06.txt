       IDENTIFICATION DIVISION.                                         00010000
       PROGRAM-ID. CIOCCS06.                                            00020000
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
       77 WS-BEGIN              PIC X(17) VALUE 'CIOCCS06 WS BEGIN'.    00190000
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
       01 WS-COMMAREA.                                                  00521002
          05 WS-FIRST-SEND      PIC X(1).                               00522002
          05 WS-STEP            PIC 9(3).                               00523002
          05 WS-NUMB            PIC 9(16).                              00524002
          05 WS-CUST-ID         PIC 9(18).                              00525002
      *                                                                 00526002
       01 WS-COMMAREA-CS05.                                             00530002
          05 WS-FIRST-SEND      PIC X(1).                               00540000
          05 WS-STEP            PIC 9(3).                               00550000
          05 WS-NUMB            PIC 9(16).                              00551009
          05 WS-CUST-ID         PIC 9(18).                              00560000
      *                                                                 00560118
       01 WS-COMMAREA-CS07.                                             00561018
          05 WS-FIRST-SEND      PIC X(1).                               00562018
          05 WS-STEP            PIC 9(3).                               00563018
          05 WS-NUMB            PIC 9(16).                              00564018
          05 WS-CUST-ID         PIC 9(18).                              00565018
       77 WS-END                PIC X(17) VALUE 'CIOCCS06 WS END'.      00570000
      *                                                                 00580000
       LINKAGE SECTION.                                                 00590000
       01 DFHCOMMAREA.                                                  00600000
          05 LK-FIRST-SEND      PIC X(1).                               00610000
          05 LK-STEP            PIC 9(3).                               00620000
          05 LK-NUMB            PIC 9(16).                              00630002
          05 LK-CUST-ID         PIC 9(18).                              00631003
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
            MOVE LK-CUST-ID TO CIC0003I-ID                              01370000
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
            MOVE CIC0003O-ID-TYPE    TO IDTYPEO                         01660000
            MOVE CIC0003O-ID-NUMBER  TO IDNUMO                          01670000
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
                MOVE 'Y' TO WS-FIRST-SEND OF WS-COMMAREA-CS05           02011007
                MOVE LK-STEP TO WS-STEP OF WS-COMMAREA-CS05             02012007
                MOVE LK-NUMB TO WS-NUMB OF WS-COMMAREA-CS05             02012108
                MOVE LK-CUST-ID TO WS-CUST-ID OF WS-COMMAREA-CS05       02013007
                     EXEC CICS                                          02020000
                          XCTL PROGRAM('CIOCCS05')                      02030000
                           COMMAREA(WS-COMMAREA-CS05)                   02031007
                               RESP(WS-RESP-CODE)                       02040000
                     END-EXEC                                           02050000
                     IF WS-RESP-CODE NOT = DFHRESP(NORMAL)              02060000
                        MOVE 'PROGRAM CIOCCI05 IS NOT AVAILABLE'        02070018
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
                WHEN DFHENTER ALSO ANY                                  02340020
                     INITIALIZE WS-COMMAREA                             02350000
                     MOVE 'Y' TO WS-FIRST-SEND OF WS-COMMAREA-CS07      02360019
                     MOVE LK-STEP TO WS-STEP OF WS-COMMAREA-CS07        02370019
                     MOVE LK-NUMB TO WS-NUMB OF WS-COMMAREA-CS07        02371019
                     MOVE LK-CUST-ID TO WS-CUST-ID OF                   02380019
                          WS-COMMAREA-CS07                              02381019
                     EXEC CICS                                          02390000
                          XCTL PROGRAM('CIOCCS07')                      02400001
                               RESP(WS-RESP-CODE)                       02410000
                               COMMAREA(WS-COMMAREA-CS07)               02420021
                     END-EXEC                                           02430000
                     IF WS-RESP-CODE NOT = DFHRESP(NORMAL)              02440000
                        MOVE 'PROGRAM CIOCCS07 IS NOT AVAILABLE'        02450001
                                TO MSGO                                 02460000
                        SET WS-MAP-DATAONLY TO TRUE                     02470000
                        PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT   02480000
                     END-IF                                             02490000
      *          WHEN DFHENTER ALSO LK-STEP = 002                       02500019
      *               IF WS-RESP-CODE NOT EQUAL DFHRESP(NORMAL)         02510019
      *                  MOVE 'INVALID REQUEST, CHECK YOUR INPUT.'      02520019
      *                       TO MSGO                                   02530019
      *                  SET WS-MAP-DATAONLY TO TRUE                    02540019
      *                  PERFORM 3030-SEND-MAP                          02550019
      *                     THRU 3030-SEND-MAP-EXIT                     02560019
      *               ELSE                                              02570019
      *                  PERFORM 3010-CHECK-INPUT                       02580019
      *                     THRU 3010-CHECK-INPUT-EXIT                  02590019
      *                  PERFORM 3020-CUST-UPDATE                       02600001
      *                     THRU 3020-CUST-UPDATE-EXIT                  02610001
      *               END-IF                                            02620020
                 WHEN OTHER                                             02630000
                      MOVE 'INVALID KEY PRESSED!' TO MSGO               02640000
                      SET WS-MAP-DATAONLY TO TRUE                       02650000
                      PERFORM 3030-SEND-MAP                             02660000
                         THRU 3030-SEND-MAP-EXIT                        02670000
            END-EVALUATE                                                02680000
            .                                                           02690000
       3000-MAIN-PROCESS-EXIT.                                          02700000
            EXIT.                                                       02710000
      *                                                                 02720000
       3010-CHECK-INPUT.                                                02730000
            IF COMMUL NOT = 0                                           02740000
               INITIALIZE CIMENU-REC                                    02750000
               MOVE COMMUI TO CIMENU-TRANSID                            02760000
               EXEC CICS READ                                           02770000
                    FILE('CIMENU')                                      02780000
                    INTO(CIMENU-REC)                                    02790000
                    RIDFLD(CIMENU-TRANSID)                              02800000
                    RESP(WS-RESP-CODE)                                  02810000
               END-EXEC                                                 02820000
               EVALUATE WS-RESP-CODE                                    02830000
                   WHEN DFHRESP(NORMAL)                                 02840000
                        EXEC CICS                                       02850000
                             XCTL PROGRAM(CIMENU-PGM)                   02860000
                             COMMAREA(WS-COMMAREA)                      02870000
                             RESP(WS-RESP-CODE)                         02880000
                        END-EXEC                                        02890000
                        IF WS-RESP-CODE NOT = DFHRESP(NORMAL)           02900000
                        STRING 'PROGRAM ' DELIMITED BY SIZE             02910000
                               CIMENU-PGM DELIMITED BY SPACE            02920000
                               ' IS NOT AVAILABLE' DELIMITED BY SIZE    02930000
                               INTO MSGO                                02940000
                           SET WS-MAP-DATAONLY TO TRUE                  02950000
                           PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT02960000
                        END-IF                                          02970000
                   WHEN DFHRESP(NOTFND)                                 02980000
                        MOVE 'INVALID TRANSATION ID!' TO MSGO           02990000
                        SET WS-MAP-DATAONLY TO TRUE                     03000000
                        PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT   03010000
                   WHEN OTHER                                           03020000
                        MOVE 'CIMENU FILE ERROR!' TO MSGO               03030000
                        SET WS-MAP-DATAONLY TO TRUE                     03040000
                        PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT   03050000
               END-EVALUATE                                             03060000
            END-IF                                                      03070000
            .                                                           03480000
      *                                                                 03490000
       3010-CHECK-INPUT-EXIT.                                           03500000
            EXIT.                                                       03510000
      *                                                                 03520000
      *                                                                 03840000
       3030-SEND-MAP.                                                   03850000
      *     IF LK-STEP = 001                                            03860017
               PERFORM 3040-SET-PROTECT                                 03870000
                  THRU 3040-SET-PROTECT-EXIT                            03880000
               MOVE 'PF1=RETURN PF3=EXIT ENTER=SET INQUERY PASSWORD'    03890017
                  TO KEYO                                               03900017
      *     END-IF                                                      03960017
            PERFORM 1010-ASK-TIME-DATE                                  04000000
               THRU 1010-ASK-TIME-DATE-EXIT                             04010000
            EVALUATE TRUE                                               04020000
                WHEN WS-MAP-ERASE                                       04030000
                     EXEC CICS SEND                                     04040000
                          MAP('CICA02')                                 04050000
                          MAPSET('CICA02')                              04060000
                          FROM(CICA02O)                                 04070000
                          ERASE                                         04080000
                     END-EXEC                                           04090000
                WHEN WS-MAP-DATAONLY                                    04100000
                     EXEC CICS SEND                                     04110000
                          MAP('CICA02')                                 04120000
                          MAPSET('CICA02')                              04130000
                          FROM(CICA02O)                                 04140000
                          DATAONLY                                      04150000
                     END-EXEC                                           04160000
            END-EVALUATE                                                04170000
      *     MOVE '1' TO WS-ENTER-FLAG                                   04180000
            PERFORM 5020-RETURN-TRANS THRU 5020-RETURN-TRANS-EXIT       04190000
            .                                                           04200000
      *                                                                 04210000
       3030-SEND-MAP-EXIT.                                              04220000
            EXIT.                                                       04230000
      *                                                                 04240000
       3040-SET-PROTECT.                                                04250000
            MOVE ATTR-PROT-MDT          TO NAMEA                        04260000
            MOVE ATTR-PROT-MDT          TO ENAMEA                       04270000
            MOVE ATTR-PROT-MDT          TO NATIONA                      04280000
            MOVE ATTR-PROT-MDT          TO BIRTHA                       04290000
            MOVE ATTR-PROT-MDT          TO GENDERA                      04300000
            MOVE ATTR-PROT-MDT          TO MASTUSA                      04310000
            MOVE ATTR-PROT-MDT          TO IDTYPEA                      04320000
            MOVE ATTR-PROT-MDT          TO IDNUMA                       04330000
            MOVE ATTR-PROT-MDT          TO ANSRYA                       04340000
            MOVE ATTR-PROT-MDT          TO MOBILEA                      04350000
            MOVE ATTR-PROT-MDT          TO EMAILA                       04360000
            MOVE ATTR-PROT-MDT          TO BITYPEA                      04370000
            MOVE ATTR-PROT-MDT          TO BIADDRA                      04380000
            MOVE ATTR-PROT-MDT          TO BIDATEA                      04390000
            MOVE ATTR-PROT-MDT          TO APDATEA                      04400000
            MOVE ATTR-PROT-MDT          TO LCTRYA                       04410000
            MOVE ATTR-PROT-MDT          TO LPROVA                       04420000
            MOVE ATTR-PROT-MDT          TO LCITYA                       04430000
            MOVE ATTR-PROT-MDT          TO LDISTA                       04440000
            MOVE ATTR-PROT-MDT          TO LZIPCA                       04450000
            MOVE ATTR-PROT-MDT          TO LADDRA                       04460000
            MOVE ATTR-PROT-MDT          TO LYEARSA                      04470000
            MOVE ATTR-PROT-MDT          TO CNAMEA                       04480000
            MOVE ATTR-PROT-MDT          TO CCTRYA                       04490000
            MOVE ATTR-PROT-MDT           TO CPROVA                      04500000
            MOVE ATTR-PROT-MDT          TO CCITYA                       04510000
            MOVE ATTR-PROT-MDT           TO CDISTA                      04520000
            MOVE ATTR-PROT-MDT           TO CZIPCA                      04530000
            MOVE ATTR-PROT-MDT          TO CADDRA                       04540000
            MOVE ATTR-PROT-MDT           TO CSRVYA                      04550000
            .                                                           04560000
       3040-SET-PROTECT-EXIT.                                           04570000
            EXIT.                                                       04580000
            EXIT.                                                       04920000
       4000-POST-PROCESSING.                                            05270000
      *                                                                 05280000
       4000-POST-PROCESSING-EXIT.                                       05290000
            EXIT.                                                       05300000
      *                                                                 05310000
       5000-CLEAN-UP.                                                   05320000
            PERFORM 5010-RETURN                                         05330000
               THRU 5010-RETURN-EXIT                                    05340000
            .                                                           05350000
      *                                                                 05360000
       5000-CLEAN-UP-EXIT.                                              05370000
            EXIT.                                                       05380000
      *                                                                 05390000
       5010-RETURN.                                                     05400000
            EXEC CICS RETURN END-EXEC                                   05410000
            .                                                           05420000
       5010-RETURN-EXIT.                                                05430000
            EXIT.                                                       05440000
      *                                                                 05450000
       5020-RETURN-TRANS.                                               05460000
            INITIALIZE WS-COMMAREA                                      05470000
            MOVE 'N' TO WS-FIRST-SEND OF WS-COMMAREA                    05480002
            MOVE LK-STEP TO WS-STEP OF WS-COMMAREA                      05490002
            MOVE LK-NUMB TO WS-NUMB OF WS-COMMAREA                      05491002
            MOVE LK-CUST-ID TO WS-CUST-ID OF WS-COMMAREA                05500002
            EXEC CICS RETURN TRANSID('CIB6')                            05510010
                      COMMAREA(WS-COMMAREA)                             05520000
            END-EXEC                                                    05530000
            .                                                           05540000
       5020-RETURN-TRANS-EXIT.                                          05550000
            EXIT.                                                       05560000
