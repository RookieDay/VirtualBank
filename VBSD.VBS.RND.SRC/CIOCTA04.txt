       IDENTIFICATION DIVISION.                                         00010001
       PROGRAM-ID. CIOCTA04.                                            00020001
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
      *2015-02-03    KRIS      INITIAL VERSION                          00130001
      ***************************************************************** 00140001
       ENVIRONMENT DIVISION.                                            00150001
       DATA DIVISION.                                                   00160001
       WORKING-STORAGE SECTION.                                         00170001
      *                                                                 00180001
       77 WS-BEGIN              PIC X(17) VALUE 'CIOCTA04 WS BEGIN'.    00190001
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
       COPY CICA02.                                                     00350001
      *MAP CONTROL                                                      00360001
       COPY DFHBMSCA.                                                   00370001
      *CICS FUNCTION KEYS                                               00380001
       COPY DFHAID.                                                     00390001
      *CIMENU                                                           00400001
       COPY CIMENU.                                                     00410001
      *                                                                 00420001
       COPY CIC0025I.                                                   00430004
       COPY CIC0025O.                                                   00440004
      *                                                                 00450001
       COPY CIC0002I.                                                   00460001
       COPY CIC0002O.                                                   00470001
      *                                                                 00480001
       01 WS-SRV-COMMAREA.                                              00490001
      *SERVICE REQUEST/RESPONSE COMMAREA                                00500001
       COPY SD01WS.                                                     00510001
      *                                                                 00520001
       01 WS-COMMAREA.                                                  00530001
          05 WS-FIRST-SEND      PIC X(1).                               00540001
      *   05 WS-STEP            PIC 9(3).                               00550003
          05 WS-NUMB            PIC 9(16).                              00560001
          05 WS-QUEUE           PIC 9(08).                              00570009
      *                                                                 00580001
       01 WS-COMMAREA-TA03.                                             00590007
          05 WS-FIRST-SEND      PIC X(1).                               00600001
          05 WS-STEP            PIC 9(3).                               00610008
          05 WS-QUEUE           PIC X(8).                               00611009
      *   05 WS-NUMB            PIC 9(16).                              00620008
      *   05 WS-CUST-ID         PIC 9(18).                              00630003
       01 WS-QUEUE-SAVE         PIC X(08).                              00631009
      *                                                                 00640001
      *01 WS-COMMAREA-CS07.                                             00650002
      *   05 WS-FIRST-SEND      PIC X(1).                               00660003
      *   05 WS-STEP            PIC 9(3).                               00670003
      *   05 WS-NUMB            PIC 9(16).                              00680003
      *   05 WS-CUST-ID         PIC 9(18).                              00690003
       77 WS-END                PIC X(17) VALUE 'CIOCTA04 WS END'.      00700001
      *                                                                 00710001
       LINKAGE SECTION.                                                 00720001
       01 DFHCOMMAREA.                                                  00730001
          05 LK-FIRST-SEND      PIC X(1).                               00740001
      *   05 LK-STEP            PIC 9(3).                               00750003
          05 LK-NUMB            PIC 9(16).                              00760001
          05 LK-QUEUE           PIC X(08).                              00770009
      *                                                                 00780001
       PROCEDURE DIVISION.                                              00790001
       0000-MAINLINE.                                                   00800001
      *                                                                 00810001
            PERFORM 1000-INIT                                           00820001
               THRU 1000-INIT-EXIT                                      00830001
      *                                                                 00840001
            PERFORM 2000-PRE-PROCESSING                                 00850001
               THRU 2000-PRE-PROCESSING-EXIT                            00860001
      *                                                                 00870001
            PERFORM 3000-MAIN-PROCESS                                   00880001
               THRU 3000-MAIN-PROCESS-EXIT                              00890001
      *                                                                 00900001
            PERFORM 4000-POST-PROCESSING                                00910001
               THRU 4000-POST-PROCESSING-EXIT                           00920001
      *                                                                 00930001
            PERFORM 5000-CLEAN-UP                                       00940001
               THRU 5000-CLEAN-UP-EXIT                                  00950001
            .                                                           00960001
      *                                                                 00970001
       0000-EXIT.                                                       00980001
            EXIT.                                                       00990001
      *                                                                 01000001
       1000-INIT.                                                       01010001
            IF LK-FIRST-SEND = 'Y'                                      01020001
               MOVE LK-QUEUE TO WS-QUEUE-SAVE                           01021009
               MOVE LOW-VALUES TO CICA02O                               01030001
               PERFORM 1020-GET-EXISTING-CARD                           01040005
                  THRU 1020-GET-EXISTING-CARD-EXIT                      01050005
               SET WS-MAP-ERASE TO TRUE                                 01060001
               PERFORM 3030-SEND-MAP                                    01070001
                  THRU 3030-SEND-MAP-EXIT                               01080001
      * NOT FIRST SHOW                                                  01090001
            ELSE                                                        01100001
               MOVE LK-QUEUE TO WS-QUEUE-SAVE                           01101010
                  MOVE LOW-VALUES TO CICA02I                            01110001
                  EXEC CICS RECEIVE MAP('CICA02')                       01120001
                                   MAPSET('CICA02')                     01130001
                                   INTO(CICA02I)                        01140001
                                   RESP(WS-RESP-CODE)                   01150001
                  END-EXEC                                              01160001
            END-IF                                                      01170001
            .                                                           01180001
       1000-INIT-EXIT.                                                  01190001
            EXIT.                                                       01200001
      *                                                                 01210001
       1010-ASK-TIME-DATE.                                              01220001
      *                                                                 01230001
            EXEC CICS                                                   01240001
                 ASKTIME                                                01250001
                 ABSTIME(WS-GETTIME)                                    01260001
            END-EXEC                                                    01270001
            EXEC CICS                                                   01280001
                 FORMATTIME                                             01290001
                 ABSTIME(WS-GETTIME)                                    01300001
                 DATESEP('/')                                           01310001
                 YYYYMMDD(WS-DATEOUT)                                   01320001
            END-EXEC                                                    01330001
            EXEC CICS                                                   01340001
                 FORMATTIME                                             01350001
                 ABSTIME(WS-GETTIME)                                    01360001
                 TIMESEP                                                01370001
                 TIME(WS-TIMEOUT)                                       01380001
            END-EXEC                                                    01390001
            MOVE WS-DATEOUT TO SYSDO                                    01400001
            MOVE WS-TIMEOUT TO SYSTO                                    01410001
            .                                                           01420001
      *                                                                 01430001
       1010-ASK-TIME-DATE-EXIT.                                         01440001
            EXIT.                                                       01450001
      *                                                                 01460001
       1020-GET-EXISTING-CARD.                                          01470003
            INITIALIZE SDCA-SERVICE-COMMAREA                            01480001
            MOVE 'VBS.CI.CUSTOMER.IN1' TO SD-SRV-NAME                   01490001
            INITIALIZE CIC0025I-REC                                     01500004
            MOVE LK-NUMB TO CIC0025I-NUMB                               01510006
            MOVE CIC0025I-REC TO SD-SRV-INPUT-DATA                      01520004
            EXEC CICS                                                   01530001
                 LINK                                                   01540001
                 PROGRAM(WS-PGM-SRV-DRIVER)                             01550001
                 COMMAREA(WS-SRV-COMMAREA)                              01560001
                 RESP(WS-RESP-CODE)                                     01570001
            END-EXEC                                                    01580001
            EVALUATE WS-RESP-CODE                                       01590001
                WHEN DFHRESP(NORMAL)                                    01600001
                     IF SD-RESP-CODE EQUAL ZEROS                        01610001
                        PERFORM 1030-POPULATE-CUST                      01620001
                           THRU 1030-POPULATE-CUST-EXIT                 01630001
                     END-IF                                             01640001
            END-EVALUATE                                                01650001
            .                                                           01660001
      *                                                                 01670001
       1020-GET-EXISTING-CARD-EXIT.                                     01680003
            EXIT.                                                       01690001
      *                                                                 01700001
       1030-POPULATE-CUST.                                              01710001
            INITIALIZE CIC0025O-REC                                     01720004
            MOVE SD-SRV-OUTPUT-DATA       TO CIC0025O-REC               01730004
            MOVE CIC0025O-NAME            TO NAMEO                      01740004
            MOVE CIC0025O-ENGLISH-NAME    TO ENAMEO                     01750004
            MOVE CIC0025O-NATIONALITY     TO NATIONO                    01760004
            MOVE CIC0025O-BIRTH-DATE      TO BIRTHO                     01770004
            MOVE CIC0025O-GENDER          TO GENDERO                    01780004
            MOVE CIC0025O-MARITAL-STATUS  TO MASTUSO                    01790004
            MOVE CIC0025O-ID-TYPE    TO IDTYPEO                         01800004
            MOVE CIC0025O-ID-NUMBER  TO IDNUMO                          01810004
            MOVE CIC0025O-ANNUAL-SALARY   TO ANSRYO                     01820004
            MOVE CIC0025O-MOBILE          TO MOBILEO                    01830004
            MOVE CIC0025O-EMAIL           TO EMAILO                     01840004
            MOVE CIC0025O-BILL-TYPE       TO BITYPEO                    01850004
            MOVE CIC0025O-BILL-ADDR       TO BIADDRO                    01860004
            MOVE CIC0025O-BILL-DATE       TO BIDATEO                    01870004
            MOVE CIC0025O-APP-DATE        TO APDATEO                    01880004
            MOVE CIC0025O-LIVE-COUNTRY    TO LCTRYO                     01890004
            MOVE CIC0025O-LIVE-PROVINCE   TO LPROVO                     01900004
            MOVE CIC0025O-LIVE-CITY       TO LCITYO                     01910004
            MOVE CIC0025O-LIVE-DISTRICT   TO LDISTO                     01920004
            MOVE CIC0025O-LIVE-ZIP-CODE   TO LZIPCO                     01930004
            MOVE CIC0025O-LIVE-ADDRESS    TO LADDRO                     01940004
            MOVE CIC0025O-LIVE-YEARS      TO LYEARSO                    01950004
            MOVE CIC0025O-COMPANY-NAME    TO CNAMEO                     01960004
            MOVE CIC0025O-COMPANY-COUNTRY TO CCTRYO                     01970004
            MOVE CIC0025O-COMPANY-PROVINCE TO CPROVO                    01980004
            MOVE CIC0025O-COMPANY-CITY    TO CCITYO                     01990004
            MOVE CIC0025O-COMPANY-DISTRICT TO CDISTO                    02000004
            MOVE CIC0025O-COMPANY-ZIP-CODE TO CZIPCO                    02010004
            MOVE CIC0025O-COMPANY-ADRESS  TO CADDRO                     02020004
            MOVE CIC0025O-COMPANY-SERVE-YEAR TO CSRVYO                  02030004
            .                                                           02040001
      *                                                                 02050001
       1030-POPULATE-CUST-EXIT.                                         02060001
            EXIT.                                                       02070001
       2000-PRE-PROCESSING.                                             02080001
      *                                                                 02090001
       2000-PRE-PROCESSING-EXIT.                                        02100001
            EXIT.                                                       02110001
      *                                                                 02120001
       3000-MAIN-PROCESS.                                               02130001
            EVALUATE EIBAID                                             02140007
                WHEN DFHPF1                                             02150007
                MOVE 'Y' TO WS-FIRST-SEND OF WS-COMMAREA-TA03           02160007
                MOVE 002     TO WS-STEP OF WS-COMMAREA-TA03             02180011
      *         MOVE 000     TO WS-STEP OF WS-COMMAREA-TA03             02181011
                MOVE WS-QUEUE-SAVE TO WS-QUEUE OF WS-COMMAREA-TA03      02190010
                     EXEC CICS                                          02200001
                          XCTL PROGRAM('CIOCTA03')                      02210003
                           COMMAREA(WS-COMMAREA-TA03)                   02220007
                               RESP(WS-RESP-CODE)                       02230001
                     END-EXEC                                           02240001
                     IF WS-RESP-CODE NOT = DFHRESP(NORMAL)              02250001
                        MOVE 'PROGRAM CIOCTA03 IS NOT AVAILABLE'        02260003
                                TO MSGO                                 02270001
                        SET WS-MAP-DATAONLY TO TRUE                     02280001
                        PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT   02290001
                     END-IF                                             02300001
                WHEN DFHPF3                                             02310007
                     MOVE 'THANK YOU FOR USING THE SYSTEM' TO WS-MESSAGE02320001
                     EXEC CICS                                          02330001
                          SEND CONTROL                                  02340001
                          CURSOR                                        02350001
                          ERASE                                         02360001
                          FREEKB                                        02370001
                          ALARM                                         02380001
                     END-EXEC                                           02390001
                     EXEC CICS                                          02400001
                          SEND FROM(WS-MESSAGE)                         02410001
                     END-EXEC                                           02420001
                     PERFORM 5010-RETURN THRU 5010-RETURN-EXIT          02430001
                WHEN DFHCLEAR                                           02440007
                     EXEC CICS                                          02450001
                           SEND CONTROL                                 02460001
                           CURSOR                                       02470001
                           ERASE                                        02480001
                           FREEKB                                       02490001
                           ALARM                                        02500001
                     END-EXEC                                           02510001
                     PERFORM 5010-RETURN THRU 5010-RETURN-EXIT          02520001
      *         WHEN DFHENTER ALSO ANY                                  02530003
      *              INITIALIZE WS-COMMAREA                             02540003
      *              MOVE 'Y' TO WS-FIRST-SEND OF WS-COMMAREA-CS07      02550003
      *              MOVE LK-STEP TO WS-STEP OF WS-COMMAREA-CS07        02560003
      *              MOVE LK-NUMB TO WS-NUMB OF WS-COMMAREA-CS07        02570003
      *              MOVE LK-CUST-ID TO WS-CUST-ID OF                   02580003
      *                   WS-COMMAREA-CS07                              02590003
      *              EXEC CICS                                          02600003
      *                   XCTL PROGRAM('CIOCCS07')                      02610003
      *                        RESP(WS-RESP-CODE)                       02620003
      *                        COMMAREA(WS-COMMAREA-CS07)               02630003
      *              END-EXEC                                           02640003
      *              IF WS-RESP-CODE NOT = DFHRESP(NORMAL)              02650003
      *                 MOVE 'PROGRAM CIOCCS07 IS NOT AVAILABLE'        02660003
      *                         TO MSGO                                 02670003
      *                 SET WS-MAP-DATAONLY TO TRUE                     02680003
      *                 PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT   02690003
      *              END-IF                                             02700003
      *          WHEN DFHENTER ALSO LK-STEP = 002                       02710001
      *               IF WS-RESP-CODE NOT EQUAL DFHRESP(NORMAL)         02720001
      *                  MOVE 'INVALID REQUEST, CHECK YOUR INPUT.'      02730001
      *                       TO MSGO                                   02740001
      *                  SET WS-MAP-DATAONLY TO TRUE                    02750001
      *                  PERFORM 3030-SEND-MAP                          02760001
      *                     THRU 3030-SEND-MAP-EXIT                     02770001
      *               ELSE                                              02780001
      *                  PERFORM 3010-CHECK-INPUT                       02790001
      *                     THRU 3010-CHECK-INPUT-EXIT                  02800001
      *                  PERFORM 3020-CUST-UPDATE                       02810001
      *                     THRU 3020-CUST-UPDATE-EXIT                  02820001
      *               END-IF                                            02830001
                 WHEN OTHER                                             02840001
                      MOVE 'INVALID KEY PRESSED!' TO MSGO               02850001
                      SET WS-MAP-DATAONLY TO TRUE                       02860001
                      PERFORM 3030-SEND-MAP                             02870001
                         THRU 3030-SEND-MAP-EXIT                        02880001
            END-EVALUATE                                                02890001
            .                                                           02900001
       3000-MAIN-PROCESS-EXIT.                                          02910001
            EXIT.                                                       02920001
      *                                                                 02930001
       3010-CHECK-INPUT.                                                02940001
            IF COMMUL NOT = 0                                           02950001
               INITIALIZE CIMENU-REC                                    02960001
               MOVE COMMUI TO CIMENU-TRANSID                            02970001
               EXEC CICS READ                                           02980001
                    FILE('CIMENU')                                      02990001
                    INTO(CIMENU-REC)                                    03000001
                    RIDFLD(CIMENU-TRANSID)                              03010001
                    RESP(WS-RESP-CODE)                                  03020001
               END-EXEC                                                 03030001
               EVALUATE WS-RESP-CODE                                    03040001
                   WHEN DFHRESP(NORMAL)                                 03050001
                        EXEC CICS                                       03060001
                             XCTL PROGRAM(CIMENU-PGM)                   03070001
                             COMMAREA(WS-COMMAREA)                      03080001
                             RESP(WS-RESP-CODE)                         03090001
                        END-EXEC                                        03100001
                        IF WS-RESP-CODE NOT = DFHRESP(NORMAL)           03110001
                        STRING 'PROGRAM ' DELIMITED BY SIZE             03120001
                               CIMENU-PGM DELIMITED BY SPACE            03130001
                               ' IS NOT AVAILABLE' DELIMITED BY SIZE    03140001
                               INTO MSGO                                03150001
                           SET WS-MAP-DATAONLY TO TRUE                  03160001
                           PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT03170001
                        END-IF                                          03180001
                   WHEN DFHRESP(NOTFND)                                 03190001
                        MOVE 'INVALID TRANSATION ID!' TO MSGO           03200001
                        SET WS-MAP-DATAONLY TO TRUE                     03210001
                        PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT   03220001
                   WHEN OTHER                                           03230001
                        MOVE 'CIMENU FILE ERROR!' TO MSGO               03240001
                        SET WS-MAP-DATAONLY TO TRUE                     03250001
                        PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT   03260001
               END-EVALUATE                                             03270001
            END-IF                                                      03280001
            .                                                           03290001
      *                                                                 03300001
       3010-CHECK-INPUT-EXIT.                                           03310001
            EXIT.                                                       03320001
      *                                                                 03330001
      *                                                                 03340001
       3030-SEND-MAP.                                                   03350001
      *     IF LK-STEP = 001                                            03360001
               PERFORM 3040-SET-PROTECT                                 03370001
                  THRU 3040-SET-PROTECT-EXIT                            03380001
               MOVE 'PF1=RETURN PF3=EXIT ENTER=SET INQUERY PASSWORD'    03390001
                  TO KEYO                                               03400001
      *     END-IF                                                      03410001
            PERFORM 1010-ASK-TIME-DATE                                  03420001
               THRU 1010-ASK-TIME-DATE-EXIT                             03430001
            EVALUATE TRUE                                               03440001
                WHEN WS-MAP-ERASE                                       03450001
                     EXEC CICS SEND                                     03460001
                          MAP('CICA02')                                 03470001
                          MAPSET('CICA02')                              03480001
                          FROM(CICA02O)                                 03490001
                          ERASE                                         03500001
                     END-EXEC                                           03510001
                WHEN WS-MAP-DATAONLY                                    03520001
                     EXEC CICS SEND                                     03530001
                          MAP('CICA02')                                 03540001
                          MAPSET('CICA02')                              03550001
                          FROM(CICA02O)                                 03560001
                          DATAONLY                                      03570001
                     END-EXEC                                           03580001
            END-EVALUATE                                                03590001
      *     MOVE '1' TO WS-ENTER-FLAG                                   03600001
            PERFORM 5020-RETURN-TRANS THRU 5020-RETURN-TRANS-EXIT       03610001
            .                                                           03620001
      *                                                                 03630001
       3030-SEND-MAP-EXIT.                                              03640001
            EXIT.                                                       03650001
      *                                                                 03660001
       3040-SET-PROTECT.                                                03670001
            MOVE ATTR-PROT-MDT          TO NAMEA                        03680001
            MOVE ATTR-PROT-MDT          TO ENAMEA                       03690001
            MOVE ATTR-PROT-MDT          TO NATIONA                      03700001
            MOVE ATTR-PROT-MDT          TO BIRTHA                       03710001
            MOVE ATTR-PROT-MDT          TO GENDERA                      03720001
            MOVE ATTR-PROT-MDT          TO MASTUSA                      03730001
            MOVE ATTR-PROT-MDT          TO IDTYPEA                      03740001
            MOVE ATTR-PROT-MDT          TO IDNUMA                       03750001
            MOVE ATTR-PROT-MDT          TO ANSRYA                       03760001
            MOVE ATTR-PROT-MDT          TO MOBILEA                      03770001
            MOVE ATTR-PROT-MDT          TO EMAILA                       03780001
            MOVE ATTR-PROT-MDT          TO BITYPEA                      03790001
            MOVE ATTR-PROT-MDT          TO BIADDRA                      03800001
            MOVE ATTR-PROT-MDT          TO BIDATEA                      03810001
            MOVE ATTR-PROT-MDT          TO APDATEA                      03820001
            MOVE ATTR-PROT-MDT          TO LCTRYA                       03830001
            MOVE ATTR-PROT-MDT          TO LPROVA                       03840001
            MOVE ATTR-PROT-MDT          TO LCITYA                       03850001
            MOVE ATTR-PROT-MDT          TO LDISTA                       03860001
            MOVE ATTR-PROT-MDT          TO LZIPCA                       03870001
            MOVE ATTR-PROT-MDT          TO LADDRA                       03880001
            MOVE ATTR-PROT-MDT          TO LYEARSA                      03890001
            MOVE ATTR-PROT-MDT          TO CNAMEA                       03900001
            MOVE ATTR-PROT-MDT          TO CCTRYA                       03910001
            MOVE ATTR-PROT-MDT           TO CPROVA                      03920001
            MOVE ATTR-PROT-MDT          TO CCITYA                       03930001
            MOVE ATTR-PROT-MDT           TO CDISTA                      03940001
            MOVE ATTR-PROT-MDT           TO CZIPCA                      03950001
            MOVE ATTR-PROT-MDT          TO CADDRA                       03960001
            MOVE ATTR-PROT-MDT           TO CSRVYA                      03970001
            .                                                           03980001
       3040-SET-PROTECT-EXIT.                                           03990001
            EXIT.                                                       04000001
            EXIT.                                                       04010001
       4000-POST-PROCESSING.                                            04020001
      *                                                                 04030001
       4000-POST-PROCESSING-EXIT.                                       04040001
            EXIT.                                                       04050001
      *                                                                 04060001
       5000-CLEAN-UP.                                                   04070001
            PERFORM 5010-RETURN                                         04080001
               THRU 5010-RETURN-EXIT                                    04090001
            .                                                           04100001
      *                                                                 04110001
       5000-CLEAN-UP-EXIT.                                              04120001
            EXIT.                                                       04130001
      *                                                                 04140001
       5010-RETURN.                                                     04150001
            EXEC CICS RETURN END-EXEC                                   04160001
            .                                                           04170001
       5010-RETURN-EXIT.                                                04180001
            EXIT.                                                       04190001
      *                                                                 04200001
       5020-RETURN-TRANS.                                               04210001
            INITIALIZE WS-COMMAREA                                      04220001
            MOVE 'N' TO WS-FIRST-SEND OF WS-COMMAREA                    04230001
      *     MOVE LK-STEP TO WS-STEP OF WS-COMMAREA                      04240003
            MOVE LK-NUMB TO WS-NUMB OF WS-COMMAREA                      04250001
            MOVE WS-QUEUE-SAVE TO WS-QUEUE   OF WS-COMMAREA             04260009
            EXEC CICS RETURN TRANSID('CID4')                            04270001
                      COMMAREA(WS-COMMAREA)                             04280001
            END-EXEC                                                    04290001
            .                                                           04300001
       5020-RETURN-TRANS-EXIT.                                          04310001
            EXIT.                                                       04320001
