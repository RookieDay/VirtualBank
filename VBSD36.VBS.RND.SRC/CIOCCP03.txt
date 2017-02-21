       IDENTIFICATION DIVISION.                                         00010004
       PROGRAM-ID. CIOCCP03.                                            00020004
      ***************************************************************** 00030004
      * CIOCCIMN - CLIENT PROGRAM                                       00040004
      *                                                                 00050004
      * CREDIT ISSUANCE MAIN MENU                                       00060004
      *                                                                 00070004
      ***************************************************************** 00080004
      *                         VERSION HISTORY                         00090004
      *---------------------------------------------------------------- 00100004
      *DATE/TIME ³   AUTHOR   ³ DESCRIPTION                             00110004
      *---------------------------------------------------------------- 00120004
      *2015-01-06    KEVIN      INITIAL VERSION                         00130004
      ***************************************************************** 00140004
       ENVIRONMENT DIVISION.                                            00150004
       DATA DIVISION.                                                   00160004
       WORKING-STORAGE SECTION.                                         00170004
      *                                                                 00180004
       77 WS-BEGIN              PIC X(17) VALUE 'CIOCCS02 WS BEGIN'.    00190004
       01 WS-VAR.                                                       00200004
          05 WS-GETTIME         PIC X(20).                              00210004
          05 WS-DATEOUT         PIC X(10).                              00220004
          05 WS-TIMEOUT         PIC X(8).                               00230004
          05 WS-RESP-CODE       PIC S9(8) COMP.                         00240004
          05 WS-MESSAGE         PIC X(40).                              00250004
          05 WS-ENTER-FLAG      PIC X(1).                               00260004
          05 WS-TRANSID         PIC X(4).                               00270004
       01 WS-MAP-OPTION         PIC X(1).                               00280004
          88 WS-MAP-ERASE       VALUE '0'.                              00290004
          88 WS-MAP-DATAONLY    VALUE '1'.                              00300004
       01 IX                    PIC 9(3).                               00310004
      *                                                                 00320004
      *SCREEN HANDLER                                                   00330004
       COPY SD11WS.                                                     00340004
      * SYMBOLIC MAP                                                    00350004
       COPY CICP03.                                                     00360007
      *MAP CONTROL                                                      00370004
       COPY DFHBMSCA.                                                   00380004
      *CICS FUNCTION KEYS                                               00390004
       COPY DFHAID.                                                     00400004
      *CIMENU                                                           00410004
       COPY CIMENU.                                                     00420004
      *                                                                 00430004
       COPY CIC0015I.                                                   00440005
       COPY CIC0015O.                                                   00450005
      *                                                                 00460004
       COPY CIC0016I.                                                   00470005
       COPY CIC0016O.                                                   00480005
      *                                                                 00490004
       01 WS-SRV-COMMAREA.                                              00500004
      *SERVICE REQUEST/RESPONSE COMMAREA                                00510004
       COPY SD01WS.                                                     00520004
      *                                                                 00530004
       01 WS-COMMAREA-CP04.                                             00630021
          05 WS-FIRST-SEND      PIC X(1).                               00640004
          05 WS-OPTION          PIC 9(3).                               00641021
          05 WS-CARD-NUMB       PIC 9(16).                              00650021
      *                                                                 00660021
       01 WS-COMMAREA.                                                  00661021
          05 WS-FIRST-SEND      PIC X(1).                               00662021
          05 WS-OPTION          PIC 9(3).                               00663021
          05 WS-CUST-ID-NUMB    PIC 9(18).                              00664021
      *                                                                 00670004
       01 WS-COMMAREA-CP00      PIC X(4).                               00680007
       77 WS-END                PIC X(17) VALUE 'CIOCCS02 WS END'.      00690004
      *                                                                 00700004
       LINKAGE SECTION.                                                 00710004
       01 DFHCOMMAREA.                                                  00720004
          05 LK-FIRST-SEND      PIC X(1).                               00730021
          05 LK-OPTION          PIC 9(3).                               00731021
          05 LK-CUST-ID-NUMB    PIC 9(18).                              00740005
      *                                                                 00760004
       PROCEDURE DIVISION.                                              00770004
       0000-MAINLINE.                                                   00780004
      *                                                                 00790004
            PERFORM 1000-INIT                                           00800004
               THRU 1000-INIT-EXIT                                      00810004
      *                                                                 00820004
            PERFORM 2000-PRE-PROCESSING                                 00830004
               THRU 2000-PRE-PROCESSING-EXIT                            00840004
      *                                                                 00850004
            PERFORM 3000-MAIN-PROCESS                                   00860004
               THRU 3000-MAIN-PROCESS-EXIT                              00870004
      *                                                                 00880004
            PERFORM 4000-POST-PROCESSING                                00890004
               THRU 4000-POST-PROCESSING-EXIT                           00900004
      *                                                                 00910004
            PERFORM 5000-CLEAN-UP                                       00920004
               THRU 5000-CLEAN-UP-EXIT                                  00930004
            .                                                           00940004
      *                                                                 00950004
       0000-EXIT.                                                       00960004
            EXIT.                                                       00970004
      *                                                                 00980004
       1000-INIT.                                                       00990004
            IF LK-FIRST-SEND = 'Y'                                      01000004
      *                                                                 01010004
               PERFORM 1020-GET-CARD-LIST                               01020009
                  THRU 1020-GET-CARD-LIST-EXIT                          01030009
      *                                                                 01040004
               MOVE LOW-VALUES TO CICP03O                               01050005
               IF CIC0016O-COUNT = 0                                    01060009
                  MOVE 'NO RECORDS LISTED!' TO MSGO                     01070020
               END-IF                                                   01080004
      *                                                                 01090004
               PERFORM 1030-POPULATE-MAP                                01100009
                  THRU 1030-POPULATE-MAP-EXIT                           01110009
      *                                                                 01120004
               SET WS-MAP-ERASE TO TRUE                                 01130004
               PERFORM 3030-SEND-MAP                                    01140004
                  THRU 3030-SEND-MAP-EXIT                               01150004
      * NOT FIRST SHOW                                                  01160004
            ELSE                                                        01170004
               MOVE LOW-VALUES TO CICP03I                               01180005
               EXEC CICS RECEIVE MAP('CICP03')                          01190005
                                 MAPSET('CICP03')                       01200005
                                 INTO(CICP03I)                          01210005
                                 RESP(WS-RESP-CODE)                     01220004
               END-EXEC                                                 01230004
            END-IF                                                      01240004
            .                                                           01250004
       1000-INIT-EXIT.                                                  01260004
            EXIT.                                                       01270004
      *                                                                 01280004
       1010-ASK-TIME-DATE.                                              01290004
      *                                                                 01300004
            EXEC CICS                                                   01310004
                 ASKTIME                                                01320004
                 ABSTIME(WS-GETTIME)                                    01330004
            END-EXEC                                                    01340004
            EXEC CICS                                                   01350004
                 FORMATTIME                                             01360004
                 ABSTIME(WS-GETTIME)                                    01370004
                 DATESEP('/')                                           01380004
                 YYYYMMDD(WS-DATEOUT)                                   01390004
            END-EXEC                                                    01400004
            EXEC CICS                                                   01410004
                 FORMATTIME                                             01420004
                 ABSTIME(WS-GETTIME)                                    01430004
                 TIMESEP                                                01440004
                 TIME(WS-TIMEOUT)                                       01450004
            END-EXEC                                                    01460004
            MOVE WS-DATEOUT TO SYSDO                                    01470004
            MOVE WS-TIMEOUT TO SYSTO                                    01480004
            .                                                           01490004
      *                                                                 01500004
       1010-ASK-TIME-DATE-EXIT.                                         01510004
            EXIT.                                                       01520004
      *                                                                 01530004
       1020-GET-CARD-LIST.                                              01540006
            INITIALIZE SDCA-SERVICE-COMMAREA                            01550004
            MOVE 'VBS.CI.CREDCARD.IN1' TO SD-SRV-NAME                   01560005
            INITIALIZE CIC0016I-REC                                     01570005
            MOVE LK-CUST-ID-NUMB TO CIC0016I-ID-NUMB                    01580014
            MOVE CIC0016I-REC TO SD-SRV-INPUT-DATA                      01590005
            EXEC CICS                                                   01600004
                 LINK                                                   01610004
                 PROGRAM(WS-PGM-SRV-DRIVER)                             01620004
                 COMMAREA(WS-SRV-COMMAREA)                              01630004
                 RESP(WS-RESP-CODE)                                     01640004
            END-EXEC                                                    01650004
            EVALUATE WS-RESP-CODE                                       01660004
                WHEN DFHRESP(NORMAL)                                    01670004
      *              IF SD-RESP-CODE EQUAL ZEROS                        01680004
                        INITIALIZE CIC0016O-REC                         01690005
                        MOVE SD-SRV-OUTPUT-DATA TO CIC0016O-REC         01700005
      *              END-IF                                             01710004
            END-EVALUATE                                                01720004
            .                                                           01730004
      *                                                                 01740004
       1020-GET-CARD-LIST-EXIT.                                         01750006
            EXIT.                                                       01760004
      *                                                                 01770004
       1030-POPULATE-MAP.                                               01780006
            PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 14                01790004
             IF IX <= CIC0016O-COUNT                                    01800010
                INITIALIZE CIC0015I-REC                                 01810010
                MOVE CIC0016O-CARD-NUMB(IX) TO CIC0015I-NUMB            01820015
                PERFORM 1040-GET-CARD                                   01830018
                     THRU 1040-GET-CARD-EXIT                            01840018
                MOVE CIC0015O-NUMB           TO CADIDO(IX)              01850014
                MOVE ATTR-PROT-SKIP-MDT           TO CADIDA(IX)         01860010
                MOVE CIC0015O-PROD-TYPE      TO PRTIDO(IX)              01870014
                MOVE CIC0015O-CURR-EXPIRY-DATE TO EXTIMEO(IX)           01890014
                MOVE CIC0015O-LAST-DATE      TO LTIMEO(IX)              01910014
                EVALUATE CIC0015O-STATUS                                01920014
                     WHEN 001                                           01930004
                          MOVE 'NEW'          TO STATSO(IX)             01940011
                     WHEN 002                                           01950004
                          MOVE 'CHECKED'      TO STATSO(IX)             01960011
                     WHEN 003                                           01970004
                          MOVE 'REVIEWED'     TO STATSO(IX)             01980011
                     WHEN 004                                           01990004
                          MOVE 'INVESTIED'    TO STATSO(IX)             02000011
                     WHEN 005                                           02010004
                          MOVE 'CREDITED'     TO STATSO(IX)             02020011
                     WHEN 010                                           02030004
                          MOVE 'REVRETN'      TO STATSO(IX)             02040011
                     WHEN 011                                           02050004
                          MOVE 'INVRETN'      TO STATSO(IX)             02060011
                     WHEN 012                                           02070004
                          MOVE 'CHKFAIL'      TO STATSO(IX)             02080011
                     WHEN 013                                           02090004
                          MOVE 'REVFAIL'      TO STATSO(IX)             02100011
                     WHEN 014                                           02110004
                          MOVE 'INVFAIL'      TO STATSO(IX)             02120011
                     WHEN 015                                           02130004
                          MOVE 'CREFAIL'      TO STATSO(IX)             02140011
                     WHEN OTHER                                         02150004
                          MOVE 'UNKNOWN'      TO STATSO(IX)             02160011
                 END-EVALUATE                                           02170004
              ELSE                                                      02180004
                 MOVE ATTR-PROT-DARK TO OPTA(IX)                        02190004
              END-IF                                                    02200004
            END-PERFORM                                                 02210004
            .                                                           02220004
      *                                                                 02230004
       1030-POPULATE-MAP-EXIT.                                          02240006
            EXIT.                                                       02250004
      *                                                                 02260004
       1040-GET-CARD.                                                   02270018
            INITIALIZE SDCA-SERVICE-COMMAREA                            02280004
            MOVE 'VBS.CI.CREDCARD.INQ' TO SD-SRV-NAME                   02290016
            MOVE CIC0015I-REC TO SD-SRV-INPUT-DATA                      02300007
            EXEC CICS                                                   02310004
                 LINK                                                   02320004
                 PROGRAM(WS-PGM-SRV-DRIVER)                             02330004
                 COMMAREA(WS-SRV-COMMAREA)                              02340004
                 RESP(WS-RESP-CODE)                                     02350004
            END-EXEC                                                    02360004
            EVALUATE WS-RESP-CODE                                       02370004
                WHEN DFHRESP(NORMAL)                                    02380004
                     IF SD-RESP-CODE EQUAL ZEROS                        02390004
                        INITIALIZE CIC0015O-REC                         02400007
                        MOVE SD-SRV-OUTPUT-DATA TO CIC0015O-REC         02410007
                     END-IF                                             02420004
            END-EVALUATE                                                02430004
            .                                                           02440004
       1040-GET-CARD-EXIT.                                              02450018
            EXIT.                                                       02460004
      *                                                                 02470004
      *                                                                 02480004
       2000-PRE-PROCESSING.                                             02490004
                                                                        02500004
       2000-PRE-PROCESSING-EXIT.                                        02510004
            EXIT.                                                       02520004
      *                                                                 02530004
       3000-MAIN-PROCESS.                                               02540004
            EVALUATE EIBAID                                             02550004
                WHEN DFHPF1                                             02560004
                     EXEC CICS                                          02580004
                          XCTL PROGRAM('CIOCCP00')                      02590007
                               COMMAREA(WS-COMMAREA-CP00)               02600023
                               RESP(WS-RESP-CODE)                       02610004
                     END-EXEC                                           02620004
                     IF WS-RESP-CODE NOT = DFHRESP(NORMAL)              02630004
                        MOVE 'PROGRAM CIOCCP00 IS NOT AVAILABLE'        02640007
                                TO MSGO                                 02650004
                        SET WS-MAP-DATAONLY TO TRUE                     02660004
                        PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT   02670004
                     END-IF                                             02680004
                WHEN DFHPF3                                             02690004
                     MOVE 'THANK YOU FOR USING THE SYSTEM' TO WS-MESSAGE02700004
                     EXEC CICS                                          02710004
                          SEND CONTROL                                  02720004
                          CURSOR                                        02730004
                          ERASE                                         02740004
                          FREEKB                                        02750004
                          ALARM                                         02760004
                     END-EXEC                                           02770004
                     EXEC CICS                                          02780004
                          SEND FROM(WS-MESSAGE)                         02790004
                     END-EXEC                                           02800004
                     PERFORM 5010-RETURN THRU 5010-RETURN-EXIT          02810004
                WHEN DFHCLEAR                                           02820004
                     EXEC CICS                                          02830004
                           SEND CONTROL                                 02840004
                           CURSOR                                       02850004
                           ERASE                                        02860004
                           FREEKB                                       02870004
                           ALARM                                        02880004
                     END-EXEC                                           02890004
                     PERFORM 5010-RETURN THRU 5010-RETURN-EXIT          02900004
                WHEN DFHPF9                                             02910004
                     PERFORM 1020-GET-CARD-LIST                         02920009
                        THRU 1020-GET-CARD-LIST-EXIT                    02930009
      *                                                                 02940004
      *              MOVE LOW-VALUES TO CICS02O                         02950004
                     IF CIC0016O-COUNT = ZEROS                          02960017
                        MOVE 'NO RECORDS LISTED!' TO MSGO               02970004
                     ELSE                                               02980004
                     MOVE LOW-VALUES TO CICP03O                         02990007
                        PERFORM 1030-POPULATE-MAP                       03000007
                           THRU 1030-POPULATE-MAP-EXIT                  03010007
                     END-IF                                             03020004
      *                                                                 03030004
                     SET WS-MAP-ERASE TO TRUE                           03040004
                     PERFORM 3030-SEND-MAP                              03050004
                        THRU 3030-SEND-MAP-EXIT                         03060004
                WHEN DFHENTER                                           03070004
                     IF WS-RESP-CODE NOT EQUAL DFHRESP(NORMAL)          03080004
                        MOVE 'INVALID REQUEST, CHECK YOUR INPUT.'       03090004
                             TO MSGO                                    03100004
                        SET WS-MAP-DATAONLY TO TRUE                     03110004
                        PERFORM 3030-SEND-MAP                           03120004
                           THRU 3030-SEND-MAP-EXIT                      03130004
                     ELSE                                               03140004
                        PERFORM 3010-CHECK-INPUT                        03150004
                           THRU 3010-CHECK-INPUT-EXIT                   03160004
                        PERFORM 3020-XCTL                               03170004
                           THRU 3020-XCTL-EXIT                          03180004
                     END-IF                                             03190004
                WHEN OTHER                                              03200004
                     MOVE 'INVALID KEY PRESSED!' TO MSGO                03210004
                     SET WS-MAP-DATAONLY TO TRUE                        03220004
                     PERFORM 3030-SEND-MAP                              03230004
                        THRU 3030-SEND-MAP-EXIT                         03240004
            END-EVALUATE                                                03250004
            .                                                           03260004
       3000-MAIN-PROCESS-EXIT.                                          03270004
            EXIT.                                                       03280004
      *                                                                 03290004
       3010-CHECK-INPUT.                                                03300004
            IF COMMUL NOT = 0                                           03310004
               INITIALIZE CIMENU-REC                                    03320004
               MOVE COMMUI TO CIMENU-TRANSID                            03330004
               EXEC CICS READ                                           03340004
                    FILE('CIMENU')                                      03350004
                    INTO(CIMENU-REC)                                    03360004
                    RIDFLD(CIMENU-TRANSID)                              03370004
                    RESP(WS-RESP-CODE)                                  03380004
               END-EXEC                                                 03390004
               EVALUATE WS-RESP-CODE                                    03400004
                   WHEN DFHRESP(NORMAL)                                 03410004
                        EXEC CICS                                       03420004
                             XCTL PROGRAM(CIMENU-PGM)                   03430004
                             COMMAREA(CIMENU-TRANSID)                   03440004
                             RESP(WS-RESP-CODE)                         03450004
                        END-EXEC                                        03460004
                        IF WS-RESP-CODE NOT = DFHRESP(NORMAL)           03470004
                        STRING 'PROGRAM ' DELIMITED BY SIZE             03480004
                               CIMENU-PGM DELIMITED BY SPACE            03490004
                               ' IS NOT AVAILABLE' DELIMITED BY SIZE    03500004
                               INTO MSGO                                03510004
                           SET WS-MAP-DATAONLY TO TRUE                  03520004
                           PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT03530004
                        END-IF                                          03540004
                   WHEN DFHRESP(NOTFND)                                 03550004
                        MOVE 'INVALID TRANSATION ID!' TO MSGO           03560004
                        SET WS-MAP-DATAONLY TO TRUE                     03570004
                        PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT   03580004
                   WHEN OTHER                                           03590004
                        MOVE 'CIMENU FILE ERROR!' TO MSGO               03600004
                        SET WS-MAP-DATAONLY TO TRUE                     03610004
                        PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT   03620004
               END-EVALUATE                                             03630004
            END-IF                                                      03640004
            .                                                           03650004
      *                                                                 03660004
       3010-CHECK-INPUT-EXIT.                                           03670004
            EXIT.                                                       03680004
      *                                                                 03690004
       3020-XCTL.                                                       03700004
            PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 14                03710004
               IF OPTI(IX) = 'S'                                        03720004
                 INITIALIZE WS-COMMAREA-CP04                            03730021
                 MOVE 'Y'   TO WS-FIRST-SEND OF WS-COMMAREA-CP04        03740021
                 MOVE LK-OPTION TO WS-OPTION OF WS-COMMAREA-CP04        03741024
                 MOVE CADIDI(IX) TO WS-CARD-NUMB OF WS-COMMAREA-CP04    03750022
                  EXEC CICS                                             03760004
                       XCTL PROGRAM('CIOCCP04')                         03770007
                            COMMAREA(WS-COMMAREA-CP04)                  03780021
                            RESP(WS-RESP-CODE)                          03790004
                  END-EXEC                                              03800004
                  IF WS-RESP-CODE NOT = DFHRESP(NORMAL)                 03810004
                     MOVE 'PROGRAM CIOCCP04 IS NOT AVAILABLE'           03820007
                             TO MSGO                                    03830004
                     SET WS-MAP-DATAONLY TO TRUE                        03840004
                     PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT      03850004
                  END-IF                                                03860004
               END-IF                                                   03870004
            END-PERFORM                                                 03880004
            MOVE 'INVALID SELECTION!' TO MSGO                           03890004
            SET WS-MAP-DATAONLY TO TRUE                                 03900004
            PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT               03910004
            .                                                           03920004
      *                                                                 03930004
       3020-XCTL-EXIT.                                                  03940004
            EXIT.                                                       03950004
      *                                                                 03960004
       3030-SEND-MAP.                                                   03970004
            PERFORM 1010-ASK-TIME-DATE                                  03990004
               THRU 1010-ASK-TIME-DATE-EXIT                             04000004
               DISPLAY 'BEFORE SEND'                                    04001019
            EVALUATE TRUE                                               04010004
                WHEN WS-MAP-ERASE                                       04020004
                     EXEC CICS SEND                                     04030004
                          MAP('CICP03')                                 04040007
                          MAPSET('CICP03')                              04050007
                          FROM(CICP03O)                                 04060007
                          ERASE                                         04070004
                     END-EXEC                                           04080004
                WHEN WS-MAP-DATAONLY                                    04090004
                     EXEC CICS SEND                                     04100004
                          MAP('CICP03')                                 04110007
                          MAPSET('CICP03')                              04120007
                          FROM(CICP03O)                                 04130007
                          DATAONLY                                      04140004
                     END-EXEC                                           04150004
            END-EVALUATE                                                04160004
            DISPLAY 'AFTER SEND'                                        04161019
            PERFORM 5020-RETURN-TRANS THRU 5020-RETURN-TRANS-EXIT       04170004
            .                                                           04180004
      *                                                                 04190004
       3030-SEND-MAP-EXIT.                                              04200004
            EXIT.                                                       04210004
      *                                                                 04220004
      *                                                                 04230004
       3040-POPULATE-CICUS-EXIT.                                        04240004
            EXIT.                                                       04250004
      *                                                                 04260004
       4000-POST-PROCESSING.                                            04270004
                                                                        04280004
       4000-POST-PROCESSING-EXIT.                                       04290004
            EXIT.                                                       04300004
      *                                                                 04310004
       5000-CLEAN-UP.                                                   04320004
            PERFORM 5010-RETURN                                         04330004
               THRU 5010-RETURN-EXIT                                    04340004
            .                                                           04350004
      *                                                                 04360004
       5000-CLEAN-UP-EXIT.                                              04370004
            EXIT.                                                       04380004
      *                                                                 04390004
       5010-RETURN.                                                     04400004
            EXEC CICS RETURN END-EXEC                                   04410004
            .                                                           04420004
       5010-RETURN-EXIT.                                                04430004
            EXIT.                                                       04440004
      *                                                                 04450004
       5020-RETURN-TRANS.                                               04460004
            INITIALIZE WS-COMMAREA                                      04470013
            MOVE 'N'  TO WS-FIRST-SEND OF WS-COMMAREA                   04480012
            MOVE LK-OPTION TO WS-OPTION OF WS-COMMAREA                  04481025
            MOVE LK-CUST-ID-NUMB TO WS-CUST-ID-NUMB OF WS-COMMAREA      04490013
            EXEC CICS RETURN TRANSID('CIC3')                            04510023
                      COMMAREA(WS-COMMAREA)                             04520008
            END-EXEC                                                    04530004
            .                                                           04540004
       5020-RETURN-TRANS-EXIT.                                          04550004
            EXIT.                                                       04560004
