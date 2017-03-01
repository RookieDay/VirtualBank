       IDENTIFICATION DIVISION.                                         00010002
       PROGRAM-ID. CIOCCS08.                                            00020002
      ***************************************************************** 00030002
      * CIOCCIMN - CLIENT PROGRAM                                       00040002
      *                                                                 00050002
      * CREDIT ISSUANCE MAIN MENU                                       00060002
      *                                                                 00070002
      ***************************************************************** 00080002
      *                         VERSION HISTORY                         00090002
      *---------------------------------------------------------------- 00100002
      *DATE/TIME ³   AUTHOR   ³ DESCRIPTION                             00110002
      *---------------------------------------------------------------- 00120002
      *2015-02-03    KRIS      INITIAL VERSION                          00130002
      ***************************************************************** 00140002
       ENVIRONMENT DIVISION.                                            00150002
       DATA DIVISION.                                                   00160002
       WORKING-STORAGE SECTION.                                         00170002
      *                                                                 00180002
       77 WS-BEGIN              PIC X(17) VALUE 'CIOCCS08 WS BEGIN'.    00190002
       01 WS-VAR.                                                       00200002
          05 WS-GETTIME         PIC X(20).                              00210002
          05 WS-DATEOUT         PIC X(10).                              00220002
          05 WS-TIMEOUT         PIC X(8).                               00230002
          05 WS-RESP-CODE       PIC S9(8) COMP.                         00240002
          05 WS-MESSAGE         PIC X(40).                              00250002
          05 WS-ENTER-FLAG      PIC X(1).                               00260002
          05 WS-TRANSID         PIC X(4).                               00270002
       01 WS-MAP-OPTION         PIC X(1).                               00280002
          88 WS-MAP-ERASE       VALUE '0'.                              00290002
          88 WS-MAP-DATAONLY    VALUE '1'.                              00300002
      *                                                                 00310002
      *SCREEN HANDLER                                                   00320002
       COPY SD11WS.                                                     00330002
      * SYMBOLIC MAP                                                    00340002
       COPY CICS08.                                                     00350002
      *MAP CONTROL                                                      00360002
       COPY DFHBMSCA.                                                   00370002
      *CICS FUNCTION KEYS                                               00380002
       COPY DFHAID.                                                     00390002
      *CIMENU                                                           00400002
       COPY CIMENU.                                                     00410002
      *                                                                 00420002
       COPY CIC0015I.                                                   00430009
       COPY CIC0015O.                                                   00440009
      *                                                                 00440110
       COPY CIC0014I.                                                   00441010
       COPY CIC0014O.                                                   00442010
      *                                                                 00450002
       01 WS-SRV-COMMAREA.                                              00460002
      *SERVICE REQUEST/RESPONSE COMMAREA                                00470002
       COPY SD01WS.                                                     00480002
      *                                                                 00490002
       01 WS-COMMAREA.                                                  00500002
          05 WS-FIRST-SEND      PIC X(1).                               00510002
          05 WS-OPTION          PIC 9(3).                               00520002
          05 WS-NUMB            PIC 9(16).                              00530002
          05 WS-CUST-ID         PIC 9(18).                              00540002
      *                                                                 00550002
       01 WS-COMMAREA-CS05.                                             00560003
          05 WS-FIRST-SEND      PIC X(1).                               00570002
          05 WS-OPTION          PIC 9(3).                               00580002
          05 WS-NUMB            PIC 9(16).                              00590002
      *                                                                 00600016
       01 WS-COMMAREA-CS07.                                             00601016
          05 WS-FIRST-SEND      PIC X(1).                               00602016
          05 WS-OPTION          PIC 9(3).                               00603016
          05 WS-NUMB            PIC 9(16).                              00604016
          05 WS-CUST-ID         PIC 9(18).                              00605016
      *                                                                 00610002
       01 WS-COMMAREA-CS09.                                             00620003
          05 WS-FIRST-SEND      PIC X(1).                               00630002
          05 WS-OPTION          PIC 9(3).                               00640002
          05 WS-NUMB            PIC 9(16).                              00650002
          05 WS-CUST-ID         PIC 9(18).                              00660002
       77 WS-END                PIC X(17) VALUE 'CIOCCS08 WS END'.      00670002
      *                                                                 00680002
       LINKAGE SECTION.                                                 00690002
       01 DFHCOMMAREA.                                                  00700002
          05 LK-FIRST-SEND      PIC X(1).                               00710002
          05 LK-OPTION          PIC 9(3).                               00720002
          05 LK-NUMB            PIC 9(16).                              00730002
          05 LK-CUST-ID         PIC 9(18).                              00740002
      *                                                                 00750002
       PROCEDURE DIVISION.                                              00760002
       0000-MAINLINE.                                                   00770002
      *                                                                 00780002
            PERFORM 1000-INIT                                           00790002
               THRU 1000-INIT-EXIT                                      00800002
      *                                                                 00810002
            PERFORM 2000-PRE-PROCESSING                                 00820002
               THRU 2000-PRE-PROCESSING-EXIT                            00830002
      *                                                                 00840002
            PERFORM 3000-MAIN-PROCESS                                   00850002
               THRU 3000-MAIN-PROCESS-EXIT                              00860002
      *                                                                 00870002
            PERFORM 4000-POST-PROCESSING                                00880002
               THRU 4000-POST-PROCESSING-EXIT                           00890002
      *                                                                 00900002
            PERFORM 5000-CLEAN-UP                                       00910002
               THRU 5000-CLEAN-UP-EXIT                                  00920002
            .                                                           00930002
      *                                                                 00940002
       0000-EXIT.                                                       00950002
            EXIT.                                                       00960002
      *                                                                 00970002
       1000-INIT.                                                       00980002
            IF LK-FIRST-SEND = 'Y'                                      00990002
               MOVE LOW-VALUES TO CICS08O                               01000002
      *        PERFORM 1020-GET-EXISTING-CUST                           01010002
      *           THRU 1020-GET-EXISTING-CUST-EXIT                      01020002
               SET WS-MAP-ERASE TO TRUE                                 01030002
               PERFORM 3030-SEND-MAP                                    01040002
                  THRU 3030-SEND-MAP-EXIT                               01050002
      * NOT FIRST SHOW                                                  01060002
            ELSE                                                        01070002
                  MOVE LOW-VALUES TO CICS08I                            01080002
                  EXEC CICS RECEIVE MAP('CICS08')                       01090002
                                   MAPSET('CICS08')                     01100002
                                   INTO(CICS08I)                        01110002
                                   RESP(WS-RESP-CODE)                   01120002
                  END-EXEC                                              01130002
            END-IF                                                      01140002
            .                                                           01150002
       1000-INIT-EXIT.                                                  01160002
            EXIT.                                                       01170002
      *                                                                 01180002
       1010-ASK-TIME-DATE.                                              01190002
      *                                                                 01200002
            EXEC CICS                                                   01210002
                 ASKTIME                                                01220002
                 ABSTIME(WS-GETTIME)                                    01230002
            END-EXEC                                                    01240002
            EXEC CICS                                                   01250002
                 FORMATTIME                                             01260002
                 ABSTIME(WS-GETTIME)                                    01270002
                 DATESEP('/')                                           01280002
                 YYYYMMDD(WS-DATEOUT)                                   01290002
            END-EXEC                                                    01300002
            EXEC CICS                                                   01310002
                 FORMATTIME                                             01320002
                 ABSTIME(WS-GETTIME)                                    01330002
                 TIMESEP                                                01340002
                 TIME(WS-TIMEOUT)                                       01350002
            END-EXEC                                                    01360002
            MOVE WS-DATEOUT TO SYSDO                                    01370002
            MOVE WS-TIMEOUT TO SYSTO                                    01380002
            .                                                           01390002
      *                                                                 01400002
       1010-ASK-TIME-DATE-EXIT.                                         01410002
            EXIT.                                                       01420002
      *                                                                 01430002
       1020-GET-EXISTING-CARD.                                          01440019
            INITIALIZE SDCA-SERVICE-COMMAREA                            01450002
            MOVE 'VBS.CI.CREDCARD.INQ' TO SD-SRV-NAME                   01460011
            INITIALIZE CIC0015I-REC                                     01470011
            MOVE  LK-NUMB TO CIC0015I-NUMB                              01480011
            MOVE CIC0015I-REC TO SD-SRV-INPUT-DATA                      01490011
            EXEC CICS                                                   01500002
                 LINK                                                   01510002
                 PROGRAM(WS-PGM-SRV-DRIVER)                             01520002
                 COMMAREA(WS-SRV-COMMAREA)                              01530002
                 RESP(WS-RESP-CODE)                                     01540002
            END-EXEC                                                    01550002
            EVALUATE WS-RESP-CODE                                       01560002
                WHEN DFHRESP(NORMAL)                                    01570002
                     IF SD-RESP-CODE EQUAL ZEROS                        01580002
                        INITIALIZE CIC0015O-REC                         01590011
                        MOVE SD-SRV-OUTPUT-DATA TO CIC0015O-REC         01600011
                     ELSE                                               01610011
                        MOVE 'CUSTOMER READ ERROR' TO MSGO              01611011
                        SET WS-MAP-DATAONLY TO TRUE                     01612011
                        PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT   01613011
                     END-IF                                             01614011
                WHEN OTHER                                              01615011
                     MOVE 'SERVICE PROGRAM ERROR' TO MSGO               01616011
                     SET WS-MAP-DATAONLY TO TRUE                        01617011
                     PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT      01618011
            END-EVALUATE                                                01619011
                PERFORM 3020-UPDATE-CARD                                01619115
                        THRU 3020-UPDATE-CARD-EXIT                      01619215
            .                                                           01620002
      *                                                                 01630002
       1020-GET-EXISTING-CARD-EXIT.                                     01640019
            EXIT.                                                       01650002
      *                                                                 01650111
      *                                                                 01662002
       2000-PRE-PROCESSING.                                             01670002
      *                                                                 01680002
       2000-PRE-PROCESSING-EXIT.                                        01690002
            EXIT.                                                       01700002
      *                                                                 01710002
       3000-MAIN-PROCESS.                                               01720002
            EVALUATE EIBAID ALSO TRUE                                   01730002
                WHEN DFHPF1 ALSO LK-OPTION = 001                        01740016
                     MOVE 'Y' TO WS-FIRST-SEND OF WS-COMMAREA-CS07      01750017
                     MOVE 001 TO WS-OPTION OF WS-COMMAREA-CS07          01760017
                     MOVE LK-NUMB TO WS-NUMB OF WS-COMMAREA-CS07        01770017
                     MOVE LK-CUST-ID TO WS-CUST-ID OF WS-COMMAREA-CS07  01780017
                     EXEC CICS                                          01790002
                          XCTL PROGRAM('CIOCCS07')                      01800016
                           COMMAREA(WS-COMMAREA-CS07)                   01810016
                               RESP(WS-RESP-CODE)                       01820002
                     END-EXEC                                           01830002
                     IF WS-RESP-CODE NOT = DFHRESP(NORMAL)              01840002
                        MOVE 'PROGRAM CIOCCI07 IS NOT AVAILABLE'        01850016
                                TO MSGO                                 01860002
                        SET WS-MAP-DATAONLY TO TRUE                     01870002
                        PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT   01880002
                     END-IF                                             01890002
                WHEN DFHPF1 ALSO LK-OPTION = 002                        01891016
                     MOVE 'Y' TO WS-FIRST-SEND OF WS-COMMAREA-CS05      01892017
                     MOVE 003 TO WS-OPTION OF WS-COMMAREA-CS05          01893017
                     MOVE LK-NUMB TO WS-NUMB OF WS-COMMAREA-CS05        01894017
                     EXEC CICS                                          01895016
                          XCTL PROGRAM('CIOCCS05')                      01896016
                           COMMAREA(WS-COMMAREA-CS05)                   01897016
                               RESP(WS-RESP-CODE)                       01898016
                     END-EXEC                                           01899016
                     IF WS-RESP-CODE NOT = DFHRESP(NORMAL)              01899116
                        MOVE 'PROGRAM CIOCCS06 IS NOT AVAILABLE'        01899216
                                TO MSGO                                 01899316
                        SET WS-MAP-DATAONLY TO TRUE                     01899416
                        PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT   01899516
                     END-IF                                             01899616
                WHEN DFHPF3 ALSO ANY                                    01900002
                     MOVE 'THANK YOU FOR USING THE SYSTEM' TO WS-MESSAGE01910002
                     EXEC CICS                                          01920002
                          SEND CONTROL                                  01930002
                          CURSOR                                        01940002
                          ERASE                                         01950002
                          FREEKB                                        01960002
                          ALARM                                         01970002
                     END-EXEC                                           01980002
                     EXEC CICS                                          01990002
                          SEND FROM(WS-MESSAGE)                         02000002
                     END-EXEC                                           02010002
                     PERFORM 5010-RETURN THRU 5010-RETURN-EXIT          02020002
                WHEN DFHCLEAR ALSO ANY                                  02030002
                     EXEC CICS                                          02040002
                           SEND CONTROL                                 02050002
                           CURSOR                                       02060002
                           ERASE                                        02070002
                           FREEKB                                       02080002
                           ALARM                                        02090002
                     END-EXEC                                           02100002
                     PERFORM 5010-RETURN THRU 5010-RETURN-EXIT          02110002
                 WHEN DFHENTER ALSO LK-OPTION = 001                     02111014
                      IF WS-RESP-CODE NOT EQUAL DFHRESP(NORMAL)         02112014
                         MOVE 'INVALID REQUEST, CHECK YOUR INPUT.'      02113014
                              TO MSGO                                   02114014
                         SET WS-MAP-DATAONLY TO TRUE                    02115014
                         PERFORM 3030-SEND-MAP                          02116014
                            THRU 3030-SEND-MAP-EXIT                     02117014
                      ELSE                                              02118014
                         PERFORM 3010-CHECK-INPUT                       02119014
                            THRU 3010-CHECK-INPUT-EXIT                  02119114
                         PERFORM 1020-GET-EXISTING-CARD                 02119219
                            THRU 1020-GET-EXISTING-CARD-EXIT            02119319
                      END-IF                                            02119414
                WHEN DFHENTER ALSO LK-OPTION = 002                      02120009
                     INITIALIZE WS-COMMAREA                             02130002
                     MOVE 'Y' TO WS-FIRST-SEND OF WS-COMMAREA-CS09      02140003
                     MOVE '001' TO WS-OPTION OF WS-COMMAREA-CS09        02150014
                     MOVE LK-NUMB TO WS-NUMB OF WS-COMMAREA-CS09        02160003
                     MOVE LK-CUST-ID TO WS-CUST-ID OF                   02170002
                          WS-COMMAREA-CS09                              02180003
                     EXEC CICS                                          02190002
                          XCTL PROGRAM('CIOCCS09')                      02200003
                               RESP(WS-RESP-CODE)                       02210002
                               COMMAREA(WS-COMMAREA-CS09)               02220003
                     END-EXEC                                           02230002
                     IF WS-RESP-CODE NOT = DFHRESP(NORMAL)              02240002
                        MOVE 'PROGRAM CIOCCS09 IS NOT AVAILABLE'        02250007
                                TO MSGO                                 02260002
                        SET WS-MAP-DATAONLY TO TRUE                     02270002
                        PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT   02280002
                     END-IF                                             02290002
                 WHEN OTHER                                             02430002
                      MOVE 'INVALID KEY PRESSED!' TO MSGO               02440002
                      SET WS-MAP-DATAONLY TO TRUE                       02450002
                      PERFORM 3030-SEND-MAP                             02460002
                         THRU 3030-SEND-MAP-EXIT                        02470002
            END-EVALUATE                                                02480002
            .                                                           02490002
       3000-MAIN-PROCESS-EXIT.                                          02500002
            EXIT.                                                       02510002
      *                                                                 02520002
       3010-CHECK-INPUT.                                                02530002
            IF COMMUL NOT = 0                                           02540002
               INITIALIZE CIMENU-REC                                    02550002
               MOVE COMMUI TO CIMENU-TRANSID                            02560002
               EXEC CICS READ                                           02570002
                    FILE('CIMENU')                                      02580002
                    INTO(CIMENU-REC)                                    02590002
                    RIDFLD(CIMENU-TRANSID)                              02600002
                    RESP(WS-RESP-CODE)                                  02610002
               END-EXEC                                                 02620002
               EVALUATE WS-RESP-CODE                                    02630002
                   WHEN DFHRESP(NORMAL)                                 02640002
                        EXEC CICS                                       02650002
                             XCTL PROGRAM(CIMENU-PGM)                   02660002
                             COMMAREA(WS-COMMAREA)                      02670002
                             RESP(WS-RESP-CODE)                         02680002
                        END-EXEC                                        02690002
                        IF WS-RESP-CODE NOT = DFHRESP(NORMAL)           02700002
                        STRING 'PROGRAM ' DELIMITED BY SIZE             02710002
                               CIMENU-PGM DELIMITED BY SPACE            02720002
                               ' IS NOT AVAILABLE' DELIMITED BY SIZE    02730002
                               INTO MSGO                                02740002
                           SET WS-MAP-DATAONLY TO TRUE                  02750002
                           PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT02760002
                        END-IF                                          02770002
                   WHEN DFHRESP(NOTFND)                                 02780002
                        MOVE 'INVALID TRANSATION ID!' TO MSGO           02790002
                        SET WS-MAP-DATAONLY TO TRUE                     02800002
                        PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT   02810002
                   WHEN OTHER                                           02820002
                        MOVE 'CIMENU FILE ERROR!' TO MSGO               02830002
                        SET WS-MAP-DATAONLY TO TRUE                     02840002
                        PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT   02850002
               END-EVALUATE                                             02860002
            END-IF                                                      02870002
                IF (PWD1L NOT = 6 OR PWD2L NOT = 6)                     02890008
                   MOVE 'THE PASSWORD LENGTH MUST BE 6' TO MSGO         02900008
                   SET WS-MAP-DATAONLY TO TRUE                          02910008
                   PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT        02920008
                END-IF                                                  02930008
                IF (PWD1I IS NOT NUMERIC OR PWD2I IS NOT NUMERIC)       02931014
                   MOVE 'THE PASSWORD MUST BE NUMBER!' TO MSGO          02932011
                   SET WS-MAP-DATAONLY TO TRUE                          02933011
                   PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT        02934011
                END-IF                                                  02935011
                IF (PWD1I NOT = PWD2I)                                  02940008
                   MOVE 'THE TWO INPUT PASSWORD DOES NOT MATCH' TO MSGO 02950008
                   SET WS-MAP-DATAONLY TO TRUE                          02960008
                   PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT        02970008
                END-IF                                                  02980008
            .                                                           02986008
      *                                                                 02990002
       3010-CHECK-INPUT-EXIT.                                           03000002
            EXIT.                                                       03010002
      *                                                                 03020002
       3020-UPDATE-CARD.                                                03021014
            INITIALIZE SDCA-SERVICE-COMMAREA                            03022012
            MOVE 'VBS.CI.CREDCARD.UPD' TO SD-SRV-NAME                   03023014
            INITIALIZE CIC0014I-REC                                     03024012
            MOVE CIC0015O-REC TO CIC0014I-REC                           03025012
            MOVE PWD1I TO CIC0014I-TRNPWD                               03026012
             PERFORM 1010-ASK-TIME-DATE                                 03026120
                THRU 1010-ASK-TIME-DATE-EXIT                            03026220
            MOVE WS-DATEOUT TO CIC0014I-TRNPWD-LAST-DATE                03026318
            MOVE CIC0014I-REC  TO SD-SRV-INPUT-DATA                     03027012
                 EXEC CICS                                              03028119
                 LINK                                                   03029012
                 PROGRAM(WS-PGM-SRV-DRIVER)                             03029112
                 COMMAREA(WS-SRV-COMMAREA)                              03029212
                 RESP(WS-RESP-CODE)                                     03029312
            END-EXEC                                                    03029412
            EVALUATE WS-RESP-CODE                                       03029512
                WHEN DFHRESP(NORMAL)                                    03029612
                     IF SD-RESP-CODE EQUAL ZEROS                        03029712
                        MOVE 'TRANSACTION PASSWORD SET SUCCESSFULLY'    03029814
                          TO MSGO                                       03029912
                        MOVE 002 TO LK-OPTION                           03030014
                     ELSE                                               03030212
                        MOVE SD-RESP-ADDITIONAL TO MSGO                 03030312
                     END-IF                                             03030412
                WHEN OTHER                                              03030512
                     MOVE 'SERVICE PROGRAM ERROR' TO MSGO               03030612
            END-EVALUATE                                                03030712
            SET WS-MAP-DATAONLY TO TRUE                                 03030812
            PERFORM 3030-SEND-MAP                                       03030912
               THRU 3030-SEND-MAP-EXIT                                  03031014
            .                                                           03031114
      *                                                                 03031212
       3020-UPDATE-CARD-EXIT.                                           03031314
                  EXIT.                                                 03031412
      *                                                                 03032002
       3030-SEND-MAP.                                                   03040002
            IF LK-OPTION = 001                                          03050014
               MOVE 'PF1=RETURN PF3=EXIT ENTER=PROCESS' TO KEYO         03080002
            ELSE                                                        03081014
               MOVE 'PF1=RETURN PF3=EXIT ENTER=SET PASSWORD NEED'       03082014
                    TO KEYO                                             03083014
            END-IF                                                      03090014
            PERFORM 1010-ASK-TIME-DATE                                  03100002
               THRU 1010-ASK-TIME-DATE-EXIT                             03110002
            EVALUATE TRUE                                               03120002
                WHEN WS-MAP-ERASE                                       03130002
                     EXEC CICS SEND                                     03140002
                          MAP('CICS08')                                 03150002
                          MAPSET('CICS08')                              03160002
                          FROM(CICS08O)                                 03170002
                          ERASE                                         03180002
                     END-EXEC                                           03190002
                WHEN WS-MAP-DATAONLY                                    03200002
                     EXEC CICS SEND                                     03210002
                          MAP('CICS08')                                 03220002
                          MAPSET('CICS08')                              03230002
                          FROM(CICS08O)                                 03240002
                          DATAONLY                                      03250002
                     END-EXEC                                           03260002
            END-EVALUATE                                                03270002
      *     MOVE '1' TO WS-ENTER-FLAG                                   03280002
            PERFORM 5020-RETURN-TRANS THRU 5020-RETURN-TRANS-EXIT       03290002
            .                                                           03300002
      *                                                                 03310002
       3030-SEND-MAP-EXIT.                                              03320002
            EXIT.                                                       03330002
      *                                                                 03340002
       4000-POST-PROCESSING.                                            03350002
      *                                                                 03360002
       4000-POST-PROCESSING-EXIT.                                       03370002
            EXIT.                                                       03380002
      *                                                                 03390002
       5000-CLEAN-UP.                                                   03400002
            PERFORM 5010-RETURN                                         03410002
               THRU 5010-RETURN-EXIT                                    03420002
            .                                                           03430002
      *                                                                 03440002
       5000-CLEAN-UP-EXIT.                                              03450002
            EXIT.                                                       03460002
      *                                                                 03470002
       5010-RETURN.                                                     03480002
            EXEC CICS RETURN END-EXEC                                   03490002
            .                                                           03500002
       5010-RETURN-EXIT.                                                03510002
            EXIT.                                                       03520002
      *                                                                 03530002
       5020-RETURN-TRANS.                                               03540002
            INITIALIZE WS-COMMAREA                                      03550002
            MOVE 'N' TO WS-FIRST-SEND OF WS-COMMAREA                    03560002
            MOVE LK-OPTION TO WS-OPTION OF WS-COMMAREA                  03570002
            MOVE LK-NUMB TO WS-NUMB OF WS-COMMAREA                      03580002
            MOVE LK-CUST-ID TO WS-CUST-ID OF WS-COMMAREA                03590002
            EXEC CICS RETURN TRANSID('CIB8')                            03600003
                      COMMAREA(WS-COMMAREA)                             03610002
            END-EXEC                                                    03620002
            .                                                           03630002
       5020-RETURN-TRANS-EXIT.                                          03640002
            EXIT.                                                       03650002
