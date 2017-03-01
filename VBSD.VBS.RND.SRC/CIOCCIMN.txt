       IDENTIFICATION DIVISION.                                         00010000
       PROGRAM-ID. CIOCCIMN.                                            00020097
      ***************************************************************** 00030048
      * CIOCCIMN - CLIENT PROGRAM                                       00040092
      *                                                                 00050092
      * CREDIT ISSUANCE MAIN MENU                                       00060092
      *                                                                 00070048
      ***************************************************************** 00080048
      *                         VERSION HISTORY                         00090048
      *---------------------------------------------------------------- 00100048
      *DATE/TIME ³   AUTHOR   ³ DESCRIPTION                             00110048
      *---------------------------------------------------------------- 00120048
      *2015-01-06    KEVIN      INITIAL VERSION                         00130092
      ***************************************************************** 00150048
       ENVIRONMENT DIVISION.                                            00160000
       DATA DIVISION.                                                   00170000
       WORKING-STORAGE SECTION.                                         00180000
      *                                                                 00190048
       77 WS-BEGIN              PIC X(17) VALUE 'CIOCCIMN WS BEGIN'.    00200097
       01 WS-VAR.                                                       00210082
          05 WS-GETTIME         PIC X(20).                              00220000
          05 WS-DATEOUT         PIC X(10).                              00230000
          05 WS-TIMEOUT         PIC X(8).                               00240000
          05 WS-RESP-CODE       PIC S9(8) COMP.                         00250082
          05 WS-MESSAGE         PIC X(40).                              00260082
          05 WS-ENTER-FLAG      PIC X(1).                               00270082
          05 WS-TRANSID         PIC X(4).                               00271093
       01 WS-MAP-OPTION         PIC X(1).                               00280082
          88 WS-MAP-ERASE       VALUE '0'.                              00290082
          88 WS-MAP-DATAONLY    VALUE '1'.                              00300082
      *                                                                 00310048
      *SCREEN HANDLER                                                   00320048
       COPY SD11WS.                                                     00330048
      * SYMBOLIC MAP                                                    00340048
       COPY CIMN00.                                                     00350097
      *MAP CONTROL                                                      00360048
       COPY DFHBMSCA.                                                   00370017
      *CICS FUNCTION KEYS                                               00380048
       COPY DFHAID.                                                     00390000
      *CIMENU                                                           00400093
       COPY CIMENU.                                                     00420094
      *                                                                 00421093
       01 WS-SRV-COMMAREA.                                              00430082
      *SERVICE REQUEST/RESPONSE COMMAREA                                00440048
       COPY SD01WS.                                                     00450048
       77 WS-END                PIC X(15) VALUE 'CIOCCIMN WS END'.      00460097
      *                                                                 00470082
       LINKAGE SECTION.                                                 00480000
       01 DFHCOMMAREA.                                                  00490000
      *COMMON CICS SCREEN HANDLE VARIABLES                              00500048
       COPY SD00WS.                                                     00510048
      *                                                                 00520082
       PROCEDURE DIVISION.                                              00530000
       0000-MAINLINE.                                                   00540048
      *                                                                 00550048
            PERFORM 1000-INIT                                           00560048
               THRU 1000-INIT-EXIT                                      00570048
      *                                                                 00580048
            PERFORM 2000-PRE-PROCESSING                                 00590048
               THRU 2000-PRE-PROCESSING-EXIT                            00600048
      *                                                                 00610048
            PERFORM 3000-MAIN-PROCESS                                   00620048
               THRU 3000-MAIN-PROCESS-EXIT                              00630048
      *                                                                 00640048
            PERFORM 4000-POST-PROCESSING                                00650048
               THRU 4000-POST-PROCESSING-EXIT                           00660048
      *                                                                 00670048
            PERFORM 5000-CLEAN-UP                                       00680048
               THRU 5000-CLEAN-UP-EXIT                                  00690048
            .                                                           00700082
      *                                                                 00710048
       0000-EXIT.                                                       00720048
            EXIT.                                                       00730082
      *                                                                 00740082
       1000-INIT.                                                       00750082
            IF EIBCALEN = 0                                             00760082
               MOVE LOW-VALUES TO CIMN00O                               00770092
               SET WS-MAP-ERASE TO TRUE                                 00780082
               PERFORM 3030-SEND-MAP                                    00790082
                  THRU 3030-SEND-MAP-EXIT                               00800082
      * NOT FIRST SHOW                                                  00810082
            ELSE                                                        00820082
               IF SDCA-CICS-SECONDENTER                                 00830082
                  MOVE LOW-VALUES TO CIMN00I                            00840092
                  EXEC CICS RECEIVE MAP('CIMN00')                       00850097
                                   MAPSET('CIMN00')                     00860097
                                   INTO(CIMN00I)                        00870097
                                   RESP(WS-RESP-CODE)                   00880082
                  END-EXEC                                              00890082
               END-IF                                                   00900082
            END-IF                                                      00910082
            .                                                           00920082
       1000-INIT-EXIT.                                                  00930082
            EXIT.                                                       00940082
      *                                                                 00950082
       1010-ASK-TIME-DATE.                                              00960000
      *                                                                 00970082
            EXEC CICS                                                   00980000
                 ASKTIME                                                00990000
                 ABSTIME(WS-GETTIME)                                    01000000
            END-EXEC                                                    01010000
            EXEC CICS                                                   01020000
                 FORMATTIME                                             01030000
                 ABSTIME(WS-GETTIME)                                    01040000
                 DATESEP('/')                                           01050000
                 YYYYMMDD(WS-DATEOUT)                                   01060000
            END-EXEC                                                    01070000
            EXEC CICS                                                   01080000
                 FORMATTIME                                             01090000
                 ABSTIME(WS-GETTIME)                                    01100000
                 TIMESEP                                                01110010
                 TIME(WS-TIMEOUT)                                       01120020
            END-EXEC                                                    01130030
            MOVE WS-DATEOUT TO SYSDO                                    01140040
            MOVE WS-TIMEOUT TO SYSTO                                    01150050
            .                                                           01160082
      *                                                                 01170082
       1010-ASK-TIME-DATE-EXIT.                                         01180000
            EXIT.                                                       01190082
      *                                                                 01200082
       2000-PRE-PROCESSING.                                             01210082
      *                                                                 01220082
       2000-PRE-PROCESSING-EXIT.                                        01230082
            EXIT.                                                       01240082
      *                                                                 01250082
       3000-MAIN-PROCESS.                                               01260048
            EVALUATE EIBAID                                             01270082
                WHEN DFHPF3                                             01280082
                     MOVE 'THANK YOU FOR USING THE SYSTEM' TO WS-MESSAGE01290082
                     EXEC CICS                                          01300082
                          SEND CONTROL                                  01310082
                          CURSOR                                        01320082
                          ERASE                                         01330082
                          FREEKB                                        01340082
                          ALARM                                         01350082
                     END-EXEC                                           01360082
                     EXEC CICS                                          01370082
                          SEND FROM(WS-MESSAGE)                         01380082
                     END-EXEC                                           01390082
                     PERFORM 5010-RETURN THRU 5010-RETURN-EXIT          01400082
                WHEN DFHCLEAR                                           01410082
                     EXEC CICS                                          01420082
                           SEND CONTROL                                 01430082
                           CURSOR                                       01440082
                           ERASE                                        01450082
                           FREEKB                                       01460082
                           ALARM                                        01470082
                     END-EXEC                                           01480082
                     PERFORM 5010-RETURN THRU 5010-RETURN-EXIT          01490082
                WHEN DFHPF9                                             01500082
                     MOVE LOW-VALUES TO CIMN00O                         01510092
                     SET WS-MAP-ERASE TO TRUE                           01520082
                     PERFORM 3030-SEND-MAP                              01530082
                        THRU 3030-SEND-MAP-EXIT                         01540082
                 WHEN DFHENTER                                          01550082
                      IF WS-RESP-CODE NOT EQUAL DFHRESP(NORMAL)         01560082
                         MOVE 'INVALID REQUEST, CHECK YOUR INPUT.'      01570082
                              TO MSGO                                   01580082
                         SET WS-MAP-DATAONLY TO TRUE                    01590082
                         PERFORM 3030-SEND-MAP                          01600082
                            THRU 3030-SEND-MAP-EXIT                     01610082
                      ELSE                                              01620082
                         PERFORM 3010-CHECK-INPUT                       01630082
                            THRU 3010-CHECK-INPUT-EXIT                  01640082
                         PERFORM 3020-XCTL                              01650093
                            THRU 3020-XCTL-EXIT                         01660093
                      END-IF                                            01670082
                 WHEN OTHER                                             01680082
                      MOVE 'INVALID KEY PRESSED!' TO MSGO               01690082
                      SET WS-MAP-DATAONLY TO TRUE                       01700082
                      PERFORM 3030-SEND-MAP                             01710082
                         THRU 3030-SEND-MAP-EXIT                        01720082
            END-EVALUATE                                                01730082
            .                                                           01740082
       3000-MAIN-PROCESS-EXIT.                                          01750048
            EXIT.                                                       01760021
      *                                                                 01770019
       3010-CHECK-INPUT.                                                01780082
            INITIALIZE CIMENU-REC                                       01781093
            EVALUATE TRUE                                               01790093
                WHEN (COMMUL NOT = 0)                                   01800093
                     MOVE COMMUI TO CIMENU-TRANSID                      01810093
                WHEN (MENUL  NOT = 0)                                   01890093
                     MOVE MENUI  TO CIMENU-TRANSID                      01900093
                WHEN (OPT1L  NOT = 0 AND OPT1I = 'S')                   01910093
                     MOVE 'CICA' TO CIMENU-TRANSID                      01920093
                WHEN (OPT2L  NOT = 0 AND OPT2I = 'S')                   01930093
                     MOVE 'CICS' TO CIMENU-TRANSID                      01940093
                WHEN (OPT3L  NOT = 0 AND OPT3I = 'S')                   01941099
                     MOVE 'CICP' TO CIMENU-TRANSID                      01942099
                WHEN OTHER                                              01950093
                     MOVE 'INVALID INPUT' TO MSGO                       01960093
                     SET WS-MAP-DATAONLY TO TRUE                        01970093
                     PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT      01980093
            END-EVALUATE                                                01990093
            .                                                           02015083
      *                                                                 02020082
       3010-CHECK-INPUT-EXIT.                                           02030082
            EXIT.                                                       02040021
      *                                                                 02050019
       3020-XCTL.                                                       02060093
            EXEC CICS READ                                              02060193
                 FILE('CIMENU')                                         02060293
                 INTO(CIMENU-REC)                                       02060393
                 RIDFLD(CIMENU-TRANSID)                                 02060493
                 RESP(WS-RESP-CODE)                                     02060593
            END-EXEC                                                    02060693
            EVALUATE WS-RESP-CODE                                       02060793
                WHEN DFHRESP(NORMAL)                                    02060893
                     EXEC CICS                                          02060993
                          XCTL PROGRAM(CIMENU-PGM)                      02061093
                               COMMAREA(CIMENU-TRANSID)                 02061199
                               RESP(WS-RESP-CODE)                       02061298
                     END-EXEC                                           02061393
                     IF WS-RESP-CODE NOT = DFHRESP(NORMAL)              02061498
                        STRING 'PROGRAM ' DELIMITED BY SIZE             02061598
                               CIMENU-PGM DELIMITED BY SPACE            02061698
                               ' IS NOT AVAILABLE' DELIMITED BY SIZE    02061799
                               INTO MSGO                                02061898
                        SET WS-MAP-DATAONLY TO TRUE                     02061998
                        PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT   02062098
                     END-IF                                             02062198
                WHEN DFHRESP(NOTFND)                                    02062293
                     MOVE 'INVALID TRANSATION ID!' TO MSGO              02062393
                     SET WS-MAP-DATAONLY TO TRUE                        02062493
                     PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT      02062593
                WHEN OTHER                                              02062693
                     MOVE 'CIMENU FILE ERROR!' TO MSGO                  02062793
                     SET WS-MAP-DATAONLY TO TRUE                        02062893
                     PERFORM 3030-SEND-MAP THRU 3030-SEND-MAP-EXIT      02062993
            END-EVALUATE                                                02063093
            .                                                           02290082
      *                                                                 02300082
       3020-XCTL-EXIT.                                                  02310093
            EXIT.                                                       02320021
      *                                                                 02330019
       3030-SEND-MAP.                                                   02340082
            PERFORM 1010-ASK-TIME-DATE                                  02350082
               THRU 1010-ASK-TIME-DATE-EXIT                             02360082
            EVALUATE TRUE                                               02370083
                WHEN WS-MAP-ERASE                                       02380082
                     EXEC CICS SEND                                     02390082
                          MAP('CIMN00')                                 02400097
                          MAPSET('CIMN00')                              02410097
                          FROM(CIMN00O)                                 02420092
                          ERASE                                         02430082
                     END-EXEC                                           02440082
                WHEN WS-MAP-DATAONLY                                    02450082
                     EXEC CICS SEND                                     02460082
                          MAP('CIMN00')                                 02470097
                          MAPSET('CIMN00')                              02480097
                          FROM(CIMN00O)                                 02490092
                          DATAONLY                                      02500082
                     END-EXEC                                           02510082
            END-EVALUATE                                                02520082
            MOVE '1' TO WS-ENTER-FLAG                                   02530082
            PERFORM 5020-RETURN-TRANS THRU 5020-RETURN-TRANS-EXIT       02540082
            .                                                           02550083
      *                                                                 02560082
       3030-SEND-MAP-EXIT.                                              02570082
            EXIT.                                                       02580021
      *                                                                 02590082
       4000-POST-PROCESSING.                                            02600082
      *                                                                 02610082
       4000-POST-PROCESSING-EXIT.                                       02620082
            EXIT.                                                       02630082
      *                                                                 02640082
       5000-CLEAN-UP.                                                   02650082
            PERFORM 5010-RETURN                                         02660082
               THRU 5010-RETURN-EXIT                                    02670082
            .                                                           02680082
      *                                                                 02690082
       5000-CLEAN-UP-EXIT.                                              02700082
            EXIT.                                                       02710021
      *                                                                 02720082
       5010-RETURN.                                                     02730082
            EXEC CICS RETURN END-EXEC                                   02740082
            .                                                           02750082
       5010-RETURN-EXIT.                                                02760082
            EXIT.                                                       02770082
      *                                                                 02780082
       5020-RETURN-TRANS.                                               02790082
            EXEC CICS RETURN TRANSID('CIMN')                            02800097
                      COMMAREA(WS-ENTER-FLAG)                           02810082
            END-EXEC                                                    02820082
            .                                                           02830082
       5020-RETURN-TRANS-EXIT.                                          02840082
            EXIT.                                                       02850082
