       IDENTIFICATION DIVISION.                                         00010000
       PROGRAM-ID. CIOH0013.                                            00020048
      ***************************************************************** 00021048
      * CIOH0013 - SERVICE DEMO                                         00022048
      *            HOST PROGRAM                                         00023048
      * DEMO HOST PROGRAM WHICH WILL BE CALLED BY DRIVER PROGRAM        00023148
      *                                                                 00023248
      *  CD-RESP-CODE    CD-RESP-ADDITIONAL
      *     '0000'       'APPLICATION ADDED SUCCESSFULLY'
      *     '1000'       'APPLICATION EXISTED'
      *     '2000'       'APPLICATION FILE NOT OPEN'
      *     '3000'       'INVALID REQUEST'
      *     '4000'       'APPLICATION FILE NOT FOUND'
      *     '9000'       'APPLICATION ADDED FAILED'
      ***************************************************************** 00024048
      *                         VERSION HISTORY                         00024148
      *---------------------------------------------------------------- 00024248
      *DATE/TIME ³   AUTHOR   ³ DESCRIPTION                             00024348
      *---------------------------------------------------------------- 00024448
      *2015-02-05    KEVIN      INITIAL VERSION                         00024548
      ***************************************************************** 00025048
       ENVIRONMENT DIVISION.                                            00030000
       DATA DIVISION.                                                   00040000
       WORKING-STORAGE SECTION.                                         00050000
      *                                                                 00050148
       77 WS-BEGIN              PIC X(17) VALUE 'CIOH0013 WS BEGIN'.    00051048
       01 WS-VAR.
          05 WS-RESP-CODE       PIC S9(8) COMP.
          05 WS-GETTIME         PIC X(20).
          05 WS-DATEOUT         PIC X(10).
          05 WS-TIMEOUT         PIC X(8).
          05 WS-CARDNUMB        PIC 9(16).
          05 WS-CARDNUMB-X REDEFINES WS-CARDNUMB PIC X(16).
      *      10 WS-CARD-FIRST   PIC X(6).
      *      10 WS-CARD-TYPE    PIC X(3).
      *      10 WS-CARD-THIRD   PIC X(7).
      *
      *APPLICATION FILE CICARD
       COPY CICAD001.
      *
      *SERVICE I/O
       COPY CIC0013I.
       COPY CIC0013O.
      *                                                                 00052048
       77 WS-END                PIC X(15) VALUE 'CIOH0013 WS END'.      00128048
      *
       LINKAGE SECTION.                                                 00128100
       01 DFHCOMMAREA.                                                  00128200
      *SERVICE REQUEST/RESPONSE COMMAREA                                00128348
       COPY SD02WS.                                                     00128448
      *
       PROCEDURE DIVISION.                                              00129000
       0000-MAINLINE.                                                   00130048
      *                                                                 00131048
            PERFORM 1000-INIT                                           00132048
               THRU 1000-INIT-EXIT                                      00133048
      *                                                                 00134048
            PERFORM 2000-PRE-PROCESSING                                 00135048
               THRU 2000-PRE-PROCESSING-EXIT                            00136048
      *                                                                 00137048
            PERFORM 3000-MAIN-PROCESS                                   00138048
               THRU 3000-MAIN-PROCESS-EXIT                              00139048
      *                                                                 00139148
            PERFORM 4000-POST-PROCESSING                                00139248
               THRU 4000-POST-PROCESSING-EXIT                           00139348
      *                                                                 00139448
            PERFORM 5000-CLEAN-UP                                       00139548
               THRU 5000-CLEAN-UP-EXIT                                  00139648
            .
      *                                                                 00139748
       0000-EXIT.                                                       00140048
            EXIT.
      *
       1000-INIT.
            INITIALIZE CIC0013I-REC
            MOVE CD-SRV-INPUT-DATA   TO CIC0013I-REC
            .
       1000-INIT-EXIT.
            EXIT.
      *
       1010-ASK-TIME-DATE.
      *
            EXEC CICS
                 ASKTIME
                 ABSTIME(WS-GETTIME)
            END-EXEC
            EXEC CICS
                 FORMATTIME
                 ABSTIME(WS-GETTIME)
                 DATESEP('-')
                 YYYYMMDD(WS-DATEOUT)
            END-EXEC
            EXEC CICS
                 FORMATTIME
                 ABSTIME(WS-GETTIME)
                 TIMESEP
                 TIME(WS-TIMEOUT)
            END-EXEC
            .
      *
       1010-ASK-TIME-DATE-EXIT.
            EXIT.
      *
       1020-GET-CARDID.
            MOVE HIGH-VALUES TO WS-CARDNUMB-X
            EXEC CICS STARTBR
                 FILE('CICARD')
                 RIDFLD(WS-CARDNUMB-X)
                 RESP(WS-RESP-CODE)
            END-EXEC
            EVALUATE WS-RESP-CODE
                WHEN DFHRESP(NORMAL)
                     INITIALIZE CICAD-REC
                     EXEC CICS READPREV
                          FILE('CICARD')
                          RIDFLD(WS-CARDNUMB-X)
                          INTO(CICAD-REC)
                          RESP(WS-RESP-CODE)
                     END-EXEC
                     IF WS-RESP-CODE = DFHRESP(NORMAL)
                        ADD 1 TO CICAD-NUMB
                        MOVE CICAD-NUMB TO WS-CARDNUMB
                     ELSE
                        MOVE '8000' TO CD-RESP-CODE
                        MOVE 'ERROR WHEN GET MAX CARD NUMBER'
                                 TO CD-RESP-ADDITIONAL
                        PERFORM 5000-CLEAN-UP
                           THRU 5000-CLEAN-UP-EXIT
                     END-IF
                WHEN OTHER
                     MOVE '7000' TO CD-RESP-CODE
                     MOVE 'ERROR WHEN GET MAX CARD NUMBER'
                                 TO CD-RESP-ADDITIONAL
                     PERFORM 5000-CLEAN-UP
                        THRU 5000-CLEAN-UP-EXIT
            END-EVALUATE
            EXEC CICS ENDBR
                 FILE('CICARD')
            END-EXEC
            .
       1020-GET-CARDID-EXIT.
            EXIT.
      *
       2000-PRE-PROCESSING.
            PERFORM 1010-ASK-TIME-DATE THRU
                    1010-ASK-TIME-DATE-EXIT
            PERFORM 1020-GET-CARDID THRU
                    1020-GET-CARDID-EXIT
      *
            INITIALIZE CICAD-REC
            MOVE    001                  TO CICAD-PROD-TYPE
            MOVE    WS-CARDNUMB          TO CICAD-NUMB
            MOVE    001                  TO CICAD-STATUS
            MOVE    WS-CARDNUMB          TO CICAD-ACCT-NUMB
            MOVE    CIC0013I-CUST-NUMB   TO CICAD-CUST-NUMB
            MOVE    WS-DATEOUT           TO CICAD-LAST-DATE
            MOVE    WS-DATEOUT           TO CICAD-OPEN-DATE
            MOVE    02                   TO CICAD-DISP-IND
            MOVE    '0001-01-01'         TO CICAD-DISP-SUBDATE
            MOVE    '0001-01-01'         TO CICAD-DISP-DATE
            MOVE    02                   TO CICAD-TYPE
            MOVE    01                   TO CICAD-PRISEC-IND
            MOVE    01                   TO CICAD-LOCK-IND
            MOVE    '0001-01-01'         TO CICAD-LOCK-DATE
            MOVE    '2020-01-01'         TO CICAD-CURR-EXPIRY-DATE
            MOVE    02                   TO CICAD-ACTIVATE-IND
            MOVE    '0001-01-01'         TO CICAD-ACTIVATE-DATE
            MOVE    01                   TO CICAD-CURR-ACTION
            MOVE    01                   TO CICAD-LAST-ACTION
            MOVE    1111                 TO CICAD-CVV
            MOVE    567                  TO CICAD-CVV2
            MOVE    CIC0013I-CUST-NAME   TO CICAD-CUST-NAME
            MOVE    02                   TO CICAD-TRNPWD-IND
            MOVE    000000               TO CICAD-TRNPWD
            MOVE    00                   TO CICAD-TRNPWD-WRGCNT
            MOVE    '0001-01-01'         TO CICAD-TRNPWD-WRGDATE
            MOVE    '00:00'              TO CICAD-TRNPWD-WRGTIME
            MOVE    '0001-01-01'         TO CICAD-TRNPWD-LAST-DATE
            .
      *
       2000-PRE-PROCESSING-EXIT.
            EXIT.
      *
       3000-MAIN-PROCESS.                                               00138048
            EXEC CICS WRITE
                 FILE('CICARD')
                 FROM(CICAD-REC)
                 RIDFLD(CICAD-NUMB)
                 RESP(WS-RESP-CODE)
            END-EXEC
            EVALUATE (WS-RESP-CODE)
                WHEN DFHRESP(NORMAL)
                     MOVE '0000' TO CD-RESP-CODE
                     MOVE 'CARD NUMBER ADDED SUCCESSFULLY'
                                 TO CD-RESP-ADDITIONAL
                     INITIALIZE CIC0013O-REC
                     MOVE CICAD-NUMB TO CIC0013O-NUMB
                     MOVE CICAD-ACCT-NUMB TO CIC0013O-ACCT-NUMB
                     INITIALIZE CD-SRV-OUTPUT-DATA
                     MOVE CIC0013O-REC TO CD-SRV-OUTPUT-DATA
                WHEN DFHRESP(DUPREC)
                     MOVE '1000' TO CD-RESP-CODE
                     MOVE 'CARD NUMBER EXISTED'
                                 TO CD-RESP-ADDITIONAL
                WHEN DFHRESP(NOTOPEN)
                     MOVE '2000' TO CD-RESP-CODE
                     MOVE 'CARD NUMBER FILE NOT OPEN'
                                 TO CD-RESP-ADDITIONAL
                WHEN DFHRESP(INVREQ)
                     MOVE '3000' TO CD-RESP-CODE
                     MOVE 'INVALID REQUEST'
                                 TO CD-RESP-ADDITIONAL
                WHEN DFHRESP(FILENOTFOUND)
                     MOVE '4000' TO CD-RESP-CODE
                     MOVE 'CARD NUMBER FILE NOT FOUND'
                                 TO CD-RESP-ADDITIONAL
                WHEN OTHER
                     MOVE '9000' TO CD-RESP-CODE
                     MOVE 'CARD NUMBER ADDED FAILED'
                                 TO CD-RESP-ADDITIONAL
            END-EVALUATE
            .
       3000-MAIN-PROCESS-EXIT.                                          00139048
            EXIT.                                                       00151021
      *                                                                 00152019
       4000-POST-PROCESSING.
      *
       4000-POST-PROCESSING-EXIT.
            EXIT.                                                       00151021
      *
       5000-CLEAN-UP.
            EXEC CICS RETURN END-EXEC.
      *
       5000-CLEAN-UP-EXIT.
            EXIT.                                                       00151021
      *
