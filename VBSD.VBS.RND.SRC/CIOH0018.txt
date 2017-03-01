       IDENTIFICATION DIVISION.                                         00010000
       PROGRAM-ID. CIOH0018.                                            00020048
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
       77 WS-BEGIN              PIC X(17) VALUE 'CIOH0018 WS BEGIN'.    00051048
       01 WS-VAR.
          05 WS-RESP-CODE       PIC S9(8) COMP.
          05 WS-GETTIME         PIC X(20).
          05 WS-DATEOUT         PIC X(10).
          05 WS-TIMEOUT         PIC X(8).
          05 WS-ACCTNUMB        PIC 9(16).
          05 WS-ACCTNUMB-X REDEFINES WS-ACCTNUMB PIC X(16).
      *
       COPY CIACT001.
      *
      *SERVICE I/O
       COPY CIC0018I.
       COPY CIC0018O.
      *                                                                 00052048
       77 WS-END                PIC X(15) VALUE 'CIOH0018 WS END'.      00128048
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
            INITIALIZE CIC0018I-REC
            MOVE CD-SRV-INPUT-DATA   TO CIC0018I-REC
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
       1020-GET-ACCTID.
            MOVE HIGH-VALUES TO WS-ACCTNUMB-X
            EXEC CICS STARTBR
                 FILE('CIACCT')
                 RIDFLD(WS-ACCTNUMB-X)
                 RESP(WS-RESP-CODE)
            END-EXEC
            EVALUATE WS-RESP-CODE
                WHEN DFHRESP(NORMAL)
                     INITIALIZE CIACT-REC
                     EXEC CICS READPREV
                          FILE('CIACCT')
                          RIDFLD(WS-ACCTNUMB-X)
                          INTO(CIACT-REC)
                          RESP(WS-RESP-CODE)
                     END-EXEC
                     IF WS-RESP-CODE = DFHRESP(NORMAL)
                        ADD 1 TO CIACT-NUMB
                        MOVE CIACT-NUMB TO WS-ACCTNUMB
                     ELSE
                        MOVE '8000' TO CD-RESP-CODE
                        MOVE 'ERROR WHEN GET MAX ACCT NUMBER'
                                 TO CD-RESP-ADDITIONAL
                        PERFORM 5000-CLEAN-UP
                           THRU 5000-CLEAN-UP-EXIT
                     END-IF
                WHEN OTHER
                     MOVE '7000' TO CD-RESP-CODE
                     MOVE 'ERROR WHEN GET MAX ACCT NUMBER'
                                 TO CD-RESP-ADDITIONAL
                     PERFORM 5000-CLEAN-UP
                        THRU 5000-CLEAN-UP-EXIT
            END-EVALUATE
            EXEC CICS ENDBR
                 FILE('CIACCT')
            END-EXEC
            .
       1020-GET-ACCTID-EXIT.
            EXIT.
      *
       2000-PRE-PROCESSING.
            PERFORM 1010-ASK-TIME-DATE THRU
                    1010-ASK-TIME-DATE-EXIT
            PERFORM 1020-GET-ACCTID THRU
                    1020-GET-ACCTID-EXIT
      *
            INITIALIZE CIACT-REC
            MOVE WS-ACCTNUMB             TO CIACT-NUMB
            MOVE 001                     TO CIACT-STATUS
            .
      *
       2000-PRE-PROCESSING-EXIT.
            EXIT.
      *
       3000-MAIN-PROCESS.                                               00138048
            EXEC CICS WRITE
                 FILE('CIACCT')
                 FROM(CIACT-REC)
                 RIDFLD(CIACT-NUMB)
                 RESP(WS-RESP-CODE)
            END-EXEC
            EVALUATE (WS-RESP-CODE)
                WHEN DFHRESP(NORMAL)
                     MOVE '0000' TO CD-RESP-CODE
                     MOVE 'ACCT NUMBER ADDED SUCCESSFULLY'
                                 TO CD-RESP-ADDITIONAL
                     INITIALIZE CIC0018O-REC
                     MOVE CIACT-NUMB TO CIC0018O-NUMB
                     INITIALIZE CD-SRV-OUTPUT-DATA
                     MOVE CIC0018O-REC TO CD-SRV-OUTPUT-DATA
                WHEN DFHRESP(DUPREC)
                     MOVE '1000' TO CD-RESP-CODE
                     MOVE 'ACCT NUMBER EXISTED'
                                 TO CD-RESP-ADDITIONAL
                WHEN DFHRESP(NOTOPEN)
                     MOVE '2000' TO CD-RESP-CODE
                     MOVE 'ACCT NUMBER FILE NOT OPEN'
                                 TO CD-RESP-ADDITIONAL
                WHEN DFHRESP(INVREQ)
                     MOVE '3000' TO CD-RESP-CODE
                     MOVE 'INVALID REQUEST'
                                 TO CD-RESP-ADDITIONAL
                WHEN DFHRESP(FILENOTFOUND)
                     MOVE '4000' TO CD-RESP-CODE
                     MOVE 'ACCT NUMBER FILE NOT FOUND'
                                 TO CD-RESP-ADDITIONAL
                WHEN OTHER
                     MOVE '9000' TO CD-RESP-CODE
                     MOVE 'ACCT NUMBER ADDED FAILED'
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