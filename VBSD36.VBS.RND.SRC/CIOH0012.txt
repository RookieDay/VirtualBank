       IDENTIFICATION DIVISION.                                         00010000
       PROGRAM-ID. CICH0012.                                            00020048
      ***************************************************************** 00021048
      * DMOH0001 - SERVICE DEMO                                         00022048
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
      *2014-12-17    KRIS      INITIAL VERSION                          00024548
      ***************************************************************** 00025048
       ENVIRONMENT DIVISION.                                            00030000
       DATA DIVISION.                                                   00040000
       WORKING-STORAGE SECTION.                                         00050000
      *                                                                 00050148
       77 WS-BEGIN              PIC X(17) VALUE 'CICH0012 WS BEGIN'.    00051048
       01 WS-VAR.
          05 WS-RESP-CODE       PIC S9(8) COMP.
          05 WS-APPLID          PIC 9(13).
      *
      *APPLICATION FILE DMCUS
       COPY CIAPP001.
       COPY CICUS001.
      *
      *SERVICE I/O
       COPY CIC0012I.
       COPY CIC0012O.
      *                                                                 00052048
       77 WS-END                PIC X(15) VALUE 'CICH0012 WS END'.      00128048
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
            INITIALIZE CIC0012I-REC
            MOVE CD-SRV-INPUT-DATA   TO CIC0012I-REC
            .
       1000-INIT-EXIT.
            EXIT.
      *
       2000-PRE-PROCESSING.
            INITIALIZE CIAPP-REC
            MOVE CIC0012I-APPL-ID             TO CIAPP-ID
            .
      *
       2000-PRE-PROCESSING-EXIT.
            EXIT.
      *
       3000-MAIN-PROCESS.                                               00138048
            EXEC CICS READ
                 FILE('CIAPP')
                 INTO(CIAPP-REC)
                 RIDFLD(CIAPP-ID)
                 RESP(WS-RESP-CODE)
            END-EXEC
            EVALUATE (WS-RESP-CODE)
                WHEN DFHRESP(NORMAL)
                     MOVE '0000' TO CD-RESP-CODE
                     MOVE 'APPLICATION READED SUCCESSFULLY'
                                 TO CD-RESP-ADDITIONAL
                     INITIALIZE CIC0012O-REC
                     MOVE CIAPP-REC TO CIC0012O-APPL-REC
                     PERFORM 3010-GET-CUST
                        THRU 3010-GET-CUST-EXIT
                     INITIALIZE CD-SRV-OUTPUT-DATA
                     MOVE  CIC0012O-REC TO CD-SRV-OUTPUT-DATA
                WHEN DFHRESP(DUPREC)
                     MOVE '1000' TO CD-RESP-CODE
                     MOVE 'APPLICATION EXISTED'
                                 TO CD-RESP-ADDITIONAL
                WHEN DFHRESP(NOTOPEN)
                     MOVE '2000' TO CD-RESP-CODE
                     MOVE 'APPLICATION FILE NOT OPEN'
                                 TO CD-RESP-ADDITIONAL
                WHEN DFHRESP(INVREQ)
                     MOVE '3000' TO CD-RESP-CODE
                     MOVE 'INVALID REQUEST'
                                 TO CD-RESP-ADDITIONAL
                WHEN DFHRESP(FILENOTFOUND)
                     MOVE '4000' TO CD-RESP-CODE
                     MOVE 'APPLICATION FILE NOT FOUND'
                                 TO CD-RESP-ADDITIONAL
                WHEN OTHER
                     MOVE '9000' TO CD-RESP-CODE
                     MOVE 'APPLICATION ADDED FAILED'
                                 TO CD-RESP-ADDITIONAL
            END-EVALUATE
            .
       3000-MAIN-PROCESS-EXIT.                                          00139048
            EXIT.                                                       00151021
       3010-GET-CUST.
            INITIALIZE CICUS-REC
            EXEC CICS READ
                 FILE('CICUS')
                 INTO(CICUS-REC)
                 RIDFLD(CIAPP-ID-NUMBER)
                 RESP(WS-RESP-CODE)
            END-EXEC
            EVALUATE (WS-RESP-CODE)
                WHEN DFHRESP(NORMAL)
                     MOVE '0000' TO CD-RESP-CODE
                     MOVE 'CUSTOMER READED SUCCESSFULLY'
                                 TO CD-RESP-ADDITIONAL
                     MOVE CICUS-REC TO CIC0012O-CUST-REC
                WHEN DFHRESP(DUPREC)
                     MOVE '1000' TO CD-RESP-CODE
                     MOVE 'CUSTOMER EXISTED'
                                 TO CD-RESP-ADDITIONAL
                WHEN DFHRESP(NOTOPEN)
                     MOVE '2000' TO CD-RESP-CODE
                     MOVE 'CUSTOMER FILE NOT OPEN'
                                 TO CD-RESP-ADDITIONAL
                WHEN DFHRESP(INVREQ)
                     MOVE '3000' TO CD-RESP-CODE
                     MOVE 'CUSTOMER REQUEST'
                                 TO CD-RESP-ADDITIONAL
                WHEN DFHRESP(FILENOTFOUND)
                     MOVE '4000' TO CD-RESP-CODE
                     MOVE 'CUSTOMER FILE NOT FOUND'
                                 TO CD-RESP-ADDITIONAL
                WHEN OTHER
                     MOVE '9000' TO CD-RESP-CODE
                     MOVE 'CUSTOMER READ FAILED'
                                 TO CD-RESP-ADDITIONAL
            END-EVALUATE
            .
       3010-GET-CUST-EXIT.
            EXIT.

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