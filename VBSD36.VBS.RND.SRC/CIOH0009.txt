       IDENTIFICATION DIVISION.                                         00010000
       PROGRAM-ID. CIOH0009.                                            00020048
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
      *2015-01-22    KEVIN     INITIAL VERSION                          00024548
      ****************************************************************  00025048
       ENVIRONMENT DIVISION.                                            00030000
       DATA DIVISION.                                                   00040000
       WORKING-STORAGE SECTION.                                         00050000
      *                                                                 00050148
       77 WS-BEGIN              PIC X(17) VALUE 'CIOH0009 WS BEGIN'.    00051048
       01 WS-VAR.
          05 WS-RESP-CODE       PIC S9(8) COMP.
          05 WS-APPL-ID         PIC 9(13).
          05 IX                 PIC 9(2).
          05 WS-COUNT           PIC 9(2).
          05 WS-SELECTED        PIC X.
      *
      *CUSTOMER  FILE CICUS.
       COPY CIAPP001.
      *
      *SERVICE I/O
       COPY CIC0009I.
       COPY CIC0009O.
      *                                                                 00052048
       77 WS-END                PIC X(15) VALUE 'CIOH0009 WS END'.      00128048
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
            INITIALIZE CIC0009I-REC
            MOVE CD-SRV-INPUT-DATA   TO CIC0009I-REC
            .
       1000-INIT-EXIT.
            EXIT.
      *
       2000-PRE-PROCESSING.
            INITIALIZE CIAPP-REC
            IF CIC0009I-DIRECTION = 'F7'
               SUBTRACT 1 FROM CIC0009I-APPL-ID GIVING CIAPP-ID
            ELSE
               ADD 1 TO CIC0009I-APPL-ID GIVING CIAPP-ID
            END-IF
            .
      *
       2000-PRE-PROCESSING-EXIT.
            EXIT.
      *
       3000-MAIN-PROCESS.                                               00138048
            INITIALIZE CIC0009O-REC
            EXEC CICS STARTBR
                 FILE('CIAPP')
                 RIDFLD(CIAPP-ID)
                 RESP(WS-RESP-CODE)
            END-EXEC
            EVALUATE (WS-RESP-CODE)
                WHEN DFHRESP(NORMAL)
                     EXEC CICS READNEXT
                            FILE('CIAPP')
                            RIDFLD(CIAPP-ID)
                            INTO(CIAPP-REC)
                            RESP(WS-RESP-CODE)
                     END-EXEC
                     IF CIC0009I-DIRECTION = 'F7'
                        EXEC CICS READPREV
                             FILE('CIAPP')
                             RIDFLD(CIAPP-ID)
                             INTO(CIAPP-REC)
                             RESP(WS-RESP-CODE)
                        END-EXEC
                     END-IF
                     PERFORM UNTIL WS-RESP-CODE = DFHRESP(ENDFILE) OR
                                   CIC0009O-COUNT = 14
                        MOVE 'N' TO WS-SELECTED
                        EVALUATE CIC0009I-OPTION
                            WHEN 001
                                 IF CIAPP-STATUS = 001 OR
                                    CIAPP-STATUS = 010 OR
                                    CIAPP-STATUS = 011
                                    MOVE 'Y' TO WS-SELECTED
                                 END-IF
                            WHEN 002
                                 IF CIAPP-STATUS = 002
                                    MOVE 'Y' TO WS-SELECTED
                                 END-IF
                            WHEN 003
                                 IF CIAPP-STATUS = 003
                                    MOVE 'Y' TO WS-SELECTED
                                 END-IF
                            WHEN 004
                                 IF CIAPP-STATUS = 004
                                    MOVE 'Y' TO WS-SELECTED
                                 END-IF
                        END-EVALUATE
                        IF WS-SELECTED = 'Y'
                           ADD 1 TO CIC0009O-COUNT
                           MOVE CIAPP-ID TO
                                     CIC0009O-APPL-ID(CIC0009O-COUNT)
                        END-IF
                        IF CIC0009I-DIRECTION = 'F7'
                           EXEC CICS READPREV
                                FILE('CIAPP')
                                RIDFLD(CIAPP-ID)
                                INTO(CIAPP-REC)
                                RESP(WS-RESP-CODE)
                           END-EXEC
                        ELSE
                           EXEC CICS READNEXT
                                FILE('CIAPP')
                                RIDFLD(CIAPP-ID)
                                INTO(CIAPP-REC)
                                RESP(WS-RESP-CODE)
                           END-EXEC
                        END-IF
                     END-PERFORM
                     MOVE '0000' TO CD-RESP-CODE
                     MOVE 'GET APPLICATION LIST SUCCESSFULLY'
                                      TO CD-RESP-ADDITIONAL
                WHEN DFHRESP(NOTFND)
                     MOVE '1000' TO CD-RESP-CODE
                     MOVE 'NO RECORDS FOUND'
                                 TO CD-RESP-ADDITIONAL
                     PERFORM 5000-CLEAN-UP
                        THRU 5000-CLEAN-UP-EXIT
                WHEN DFHRESP(NOTOPEN)
                     MOVE '2000' TO CD-RESP-CODE
                     MOVE 'APPLICATION FILE NOT OPEN'
                                 TO CD-RESP-ADDITIONAL
                     PERFORM 5000-CLEAN-UP
                        THRU 5000-CLEAN-UP-EXIT
                WHEN DFHRESP(INVREQ)
                     MOVE '3000' TO CD-RESP-CODE
                     MOVE 'INVALID REQUEST'
                                 TO CD-RESP-ADDITIONAL
                     PERFORM 5000-CLEAN-UP
                        THRU 5000-CLEAN-UP-EXIT
                WHEN DFHRESP(FILENOTFOUND)
                     MOVE '4000' TO CD-RESP-CODE
                     MOVE 'APPLICATION FILE NOT FOUND'
                                 TO CD-RESP-ADDITIONAL
                     PERFORM 5000-CLEAN-UP
                        THRU 5000-CLEAN-UP-EXIT
                WHEN OTHER
                     MOVE '9000' TO CD-RESP-CODE
                     MOVE 'APPLICATION FILE READ FAILED'
                                 TO CD-RESP-ADDITIONAL
                     PERFORM 5000-CLEAN-UP
                        THRU 5000-CLEAN-UP-EXIT
            END-EVALUATE
            EXEC CICS ENDBR
                 FILE('CIAPP')
            END-EXEC
            .
       3000-MAIN-PROCESS-EXIT.                                          00139048
            EXIT.                                                       00151021
      *                                                                 00152019
       4000-POST-PROCESSING.
            IF CIC0009I-DIRECTION = 'F7'
               INITIALIZE WS-APPL-ID WS-COUNT
               MOVE CIC0009O-COUNT TO WS-COUNT
               PERFORM VARYING IX FROM 1 BY 1 UNTIL IX >= WS-COUNT
                 MOVE CIC0009O-APPL-ID(IX) TO WS-APPL-ID
                 MOVE CIC0009O-APPL-ID(WS-COUNT) TO CIC0009O-APPL-ID(IX)
                 MOVE WS-APPL-ID TO CIC0009O-APPL-ID(WS-COUNT)
                 SUBTRACT 1 FROM WS-COUNT
               END-PERFORM
            END-IF
            .
       4000-POST-PROCESSING-EXIT.
            EXIT.                                                       00151021
      *
       5000-CLEAN-UP.
            MOVE CIC0009O-REC TO CD-SRV-OUTPUT-DATA
            EXEC CICS RETURN END-EXEC.
      *
       5000-CLEAN-UP-EXIT.
            EXIT.                                                       00151021
      *
