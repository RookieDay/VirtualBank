       IDENTIFICATION DIVISION.                                         00010000
       PROGRAM-ID. CIOH0016.                                            00020048
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
      ****************************************************************  00025048
       ENVIRONMENT DIVISION.                                            00030000
       DATA DIVISION.                                                   00040000
       WORKING-STORAGE SECTION.                                         00050000
      *                                                                 00050148
       77 WS-BEGIN              PIC X(17) VALUE 'CIOH0016 WS BEGIN'.    00051048
       01 WS-VAR.
          05 WS-RESP-CODE       PIC S9(8) COMP.
          05 WS-CUST-ID-NUMBER  PIC 9(18).
      *
      *CUSTOMER  FILE CICUS.
       COPY CICAD001.
      *
      *SERVICE I/O
       COPY CIC0016I.
       COPY CIC0016O.
      *                                                                 00052048
       77 WS-END                PIC X(15) VALUE 'CIOH0016 WS END'.      00128048
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
            INITIALIZE CIC0016I-REC
            MOVE CD-SRV-INPUT-DATA   TO CIC0016I-REC
            .
       1000-INIT-EXIT.
            EXIT.
      *
       2000-PRE-PROCESSING.
      *
       2000-PRE-PROCESSING-EXIT.
            EXIT.
      *
       3000-MAIN-PROCESS.                                               00138048
            INITIALIZE CIC0016O-REC
            EXEC CICS STARTBR
                 FILE('CICARD1')
                 RIDFLD(CIC0016I-ID-NUMB)
                 EQUAL
                 RESP(WS-RESP-CODE)
            END-EXEC.
            EVALUATE (WS-RESP-CODE)
                WHEN DFHRESP(NORMAL)
                     MOVE '0000' TO CD-RESP-CODE
                     MOVE 'CUST-NUMBER READED SUCCESSFULLY'
                                      TO CD-RESP-ADDITIONAL
                     MOVE CIC0016I-ID-NUMB TO WS-CUST-ID-NUMBER
                     INITIALIZE CICAD-REC
                     EXEC CICS READNEXT
                            FILE('CICARD1')
                            RIDFLD(WS-CUST-ID-NUMBER)
                            INTO(CICAD-REC)
                            RESP(WS-RESP-CODE)
                     END-EXEC
                     PERFORM UNTIL WS-RESP-CODE = DFHRESP(ENDFILE)
                        OR CICAD-CUST-NUMB NOT = CIC0016I-ID-NUMB
                        ADD 1 TO CIC0016O-COUNT
                        MOVE CICAD-NUMB TO
                         CIC0016O-CARD-NUMB(CIC0016O-COUNT)
                       INITIALIZE CICAD-REC
                       EXEC CICS READNEXT
                            FILE('CICARD1')
                            RIDFLD(WS-CUST-ID-NUMBER)
                            INTO(CICAD-REC)
                            RESP(WS-RESP-CODE)
                       END-EXEC
                     END-PERFORM
                     EXEC CICS ENDBR
                          FILE('CICARD1')
                     END-EXEC
                WHEN DFHRESP(NOTFND)
                     MOVE '1000' TO CD-RESP-CODE
                     MOVE 'NO CARD INFORMATION FOUND'
                                 TO CD-RESP-ADDITIONAL
                WHEN DFHRESP(NOTOPEN)
                     MOVE '2000' TO CD-RESP-CODE
                     MOVE 'CARD FILE NOT OPEN'
                                 TO CD-RESP-ADDITIONAL
                WHEN DFHRESP(INVREQ)
                     MOVE '3000' TO CD-RESP-CODE
                     MOVE 'INVALID REQUEST'
                                 TO CD-RESP-ADDITIONAL
                WHEN DFHRESP(FILENOTFOUND)
                     MOVE '4000' TO CD-RESP-CODE
                     MOVE 'CARD FILE NOT FOUND'
                                 TO CD-RESP-ADDITIONAL
                WHEN OTHER
                     MOVE '9000' TO CD-RESP-CODE
                     MOVE 'CARD FILE READ FAILED'
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
            INITIALIZE CD-SRV-OUTPUT-DATA
            MOVE CIC0016O-REC        TO CD-SRV-OUTPUT-DATA
            EXEC CICS RETURN END-EXEC.
      *
       5000-CLEAN-UP-EXIT.
            EXIT.                                                       00151021
      *
