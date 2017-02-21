       IDENTIFICATION DIVISION.                                         00010000
       PROGRAM-ID. CIOH0002.                                            00020048
      ***************************************************************** 00021048
      * DMOH0001 - SERVICE DEMO                                         00022048
      *            HOST PROGRAM                                         00023048
      * DEMO HOST PROGRAM WHICH WILL BE CALLED BY DRIVER PROGRAM        00023148
      *                                                                 00023248
      *  CD-RESP-CODE    CD-RESP-ADDITIONAL
      *     '0000'       'CUSTOMER ADDED SUCCESSFULLY'
      *     '1000'       'CUSTOMER EXISTED'
      *     '2000'       'CUSTOMER FILE NOT OPEN'
      *     '3000'       'INVALID REQUEST'
      *     '4000'       'CUSTOMER FILE NOT FOUND'
      *     '9000'       'CUSTOMER ADDED FAILED'
      ***************************************************************** 00024048
      *                         VERSION HISTORY                         00024148
      *---------------------------------------------------------------- 00024248
      *DATE/TIME ³   AUTHOR   ³ DESCRIPTION                             00024348
      *---------------------------------------------------------------- 00024448
      *2014-12-17    KEVIN      INITIAL VERSION                         00024548
      ***************************************************************** 00025048
       ENVIRONMENT DIVISION.                                            00030000
       DATA DIVISION.                                                   00040000
       WORKING-STORAGE SECTION.                                         00050000
      *                                                                 00050148
       77 WS-BEGIN              PIC X(17) VALUE 'CIOH0002 WS BEGIN'.    00051048
       01 WS-VAR.
          05 WS-RESP-CODE       PIC S9(8) COMP.
      *
      *CUSTOMER FILE DMCUS
       COPY CICUS001.
      *
      *SERVICE I/O
       COPY CIC0002I.
       COPY CIC0002O.
      *                                                                 00052048
       77 WS-END                PIC X(15) VALUE 'CIOH0002 WS END'.      00128048
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
            INITIALIZE CIC0002I-REC
            MOVE CD-SRV-INPUT-DATA   TO CIC0002I-REC
            .
       1000-INIT-EXIT.
            EXIT.
      *
       2000-PRE-PROCESSING.
            INITIALIZE CICUS-REC
            MOVE CIC0002I-ID-NUMBER  TO CICUS-ID-NUMBER
            .
      *
       2000-PRE-PROCESSING-EXIT.
            EXIT.
      *
       3000-MAIN-PROCESS.                                               00138048
           EXEC CICS READ
                 FILE('CICUS')
                 INTO(CICUS-REC)
                 RIDFLD(CICUS-ID-NUMBER)
                 RESP(WS-RESP-CODE)
                 UPDATE
            END-EXEC
            IF WS-RESP-CODE = DFHRESP(NORMAL)
               INITIALIZE CICUS-REC
               MOVE CIC0002I-REC        TO CICUS-REC
               EXEC CICS REWRITE
                    FILE('CICUS')
                    FROM(CICUS-REC)
                    RESP(WS-RESP-CODE)
               END-EXEC
            EVALUATE (WS-RESP-CODE)
                WHEN DFHRESP(NORMAL)
                     MOVE '0000' TO CD-RESP-CODE
                     MOVE 'CUSTOMER UPDATE SUCCESSFULLY'
                                 TO CD-RESP-ADDITIONAL
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
                     MOVE 'INVALID REQUEST'
                                 TO CD-RESP-ADDITIONAL
                WHEN DFHRESP(FILENOTFOUND)
                     MOVE '4000' TO CD-RESP-CODE
                     MOVE 'CUSTOMER FILE NOT FOUND'
                                 TO CD-RESP-ADDITIONAL
                WHEN DFHRESP(NOTFND)
                     MOVE '5000' TO CD-RESP-CODE
                     MOVE 'CUSTOMER NOT FOUND'
                                 TO CD-RESP-ADDITIONAL
                WHEN OTHER
                     MOVE '9000' TO CD-RESP-CODE
                     MOVE 'CUSTOMER ADDED FAILED'
                                 TO CD-RESP-ADDITIONAL
            END-EVALUATE
            END-IF
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