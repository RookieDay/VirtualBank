       IDENTIFICATION DIVISION.                                         00010000
       PROGRAM-ID. SMOSDDRV.                                            00020048
      ***************************************************************** 00021048
      * SMOSDDRV - SERVICE DEMO                                         00022048
      *            DRIVER PROGRAM                                       00023048
      * DEMO CALLED BY CLIENT PROGRAM AND CALL HOST PROGRAM             00023148
      *                                                                 00023248
      * SD-RESP-CODE    SD-RESP-ADDITIONAL
      * '0111'          'SERVICE NAME NOT FOUND IN SMSD'
      * '0222'          'SERVICE FILE ERROR'
      * '0333'          'SERVICE PROGRAM ERROR'
      * '0999'          'SERVICE NAME NOT SPECIFIED'
      ***************************************************************** 00024048
      *                         VERSION HISTORY                         00024148
      *---------------------------------------------------------------- 00024248
      *DATE/TIME ³   AUTHOR   ³ DESCRIPTION                             00024348
      *---------------------------------------------------------------- 00024448
      *2014-12-16    KEVIN      INITIAL VERSION                         00024548
      ***************************************************************** 00025048
       ENVIRONMENT DIVISION.                                            00030000
       DATA DIVISION.                                                   00040000
       WORKING-STORAGE SECTION.                                         00050000
      *                                                                 00050148
       77 WS-BEGIN              PIC X(17) VALUE 'SMOSDDRV WS BEGIN'.    00051048
       01 WS-VAR.
          05 WS-RESP-CODE       PIC S9(8) COMP.
      *                                                                 00052048
       COPY SMSDMSGD.
      *
       01 WS-SRV-COMMAREA.
      *SERVICE REQUEST/RESPONSE COMMAREA
       COPY SD02WS.
       77 WS-END                PIC X(15) VALUE 'SMOSDDRV WS END'.      00128048
      *
       LINKAGE SECTION.                                                 00128100
       01 DFHCOMMAREA.                                                  00128200
      *SERVICE REQUEST/RESPONSE COMMAREA                                00128348
       COPY SD01WS.                                                     00128448
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
            IF SD-SRV-NAME EQUAL TO SPACE
               MOVE '0999' TO SD-RESP-CODE
               MOVE 'SERVICE NAME NOT SPECIFIED' TO SD-RESP-ADDITIONAL
               PERFORM 5010-RETURN
                  THRU 5010-RETURN-EXIT
            END-IF
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
            INITIALIZE SMD-SERVICE-DIRECTORY
            MOVE SD-SRV-NAME TO SMD-SRV-NAME
            EXEC CICS READ
                 FILE('SMSD')
                 INTO(SMD-SERVICE-DIRECTORY)
                 RIDFLD(SMD-SRV-NAME)
                 RESP(WS-RESP-CODE)
            END-EXEC
            EVALUATE WS-RESP-CODE
                WHEN DFHRESP(NORMAL)
                     PERFORM 3010-LINK-CLIENT
                        THRU 3010-LINK-CLIENT-EXIT
                WHEN DFHRESP(NOTFND)
                     MOVE '0111' TO SD-RESP-CODE
                     MOVE 'SERVICE NAME NOT FOUND IN SMSD'
                                 TO SD-RESP-ADDITIONAL
                     PERFORM 5010-RETURN
                        THRU 5010-RETURN-EXIT
                WHEN OTHER
                     MOVE '0222' TO SD-RESP-CODE
                     MOVE 'SERVICE FILE ERROR'
                                 TO SD-RESP-ADDITIONAL
                     PERFORM 5010-RETURN
                        THRU 5010-RETURN-EXIT
            END-EVALUATE
            .
       3000-MAIN-PROCESS-EXIT.                                          00139048
            EXIT.                                                       00151021
      *                                                                 00152019
       3010-LINK-CLIENT.
            INITIALIZE WS-SRV-COMMAREA
            MOVE SD-SRV-INPUT-DATA TO CD-SRV-INPUT-DATA
            EXEC CICS
                 LINK
                 PROGRAM(SMD-SRV-PGM)
                 COMMAREA(WS-SRV-COMMAREA)
                 RESP(WS-RESP-CODE)
            END-EXEC
            EVALUATE WS-RESP-CODE
                WHEN DFHRESP(NORMAL)
                     MOVE CD-RESP-CODE TO SD-RESP-CODE
                     MOVE CD-RESP-ADDITIONAL
                                       TO SD-RESP-ADDITIONAL
                     MOVE CD-SRV-OUTPUT-DATA
                                       TO SD-SRV-OUTPUT-DATA
                WHEN OTHER
                     MOVE '0333' TO SD-RESP-CODE
                     MOVE 'SERVICE PROGRAM ERROR'
                                 TO SD-RESP-ADDITIONAL
            END-EVALUATE
            .
      *
       3010-LINK-CLIENT-EXIT.
            EXIT.                                                       00151021
      *                                                                 00152019
       4000-POST-PROCESSING.
      *
       4000-POST-PROCESSING-EXIT.
            EXIT.                                                       00151021
      *
       5000-CLEAN-UP.
            PERFORM 5010-RETURN
               THRU 5010-RETURN-EXIT
            .
      *
       5000-CLEAN-UP-EXIT.
            EXIT.                                                       00151021
      *
       5010-RETURN.
            EXEC CICS RETURN END-EXEC
            .
       5010-RETURN-EXIT.
            EXIT.
      *
