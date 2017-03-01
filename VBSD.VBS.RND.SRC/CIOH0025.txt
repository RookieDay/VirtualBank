       IDENTIFICATION DIVISION.
       PROGRAM-ID. CIOH0025.
      *****************************************************************
      * DMOH0001 - SERVICE DEMO
      *            HOST PROGRAM
      * DEMO HOST PROGRAM WHICH WILL BE CALLED BY DRIVER PROGRAM
      *
      *  CD-RESP-CODE    CD-RESP-ADDITIONAL
      *     '0000'       'APPLICATION ADDED SUCCESSFULLY'
      *     '1000'       'APPLICATION EXISTED'
      *     '2000'       'APPLICATION FILE NOT OPEN'
      *     '3000'       'INVALID REQUEST'
      *     '4000'       'APPLICATION FILE NOT FOUND'
      *     '9000'       'APPLICATION ADDED FAILED'
      *****************************************************************
      *                         VERSION HISTORY
      *----------------------------------------------------------------
      *DATE/TIME ³   AUTHOR   ³ DESCRIPTION
      *----------------------------------------------------------------
      *2014-12-17    KRIS      INITIAL VERSION
      *****************************************************************
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *
       77 WS-BEGIN              PIC X(17) VALUE 'CIOH0025 WS BEGIN'.
       01 WS-VAR.
          05 WS-RESP-CODE       PIC S9(8) COMP.
          05 WS-APPLID          PIC 9(13).
      *
      *APPLICATION FILE DMCUS
       COPY CICAD001.
      *
       COPY CICUS001.
      *
      *SERVICE I/O
       COPY CIC0025I.
       COPY CIC0025O.
      *
       77 WS-END                PIC X(15) VALUE 'CIOH0025 WS END'.
      *
       LINKAGE SECTION.
       01 DFHCOMMAREA.
      *SERVICE REQUEST/RESPONSE COMMAREA
       COPY SD02WS.
      *
       PROCEDURE DIVISION.
       0000-MAINLINE.
      *
            PERFORM 1000-INIT
               THRU 1000-INIT-EXIT
      *
            PERFORM 2000-PRE-PROCESSING
               THRU 2000-PRE-PROCESSING-EXIT
      *
            PERFORM 3000-MAIN-PROCESS
               THRU 3000-MAIN-PROCESS-EXIT
      *
            PERFORM 4000-POST-PROCESSING
               THRU 4000-POST-PROCESSING-EXIT
      *
            PERFORM 5000-CLEAN-UP
               THRU 5000-CLEAN-UP-EXIT
            .
      *
       0000-EXIT.
            EXIT.
      *
       1000-INIT.
            INITIALIZE CIC0025I-REC
            MOVE CD-SRV-INPUT-DATA   TO CIC0025I-REC
            .
       1000-INIT-EXIT.
            EXIT.
      *
       2000-PRE-PROCESSING.
            INITIALIZE CICAD-REC
            MOVE CIC0025I-NUMB      TO CICAD-NUMB
            .
      *
       2000-PRE-PROCESSING-EXIT.
            EXIT.
      *
       3000-MAIN-PROCESS.
            EXEC CICS READ
                 FILE('CICARD')
                 INTO(CICAD-REC)
                 RIDFLD(CICAD-NUMB)
                 RESP(WS-RESP-CODE)
            END-EXEC
            EVALUATE (WS-RESP-CODE)
                WHEN DFHRESP(NORMAL)
                     PERFORM 3010-GET-CUST
                        THRU 3010-GET-CUST-EXIT
      *              MOVE '0000' TO CD-RESP-CODE
      *              MOVE 'CREDCARD READED SUCCESSFULLY'
      *                          TO CD-RESP-ADDITIONAL
      *              INITIALIZE CIC0025O-REC
      *              MOVE CICAD-REC TO CIC0025O-REC
      *              INITIALIZE CD-SRV-OUTPUT-DATA
      *              MOVE  CIC0025O-REC TO CD-SRV-OUTPUT-DATA
                WHEN DFHRESP(NOTFND)
                     MOVE '1000' TO CD-RESP-CODE
                     MOVE 'CREDCARD NOT FOUND'
                                 TO CD-RESP-ADDITIONAL
                WHEN DFHRESP(NOTOPEN)
                     MOVE '2000' TO CD-RESP-CODE
                     MOVE 'CREDCARD FILE NOT OPEN'
                                 TO CD-RESP-ADDITIONAL
                WHEN DFHRESP(INVREQ)
                     MOVE '3000' TO CD-RESP-CODE
                     MOVE 'INVALID REQUEST'
                                 TO CD-RESP-ADDITIONAL
                WHEN DFHRESP(FILENOTFOUND)
                     MOVE '4000' TO CD-RESP-CODE
                     MOVE 'CREDCARD FILE NOT FOUND'
                                 TO CD-RESP-ADDITIONAL
                WHEN OTHER
                     MOVE '9000' TO CD-RESP-CODE
                     MOVE 'CREDCARD ADDED FAILED'
                                 TO CD-RESP-ADDITIONAL
            END-EVALUATE
            .
       3000-MAIN-PROCESS-EXIT.
            EXIT.
       3010-GET-CUST.
            EXEC CICS READ
                 FILE('CICUS')
                 INTO(CICUS-REC)
                 RIDFLD(CICAD-CUST-NUMB)
                 RESP(WS-RESP-CODE)
            END-EXEC
            EVALUATE (WS-RESP-CODE)
                WHEN DFHRESP(NORMAL)
                     MOVE '0000' TO CD-RESP-CODE
                     MOVE 'CUSTOMER READED SUCCESSFULLY'
                                 TO CD-RESP-ADDITIONAL
                     INITIALIZE CIC0025O-REC
                     MOVE CICUS-REC TO CIC0025O-REC
                     INITIALIZE CD-SRV-OUTPUT-DATA
                     MOVE  CIC0025O-REC TO CD-SRV-OUTPUT-DATA
                WHEN DFHRESP(NOTFND)
                     MOVE '1000' TO CD-RESP-CODE
                     MOVE 'CUSTOMER NOT FOUND'
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
                WHEN OTHER
                     MOVE '9000' TO CD-RESP-CODE
                     MOVE 'CUSTOMER READ FAILED'
                                 TO CD-RESP-ADDITIONAL
            END-EVALUATE
            .
       3010-GET-CUST-EXIT.
            EXIT.
      *
       4000-POST-PROCESSING.
      *
       4000-POST-PROCESSING-EXIT.
            EXIT.
      *
       5000-CLEAN-UP.
            EXEC CICS RETURN END-EXEC.
      *
       5000-CLEAN-UP-EXIT.
            EXIT.
      *
