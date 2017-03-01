       IDENTIFICATION DIVISION.                                         00021001
       PROGRAM-ID. CIOH0006.                                            00022001
      ***************************************************************** 00023001
      * DMOH0001 - SERVICE DEMO                                         00024001
      *            HOST PROGRAM                                         00025001
      * DEMO HOST PROGRAM WHICH WILL BE CALLED BY DRIVER PROGRAM        00026001
      *                                                                 00027001
      *  CD-RESP-CODE    CD-RESP-ADDITIONAL                             00028001
      *     '0000'       'CUSTOMER ADDED SUCCESSFULLY'                  00029001
      *     '1000'       'CUSTOMER EXISTED'                             00029101
      *     '2000'       'CUSTOMER FILE NOT OPEN'                       00029201
      *     '3000'       'INVALID REQUEST'                              00029301
      *     '4000'       'CUSTOMER FILE NOT FOUND'                      00029401
      *     '9000'       'CUSTOMER ADDED FAILED'                        00029501
      ***************************************************************** 00029601
      *                         VERSION HISTORY                         00029701
      *---------------------------------------------------------------- 00029801
      *DATE/TIME ³   AUTHOR   ³ DESCRIPTION                             00029901
      *---------------------------------------------------------------- 00030001
      *2014-12-17    KEVIN      INITIAL VERSION                         00030101
      ***************************************************************** 00030201
       ENVIRONMENT DIVISION.                                            00030301
       DATA DIVISION.                                                   00030401
       WORKING-STORAGE SECTION.                                         00030501
      *                                                                 00030601
       77 WS-BEGIN              PIC X(17) VALUE 'CIOH0006 WS BEGIN'.    00030701
       01 WS-VAR.                                                       00030801
          05 WS-RESP-CODE       PIC S9(8) COMP.                         00030901
          05 WS-GETTIME         PIC X(20).                              00031005
          05 WS-DATEOUT         PIC X(10).                              00031105
          05 WS-TIMEOUT         PIC X(8).                               00031205
      *                                                                 00031301
      *CUSTOMER FILE DMCUS                                              00031401
       COPY CIAPP001.                                                   00031501
      *                                                                 00031601
      *SERVICE I/O                                                      00031701
       COPY CIC0006I.                                                   00031801
       COPY CIC0006O.                                                   00031901
      *                                                                 00032001
       77 WS-END                PIC X(15) VALUE 'CIOH0006 WS END'.      00032101
      *                                                                 00032201
       LINKAGE SECTION.                                                 00032301
       01 DFHCOMMAREA.                                                  00032401
      *SERVICE REQUEST/RESPONSE COMMAREA                                00032501
       COPY SD02WS.                                                     00032601
      *                                                                 00032701
       PROCEDURE DIVISION.                                              00032801
       0000-MAINLINE.                                                   00032901
      *                                                                 00033001
            PERFORM 1000-INIT                                           00033101
               THRU 1000-INIT-EXIT                                      00033201
      *                                                                 00033301
            PERFORM 2000-PRE-PROCESSING                                 00033401
               THRU 2000-PRE-PROCESSING-EXIT                            00033501
      *                                                                 00033601
            PERFORM 3000-MAIN-PROCESS                                   00033701
               THRU 3000-MAIN-PROCESS-EXIT                              00033801
      *                                                                 00033901
            PERFORM 4000-POST-PROCESSING                                00034001
               THRU 4000-POST-PROCESSING-EXIT                           00034101
      *                                                                 00034201
            PERFORM 5000-CLEAN-UP                                       00034301
               THRU 5000-CLEAN-UP-EXIT                                  00034401
            .                                                           00034501
      *                                                                 00034601
       0000-EXIT.                                                       00034701
            EXIT.                                                       00034801
      *                                                                 00034901
       1000-INIT.                                                       00035001
            INITIALIZE CIC0006I-REC                                     00035101
            MOVE CD-SRV-INPUT-DATA   TO CIC0006I-REC                    00035201
            .                                                           00035301
       1000-INIT-EXIT.                                                  00035401
            EXIT.                                                       00035501
      *                                                                 00035601
       1010-ASK-TIME-DATE.                                              00035705
      *                                                                 00035805
            EXEC CICS                                                   00035905
                 ASKTIME                                                00036005
                 ABSTIME(WS-GETTIME)                                    00036105
            END-EXEC                                                    00036205
            EXEC CICS                                                   00036305
                 FORMATTIME                                             00036405
                 ABSTIME(WS-GETTIME)                                    00036505
                 DATESEP('-')                                           00036605
                 YYYYMMDD(WS-DATEOUT)                                   00036705
            END-EXEC                                                    00036805
            EXEC CICS                                                   00036905
                 FORMATTIME                                             00037005
                 ABSTIME(WS-GETTIME)                                    00037105
                 TIMESEP                                                00037205
                 TIME(WS-TIMEOUT)                                       00037305
            END-EXEC                                                    00037405
            .                                                           00037505
      *                                                                 00037605
       1010-ASK-TIME-DATE-EXIT.                                         00037705
            EXIT.                                                       00037805
      *                                                                 00037905
       2000-PRE-PROCESSING.                                             00038001
            INITIALIZE CIAPP-REC                                        00038101
            MOVE CIC0006I-ID  TO CIAPP-ID                               00038203
            .                                                           00038301
      *                                                                 00038401
       2000-PRE-PROCESSING-EXIT.                                        00038501
            EXIT.                                                       00038601
      *                                                                 00038701
       3000-MAIN-PROCESS.                                               00038801
           EXEC CICS READ                                               00038901
                 FILE('CIAPP')                                          00039001
                 INTO(CIAPP-REC)                                        00039101
                 RIDFLD(CIAPP-ID)                                       00039201
                 RESP(WS-RESP-CODE)                                     00039301
                 UPDATE                                                 00039401
            END-EXEC                                                    00039501
            IF WS-RESP-CODE = DFHRESP(NORMAL)                           00039601
               INITIALIZE CIAPP-REC                                     00039701
               MOVE CIC0006I-REC        TO CIAPP-REC                    00039801
               PERFORM 3010-POPULATE-LAST                               00039905
                  THRU 3010-POPULATE-LAST-EXIT                          00040005
               EXEC CICS REWRITE                                        00040101
                    FILE('CIAPP')                                       00040201
                    FROM(CIAPP-REC)                                     00040301
                    RESP(WS-RESP-CODE)                                  00040401
               END-EXEC                                                 00040501
               EVALUATE (WS-RESP-CODE)                                  00040604
                   WHEN DFHRESP(NORMAL)                                 00040704
                        MOVE '0000' TO CD-RESP-CODE                     00040804
                        MOVE 'APPLICATION UPDATE SUCCESSFULLY'          00040904
                                 TO CD-RESP-ADDITIONAL                  00041001
                   WHEN DFHRESP(DUPREC)                                 00041104
                        MOVE '1000' TO CD-RESP-CODE                     00041204
                        MOVE 'APPLICATION EXISTED'                      00041304
                                 TO CD-RESP-ADDITIONAL                  00041401
                   WHEN DFHRESP(NOTOPEN)                                00041504
                        MOVE '2000' TO CD-RESP-CODE                     00041604
                        MOVE 'APPLICATION FILE NOT OPEN'                00041704
                                 TO CD-RESP-ADDITIONAL                  00041801
                   WHEN DFHRESP(INVREQ)                                 00041904
                        MOVE '3000' TO CD-RESP-CODE                     00042004
                        MOVE 'INVALID REQUEST'                          00042104
                                 TO CD-RESP-ADDITIONAL                  00042201
                   WHEN DFHRESP(FILENOTFOUND)                           00042304
                        MOVE '4000' TO CD-RESP-CODE                     00042404
                        MOVE 'APPLICATION FILE NOT FOUND'               00042504
                                 TO CD-RESP-ADDITIONAL                  00042601
                   WHEN DFHRESP(NOTFND)                                 00042704
                        MOVE '5000' TO CD-RESP-CODE                     00042804
                        MOVE 'APPLICATION NOT FOUND'                    00042904
                                 TO CD-RESP-ADDITIONAL                  00043001
                   WHEN OTHER                                           00043104
                        MOVE '9000' TO CD-RESP-CODE                     00043204
                        MOVE 'APPLICATION ADDED FAILED'                 00043304
                                 TO CD-RESP-ADDITIONAL                  00043401
              END-EVALUATE                                              00043504
            ELSE                                                        00043604
              MOVE '9999' TO CD-RESP-CODE                               00043704
              MOVE 'CIAPP FILE ERROR'                                   00043804
                          TO CD-RESP-ADDITIONAL                         00043904
            END-IF                                                      00044001
            .                                                           00044101
       3000-MAIN-PROCESS-EXIT.                                          00044201
            EXIT.                                                       00045701
      *                                                                 00045801
       3010-POPULATE-LAST.                                              00045905
            PERFORM 1010-ASK-TIME-DATE                                  00046005
               THRU 1010-ASK-TIME-DATE-EXIT                             00046105
      *                                                                 00046205
            MOVE WS-DATEOUT              TO CIAPP-LAST-DATE             00046305
            MOVE WS-TIMEOUT              TO CIAPP-LAST-TIME             00046405
            .                                                           00046505
      *                                                                 00046601
       3010-POPULATE-LAST-EXIT.                                         00046705
            EXIT.                                                       00046801
      *                                                                 00046905
       4000-POST-PROCESSING.                                            00047005
      *                                                                 00047105
       4000-POST-PROCESSING-EXIT.                                       00047205
            EXIT.                                                       00047305
      *                                                                 00047405
       5000-CLEAN-UP.                                                   00047505
            EXEC CICS RETURN END-EXEC.                                  00047605
      *                                                                 00047705
       5000-CLEAN-UP-EXIT.                                              00047805
            EXIT.                                                       00048005
