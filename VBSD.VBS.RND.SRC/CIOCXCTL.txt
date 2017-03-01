       IDENTIFICATION DIVISION.                                         00010000
       PROGRAM-ID. CIOCXCTL.                                            00020001
      ***************************************************************** 00030000
      * CIOCCIMN - CLIENT PROGRAM                                       00040000
      *                                                                 00050000
      * CREDIT ISSUANCE MAIN MENU                                       00060000
      *                                                                 00070000
      ***************************************************************** 00080000
      *                         VERSION HISTORY                         00090000
      *---------------------------------------------------------------- 00100000
      *DATE/TIME ³   AUTHOR   ³ DESCRIPTION                             00110000
      *---------------------------------------------------------------- 00120000
      *2015-01-27    KEVIN      INITIAL VERSION                         00130001
      ***************************************************************** 00150000
       ENVIRONMENT DIVISION.                                            00160000
       DATA DIVISION.                                                   00170000
       WORKING-STORAGE SECTION.                                         00180000
      *                                                                 00190000
       77 WS-BEGIN              PIC X(17) VALUE 'CIOCXCTL WS BEGIN'.    00200002
       01 WS-VAR.                                                       00210000
          05 WS-RESP-CODE       PIC S9(8) COMP.                         00250000
          05 WS-MESSAGE         PIC X(40).                              00260000
          05 WS-TRNID           PIC X(4).                               00271003
      *                                                                 00271102
       01 WS-COMMAREA.                                                  00272001
          05 WS-FIRST-SEND      PIC X(1).                               00273001
          05 WS-OPTION          PIC 9(3).                               00274001
      *                                                                 00421000
       77 WS-END                PIC X(15) VALUE 'CIOCXCTL WS END'.      00460002
      *                                                                 00470000
       LINKAGE SECTION.                                                 00480000
       01 DFHCOMMAREA.                                                  00490000
      *COMMON CICS SCREEN HANDLE VARIABLES                              00500000
         05 LK-TRNID            PIC X(4).                               00510002
      *                                                                 00520000
       PROCEDURE DIVISION.                                              00530000
       0000-MAINLINE.                                                   00540000
      *                                                                 00550000
            PERFORM 1000-INIT                                           00560000
               THRU 1000-INIT-EXIT                                      00570000
      *                                                                 00580000
            PERFORM 2000-PRE-PROCESSING                                 00590000
               THRU 2000-PRE-PROCESSING-EXIT                            00600000
      *                                                                 00610000
            PERFORM 3000-MAIN-PROCESS                                   00620000
               THRU 3000-MAIN-PROCESS-EXIT                              00630000
      *                                                                 00640000
            PERFORM 4000-POST-PROCESSING                                00650000
               THRU 4000-POST-PROCESSING-EXIT                           00660000
      *                                                                 00670000
            PERFORM 5000-CLEAN-UP                                       00680000
               THRU 5000-CLEAN-UP-EXIT                                  00690000
            .                                                           00700000
      *                                                                 00710000
       0000-EXIT.                                                       00720000
            EXIT.                                                       00730000
      *                                                                 00740000
       1000-INIT.                                                       00750000
            IF EIBCALEN = 0                                             00760000
               MOVE EIBTRNID TO WS-TRNID                                00810002
            ELSE                                                        00820000
               MOVE LK-TRNID TO WS-TRNID                                00830002
            END-IF                                                      00910000
            .                                                           00920000
       1000-INIT-EXIT.                                                  00930000
            EXIT.                                                       00940000
      *                                                                 00950000
       2000-PRE-PROCESSING.                                             01210000
      *                                                                 01220000
       2000-PRE-PROCESSING-EXIT.                                        01230000
            EXIT.                                                       01240000
      *                                                                 01250000
       3000-MAIN-PROCESS.                                               01260000
            EVALUATE WS-TRNID                                           01261002
                WHEN 'CIMN'                                             01262002
                     EXEC CICS XCTL                                     01263002
                          PROGRAM('CIOCCIMN')                           01264002
                          RESP(WS-RESP-CODE)                            01265002
                     END-EXEC                                           01266002
                WHEN 'CICA'                                             01267002
                     EXEC CICS XCTL                                     01268002
                          PROGRAM('CIOCCA00')                           01269002
                          RESP(WS-RESP-CODE)                            01269102
                     END-EXEC                                           01269202
                WHEN 'CICS'                                             01269302
                     EXEC CICS XCTL                                     01269402
                          PROGRAM('CIOCCS00')                           01269502
                          RESP(WS-RESP-CODE)                            01269602
                     END-EXEC                                           01269702
                WHEN 'CICP'                                             01269804
                     EXEC CICS XCTL                                     01269904
                          PROGRAM('CIOCCP00')                           01270004
                          RESP(WS-RESP-CODE)                            01270104
                     END-EXEC                                           01270204
                WHEN 'CI01'                                             01270304
                     INITIALIZE WS-COMMAREA                             01270404
                     MOVE 'Y' TO WS-FIRST-SEND                          01270504
                     MOVE 000 TO WS-OPTION                              01270604
                     EXEC CICS XCTL                                     01270704
                          PROGRAM('CIOCCA01')                           01270804
                          COMMAREA(WS-COMMAREA)                         01270904
                          RESP(WS-RESP-CODE)                            01271004
                     END-EXEC                                           01271104
                WHEN 'CI02'                                             01271204
                     INITIALIZE WS-COMMAREA                             01271304
                     MOVE 'Y' TO WS-FIRST-SEND                          01271404
                     MOVE 001 TO WS-OPTION                              01271504
                     EXEC CICS XCTL                                     01271604
                          PROGRAM('CIOCCA03')                           01271704
                          COMMAREA(WS-COMMAREA)                         01271804
                          RESP(WS-RESP-CODE)                            01271904
                     END-EXEC                                           01272004
                WHEN 'CI03'                                             01272104
                     INITIALIZE WS-COMMAREA                             01272204
                     MOVE 'Y' TO WS-FIRST-SEND                          01272304
                     MOVE 002 TO WS-OPTION                              01272404
                     EXEC CICS XCTL                                     01272504
                          PROGRAM('CIOCCA03')                           01272604
                          COMMAREA(WS-COMMAREA)                         01272704
                          RESP(WS-RESP-CODE)                            01272804
                     END-EXEC                                           01272904
                WHEN 'CI04'                                             01273004
                     INITIALIZE WS-COMMAREA                             01273104
                     MOVE 'Y' TO WS-FIRST-SEND                          01273204
                     MOVE 003 TO WS-OPTION                              01273304
                     EXEC CICS XCTL                                     01273404
                          PROGRAM('CIOCCA03')                           01273504
                          COMMAREA(WS-COMMAREA)                         01273604
                          RESP(WS-RESP-CODE)                            01273704
                     END-EXEC                                           01273804
                WHEN 'CI05'                                             01273904
                     INITIALIZE WS-COMMAREA                             01274004
                     MOVE 'Y' TO WS-FIRST-SEND                          01274104
                     MOVE 004 TO WS-OPTION                              01274204
                     EXEC CICS XCTL                                     01274304
                          PROGRAM('CIOCCA03')                           01274404
                          COMMAREA(WS-COMMAREA)                         01274504
                          RESP(WS-RESP-CODE)                            01274604
                     END-EXEC                                           01274704
                WHEN 'CI06'                                             01274804
                     EXEC CICS XCTL                                     01274904
                          PROGRAM('CIOCCS01')                           01275004
                          RESP(WS-RESP-CODE)                            01275104
                     END-EXEC                                           01275204
                WHEN 'CI09'                                             01275305
                     INITIALIZE WS-COMMAREA                             01275405
                     MOVE 'Y' TO WS-FIRST-SEND                          01275505
                     MOVE 001 TO WS-OPTION                              01275605
                     EXEC CICS XCTL                                     01275705
                          PROGRAM('CIOCCP01')                           01275805
                          COMMAREA(WS-COMMAREA)                         01275905
                          RESP(WS-RESP-CODE)                            01276005
                     END-EXEC                                           01276105
                WHEN 'CI10'                                             01276605
                     INITIALIZE WS-COMMAREA                             01276705
                     MOVE 'Y' TO WS-FIRST-SEND                          01276805
                     MOVE 002 TO WS-OPTION                              01276905
                     EXEC CICS XCTL                                     01277005
                          PROGRAM('CIOCCP01')                           01277105
                          COMMAREA(WS-COMMAREA)                         01277205
                          RESP(WS-RESP-CODE)                            01277305
                     END-EXEC                                           01277405
                WHEN 'CI11'                                             01277905
                     INITIALIZE WS-COMMAREA                             01278006
                     MOVE 'Y' TO WS-FIRST-SEND                          01278106
                     EXEC CICS XCTL                                     01278305
                          PROGRAM('CIOCCP05')                           01278405
                          COMMAREA(WS-COMMAREA)                         01278506
                          RESP(WS-RESP-CODE)                            01278605
                     END-EXEC                                           01278705
            END-EVALUATE                                                01279005
            .                                                           01740000
       3000-MAIN-PROCESS-EXIT.                                          01750000
            EXIT.                                                       01760000
      *                                                                 02590000
       4000-POST-PROCESSING.                                            02600000
      *                                                                 02610000
       4000-POST-PROCESSING-EXIT.                                       02620000
            EXIT.                                                       02630000
      *                                                                 02640000
       5000-CLEAN-UP.                                                   02650000
            EXEC CICS RETURN END-EXEC                                   02660002
            .                                                           02680000
      *                                                                 02690000
       5000-CLEAN-UP-EXIT.                                              02700000
            EXIT.                                                       02710000
      *                                                                 02720000
