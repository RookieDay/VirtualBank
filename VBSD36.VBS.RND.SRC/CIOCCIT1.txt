       IDENTIFICATION DIVISION.                                         00010000
       PROGRAM-ID. CIOCCIT1.                                            00020013
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
      *2015-01-06    KEVIN      INITIAL VERSION                         00130000
      ***************************************************************** 00150000
       ENVIRONMENT DIVISION.                                            00160000
       DATA DIVISION.                                                   00170000
       WORKING-STORAGE SECTION.                                         00180000
      *                                                                 00190000
       01 WS-COMMAREA.                                                  00451007
          05 WS-INPUT              PIC X(130).                          00453007
       01 WS-RESP-CODE             PIC S9(8) COMP.                      00454008
      *                                                                 00470000
       LINKAGE SECTION.                                                 00480000
      *                                                                 00520000
       PROCEDURE DIVISION.                                              00530000
       0000-MAINLINE.                                                   00540000
      *                                                                 00550000
            INITIALIZE WS-COMMAREA                                      00550107
            MOVE 'Y230105199203230713' TO WS-INPUT                      00550212
            EXEC CICS                                                   00551001
                 XCTL PROGRAM('CIOCCP03')                               00552011
                 COMMAREA(WS-COMMAREA)                                  00552102
                 RESP(WS-RESP-CODE)                                     00553001
            END-EXEC                                                    00554001
      *                                                                 00554107
            EXEC CICS                                                   00555007
                 RETURN                                                 00556007
            END-EXEC                                                    00559007
            .                                                           00700000
      *                                                                 00710000
       0000-EXIT.                                                       00720000
            EXIT.                                                       00730000
      *                                                                 00740000
