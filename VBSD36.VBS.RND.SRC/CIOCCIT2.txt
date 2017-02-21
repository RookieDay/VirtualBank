       IDENTIFICATION DIVISION.                                         00010000
       PROGRAM-ID. CIOCCIT2.                                            00020000
      ***************************************************************** 00030000
      * DMOCCUAD - SERVICE DEMO                                         00040000
      *            CLIENT PROGRAM                                       00050000
      * DEMO HOW TO INTERACT WITH SCREEN AND CALL SERVICE DRIVER        00060000
      *                                                                 00070000
      ***************************************************************** 00150000
       ENVIRONMENT DIVISION.                                            00160000
       DATA DIVISION.                                                   00170000
       WORKING-STORAGE SECTION.                                         00180000
      *                                                                 00190000
       01 WS-RESP-CODE       PIC S9(8) COMP.                            00250000
       01 WS-MESSAGE.                                                   00260000
          05 WS-CODE         PIC X(4).                                  00270000
          05 WS-MSG          PIC X(40).                                 00280000
      *                                                                 00310000
       01 WS-INPUT           PIC X(200).                                00320000
      *                                                                 00420000
       01 WS-SRV-COMMAREA.                                              00430000
      *SERVICE REQUEST/RESPONSE COMMAREA                                00440000
       COPY SD01WS.                                                     00450000
      *                                                                 00451000
       COPY SD11WS.                                                     00460000
      *                                                                 00470000
       LINKAGE SECTION.                                                 00480000
      *                                                                 00520000
       PROCEDURE DIVISION.                                              00530000
       0000-MAINLINE.                                                   00540000
      *                                                                 00550000
            INITIALIZE SDCA-SERVICE-COMMAREA                            00560000
            MOVE 'VBS.CI.CREDCARD.IN1' TO SD-SRV-NAME                   00570003
            MOVE '230105199203230713' TO SD-SRV-INPUT-DATA              00580003
            EXEC CICS                                                   00590000
                 LINK                                                   00600000
                 PROGRAM(WS-PGM-SRV-DRIVER)                             00610000
                 COMMAREA(WS-SRV-COMMAREA)                              00620000
                 RESP(WS-RESP-CODE)                                     00630000
            END-EXEC                                                    00640000
            EVALUATE WS-RESP-CODE                                       00650000
                WHEN DFHRESP(NORMAL)                                    00660000
                     MOVE SD-RESP-CODE            TO WS-CODE            00671000
                     MOVE SD-RESP-ADDITIONAL      TO WS-MSG             00672000
                WHEN OTHER                                              00695000
                     MOVE 9999                    TO WS-CODE            00695100
                     MOVE 'SERVICE PROGRAM ERROR' TO WS-MSG             00696000
            END-EVALUATE                                                00697000
            EXEC CICS                                                   00699000
                 SEND CONTROL                                           00699100
                 CURSOR                                                 00699200
                 ERASE                                                  00699300
                 FREEKB                                                 00699400
                 ALARM                                                  00699500
            END-EXEC                                                    00699600
            EXEC CICS                                                   00699700
                 SEND FROM(WS-MESSAGE)                                  00699800
            END-EXEC                                                    00699900
            EXEC CICS RETURN END-EXEC                                   00700000
            .                                                           00701000
      *                                                                 00710000
       0000-EXIT.                                                       00720000
            EXIT.                                                       00730000
