       IDENTIFICATION DIVISION.                                         00010000
       PROGRAM-ID. CIOCCITT.                                            00020092
      ***************************************************************** 00030048
      * DMOCCUAD - SERVICE DEMO                                         00040089
      *            CLIENT PROGRAM                                       00050090
      * DEMO HOW TO INTERACT WITH SCREEN AND CALL SERVICE DRIVER        00060048
      *                                                                 00070048
      ***************************************************************** 00150048
       ENVIRONMENT DIVISION.                                            00160000
       DATA DIVISION.                                                   00170000
       WORKING-STORAGE SECTION.                                         00180000
      *                                                                 00190048
       01 WS-RESP-CODE       PIC S9(8) COMP.                            00250093
       01 WS-MESSAGE.                                                   00260093
          05 WS-CODE         PIC X(4).                                  00270093
          05 WS-MSG          PIC X(40).                                 00280093
      *                                                                 00310048
       01 WS-INPUT           PIC X(200).                                00320093
      *                                                                 00420048
       01 WS-SRV-COMMAREA.                                              00430082
      *SERVICE REQUEST/RESPONSE COMMAREA                                00440048
       COPY SD01WS.                                                     00450048
      *                                                                 00451094
       COPY SD11WS.                                                     00460094
      *                                                                 00470082
       LINKAGE SECTION.                                                 00480000
      *                                                                 00520082
       PROCEDURE DIVISION.                                              00530000
       0000-MAINLINE.                                                   00540048
      *                                                                 00550048
            INITIALIZE SDCA-SERVICE-COMMAREA                            00560092
            MOVE 'VBS.CI.APPLICAN.IN2' TO SD-SRV-NAME                   00570098
            MOVE '001' TO SD-SRV-INPUT-DATA                             00580098
            EXEC CICS                                                   00590092
                 LINK                                                   00600092
                 PROGRAM(WS-PGM-SRV-DRIVER)                             00610092
                 COMMAREA(WS-SRV-COMMAREA)                              00620092
                 RESP(WS-RESP-CODE)                                     00630092
            END-EXEC                                                    00640092
            EVALUATE WS-RESP-CODE                                       00650092
                WHEN DFHRESP(NORMAL)                                    00660092
                     MOVE SD-RESP-CODE            TO WS-CODE            00671093
                     MOVE SD-RESP-ADDITIONAL      TO WS-MSG             00672094
                WHEN OTHER                                              00695092
                     MOVE 9999                    TO WS-CODE            00695193
                     MOVE 'SERVICE PROGRAM ERROR' TO WS-MSG             00696094
            END-EVALUATE                                                00697092
            EXEC CICS                                                   00699093
                 SEND CONTROL                                           00699193
                 CURSOR                                                 00699293
                 ERASE                                                  00699393
                 FREEKB                                                 00699493
                 ALARM                                                  00699593
            END-EXEC                                                    00699693
            EXEC CICS                                                   00699793
                 SEND FROM(WS-MESSAGE)                                  00699893
            END-EXEC                                                    00699993
            EXEC CICS RETURN END-EXEC                                   00700092
            .                                                           00701082
      *                                                                 00710048
       0000-EXIT.                                                       00720048
            EXIT.                                                       00730082
