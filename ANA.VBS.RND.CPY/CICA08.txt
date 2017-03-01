       01  CICA08I.
           02  FILLER PIC X(12).
           02  SYSDL    COMP  PIC  S9(4).
           02  SYSDF    PICTURE X.
           02  FILLER REDEFINES SYSDF.
             03 SYSDA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  SYSDI  PIC X(10).
           02  COMMUL    COMP  PIC  S9(4).
           02  COMMUF    PICTURE X.
           02  FILLER REDEFINES COMMUF.
             03 COMMUA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  COMMUI  PIC X(4).
           02  SYSTL    COMP  PIC  S9(4).
           02  SYSTF    PICTURE X.
           02  FILLER REDEFINES SYSTF.
             03 SYSTA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  SYSTI  PIC X(10).
           02  REVIDL    COMP  PIC  S9(4).
           02  REVIDF    PICTURE X.
           02  FILLER REDEFINES REVIDF.
             03 REVIDA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  REVIDI  PIC X(8).
           02  REVDTL    COMP  PIC  S9(4).
           02  REVDTF    PICTURE X.
           02  FILLER REDEFINES REVDTF.
             03 REVDTA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  REVDTI  PIC X(10).
           02  REVRTL    COMP  PIC  S9(4).
           02  REVRTF    PICTURE X.
           02  FILLER REDEFINES REVRTF.
             03 REVRTA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  REVRTI  PIC X(3).
           02  REVRRL    COMP  PIC  S9(4).
           02  REVRRF    PICTURE X.
           02  FILLER REDEFINES REVRRF.
             03 REVRRA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  REVRRI  PIC X(3).
           02  COMMTL    COMP  PIC  S9(4).
           02  COMMTF    PICTURE X.
           02  FILLER REDEFINES COMMTF.
             03 COMMTA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  COMMTI  PIC X(60).
           02  MSGL    COMP  PIC  S9(4).
           02  MSGF    PICTURE X.
           02  FILLER REDEFINES MSGF.
             03 MSGA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  MSGI  PIC X(79).
           02  KEYL    COMP  PIC  S9(4).
           02  KEYF    PICTURE X.
           02  FILLER REDEFINES KEYF.
             03 KEYA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  KEYI  PIC X(79).
       01  CICA08O REDEFINES CICA08I.
           02  FILLER PIC X(12).
           02  FILLER PICTURE X(3).
           02  SYSDC    PICTURE X.
           02  SYSDH    PICTURE X.
           02  SYSDO  PIC X(10).
           02  FILLER PICTURE X(3).
           02  COMMUC    PICTURE X.
           02  COMMUH    PICTURE X.
           02  COMMUO  PIC X(4).
           02  FILLER PICTURE X(3).
           02  SYSTC    PICTURE X.
           02  SYSTH    PICTURE X.
           02  SYSTO  PIC X(10).
           02  FILLER PICTURE X(3).
           02  REVIDC    PICTURE X.
           02  REVIDH    PICTURE X.
           02  REVIDO  PIC X(8).
           02  FILLER PICTURE X(3).
           02  REVDTC    PICTURE X.
           02  REVDTH    PICTURE X.
           02  REVDTO  PIC X(10).
           02  FILLER PICTURE X(3).
           02  REVRTC    PICTURE X.
           02  REVRTH    PICTURE X.
           02  REVRTO  PIC X(3).
           02  FILLER PICTURE X(3).
           02  REVRRC    PICTURE X.
           02  REVRRH    PICTURE X.
           02  REVRRO  PIC X(3).
           02  FILLER PICTURE X(3).
           02  COMMTC    PICTURE X.
           02  COMMTH    PICTURE X.
           02  COMMTO  PIC X(60).
           02  FILLER PICTURE X(3).
           02  MSGC    PICTURE X.
           02  MSGH    PICTURE X.
           02  MSGO  PIC X(79).
           02  FILLER PICTURE X(3).
           02  KEYC    PICTURE X.
           02  KEYH    PICTURE X.
           02  KEYO  PIC X(79).
