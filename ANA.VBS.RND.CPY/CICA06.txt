       01  CICA06I.
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
           02  COMPLL    COMP  PIC  S9(4).
           02  COMPLF    PICTURE X.
           02  FILLER REDEFINES COMPLF.
             03 COMPLA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  COMPLI  PIC X(8).
           02  COMPRL    COMP  PIC  S9(4).
           02  COMPRF    PICTURE X.
           02  FILLER REDEFINES COMPRF.
             03 COMPRA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  COMPRI  PIC X(3).
           02  REFUR1L    COMP  PIC  S9(4).
           02  REFUR1F    PICTURE X.
           02  FILLER REDEFINES REFUR1F.
             03 REFUR1A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  REFUR1I  PIC X(3).
           02  CREDIL    COMP  PIC  S9(4).
           02  CREDIF    PICTURE X.
           02  FILLER REDEFINES CREDIF.
             03 CREDIA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  CREDII  PIC X(8).
           02  CREDDL    COMP  PIC  S9(4).
           02  CREDDF    PICTURE X.
           02  FILLER REDEFINES CREDDF.
             03 CREDDA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  CREDDI  PIC X(10).
           02  CREDRL    COMP  PIC  S9(4).
           02  CREDRF    PICTURE X.
           02  FILLER REDEFINES CREDRF.
             03 CREDRA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  CREDRI  PIC X(3).
           02  REFUR2L    COMP  PIC  S9(4).
           02  REFUR2F    PICTURE X.
           02  FILLER REDEFINES REFUR2F.
             03 REFUR2A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  REFUR2I  PIC X(3).
           02  COMMTL    COMP  PIC  S9(4).
           02  COMMTF    PICTURE X.
           02  FILLER REDEFINES COMMTF.
             03 COMMTA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  COMMTI  PIC X(60).
           02  FINLL    COMP  PIC  S9(4).
           02  FINLF    PICTURE X.
           02  FILLER REDEFINES FINLF.
             03 FINLA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  FINLI  PIC X(8).
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
       01  CICA06O REDEFINES CICA06I.
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
           02  COMPLC    PICTURE X.
           02  COMPLH    PICTURE X.
           02  COMPLO  PIC X(8).
           02  FILLER PICTURE X(3).
           02  COMPRC    PICTURE X.
           02  COMPRH    PICTURE X.
           02  COMPRO  PIC X(3).
           02  FILLER PICTURE X(3).
           02  REFUR1C    PICTURE X.
           02  REFUR1H    PICTURE X.
           02  REFUR1O  PIC X(3).
           02  FILLER PICTURE X(3).
           02  CREDIC    PICTURE X.
           02  CREDIH    PICTURE X.
           02  CREDIO  PIC X(8).
           02  FILLER PICTURE X(3).
           02  CREDDC    PICTURE X.
           02  CREDDH    PICTURE X.
           02  CREDDO  PIC X(10).
           02  FILLER PICTURE X(3).
           02  CREDRC    PICTURE X.
           02  CREDRH    PICTURE X.
           02  CREDRO  PIC X(3).
           02  FILLER PICTURE X(3).
           02  REFUR2C    PICTURE X.
           02  REFUR2H    PICTURE X.
           02  REFUR2O  PIC X(3).
           02  FILLER PICTURE X(3).
           02  COMMTC    PICTURE X.
           02  COMMTH    PICTURE X.
           02  COMMTO  PIC X(60).
           02  FILLER PICTURE X(3).
           02  FINLC    PICTURE X.
           02  FINLH    PICTURE X.
           02  FINLO  PIC X(8).
           02  FILLER PICTURE X(3).
           02  MSGC    PICTURE X.
           02  MSGH    PICTURE X.
           02  MSGO  PIC X(79).
           02  FILLER PICTURE X(3).
           02  KEYC    PICTURE X.
           02  KEYH    PICTURE X.
           02  KEYO  PIC X(79).
