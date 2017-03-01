       01  CICS01I.
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
           02  IDTYPEL    COMP  PIC  S9(4).
           02  IDTYPEF    PICTURE X.
           02  FILLER REDEFINES IDTYPEF.
             03 IDTYPEA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  IDTYPEI  PIC X(3).
           02  IDNUML    COMP  PIC  S9(4).
           02  IDNUMF    PICTURE X.
           02  FILLER REDEFINES IDNUMF.
             03 IDNUMA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  IDNUMI  PIC X(18).
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
       01  CICS01O REDEFINES CICS01I.
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
           02  IDTYPEC    PICTURE X.
           02  IDTYPEH    PICTURE X.
           02  IDTYPEO  PIC X(3).
           02  FILLER PICTURE X(3).
           02  IDNUMC    PICTURE X.
           02  IDNUMH    PICTURE X.
           02  IDNUMO  PIC X(18).
           02  FILLER PICTURE X(3).
           02  MSGC    PICTURE X.
           02  MSGH    PICTURE X.
           02  MSGO  PIC X(79).
           02  FILLER PICTURE X(3).
           02  KEYC    PICTURE X.
           02  KEYH    PICTURE X.
           02  KEYO  PIC X(79).
