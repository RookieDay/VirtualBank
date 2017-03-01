       01  CICP05I.
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
           02  APPIDL    COMP  PIC  S9(4).
           02  APPIDF    PICTURE X.
           02  FILLER REDEFINES APPIDF.
             03 APPIDA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  APPIDI  PIC X(13).
           02  PRICARDL    COMP  PIC  S9(4).
           02  PRICARDF    PICTURE X.
           02  FILLER REDEFINES PRICARDF.
             03 PRICARDA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  PRICARDI  PIC X(16).
           02  SECCARDL    COMP  PIC  S9(4).
           02  SECCARDF    PICTURE X.
           02  FILLER REDEFINES SECCARDF.
             03 SECCARDA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  SECCARDI  PIC X(16).
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
       01  CICP05O REDEFINES CICP05I.
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
           02  APPIDC    PICTURE X.
           02  APPIDH    PICTURE X.
           02  APPIDO  PIC X(13).
           02  FILLER PICTURE X(3).
           02  PRICARDC    PICTURE X.
           02  PRICARDH    PICTURE X.
           02  PRICARDO  PIC X(16).
           02  FILLER PICTURE X(3).
           02  SECCARDC    PICTURE X.
           02  SECCARDH    PICTURE X.
           02  SECCARDO  PIC X(16).
           02  FILLER PICTURE X(3).
           02  MSGC    PICTURE X.
           02  MSGH    PICTURE X.
           02  MSGO  PIC X(79).
           02  FILLER PICTURE X(3).
           02  KEYC    PICTURE X.
           02  KEYH    PICTURE X.
           02  KEYO  PIC X(79).
