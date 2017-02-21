       01  CICP00I.
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
           02  MENUL    COMP  PIC  S9(4).
           02  MENUF    PICTURE X.
           02  FILLER REDEFINES MENUF.
             03 MENUA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  MENUI  PIC X(4).
           02  OPT1L    COMP  PIC  S9(4).
           02  OPT1F    PICTURE X.
           02  FILLER REDEFINES OPT1F.
             03 OPT1A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  OPT1I  PIC X(1).
           02  OPT2L    COMP  PIC  S9(4).
           02  OPT2F    PICTURE X.
           02  FILLER REDEFINES OPT2F.
             03 OPT2A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  OPT2I  PIC X(1).
           02  OPT3L    COMP  PIC  S9(4).
           02  OPT3F    PICTURE X.
           02  FILLER REDEFINES OPT3F.
             03 OPT3A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  OPT3I  PIC X(1).
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
       01  CICP00O REDEFINES CICP00I.
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
           02  MENUC    PICTURE X.
           02  MENUH    PICTURE X.
           02  MENUO  PIC X(4).
           02  FILLER PICTURE X(3).
           02  OPT1C    PICTURE X.
           02  OPT1H    PICTURE X.
           02  OPT1O  PIC X(1).
           02  FILLER PICTURE X(3).
           02  OPT2C    PICTURE X.
           02  OPT2H    PICTURE X.
           02  OPT2O  PIC X(1).
           02  FILLER PICTURE X(3).
           02  OPT3C    PICTURE X.
           02  OPT3H    PICTURE X.
           02  OPT3O  PIC X(1).
           02  FILLER PICTURE X(3).
           02  MSGC    PICTURE X.
           02  MSGH    PICTURE X.
           02  MSGO  PIC X(79).
           02  FILLER PICTURE X(3).
           02  KEYC    PICTURE X.
           02  KEYH    PICTURE X.
           02  KEYO  PIC X(79).
