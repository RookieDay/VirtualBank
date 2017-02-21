       01  CICA05I.
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
           02  INVIDL    COMP  PIC  S9(4).
           02  INVIDF    PICTURE X.
           02  FILLER REDEFINES INVIDF.
             03 INVIDA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  INVIDI  PIC X(8).
           02  INVDTL    COMP  PIC  S9(4).
           02  INVDTF    PICTURE X.
           02  FILLER REDEFINES INVDTF.
             03 INVDTA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  INVDTI  PIC X(10).
           02  HAACCL    COMP  PIC  S9(4).
           02  HAACCF    PICTURE X.
           02  FILLER REDEFINES HAACCF.
             03 HAACCA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  HAACCI  PIC X(3).
           02  HDCCDL    COMP  PIC  S9(4).
           02  HDCCDF    PICTURE X.
           02  FILLER REDEFINES HDCCDF.
             03 HDCCDA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  HDCCDI  PIC X(3).
           02  CDTHSYL    COMP  PIC  S9(4).
           02  CDTHSYF    PICTURE X.
           02  FILLER REDEFINES CDTHSYF.
             03 CDTHSYA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  CDTHSYI  PIC X(3).
           02  INVRLTL    COMP  PIC  S9(4).
           02  INVRLTF    PICTURE X.
           02  FILLER REDEFINES INVRLTF.
             03 INVRLTA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  INVRLTI  PIC X(3).
           02  RFERSNL    COMP  PIC  S9(4).
           02  RFERSNF    PICTURE X.
           02  FILLER REDEFINES RFERSNF.
             03 RFERSNA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  RFERSNI  PIC X(3).
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
       01  CICA05O REDEFINES CICA05I.
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
           02  INVIDC    PICTURE X.
           02  INVIDH    PICTURE X.
           02  INVIDO  PIC X(8).
           02  FILLER PICTURE X(3).
           02  INVDTC    PICTURE X.
           02  INVDTH    PICTURE X.
           02  INVDTO  PIC X(10).
           02  FILLER PICTURE X(3).
           02  HAACCC    PICTURE X.
           02  HAACCH    PICTURE X.
           02  HAACCO  PIC X(3).
           02  FILLER PICTURE X(3).
           02  HDCCDC    PICTURE X.
           02  HDCCDH    PICTURE X.
           02  HDCCDO  PIC X(3).
           02  FILLER PICTURE X(3).
           02  CDTHSYC    PICTURE X.
           02  CDTHSYH    PICTURE X.
           02  CDTHSYO  PIC X(3).
           02  FILLER PICTURE X(3).
           02  INVRLTC    PICTURE X.
           02  INVRLTH    PICTURE X.
           02  INVRLTO  PIC X(3).
           02  FILLER PICTURE X(3).
           02  RFERSNC    PICTURE X.
           02  RFERSNH    PICTURE X.
           02  RFERSNO  PIC X(3).
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