       01  CICA04I.
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
           02  CHKIDL    COMP  PIC  S9(4).
           02  CHKIDF    PICTURE X.
           02  FILLER REDEFINES CHKIDF.
             03 CHKIDA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  CHKIDI  PIC X(8).
           02  CHKDTL    COMP  PIC  S9(4).
           02  CHKDTF    PICTURE X.
           02  FILLER REDEFINES CHKDTF.
             03 CHKDTA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  CHKDTI  PIC X(10).
           02  CHKRTL    COMP  PIC  S9(4).
           02  CHKRTF    PICTURE X.
           02  FILLER REDEFINES CHKRTF.
             03 CHKRTA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  CHKRTI  PIC X(3).
           02  CHKRRL    COMP  PIC  S9(4).
           02  CHKRRF    PICTURE X.
           02  FILLER REDEFINES CHKRRF.
             03 CHKRRA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  CHKRRI  PIC X(3).
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
       01  CICA04O REDEFINES CICA04I.
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
           02  CHKIDC    PICTURE X.
           02  CHKIDH    PICTURE X.
           02  CHKIDO  PIC X(8).
           02  FILLER PICTURE X(3).
           02  CHKDTC    PICTURE X.
           02  CHKDTH    PICTURE X.
           02  CHKDTO  PIC X(10).
           02  FILLER PICTURE X(3).
           02  CHKRTC    PICTURE X.
           02  CHKRTH    PICTURE X.
           02  CHKRTO  PIC X(3).
           02  FILLER PICTURE X(3).
           02  CHKRRC    PICTURE X.
           02  CHKRRH    PICTURE X.
           02  CHKRRO  PIC X(3).
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