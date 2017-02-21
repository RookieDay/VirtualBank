       01  CICP03I.
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
           02  TITLEL    COMP  PIC  S9(4).
           02  TITLEF    PICTURE X.
           02  FILLER REDEFINES TITLEF.
             03 TITLEA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  TITLEI  PIC X(28).
           02  LISTIN OCCURS 14 TIMES.
               05  OPTL    COMP  PIC  S9(4).
               05  OPTF    PICTURE X.
               05  FILLER REDEFINES OPTF.
                 10 OPTA    PICTURE X.
               05  FILLER   PICTURE X(2).
               05  OPTI  PIC X(1).
               05  CADIDL    COMP  PIC  S9(4).
               05  CADIDF    PICTURE X.
               05  FILLER REDEFINES CADIDF.
                 10 CADIDA    PICTURE X.
               05  FILLER   PICTURE X(2).
               05  CADIDI  PIC X(16).
               05  PRTIDL    COMP  PIC  S9(4).
               05  PRTIDF    PICTURE X.
               05  FILLER REDEFINES PRTIDF.
                 10 PRTIDA    PICTURE X.
               05  FILLER   PICTURE X(2).
               05  PRTIDI  PIC X(13).
               05  STATSL    COMP  PIC  S9(4).
               05  STATSF    PICTURE X.
               05  FILLER REDEFINES STATSF.
                 10 STATSA    PICTURE X.
               05  FILLER   PICTURE X(2).
               05  STATSI  PIC X(11).
               05  EXTIMEL    COMP  PIC  S9(4).
               05  EXTIMEF    PICTURE X.
               05  FILLER REDEFINES EXTIMEF.
                 10 EXTIMEA    PICTURE X.
               05  FILLER   PICTURE X(2).
               05  EXTIMEI  PIC X(10).
               05  LTIMEL    COMP  PIC  S9(4).
               05  LTIMEF    PICTURE X.
               05  FILLER REDEFINES LTIMEF.
                 10 LTIMEA    PICTURE X.
               05  FILLER   PICTURE X(2).
               05  LTIMEI  PIC X(10).
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
       01  CICP03O REDEFINES CICP03I.
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
           02  TITLEC    PICTURE X.
           02  TITLEH    PICTURE X.
           02  TITLEO  PIC X(28).
           02  LISTOUT OCCURS 14 TIMES.
               05  FILLER PICTURE X(3).
               05  OPTC      PICTURE X.
               05  OPTH      PICTURE X.
               05  OPTO      PIC X(1).
               05  FILLER PICTURE X(3).
               05  CADIDC    PICTURE X.
               05  CADIDH    PICTURE X.
               05  CADIDO    PIC X(16).
               05  FILLER PICTURE X(3).
               05  PRTIDC    PICTURE X.
               05  PRTIDH    PICTURE X.
               05  PRTIDO    PIC X(13).
               05  FILLER PICTURE X(3).
               05  STATSC    PICTURE X.
               05  STATSH    PICTURE X.
               05  STATSO    PIC X(11).
               05  FILLER PICTURE X(3).
               05  EXTIMEC   PICTURE X.
               05  EXTIMEH   PICTURE X.
               05  EXTIMEO   PIC X(10).
               05  FILLER PICTURE X(3).
               05  LTIMEC    PICTURE X.
               05  LTIMEH    PICTURE X.
               05  LTIMEO    PIC X(10).
           02  FILLER PICTURE X(3).
           02  MSGC    PICTURE X.
           02  MSGH    PICTURE X.
           02  MSGO  PIC X(79).
           02  FILLER PICTURE X(3).
           02  KEYC    PICTURE X.
           02  KEYH    PICTURE X.
           02  KEYO  PIC X(79).
