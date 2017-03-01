       01  CICP06I.
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
           02  TITLEI  PIC X(25).
           02  CARDTYL    COMP  PIC  S9(4).
           02  CARDTYF    PICTURE X.
           02  FILLER REDEFINES CARDTYF.
             03 CARDTYA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  CARDTYI  PIC X(3).
           02  LISTIN OCCURS 10 TIMES.
               05  OPTL    COMP  PIC  S9(4).
               05  OPTF    PICTURE X.
               05  FILLER REDEFINES OPTF.
                   10 OPTA    PICTURE X.
               05  FILLER   PICTURE X(2).
               05  OPTI  PIC X(1).
               05  CARDL    COMP  PIC  S9(4).
               05  CARDF    PICTURE X.
               05  FILLER REDEFINES CARDF.
                   10 CARDA    PICTURE X.
               05  FILLER   PICTURE X(2).
               05  CARDI  PIC X(16).
               05  STATUSL    COMP  PIC  S9(4).
               05  STATUSF    PICTURE X.
               05  FILLER REDEFINES STATUSF.
                   10 STATUSA    PICTURE X.
               05  FILLER   PICTURE X(2).
               05  STATUSI  PIC X(3).
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
       01  CICP06O REDEFINES CICP06I.
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
           02  TITLEO  PIC X(25).
           02  FILLER PICTURE X(3).
           02  CARDTYC    PICTURE X.
           02  CARDTYH    PICTURE X.
           02  CARDTYO  PIC X(3).
           02  LISTOUT OCCURS 14 TIMES.
               05  FILLER PICTURE X(3).
               05  OPTC    PICTURE X.
               05  OPTH    PICTURE X.
               05  OPTO  PIC X(1).
               05  FILLER PICTURE X(3).
               05  CARDC    PICTURE X.
               05  CARDH    PICTURE X.
               05  CARDO  PIC X(16).
               05  FILLER PICTURE X(3).
               05  STATUSC    PICTURE X.
               05  STATUSH    PICTURE X.
               05  STATUSO  PIC X(3).
           02  FILLER PICTURE X(3).
           02  MSGC    PICTURE X.
           02  MSGH    PICTURE X.
           02  MSGO  PIC X(79).
           02  FILLER PICTURE X(3).
           02  KEYC    PICTURE X.
           02  KEYH    PICTURE X.
           02  KEYO  PIC X(79).
