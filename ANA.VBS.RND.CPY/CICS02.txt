       01  CICS02I.
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
           02  TITLEI  PIC X(20).
           02  LISTIN OCCURS 14 TIMES.
             05  OPTL    COMP  PIC  S9(4).
             05  OPTF    PICTURE X.
             05  FILLER REDEFINES OPTF.
               10 OPTA    PICTURE X.
             05  FILLER   PICTURE X(2).
             05  OPTI  PIC X(1).
             05  APPIDL    COMP  PIC  S9(4).
             05  APPIDF    PICTURE X.
             05  FILLER REDEFINES APPIDF.
               10 APPIDA    PICTURE X.
             05  FILLER   PICTURE X(2).
             05  APPIDI  PIC X(13).
             05  NAMEL    COMP  PIC  S9(4).
             05  NAMEF    PICTURE X.
             05  FILLER REDEFINES NAMEF.
               10 NAMEA    PICTURE X.
             05  FILLER   PICTURE X(2).
             05  NAMEI  PIC X(15).
             05  APPLL    COMP  PIC  S9(4).
             05  APPLF    PICTURE X.
             05  FILLER REDEFINES APPLF.
               10 APPLA    PICTURE X.
             05  FILLER   PICTURE X(2).
             05  APPLI  PIC X(16).
             05  LASTL    COMP  PIC  S9(4).
             05  LASTF    PICTURE X.
             05  FILLER REDEFINES LASTF.
               10 LASTA    PICTURE X.
             05  FILLER   PICTURE X(2).
             05  LASTI  PIC X(16).
             05  STATL    COMP  PIC  S9(4).
             05  STATF    PICTURE X.
             05  FILLER REDEFINES STATF.
               10 STATA    PICTURE X.
             05  FILLER   PICTURE X(2).
             05  STATI  PIC X(9).
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
       01  CICS02O REDEFINES CICS02I.
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
           02  TITLEO  PIC X(20).
           02  LISTOUT OCCURS 14 TIMES.
             05  FILLER PICTURE X(3).
             05  OPTC    PICTURE X.
             05  OPTH    PICTURE X.
             05  OPTO  PIC X(1).
             05  FILLER PICTURE X(3).
             05  APPIDC    PICTURE X.
             05  APPIDH    PICTURE X.
             05  APPIDO  PIC X(13).
             05  FILLER PICTURE X(3).
             05  NAMEC    PICTURE X.
             05  NAMEH    PICTURE X.
             05  NAMEO  PIC X(15).
             05  FILLER PICTURE X(3).
             05  APPLC    PICTURE X.
             05  APPLH    PICTURE X.
             05  APPLO  PIC X(16).
             05  FILLER PICTURE X(3).
             05  LASTC    PICTURE X.
             05  LASTH    PICTURE X.
             05  LASTO  PIC X(16).
             05  FILLER PICTURE X(3).
             05  STATC    PICTURE X.
             05  STATH    PICTURE X.
             05  STATO  PIC X(9).
           02  FILLER PICTURE X(3).
           02  MSGC    PICTURE X.
           02  MSGH    PICTURE X.
           02  MSGO  PIC X(79).
           02  FILLER PICTURE X(3).
           02  KEYC    PICTURE X.
           02  KEYH    PICTURE X.
           02  KEYO  PIC X(79).
