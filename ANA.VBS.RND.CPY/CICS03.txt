       01  CICS03I.
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
           02  CNAMEL    COMP  PIC  S9(4).
           02  CNAMEF    PICTURE X.
           02  FILLER REDEFINES CNAMEF.
             03 CNAMEA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  CNAMEI  PIC X(30).
           02  IDNUML    COMP  PIC  S9(4).
           02  IDNUMF    PICTURE X.
           02  FILLER REDEFINES IDNUMF.
             03 IDNUMA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  IDNUMI  PIC X(18).
           02  MOBILEL    COMP  PIC  S9(4).
           02  MOBILEF    PICTURE X.
           02  FILLER REDEFINES MOBILEF.
             03 MOBILEA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  MOBILEI  PIC X(11).
           02  BIRTHL    COMP  PIC  S9(4).
           02  BIRTHF    PICTURE X.
           02  FILLER REDEFINES BIRTHF.
             03 BIRTHA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  BIRTHI  PIC X(10).
           02  APPLYIDL    COMP  PIC  S9(4).
           02  APPLYIDF    PICTURE X.
           02  FILLER REDEFINES APPLYIDF.
             03 APPLYIDA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  APPLYIDI  PIC X(13).
           02  IDTYPEL    COMP  PIC  S9(4).
           02  IDTYPEF    PICTURE X.
           02  FILLER REDEFINES IDTYPEF.
             03 IDTYPEA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  IDTYPEI  PIC X(3).
           02  COMNAMEL    COMP  PIC  S9(4).
           02  COMNAMEF    PICTURE X.
           02  FILLER REDEFINES COMNAMEF.
             03 COMNAMEA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  COMNAMEI  PIC X(40).
           02  COMADDL    COMP  PIC  S9(4).
           02  COMADDF    PICTURE X.
           02  FILLER REDEFINES COMADDF.
             03 COMADDA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  COMADDI  PIC X(40).
           02  EMAILL    COMP  PIC  S9(4).
           02  EMAILF    PICTURE X.
           02  FILLER REDEFINES EMAILF.
             03 EMAILA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  EMAILI  PIC X(40).
           02  BILLADDL    COMP  PIC  S9(4).
           02  BILLADDF    PICTURE X.
           02  FILLER REDEFINES BILLADDF.
             03 BILLADDA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  BILLADDI  PIC X(40).
           02  STATUSL    COMP  PIC  S9(4).
           02  STATUSF    PICTURE X.
           02  FILLER REDEFINES STATUSF.
             03 STATUSA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  STATUSI  PIC X(60).
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
       01  CICS03O REDEFINES CICS03I.
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
           02  CNAMEC    PICTURE X.
           02  CNAMEH    PICTURE X.
           02  CNAMEO  PIC X(30).
           02  FILLER PICTURE X(3).
           02  IDNUMC    PICTURE X.
           02  IDNUMH    PICTURE X.
           02  IDNUMO  PIC X(18).
           02  FILLER PICTURE X(3).
           02  MOBILEC    PICTURE X.
           02  MOBILEH    PICTURE X.
           02  MOBILEO  PIC X(11).
           02  FILLER PICTURE X(3).
           02  BIRTHC    PICTURE X.
           02  BIRTHH    PICTURE X.
           02  BIRTHO  PIC X(10).
           02  FILLER PICTURE X(3).
           02  APPLYIDC    PICTURE X.
           02  APPLYIDH    PICTURE X.
           02  APPLYIDO  PIC X(13).
           02  FILLER PICTURE X(3).
           02  IDTYPEC    PICTURE X.
           02  IDTYPEH    PICTURE X.
           02  IDTYPEO  PIC X(3).
           02  FILLER PICTURE X(3).
           02  COMNAMEC    PICTURE X.
           02  COMNAMEH    PICTURE X.
           02  COMNAMEO  PIC X(40).
           02  FILLER PICTURE X(3).
           02  COMADDC    PICTURE X.
           02  COMADDH    PICTURE X.
           02  COMADDO  PIC X(40).
           02  FILLER PICTURE X(3).
           02  EMAILC    PICTURE X.
           02  EMAILH    PICTURE X.
           02  EMAILO  PIC X(40).
           02  FILLER PICTURE X(3).
           02  BILLADDC    PICTURE X.
           02  BILLADDH    PICTURE X.
           02  BILLADDO  PIC X(40).
           02  FILLER PICTURE X(3).
           02  STATUSC    PICTURE X.
           02  STATUSH    PICTURE X.
           02  STATUSO  PIC X(60).
           02  FILLER PICTURE X(3).
           02  MSGC    PICTURE X.
           02  MSGH    PICTURE X.
           02  MSGO  PIC X(79).
           02  FILLER PICTURE X(3).
           02  KEYC    PICTURE X.
           02  KEYH    PICTURE X.
           02  KEYO  PIC X(79).
