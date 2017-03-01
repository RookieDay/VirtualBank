       01 ATTRB.
          05 ATTR-UNPROT                 PIC X   VALUE ' '.
          05 ATTR-UNPROT-MDT             PIC X   VALUE X'C1'.
          05 ATTR-UNPROT-BRT             PIC X   VALUE X'C8'.
          05 ATTR-UNPROT-BRT-MDT         PIC X   VALUE X'C9'.
          05 ATTR-PROT                   PIC X   VALUE X'60'.
          05 ATTR-PROT-MDT               PIC X   VALUE X'61'.
          05 ATTR-PROT-DARK              PIC X   VALUE '%'.
          05 ATTR-PROT-BRT               PIC X   VALUE X'E8'.
          05 ATTR-PROT-BRT-MDT           PIC X   VALUE X'E9'.
          05 ATTR-PROT-SKIP              PIC X   VALUE X'F0'.
          05 ATTR-PROT-SKIP-MDT          PIC X   VALUE X'F1'.
          05 ATTR-PROT-SKIP-BRT          PIC X   VALUE X'F8'.
          05 ATTR-PROT-SKIP-BRT-MDT      PIC X   VALUE X'F9'.
      *****************************************************************
      * PROGRAM ROUNTER COMMAREA
      *****************************************************************
       01 PROGRAM-NAME.
          05 WS-PGM-SRV-DRIVER           PIC  X(08) VALUE 'SMOSDDRV'.
