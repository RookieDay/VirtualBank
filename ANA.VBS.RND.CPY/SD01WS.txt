         05 SDCA-SERVICE-COMMAREA.
      *****************************************************************
      * SERVICE COMMAREA
      *****************************************************************
           10 SERVICE-IO.
             15 SD-GENERAL-CONTEXT.
               20 SD-MESSAGE-ID         PIC  X(06).
               20 SD-RESP-CODE          PIC  X(04).
               20 SD-RESP-ADDITIONAL    PIC  X(40).
             15 SD-SRV-NAME.
               20 SD-SRV-SYS            PIC  X(03).
               20 FILLER                PIC  X(01).
               20 SD-SRV-MODULE         PIC  X(02).
               20 FILLER                PIC  X(01).
               20 SD-SRV-ALIAS          PIC  X(08).
               20 FILLER                PIC  X(01).
               20 SD-SRV-FUNC           PIC  X(03).
             15 SD-SRV-INPUT-DATA       PIC  X(1024).
             15 SD-SRV-OUTPUT-DATA      PIC  X(1024).
