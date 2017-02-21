         05 SDCA-SERVICE-COMMAREA.
      *****************************************************************
      * SERVICE COMMAREA
      *****************************************************************
           10 SERVICE-IO.
             15 CD-GENERAL-CONTEXT.
               20 CD-MESSAGE-ID         PIC  X(06).
               20 CD-RESP-CODE          PIC  X(04).
               20 CD-RESP-ADDITIONAL    PIC  X(40).
             15 CD-SRV-INPUT-DATA       PIC  X(1024).
             15 CD-SRV-OUTPUT-DATA      PIC  X(1024).
