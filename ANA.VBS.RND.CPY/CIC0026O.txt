         01 CIC0026O-REC.
            05 CIC0026O-COUNT                PIC 9(03).
            05 CIC0026O-KEY                  OCCURS 14 TIMES.
               10 CIC0026O-CARD-NUMB         PIC 9(16).
               10 CIC0026O-TIMESTAMP         PIC 9(14).
               10 CIC0026O-AUTH-CODE         PIC 9(06).
