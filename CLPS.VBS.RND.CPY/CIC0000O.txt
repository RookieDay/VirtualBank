       01 CIC0000O-REC.
          05 CIC0000O-COUNT        PIC 9(3).
          05 CIC0000O-CARD.
             10 CIC0000O-CARD-MESS OCCURS 10 TIMES.
                15 CIC0000O-PROD-TYPE  PIC 9(03).
                15 CIC0000O-CARD-NUMB  PIC 9(16).
                15 CIC0000O-STATUS     PIC 9(03).
