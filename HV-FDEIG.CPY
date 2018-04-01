      **************************************** Hausverwaltungsprogramm *
       FD  EIGENTUM     external       LABEL RECORDS STANDARD.
       01  EG-SATZ.
           03  EG-KEY.
               05 EG-KTONR             PIC 9(4)      COMP.
           03  EG-MCODE                PIC X(8).
           03  EG-BEZ                  PIC X(110).
           03  EG-BANK.
               05 EG-IBAN              PIC X(4).
               05 EG-BKLZL             PIC 9(6)      COMP.
               05 EG-BKKTO             PIC X(14).
               05 EG-BANKNAME          PIC X(20).
           03  EG-ANMERK               PIC X(60).
           03  EG-ANTEILW              PIC 9(5)      COMP.
           03  EG-ANTEILG              PIC 9(5)      COMP.
           03  EG-NUTZFL               PIC 999V99    COMP.
           03  EG-WASSER               PIC 9(6)V99   COMP.
           03  EG-VS         OCCURS 5.
               05 EG-VSDAT             PIC 9(8)V99   COMP.
               05 EG-BKH               PIC 9(4)V99   COMP.
               05 EG-USTH              PIC 9(4)V99   COMP.
               05 EG-BKG               PIC 9(4)V99   COMP.
               05 EG-USTG              PIC 9(4)V99   COMP.
               05 EG-RLAG              PIC 9(4)V99   COMP.
           03  EG-UEBDAT               PIC 9(8)      COMP.
           03  EG-REDAT                PIC 9(6)      COMP.
           03  EG-SALDO                PIC S9(6)V99  COMP.
