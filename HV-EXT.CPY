       01  WE-EXT       external.
           03  WK-UID.
               05 WK-UIDBEZ            PIC XXX.
               05 WK-UIDNUM            PIC 9(8)       COMP.
           03  WE-DSTG.
               05 WE-STG               PIC X(40)     OCCURS 10.
           03  WH-KEY                  PIC 9(5).
           03  WH-BUKEY                PIC 9(5).
           03  WH-BUZEI                PIC X(5).
           03  WE-USER                 PIC XX.
           03  WE-FNR                  PIC 99       COMP.
                 88 ZENTRAL  VALUE 1 THRU 6.
           03  WE-AUF                  PIC 99       COMP.
           03  WM-DRU                  PIC 99       COMP.
           03  WM-SIZE                 PIC 99       COMP.
           03  WM-RE                   PIC 99       COMP.
           03  WM-NEU                  PIC 99       COMP.
           03  WH-TX                   PIC X(51).
           03  WH-PROTNAM.
               05 WH-PN                PIC X(5).
               05 WH-PER               PIC 999.
               05 FILLER               PIC X(4).
           03  WH-RENUM                PIC 9(6)      COMP.
           03  WS-LF                   PIC 9999      COMP.
           03  WA-LG                   PIC 9999      COMP.
           03  WE-DATUM                PIC 9(6)      COMP.
           03  WE-KTONR                PIC 9(6)      COMP.
           03  WE-SA                   PIC X.
           03  WE-VKF                  PIC 99        COMP.
           03  WH-KART                 PIC 9999      COMP.
           03  WL-ZEILEN.
               05 WT-LINE              PIC X(40)   OCCURS 30.
