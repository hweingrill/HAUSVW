      $SET LINKCOUNT"480" ANS85 BOUND AUTOLOCK
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     HV-VORL.
      ******************************************************************
       ENVIRONMENT    DIVISION.
       CONFIGURATION   SECTION.
       SOURCE-COMPUTER.     PC.
       SPECIAL-NAMES.    DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY HV-SEEIG.CPY.
           SELECT SECDAT    ASSIGN TO DATEI
                            ORGANIZATION SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       COPY HV-FDEIG.CPY.
      ************************************************* Security-Datei *
       FD  SECDAT.
       01  SEC-SATZ                    PIC X(128).
      ******************************************************************
       WORKING-STORAGE SECTION.
       01  WH-CALL.
           03  WL-CA                   PIC 99.
           03  WL-ECK                  PIC 9999.
           03  FILLER REDEFINES WL-ECK.
               05  WL-AZ               PIC 99.
               05  WL-VL               PIC 99.
           03  WL-GROSS                PIC 9999.
           03  FILLER REDEFINES WL-GROSS.
               05  WL-VP               PIC 99.
               05  WL-SZ               PIC 99.
           03  WL-KO                   PIC 99.
           03  WL-MA                   PIC 9.
           03  WL-ATTR                 PIC XX.
       COPY WHCREG.CPY.
       01  WK-DRSTG     external.
           03  WK-XTX                  PIC X(35)   OCCURS 17.
           03  WX-LPT                  PIC X(8).
       01  WN-REG       external.
           03  WN-LBHK                 PIC X(20).
           03  WN-LBHZ                 PIC X(20).
       01  WF-REG.
           03  WA-HIDAT                PIC 9(8)     COMP.
           03  WV-HIDAT                PIC 9(8)     COMP.
           03  WD-POS                  PIC ZZ9.
           03  WD-KZ                   PIC 9.
           03  WH-PX                   PIC XX       OCCURS 2.
           03  WH-ART                  PIC 99.
           03  WH-SEC                  PIC 9(14)    COMP-X.
           03  WV-X                    PIC X.
           03  WE-EXTN                 PIC X(7).
           03  WV-NUM.
               05 WV-NR                PIC 999.
           03  WD-DPAR                 PIC X(20).
           03  WD-NUM                  PIC ZZ.ZZ9.
       COPY HV-EXT.CPY.
      *----------------------------------------------------------------*
       LINKAGE SECTION.
       01  WL-CALL                     PIC X(15).
       01  WL-CREG                     PIC X(1152).
       PROCEDURE DIVISION using WL-CALL WL-CREG.
       DECLARATIVES.
       DECL-A SECTION.         USE AFTER ERROR PROCEDURE ON EIGENTUM.
       A.  CALL "CADECL" USING "ANBARTIK.DAT" WH-CREG.
       Z.  EXIT.
       END DECLARATIVES.
      ******************************************************************
       STEUER SECTION.
       A.  MOVE WL-CALL TO WH-CALL.
           MOVE WL-CREG TO WH-CREG.
           PERFORM VORLAUF.
       X.  MOVE WH-CREG TO WL-CREG.
       Z.  EXIT PROGRAM.
      ******************************************************************
       VORLAUF SECTION.
       A.  MOVE "  Hausverwaltung 1.0" TO WK-GEB.
           IF WH-PG = 9 GO B.
           PERFORM LAD-DRUCK.
           MOVE 0101 TO VDU-ECK.
           ACCEPT WZ-DATUM FROM DATE.
           CALL "CAUP" USING "03DATUM" WH-CREG.
           MOVE WH-DATUM TO WM-DATUM.
           PERFORM VARYING WX FROM 40 BY -1 UNTIL
               WT-TX(WX:1) NOT = SPACE CONTINUE.
           ADD -1 TO WX.
           OPEN I-O EIGENTUM.
           IF WF-STATUS not = "05" MOVE "HV-EIGEN.DAT" TO DATEI
               PERFORM SEC.
           PERFORM AUTOLOESCH.
           MOVE SPACE TO WX-CODE(1) WX-CODE(2).
       B.  MOVE 1 TO WH-KEY.
           READ KONSTANT IGNORE LOCK NOT INVALID GO J.
           IF ZUGRIF PERFORM BESETZT GO B.
           CALL "CAUP" USING "06NKOPF" WH-CREG.
           DISPLAY WD-DPAR(1:9) AT 0172.
           DISPLAY "Bitte Kopf anlegen!" with highlight AT 0426.
           INITIALIZE KO-SATZ.
       C.  MOVE KO-FIRMA TO WT-TX.
           DISPLAY KO-FIRMA AT 0127.
           CALL "CAUP" USING "0201270122" WH-CREG.
           MOVE WT-TX TO KO-FIRMA.
           IF ESC STOP RUN.
           MOVE 20 TO KO-UST(1).
           MOVE 10 TO KO-UST(2).
           MOVE 30 TO KO-UST(3).
           MOVE 0 TO KO-UST(4) KO-UST(5) KO-UST(6).
           MOVE "Graz," TO KO-ORT.
           MOVE 1 TO KO-RENUM KO-ABNUM KO-NUM WH-KEY.
           WRITE KO-SATZ.
           GO B.
       J.  UNLOCK KONSTANT.
           MOVE KO-FIRMA TO WK-FIRMA.
           MOVE KO-SPERRE TO WX-CODE(2).
           MOVE KO-ORT TO WT-TX.
           MOVE KO-DRU TO WM-DRU.
           MOVE KO-ZEIL TO WM-NEU.
           PERFORM VARYING TY FROM 15 BY -1 UNTIL TY < 1 OR
               WR-TX(TY) NOT = SPACE CONTINUE.
           PERFORM VARYING FY FROM 15 BY -1 UNTIL TY < 1
               MOVE WR-TX(TY) TO WR-TX(FY)
               SET TY DOWN BY 1.
           MOVE WT-TX TO VDU-ORT.
           MOVE WH-DATUM TO WM-DATUM WZ-DATUM.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 6
               MOVE KO-UST(WX) TO WT-UST(WX).
           MOVE 0 TO WE-AKK WE-AUFPZ.
           MOVE WM-DATUM TO WV-HIDAT.
           IF WM-DATUM < 500000 ADD 20000000 TO WV-HIDAT
                           else ADD 19000000 TO WV-HIDAT.
           PERFORM VARYING WX FROM 2 BY -1 UNTIL WX = 0
                IF KO-AKKDAT(WX) not = 0
                    MOVE KO-AKKDAT(WX) TO WA-HIDAT
                    IF KO-AKKDAT(WX) < 500000 ADD 20000000 TO WA-HIDAT
                                         else ADD 19000000 TO WA-HIDAT
                    end-if
                    IF WA-HIDAT < WV-HIDAT exit perform.
           IF WX > 0 MOVE KO-AKKSATZ(WX) TO WE-AKK
                     MOVE KO-LNK(WX) TO WE-LNK
                     MOVE KO-AUFPZ(WX) TO WE-AUFPZ.
           CALL "CAUP" USING "06NKOPF" WH-CREG.
           DISPLAY WD-DPAR(1:9) AT 0172.
       P.  IF WX-CODE(2) = WX-CODE(1) GO Q.
           DISPLAY "<esc>= Abbruch" AT 2401.
           MOVE 99 TO WH-PG.
           DISPLAY "Codeworteingabe: " AT 2301.
           MOVE SPACE TO WT-TX.
           CALL "CAUP" USING "0223180106" WH-CREG.
           MOVE 9 TO WH-PG.
           IF ESC GO R.
           MOVE WT-TX TO WX-CODE(1).
           GO P.
       Q.  DISPLAY "<ret-leer>/<esc>= Programmende" AT 2301.
           DISPLAY "Programmwahl"        AT 0524
                   "ƒƒƒƒƒƒƒƒƒƒƒƒ"        AT 0024
                   "1 - Abrechnung"      AT 0824
                   "2 -            "     AT 0024
                   "3 - Druckprogramm"   AT 1124
                   "4 - Stammdaten"      AT 0024
                   "5 - Listung"         AT 1424
                   "6 - Datum"           AT 0024
                  " "                    AT 0023
                   "bitte wÑhlen Sie:   " with highlight AT 1839.
           CALL "CAUP" USING "0018572002" WH-CREG.
       R.  IF ESC MOVE 0 TO WH-NUM WH-PG
               SET RET TO TRUE.
           IF NOT RET GO Q.
           MOVE WH-NUM TO WH-PG.
           EVALUATE WH-PG
               WHEN 0 CLOSE OENORM MODULE AUFKOPF AUFZEIL
                                          LBHKOPF LBHZEIL KONSTANT
                      CALL "CAUP" USING "1301012480000" WH-CREG
                      DISPLAY " Programm beendet " AT 1231
                      STOP RUN
               WHEN 99
               WHEN 1 MOVE "ANBOERF 01ANBOT"  TO WT-TX
               WHEN 2 MOVE "ANBOMOD 40MODANL" TO WT-TX
               WHEN 3 MOVE "CADRUCK 40DRUCK"  TO WT-TX
               WHEN 4 PERFORM TEILSTAMM
                         IF WH-PG = 9 GO A
               WHEN 5 MOVE "ANBODRU 80ANBOLI" TO WT-TX
               WHEN 6 MOVE 0 TO WZ-DATUM WH-PG
                      CALL "CAUP" USING "03DATUM" WH-CREG GO Q
               WHEN OTHER GO Q.
       Z.  EXIT.
      ************************************************* Teilstammdaten *
       TEILSTAMM SECTION.
       A.  MOVE "     Teilstammdaten" TO WK-GEB.
           CALL "CAUP" USING "06OKOPF" WH-CREG.
           DISPLAY WD-DPAR(1:9) AT 0172.
       C.  DISPLAY "<ret-leer>/<esc>= Abbruch" AT 2301.
           DISPLAY "Auswahl" AT 0524
                   "ƒƒƒƒƒƒƒ" AT 0024
                   "1 -       "          AT 0824
                   "2 -                " AT 0024
                   "3 - Rechnungs-Nr."   AT 0024
                   "4 - Kopf / Code"     AT 0024
                   "leer = Programmende" AT 0024
                   "bitte wÑhlen Sie:   " with highlight AT 1339.
           CALL "CAUP" USING "0013571001" WH-CREG.
           IF ESC MOVE 0 TO WH-NUM WH-PG
               SET RET TO TRUE.
           IF NOT RET GO C.
           MOVE WH-NUM TO WH-PG.
           EVALUATE WH-PG
               WHEN 0 MOVE 9 TO WH-PG GO Z
               WHEN 3 PERFORM NUMMERN
               WHEN 4 PERFORM STAM
               WHEN OTHER GO C.
           GO A.
       Z.  EXIT.
      ******************************************* Konstantenverwaltung *
       NUMMERN SECTION.
       A.  MOVE 1 TO WH-KEY.
           MOVE 9 TO WH-PG.
       C.  READ KONSTANT INVALID STOP RUN.
           IF ZUGRIF PERFORM BESETZT GO C.
           CALL "CAUP" USING "0708100751000" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " sonst. Konstanten " with highlight AT VDU-LP.
           ADD 303 VDU-ECK GIVING VDU-LP.
           DISPLAY "nÑchste Rechnung.(1)..:" AT VDU-LP.
           ADD 100 TO VDU-LP.
           DISPLAY "Rechnungsort..........:" AT VDU-LP
           ADD 100 TO VDU-LP.
           DISPLAY "Druckervariante.......:" AT VDU-LP.
      *----------------------------------------------> Werte anzeigen <-
           ADD 327 VDU-ECK GIVING VDU-LP.
           MOVE KO-RENUM TO WD-NUM.
           DISPLAY WD-NUM with highlight AT VDU-LP.
           ADD 100 TO VDU-LP.
           DISPLAY KO-ORT with highlight AT VDU-LP.
           ADD 100 TO VDU-LP.
           MOVE KO-DRU TO WD-KZ.
           DISPLAY WD-KZ with highlight AT VDU-LP " / "
           EVALUATE WD-KZ
               WHEN 0 DISPLAY "Nadeldrucker" AT 0000
               WHEN 1 DISPLAY "Laserdrucker" AT 0000.
      *--------------------------------------------> anlegen / Ñndern <-
       G.  DISPLAY "<esc>= Abbruch, <ret>= Rechnungsnummer" AT 2301.
           ADD 327 VDU-ECK GIVING VDU-LP.
           MOVE KO-RENUM TO WD-NUM WH-WERT.
           DISPLAY WD-NUM with highlight AT VDU-LP.
           CALL "CAUP" USING "1003275005" WH-CREG.
           IF ESC GO X.
           IF not RET GO G.
           MOVE WH-WERT TO KO-RENUM WD-NUM.
           DISPLAY WD-NUM with highlight AT VDU-LP.
       I.  DISPLAY "<esc>= Abbruch, < />= zurÅck, <ret>= Ort" AT 2301.
           MOVE KO-ORT TO WT-TX.
           ADD 427 VDU-ECK GIVING VDU-LP.
           DISPLAY KO-ORT with highlight AT VDU-LP.
           CALL "CAUP" USING "1204270115" WH-CREG.
           IF ESC GO X.
           IF WOLI or AUF GO G.
           IF not RET GO I.
           MOVE WT-TX TO KO-ORT.
           DISPLAY KO-ORT with highlight AT VDU-LP.
       K.  DISPLAY "<esc>= Abbruch, < />= zurÅck, <ret>= Drucker"
               AT 2301.
           DISPLAY "0= Nadeldruckerm, 1= Laserdrucker" AT 2401.
           ADD 427 VDU-ECK GIVING VDU-LP.
           MOVE KO-DRU TO WH-WERT.
           CALL "CAUP" USING "1005271001" WH-CREG.
           IF ESC GO X.
           IF WOLI or AUF GO I.
           IF not RET GO E.
           IF WH-WERT > 1 GO K.
           MOVE WH-WERT TO KO-DRU WD-KZ.
           DISPLAY WD-KZ with highlight AT VDU-LP.
           DISPLAY WD-KZ with highlight AT VDU-LP " / "
           EVALUATE WD-KZ
               WHEN 0 DISPLAY "Nadeldrucker" AT 0000
               WHEN 1 DISPLAY "Laserdrucker" AT 0000.
       M.  DISPLAY "<esc>= Abbruch, <ret>= speichern, < />= zurÅck < >"
              AT 2301.
           CALL "CAUP" USING "0023500000" WH-CREG.
           IF WOLI or AUF GO K.
           IF ESC GO X.
           IF not RET GO M.
           REWRITE KO-SATZ.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
           UNLOCK KONSTANT.
       Z.  EXIT.
      ******************************************************************
       sec section.
       a.  open i-o secdat.
           read secdat at end go x.
           compute wh-sec = 92837465 * 970328 + 28473703.
           if sec-satz(23:14) = wh-sec go x.
           move sec-satz(23:6) to wz-datum.
           add 4 to wz-monat.
           if wz-monat > 12 add 1 to wz-jahr
               subtract 12 from wz-monat.
           if wz-datum > wh-datum go x.
           display "Security-Error (007) - " at 2401.
           display "weiter mit <ret>" at 2424.
           call "CAUP" using "0124416006" wh-creg.
           compute wv-datum = wh-datum + 4711 - 470000.
           if wv-datum not = wh-num stop run.
           move wh-sec to sec-satz(23:14).
           rewrite sec-satz.
       x.  close secdat.
       z.  exit.
      ******************************************************************
       BESETZT SECTION.
       A.  DISPLAY "Record - besetzt" AT 2401.
           PERFORM WEITER.
       Z.  EXIT.
      ******************************************************************
       WEITER SECTION.
       A.  DISPLAY " weiter mit <ret>: " AT 0000.
           MOVE SPACE TO WH-X.
           ACCEPT WH-X AT 0000.
           CALL "CAUP" using "1324012480000" WH-CREG.
       Z.  EXIT.
      *****************************************************************
       NO-REC SECTION.
       A.  DISPLAY "keine Daten vorhanden" AT 2401.
           PERFORM WEITER.
       Z.  EXIT.
      ******************************************************************
       STAM SECTION.
       A.  MOVE 1 TO WH-KEY.
       B.  READ KONSTANT INVALID KEY GO Z.
           IF ZUGRIF PERFORM BESETZT GO B.
      *------------------------------> éndern Firmenkopf u. Codewort <-
       D.  MOVE KO-FIRMA TO WT-TX.
           DISPLAY KO-FIRMA AT 0127.
           CALL "CAUP" USING "0201270122" WH-CREG.
           MOVE WT-TX TO KO-FIRMA.
       F.  CALL "CAUP" USING "0705430528000" WH-CREG.
           DISPLAY " Codewort " with highlight AT 0545.
       G.  DISPLAY "Altes Codewort:" AT 0645.
           DISPLAY KO-SPERRE AT 0663.
           MOVE SPACE TO WX-CODE(1) WX-CODE(2).
           DISPLAY "Neues Codewort:" AT 0745.
           ACCEPT WX-CODE(1) AT 0763.
           IF NOT RET GO G.
           MOVE WX-CODE(1) TO KO-SPERRE.
       H.  DISPLAY "Kontrolleingabe:" AT 0845.
           ACCEPT WX-CODE(2) AT 0863.
           IF NOT RET GO H.
           IF WX-CODE(2) NOT = WX-CODE(1)
               DISPLAY "Eingaben nicht gleich" AT 0445 GO G.
           CALL "CAUP" USING "08CLOFEN" WH-CREG.
       I.  REWRITE KO-SATZ.
       Z.  EXIT.
      ****************************** Buchung Ñlter als 7 Jahre lîschen *
       AUTOLOESCH SECTION.
       A.  MOVE 0 TO WY.
           MOVE WH-DATUM TO WV-HIDAT.
           IF WH-DATUM < 500000 ADD 20000000 TO WV-HIDAT
                           else ADD 19000000 TO WV-HIDAT.
           ADD -10000 TO WV-HIDAT.
           MOVE WV-HIDAT TO WV-DATUM WC-DATUM.
           CALL "CAUP" USING "04DATDREH" WH-CREG.
           MOVE LOW-VALUE TO AK-SATZ LK-SATZ.
           MOVE 900101 TO AK-DATUM LK-DATUM.
           MOVE "    automat. Lîschung " TO WK-GEB.
       Z.  EXIT.
      ***************************************** Druckerparameter laden *
       LAD-DRUCK SECTION.
      *                                  11  *> quer A4 /10" 6 Zeilen <-
       A.  MOVE "E(10U&l26a6d1O(s0p10h0b0s6T" TO WE-STG(1).
      *                                  12  *> quer A4 /12" 6 Zeilen <-
           MOVE "E(10U&l26a6d1O(s0p12h0b0s6T" TO WE-STG(2).
      *                                  13  *> quer A4 /16" 6 Zeilen <-
           MOVE "E(10U&l26a6d1O(s0p14.6h0b0s6T" TO WE-STG(3).
      *                                  14  *> hoch A4 /10" 6 Zeilen <-
       A.  MOVE "E(10U&l26a6d0O(s0p10h0b0s3T" TO WE-STG(4).
      *                                  15  *> hoch A4 /12" 6 Zeilen <-
           MOVE "E(10U&l26a6d67f0O(s0p12h0b0s3T" TO WE-STG(5).
      *                                  16  *> hoch A4 /16" 6 Zeilen <-
           MOVE "E(10U&l26a6d0O(s0p14.6h0b0s6T" TO WE-STG(6).
      *                                  17  *> hoch A4 /16" 6 Zeilen <-
           MOVE "E(10U&l26a6d0O(s0p15.6h0b0s6T" TO WE-STG(7).
      *                                  18  *> hoch A4 /12" 6 Zeilen <-
           MOVE "E(10U&l3a6d0O(s0p12h0b0s3T" TO WE-STG(8).
      *    &la: Format, c: 6 Zeilen, 1/0O: quer/hoch
      *    (sp: Abstand, h: Zeich./Zoll b: StÑrke s: Schrift T: Schrift
181100*----------> (10U wegen Drucker bei Caska <----------------------
       Z.  EXIT.
