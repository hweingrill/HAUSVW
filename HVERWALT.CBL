      $SET LINKCOUNT"128" ANS85 BOUND AUTOLOCK
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    HVERWALT.
      ******************************************************************
      *        Hausverwaltungsprogramm fÅr Kollwitzgasse 30-41         *
      ******************************************************************
       ENVIRONMENT    DIVISION.
       CONFIGURATION   SECTION.
       SOURCE-COMPUTER.     PC.
       SPECIAL-NAMES.    DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       DATA DIVISION.
      ******************************************************************
       WORKING-STORAGE SECTION.
       COPY WHCREG.CPY.
       01  WT-CALL                     PIC X(8).
       PROCEDURE DIVISION.
      *****************************************************************
       STEUER SECTION.
       A.  CALL "CAUP" USING "9901012480000" WH-CREG.
           CALL "CADECL" USING "99AUFRUF" WH-CREG.
           CALL "CADRUCK" USING "99LOAD" WH-CREG.
       B.  CALL "ANBOVORL" USING "99VORLAUF" WH-CREG.
           CANCEL "HV-VORL".
       C.  IF WH-PG = 99 GO B.
           MOVE WT-TX TO WT-CALL.
           IF WT-TX NOT = SPACE CALL WT-CALL USING WT-TX(9:15) WH-CREG
                                CANCEL WT-CALL.
           IF WH-PG = 49 GO C.
       D.  MOVE 9 TO WH-PG.
           GO B.
       Z.  STOP RUN.
