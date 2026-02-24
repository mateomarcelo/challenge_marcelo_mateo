       IDENTIFICATION DIVISION.
       PROGRAM-ID. CICBP02.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  ACC-RECORD.
           05  ACC-NUMBER              PIC X(12).
           05  ACC-TYPE                PIC X.
               88  ACC-TYPE-CHECKING   VALUE 'C'.
               88  ACC-TYPE-SAVINGS    VALUE 'S'.
           05  ACC-STATUS              PIC X.
               88  ACC-OPEN            VALUE 'O'.
               88  ACC-CLOSED          VALUE 'X'.
               88  ACC-BLOCKED         VALUE 'B'.
           05  ACC-BALANCE             PIC S9(11)V99 COMP-3.
           05  ACC-OD-LIMIT            PIC S9(7)V99  COMP-3.
           05  ACC-RISK-RATING         PIC 9.
           05  ACC-FEES.
               10  ACC-FEE OCCURS 0 TO 5 TIMES
                   DEPENDING ON ACC-FEE-COUNT.
                   15  ACC-FEE-CODE    PIC X(3).
                   15  ACC-FEE-AMOUNT  PIC S9(5)V99 COMP-3.
           05  ACC-FEE-COUNT           PIC 9.

       01  PRM-FLAGS.
           05  PRM-TEST-MODE        PIC X VALUE 'N'.
               88  PRM-TEST-YES     VALUE 'Y'.
           05  PRM-DATE-CUTOFF      PIC 9(8).
           05  PRM-ALLOW-NEGATIVE   PIC X VALUE 'N'.
               88  PRM-ALLOW-NEG    VALUE 'Y'.

       01  WS-AUTH-DECISION     PIC X VALUE 'N'.
           88 WS-AUTH-YES       VALUE 'Y'.
           88 WS-AUTH-NO        VALUE 'N'.
       01  WS-MOD-NAME          PIC X(8) VALUE 'UTLVAL01'.
       LINKAGE SECTION.
       01  LK-ACC               PIC X(12).
       PROCEDURE DIVISION USING LK-ACC.
       0000-MAIN.
           MOVE LK-ACC TO ACC-NUMBER
           EVALUATE TRUE
              WHEN ACC-BLOCKED
                   SET WS-AUTH-NO TO TRUE
              WHEN ACC-RISK-RATING > 5
                   SET WS-AUTH-NO TO TRUE
              WHEN OTHER
                   IF ACC-BALANCE + ACC-OD-LIMIT > 0
                      SET WS-AUTH-YES TO TRUE
                   ELSE
                      SET WS-AUTH-NO TO TRUE
                   END-IF
           END-EVALUATE

           *> Llamada dinámica a validador técnico (capcioso)
           CALL WS-MOD-NAME USING BY REFERENCE ACC-RECORD, PRM-FLAGS
              ON EXCEPTION CONTINUE
           END-CALL

           GOBACK.
