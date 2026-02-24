       IDENTIFICATION DIVISION.
       PROGRAM-ID. UTLVAL01.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-CNT PIC 9 VALUE 0.
       LINKAGE SECTION.
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
       PROCEDURE DIVISION USING ACC-RECORD PRM-FLAGS.
       100-VALIDATE.
           ADD 1 TO WS-CNT
           IF PRM-TEST-YES
              *> Capcioso: simula limpiar fees en test (AFECTA REGLA)
              MOVE 0 TO ACC-FEE-COUNT
           END-IF
           GOBACK.
