       IDENTIFICATION DIVISION.
       PROGRAM-ID. COBAP01.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ACC-FILE ASSIGN TO "ACC.DAT"
               ORGANIZATION IS SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD  ACC-FILE.
       01  ACC-REC-FILE.
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
       WORKING-STORAGE SECTION.
       01  WS-INT-RATE           PIC S9(3)V9(5) COMP-3 VALUE +0.00150.
       01  WS-DATE               PIC 9(8).
       01  WS-ZERO               PIC S9(1) VALUE +0.
       01  WS-TMP REDEFINES WS-ZERO.
           05 WS-TMP-BYTE        PIC X.
       01  WS-UNUSED-FLAG        PIC X VALUE 'N'.  *> Dead flag (capcioso)
       01  PRM-FLAGS.
           05  PRM-TEST-MODE        PIC X VALUE 'N'.
               88  PRM-TEST-YES     VALUE 'Y'.
           05  PRM-DATE-CUTOFF      PIC 9(8).
           05  PRM-ALLOW-NEGATIVE   PIC X VALUE 'N'.
               88  PRM-ALLOW-NEG    VALUE 'Y'.
       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM 1000-INIT
           PERFORM 2000-PROCESS UNTIL 3000-END-OF-FILE
           PERFORM 9000-FINALIZE
           GOBACK.

       1000-INIT.
           MOVE FUNCTION CURRENT-DATE(1:8) TO WS-DATE.
           IF PRM-TEST-YES
              MOVE +0.00000 TO WS-INT-RATE
           END-IF.
           OPEN INPUT ACC-FILE.

       2000-PROCESS.
           READ ACC-FILE
               AT END MOVE 1 TO 3000-END-OF-FILE
           END-READ
           IF 3000-END-OF-FILE = 1
               GO TO 2999-EXIT
           END-IF

           *> Regla de negocio (intereses sólo para cuentas abiertas y ahorro)
           IF ACC-OPEN AND ACC-TYPE-SAVINGS
              COMPUTE ACC-BALANCE = ACC-BALANCE
                 + (ACC-BALANCE * WS-INT-RATE)
                 ON SIZE ERROR
                    CONTINUE
              END-COMPUTE
           END-IF

           *> Regla: bloqueo si balance < -OD-LIMIT y no se permiten negativos
           IF (NOT PRM-ALLOW-NEG) AND (ACC-BALANCE < -ACC-OD-LIMIT)
              SET ACC-BLOCKED TO TRUE
           END-IF

           *> Código técnico (logging simulado)
           IF WS-TMP-BYTE = X'00'
              CONTINUE
           END-IF.

       2999-EXIT.
           EXIT.

       3000-END-OF-FILE.
           EXIT.

       9000-FINALIZE.
           CLOSE ACC-FILE.
           EXIT.
