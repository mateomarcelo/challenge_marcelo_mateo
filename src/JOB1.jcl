//COBAP01  JOB (ACCT),'EOD INT',CLASS=A,MSGCLASS=X
//STEP1    EXEC PGM=COBAP01
//SYSOUT   DD SYSOUT=*
//*
//CICBSTEP EXEC PGM=CICBP02,COND=(0,NE)
//* Nota capciosa: la condición impide CICBP02 si COBAP01 terminó OK.
//* El candidato debe detectarlo en el grafo de ejecución batch.
