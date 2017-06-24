       PROGRAM-ID. INDEXER.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT CPR          ASSIGN TO DISK "cpr.txt"
                               ORGANIZATION IS LINE SEQUENTIAL
                               FILE STATUS IS FS-CPR.

           SELECT CPR-INDEX    ASSIGN TO "cpr_indexed.txt"
                               ACCESS MODE IS SEQUENTIAL
                               ORGANIZATION IS INDEXED
                               RECORD KEY IS CPR-INDEX-CUIT-CONS
                               ALTERNATE RECORD KEY IS
                                   CPR-INDEX-COD-PROV WITH DUPLICATES.

           SELECT PROV         ASSIGN TO DISK "prov.txt"
                               ORGANIZATION IS LINE SEQUENTIAL
                               FILE STATUS IS FS-PROV.

           SELECT PROV-INDEX   ASSIGN TO "prov_indexed.txt"
                               ACCESS MODE IS RANDOM
                               ORGANIZATION IS INDEXED
                               RECORD KEY IS PRO-INDEX-COD-PROV
                               FILE STATUS IS FS-PROV-INDEX.

       DATA DIVISION.
       FILE SECTION.

       FD  PROV.
       01  REG-PROV.
           03 PRO-COD-PROV PIC 9(8).
           03 PRO-DIR PIC X(30).
           03 PRO-TEL PIC X(15).
           03 PRO-RUBRO PIC 9(4).
           03 PRO-DESCR-RUBRO PIC X(15).
           03 PRO-FECHA-ALTA.
               05 PRO-ANIO PIC 9(4).
               05 PRO-MES PIC 9(2).
               05 PRO-DIA PIC 9(2).
           03 PRO-CANT-CONS-ASIG PIC 9(3).

       FD  PROV-INDEX.
       01  REG-PROV-INDEX.
           03 PRO-INDEX-COD-PROV PIC 9(8).
           03 PRO-INDEX-DIR PIC X(30).
           03 PRO-INDEX-TEL PIC X(15).
           03 PRO-INDEX-RUBRO PIC 9(4).
           03 PRO-INDEX-DESCR-RUBRO PIC X(15).
           03 PRO-INDEX-FECHA-ALTA.
               05 PRO-INDEX-ANIO PIC 9(4).
               05 PRO-INDEX-MES PIC 9(2).
               05 PRO-INDEX-DIA PIC 9(2).
           03 PRO-INDEX-CANT-CONS-ASIG PIC 9(3).

       FD  CPR.
       01  REG-CPR.
           03 CPR-CUIT-CONS PIC 9(15).
           03 CPR-COD-PROV PIC 9(8).
           03 CPR-FECHA-ALTA.
               05 CPR-ANIO PIC 9(4).
               05 CPR-MES PIC 9(2).
               05 CPR-DIA PIC 9(2).

       FD  CPR-INDEX.
       01  REG-CPR-INDEX.
           03 CPR-INDEX-CUIT-CONS PIC 9(15).
           03 CPR-INDEX-COD-PROV PIC 9(8).
           03 CPR-INDEX-FECHA-ALTA.
               05 CPR-INDEX-ANIO PIC 9(4).
               05 CPR-INDEX-MES PIC 9(2).
               05 CPR-INDEX-DIA PIC 9(2).

       WORKING-STORAGE SECTION.

       77  FS-CPR PIC XX.
           88 OK-CPR VALUE "00".
           88 NO-CPR VALUE "23".
           88 EOF-CPR VALUE "10".

       77  FS-CPR-INDEX PIC XX.
           88 OK-CPR-INDEX VALUE "00".
           88 NO-CPR-INDEX VALUE "23".
           88 EOF-CPR-INDEX VALUE "10".

       77  FS-PROV PIC XX.
           88 OK-PROV VALUE "00".
           88 NO-PROV VALUE "23".
           88 EOF-PROV VALUE "10".

       77  FS-PROV-INDEX PIC XX.
           88 OK-PROV-INDEX VALUE "00".
           88 NO-PROV-INDEX VALUE "23".
           88 EOF-PROV-INDEX VALUE "10".

       PROCEDURE DIVISION.
       MAIN.
           OPEN INPUT CPR.
           OPEN INPUT PROV.
           OPEN OUTPUT CPR-INDEX.
           OPEN OUTPUT PROV-INDEX.
           PERFORM CONVERTIR-CPR.
           PERFORM CONVERTIR-PROV.
           CLOSE CPR.
           CLOSE PROV.
           CLOSE CPR-INDEX.
           CLOSE PROV-INDEX.
           PERFORM MOSTRAR-RESULTADO.
           STOP RUN.

       CONVERTIR-CPR.
           PERFORM LEER-CPR.
           PERFORM PROCESAR-CPR UNTIL EOF-CPR.

       LEER-CPR.
           READ CPR AT END MOVE "10" TO FS-CPR.

       PROCESAR-CPR.
           MOVE REG-CPR TO REG-CPR-INDEX.
           WRITE REG-CPR-INDEX.
           PERFORM LEER-CPR.

       CONVERTIR-PROV.
           PERFORM LEER-PROV.
           PERFORM PROCESAR-PROV UNTIL EOF-PROV.

       LEER-PROV.
           READ PROV AT END MOVE "10" TO FS-PROV.

       PROCESAR-PROV.
           MOVE REG-PROV TO REG-PROV-INDEX.
           WRITE REG-PROV-INDEX.
           PERFORM LEER-PROV.

       MOSTRAR-RESULTADO.
           OPEN INPUT CPR-INDEX.
           OPEN INPUT PROV-INDEX.
           READ CPR-INDEX RECORD.
           PERFORM LISTAR-CPR UNTIL EOF-CPR-INDEX.
           PERFORM PROBAR-PROV.
           CLOSE CPR-INDEX.
           CLOSE PROV-INDEX.

       LISTAR-CPR.
           DISPLAY REG-CPR-INDEX.
           READ CPR-INDEX RECORD AT END MOVE "10" TO FS-CPR-INDEX.

       PROBAR-PROV.
           MOVE 33333333 TO PRO-INDEX-COD-PROV.
           READ PROV-INDEX RECORD.
           DISPLAY REG-PROV-INDEX.
           MOVE 22222222 TO PRO-INDEX-COD-PROV.
           READ PROV-INDEX RECORD.
           DISPLAY REG-PROV-INDEX.

       END PROGRAM INDEXER.
