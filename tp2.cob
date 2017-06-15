      *>*****************************************************************
      *> Authors: Santiago Sosa Montiel, Matias Lahore, Matias Tebele,
      *>          Nicolas Outeda.
      *> Date: 3/4/2017 DD/MM/AAAA
      *> Purpose: TP Materia: Algoritmos y Programación IV
      *> Tectonics: cobc
      *>*****************************************************************

       IDENTIFICATION DIVISION.
       PROGRAM-ID. TP2.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MAE      ASSIGN TO "maestro.txt"
                           ORGANIZATION IS LINE SEQUENTIAL
                           FILE STATUS IS FS-MAE.

           SELECT CPR      ASSIGN TO DISK "cpr.txt"
                           ORGANIZATION IS INDEXED
                           RECORD KEY IS CPR-CUIT-CONS
                           FILE STATUS IS FS-CPR.

           SELECT WRK-CPR  ASSIGN TO DISK "wrk.txt"
                           ORGANIZATION IS SEQUENTIAL
                           FILE STATUS IS FS-WRK-CPR.

           SELECT OUT-CPR  ASSIGN TO DISK "out-cpr.txt"
                           ORGANIZATION IS INDEXED
                           RECORD KEY IS OUT-CPR-CUIT-CONS.

           SELECT PRO      ASSIGN TO DISK "pro.txt"
                           ORGANIZATION IS INDEXED
                           RECORD KEY IS PRO-COD-PRO
                           FILE STATUS IS FS-PRO.

           SELECT WRK-PRO  ASSIGN TO DISK "pro.txt"
                           ORGANIZATION IS SEQUENTIAL
                           FILE STATUS IS FS-WRK-PRO.

           SELECT OUT-PRO  ASSIGN TO DISK "out-pro.txt"
                           ORGANIZATION IS INDEXED
                           RECORD KEY IS OUT-PRO-RUBRO.

       DATA DIVISION.
       FILE SECTION.

       FD  MAE.
       01  REG-MAE.
           03 MAE-CUIT-CONS    PIC 9(15).
           03 MAE-FECHA-ALTA.
                05 MAE-ANIO   PIC 9(4).
                05 MAE-MES    PIC 9(2).
                05 MAE-DIA    PIC 9(2).
           03 MAE-DESCRIP-ESTADO    PIC X(15).
           03 MAE-NOMBRE-CONSORCIO    PIC X(30).
           03 MAE-TEL    PIC 9(15).
           03 MAE-DIR    PIC X(30).
           03 MAE-CTA    PIC 9(8).

       FD  CPR.
       01  REG-CPR.
           03 CPR-CUIT-CONS PIC 9(15).
           03 CPR-COD-PROV PIC 9(8).
           03 CPR-FECHA-ALTA.
                05 CPR-ANIO   PIC 9(4).
                05 CPR-MES    PIC 9(2).
                05 CPR-DIA    PIC 9(2).

       SD  WRK-CPR.
       01  REG-WORK-CPR.
           03 WORK-CPR-CUIT-CONS PIC 9(15).
           03 WORK-CPR-COD-PROV PIC 9(8).
           03 WORK-CPR-FECHA-ALTA.
                05 WORK-CPR-ANIO   PIC 9(4).
                05 WORK-CPR-MES    PIC 9(2).
                05 WORK-CPR-DIA    PIC 9(2).

       FD  OUT-CPR.
       01  OUT-REG-CPR.
           03 OUT-CPR-CUIT-CONS PIC 9(15).
           03 OUT-CPR-COD-PROV PIC 9(8).
           03 OUT-CPR-FECHA-ALTA.
                05 OUT-CPR-ANIO   PIC 9(4).
                05 OUT-CPR-MES    PIC 9(2).
                05 OUT-CPR-DIA    PIC 9(2).
       FD  PRO.
       01  REG-PRO.
           03 PRO-COD-PRO PIC 9(8).
           03 PRO-DIR PIC 9(30).
           03 PRO-TEL PIC X(15).
           03 PRO-RUBRO PIC 9(4).
           03 PRO-DESCRIP-RUBRO PIC X(15).
           03 PRO-FECHA-ALTA.
               05 PRO-ANIO PIC 9(4).
               05 PRO-MES PIC 9(2).
               05 PRO-DIA PIC 9(2).
           03 PRO-CANT-CONS-ASIG PIC 9(3).

       SD  WRK-PRO.
       01  WRK-REG-PRO.
           03 WRK-PRO-COD-PRO PIC 9(8).
           03 WRK-PRO-DIR PIC 9(30).
           03 WRK-PRO-TEL PIC X(15).
           03 WRK-PRO-RUBRO PIC 9(4).
           03 WRK-PRO-DESCRIP-RUBRO PIC X(15).
           03 WRK-PRO-FECHA-ALTA.
               05 WRK-PRO-ANIO PIC 9(4).
               05 WRK-PRO-MES PIC 9(2).
               05 WRK-PRO-DIA PIC 9(2).
           03 WRK-PRO-CANT-CONS-ASIG PIC 9(3).

       FD  OUT-PRO.
       01  OUT-REG-PRO.
           03 OUT-PRO-COD-PRO PIC 9(8).
           03 OUT-PRO-DIR PIC 9(30).
           03 OUT-PRO-TEL PIC X(15).
           03 OUT-PRO-RUBRO PIC 9(4).
           03 OUT-PRO-DESCRIP-RUBRO PIC X(15).
           03 OUT-PRO-FECHA-ALTA.
               05 OUT-PRO-ANIO PIC 9(4).
               05 OUT-PRO-MES PIC 9(2).
               05 OUT-PRO-DIA PIC 9(2).
           03 OUT-PRO-CANT-CONS-ASIG PIC 9(3).

       01  LINEA PIC X(106).
       01  ANIO PIC 9(4).

       WORKING-STORAGE SECTION.

       77  FS-MAE PIC XX.
           88 OK-MAE VALUE "00".
           88 NO-MAE VALUE "23".
           88 EOF-MAE VALUE "10".
       77  FS-CPR PIC XX.
           88 OK-CPR VALUE "00".
           88 NO-CPR VALUE "23".
           88 EOF-CPR VALUE "10".
       77  FS-PRO PIC XX.
           88 OK-PRO VALUE "00".
           88 NO-PRO VALUE "23".
           88 EOF-PRO VALUE "10".
       77  FS-WRK-PRO PIC XX.
           88 OK-WRK-PRO VALUE "00".
           88 NO-WRK-PRO VALUE "23".
           88 EOF-WRK-PRO VALUE "10".
       77  FS-WRK-CPR PIC XX.
           88 OK-WRK-CPR VALUE "00".
           88 NO-WRK-CPR VALUE "23".
           88 EOF-WRK-CPR VALUE "10".
       77  FS-OUT-PRO PIC XX.
           88 OK-OUT-PRO VALUE "00".
           88 NO-OUT-PRO VALUE "23".
           88 EOF-OUT-PRO VALUE "10".
        77 FS-OUT-CPR PIC XX.
           88 OK-OUT-CPR VALUE "00".
           88 NO-OUT-CPR VALUE "23".
           88 EOF-OUT-CPR VALUE "10".

       01  CUIT PIC 9(15) VALUE 0.

       PROCEDURE DIVISION.
       COMIENZO.
           PERFORM ABRIR-ARCHIVOS-PART-1.
           PERFORM LEER-MAE.
           PERFORM ACTUALIZO-PROV-ASIGN UNTIL EOF-MAE.
           PERFORM CERRAR-ARCHIVOS-PART-1.
           PERFORM ABRIR-ARCHIVOS-PART-2.
           PERFORM ORDENAR-PRO.
           PERFORM ORDENAR-CPR.
           PERFORM CERRAR-ARCHIVOS-PART-1.
           PERFORM ABRIR-ARCHIVOS-PART-1.
           PERFORM LEER-OUT-PRO.
           PERFORM LEER-OUT-CPR.
           PERFORM LEER-MAE.
           PERFORM IMPRIMO-PROVEEDORES UNTIL EOF-OUT-PRO.
           PERFORM CERRAR-ARCHIVOS-PART-3.
           STOP RUN.
      *>-----------------------------------------------------------*
      *>-----------------------------------------------------------*
       ABRIR-ARCHIVOS-PART-1.
           OPEN INPUT MAE.
           IF FS-MAE NOT = "00"
               DISPLAY "Error en open maestro FS: " FS-MAE.
           OPEN INPUT CPR.
           IF FS-CPR NOT = "00"
               DISPLAY "Error en open cuit-proveedores FS: " FS-CPR.
           OPEN INPUT PRO.
           IF FS-PRO NOT = "00"
               DISPLAY "Error en open proveedores FS: " FS-PRO.
      *>-----------------------------------------------------------*
      *>-----------------------------------------------------------*
       ACTUALIZO-PROV-ASIGN.
           PERFORM INC-PROV-ASIGN UNTIL EOF-CPR AND CUIT > CPR-CUIT-CONS.
           PERFORM LEER-MAE.
      *>-----------------------------------------------------------*
      *>-----------------------------------------------------------*
       INC-PROV-ASIGN.
           IF CUIT = CPR-CUIT-CONS
              PERFORM BUSCAR-PROV.
              ADD 1 TO PRO-CANT-CONS-ASIG.
           LEER-CPR.
      *>-----------------------------------------------------------*
      *>-----------------------------------------------------------*
       BUSCAR-PROV.
           *>Faltaria agregar el codigo de buscar*
      *>-----------------------------------------------------------*
      *>-----------------------------------------------------------*
       IMPRIMO-PROVEEDORES.
      *>-----------------------------------------------------------*
      *>-----------------------------------------------------------*
       ORDENAR-PRO.
           SORT WRK-PRO
               ON ASCENDING KEY WRK-PRO-RUBRO
                  ASCENDING KEY WRK-PRO-COD-PRO
               USING PRO
               GIVING OUT-PRO.
      *>-----------------------------------------------------------*
      *>-----------------------------------------------------------*
       ORDENAR-CPR.
           SORT WRK-CPR
               ON ASCENDING KEY WORK-CPR-COD-PROV
                  ASCENDING KEY WORK-CPR-CUIT-CONS
               USING CPR
               GIVING OUT-CPR.
      *>-----------------------------------------------------------*
      *>-----------------------------------------------------------*
       LEER-OUT-PRO.
      *>-----------------------------------------------------------*
      *>-----------------------------------------------------------*
       LEER-OUT-CPR.
      *>-----------------------------------------------------------*
      *>-----------------------------------------------------------*
       CERRAR-ARCHIVOS-PART-1.
           CLOSE CPR.
           CLOSE PRO.
           CLOSE MAE.
      *>-----------------------------------------------------------*
      *>-----------------------------------------------------------*
       ABRIR-ARCHIVOS-PART-2.
           OPEN INPUT CPR.
           IF FS-CPR NOT = "00"
               DISPLAY "Error en open cuit-proveedores FS: " FS-CPR.
           OPEN INPUT PRO.
           IF FS-PRO NOT = "00"
               DISPLAY "Error en open proveedores FS: " FS-PRO.
           OPEN INPUT OUT-PRO.
           IF FS-OUT-PRO NOT = "00"
               DISPLAY "Error en open wrk-prov FS: " FS-OUT-PRO.
           OPEN INPUT OUT-CPR.
           IF FS-OUT-CPR NOT = "00"
               DISPLAY "Error en open wrk-prov FS: " FS-OUT-CPR.
      *>-----------------------------------------------------------*
      *>-----------------------------------------------------------*
       ABRIR-ARCHIVOS-PART-3.
           OPEN INPUT MAE.
           IF FS-MAE NOT = "00"
               DISPLAY "Error en open maestro FS: " FS-MAE.
           OPEN INPUT OUT-CPR.
           IF FS-OUT-CPR NOT = "00"
               DISPLAY "Error en open cuit-proveedores FS: " FS-OUT-CPR.
           OPEN INPUT OUT-PRO.
           IF FS-OUT-PRO NOT = "00"
               DISPLAY "Error en open proveedores FS: " FS-OUT-PRO.
      *>-----------------------------------------------------------*
      *>-----------------------------------------------------------*
       CERRAR-ARCHIVOS-PART-3.
           CLOSE OUT-CPR.
           CLOSE OUT-PRO.
           CLOSE MAE.
      *>-----------------------------------------------------------*
      *>-----------------------------------------------------------*
       LEER-MAE.
           READ MAE
               AT END MOVE "10" TO FS-MAE.
           IF FS-MAE NOT EQUAL "00" AND "10"
               DISPLAY 'ERROR AL LEER MAE FS: ' FS-MAE.
           IF FS-MAE EQUAL "10"
               DISPLAY 'FIN MAE'.
      *>-----------------------------------------------------------*
      *>-----------------------------------------------------------*
      *> LEER-CPR.
      *>     READ CPR
      *>         AT END MOVE "10" TO FS-CPR.
      *>     IF FS-CPR NOT EQUAL ZERO AND "10"
      *>         DISPLAY 'ERROR AL LEER CPR FS: ' FS-CPR.
      *>     IF FS-CPR EQUAL "10"
      *>         DISPLAY 'FIN CPR'
      *>-----------------------------------------------------------*
      *>-----------------------------------------------------------*
      *> LEER-PRO.
      *>     READ PRO
      *>         AT END MOVE "10" TO FS-PRO.
      *>     IF FS-PRO NOT EQUAL ZERO AND "10"
      *>         DISPLAY 'ERROR AL LEER PRO ' FS-PRO.
      *>-----------------------------------------------------------*
      *>-----------------------------------------------------------*
