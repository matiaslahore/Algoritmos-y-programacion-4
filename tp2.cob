      ******************************************************************
      * Authors: Santiago Sosa Montiel, Matias Lahore, Matias Tebele,
      *          Nicolas Outeda.
      * Date: 14/6/2017 DD/MM/AAAA
      * Purpose: TP Materia: Algoritmos y Programación IV
      * Tectonics: cobc
      ******************************************************************

       IDENTIFICATION DIVISION.
       PROGRAM-ID. TP2.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MAE   ASSIGN TO "maestro.txt"
                        ORGANIZATION IS LINE SEQUENTIAL
                        FILE STATUS IS FS-MAE.

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
           88 EOF-CPR VALUE "10".d 
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
       77 FS-MAE PIC XX.
           88 OK-MAE VALUE "00".
           88 NO-MAE VALUE "23".
           88 EOF-MAE VALUE "10".

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
           PERFORM ACTUALIZO-PROV-ASIGN UNTIL EOF-OUT-PRO.
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
           OPEN INPUT WRK-PRO.
           IF FS-WRK-PRO NOT = "00"
               DISPLAY "Error en open wrk-prov FS: " FS-WRK-PRO.
           OPEN INPUT OUT-PRO.
           IF FS-OUT-PRO NOT = "00"
               DISPLAY "Error en open wrk-prov FS: " FS-OUT-PRO.
           OPEN INPUT WRK-CPR.
           IF FS-WRK-CPR NOT = "00"
               DISPLAY "Error en open wrk-prov FS: " FS-WRK-CPR.
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
               DISPLAY 'FIN MAE'
      *>-----------------------------------------------------------*
      *>-----------------------------------------------------------*
       LEER-CPR.
           READ CPR
               AT END MOVE "10" TO FS-CPR.
           IF FS-CPR NOT EQUAL ZERO AND "10"
               DISPLAY 'ERROR AL LEER CPR FS: ' FS-CPR.
           IF FS-CPR EQUAL "10"
               DISPLAY 'FIN CPR'
      *>-----------------------------------------------------------*
      *>-----------------------------------------------------------*
       LEER-PRO.
           READ PRO
               AT END MOVE "10" TO FS-PRO.
           IF FS-PRO NOT EQUAL ZERO AND "10"
               DISPLAY 'ERROR AL LEER PRO ' FS-PRO.
           IF FS-PRO EQUAL "10"
               MOVE 999999999999999 TO PRO-CUIT-CONS.
      *>-----------------------------------------------------------*
      *>-----------------------------------------------------------*
       