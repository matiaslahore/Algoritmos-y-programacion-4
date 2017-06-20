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

           SELECT ARCH-ORD ASSIGN TO DISK "orden.txt"
                           SORT STATUS IS FS-ARCH-ORD.

           SELECT PRO      ASSIGN TO DISK "pro.txt"
                           ORGANIZATION IS INDEXED
                           RECORD KEY IS PRO-COD-PRO
                           FILE STATUS IS FS-PRO.

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

       SD  ARCH-ORD.
       01  REG-ORD.
           03 ORD-RUBRO PIC 9(4).
           03 ORD-DESCR-RUBRO PIC X(15).
           03 ORD-COD-PROV PIC 9(8).
           03 ORD-CUIT-CONS PIC 9(15).
           03 ORD-NOMB-CONS PIC X(30).
           03 ORD-TELEFONO PIC X(15).
           03 ORD-DIR PIC X(30).

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

       01  RUBRO-AUX PIC 9(4) VALUE 0.
       01  TOTXRUBRO PIC 9(3) VALUE 0.
       01  RUBROS-TOTALES PIC 9(3) VALUE 0.
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
       77  FS-ARCH-ORD PIC XX.
           88 OK-ORD VALUE "00".
           88 NO-ORD VALUE "23".
           88 EOF-ORD VALUE "10".

       01  CUIT PIC 9(15) VALUE 0.

       PROCEDURE DIVISION.
       COMIENZO.
           PERFORM INICIO.
      *    OP = 1
           PERFORM CALLSUBPRG.
           PERFORM ACTUALIZO-CONS-AND-SORT.
      *    OP = 3
           PERFORM CALLSUBPRG.
           PERFORM FIN.
           STOP RUN.
      *>-----------------------------------------------------------*
      *>-----------------------------------------------------------*
       INICIO.
           OPEN INPUT MAE.
           IF FS-MAE NOT = "00"
               DISPLAY "Error en open maestro FS: " FS-MAE.
           OPEN INPUT CPR.
           IF FS-CPR NOT = "00"
               DISPLAY "Error en open cuit-proveedores FS: " FS-CPR.
      *>-----------------------------------------------------------*
      *>-----------------------------------------------------------*
       CALLSUBPRG.
           OPEN INPUT PRO.
           IF FS-PRO NOT = "00"
               DISPLAY "Error en open proveedores FS: " FS-PRO.
      *>-----------------------------------------------------------*
      *>-----------------------------------------------------------*
       ACTUALIZO-CONS-AND-SORT.
           SORT ARCH-ORD
                ON ASCENDING KEY ORD-RUBRO
                ON ASCENDING KEY ORD-COD-PROV
                ON ASCENDING KEY ORD-CUIT-CONS
                INPUT PROCEDURE IS ENTRADA
                OUTPUT PROCEDURE IS SALIDA.
           STOP RUN.
      *>-----------------------------------------------------------*
      *>-----------------------------------------------------------*
       ENTRADA SECTION.
           PERFORM LEER-MAE.
           PERFORM PROCESAR-MAE UNTIL EOF-MAE.
      *>-----------------------------------------------------------*
      *>-----------------------------------------------------------*
       PROCESAR-MAE.
           PERFORM BUSCAR-PROV.
           PERFORM LEER-MAE.
      *>-----------------------------------------------------------*
      *>-----------------------------------------------------------*
       BUSCAR-PROV.
           MOVE MAE-CUIT-CONS TO CPR-CUIT-CONS
           START CPR KEY IS EQUAL CPR-CUIT-CONS.
           IF NOT OK-CPR
               IF NO-CPR
                   DISPLAY "Cuit no encontrado"
               ELSE
                   DISPLAY "Error, FS: " FS-CPR
           ELSE
                  READ CPR NEXT RECORD.
                  IF NOT OK-CPR AND NOT EOF-CPR
                      DISPLAY "Error, FS: " FS-CPR.
                  PERFORM PROCESAR-CPR
                      UNTIL EOF-CPR OR CPR-CUIT-CONS <> MAE-CUIT-CONS.
      *>-----------------------------------------------------------*
      *>-----------------------------------------------------------*
       PROCESAR-CPR.
      * OP = 2
           PERFORM CALLSUBPRG.
           PERFORM GRABO-SORT.
           READ CPR NEXT RECORD.
      *>-----------------------------------------------------------*
      *>-----------------------------------------------------------*
       GRABO-SORT.
           MOVE PRO-RUBRO TO ORD-RUBRO.
           MOVE PRO-DESCRIP-RUBRO TO ORD-DESCR-RUBRO.
           MOVE CPR-COD-PROV TO ORD-COD-PROV.
           MOVE MAE-CUIT-CONS TO ORD-CUIT-CONS.
           MOVE MAE-NOMBRE-CONSORCIO TO ORD-NOMB-CONS.
           MOVE MAE-TEL TO ORD-TELEFONO.
           MOVE MAE-DIR TO ORD-DIR.
           RELEASE REG-ORD.
      *>-----------------------------------------------------------*
      *>-----------------------------------------------------------*
       SALIDA SECTION.
           PERFORM IMPRIMIR-CABECERA.
           RETURN ARCH-ORD AT END MOVE "10" TO FS-ARCH-ORD.
           MOVE ORD-RUBRO TO RUBRO-AUX.
           PERFORM PROCESO-ORD UNTIL EOF-ORD.

      *>-----------------------------------------------------------*
      *>-----------------------------------------------------------*
       PROCESO-ORD.
           PERFORM IMPRIMIR-RUBRO-DESCR.
           PERFORM IMPRIMIR-FEAUTURE.
           PERFORM PROCESO-RUBRO UNTIL RUBRO-AUX <> ORD-RUBRO.
           PERFORM IMPRIMIR-TOTXRUBRO.
           ADD 1 TO RUBROS-TOTALES.
           MOVE 0 TO TOTXRUBRO.
           MOVE ORD-RUBRO TO RUBRO-AUX.

      *>-----------------------------------------------------------*
      *>-----------------------------------------------------------*
       PROCESO-RUBRO.
           PERFORM IMPRIMIR-REG-ORD.
           ADD 1 TO TOTXRUBRO.
           RETURN ARCH-ORD AT END MOVE "10" TO FS-ARCH-ORD.
      *>-----------------------------------------------------------*
      *>-----------------------------------------------------------*
       IMPRIMIR-CABECERA.
      *>-----------------------------------------------------------*
      *>-----------------------------------------------------------*
       IMPRIMIR-RUBRO-DESCR.
      *>-----------------------------------------------------------*
      *>-----------------------------------------------------------*
       IMPRIMIR-FEAUTURE.
      *>-----------------------------------------------------------*
      *>-----------------------------------------------------------*
       IMPRIMIR-TOTXRUBRO.
      *>-----------------------------------------------------------*
      *>-----------------------------------------------------------*
       IMPRIMIR-REG-ORD.
      *>-----------------------------------------------------------*
      *>-----------------------------------------------------------*
       FIN.
           CLOSE CPR.
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
