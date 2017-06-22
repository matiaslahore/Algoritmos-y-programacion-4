      *>*****************************************************************
      *> Authors: Santiago Sosa Montiel, Matias Lahore, Matias Tebele,
      *>          Nicolas Outeda.
      *> Date: 3/4/2017 DD/MM/AAAA
      *> Purpose: TP Materia: Algoritmos y Programacion IV
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

           SELECT CPR      ASSIGN TO RANDOM "cpr_indexed.txt"
                           ORGANIZATION IS INDEXED
                           ACCESS MODE IS DYNAMIC
                           RECORD KEY IS CPR-CUIT-CONS
                           FILE STATUS IS FS-CPR.

           SELECT ARCH-ORD ASSIGN TO DISK "orden.txt"
                           SORT STATUS IS FS-ARCH-ORD.

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

       01  RUBRO-AUX PIC 9(4) VALUE 0.
       01  TOTXRUBRO PIC 9(3) VALUE 0.
       01  RUBROS-TOTALES PIC 9(3) VALUE 0.

       01  LINEA1.
           03 FILLER PIC X(7) VALUE "Fecha: ".
           03 FECHA PIC X(10).
           03 FILLER PIC X(62) VALUE SPACES.
           03 FILLER PIC X(9) VALUE "Hoja nro ".
           03 HOJA PIC 9(2).

       01  TITULO.
           03 FILLER PIC X(25) VALUE SPACES.
           03 FILLER PIC X(30) VALUE "LISTA DE PROVEEDORES ASIGNADOS".
           03 FILLER PIC X(15) VALUE SPACES.

       01  RUBRO-DESCRIPCION-FEAUTURE.
           03 RUBRO-FEAUTURE PIC X(6) VALUE "RUBRO ".
           03 DESCRIPCION-FEAUTURE PIC X(18) VALUE "DESCRIPCION-RUBRO ".

       01  RUBRO-DESCRIPCION.
           03 IMP-RUBRO PIC X(4).
           03 FILLER PIC X(1) VALUE SPACES.
           03 IMP-DESCRIPCION PIC X(15).

       01  LISTADO-PROVEEDORES-FEATURE.
           03 FILLER PIC X(9) VALUE "COD-PROV ".
           03 FILLER PIC X(15) VALUE "CUIT-CONSORCIO ".
           03 FILLER PIC X(17) VALUE "NOMBRE-CONSORCIO ".
           03 FILLER PIC X(9) VALUE "TEL-CONS ".
           03 FILLER PIC X(9) VALUE "DIR-CONS ".

       01  LISTADO-PROVEEDORES.
           03 IMP-COD-PROV PIC 9(8).
           03 FILLER PIC X(1) VALUE SPACES.
           03 IMP-CUIT-CONS PIC 9(15).
           03 FILLER PIC X(1) VALUE SPACES.
           03 IMP-NOMB-CONS PIC X(30).
           03 FILLER PIC X(1) VALUE SPACES.
           03 IMP-TELEFONO PIC X(15).
           03 FILLER PIC X(1) VALUE SPACES.
           03 IMP-DIR PIC X(30).

       01  TOTAL-PROV-RUBRO.
           03 FILLER PIC X(29) VALUE "Total proveedores por rubro: ".
           03 IMP-TOTXRUBRO PIC X(3).

       01  LINEA PIC X(106).

       01  OP PIC 9(1).
       01  COD-PROV PIC 9(8).
       01  RUBRO PIC 9(4).
       01  DESCRIP-RUBRO PIC X(15).
       01  COD-RET PIC 9(2).

       PROCEDURE DIVISION.
       COMIENZO.
           PERFORM INICIO.
           MOVE 1 TO OP.
           CALL "SUBPRG" USING OP,COD-PROV,RUBRO,DESCRIP-RUBRO,COD-RET
           PERFORM ACTUALIZO-CONS-AND-SORT.
           MOVE 3 TO OP.
           CALL "SUBPRG" USING OP,COD-PROV,RUBRO,DESCRIP-RUBRO,COD-RET.
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
       ACTUALIZO-CONS-AND-SORT.
           SORT ARCH-ORD
                ON ASCENDING KEY ORD-RUBRO
                ON ASCENDING KEY ORD-COD-PROV
                ON ASCENDING KEY ORD-CUIT-CONS
                INPUT PROCEDURE IS ENTRADA
                OUTPUT PROCEDURE IS SALIDA.
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
           MOVE 2 TO OP.
           CALL "SUBPRG" USING OP,COD-PROV,RUBRO,DESCRIP-RUBRO,COD-RET.
           PERFORM GRABO-SORT.
           READ CPR NEXT RECORD.
      *>-----------------------------------------------------------*
      *>-----------------------------------------------------------*
       GRABO-SORT.
           MOVE RUBRO TO ORD-RUBRO.
           MOVE DESCRIP-RUBRO TO ORD-DESCR-RUBRO.
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
           MOVE "2017/06/22" TO FECHA.
           MOVE 1 TO HOJA.
           MOVE LINEA1 TO LINEA.
           DISPLAY LINEA.
           MOVE TITULO TO LINEA.
           DISPLAY LINEA.
      *>-----------------------------------------------------------*
      *>-----------------------------------------------------------*
       IMPRIMIR-RUBRO-DESCR.
           MOVE RUBRO-DESCRIPCION-FEAUTURE TO LINEA.
           DISPLAY LINEA.
           MOVE ORD-RUBRO TO IMP-RUBRO.
           MOVE ORD-DESCR-RUBRO TO IMP-DESCRIPCION.
           MOVE RUBRO-DESCRIPCION TO LINEA.
           DISPLAY LINEA.
      *>-----------------------------------------------------------*
      *>-----------------------------------------------------------*
       IMPRIMIR-FEAUTURE.
           MOVE LISTADO-PROVEEDORES-FEATURE TO LINEA.
           DISPLAY LINEA.
      *>-----------------------------------------------------------*
      *>-----------------------------------------------------------*
       IMPRIMIR-TOTXRUBRO.
           MOVE TOTXRUBRO TO IMP-TOTXRUBRO.
           MOVE TOTAL-PROV-RUBRO TO LINEA.
           DISPLAY LINEA.
      *>-----------------------------------------------------------*
      *>-----------------------------------------------------------*
       IMPRIMIR-REG-ORD.
           MOVE ORD-COD-PROV TO IMP-COD-PROV.
           MOVE ORD-CUIT-CONS TO IMP-CUIT-CONS.
           MOVE ORD-NOMB-CONS TO IMP-NOMB-CONS.
           MOVE ORD-TELEFONO TO IMP-TELEFONO.
           MOVE ORD-DIR TO IMP-DIR.
           MOVE LISTADO-PROVEEDORES TO LINEA.
           DISPLAY LINEA.
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
