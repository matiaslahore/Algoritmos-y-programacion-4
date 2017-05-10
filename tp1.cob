       IDENTIFICATION DIVISION.
       PROGRAM-ID. TP.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
        SELECT CONS1    ASSIGN TO DISK
                        ORGANIZATION IS LINE SEQUENTIAL
                        FILE STATUS IS FS-CONS1.
        SELECT CONS2    ASSIGN TO DISK
                        ORGANIZATION IS LINE SEQUENTIAL
                        FILE STATUS IS FS-CONS2.
        SELECT CONS3    ASSIGN TO DISK
                        ORGANIZATION IS LINE SEQUENTIAL
                        FILE STATUS IS FS-CONS1.
        SELECT CTA      ASSIGN TO DISK
                        ORGANIZATION IS LINE SEQUENTIAL
                        FILE STATUS IS FS-CTA.
        SELECT EST      ASSIGN TO DISK
                        ORGANIZATION IS LINE SEQUENTIAL
                        FILE STATUS IS FS-EST.
        SELECT MAE      ASSIGN TO DISK
                        ORGANIZATION IS LINE SEQUENTIAL
                        FILE STATUS IS FS-MAE.

       DATA DIVISION.
       FILE SECTION.

       FD CONS1.
       01 REG-CONS1.
            03 CONS1-CUIT-CONS    PIC 9(15).
            03 CONS1-FECHA-ALTA.
                05 CONS1-ALTA-ANIO   PIC 9(4).
                05 CONS1-ALTA-MES    PIC 9(2).
                05 CONS1-ALTA-DIA    PIC 9(2).
            03 CONS1-FECHA-BAJA.
                05 CONS1-BAJA-ANIO   PIC 9(4).
                05 CONS1-BAJA-MES    PIC 9(2).
                05 CONS1-BAJA-DIA    PIC 9(2).
            03 CONS1-ESTADO     PIC 9(2).
            03 CONS1-NOMBRE-CONSORCIO   PIC X(30).
            03 CONS1-TEL        PIC X(15).
            03 CONS1-DIR        PIC X(30).

       FD CONS2.
       01 REG-CONS2.
            03 CONS2-CUIT-CONS    PIC 9(15).
            03 CONS2-FECHA-ALTA.
                05 CONS2-ALTA-ANIO   PIC 9(4).
                05 CONS2-ALTA-MES    PIC 9(2).
                05 CONS2-ALTA-DIA    PIC 9(2).
            03 CONS2-FECHA-BAJA.
                05 CONS2-BAJA-ANIO   PIC 9(4).
                05 CONS2-BAJA-MES    PIC 9(2).
                05 CONS2-BAJA-DIA    PIC 9(2).
            03 CONS2-ESTADO     PIC 9(2).
            03 CONS2-NOMBRE-CONSORCIO   PIC X(30).
            03 CONS2-TEL        PIC X(15).
            03 CONS2-DIR        PIC X(30).

       FD CONS3.
       01 REG-CONS3.
            03 CONS3-CUIT-CONS    PIC 9(15).
            03 CONS3-FECHA-ALTA.
                05 CONS3-ALTA-ANIO   PIC 9(4).
                05 CONS3-ALTA-MES    PIC 9(2).
                05 CONS3-ALTA-DIA    PIC 9(2).
            03 CONS3-FECHA-BAJA.
                05 CONS3-BAJA-ANIO   PIC 9(4).
                05 CONS3-BAJA-MES    PIC 9(2).
                05 CONS3-BAJA-DIA    PIC 9(2).
            03 CONS3-ESTADO     PIC 9(2).
            03 CONS3-NOMBRE-CONSORCIO   PIC X(30).
            03 CONS3-TEL        PIC X(15).
            03 CONS3-DIR        PIC X(30).

       FD CTA.
       01 REG-CTA.
          03 CTA-CUIT-CONS    PIC 9(15).
          03 CTA-NRO-CTA      PIC 9(8).
          03 CTA-FECHA-ALTA.
                05 CTA-ANIO   PIC 9(4).
                05 CTA-MES    PIC 9(2).
                05 CTA-DIA    PIC 9(2).
          03 CTA-ENTIDAD      PIC 9(3).
          03 CTA-SUCURSAL     PIC 9(3).

       FD EST.
       01 REG-EST.
          03 EST-ESTADO     PIC 9(2).
          03 EST-DESCRIP    PIC X(15).

       FD MAE.
       01 REG-MAE.
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
       01  LINEA PIC X(60).

       WORKING-STORAGE SECTION.

       77  FS-CONS1 PIC XX.
            88 OK-CONS1 VALUE "00".
            88 NO-CONS1 VALUE "23".
            88 EOF-CONS1 VALUE "10".
       77  FS-CONS2 PIC XX.
            88 OK-CONS2 VALUE "00".
            88 NO-CONS2 VALUE "23".
            88 EOF-CONS2 VALUE "10".
       77  FS-CONS3 PIC XX.
            88 OK-CONS3 VALUE "00".
            88 NO-CONS3 VALUE "23".
            88 EOF-CONS3 VALUE "10".
       77  FS-CTA PIC XX.
            88 OK-CTA VALUE "00".
            88 NO-CTA VALUE "23".
            88 EOF-CTA VALUE "10".
       77  FS-EST PIC XX.
            88 OK-EST VALUE "00".
            88 NO-EST VALUE "23".
            88 EOF-EST VALUE "10".
       77 FS-MAE PIC XX.
            88 OK-MAE VALUE "00".
            88 NO-MAE VALUE "23".
            88 EOF-MAE VALUE "10".

       01  SUBINDICE PIC 9(2) VALUE 1.
       01  BAJAS PIC 9(3) VALUE 0.
       01  MIN PIC 9(15).
       01  NRO-CTA-MAE PIC 9(15) VALUE 0.

       01  TABLA-ESTADO.
           02 TAB-ESTADO OCCURS 30 TIMES.
               03 TAB-EST-ESTADO PIC 9(2).
               03 TAB-EST-DESCRIP PIC X(15).

       01  TABLA-ESTADISTICAS.
           02 TAB-ESTADIS OCCURS 10 TIMES.
               03 TAB-ANIO PIC 9(4) VALUE 0.
               03 TAB-CANT PIC 9(3) VALUE 0.

       01  LINEA1.
           03 FILLER PIC X(7) VALUE "Fecha: ".
           03 FECHA PIC X(10).
           03 FILLER PIC X(32) VALUE SPACES.
           03 FILLER PIC X(9) VALUE "Hoja nro ".
           03 HOJA PIC 9(2).
       01  TITULO.
           03 FILLER PIC X(15) VALUE SPACES.
           03 FILLER PIC X(29) VALUE "LISTADO DE CONSORCIOS DE BAJA".
           03 FILLER PIC X(16) VALUE SPACES.
       01  IMP-BAJAS.
           03 FILLER PIC X(32) VALUE "Total consorcios dados de baja: ".
           03 CANT-BAJAS PIC 9(3) VALUE 0.
           03 FILLER PIC X(25) VALUE SPACES.

       PROCEDURE DIVISION.
       COMIENZO.
            DISPLAY 'Bienvenidos'.
            PERFORM INICIO.
            PERFORM LEER-CONS1.
            PERFORM LEER-CONS2.
            PERFORM LEER-CONS3.
            PERFORM LEER-CTA.
            PERFORM IMPR-CABECERA.
            PERFORM CARGAR-TABLAS.
            PERFORM PROCESAR-ARCHIVOS UNTIL EOF-CONS1
                                      AND EOF-CONS2
                                      AND EOF-CONS3.
            PERFORM IMPR-TOT-BAJAS.
            PERFORM IMPR-ESTADISTICAS.
            PERFORM CERRAR-ARCHIVOS.
            STOP RUN.

      *-----------------------------------------------------------*
      *-----------------------------------------------------------*
       INICIO.
        OPEN INPUT CONS1.
        OPEN INPUT CONS2.
        OPEN INPUT CONS3.
        OPEN INPUT CTA.
        OPEN INPUT EST.
        OPEN OUTPUT MAE.
      *-----------------------------------------------------------*
      *-----------------------------------------------------------*
       CERRAR-ARCHIVOS.
        CLOSE CONS1.
        CLOSE CONS2.
        CLOSE CONS3.
        CLOSE CTA.
        CLOSE EST.
        CLOSE MAE.
      *-----------------------------------------------------------*
      *-----------------------------------------------------------*
       CARGAR-TABLAS.
        PERFORM CARGAR-ESTADO UNTIL EOF-EST.
      *  MOVE 1 TO SUBINDICE.
      *  PERFORM CARGAR-ESTADISTICAS UNTIL ??.
      *-----------------------------------------------------------*
      *-----------------------------------------------------------*
       CARGAR-ESTADO.
        PERFORM LEER-EST.
        PERFORM LLENAR-TABLA-EST UNTIL EOF-EST OR SUBINDICE > 30.
      *-----------------------------------------------------------*
      *-----------------------------------------------------------*
       LLENAR-TABLA-EST.
        MOVE EST-ESTADO TO TAB-EST-ESTADO(SUBINDICE).
        MOVE EST-DESCRIP  TO TAB-EST-DESCRIP(SUBINDICE).
        ADD 1 TO SUBINDICE.
        PERFORM LEER-EST.
      *-----------------------------------------------------------*
      *-----------------------------------------------------------*
       CARGAR-ESTADISTICAS.
      * MOVE ID TO TAB-EST-ID(SUBINDICE).
      * ADD 1 TO SUBINDICE.
      * PERFORM LEER-ESTADISTICAS.

      *-----------------------------------------------------------*
      *-----------------------------------------------------------*
       LEER-CONS1.
        READ CONS1
            AT END MOVE "10" TO FS-CONS1.
        IF FS-CONS1 NOT EQUAL ZERO AND "10"
            DISPLAY 'ERROR AL LEER CONS1' FS-CONS1.
      *-----------------------------------------------------------*
      *-----------------------------------------------------------*
       LEER-CONS2.
        READ CONS2
            AT END MOVE "10" TO FS-CONS2.
        IF FS-CONS2 NOT EQUAL ZERO AND "10"
            DISPLAY 'ERROR AL LEER CONS2' FS-CONS2.
      *-----------------------------------------------------------*
      *-----------------------------------------------------------*
       LEER-CONS3.
        READ CONS3
            AT END MOVE "10" TO FS-CONS3.
        IF FS-CONS3 NOT EQUAL ZERO AND "10"
            DISPLAY 'ERROR AL LEER CONS3' FS-CONS3.
      *-----------------------------------------------------------*
      *-----------------------------------------------------------*
       LEER-CTA.
        READ CTA
            AT END MOVE "10" TO FS-CTA.
        IF FS-CTA NOT EQUAL ZERO AND "10"
            DISPLAY 'ERROR AL LEER CTA' FS-CTA.
      *-----------------------------------------------------------*
      *-----------------------------------------------------------*
       IMPR-CABECERA.
           MOVE "1993/05/22" TO FECHA.
           MOVE 1 TO HOJA.
           MOVE LINEA1 TO LINEA.
           DISPLAY LINEA.
           MOVE TITULO TO LINEA.
           DISPLAY LINEA.
      *-----------------------------------------------------------*
      *-----------------------------------------------------------*
       LEER-EST.
        READ EST
            AT END MOVE "10" TO FS-EST.
        IF FS-EST NOT EQUAL ZERO AND "10"
            DISPLAY 'ERROR AL LEER EST' FS-EST.
      *-----------------------------------------------------------*
      *-----------------------------------------------------------*
       PROCESAR-ARCHIVOS.
           PERFORM DET-MIN.
           PERFORM POS-CTAS.
           MOVE 0 TO NRO-CTA-MAE.
           IF MIN EQUAL CONS1-CUIT-CONS
               PERFORM PROCESO-C1.
           IF MIN EQUAL CONS2-CUIT-CONS
               PERFORM PROCESO-C2.
           IF MIN EQUAL CONS3-CUIT-CONS
               PERFORM PROCESO-C3.

      *-----------------------------------------------------------*
      *-----------------------------------------------------------*
       DET-MIN.
           MOVE CONS1-CUIT-CONS TO MIN.
           IF MIN >= CONS2-CUIT-CONS
               MOVE CONS2-CUIT-CONS TO MIN.
           IF MIN >= CONS3-CUIT-CONS
               MOVE CONS3-CUIT-CONS TO MIN.
           IF MIN >= CTA-CUIT-CONS
               MOVE CTA-CUIT-CONS TO MIN.
      *-----------------------------------------------------------*
      *-----------------------------------------------------------*
       POS-CTAS.
           IF MIN EQUAL CTA-CUIT-CONS
               MOVE CTA-NRO-CTA TO NRO-CTA-MAE.
           PERFORM LEER-CTA.
       PROCESO-C1.
       PROCESO-C2.
       PROCESO-C3.
      *-----------------------------------------------------------*
      *-----------------------------------------------------------*
       IMPR-TOT-BAJAS.
           MOVE BAJAS TO CANT-BAJAS.
           MOVE IMP-BAJAS TO LINEA.
           DISPLAY LINEA.
      *-----------------------------------------------------------*
      *-----------------------------------------------------------*
       IMPR-ESTADISTICAS.
      *-----------------------------------------------------------*
      *-----------------------------------------------------------*
