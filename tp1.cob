      ******************************************************************
      * Authors: Santiago Sosa Montiel, Matias Lahore, Matias Tebele,
      *          Nicolas Outeda.
      * Date: 3/4/2017 DD/MM/AAAA
      * Purpose: TP Materia: Algoritmos y Programacion IV
      * Tectonics: cobc
      ******************************************************************

       IDENTIFICATION DIVISION.
       PROGRAM-ID. TP.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CONS1 ASSIGN TO "cons1.txt"
                        ORGANIZATION IS LINE SEQUENTIAL
                        FILE STATUS IS FS-CONS1.
           SELECT CONS2 ASSIGN TO "cons2.txt"
                        ORGANIZATION IS LINE SEQUENTIAL
                        FILE STATUS IS FS-CONS2.
           SELECT CONS3 ASSIGN TO "cons3.txt"
                        ORGANIZATION IS LINE SEQUENTIAL
                        FILE STATUS IS FS-CONS3.
           SELECT CTA   ASSIGN TO "cta.txt"
                        ORGANIZATION IS LINE SEQUENTIAL
                        FILE STATUS IS FS-CTA.
           SELECT EST   ASSIGN TO "estado.txt"
                        ORGANIZATION IS LINE SEQUENTIAL
                        FILE STATUS IS FS-EST.
           SELECT MAE   ASSIGN TO "maestro.txt"
                        ORGANIZATION IS LINE SEQUENTIAL
                        FILE STATUS IS FS-MAE.

       DATA DIVISION.
       FILE SECTION.

       FD  CONS1.
       01  REG-CONS1.
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
       01  REG-CONS2.
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
       01  REG-CONS3.
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

       FD  CTA.
       01  REG-CTA.
           03 CTA-CUIT-CONS    PIC 9(15).
           03 CTA-NRO-CTA      PIC 9(8).
           03 CTA-FECHA-ALTA.
                05 CTA-ANIO   PIC 9(4).
                05 CTA-MES    PIC 9(2).
                05 CTA-DIA    PIC 9(2).
           03 CTA-ENTIDAD      PIC 9(3).
           03 CTA-SUCURSAL     PIC 9(3).

       FD  EST.
       01  REG-EST.
           03 EST-ESTADO     PIC 9(2).
           03 EST-DESCRIP    PIC X(15).

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
       01  ALTAS PIC 9(3) VALUE 0.
       01  MIN PIC 9(15) VALUE 0.
       01  NRO-CTA-MAE PIC 9(15) VALUE 0.

       01  DIR-CONSORCIO PIC X(30).
       01  TEL-CONSORCIO PIC X(15).
       01  NOMBRE-CONSORCIO PIC X(30).
       01  ESTADO-CONSORCIO PIC 9(02) VALUE 0.
       01  FECHA-ALTA-CONSORCIO.
           03 FAC-ANIO   PIC 9(4).
           03 FAC-MES    PIC 9(2).
           03 FAC-DIA    PIC 9(2).
       01  FECHA-BAJA-CONSORCIO.
           03 FBC-ANIO   PIC 9(4).
           03 FBC-MES    PIC 9(2).
           03 FBC-DIA    PIC 9(2).
       01  CUIT-CONSORCIO PIC 9(15).

       01  TABLA-ESTADO.
           03 TAB-ESTADO OCCURS 30 TIMES
           INDEXED BY TABLA-ID-EST-INDEX.
              05 TAB-EST-ESTADO PIC 9(2).
              05 TAB-EST-DESCRIP PIC X(15).

       01  TABLA-ESTADISTICAS.
           03 TAB-ESTADIS OCCURS 10 TIMES
           INDEXED BY TABLA-ID-ESTADISTICAS-INDEX.
               05 TAB-ANIO PIC 9(4) VALUE 0.
               05 TAB-CANT PIC 9(3) VALUE 0.

       01  LINEA1.
           03 FILLER PIC X(7) VALUE "Fecha: ".
           03 FECHA PIC X(10).
           03 FILLER PIC X(62) VALUE SPACES.
           03 FILLER PIC X(9) VALUE "Hoja nro ".
           03 HOJA PIC 9(2).

       01  CONSOR-BAJA-ROTULO.
           03 FILLER PIC X(16) VALUE "CUIT-CONS       ".
           03 FILLER PIC X(9) VALUE "FEC-ALTA ".
           03 FILLER PIC X(9) VALUE "FEC-BAJA ".
           03 FILLER PIC X(30) VALUE "NOMBRE                        ".
           03 FILLER PIC X(15) VALUE "TELEFONO       ".
           03 FILLER PIC X(30) VALUE "DIRECCION                     ".

       01  CONSOR-BAJA.
           03 IMPR-CUIT-CONS PIC 9(15).
           03 FILLER PIC X(1) VALUE SPACES.
           03 IMPR-FECHA-ALTA-CONSORCIO.
               05 IMPR-FAC-ANIO   PIC 9(4).
               05 IMPR-FAC-MES    PIC 9(2).
               05 IMPR-FAC-DIA    PIC 9(2).
           03 FILLER PIC X(1) VALUE SPACES.
           03 IMPR-FECHA-BAJA-CONSORCIO.
               05 IMPR-FAC-ANIO   PIC 9(4).
               05 IMPR-FAC-MES    PIC 9(2).
               05 IMPR-FAC-DIA    PIC 9(2).
           03 FILLER PIC X(1) VALUE SPACES.
           03 IMPR-NOMBRE PIC X(30) VALUE SPACES.
           03 IMPR-TEL PIC X(15) VALUE SPACES.
           03 IMPR-DIR PIC X(30) VALUE SPACES.

       01  TITULO.
           03 FILLER PIC X(25) VALUE SPACES.
           03 FILLER PIC X(29) VALUE "LISTADO DE CONSORCIOS DE BAJA".
           03 FILLER PIC X(16) VALUE SPACES.

       01  IMP-BAJAS.
           03 FILLER PIC X(32) VALUE "Total consorcios dados de baja: ".
           03 CANT-BAJAS PIC 9(3) VALUE 0.
           03 FILLER PIC X(25) VALUE SPACES.

       01  IMPR-EST-LINEA.
           03 IMPR-EST-ANIO PIC 9(4) VALUE 0.
           03 FILLER PIC X(1) VALUE SPACES.
           03 IMPR-EST-CANT PIC 9(3) VALUE 0.

       PROCEDURE DIVISION.
       COMIENZO.
           PERFORM INICIO.
           PERFORM LEER-CONS1.
           PERFORM LEER-CONS2.
           PERFORM LEER-CONS3.
           PERFORM LEER-CTA.
           PERFORM IMPR-CABECERA.
           PERFORM CARGAR-TABLAS.
           PERFORM PROCESAR-ARCHIVOS UNTIL EOF-CONS1
                                      AND EOF-CONS2
                                      AND EOF-CONS3
                                      AND EOF-CTA.
           PERFORM IMPR-TOT-BAJAS.
           PERFORM IMPR-ESTADISTICAS.
           PERFORM CERRAR-ARCHIVOS.
           STOP RUN.
      *>-----------------------------------------------------------*
      *>-----------------------------------------------------------*
       INICIO.
           OPEN INPUT CONS1.
           IF FS-CONS1 NOT = "00"
               DISPLAY "Error en open cons1 FS: " FS-CONS1.
           OPEN INPUT CONS2.
           IF FS-CONS2 NOT = "00"
               DISPLAY "Error en open cons2 FS: " FS-CONS2.
           OPEN INPUT CONS3.
           IF FS-CONS3 NOT = "00"
               DISPLAY "Error en open cons3 FS: " FS-CONS3.
           OPEN INPUT CTA.
           IF FS-CTA NOT = "00"
               DISPLAY "Error en open cta FS: " FS-CTA.
           OPEN INPUT EST.
           IF FS-EST NOT = "00"
               DISPLAY "Error en open est FS: " FS-EST.
           OPEN OUTPUT MAE.
      *>-----------------------------------------------------------*
      *>-----------------------------------------------------------*
       CERRAR-ARCHIVOS.
           CLOSE CONS1.
           CLOSE CONS2.
           CLOSE CONS3.
           CLOSE CTA.
           CLOSE EST.
           CLOSE MAE.
      *>-----------------------------------------------------------*
      *>-----------------------------------------------------------*
       CARGAR-TABLAS.
           MOVE 1 TO SUBINDICE.
           PERFORM LEER-EST.
           PERFORM LLENAR-TABLA-EST UNTIL EOF-EST OR SUBINDICE > 30.
           MOVE 1 TO SUBINDICE.
           MOVE 2007 TO ANIO.
           PERFORM LLENAR-ANIO-ESTADIS UNTIL SUBINDICE>10.
      *>-----------------------------------------------------------*
      *>-----------------------------------------------------------*
       LLENAR-TABLA-EST.
           MOVE EST-ESTADO TO TAB-EST-ESTADO(SUBINDICE).
           MOVE EST-DESCRIP  TO TAB-EST-DESCRIP(SUBINDICE).
           ADD 1 TO SUBINDICE.
           PERFORM LEER-EST.
      *>-----------------------------------------------------------*
      *>-----------------------------------------------------------*
       LLENAR-ANIO-ESTADIS.
           MOVE ANIO TO TAB-ANIO(SUBINDICE).
           MOVE 0 TO TAB-CANT(SUBINDICE).
           ADD 1 TO ANIO.
           ADD 1 TO SUBINDICE.
      *>-----------------------------------------------------------*
      *>-----------------------------------------------------------*
       LEER-CONS1.
           READ CONS1
               AT END MOVE "10" TO FS-CONS1.
           IF FS-CONS1 NOT EQUAL "00" AND "10"
               DISPLAY 'ERROR AL LEER CONS1 FS: ' FS-CONS1.
           IF FS-CONS1 EQUAL "10"
               MOVE 999999999999999 TO CONS1-CUIT-CONS.
      *>-----------------------------------------------------------*
      *>-----------------------------------------------------------*
       LEER-CONS2.
           READ CONS2
               AT END MOVE "10" TO FS-CONS2.
           IF FS-CONS2 NOT EQUAL ZERO AND "10"
               DISPLAY 'ERROR AL LEER CONS2 FS: ' FS-CONS2.
           IF FS-CONS2 EQUAL "10"
               MOVE 999999999999999 TO CONS2-CUIT-CONS.
      *>-----------------------------------------------------------*
      *>-----------------------------------------------------------*
       LEER-CONS3.
           READ CONS3
               AT END MOVE "10" TO FS-CONS3.
           IF FS-CONS3 NOT EQUAL ZERO AND "10"
               DISPLAY 'ERROR AL LEER CONS3 ' FS-CONS3.
           IF FS-CONS3 EQUAL "10"
               MOVE 999999999999999 TO CONS3-CUIT-CONS.
      *>-----------------------------------------------------------*
      *>-----------------------------------------------------------*
       LEER-CTA.
           READ CTA
               AT END MOVE "10" TO FS-CTA.
           IF FS-CTA NOT EQUAL ZERO AND "10"
               DISPLAY 'ERROR AL LEER CTA ' FS-CTA.
           IF FS-CTA EQUAL "10"
               MOVE 999999999999999 TO CTA-CUIT-CONS.
      *>-----------------------------------------------------------*
      *>-----------------------------------------------------------*
       IMPR-CABECERA.
           MOVE "2017/05/11" TO FECHA.
           MOVE 1 TO HOJA.
           MOVE LINEA1 TO LINEA.
           DISPLAY LINEA.
           MOVE TITULO TO LINEA.
           DISPLAY LINEA.
           MOVE CONSOR-BAJA-ROTULO TO LINEA.
           DISPLAY LINEA.
      *>-----------------------------------------------------------*
      *>-----------------------------------------------------------*
       LEER-EST.
           READ EST
               AT END MOVE "10" TO FS-EST.
           IF FS-EST NOT EQUAL ZERO AND "10"
               DISPLAY 'ERROR AL LEER EST' FS-EST.
      *>-----------------------------------------------------------*
      *>-----------------------------------------------------------*
       PROCESAR-ARCHIVOS.
           PERFORM DET-MIN.
      *    SI NO EXISTE NUMERO DE CUENTA LO DEJO INICIALIZADO EN 0
           MOVE 00000000 TO MAE-CTA
           PERFORM POS-CTAS.
           IF MIN EQUAL CONS1-CUIT-CONS
              PERFORM ULTIMO-REG-CONS1 UNTIL EOF-CONS1
                              OR MIN <> CONS1-CUIT-CONS.
           IF MIN EQUAL CONS2-CUIT-CONS
              PERFORM ULTIMO-REG-CONS2 UNTIL EOF-CONS2
                              OR MIN <> CONS2-CUIT-CONS.
           IF MIN EQUAL CONS3-CUIT-CONS
              PERFORM ULTIMO-REG-CONS3 UNTIL EOF-CONS3
                              OR MIN <> CONS3-CUIT-CONS.
           IF ESTADO-CONSORCIO = 2
               PERFORM IMPR-CONS
               ADD 1 TO BAJAS.
           IF ESTADO-CONSORCIO <> 2
               PERFORM BUSCAR-ESTADO
               PERFORM ESCRIBO-MAE
               ADD 1 TO ALTAS
               PERFORM BUSCAR-ESTADISTICAS.
      *>-----------------------------------------------------------*
      *>-----------------------------------------------------------*
       DET-MIN.
               MOVE CONS1-CUIT-CONS TO MIN.
               IF MIN >= CONS2-CUIT-CONS
                   MOVE CONS2-CUIT-CONS TO MIN.
               IF MIN >= CONS3-CUIT-CONS
                   MOVE CONS3-CUIT-CONS TO MIN.
               IF MIN >= CTA-CUIT-CONS
                   MOVE CTA-CUIT-CONS TO MIN.
      *>-----------------------------------------------------------*
      *>-----------------------------------------------------------*
       POS-CTAS.
           IF MIN EQUAL CTA-CUIT-CONS
               MOVE CTA-NRO-CTA TO MAE-CTA
               MOVE CTA-FECHA-ALTA TO MAE-FECHA-ALTA
               IF FS-CTA NOT EQUAL "10"
                   PERFORM LEER-CTA.
      *>-----------------------------------------------------------*
      *>-----------------------------------------------------------*
       BUSCAR-ESTADO.
           MOVE 1 TO TABLA-ID-EST-INDEX.
           SEARCH TAB-ESTADO
               AT END MOVE 'SIN ESTADO' TO MAE-DESCRIP-ESTADO
           WHEN TAB-EST-ESTADO(TABLA-ID-EST-INDEX) = ESTADO-CONSORCIO
               MOVE TAB-EST-DESCRIP(TABLA-ID-EST-INDEX)
               TO MAE-DESCRIP-ESTADO.
      *>-----------------------------------------------------------*
      *>-----------------------------------------------------------*
       ESCRIBO-MAE.
           MOVE MIN TO MAE-CUIT-CONS
           MOVE NOMBRE-CONSORCIO TO MAE-NOMBRE-CONSORCIO.
           MOVE TEL-CONSORCIO TO MAE-TEL.
           MOVE DIR-CONSORCIO TO MAE-DIR.
           MOVE FECHA-ALTA-CONSORCIO TO MAE-FECHA-ALTA.
           WRITE REG-MAE.
      *>-----------------------------------------------------------*
      *>-----------------------------------------------------------*
       BUSCAR-ESTADISTICAS.
           MOVE 1 TO TABLA-ID-ESTADISTICAS-INDEX.
           SEARCH TAB-ESTADIS
               AT END PERFORM EST-AGREGAR-NUEVO
           WHEN TAB-ANIO(TABLA-ID-ESTADISTICAS-INDEX)
               EQUAL MAE-ANIO
               ADD 1 TO TAB-CANT(TABLA-ID-ESTADISTICAS-INDEX).
      *>-----------------------------------------------------------*
      *>-----------------------------------------------------------*
       IMPR-TOT-BAJAS.
           MOVE BAJAS TO CANT-BAJAS.
           MOVE IMP-BAJAS TO LINEA.
           DISPLAY LINEA.
      *>-----------------------------------------------------------*
      *>-----------------------------------------------------------*
       EST-AGREGAR-NUEVO.
      *    MOVE MAE-FECHA-ALTA TO TAB-ANIO(TABLA-ID-ESTADISTICAS-INDEX).
      *     MOVE 1 TO TAB-CANT(TABLA-ID-ESTADISTICAS-INDEX).
      *>-----------------------------------------------------------*
      *>-----------------------------------------------------------*
       ULTIMO-REG-CONS1.
           MOVE CONS1-DIR TO DIR-CONSORCIO.
           MOVE CONS1-TEL TO TEL-CONSORCIO.
           MOVE CONS1-NOMBRE-CONSORCIO TO NOMBRE-CONSORCIO.
           MOVE CONS1-ESTADO TO ESTADO-CONSORCIO.
           MOVE CONS1-FECHA-BAJA TO FECHA-BAJA-CONSORCIO.
           MOVE CONS1-FECHA-ALTA TO FECHA-ALTA-CONSORCIO.
           MOVE CONS1-CUIT-CONS TO CUIT-CONSORCIO.
           PERFORM LEER-CONS1.
      *>-----------------------------------------------------------*
      *>-----------------------------------------------------------*
       ULTIMO-REG-CONS2.
           MOVE CONS2-DIR TO DIR-CONSORCIO.
           MOVE CONS2-TEL TO TEL-CONSORCIO.
           MOVE CONS2-NOMBRE-CONSORCIO TO NOMBRE-CONSORCIO.
           MOVE CONS2-ESTADO TO ESTADO-CONSORCIO.
           MOVE CONS2-FECHA-BAJA TO FECHA-BAJA-CONSORCIO.
           MOVE CONS2-FECHA-ALTA TO FECHA-ALTA-CONSORCIO.
           PERFORM LEER-CONS2.
      *>-----------------------------------------------------------*
      *>-----------------------------------------------------------*
       ULTIMO-REG-CONS3.
           MOVE CONS3-DIR TO DIR-CONSORCIO.
           MOVE CONS3-TEL TO TEL-CONSORCIO.
           MOVE CONS3-NOMBRE-CONSORCIO TO NOMBRE-CONSORCIO.
           MOVE CONS3-ESTADO TO ESTADO-CONSORCIO.
           MOVE CONS3-FECHA-BAJA TO FECHA-BAJA-CONSORCIO.
           MOVE CONS3-FECHA-ALTA TO FECHA-ALTA-CONSORCIO.
           PERFORM LEER-CONS3.
      *>-----------------------------------------------------------*
      *>-----------------------------------------------------------*
       IMPR-ESTADISTICAS.
           DISPLAY "ANIO CANT                                          "
           MOVE 1 TO TABLA-ID-ESTADISTICAS-INDEX.
           PERFORM IMPR-LISTADO UNTIL TABLA-ID-ESTADISTICAS-INDEX > 10.
      *>-----------------------------------------------------------*
      *>-----------------------------------------------------------*
       IMPR-LISTADO.
           MOVE TAB-ANIO(TABLA-ID-ESTADISTICAS-INDEX) TO IMPR-EST-ANIO.
           MOVE TAB-CANT(TABLA-ID-ESTADISTICAS-INDEX) TO IMPR-EST-CANT.
           MOVE IMPR-EST-LINEA TO LINEA.
           DISPLAY LINEA.
           ADD 1 TO TABLA-ID-ESTADISTICAS-INDEX.
      *>-----------------------------------------------------------*
      *>-----------------------------------------------------------*
       IMPR-CONS.
           MOVE MIN TO IMPR-CUIT-CONS.
           MOVE FECHA-ALTA-CONSORCIO TO IMPR-FECHA-ALTA-CONSORCIO.
           MOVE FECHA-BAJA-CONSORCIO TO IMPR-FECHA-BAJA-CONSORCIO.
           MOVE NOMBRE-CONSORCIO TO IMPR-NOMBRE.
           MOVE TEL-CONSORCIO TO IMPR-TEL.
           MOVE DIR-CONSORCIO TO IMPR-DIR.
           MOVE CONSOR-BAJA TO LINEA.
           DISPLAY LINEA.
      *>-----------------------------------------------------------*
