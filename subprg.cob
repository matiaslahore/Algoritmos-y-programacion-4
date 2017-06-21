      ******************************************************************
      * Author: Santiago Sosa Montiel, Matias Lahore
      * Date:2017/06/21
      * Purpose: Trabajo Práctico, Algoritmos y Programación IV
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SUBPRG.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PROV     ASSIGN TO DISK "prov.txt"
                           ORGANIZATION IS INDEXED
                           RECORD KEY IS PRO-COD-PROV
                           FILE STATUS IS FS-PROV.

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
               05 PRO-ANIO   PIC 9(4).
               05 PRO-MES    PIC 9(2).
               05 PRO-DIA    PIC 9(2).
           03 PRO-CANT-CONS-ASIG PIC 9(3).

       WORKING-STORAGE SECTION.

       77  FS-PROV PIC XX.
           88 OK-PROV VALUE "00".
           88 NO-PROV VALUE "23".
           88 EOF-PROV VALUE "10".

       LINKAGE SECTION.
       01  OP PIC 9(1).
       01  COD-PROV PIC 9(8).
       01  RUBRO PIC 9(4).
       01  DESCRIP-RUBRO PIC X(15).
       01  COD-RET PIC 9(2).

       PROCEDURE DIVISION USING OP,COD-PROV,RUBRO,DESCRIP-RUBRO,COD-RET.
       MAIN-PROCEDURE.
            IF OP=1
                OPEN I-O PROV
                IF FS-PROV NOT = '00'
                    DISPLAY "Error en open prov, FS: ", FS-PROV
                    EXIT PROGRAM.
            IF OP=2
                MOVE COD-PROV TO PRO-COD-PROV.
                READ PROV RECORD
                IF OK-PROV
                    ADD 1 TO PRO-CANT-CONS-ASIG
                    REWRITE REG-PROV
                    MOVE PRO-RUBRO TO RUBRO
                    MOVE PRO-DESCR-RUBRO TO DESCRIP-RUBRO
                    MOVE FS-PROV TO COD-RET
                ELSE
                    MOVE FS-PROV TO COD-RET.
                EXIT PROGRAM.
            IF OP=3
                CLOSE PROV
                EXIT PROGRAM.

       END PROGRAM SUBPRG.
