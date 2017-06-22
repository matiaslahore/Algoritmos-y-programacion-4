       PROGRAM-ID. INDEXER.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OLD
               ASSIGN TO "cpr.txt"
                   ORGANIZATION IS LINE SEQUENTIAL
                   FILE STATUS IS FS-CPR.
           SELECT NEW
               ASSIGN TO "cpr_indexed.txt"
                   ACCESS MODE IS SEQUENTIAL
                   ORGANIZATION IS INDEXED
                   RECORD KEY IS CPR-CUIT-CONS2
                   ALTERNATE RECORD KEY IS CPR-COD-PROV2.

       DATA DIVISION.
       FILE SECTION.

      *>  FD OLD.
      *>          01 OLD-IN.
      *>              03 PRO-COD-PROV PIC 9(8).
      *>              03 PRO-DIR PIC X(30).
      *>              03 PRO-TEL PIC X(15).
      *>              03 PRO-RUBRO PIC 9(4).
      *>              03 PRO-DESCR-RUBRO PIC X(15).
      *>              03 PRO-FECHA-ALTA.
      *>                  05 PRO-ANIO   PIC 9(4).
      *>                  05 PRO-MES    PIC 9(2).
      *>                  05 PRO-DIA    PIC 9(2).
      *>              03 PRO-CANT-CONS-ASIG PIC 9(3).

      *>  FD NEW.
      *>          01 NEW-OUT.
      *>              03 PRO-COD-PROV2 PIC 9(8).
      *>              03 PRO-DIR2 PIC X(30).
      *>              03 PRO-TEL2 PIC X(15).
      *>              03 PRO-RUBRO2 PIC 9(4).
      *>              03 PRO-DESCR-RUBRO2 PIC X(15).
      *>              03 PRO-FECHA-ALTA2.
      *>                  05 PRO-ANIO2   PIC 9(4).
      *>                  05 PRO-MES2    PIC 9(2).
      *>                  05 PRO-DIA2    PIC 9(2).
      *>              03 PRO-CANT-CONS-ASIG2 PIC 9(3).

            FD OLD.
                    01 OLD-IN.
                        03 CPR-CUIT-CONS PIC 9(15).
                        03 CPR-COD-PROV PIC 9(8).
                        03 CPR-FECHA-ALTA.
                            05 CPR-ANIO   PIC 9(4).
                            05 CPR-MES    PIC 9(2).
                            05 CPR-DIA    PIC 9(2).

                FD NEW.
                    01 NEW-OUT.
                        03 CPR-CUIT-CONS2 PIC 9(15).
                        03 CPR-COD-PROV2 PIC 9(8).
                        03 CPR-FECHA-ALTA2.
                            05 CPR-ANIO2   PIC 9(4).
                            05 CPR-MES2    PIC 9(2).
                            05 CPR-DIA2    PIC 9(2).

       WORKING-STORAGE SECTION.
           77  FS-CPR PIC XX.
               88 OK-CPR VALUE "00".
               88 NO-CPR VALUE "23".
               88 EOF-CPR VALUE "10".

       PROCEDURE DIVISION.
       REBUILD-RTN.
           OPEN INPUT  OLD.
           OPEN OUTPUT NEW.
           READ OLD
       END-READ.

       PERFORM UNTIL FS-CPR = "10"
           WRITE NEW-OUT FROM OLD-IN
               INVALID KEY DISPLAY "NO RECORD"
           END-WRITE

           READ OLD
           END-READ
       END-PERFORM.

       CLOSE NEW, OLD.
       STOP RUN.
