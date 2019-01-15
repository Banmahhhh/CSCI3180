       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       file-control.

       DATA DIVISION.
       FILE SECTION.


       WORKING-STORAGE SECTION.
       01 x pic 9 value 5.
       01 y pic 9 value 9.
       01 z pic 9.

       PROCEDURE DIVISION.
       compute z = x +
       y
       display z
       STOP RUN.
       END PROGRAM YOUR-PROGRAM-NAME.
