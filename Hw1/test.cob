       identification division.
       program-id. laji.

       environment division.
            input-output section.
            file-control.
              select tempfile assign to 'new.txt'
              organization is line sequential.

       data division.
            file section.
            fd tempfile.
            01 banma pic x(25).
            01 date-line.
                05 datealp pic x(6).
                05 NAME-OF-MONTH-01 PIC X(7).
                05 NAME-OF-MONTH-02 PIC X(8).
                05 NAME-OF-MONTH-03 PIC X(5).
                05 NAME-OF-MONTH-04 PIC X(5).
                *> 05 NAME-OF-MONTH-05 PIC X(09) VALUE 'MAY'.
                *> 05 NAME-OF-MONTH-06 PIC X(09) VALUE 'JUNE'.
                *> 05 NAME-OF-MONTH-07 PIC X(09) VALUE 'JULY'.
                *> 05 NAME-OF-MONTH-08 PIC X(09) VALUE 'AUGUST'.
                *> 05 NAME-OF-MONTH-09 PIC X(09) VALUE 'SEPTEMBER'.
                *> 05 NAME-OF-MONTH-10 PIC X(09) VALUE 'OCTOBER'.
                *> 05 NAME-OF-MONTH-11 PIC X(09) VALUE 'NOVEMBER'.
                *> 05 NAME-OF-MONTH-12 PIC X(09) VALUE 'DECEMBER'.
                05 notused1 pic x.
                05 fday pic 99.
                05 notused2 pic xx.
                05 fyear pic 9999.

            working-storage section.
            01 ws-date-line.
                05 ws-datealp pic x(6).
                05 ws-NAME-OF-MONTH-01 PIC X(7).
                05 ws-NAME-OF-MONTH-02 PIC X(8).
                05 ws-NAME-OF-MONTH-03 PIC X(5).
                05 ws-NAME-OF-MONTH-04 PIC X(5).
                *> 05 NAME-OF-MONTH-05 PIC X(09) VALUE 'MAY'.
                *> 05 NAME-OF-MONTH-06 PIC X(09) VALUE 'JUNE'.
                *> 05 NAME-OF-MONTH-07 PIC X(09) VALUE 'JULY'.
                *> 05 NAME-OF-MONTH-08 PIC X(09) VALUE 'AUGUST'.
                *> 05 NAME-OF-MONTH-09 PIC X(09) VALUE 'SEPTEMBER'.
                *> 05 NAME-OF-MONTH-10 PIC X(09) VALUE 'OCTOBER'.
                *> 05 NAME-OF-MONTH-11 PIC X(09) VALUE 'NOVEMBER'.
                *> 05 NAME-OF-MONTH-12 PIC X(09) VALUE 'DECEMBER'.
                05 ws-notused1 pic x.
                05 ws-fday pic 99.
                05 ws-notused2 pic xx.
                05 ws-fyear pic 9999.


       procedure division.
            main-para.
            *> if ws-month = 1
                *> then move 'Janurary' to fmonth
            *> else
                *> if ws-month = 4
                    *> then move 'April' to fmonth
                *> END-IF
            *> end-if.


            move 'Date: ' to ws-datealp.
            move 'April' to ws-NAME-OF-MONTH-04.
            move ' 'to ws-NAME-OF-MONTH-01.
            move ' ' to ws-notused1.
            move 4 to ws-fday.
            move ', ' to ws-notused2.
            move 2019 to ws-fyear.

            DISPLAY 'ws-date-line is'
            DISPLAY ws-date-line.


            move 'Daily: Attendance Summary' to banma.
            display 'banma is '
            display banma

            move ws-date-line to date-line
            display 'date-line is '
            display date-line

            *> display datealp
            *> display '01' NAME-OF-MONTH-01
            *> display '02' NAME-OF-MONTH-02
            *> display '03' NAME-OF-MONTH-03
            *> display '04' NAME-OF-MONTH-04
            *> DISPLAY 'dateline is'
            *> DISPLAY date-line.

            *> open output tempfile.
            *> write banma
            *> write date-line
            *> end-write
            *> close tempfile.
            *> display 'finish writing'

            *> open input tempfile
            *> close tempfile.

            stop run.
