       identification division.
       program-id. laji.

       environment division.
            input-output section.
            file-control.
              select att
              assign to '/Users/banma/attendance.txt'
              organization is line sequential
              file status is att-status.
              select emp
              assign to '/Users/banma/employees.txt'
              organization is line sequential
              file status is emp-status.
              select mon
              assign to '/Users/banma/monthly-attendance.txt'
              organization is line sequential
              file status is mon-status.

              select moncob
              assign to 'monthly-attendancecob.txt'
              organization is line SEQUENTIAL.

              select tempfile assign to 'test.txt'
              organization is indexed
              access mode is random
              record key is id-temp
              file status is tempfile-status.

              select summ
              assign to 'summarycob.txt'
              organization is line SEQUENTIAL.


       data division.
            file section.
            fd att.
            01 fdate.
               05 fatt-year pic 9999.
               05 fdash1 pic x.
               05 fatt-month pic 99.
               05 fdash2 pic x.
               05 fatt-day pic 99.
            01 attendance.
               05 id-att pic 9(4).
               05 a-l pic a(6).
               05 the-date pic x(11).
               05 time-hour pic 9(2).
               05 not-used pic x(1).
               05 time-minute pic 9(2).

            fd emp.
            01 employees.
               05 id-emp pic 9999.
               05 first-name pic x(10).
               05 last-name pic x(20).
               05 gender pic x.
               05 birth pic x(10).
               05 hire-date pic x(10).
               05 depart pic x(3).
               05 salary pic 999999.

            fd mon.
            01 mon-date pic 9999x99.
            01 mon-att.
                05 id-mon pic 9999.
                05 absent pic 999.
                05 15-late pic 999.
                05 overtime pic 999.

            fd moncob.
            01 mon-date1.
               05 date1-line pic 9999x99.
               05 mon-cr1 pic x.
            01 mon-att1.
                05 id-mon1 pic 9999.
                05 absent1 pic 999.
                05 15-late1 pic 999.
                05 overtime1 pic 999.
                05 mon-cr2 pic x.

            fd tempfile.
            01 temp-info.
                05 id-temp pic 9999.
                05 kong1 pic x(5).
                05 first-name-temp pic x(10).
                05 last-name-temp pic x(20).
                05 depart-temp pic x(11).
                05 sus-temp pic 9.
                05 late-temp pic 99.
                05 overtime-temp pic 9.

            fd summ.
            01 fline.
               05 first-line pic x(24).
               05 f-cr pic x.
            01 sline.
               05 second-line pic x(23).
               05 s-cr pic x.
            01 tline.
               05 third pic x(58).
               05 t-cr pic x.
            01 dash-line.
               05 dash-line1 pic x(62).
               05 d-cr pic x.

            01 sum-info.
                05 id-sum pic 9999.
                05 fspace pic x(5).
                05 first-name-sum pic x(11).
                05 last-name-sum pic x(21).
                05 depart-sum pic x(11).
                05 status-sum pic x(10).
                05 indo-cr pic x.
            01 laji.
               05 laji-line pic x(62).
               05 laji-cr pic x.
            01 presences.
                05 p-line pic x(20).
                05 p-num pic zzzz9.
                05 pre-cr pic x.
            01 absences.
                05 a-line pic x(19).
                05 a-num pic zzzz9.
                05 abs-cr pic x.
            01 late.
                05 l-line pic x(24).
                05 l-num pic zzzz9.
                05 lat-cr pic x.
            01 suspicous.
                05 s-line pic x(29).
                05 s-num pic zzzz9.
                05 sus-cr pic x.

            working-storage section.
      * att
            01 ws-date.
               05 att-year pic 9999.
               05 dash1 pic x.
               05 att-month pic 99.
               05 dash2 pic x.
               05 att-day pic 99.
            01 ws-attendance.
               05 ws-id-att pic 9(4).
               05 ws-a-l pic a(6).
               05 ws-the-date pic x(11).
               05 ws-time-hour pic 9(2).
               05 ws-not-used pic x(1).
               05 ws-time-minute pic 9(2).
            01 att-status pic xx.

      * emp
            01 ws-employees.
               05 ws-id-emp pic 9999.
               05 ws-first-name pic x(10).
               05 ws-last-name pic x(20).
               05 ws-gender pic x.
               05 ws-birth pic x(10).
               05 ws-hire-date pic x(10).
               05 ws-depart pic x(3).
               05 ws-salary pic 999999.
            01 emp-status pic xx.

      * monx
            01 ws-mon-date pic 9999x99.
            01 ws-mon-att.
                05 ws-id-mon pic 9999.
                05 ws-absent pic 999.
                05 ws-15-late pic 999.
                05 ws-overtime pic 999.
             01 mon-status pic xx.

      * tempfile
            01 ws-temp.
                05 ws-id-temp pic 9999.
                05 ws-kong1 pic x(5).
                05 ws-first-name-temp pic x(10).
                05 ws-last-name-temp pic x(20).
                05 ws-depart-temp pic x(11).
                05 ws-sus-temp pic 9.
                05 ws-late-temp pic 99.
                05 ws-overtiem-tmep pic 9.
             01 tempfile-status pic xx.

      * sum
      * variables for the second line in summary
      * Date: January 4, 2019
            01 ws-sum-date pic x(6) value 'Date: '.
            01 ws-month.
                05 NAME-OF-MONTH-01 PIC X(7) value 'January'.
                05 NAME-OF-MONTH-02 PIC X(8) value 'February'.
                05 NAME-OF-MONTH-03 PIC X(5) value 'March'.
                05 NAME-OF-MONTH-04 PIC X(5) value 'April'.
                05 NAME-OF-MONTH-05 PIC X(3) VALUE 'May'.
                05 NAME-OF-MONTH-06 PIC X(4) VALUE 'June'.
                05 NAME-OF-MONTH-07 PIC X(4) VALUE 'July'.
                05 NAME-OF-MONTH-08 PIC X(6) VALUE 'August'.
                05 NAME-OF-MONTH-09 PIC X(9) VALUE 'September'.
                05 NAME-OF-MONTH-10 PIC X(7) VALUE 'October'.
                05 NAME-OF-MONTH-11 PIC X(8) VALUE 'November'.
                05 NAME-OF-MONTH-12 PIC X(8) VALUE 'December'.
            01 ws-space pic x value ' '.
            01 ws-day1 pic 9.
            01 ws-day2 pic 99.
            01 ws-comma pic xx value  ', '.
            01 ws-year pic 9999.

            01 ws-date-line pic x(23).

      * useless variable
            01 x pic 999.

      * count people of late, absent, present, suspicious
            01 late-people pic 99999 value 0.
            01 abse-people pic 99999 value 0.
            01 pres-people pic 99999 value 0.
            01 susp-people pic 99999 value 0.

      * add CRLF at the end of lines
            01 cr pic x value X"0D".

            01 current-date pic xx.

       procedure division.
            main-para.
      * read employee date, and write them into tempfile
            open input emp.
            open output tempfile
            perform read-emp-para.
            close tempfile
            close emp.
            *> display 'employee------------------------------------------'

      * read attendance file, edit data in tempfile accordingly
      * late 4, sus 1 or 3, presen 2, absen 0
            open input att.
            open i-o tempfile
            read att into ws-date.
            move att-day to current-date.
            display 'current date is ' current-date
            perform read-att-para.
            close tempfile
            close att.
            *> display 'attendance----------------------------------------'

      * edit monthly attendance file
            open input mon
            open output moncob
            open input tempfile
            read mon into ws-mon-date
            move ws-mon-date to date1-line
            move cr to mon-cr1
            *> display 'mon-date1 ' mon-date1
            write mon-date1
            end-write
            perform edit-mon-para
            close tempfile
            close moncob
            close mon.


      * generate summarycob.txt
            open output summ
      * write the first line     Daily Attendance Summary
            move 'Daily Attendance Summary' to first-line.
            move cr to f-cr
            write fline
            end-write
      * write the second line        Date: January 4, 2019
            open input att
            perform write-date-sum
            close att
      * write the third line
            move
        'Staff-ID Name                            Department Status' to
            third
            move cr to t-cr
            write tline
            end-write
      * write the forth line, which is dash line
            move
       '--------------------------------------------------------------'
            to dash-line1
            move cr to d-cr
            write dash-line
            end-write
      * write status according to tempfile
            open input tempfile
            open input emp.
            perform write-sum-status
            close emp
            close tempfile

      * write another dash line, laji is another dash line
            move
       '--------------------------------------------------------------'
            to laji-line
            *> display 'success'
            move cr to laji-cr
            write laji
            end-write

      *write count
            move 'Number of Presences:' to p-line
            move pres-people to p-num
            move cr to pre-cr
            write presences end-write

            move 'Number of Absences:' to a-line
            move abse-people to a-num
            move cr to abs-cr
            write absences end-write

            move 'Number of Late Arrivals:' to l-line
            move late-people to l-num
            move cr to lat-cr
            write late end-write

            move 'Number of Suspicious Records:' to s-line
            move susp-people to s-num
            move cr to sus-cr
            write suspicous end-write

            close summ.

      * And then, the code is finished


       stop run.

            read-att-para.
            read att into ws-attendance
                if att-status not = 10
                    *> display ws-attendance

                    move ws-id-att to id-temp
                    read tempfile into ws-temp
                    key is id-temp
                    move ws-sus-temp to sus-temp
                    move ws-late-temp to late-temp
                    move ws-overtime to overtime-temp
                    add 1 to sus-temp

                    if ws-a-l = 'ARRIVE'
                        if ws-time-hour is not < 10
                            *> display 'arrive'
                            compute x=4 * (ws-time-hour - 10) +
                                   (ws-time-minute / 15)
                            add x to late-temp
                         end-if
                    end-if
                    if ws-a-l = 'LEAVE'
                        *> display 'leave'
                        compute x=ws-time-hour - 17
                        add x to overtime-temp
                    END-IF

                    rewrite temp-info

                    perform read-att-para
            end-if.

            read-emp-para.
            read emp into ws-employees
                if emp-status not = 10
                    move ws-id-emp to id-temp
                    move '     ' to kong1
                    move ws-first-name to first-name-temp
                    move ws-last-name to last-name-temp
                    move ws-depart to depart-temp
                    move 0 to sus-temp
                    move 0 to late-temp
                    move 0 to overtime-temp
                    *> display temp-info

                    write temp-info
                    END-WRITE
                    perform read-emp-para
            end-if.

            edit-mon-para.
            read mon into ws-mon-att
               if mon-status not = 10
      * reset on the first day every month
                   if current-date = 01
                       display 'success'
                       move ws-id-mon to id-mon1
                       move 000 to absent1
                       move 000 to 15-late1
                       move 000 to overtime1
                       move cr to mon-cr2
                       *> display 'mon-att1 is ' mon-att1
                   end-if

                   if current-date not = 01
                   string ws-mon-att cr into mon-att1
                   end-if

                   display 'mon-att1 is ' mon-att1
                   move ws-id-mon to id-temp
                   read tempfile into ws-temp
                   key is id-temp
                   if ws-sus-temp = 0
                       add 1 to absent1
                   END-IF
                   add ws-late-temp to 15-late1
                   add ws-overtiem-tmep to overtime1
      * claim at most 30 overtime
                   if overtime1 >= 30
                       move 30 to overtime1
                   end-if

                   *> display 'mon-att1 ' mon-att1
                   write mon-att1
                   end-write
                   perform edit-mon-para
            end-if.

            write-date-sum.
            read att into ws-date
            move att-year to ws-year
            if att-month = 01
                if att-day < 10 move att-day to ws-day1
                   string ws-sum-date NAME-OF-MONTH-01 ws-space ws-day1
                   ws-comma ws-year delimited by size
                   into ws-date-line end-if
                if att-day > 9 move att-day to ws-day2
                   string ws-sum-date NAME-OF-MONTH-01 ws-space ws-day2
                   ws-comma ws-year delimited by size
                   into ws-date-line end-if
                   end-if
            if att-month = 02
                if att-day < 10 move att-day to ws-day1
                   string ws-sum-date NAME-OF-MONTH-02 ws-space ws-day1
                   ws-comma ws-year delimited by size
                   into ws-date-line end-if
                if att-day > 9 move att-day to ws-day2
                   string ws-sum-date NAME-OF-MONTH-02 ws-space ws-day2
                   ws-comma ws-year delimited by size
                   into ws-date-line end-if
                   end-if
            if att-month = 03
                if att-day < 10 move att-day to ws-day1
                   string ws-sum-date NAME-OF-MONTH-03 ws-space ws-day1
                   ws-comma ws-year delimited by size
                   into ws-date-line end-if
                if att-day > 9 move att-day to ws-day2
                   string ws-sum-date NAME-OF-MONTH-03 ws-space ws-day2
                   ws-comma ws-year delimited by size
                   into ws-date-line end-if
                   end-if
            if att-month = 04
                if att-day < 10 move att-day to ws-day1
                   string ws-sum-date NAME-OF-MONTH-04 ws-space ws-day1
                   ws-comma ws-year delimited by size
                   into ws-date-line end-if
                if att-day > 9 move att-day to ws-day2
                   string ws-sum-date NAME-OF-MONTH-04 ws-space ws-day2
                   ws-comma ws-year delimited by size
                   into ws-date-line end-if
                   end-if
           if att-month = 05
                if att-day < 10 move att-day to ws-day1
                   string ws-sum-date NAME-OF-MONTH-05 ws-space ws-day1
                   ws-comma ws-year delimited by size
                   into ws-date-line end-if
                if att-day > 9 move att-day to ws-day2
                   string ws-sum-date NAME-OF-MONTH-05 ws-space ws-day2
                   ws-comma ws-year delimited by size
                   into ws-date-line end-if
                   end-if
            if att-month = 06
                if att-day < 10 move att-day to ws-day1
                   string ws-sum-date NAME-OF-MONTH-06 ws-space ws-day1
                   ws-comma ws-year delimited by size
                   into ws-date-line end-if
                if att-day > 9 move att-day to ws-day2
                   string ws-sum-date NAME-OF-MONTH-06 ws-space ws-day2
                   ws-comma ws-year delimited by size
                   into ws-date-line end-if
                   end-if
            if att-month = 07
                if att-day < 10 move att-day to ws-day1
                   string ws-sum-date NAME-OF-MONTH-07 ws-space ws-day1
                   ws-comma ws-year delimited by size
                   into ws-date-line end-if
                if att-day > 9 move att-day to ws-day2
                   string ws-sum-date NAME-OF-MONTH-07 ws-space ws-day2
                   ws-comma ws-year delimited by size
                   into ws-date-line end-if
                   end-if
            if att-month = 08
                if att-day < 10 move att-day to ws-day1
                   string ws-sum-date NAME-OF-MONTH-08 ws-space ws-day1
                   ws-comma ws-year delimited by size
                   into ws-date-line end-if
                if att-day > 9 move att-day to ws-day2
                   string ws-sum-date NAME-OF-MONTH-08 ws-space ws-day2
                   ws-comma ws-year delimited by size
                   into ws-date-line end-if
                   end-if
            if att-month = 09
                if att-day < 10 move att-day to ws-day1
                   string ws-sum-date NAME-OF-MONTH-09 ws-space ws-day1
                   ws-comma ws-year delimited by size
                   into ws-date-line end-if
                if att-day > 9 move att-day to ws-day2
                   string ws-sum-date NAME-OF-MONTH-09 ws-space ws-day2
                   ws-comma ws-year delimited by size
                   into ws-date-line end-if
                   end-if
            if att-month = 10
                if att-day < 10 move att-day to ws-day1
                   string ws-sum-date NAME-OF-MONTH-10 ws-space ws-day1
                   ws-comma ws-year delimited by size
                   into ws-date-line end-if
                if att-day > 9 move att-day to ws-day2
                   string ws-sum-date NAME-OF-MONTH-10 ws-space ws-day2
                   ws-comma ws-year delimited by size
                   into ws-date-line end-if
                   end-if
            if att-month = 11
                if att-day < 10 move att-day to ws-day1
                   string ws-sum-date NAME-OF-MONTH-11 ws-space ws-day1
                   ws-comma ws-year delimited by size
                   into ws-date-line end-if
                if att-day > 9 move att-day to ws-day2
                   string ws-sum-date NAME-OF-MONTH-11 ws-space ws-day2
                   ws-comma ws-year delimited by size
                   into ws-date-line end-if
                   end-if
            if att-month = 12
                if att-day < 10 move att-day to ws-day1
                   string ws-sum-date NAME-OF-MONTH-12 ws-space ws-day1
                   ws-comma ws-year delimited by size
                   into ws-date-line end-if
                if att-day > 9 move att-day to ws-day2
                   string ws-sum-date NAME-OF-MONTH-12 ws-space ws-day2
                   ws-comma ws-year delimited by size
                   into ws-date-line end-if
                   end-if

            move ws-date-line to second-line
            display 'sline is ' second-line
            move cr to s-cr
            write sline
            end-write.

            write-sum-status.
            read emp into ws-employees
            if emp-status not = 10
                *> display 'ws-id-emp is ' ws-id-emp
                move ws-id-emp to id-temp
                read tempfile into ws-temp
                key is id-temp
                       *> display 'ws-temp is 'ws-temp
                       move ws-id-temp to id-sum
                       move '     ' to fspace
                       move ws-first-name-temp to first-name-sum
                       move ws-last-name-temp to last-name-sum
                       move ws-depart-temp to depart-sum
                       move cr to indo-cr
                       *> display 'depart is ' ws-depart-temp
                       if ws-sus-temp = 0
                           add 1 to abse-people
                           *> display 'abse-people ' abse-people
                           move 'ABSENCE' to status-sum end-if
                       if ws-sus-temp = 1
                           add 1 to susp-people
                           *> display 'susp-people' susp-people
                           move 'SUSPICIOUS' to status-sum end-if
                       if ws-sus-temp = 2
                           if ws-late-temp > 0
                               add 1 to late-people
                               display 'late-people' late-people
                               move 'LATE' to status-sum end-if
                           if ws-late-temp = 0
                               add 1 to pres-people
                               *> display 'pres-people ' pres-people
                               move 'PRESENCE' to status-sum end-if
                       end-if
                       display 'sum-info ' sum-info
                       write sum-info
                       end-write
                       perform write-sum-status
            end-if.
