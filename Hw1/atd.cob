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
              organization is indexed
              access mode is random
              record key is id-mon1.

              select tempfile assign to 'test.txt'
              organization is indexed
              access mode is random
              record key is id-temp
              file status is tempfile-status.

              select summ
              assign to 'summarycob.txt'
              organization is SEQUENTIAL.
              *> access mode is RANDOM
              *> record key is id-sum.


       data division.
            file section.

            fd mon.
            01 mon-date pic 9999x99.
            01 mon-att.
                05 id-mon pic 9999.
                05 absent pic 999.
                05 15-late pic 999.
                05 overtime pic 999.

            fd moncob.
            01 mon-date1 pic 9999x99.
            01 mon-att1.
                05 id-mon1 pic 9999.
                05 absent1 pic 999.
                05 15-late1 pic 999.
                05 overtime1 pic 999.

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
            01 fline pic x(25).
            01 sline pic x(23).
            01 tline pic x(58).
            01 dash-line pic x(62).

            01 sum-info.
                05 id-sum pic 9999.
                05 fspace pic x(5).
                05 first-name-sum pic x(11).
                05 last-name-sum pic x(21).
                05 depart-sum pic x(11).
                05 status-sum pic x(10).
            01 laji pic x(62).
            01 presences.
                05 p-line pic x(20).
                05 p-num pic zzzz9.
            01 absences.
                05 a-line pic x(19).
                05 a-num pic zzzz9.
            01 late.
                05 l-line pic x(24).
                05 l-num pic zzzz9.
            01 suspicous.
                05 s-line pic x(29).
                05 s-num pic zzzz9.

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
               05 ws-first-name pic a(10).
               05 ws-last-name pic a(20).
               05 ws-gender pic a.
               05 ws-birth pic x(10).
               05 ws-hire-date pic x(10).
               05 ws-depart pic a(3).
            01 emp-status pic xx.

      * mon
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
            01 ws-space pic x.
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

       procedure division.
            main-para.
      * read employee date, and write them into tempfile
            open input emp.
            open output tempfile
            perform read-emp-para.
            close tempfile
            close emp.
            display 'employee------------------------------------------'

      * read attendance file, edit data in tempfile accordingly
      * late 4, sus 1 or 3, presen 2, absen 0
            open input att.
            open i-o tempfile
            read att into ws-date.
            perform read-att-para.
            close tempfile
            close att.
            display 'attendance----------------------------------------'

      * edit monthly attendance file
            open input mon
            open output moncob
            open input tempfile
            read mon into ws-mon-date
            display ws-mon-date
            display '*********************************'
            display 'mon-date is 'ws-mon-date
            display '*********************************'
            move ws-mon-date to mon-date1
            write mon-date1
            end-write
            perform edit-mon-para
            close tempfile
            close moncob
            close mon.

            display 'mon-----------------------------------------------'
      * generate summarycob.txt
            open output summ
      * write the first line     Daily Attendance Summary
            move 'Daily Attendance Summary' to fline.
            write fline
            end-write
            display 'finish the first line'
      * write the second line        Date: January 4, 2019
            open input att
            perform write-date-sum
            close att
            display 'finish the second line'
      * write the third line
            move
        'Staff-ID Name                            Department Status' to
            tline
            write tline
            end-write
            display 'finish the third line'
      * write the forth line, which is dash line
            move
       '--------------------------------------------------------------'
            to dash-line
            write dash-line
            end-write
            display 'finish the forth line'
      * write status according to tempfile
            open input tempfile
            open input emp.
            perform write-sum-status
            close emp
            close tempfile

      * write another dash line, laji is another dash line
            move
       '--------------------------------------------------------------'
            to laji
            display 'success'
            write laji
            end-write
            display 'finish another dash line'

      *write count
            move 'Number of Presences:' to p-line
            move pres-people to p-num
            write presences end-write

            move 'Number of Absences:' to a-line
            move abse-people to a-num
            write absences end-write

            move 'Number of Late Arrivals:' to l-line
            move late-people to l-num
            write late end-write

            move 'Number of Suspicious Records:' to s-line
            move susp-people to s-num
            write suspicous end-write

            close summ.

      * And then, the code is finished

       stop run.

            read-att-para.
            read att into ws-attendance
                if att-status = 10 display 'att-end--------------------'
                    end-if
                if att-status not = 10
                    display ws-attendance

                    move ws-id-att to id-temp
                    read tempfile into ws-temp
                    key is id-temp
                    move ws-sus-temp to sus-temp
                    move ws-late-temp to late-temp
                    move ws-overtime to overtime-temp
                    add 1 to sus-temp

                    if ws-a-l = 'ARRIVE'
                        if ws-time-hour is not < 10
                            display 'arrive'
                            compute x=4 * (ws-time-hour - 10) +
                                   (ws-time-minute / 15)
                            add x to late-temp
                         end-if
                    end-if
                    if ws-a-l = 'LEAVE'
                        display 'leave'
                        compute x=ws-time-hour - 17
                        add x to overtime-temp
                    END-IF

                    rewrite temp-info
                    display 'in read-att, temp-info is 'temp-info

                    perform read-att-para
            end-if.

            read-emp-para.
            read emp into ws-employees
                *> at end display 'emp-end-----------------------'
                if emp-status = 10 display 'emp-end--------------------'
                    end-if
                if emp-status not = 10
                    display ws-id-emp

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
                    display temp-info
                    perform read-emp-para
            end-if.

            edit-mon-para.
            read mon into ws-mon-att
               *> at end display 'mon-end'
               if mon-status = 10 display 'mon-end'
                   end-if
               if mon-status not = 10
                   display ws-mon-att

                   move ws-mon-att to mon-att1
                   move ws-id-mon to id-temp

                   read tempfile into ws-temp
                   key is id-temp
                   display 'ws-temp is ' ws-temp
                   if ws-sus-temp = 0
                       add 1 to absent1
                   END-IF
                   add ws-late-temp to 15-late1
                   add ws-overtiem-tmep to overtime1
                   display 'mon-att1 is ' mon-att1
                   write mon-att1
                   end-write
                   perform edit-mon-para
            end-if.

            write-date-sum.
            read att into ws-date
            display 'ws-date is ' ws-date
            move att-year to ws-year
            display 'ws-year is ' ws-year
            display 'att-month is ' att-month
            if att-month = 01
                if att-day < 10 move att-day to ws-day1 end-if
                if att-day > 9 move att-day to ws-day2 end-if
            string ws-sum-date NAME-OF-MONTH-01
            ws-space ws-day1 ws-comma ws-year delimited by size
            into ws-date-line end-if
            if att-month = 2
                if att-day < 10 move att-day to ws-day1 end-if
                if att-day > 9 move att-day to ws-day2 end-if
            string ws-sum-date NAME-OF-MONTH-02
            ws-space ws-day1 ws-comma ws-year delimited by size
            into ws-date-line end-if
            if att-month = 3
                if att-day < 10 move att-day to ws-day1 end-if
                if att-day > 9 move att-day to ws-day2 end-if
            string ws-sum-date NAME-OF-MONTH-03
            ws-space ws-day1 ws-comma ws-year delimited by size
            into ws-date-line end-if
            if att-month = 4
                if att-day < 10 move att-day to ws-day1 end-if
                if att-day > 9 move att-day to ws-day2 end-if
            string ws-sum-date NAME-OF-MONTH-04
            ws-space ws-day1 ws-comma ws-year delimited by size
            into ws-date-line end-if
            if att-month = 5
                if att-day < 10 move att-day to ws-day1 end-if
                if att-day > 9 move att-day to ws-day2 end-if
            string ws-sum-date NAME-OF-MONTH-05
            ws-space ws-day1 ws-comma ws-year delimited by size
            into ws-date-line end-if
            if att-month = 6
                if att-day < 10 move att-day to ws-day1 end-if
                if att-day > 9 move att-day to ws-day2 end-if
            string ws-sum-date NAME-OF-MONTH-06
            ws-space ws-day1 ws-comma ws-year delimited by size
            into ws-date-line end-if
            if att-month = 7
                if att-day < 10 move att-day to ws-day1 end-if
                if att-day > 9 move att-day to ws-day2 end-if
            string ws-sum-date NAME-OF-MONTH-07
            ws-space ws-day1 ws-comma ws-year delimited by size
            into ws-date-line end-if
            if att-month = 8
                if att-day < 10 move att-day to ws-day1 end-if
                if att-day > 9 move att-day to ws-day2 end-if
            string ws-sum-date NAME-OF-MONTH-08
            ws-space ws-day1 ws-comma ws-year delimited by size
            into ws-date-line end-if
            if att-month = 9
                if att-day < 10 move att-day to ws-day1 end-if
                if att-day > 9 move att-day to ws-day2 end-if
            string ws-sum-date NAME-OF-MONTH-09
            ws-space ws-day1 ws-comma ws-year delimited by size
            into ws-date-line end-if
            if att-month = 10
                if att-day < 10 move att-day to ws-day1 end-if
                if att-day > 9 move att-day to ws-day2 end-if
            string ws-sum-date NAME-OF-MONTH-10
            ws-space ws-day1 ws-comma ws-year delimited by size
            into ws-date-line end-if
            if att-month = 11
                if att-day < 10 move att-day to ws-day1 end-if
                if att-day > 9 move att-day to ws-day2 end-if
            string ws-sum-date NAME-OF-MONTH-11
            ws-space ws-day1 ws-comma ws-year delimited by size
            into ws-date-line end-if
            if att-month = 12
                if att-day < 10 move att-day to ws-day1 end-if
                if att-day > 9 move att-day to ws-day2 end-if
            string ws-sum-date NAME-OF-MONTH-12
            ws-space ws-day1 ws-comma ws-year delimited by size
            into ws-date-line end-if

            display 'ws-date-line is ' ws-date-line
            move ws-date-line to sline
            display 'sline is ' sline
            write sline
            end-write.

            write-sum-status.
            read emp into ws-employees
            if emp-status = 10 display 'finish writing into sum status'
                end-if
            if emp-status not = 10
                display 'ws-id-emp is ' ws-id-emp
                move ws-id-emp to id-temp
                read tempfile into ws-temp
                key is id-temp
                       display 'ws-temp is 'ws-temp
                       move ws-id-temp to id-sum
                       move '     ' to fspace
                       move ws-first-name-temp to first-name-sum
                       move ws-last-name-temp to last-name-sum
                       move ws-depart-temp to depart-sum
                       if ws-sus-temp = 0
                           add 1 to abse-people
                           display 'abse-people ' abse-people
                           move 'ABSENCE' to status-sum end-if
                       if ws-sus-temp = 1
                           add 1 to susp-people
                           display 'susp-people' susp-people
                           move 'SUSPICIOUS' to status-sum end-if
                       if ws-sus-temp = 2
                           if ws-late-temp > 0
                               add 1 to late-people
                               display 'late-people' late-people
                               move 'LATE' to status-sum end-if
                           if ws-late-temp = 0
                               add 1 to pres-people
                               display 'pres-people ' pres-people
                               move 'PRESENCE' to status-sum end-if
                       end-if
                       display 'sum-info ' sum-info
                       write sum-info
                       end-write
                       perform write-sum-status
            end-if.
