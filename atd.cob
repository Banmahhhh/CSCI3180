       identification division.
       program-id. laji.

       environment division.
            input-output section.
            file-control.
              select att
              assign to '/Users/banma/attendance.txt'
              organization is line sequential.
              select emp
              assign to '/Users/banma/employees.txt'
              organization is line sequential.
              select mon
              assign to '/Users/banma/monthly-attendance.txt'
              organization is line sequential.

              select moncob
              assign to 'monthly-attendancecob.txt'
              organization is indexed
              access mode is random
              record key is id-mon1.

              select tempfile assign to 'test.txt'
              organization is indexed
              access mode is random
              record key is id-temp.

              select summ
              assign to 'summarycob.txt'
              organization is INDEXED
              access mode is RANDOM
              record key is id-summ.


       data division.
            file section.
            fd att.
            01 att-date pic 9999x99x99.
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
                05 first-name pic a(10).
                05 last-name pic a(20).
                05 gender pic a.
                05 birth pic x(10).
                05 hire-date pic x(10).
                05 depart pic a(3).
                05 salary pic 9(6).

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
                05 late-temp pic 9.
                05 overtime-temp pic 9.

            fd summ.
            01 summ-info.
                05 id-summ pic 9999.

            working-storage section.
            01 ws-date pic 9999x99x99.
            01 ws-attendance.
               05 ws-id-att pic 9(4).
               05 ws-a-l pic a(6).
               05 ws-the-date pic x(11).
               05 ws-time-hour pic 9(2).
               05 ws-not-used pic x(1).
               05 ws-time-minute pic 9(2).

            01 ws-employees.
               05 ws-id-emp pic 9999.
               05 ws-first-name pic a(10).
               05 ws-last-name pic a(20).
               05 ws-gender pic a.
               05 ws-birth pic x(10).
               05 ws-hire-date pic x(10).
               05 ws-depart pic a(3).

            01 ws-mon-date pic 9999x99.
            01 ws-mon-att.
                05 ws-id-mon pic 9999.
                05 ws-absent pic 999.
                05 ws-15-late pic 999.
                05 ws-overtime pic 999.

            01 ws-temp.
                05 ws-id-temp pic 9999.
                05 ws-kong1 pic x(5).
                05 ws-first-name-temp pic x(10).
                05 ws-last-name-temp pic x(20).
                05 ws-depart-temp pic x(11).
                05 ws-sus-temp pic 9.
                05 ws-late-temp pic 9.
                05 ws-overtiem-tmep pic 9.


            01 x pic 9.


       procedure division.
            main-para.
      * read employee date, and write them into tempfile
            open input emp.
            open output tempfile
            perform read-emp-para.
            close tempfile
            close emp.

      * read attendance file, edit data in tempfile accordingly
      * late 4, sus 1 or 3, presen 2, absen 0
            open input att.
            open i-o tempfile
            read att into ws-date.
            perform read-att-para.
            close tempfile
            close att.

      * edit monthly attendance file
            open input mon
            open output moncob
            open input tempfile
            read mon into ws-mon-date
            display ws-mon-date
            move mon-date to mon-date1
            write mon-date1
            end-write
            perform edit-mon-para
            close tempfile
            close moncob
            close mon.


            stop run.

            read-att-para.
            read att into ws-attendance
                at end display 'att-end'
                not at end
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
                    if ws-a-l = 'LEAVE '
                        display 'leave'
                        compute x=ws-time-hour - 17
                        add x to overtime-temp
                    END-IF

                    rewrite temp-info
                    display temp-info

                    perform read-att-para
            end-read.

            read-emp-para.
            read emp into ws-employees
                at end display 'emp-end'
                not at end
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
            end-read.

            edit-mon-para.
            read mon into ws-mon-att
               at end display 'mon-end'
               not at end
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
            end-read.
