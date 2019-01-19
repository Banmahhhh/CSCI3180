          program banma
          implicit none

          ! employee variables
          integer :: emp_ios
          integer :: emp_id !4
          character(len=10) :: emp_first_name
          character(len=20) :: emp_last_name
          character(len=21) :: emp_notimportant
          character(len=3) :: emp_depart
          integer :: emp_salary !6

          ! attendance variables
          integer :: att_ios
          integer :: att_id ! 4
          character(len=6) :: att_a_l
          character(len=11) :: att_notused
          integer :: att_hour ! 2
          character(len=1) :: att_colon
          integer :: att_min ! 2

          ! monthly-attendance variables
          integer :: mon_ios
          character(len=7) :: mon_date
          integer :: mon_id ! 4
          integer :: mon_absence ! 3
          integer :: mon_15_late ! 3
          integer :: mon_overtime !3

          ! summaryfor variables
          integer :: sum_ios
          integer :: presences ! 4
          integer :: absences ! 4
          integer :: late ! 4
          integer :: suspicious ! 4
          

          ! wasting memory array
          ! absence: 0, 0, 0
          ! suspicious: 1, *, *
          ! presence: 2, 0, 0
          ! late: 2, >0, *
          ! overtime: 2, *, >0
          integer, dimension(9999, 5) :: record

          ! current date
          integer :: cur_year ! 4
          integer :: cur_month ! 2
          integer :: cur_date ! 2
          character(len=1) :: cur_dash ! - in the first line of attendance

          ! count people of each status
*23456789*************************************************************       
* read attendance.txt, record status of suspicious/late/overtime

          presences=0
          absences=0
          late=0
          suspicious=0

          open(1, file = 'attendance.txt', status='old')
          read(1, "(I4,A1,I2,A1,I2)") cur_year, cur_dash, 
     &    cur_month, cur_dash, cur_date
!          write(*, *) cur_year, cur_month, cur_date 

2         continue 
          read(1, "(I4,A6,A11,I2,A1,I2)", IOSTAT=att_ios) att_id,
     &    att_a_l, att_notused, att_hour, att_colon, att_min
          if (att_ios < 0) then
               print *, 'end of att'
          end if
          if(att_ios == 0) then
               write(*,*) "att not end", att_id
               if(att_a_l == 'ARRIVE') then
               ! only deal with the first arrive or leave record
               if(record(att_id, 4) == 0) then
                    write(*,*) "arrive"
                    record(att_id, 4) = 1 ! read an arrive record
                    record(att_id, 1) = record(att_id, 1)+1
                    if(att_hour > 9) then
                         record(att_id, 2)=4*(att_hour-10)+att_min/15
                    write(*,*) record(att_id, 1),record(att_id, 2)
                    end if
               end if
               end if
               if(att_a_l == 'LEAVE') then
               if(record(att_id, 5) == 0) then
                    write(*,*) "leave"
                    record(att_id, 5) = 1 ! read a leave record
                    record(att_id, 1) = record(att_id, 1)+1
                    if(att_hour > 16) then
                         record(att_id, 3)=att_hour-17
                    write(*,*) record(att_id, 1),record(att_id, 3)
                    end if
               end if
               end if
               goto 2
          end if     
          close(1)
          write(*,*) "------------------------------------"

          ! read monthly-attendance.txt, update monthly-attendacefor
          open(2, file='monthly-attendance.txt', status='old')         
          open(3, file='monthly-attendancefor.txt')  

          read(2, "(A7)", IOSTAT=mon_ios) mon_date
          write(3, "(A7)") mon_date

3         continue
          read(2, "(I4,I3,I3,I3)", IOSTAT=mon_ios)
     &         mon_id, mon_absence, mon_15_late, mon_overtime
          if(mon_ios < 0) then
               write(*,*) "mon end"
          end if
          if(mon_ios == 0) then
          write(*,*) mon_id, mon_absence, mon_15_late, mon_overtime
          ! update on the first of a month
               if(cur_date == 1) then
               mon_absence = 0
               mon_15_late = 0
               mon_overtime = 0
               end if

               if(record(mon_id, 1) == 0) then
                    mon_absence = mon_absence+1
               end if
               mon_15_late=record(mon_id, 2)+mon_15_late
               mon_overtime=mon_overtime+record(mon_id, 3)
               if(mon_overtime > 30) then
                    mon_overtime = 30
               end if
               write(3,"(I4,I0.3,I0.3,I0.3)") mon_id, mon_absence, 
     &         mon_15_late, mon_overtime
               goto 3 
          end if   
          close(2)
          close(3)
          write(*,*) "-----------------------------------------"

          ! read employees.txt, generate summaryfor.txt
          open(4, file='employees.txt')
          open(7, file='summaryfor.txt')
          write(7, "(A24)") "Daily Attendance Summary"
           ! write the laji date
          if(cur_month == 1) then
               if(cur_date < 10) then
               write(7, "(A14,I1,A2,I4)") "Date: January ", cur_date,
     &         ", ", cur_year 
               end if
               if(cur_date > 9) then
               write(7, "(A14,I2,A2,I4)") "Date: January ", cur_date,
     &         ", ", cur_year 
               end if
          end if
          if(cur_month == 2) then
               if(cur_date < 10) then
               write(7, "(A15,I1,A2,I4)") "Date: February ", cur_date,
     &         ", ", cur_year 
               end if
               if(cur_date > 9) then
               write(7, "(A15,I2,A2,I4)") "Date: February ", cur_date,
     &         ", ", cur_year 
               end if
          end if
          if(cur_month == 3) then
               if(cur_date < 10) then
               write(7, "(A12,I1,A2,I4)") "Date: March ", cur_date,
     &         ", ", cur_year 
               end if
               if(cur_date > 9) then
               write(7, "(A14,I2,A2,I4)") "Date: March ", cur_date,
     &         ", ", cur_year 
               end if
          end if
          if(cur_month == 4) then
               if(cur_date < 10) then
               write(7, "(A12,I1,A2,I4)") "Date: April ", cur_date,
     &         ", ", cur_year 
               end if
               if(cur_date > 9) then
               write(7, "(A12,I2,A2,I4)") "Date: April ", cur_date,
     &         ", ", cur_year 
               end if
          end if
          if(cur_month == 5) then
               if(cur_date < 10) then
               write(7, "(A10,I1,A2,I4)") "Date: May ", cur_date,
     &         ", ", cur_year 
               end if
               if(cur_date > 9) then
               write(7, "(A10,I2,A2,I4)") "Date: May ", cur_date,
     &         ", ", cur_year 
               end if
          end if
          if(cur_month == 6) then
               if(cur_date < 10) then
               write(7, "(A11,I1,A2,I4)") "Date: June ", cur_date,
     &         ", ", cur_year 
               end if
               if(cur_date > 9) then
               write(7, "(A11,I2,A2,I4)") "Date: June ", cur_date,
     &         ", ", cur_year 
               end if
          end if
          if(cur_month == 7) then
               if(cur_date < 10) then
               write(7, "(A11,I1,A2,I4)") "Date: July ", cur_date,
     &         ", ", cur_year 
               end if
               if(cur_date > 9) then
               write(7, "(A11,I2,A2,I4)") "Date: July ", cur_date,
     &         ", ", cur_year 
               end if
          end if
          if(cur_month == 8) then
               if(cur_date < 10) then
               write(7, "(A13,I1,A2,I4)") "Date: August ", cur_date,
     &         ", ", cur_year 
               end if
               if(cur_date > 9) then
               write(7, "(A13,I2,A2,I4)") "Date: May ", cur_date,
     &         ", ", cur_year 
               end if
          end if
          if(cur_month == 9) then
               if(cur_date < 10) then
               write(7, "(A16,I1,A2,I4)") "Date: September ", cur_date,
     &         ", ", cur_year 
               end if
               if(cur_date > 9) then
               write(7, "(A16,I2,A2,I4)") "Date: September ", cur_date,
     &         ", ", cur_year 
               end if
          end if
          if(cur_month == 10) then
               if(cur_date < 10) then
               write(7, "(A14,I1,A2,I4)") "Date: October ", cur_date,
     &         ", ", cur_year 
               end if
               if(cur_date > 9) then
               write(7, "(A14,I2,A2,I4)") "Date: October ", cur_date,
     &         ", ", cur_year 
               end if
          end if
          if(cur_month ==11) then
               if(cur_date < 10) then
               write(7, "(A15,I1,A2,I4)") "Date: November ", cur_date,
     &         ", ", cur_year 
               end if
               if(cur_date > 9) then
               write(7, "(A15,I2,A2,I4)") "Date: November ", cur_date,
     &         ", ", cur_year 
               end if
          end if
          if(cur_month == 12) then
               if(cur_date < 10) then
               write(7, "(A15,I1,A2,I4)") "Date: December ", cur_date,
     &         ", ", cur_year 
               end if
               if(cur_date > 9) then
               write(7, "(A15,I2,A2,I4)") "Date: December ", cur_date,
     &         ", ", cur_year 
               end if
          end if
          write(7,"(A58)") 
     &    "Staff-ID Name                            Department Status"
          write(7,"(A62)") 
     &"--------------------------------------------------------------"
    
          ! write the attendance status
4         continue
          read(4,"(I4,A10,A20,A21,A3,I6)", IOSTAT = emp_ios) emp_id, 
     &    emp_first_name, emp_last_name, emp_notimportant, emp_depart, 
     &    emp_salary 
          write(*,*) "emp_id is ", emp_id
          if(emp_ios < 0) then
               write(*,*) "emp end"
          end if       
          if(emp_ios == 0) then
               write(*,"(A3)") emp_depart
               if(record(emp_id,1) == 0) then 
                    write(7,"(I4,A5,A11,A21,A3,A8,A7)") 
     &              emp_id, "     ", emp_first_name, emp_last_name, 
     &              emp_depart, "        ", "ABSENCE"
                    absences = absences+1
               end if
               if(record(emp_id,1) == 1) then 
                    write(7,"(I4,A5,A11,A21,A3,A8,A10)") 
     &              emp_id, "     ", emp_first_name, emp_last_name, 
     &              emp_depart, "        ", "SUSPICIOUS"
                    suspicious = suspicious+1
               end if
               if(record(emp_id,1) == 2) then 
                    if(record(emp_id, 2) == 0) then
                    write(7,"(I4,A5,A11,A21,A3,A8,A8)") 
     &              emp_id, "     ", emp_first_name, emp_last_name, 
     &              emp_depart, "        ", "PRESENCE"
                    presences = presences+1
                    end if
                    if(record(emp_id, 2) /= 0) then
                    write(7,"(I4,A5,A11,A21,A3,A8,A4)") emp_id,
     &               "     ", emp_first_name, emp_last_name, emp_depart,
     &               "        ", "LATE"
                    late = late+1
                    end if
               end if          
               goto 4
          end if
          write(7,"(A62)") 
     &"--------------------------------------------------------------"
          write(7, "(A21,I4)") "Number of Presences: ", presences
          write(7, "(A20,I4)") "Number of Absences: ", absences
          write(7, "(A25,I4)") "Number of Late Arrivals: ", late
          write(7, "(A30,I4)") "Number of Suspicious Records: ", 
     &     suspicious

          close(4)
          close(7)

          end program banma