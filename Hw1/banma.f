          program banma
          implicit none

          ! employee variables
          integer :: e_ios ! emp_ios
          integer :: e_id !4 e_id
          character(len=10) :: e_f_n ! e_f_n
          character(len=20) :: e_l_n
          character(len=21) :: e_n_u !emp_not_uesd
          character(len=3) :: e_dep !emp_department
          integer :: e_sal !6

          ! attendance variables
          integer :: a_ios
          integer :: a_id ! 4
          character(len=6) :: a_a_l ! attendance_arrive_leave
          character(len=11) :: a_n_u !att_not_used
          integer :: a_h ! 2 att_hour
          character(len=1) :: a_cl !att_colon
          integer :: a_m ! 2 att_minute

          ! monthly-attendance variables
          integer :: m_ios
          character(len=7) :: m_d ! mon_date
          integer :: m_id ! 4 mon_id
          integer :: m_ab ! 3 mon_absence
          integer :: m_15_l ! 3 mon_15_late
          integer :: m_ot !3 mon_overtime

          ! summaryfor variables
          integer :: s_ios !sum_ios
          integer :: pr ! 4 presence
          integer :: ab ! 4 absence
          integer :: late ! 4
          integer :: su ! 4 suspicious
          

          ! wasting memory array
          ! absence: 0, 0, 0
          ! su: 1, *, *
          ! presence: 2, 0, 0
          ! late: 2, >0, *
          ! overtime: 2, *, >0
          integer, dimension(9999, 5) :: record

          ! current date
          integer :: c_y ! 4 current_year
          integer :: c_m! 2 current_month
          integer :: c_d ! 2 current_date
          character(len=1) :: c_dash ! - in the first line of attendance

          ! count people of each status
*23456789*************************************************************       
* read attendance.txt, record status of su/late/overtime

          pr=0
          ab=0
          late=0
          su=0

          open(1, file = 'attendance.txt', status='old')
          read(1, "(I4,A1,I2,A1,I2)") c_y, c_dash, 
     &    c_m, c_dash, c_d
!          write(*, *) c_y, c_m, c_d 

2         continue 
          read(1, "(I4,A6,A11,I2,A1,I2)", IOSTAT=a_ios) a_id,
     &    a_a_l, a_n_u, a_h, a_cl, a_m
          if (a_ios < 0) then
               print *, 'end of att'
          end if
          if(a_ios == 0) then
               write(*,*) "att not end", a_id
               if(a_a_l == 'ARRIVE') then
               ! only deal with the first arrive or leave record
               if(record(a_id, 4) == 0) then
                    write(*,*) "arrive"
                    record(a_id, 4) = 1 ! read an arrive record
                    record(a_id, 1) = record(a_id, 1)+1
                    if(a_h > 9) then
                         record(a_id, 2)=4*(a_h-10)+a_m/15
                    write(*,*) record(a_id, 1),record(a_id, 2)
                    end if
               end if
               end if
               if(a_a_l == 'LEAVE') then
               if(record(a_id, 5) == 0) then
                    write(*,*) "leave"
                    record(a_id, 5) = 1 ! read a leave record
                    record(a_id, 1) = record(a_id, 1)+1
                    if(a_h > 16) then
                         record(a_id, 3)=a_h-17
                    write(*,*) record(a_id, 1),record(a_id, 3)
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

          read(2, "(A7)", IOSTAT=m_ios) m_d
          write(3, "(A7)") m_d

3         continue
          read(2, "(I4,I3,I3,I3)", IOSTAT=m_ios)
     &         m_id, m_ab, m_15_l, m_ot
          if(m_ios < 0) then
               write(*,*) "mon end"
          end if
          if(m_ios == 0) then
          write(*,*) m_id, m_ab, m_15_l, m_ot
          ! update on the first of a month
               if(c_d == 1) then
               m_ab = 0
               m_15_l = 0
               m_ot = 0
               end if

               if(record(m_id, 1) == 0) then
                    m_ab = m_ab+1
               end if
               m_15_l=record(m_id, 2)+m_15_l
               m_ot=m_ot+record(m_id, 3)
               if(m_ot > 30) then
                    m_ot = 30
               end if
               write(3,"(I4,I0.3,I0.3,I0.3)") m_id, m_ab, 
     &         m_15_l, m_ot
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
          if(c_m == 1) then
               if(c_d < 10) then
               write(7, "(A14,I1,A2,I4)") "Date: January ", c_d,
     &         ", ", c_y 
               end if
               if(c_d > 9) then
               write(7, "(A14,I2,A2,I4)") "Date: January ", c_d,
     &         ", ", c_y 
               end if
          end if
          if(c_m == 2) then
               if(c_d < 10) then
               write(7, "(A15,I1,A2,I4)") "Date: February ", c_d,
     &         ", ", c_y 
               end if
               if(c_d > 9) then
               write(7, "(A15,I2,A2,I4)") "Date: February ", c_d,
     &         ", ", c_y 
               end if
          end if
          if(c_m == 3) then
               if(c_d < 10) then
               write(7, "(A12,I1,A2,I4)") "Date: March ", c_d,
     &         ", ", c_y 
               end if
               if(c_d > 9) then
               write(7, "(A14,I2,A2,I4)") "Date: March ", c_d,
     &         ", ", c_y 
               end if
          end if
          if(c_m == 4) then
               if(c_d < 10) then
               write(7, "(A12,I1,A2,I4)") "Date: April ", c_d,
     &         ", ", c_y 
               end if
               if(c_d > 9) then
               write(7, "(A12,I2,A2,I4)") "Date: April ", c_d,
     &         ", ", c_y 
               end if
          end if
          if(c_m == 5) then
               if(c_d < 10) then
               write(7, "(A10,I1,A2,I4)") "Date: May ", c_d,
     &         ", ", c_y 
               end if
               if(c_d > 9) then
               write(7, "(A10,I2,A2,I4)") "Date: May ", c_d,
     &         ", ", c_y 
               end if
          end if
          if(c_m == 6) then
               if(c_d < 10) then
               write(7, "(A11,I1,A2,I4)") "Date: June ", c_d,
     &         ", ", c_y 
               end if
               if(c_d > 9) then
               write(7, "(A11,I2,A2,I4)") "Date: June ", c_d,
     &         ", ", c_y 
               end if
          end if
          if(c_m == 7) then
               if(c_d < 10) then
               write(7, "(A11,I1,A2,I4)") "Date: July ", c_d,
     &         ", ", c_y 
               end if
               if(c_d > 9) then
               write(7, "(A11,I2,A2,I4)") "Date: July ", c_d,
     &         ", ", c_y 
               end if
          end if
          if(c_m == 8) then
               if(c_d < 10) then
               write(7, "(A13,I1,A2,I4)") "Date: August ", c_d,
     &         ", ", c_y 
               end if
               if(c_d > 9) then
               write(7, "(A13,I2,A2,I4)") "Date: May ", c_d,
     &         ", ", c_y 
               end if
          end if
          if(c_m == 9) then
               if(c_d < 10) then
               write(7, "(A16,I1,A2,I4)") "Date: September ", c_d,
     &         ", ", c_y 
               end if
               if(c_d > 9) then
               write(7, "(A16,I2,A2,I4)") "Date: September ", c_d,
     &         ", ", c_y 
               end if
          end if
          if(c_m == 10) then
               if(c_d < 10) then
               write(7, "(A14,I1,A2,I4)") "Date: October ", c_d,
     &         ", ", c_y 
               end if
               if(c_d > 9) then
               write(7, "(A14,I2,A2,I4)") "Date: October ", c_d,
     &         ", ", c_y 
               end if
          end if
          if(c_m ==11) then
               if(c_d < 10) then
               write(7, "(A15,I1,A2,I4)") "Date: November ", c_d,
     &         ", ", c_y 
               end if
               if(c_d > 9) then
               write(7, "(A15,I2,A2,I4)") "Date: November ", c_d,
     &         ", ", c_y 
               end if
          end if
          if(c_m == 12) then
               if(c_d < 10) then
               write(7, "(A15,I1,A2,I4)") "Date: December ", c_d,
     &         ", ", c_y 
               end if
               if(c_d > 9) then
               write(7, "(A15,I2,A2,I4)") "Date: December ", c_d,
     &         ", ", c_y 
               end if
          end if
          write(7,"(A58)") 
     &    "Staff-ID Name                            Department Status"
          write(7,"(A62)") 
     &"--------------------------------------------------------------"

          ! write the attendance status
4         continue
          read(4,"(I4,A10,A20,A21,A3,I6)", IOSTAT = e_ios) e_id, 
     &    e_f_n, e_l_n, e_n_u, e_dep, 
     &    e_sal 
          write(*,*) "e_id is ", e_id
          if(e_ios < 0) then
               write(*,*) "emp end"
          end if       
          if(e_ios == 0) then
               write(*,"(A3)") e_dep
               if(record(e_id,1) == 0) then 
                    write(7,"(I4,A5,A11,A21,A3,A8,A7)") 
     &              e_id, "     ", e_f_n, e_l_n, 
     &              e_dep, "        ", "ABSENCE"
                    ab = ab+1
               end if
               if(record(e_id,1) == 1) then 
                    write(7,"(I4,A5,A11,A21,A3,A8,A10)") 
     &              e_id, "     ", e_f_n, e_l_n, 
     &              e_dep, "        ", "su"
                    su = su+1
               end if
               if(record(e_id,1) == 2) then 
                    if(record(e_id, 2) == 0) then
                    write(7,"(I4,A5,A11,A21,A3,A8,A8)") 
     &              e_id, "     ", e_f_n, e_l_n, 
     &              e_dep, "        ", "PRESENCE"
                    pr = pr+1
                    end if
                    if(record(e_id, 2) /= 0) then
                    write(7,"(I4,A5,A11,A21,A3,A8,A4)") e_id,
     &               "     ", e_f_n, e_l_n, e_dep,
     &               "        ", "LATE"
                    late = late+1
                    end if
               end if          
               goto 4
          end if
          write(7,"(A62)") 
     &"--------------------------------------------------------------"
          write(7, "(A21,I4)") "Number of pr: ", pr
          write(7, "(A20,I4)") "Number of ab: ", ab
          write(7, "(A25,I4)") "Number of Late Arrivals: ", late
          write(7, "(A30,I4)") "Number of su Records: ", 
     &     su

          close(4)
          close(7)

          end program banma
