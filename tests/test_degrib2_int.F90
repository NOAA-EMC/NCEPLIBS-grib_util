! This is a test program for NCEPLIBS-grib_util.
!
! This program tests the level description in the degrib2 utility.
!
! Ed Hartnett 11/27/22
program test_degrib2_int
  implicit none
  integer :: pt_0_0(15) = (/ 2, 1, 2, 0, 11, 0, 0, 1, 0, 1, 0, 1, 255, 0, 0 /)
  integer :: pt_0_1(15) = (/ 2, 10, 0, 0, 81, 0, 0, 1, 0, 100, 0, 80000, 255, 0, 0 /)
  integer :: pt_0_2(15) = (/ 0, 21, 2, 255, 104, 65535, 255, 1, 1, 103, 0, 2, 255, 0, 0 /)
  integer :: pt_0_3(15) = (/ 19, 238, 2, 255, 104, 65535, 255, 1, 1, 100, 0, 40000, 100, 0, 30000 /)
  integer :: s1_0(13) = (/ 7, 14, 1, 1, 1, 2022, 11, 17, 19, 0, 0, 0, 1 /)
  character(len = 40) :: la
  character(len = 100) :: ta

  print *, 'Testing degrib2 level and date/time descriptions...'

  ! Template 0 with various options.
  call prlevel(0, pt_0_0, la)
  if (la .ne. " Surface") stop 10
  call prvtime(0, pt_0_0, s1_0, ta)
  if (trim(ta) .ne.  "valid  0 hour after 2022111719:00:00") stop 11

  call prlevel(0, pt_0_1, la)
  if (la .ne. " 800 mb") stop 20
  call prvtime(0, pt_0_1, s1_0, ta)
  if (trim(ta) .ne.  "valid  0 hour after 2022111719:00:00") stop 21
  
  call prlevel(0, pt_0_2, la)
  if (trim(la) .ne. "2 m above ground") stop 30
  call prvtime(0, pt_0_2, s1_0, ta)
  if (trim(ta) .ne.  "valid  1 hour after 2022111719:00:00") stop 31

  call prlevel(0, pt_0_3, la)
  if (trim(la) .ne. " 400 -  300 mb") stop 40
  call prvtime(0, pt_0_3, s1_0, ta)
  if (trim(ta) .ne.  "valid  1 hour after 2022111719:00:00") stop 41

  print *, 'SUCCESS!'
  
end program test_degrib2_int
