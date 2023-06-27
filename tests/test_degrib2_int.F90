! This is a test program for NCEPLIBS-grib_util.
!
! This program tests the level description in the degrib2 utility.
!
! Ed Hartnett 11/27/22
program test_degrib2_int
  implicit none
  integer :: pt(15) = (/ 2, 1, 2, 0, 11, 0, 0, 1, 0, 1, 0, 1, 255, 0, 0 /)
  integer :: pt_0_0(15) = (/ 2, 1, 2, 0, 11, 0, 0, 1, 0, 1, 0, 1, 255, 0, 0 /)
  integer :: pt_0_1(15) = (/ 2, 10, 0, 0, 81, 0, 0, 1, 0, 100, 0, 80000, 255, 0, 0 /)
  integer :: pt_0_2(15) = (/ 0, 21, 2, 255, 104, 65535, 255, 1, 1, 103, 0, 2, 255, 0, 0 /)
  integer :: pt_0_3(15) = (/ 19, 238, 2, 255, 104, 65535, 255, 1, 1, 100, 0, 40000, 100, 0, 30000 /)
  integer :: pt_8_0(29) = (/ 1, 228, 2, 255, 104, 65535, 255, 1, 0, 1, 0, 0, 255, 0, 0, 2022, 11, 17, &
       20, 0, 0, 1, 0, 1, 2, 1, 1, 1, 0 /)
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

  pt(10) = 101
  call prlevel(0, pt, la)
  if (la .ne. " Mean Sea Level ") stop 40

  pt(10) = 102
  call prlevel(0, pt, la)
  if (trim(la) .ne. "1 m above MSL") stop 40

  pt(10) = 102
  pt(13) = 255
  call prlevel(0, pt, la)
  if (trim(la) .ne. "1 m above MSL") stop 40
  pt(13) = 0

  pt(10) = 103
  call prlevel(0, pt, la)
  if (trim(la) .ne. "  103 (Unknown Lvl)") stop 40

  pt(10) = 103
  pt(13) = 255
  call prlevel(0, pt, la)
  if (trim(la) .ne. "1 m above ground") stop 40

  pt(10) = 104
  pt(13) = 255
  call prlevel(0, pt, la)
  if (trim(la) .ne. "1 sigma") stop 40

  pt(10) = 104
  pt(13) = 104
  call prlevel(0, pt, la)
  if (trim(la) .ne. "1 - 0 sigma") stop 40

  pt(10) = 105
  pt(13) = 255
  call prlevel(0, pt, la)
  if (trim(la) .ne. "1 hybrid lvl") stop 40

  pt(10) = 105
  pt(13) = 105
  call prlevel(0, pt, la)
  if (trim(la) .ne. "1 - 0 hybrid lvl") stop 40

  pt(10) = 106
  pt(13) = 255
  call prlevel(0, pt, la)
  if (trim(la) .ne. "1 m below land") stop 40

  pt(10) = 106
  pt(13) = 106
  call prlevel(0, pt, la)
  if (trim(la) .ne. "1 - 0 m DBLY") stop 40

  pt(10) = 107
  call prlevel(0, pt, la)
  if (trim(la) .ne. " Isentropic level") stop 40

  pt(10) = 108
  pt(13) = 108
  call prlevel(0, pt, la)
  if (trim(la) .ne. ".01 -  0 mb SPDY") stop 40

  pt(10) = 110
  call prlevel(0, pt, la)
  if (trim(la) .ne. " Layer bet 2-hyb lvl") stop 40

  pt(10) = 109
  pt(13) = 255
  call prlevel(0, pt, la)
  if (trim(la) .ne. " 1000000 pv surface") stop 40

  pt(10) = 111
  call prlevel(0, pt, la)
  if (trim(la) .ne. " Eta level") stop 40

  pt(10) = 114
  call prlevel(0, pt, la)
  print *, '/', trim(la), '/'
  if (trim(la) .ne. " Layer bet. 2-isent.") stop 40

  ! Template 8 with various options.
  call prlevel(8, pt_8_0, la)
  if (trim(la) .ne. " Surface") stop 40
  call prvtime(8, pt_8_0, s1_0, ta)
  if (trim(ta) .ne.  "(0 -1 hr) valid  0 hour after 2022111719:00:00 to 2022111720:00:00") stop 41

  print *, 'SUCCESS!'
  
end program test_degrib2_int
