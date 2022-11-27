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
  character(len = 40) :: la

  print *, 'Testing degrib2 level descriptions...'

  ! Template 0 with various options.
  call prlevel(0, pt_0_0, la)
  if (la .ne. " Surface") stop 10

  call prlevel(0, pt_0_1, la)
  if (la .ne. " 800 mb") stop 11

  call prlevel(0, pt_0_2, la)
  print *, '!', la, '!'
  if (trim(la) .ne. "2 m above ground") stop 12

  print *, 'SUCCESS!'
  
end program test_degrib2_int
