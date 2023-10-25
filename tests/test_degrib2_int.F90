! This is a test program for NCEPLIBS-grib_util.
!
! This program tests the level description in the degrib2 utility.
!
! Ed Hartnett 11/27/22
program test_degrib2_int
  implicit none
  integer :: MAX_PT
  parameter(MAX_PT = 100)
  integer :: pt(MAX_PT)
  integer :: pt_0_0(15) = (/ 2, 1, 2, 0, 11, 0, 0, 1, 0, 1, 0, 1, 255, 0, 0 /)
  integer :: pt_0_1(15) = (/ 2, 10, 0, 0, 81, 0, 0, 1, 0, 100, 0, 80000, 255, 0, 0 /)
  integer :: pt_0_2(15) = (/ 0, 21, 2, 255, 104, 65535, 255, 1, 1, 103, 0, 2, 255, 0, 0 /)
  integer :: pt_0_3(15) = (/ 19, 238, 2, 255, 104, 65535, 255, 1, 1, 100, 0, 40000, 100, 0, 30000 /)
  integer :: pt_0_4(15) = (/ 0, 192, 2, 0, 98, 0, 0, 1, 0, 106, 2, 0, 106, 2, 10 /)
  integer :: pt_0_5(15) = (/ 19, 236, 2, 255, 104, 65535, 255, 1, 1, 102, 0, 0, 255, 0, 0 /)
  integer :: pt_0_6(15) = (/ 0, 0, 2, 0, 116, 0, 0, 1, 0, 108, 0, 3000, 108, 0, 0 /)
  integer :: pt_8_0(29) = (/ 1, 228, 2, 255, 104, 65535, 255, 1, 0, 1, 0, 0, 255, 0, 0, 2022, 11, 17, &
       20, 0, 0, 1, 0, 1, 2, 1, 1, 1, 0 /)
  integer :: pt_8_1(29) = (/ 14, 201, 2, 0, 89, 0, 0, 1, -1, 105, 0, 1, 255, -127, -2147483647, 2022, &
       11, 2, 10, 0, 0, 1, 0, 0, 2, 1, 23, 255, 0 /)
  integer :: pt_2_0(17) = (/ 0, 192, 4, 70, 70, 0, 0, 1, 0, 106, 0, 0, 106, 1, 1, 0, 20 /)
  integer :: pt_2_1(17) = (/ 3, 1, 4, 70, 70, 0, 0, 1, 0, 101, 0, 0, 255, 0, 0, 0, 20 /)
  integer :: pt_15_0(18) = (/ 0, 27, 2, 255, 104, 65535, 255, 1, 1, 103, 0, 610, 100, 0, 40000, 241, 241, 241 /)
  integer :: pt_9_0(36) = (/ 1, 8, 2, 255, 104, 65535, 255, 1, 0, 1, 0, 0, 255, 0, 0, 255, 255, 1, -127, &
       255, 3, 254, 2022, 11, 17, 20, 0, 0, 1, 0, 1, 2, 1, 1, 1, 0 /)
  integer :: s1_0(13) = (/ 7, 14, 1, 1, 1, 2022, 11, 17, 19, 0, 0, 0, 1 /)
  integer :: s1_1(13) = (/ 54, 0, 4, 1, 1, 2022, 11, 17, 12, 0, 0, 0, 4 /)
  integer :: s1_2(13) = (/ 54, 0, 4, 0, 1, 2022, 11, 17, 12, 0, 0, 0, 4 /)
  integer :: s1_3(13) = (/ 7, 0, 2, 1, 1, 2022, 11, 17, 0, 0, 0, 0, 1 /)
  integer :: s1_4(13) = (/ 7, 0, 0, 1, 1, 2022, 11, 1, 12, 0, 0, 0, 1 /)
  character(len = 40) :: la
  character(len = 100) :: ta
  integer :: NUM_TN, t
  parameter(NUM_TN = 8)
  integer :: tn(NUM_TN) = (/ 999, 91, 52, 50, 48, 0, 40, 44 /)
  integer :: ipos(NUM_TN) = (/ 10, 10, 13, 10, 21, 10, 11, 16 /)
  integer :: NUM_TN_T
  parameter(NUM_TN_T = 8)
  integer :: tn_t(NUM_TN_T) = (/ 90, 91, 0, 1, 40, 44, 48, 52 /)
  integer :: iutpos(NUM_TN_T) = (/ 8, 8, 8, 8, 9, 14, 19, 11 /)
  integer :: i

  print *, 'Testing degrib2 level and date/time descriptions.'

  do i = 1, MAX_PT
     pt(i) = 0
  end do

  ! Test all the prvtime values.
  print *, '*** testing prvtime() with various pdtns...'
  do t = 1, NUM_TN_T
     print *, '***    testing prvtime() with pdtn ', tn_t(t)

     pt(iutpos(t)) = 0
     pt(iutpos(t) + 1) = 1
     call prvtime(tn_t(t), pt, s1_0, ta)
!     print *, t,'/',trim(ta),'/'
     if (t .eq. 1) then
        if (trim(ta) .ne.  "valid at     1") stop 41
     elseif (t .eq. 2) then
        if (trim(ta) .ne.  "(1 -1 hr) valid  1 minute after 2022111719:00:00 to    0000000:00:00") stop 41
     else
        if (trim(ta) .ne.  "valid  1 minute after 2022111719:00:00") stop 41     
     end if
     pt(iutpos(t) + 1) = 0

     pt(iutpos(t)) = 1
     call prvtime(tn_t(t), pt, s1_0, ta)
!     print *, t,'/',trim(ta),'/'
     if (t .eq. 1) then
        if (trim(ta) .ne.  "valid at     0") stop 41
     elseif (t .eq. 2) then
        if (trim(ta) .ne.  "(0 -0 hr) valid  0 hour after 2022111719:00:00 to    0000000:00:00") stop 41
     else
        if (trim(ta) .ne.  "valid  0 hour after 2022111719:00:00") stop 41
     end if
     pt(iutpos(t)) = 0

     pt(iutpos(t)) = 2
     call prvtime(tn_t(t), pt, s1_0, ta)
!     print *, t,'/',trim(ta),'/'
     if (t .eq. 1) then
        if (trim(ta) .ne.  "valid at     0") stop 41
     elseif (t .eq. 2) then
        if (trim(ta) .ne.  "(0 -0 hr) valid  0 day after 2022111719:00:00 to    0000000:00:00") stop 41
     else
        if (trim(ta) .ne.  "valid  0 day after 2022111719:00:00") stop 41
     end if
     pt(iutpos(t)) = 0

     pt(iutpos(t)) = 3
     call prvtime(tn_t(t), pt, s1_0, ta)
!     print *, t,'/',trim(ta),'/'
     if (t .eq. 1) then
        if (trim(ta) .ne.  "valid at     0") stop 41
     elseif (t .eq. 2) then
        if (trim(ta) .ne.  "(0 -0 hr) valid  0 month after 2022111719:00:00 to    0000000:00:00") stop 41
     else
        if (trim(ta) .ne.  "valid  0 month after 2022111719:00:00") stop 41
     end if
     pt(iutpos(t)) = 0

     pt(iutpos(t)) = 4
     call prvtime(tn_t(t), pt, s1_0, ta)
!     print *, t,'/',trim(ta),'/'
     if (t .eq. 1) then
        if (trim(ta) .ne.  "valid at     0") stop 41
     elseif (t .eq. 2) then
        if (trim(ta) .ne.  "(0 -0 hr) valid  0 year after 2022111719:00:00 to    0000000:00:00") stop 41
     else
        if (trim(ta) .ne.  "valid  0 year after 2022111719:00:00") stop 41
     end if
     pt(iutpos(t)) = 0
     
     pt(iutpos(t)) = 10
     call prvtime(tn_t(t), pt, s1_0, ta)
!     print *, t,'/',trim(ta),'/'
     if (t .eq. 1) then
        if (trim(ta) .ne.  "valid at     0") stop 41
     elseif (t .eq. 2) then
        if (trim(ta) .ne.  "(0 -0 hr) valid  0 hour after 2022111719:00:00 to    0000000:00:00") stop 41
     else
        if (trim(ta) .ne.  "valid  0 hour after 2022111719:00:00") stop 41
     end if
     pt(iutpos(t)) = 0

     pt(iutpos(t)) = 11
     call prvtime(tn_t(t), pt, s1_0, ta)
!     print *, t,'/',trim(ta),'/'
     if (t .eq. 1) then
        if (trim(ta) .ne.  "valid at     0") stop 41
     elseif (t .eq. 2) then
        if (trim(ta) .ne.  "(0 -0 hr) valid  0 hour after 2022111719:00:00 to    0000000:00:00") stop 41
     else
        if (trim(ta) .ne.  "valid  0 hour after 2022111719:00:00") stop 41
     end if
     pt(iutpos(t)) = 0
     
     pt(iutpos(t)) = 99
     call prvtime(tn_t(t), pt, s1_0, ta)
!     print *, t,'/',trim(ta),'/'
     if (t .eq. 1) then
        if (trim(ta) .ne.  "valid at     0") stop 41
     elseif (t .eq. 2) then
        if (trim(ta) .ne.  "(0 -0 hr) valid  0 hour after 2022111719:00:00 to    0000000:00:00") stop 41
     else
        if (trim(ta) .ne.  "valid  0 hour after 2022111719:00:00") stop 41
     end if
     pt(iutpos(t)) = 0
  end do
  print *, 'OK!'

  print *, '*** testing different units for secondary times...'
  t = NUM_TN_T

  ! Check different units for secondary times.
  pt(iutpos(t)) = 99
  pt(33) = 2
  call prvtime(91, pt, s1_0, ta)
!  print *, t,'/',trim(ta),'/'
  if (trim(ta) .ne.  "(0 -0 hr) valid  0 minute after 2022111719:00:00 to    0000000:00:00") stop 55
  pt(iutpos(t)) = 0
  pt(33) = 0
  
  pt(iutpos(t)) = 99
  pt(33) = 3
  call prvtime(91, pt, s1_0, ta)
!  print *, t,'/',trim(ta),'/'
  if (trim(ta) .ne.  "(0 -0 hr) valid  0 minute after 2022111719:00:00 to    0000000:00:00") stop 55
  pt(iutpos(t)) = 0
  pt(33) = 0

  pt(iutpos(t)) = 99
  pt(33) = 4
  call prvtime(91, pt, s1_0, ta)
!  print *, t,'/',trim(ta),'/'
  if (trim(ta) .ne.  "(0 -0 hr) valid  0 minute after 2022111719:00:00 to    0000000:00:00") stop 55
  pt(iutpos(t)) = 0
  pt(33) = 0
  
  pt(iutpos(t)) = 99
  pt(33) = 10
  call prvtime(91, pt, s1_0, ta)
!  print *, t,'/',trim(ta),'/'
  if (trim(ta) .ne.  "(0 -0 hr) valid  0 minute after 2022111719:00:00 to    0000000:00:00") stop 55
  pt(iutpos(t)) = 0
  pt(33) = 0
  
  pt(iutpos(t)) = 99
  pt(33) = 11
  call prvtime(91, pt, s1_0, ta)
!  print *, t,'/',trim(ta),'/'
  if (trim(ta) .ne.  "(0 -0 hr) valid  0 minute after 2022111719:00:00 to    0000000:00:00") stop 55
  pt(iutpos(t)) = 0
  pt(33) = 0

  print *, 'OK!'
  print *, '*** testing prlevel() with various pdtns...'
  
  ! Test all the prlevel values.
  do t = 1, NUM_TN
     print *, '***    testing prlevel() with pdtn ', tn(t)
     pt(ipos(t)) = 101
     call prlevel(tn(t), pt, la)
     if (la .ne. " Mean Sea Level ") stop 40

     pt(ipos(t)) = 102
     pt(ipos(t) + 1) = 0
     pt(ipos(t) + 2) = 0
     pt(ipos(t) + 3) = 255
     call prlevel(tn(t), pt, la)
!     print *, pt(ipos(t)), '/', trim(la), '/'     
     if (trim(la) .ne. "0 m above MSL") stop 40
     pt(ipos(t) + 3) = 0

     pt(ipos(t)) = 103
     pt(ipos(t) + 3) = 0
     call prlevel(tn(t), pt, la)
!     print *, pt(ipos(t)), '/', trim(la), '/'     
     if (trim(la) .ne. "  103 (Unknown Lvl)") stop 40

     pt(ipos(t)) = 103
     pt(ipos(t) + 1) = 0
     pt(ipos(t) + 2) = 0
     pt(ipos(t) + 3) = 255
     pt(ipos(t) + 4) = 1
     pt(ipos(t) + 5) = 1
     call prlevel(tn(t), pt, la)
!     print *, pt(ipos(t)), '/', trim(la), '/'     
     if (trim(la) .ne. "0 m above ground") stop 40

     pt(ipos(t)) = 103
     pt(ipos(t) + 1) = 0
     pt(ipos(t) + 2) = 0
     pt(ipos(t) + 3) = 103
     pt(ipos(t) + 4) = 1
     pt(ipos(t) + 5) = 1
     call prlevel(tn(t), pt, la)
!     print *, pt(ipos(t)), '/', trim(la), '/'     
     if (trim(la) .ne. "0 - .1 m AGL") stop 40

     pt(ipos(t)) = 104
     pt(ipos(t) + 1) = 0
     pt(ipos(t) + 2) = 0
     pt(ipos(t) + 3) = 255
     call prlevel(tn(t), pt, la)
!     print *, pt(ipos(t)), '/', trim(la), '/'     
     if (trim(la) .ne. "0 sigma") stop 40

     pt(ipos(t)) = 104
     pt(ipos(t) + 1) = 0
     pt(ipos(t) + 2) = 0
     pt(ipos(t) + 3) = 104
     pt(ipos(t) + 4) = 1
     pt(ipos(t) + 5) = 1
     call prlevel(tn(t), pt, la)
!     print *, pt(ipos(t)), '/', trim(la), '/'     
     if (trim(la) .ne. "0 - .1 sigma") stop 40

     pt(ipos(t)) = 105
     pt(ipos(t) + 1) = 0
     pt(ipos(t) + 2) = 0
     pt(ipos(t) + 3) = 255
     call prlevel(tn(t), pt, la)
!     print *, pt(ipos(t)), '/', trim(la), '/'     
     if (trim(la) .ne. "0 hybrid lvl") stop 40

     pt(ipos(t)) = 105
     pt(ipos(t) + 1) = 0
     pt(ipos(t) + 2) = 0
     pt(ipos(t) + 3) = 105
     pt(ipos(t) + 4) = 1
     pt(ipos(t) + 5) = 1
     call prlevel(tn(t), pt, la)
!     print *, pt(ipos(t)), '/', trim(la), '/'     
     if (trim(la) .ne. "0 - .1 hybrid lvl") stop 40

     pt(ipos(t)) = 106
     pt(ipos(t) + 1) = 0
     pt(ipos(t) + 2) = 0
     pt(ipos(t) + 3) = 255
     call prlevel(tn(t), pt, la)
!     print *, pt(ipos(t)), '/', trim(la), '/'     
     if (trim(la) .ne. "0 m below land") stop 40

     pt(ipos(t)) = 106
     pt(ipos(t) + 1) = 0
     pt(ipos(t) + 2) = 0
     pt(ipos(t) + 3) = 106
     pt(ipos(t) + 4) = 1
     pt(ipos(t) + 5) = 1
     call prlevel(tn(t), pt, la)
!     print *, pt(ipos(t)), '/', trim(la), '/'     
     if (trim(la) .ne. "0 - .1 m DBLY") stop 40

     pt(ipos(t)) = 107
     call prlevel(tn(t), pt, la)
!     print *, pt(ipos(t)), '/', trim(la), '/'     
     if (trim(la) .ne. " Isentropic level") stop 40

     pt(ipos(t)) = 108
     pt(ipos(t) + 1) = 0
     pt(ipos(t) + 2) = 0
     pt(ipos(t) + 3) = 108
     pt(ipos(t) + 4) = 1
     pt(ipos(t) + 5) = 1
     call prlevel(tn(t), pt, la)
!     print *, pt(ipos(t)), '/', trim(la), '/'     
     if (trim(la) .ne. " 0 - .001 mb SPDY") stop 40

     pt(ipos(t)) = 110
     call prlevel(tn(t), pt, la)
!     print *, pt(ipos(t)), '/', trim(la), '/'     
     if (trim(la) .ne. " Layer bet 2-hyb lvl") stop 40

     pt(ipos(t)) = 109
     pt(ipos(t) + 1) = 0
     pt(ipos(t) + 2) = 0
     pt(ipos(t) + 3) = 255
     call prlevel(tn(t), pt, la)
!     print *, pt(ipos(t)), '/', trim(la), '/'     
     if (trim(la) .ne. " 0 pv surface") stop 40

     pt(ipos(t)) = 111
     call prlevel(tn(t), pt, la)
!     print *, pt(ipos(t)), '/', trim(la), '/'     
     if (trim(la) .ne. " Eta level") stop 40

     pt(ipos(t)) = 114
     call prlevel(tn(t), pt, la)
!     print *, pt(ipos(t)), '/', trim(la), '/'     
     if (trim(la) .ne. " Layer bet. 2-isent.") stop 40

     pt(ipos(t)) = 117
     call prlevel(tn(t), pt, la)
!     print *, pt(ipos(t)), '/', trim(la), '/'     
     if (trim(la) .ne. " Mixed layer depth") stop 50

     pt(ipos(t)) = 120
     call prlevel(tn(t), pt, la)
!     print *, pt(ipos(t)), '/', trim(la), '/'     
     if (trim(la) .ne. " Layer bet. 2-Eta lvl") stop 50

     pt(ipos(t)) = 121
     call prlevel(tn(t), pt, la)
     if (trim(la) .ne. " Layer bet. 2-isob.") stop 50

     pt(ipos(t)) = 125
     call prlevel(tn(t), pt, la)
     if (trim(la) .ne. " Specified height lvl") stop 50

     pt(ipos(t)) = 126
     call prlevel(tn(t), pt, la)
     if (trim(la) .ne. " Isobaric level") stop 50

     pt(ipos(t)) = 160
     call prlevel(tn(t), pt, la)
     if (trim(la) .ne. " Depth below sea lvl") stop 50

     pt(ipos(t)) = 170
     call prlevel(tn(t), pt, la)
     if (trim(la) .ne. " Ionospheric D-region lvl") stop 50

     pt(ipos(t)) = 1
     call prlevel(tn(t), pt, la)
     if (trim(la) .ne. " Surface ") stop 50

     pt(ipos(t)) = 2
     call prlevel(tn(t), pt, la)
     if (trim(la) .ne. " Cloud base lvl") stop 50

     pt(ipos(t)) = 3
     call prlevel(tn(t), pt, la)
     if (trim(la) .ne. " Cloud top lvl") stop 50

     pt(ipos(t)) = 4
     call prlevel(tn(t), pt, la)
     if (trim(la) .ne. " 0 Deg Isotherm") stop 50

     pt(ipos(t)) = 5
     call prlevel(tn(t), pt, la)
     if (trim(la) .ne. " Level of adiabatic") stop 50

     pt(ipos(t)) = 6
     call prlevel(tn(t), pt, la)
     if (trim(la) .ne. " Max wind lvl") stop 50

     pt(ipos(t)) = 7
     call prlevel(tn(t), pt, la)
     if (trim(la) .ne. " Tropopause") stop 50

     pt(ipos(t)) = 8
     call prlevel(tn(t), pt, la)
     if (trim(la) .ne. " Nom. top") stop 50

     pt(ipos(t)) = 9
     call prlevel(tn(t), pt, la)
     if (trim(la) .ne. " Sea Bottom") stop 50

     pt(ipos(t)) = 10
     call prlevel(tn(t), pt, la)
     if (trim(la) .ne. " Entire Atmosphere") stop 50

     pt(ipos(t)) = 11
     call prlevel(tn(t), pt, la)
     if (trim(la) .ne. " Cumulonimbus Base") stop 50

     pt(ipos(t)) = 12
     call prlevel(tn(t), pt, la)
     if (trim(la) .ne. " Cumulonimbus Top") stop 50

     pt(ipos(t)) = 20
     call prlevel(tn(t), pt, la)
     if (trim(la) .ne. " Isothermal level") stop 50

     pt(ipos(t)) = 200
     call prlevel(tn(t), pt, la)
     if (trim(la) .ne. " Entire Atmosphere") stop 50

     pt(ipos(t)) = 201
     call prlevel(tn(t), pt, la)
     if (trim(la) .ne. " Entire ocean") stop 50

     pt(ipos(t)) = 204
     call prlevel(tn(t), pt, la)
     if (trim(la) .ne. " Highest Frz. lvl") stop 50

     pt(ipos(t)) = 206
     call prlevel(tn(t), pt, la)
     if (trim(la) .ne. " Grid scale cloud bl") stop 50

     pt(ipos(t)) = 207
     call prlevel(tn(t), pt, la)
     if (trim(la) .ne. " Grid scale cloud tl") stop 50

     pt(ipos(t)) = 209
     call prlevel(tn(t), pt, la)
     if (trim(la) .ne. " Boundary layer cbl") stop 50

     pt(ipos(t)) = 210
     call prlevel(tn(t), pt, la)
     if (trim(la) .ne. " Boundary layer ctl") stop 50

     pt(ipos(t)) = 211
     call prlevel(tn(t), pt, la)
     if (trim(la) .ne. " Boundary layer cl") stop 50

     pt(ipos(t)) = 212
     call prlevel(tn(t), pt, la)
     if (trim(la) .ne. " Low cloud bot. lvl") stop 50

     pt(ipos(t)) = 213
     call prlevel(tn(t), pt, la)
     if (trim(la) .ne. " Low cloud top lvl") stop 50

     pt(ipos(t)) = 214
     call prlevel(tn(t), pt, la)
     if (trim(la) .ne. " Low cloud layer") stop 50

     pt(ipos(t)) = 215
     call prlevel(tn(t), pt, la)
     if (trim(la) .ne. " Cloud ceiling") stop 50

     pt(ipos(t)) = 220
     call prlevel(tn(t), pt, la)
     if (trim(la) .ne. " Planetary boundary") stop 50

     pt(ipos(t)) = 221
     call prlevel(tn(t), pt, la)
     if (trim(la) .ne. " Layer 2 Hybrid lvl ") stop 50

     pt(ipos(t)) = 222
     call prlevel(tn(t), pt, la)
     if (trim(la) .ne. " Mid. cloud bot. lvl") stop 50

     pt(ipos(t)) = 223
     call prlevel(tn(t), pt, la)
     if (trim(la) .ne. " Mid. cloud top lvl") stop 50

     pt(ipos(t)) = 224
     call prlevel(tn(t), pt, la)
     if (trim(la) .ne. " Middle cloud layer") stop 50

     pt(ipos(t)) = 232
     call prlevel(tn(t), pt, la)
     if (trim(la) .ne. " High cloud bot. lvl") stop 50

     pt(ipos(t)) = 233
     call prlevel(tn(t), pt, la)
     if (trim(la) .ne. " High cloud top lvl") stop 50

     pt(ipos(t)) = 234
     call prlevel(tn(t), pt, la)
     if (trim(la) .ne. " High cloud layer") stop 50

     pt(ipos(t)) = 235
     call prlevel(tn(t), pt, la)
     if (trim(la) .ne. " Ocean Isotherm lvl") stop 50

     pt(ipos(t)) = 236
     call prlevel(tn(t), pt, la)
     if (trim(la) .ne. " Layer 2-depth below") stop 50

     pt(ipos(t)) = 237
     call prlevel(tn(t), pt, la)
     if (trim(la) .ne. " Bot. Ocean mix. lyr") stop 50

     pt(ipos(t)) = 238
     call prlevel(tn(t), pt, la)
     if (trim(la) .ne. " Bot. Ocean iso. lyr") stop 50

     pt(ipos(t)) = 239
     call prlevel(tn(t), pt, la)
     if (trim(la) .ne. " layer ocean sfc 26C") stop 50

     pt(ipos(t)) = 240
     call prlevel(tn(t), pt, la)
     if (trim(la) .ne. " Ocean Mixed Layer") stop 50

     pt(ipos(t)) = 241
     call prlevel(tn(t), pt, la)
     if (trim(la) .ne. " Order Seq. Of Data") stop 50

     pt(ipos(t)) = 242
     call prlevel(tn(t), pt, la)
     if (trim(la) .ne. " Con. cloud bot. lvl") stop 50

     pt(ipos(t)) = 243
     call prlevel(tn(t), pt, la)
     if (trim(la) .ne. " Con. cloud top lvl") stop 50

     pt(ipos(t)) = 244
     call prlevel(tn(t), pt, la)
     if (trim(la) .ne. " Conv. cloud layer") stop 50

     pt(ipos(t)) = 245
     call prlevel(tn(t), pt, la)
     if (trim(la) .ne. " Lowest lvl wet bulb") stop 50

     pt(ipos(t)) = 246
     call prlevel(tn(t), pt, la)
     if (trim(la) .ne. " Max. equi. potential") stop 50

     pt(ipos(t)) = 247
     call prlevel(tn(t), pt, la)
     if (trim(la) .ne. " Equilibrium level") stop 50

     pt(ipos(t)) = 248
     call prlevel(tn(t), pt, la)
     if (trim(la) .ne. " Shallow con. cld bl") stop 50

     pt(ipos(t)) = 249
     call prlevel(tn(t), pt, la)
     if (trim(la) .ne. " Shallow con. cld tl") stop 50

     pt(ipos(t)) = 251
     call prlevel(tn(t), pt, la)
     if (trim(la) .ne. " Deep conv. cld bl") stop 50

     pt(ipos(t)) = 252
     call prlevel(tn(t), pt, la)
     if (trim(la) .ne. " Deep conv. cld tl") stop 50

     pt(ipos(t)) = 253
     call prlevel(tn(t), pt, la)
     if (trim(la) .ne. " Lowest bot. lvl sup") stop 50

     pt(ipos(t)) = 254
     call prlevel(tn(t), pt, la)
     if (trim(la) .ne. " highest top lvl sup") stop 50

     pt(ipos(t)) = 999
     call prlevel(tn(t), pt, la)
     !  print *, '/', trim(la), '/'
     if (trim(la) .ne. "  999 (Unknown Lvl)") stop 50
  end do

  print *, 'OK!'
  print *, '*** Testing prlevel() and prvtime() with various cases from test files...'

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

  ! Template 8 with various options.
  call prlevel(8, pt_8_0, la)
  if (trim(la) .ne. " Surface") stop 40
  call prvtime(8, pt_8_0, s1_0, ta)
  if (trim(ta) .ne.  "(0 -1 hr) valid  0 hour after 2022111719:00:00 to 2022111720:00:00") stop 41

  ! From cmc_geavg.t12z.pgrb2a.0p50.f000.degrib2.
  !  GRIB MESSAGE  69  starts at 8674274
  !
  !   SECTION 0:  2 2 138064
  !   SECTION 1:  54 0 4 1 1 2022 11 17 12 0 0 0 4
  !   Contains  0  Local Sections  and  1  data fields.
  !
  !   FIELD  1
  !   SECTION 0:  2 2
  !   SECTION 1:  54 0 4 1 1 2022 11 17 12 0 0 0 4
  !   SECTION 3:  0 259920 0 0 0
  !   GRID TEMPLATE 3. 0 :  6 0 0 0 0 0 0 720 361 0 0 -90000000 0 48 90000000 359500000 500000 500000 64
  !   NO Optional List Defining Number of Data Points.
  !   PRODUCT TEMPLATE 4. 2: ( PARAMETER = SOILW    2 0 192 )  0 192 4 70 70 0 0 1 0 106 0 0 106 1 1 0 20
  !   FIELD: SOILW   0 - .1 m DBLY valid  0 hour after 2022111712:00:00
  !   NO Optional Vertical Coordinate List.
  !   Num. of Data Points =  87979    with BIT-MAP  0
  !   DRS TEMPLATE 5. 40 :  1115711650 -2 4 16 0 0 255
  !   Data Values:
  !   Num. of Data Points =  87979   Num. of Data Undefined = 0
  ! ( PARM= SOILW ) :  MIN=               0.00642044 AVE=               0.39450106 MAX=               0.88289541
  call prlevel(2, pt_2_0, la)
  if (trim(la) .ne. "0 - .1 m DBLY") stop 50
  call prvtime(2, pt_2_0, s1_1, ta)
  if (trim(ta) .ne.  "valid  0 hour after 2022111712:00:00") stop 51

  ! This is from ref_flxf2022111712.01.2022111712.grb2.degrib2.
  !  GRIB MESSAGE  6  starts at 276682
  !
  !   SECTION 0:  2 2 33672
  !   SECTION 1:  7 0 2 1 1 2022 11 17 12 0 0 0 1
  !   Contains  0  Local Sections  and  1  data fields.
  !
  !   FIELD  1
  !   SECTION 0:  2 2
  !   SECTION 1:  7 0 2 1 1 2022 11 17 12 0 0 0 1
  !   SECTION 3:  0 72960 0 0 40
  !   GRID TEMPLATE 3. 40 :  6 0 0 0 0 0 0 384 190 0 0 89277000 0 48 -89277000 359062000 938000 95 0
  !   NO Optional List Defining Number of Data Points.
  !   PRODUCT TEMPLATE 4. 0: ( PARAMETER = SOILW    2 0 192 )  0 192 2 0 98 0 0 1 0 106 2 0 106 2 10
  !   FIELD: SOILW    0 - .10 m DBLY valid  0 hour after 2022111712:00:00
  !   NO Optional Vertical Coordinate List.
  !   Num. of Data Points =  24626    with BIT-MAP  0
  !   DRS TEMPLATE 5. 40 :  1133707264 0 4 14 0 0 255
  !   Data Values:
  !   Num. of Data Points =  24626   Num. of Data Undefined = 0
  ! ( PARM= SOILW ) :  MIN=               0.02940000 AVE=               0.51248300 MAX=               1.00000000
  call prlevel(0, pt_0_4, la)
  if (trim(la) .ne. " 0 - .10 m DBLY") stop 60
  call prvtime(0, pt_0_4, s1_1, ta)
  if (trim(ta) .ne.  "valid  0 hour after 2022111712:00:00") stop 61

  !  GRIB MESSAGE  62  starts at 7836496
  !
  !   SECTION 0:  0 2 84897
  !   SECTION 1:  54 0 4 0 1 2022 11 17 12 0 0 0 4
  !   Contains  0  Local Sections  and  1  data fields.
  !
  !   FIELD  1
  !   SECTION 0:  0 2
  !   SECTION 1:  54 0 4 0 1 2022 11 17 12 0 0 0 4
  !   SECTION 3:  0 259920 0 0 0
  !   GRID TEMPLATE 3. 0 :  6 0 0 0 0 0 0 720 361 0 0 -90000000 0 48 90000000 359500000 500000 500000 64
  !   NO Optional List Defining Number of Data Points.
  !   PRODUCT TEMPLATE 4. 2: ( PARAMETER = PRMSL    0 3 1 )  3 1 4 70 70 0 0 1 0 101 0 0 255 0 0 0 20
  !   FIELD: PRMSL   Mean Sea Level  valid  0 hour after 2022111712:00:00
  !   NO Optional Vertical Coordinate List.
  !   Num. of Data Points =  259920     NO BIT-MAP 
  !   DRS TEMPLATE 5. 40 :  1203330578 2 0 12 0 0 255
  !   Data Values:
  !   Num. of Data Points =  259920   Num. of Data Undefined = 0
  ! ( PARM= PRMSL ) :  MIN=           94908.14062500 AVE=          100994.26562500 MAX=          105356.14062500
  call prlevel(2, pt_2_1, la)
  if (trim(la) .ne. " Mean Sea Level") stop 70
  call prvtime(2, pt_2_1, s1_2, ta)
  if (trim(ta) .ne.  "valid  0 hour after 2022111712:00:00") stop 71

  ! From ref_blend.t19z.core.f001.co.grib2.degrib2:
  !  GRIB MESSAGE  34  starts at 44231011
  !
  !   SECTION 0:  0 2 1371114
  !   SECTION 1:  7 14 1 1 1 2022 11 17 19 0 0 0 1
  !   Contains  0  Local Sections  and  1  data fields.
  !
  !   FIELD  1
  !   SECTION 0:  0 2
  !   SECTION 1:  7 14 1 1 1 2022 11 17 19 0 0 0 1
  !   SECTION 3:  0 3744965 0 0 30
  !   GRID TEMPLATE 3. 30 :  1 0 6371200 255 255 255 255 2345 1597 19229000 233723400 48 25000000 265000000 2539703 2539703 0 80 25000000 25000000 -90000000 0
  !   NO Optional List Defining Number of Data Points.
  !   PRODUCT TEMPLATE 4. 15: ( PARAMETER = UNKNOWN  0 0 27 )  0 27 2 255 104 65535 255 1 1 103 0 610 100 0 40000 241 241 241
  !   FIELD: UNKNOWN   103 (Unknown Lvl) valid  1 hour after 2022111719:00:00
  !   NO Optional Vertical Coordinate List.
  !   Num. of Data Points =  3744965     NO BIT-MAP 
  !   DRS TEMPLATE 5. 3 :  1159272448 2 1 7 0 1 1 1176255488 255 209539 1 3 1 1 11 6 2 1
  !   Data Values:
  !   Num. of Data Points =  3744965   Num. of Data Undefined = 0
  ! ( PARM= UNKNOWN ) :  MIN=             244.94999695 AVE=             267.56930542 MAX=             294.55001831
  call prlevel(15, pt_15_0, la)
  if (trim(la) .ne. "  103 (Unknown Lvl)") stop 80
  call prvtime(15, pt_15_0, s1_0, ta)
  if (trim(ta) .ne.  "valid  1 hour after 2022111719:00:00") stop 81

  ! This is from ref_blend.t19z.core.f001.co.grib2.degrib2:
  !  GRIB MESSAGE  36  starts at 46129905
  !
  !   SECTION 0:  0 2 775840
  !   SECTION 1:  7 14 1 1 1 2022 11 17 19 0 0 0 1
  !   Contains  0  Local Sections  and  1  data fields.
  !
  !   FIELD  1
  !   SECTION 0:  0 2
  !   SECTION 1:  7 14 1 1 1 2022 11 17 19 0 0 0 1
  !   SECTION 3:  0 3744965 0 0 30
  !   GRID TEMPLATE 3. 30 :  1 0 6371200 255 255 255 255 2345 1597 19229000 233723400 48 25000000 265000000 2539703 2539703 0 80 25000000 25000000 -90000000 0
  !   NO Optional List Defining Number of Data Points.
  !   PRODUCT TEMPLATE 4. 9: ( PARAMETER = APCP     0 1 8 )  1 8 2 255 104 65535 255 1 0 1 0 0 255 0 0 255 255 1 -127 255 3 254 2022 11 17 20 0 0 1 0 1 2 1 1 1 0
  !   FIELD: APCP     Surface (0 -1 hr) valid  0 hour after 2022111719:00:00 to 2022111720:00:00
  !   NO Optional Vertical Coordinate List.
  !   Num. of Data Points =  3744965     NO BIT-MAP 
  !   DRS TEMPLATE 5. 3 :  0 0 0 6 0 1 1 1176255488 255 82673 1 3 1 1 59 8 2 1
  !   Data Values:
  !   Num. of Data Points =  3744965   Num. of Data Undefined = 0
  ! ( PARM= APCP ) :  MIN=               0.00000000 AVE=               3.45403767 MAX=              94.00000000
  call prlevel(9, pt_9_0, la)
  if (trim(la) .ne. " Surface") stop 90
  call prvtime(9, pt_9_0, s1_0, ta)
  if (trim(ta) .ne.  "(0 -1 hr) valid  0 hour after 2022111719:00:00 to 2022111720:00:00") stop 91

  ! This is from ref_blend.t19z.core.f001.co.grib2.degrib2:
  !  GRIB MESSAGE  52  starts at 62185534
  !
  !   SECTION 0:  0 2 987256
  !   SECTION 1:  7 14 1 1 1 2022 11 17 19 0 0 0 1
  !   Contains  0  Local Sections  and  1  data fields.
  !
  !   FIELD  1
  !   SECTION 0:  0 2
  !   SECTION 1:  7 14 1 1 1 2022 11 17 19 0 0 0 1
  !   SECTION 3:  0 3744965 0 0 30
  !   GRID TEMPLATE 3. 30 :  1 0 6371200 255 255 255 255 2345 1597 19229000 233723400 48 25000000 265000000 2539703 2539703 0 80 25000000 25000000 -90000000 0
  !   NO Optional List Defining Number of Data Points.
  !   PRODUCT TEMPLATE 4. 0: ( PARAMETER = UNKNOWN  0 19 236 )  19 236 2 255 104 65535 255 1 1 102 0 0 255 0 0
  !   FIELD: UNKNOWN 0 m above MSL valid  1 hour after 2022111719:00:00
  !   NO Optional Vertical Coordinate List.
  !   Num. of Data Points =  3744965     NO BIT-MAP 
  !   DRS TEMPLATE 5. 3 :  0 3 0 7 0 1 1 1176255488 255 91497 1 3 1 1 256 8 2 2
  !   Data Values:
  !   Num. of Data Points =  3744965   Num. of Data Undefined = 0
  ! ( PARM= UNKNOWN ) :  MIN=               0.00000000 AVE=            1427.00317383 MAX=            4080.00000000
  call prlevel(0, pt_0_5, la)
  if (trim(la) .ne. "0 m above MSL") stop 100
  call prvtime(0, pt_0_5, s1_0, ta)
  if (trim(ta) .ne.  "valid  1 hour after 2022111719:00:00") stop 101

  ! From ref_hiresw.t00z.arw_5km.f00.hi.grib2.degrib2
  !  GRIB MESSAGE  197  starts at 3014616
  !
  !   SECTION 0:  0 2 11719
  !   SECTION 1:  7 0 2 1 1 2022 11 17 0 0 0 0 1
  !   Contains  0  Local Sections  and  1  data fields.
  !
  !   FIELD  1
  !   SECTION 0:  0 2
  !   SECTION 1:  7 0 2 1 1 2022 11 17 0 0 0 0 1
  !   SECTION 3:  0 37910 0 0 0
  !   GRID TEMPLATE 3. 0 :  6 0 0 0 0 0 0 223 170 0 -1 16400000 197650000 56 24005000 207640000 45000 45000 64
  !   NO Optional List Defining Number of Data Points.
  !   PRODUCT TEMPLATE 4. 0: ( PARAMETER = TMP      0 0 0 )  0 0 2 0 116 0 0 1 0 108 0 3000 108 0 0
  !   FIELD: TMP      30 -  0 mb SPDY valid  0 hour after 2022111700:00:00
  !   NO Optional Vertical Coordinate List.
  !   Num. of Data Points =  37910     NO BIT-MAP 
  !   DRS TEMPLATE 5. 3 :  1133056139 -4 0 7 0 1 0 1649987994 -1 701 0 4 1 1 29 8 2 2
  !   Data Values:
  !   Num. of Data Points =  37910   Num. of Data Undefined = 0
  ! ( PARM= TMP ) :  MIN=             274.12924194 AVE=             296.74426270 MAX=             300.44174194
  call prlevel(0, pt_0_6, la)
  if (trim(la) .ne. " 30 -  0 mb SPDY") stop 110
  call prvtime(0, pt_0_6, s1_3, ta)
  if (trim(ta) .ne.  "valid  0 hour after 2022111700:00:00") stop 111

  ! This is from ref_aqm.t12z.max_8hr_o3.227.grib2.degrib2:
  !  GRIB MESSAGE  1  starts at 1
  !
  !   SECTION 0:  0 2 757557
  !   SECTION 1:  7 0 0 1 1 2022 11 1 12 0 0 0 1
  !   Contains  0  Local Sections  and  1  data fields.
  !
  !   FIELD  1
  !   SECTION 0:  0 2
  !   SECTION 1:  7 0 0 1 1 2022 11 1 12 0 0 0 1
  !   SECTION 3:  0 1509825 0 0 30
  !   GRID TEMPLATE 3. 30 :  6 0 0 0 0 0 0 1473 1025 12190000 226541000 48 25000000 265000000 5079000 5079000 0 64 25000000 25000000 -90000000 0
  !   NO Optional List Defining Number of Data Points.
  !   PRODUCT TEMPLATE 4. 8: ( PARAMETER = OZMAX8   0 14 201 )  14 201 2 0 89 0 0 1 -1 105 0 1 255 -127 -2147483647 2022 11 2 10 0 0 1 0 0 2 1 23 255 0
  !   FIELD: OZMAX8  1 hybrid lvl (1 -24) valid  1 hour before 2022110112:00:00 to 2022110210:00:00
  !   NO Optional Vertical Coordinate List.
  !   Num. of Data Points =  1509825     NO BIT-MAP 
  !   DRS TEMPLATE 5. 3 :  1102324794 -5 0 11 0 1 1 1649987994 -1 51278 0 4 1 1 40 7 1 2
  !   Data Values:
  !   Num. of Data Points =  1509825   Num. of Data Undefined = 129961
  ! ( PARM= OZMAX8 ) :  MIN=              22.51768875 AVE=              44.23855591 MAX=              79.61143494
  call prlevel(8, pt_8_1, la)
  !print *, la
  if (trim(la) .ne. "1 hybrid lvl") stop 120
  call prvtime(8, pt_8_1, s1_4, ta)
  !print *, ta
  if (trim(ta) .ne.  "(1 -24) valid  1 hour before 2022110112:00:00 to 2022110210:00:00") stop 121
  
  print *, 'OK!'
  print *, 'SUCCESS!'
  
end program test_degrib2_int
