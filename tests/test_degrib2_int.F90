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
  integer :: pt_8_0(29) = (/ 1, 228, 2, 255, 104, 65535, 255, 1, 0, 1, 0, 0, 255, 0, 0, 2022, 11, 17, &
       20, 0, 0, 1, 0, 1, 2, 1, 1, 1, 0 /)
  integer :: s1_0(13) = (/ 7, 14, 1, 1, 1, 2022, 11, 17, 19, 0, 0, 0, 1 /)
  character(len = 40) :: la
  character(len = 100) :: ta
  integer :: NUM_TN, t
  parameter(NUM_TN = 8)
  integer :: tn(NUM_TN) = (/ 999, 91, 52, 50, 48, 0, 40, 44 /)
  integer :: ipos(NUM_TN) = (/ 10, 10, 13, 10, 21, 10, 11, 16 /)
  integer :: NUM_TN_T
  parameter(NUM_TN_T = 7)
  integer :: tn_t(NUM_TN_T) = (/ 91, 0, 1, 40, 44, 48, 52 /)
  integer :: iutpos(NUM_TN_T) = (/ 8, 8, 8, 9, 14, 19, 11 /)
  integer :: i

  print *, 'Testing degrib2 level and date/time descriptions...'

  do i = 1, MAX_PT
     pt(i) = 0
  end do

  ! Test all the prvtime values.
  do t = 1, 7
     print *, '*** Testing prvtime() with pdtn ', tn_t(t)

     pt(iutpos(t)) = 0
     pt(iutpos(t) + 1) = 1
     call prvtime(tn_t(t), pt, s1_0, ta)
     if (t .eq. 1) then
        if (trim(ta) .ne.  "(1 -1 hr) valid  1 minute after 2022111719:00:00 to    0000000:00:00") stop 41
     else
        if (trim(ta) .ne.  "valid  1 minute after 2022111719:00:00") stop 41     
     end if
     pt(iutpos(t) + 1) = 0

     pt(iutpos(t)) = 1
     call prvtime(tn_t(t), pt, s1_0, ta)
     if (t .eq. 1) then
        if (trim(ta) .ne.  "(0 -0 hr) valid  0 hour after 2022111719:00:00 to    0000000:00:00") stop 41
     else
        if (trim(ta) .ne.  "valid  0 hour after 2022111719:00:00") stop 41
     end if
     pt(iutpos(t)) = 0

     pt(iutpos(t)) = 2
     call prvtime(tn_t(t), pt, s1_0, ta)
     if (t .eq. 1) then
        if (trim(ta) .ne.  "(0 -0 hr) valid  0 day after 2022111719:00:00 to    0000000:00:00") stop 41
     else
        if (trim(ta) .ne.  "valid  0 day after 2022111719:00:00") stop 41
     end if
     pt(iutpos(t)) = 0

     pt(iutpos(t)) = 3
     call prvtime(tn_t(t), pt, s1_0, ta)
     print *, t,'/',trim(ta),'/'     
     if (t .eq. 1) then
        if (trim(ta) .ne.  "(0 -0 hr) valid  0 month after 2022111719:00:00 to    0000000:00:00") stop 41
     else
        if (trim(ta) .ne.  "valid  0 month after 2022111719:00:00") stop 41
     end if
     pt(iutpos(t)) = 0

     
  end do
  
  ! Test all the prlevel values.
  do t = 1, NUM_TN
     print *, '*** Testing prlevel() with pdtn ', tn(t)
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

  print *, 'SUCCESS!'
  
end program test_degrib2_int
