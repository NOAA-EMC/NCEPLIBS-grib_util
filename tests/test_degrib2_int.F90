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
  if (trim(la) .ne. " Layer bet. 2-isent.") stop 40

  pt(10) = 117
  call prlevel(0, pt, la)
  if (trim(la) .ne. " Mixed layer depth") stop 50

  pt(10) = 120
  call prlevel(0, pt, la)
  if (trim(la) .ne. " Layer bet. 2-Eta lvl") stop 50

  pt(10) = 121
  call prlevel(0, pt, la)
  if (trim(la) .ne. " Layer bet. 2-isob.") stop 50

  pt(10) = 125
  call prlevel(0, pt, la)
  if (trim(la) .ne. " Specified height lvl") stop 50

  pt(10) = 126
  call prlevel(0, pt, la)
  if (trim(la) .ne. " Isobaric level") stop 50

  pt(10) = 160
  call prlevel(0, pt, la)
  if (trim(la) .ne. " Depth below sea lvl") stop 50

  pt(10) = 170
  call prlevel(0, pt, la)
  if (trim(la) .ne. " Ionospheric D-region lvl") stop 50

  pt(10) = 1
  call prlevel(0, pt, la)
  if (trim(la) .ne. " Surface ") stop 50

  pt(10) = 2
  call prlevel(0, pt, la)
  if (trim(la) .ne. " Cloud base lvl") stop 50

  pt(10) = 3
  call prlevel(0, pt, la)
  if (trim(la) .ne. " Cloud top lvl") stop 50

  pt(10) = 4
  call prlevel(0, pt, la)
  if (trim(la) .ne. " 0 Deg Isotherm") stop 50

  pt(10) = 5
  call prlevel(0, pt, la)
  if (trim(la) .ne. " Level of adiabatic") stop 50

  pt(10) = 6
  call prlevel(0, pt, la)
  if (trim(la) .ne. " Max wind lvl") stop 50

  pt(10) = 7
  call prlevel(0, pt, la)
  if (trim(la) .ne. " Tropopause") stop 50

  pt(10) = 8
  call prlevel(0, pt, la)
  if (trim(la) .ne. " Nom. top") stop 50

  pt(10) = 9
  call prlevel(0, pt, la)
  if (trim(la) .ne. " Sea Bottom") stop 50

  pt(10) = 10
  call prlevel(0, pt, la)
  if (trim(la) .ne. " Entire Atmosphere") stop 50

  pt(10) = 11
  call prlevel(0, pt, la)
  if (trim(la) .ne. " Cumulonimbus Base") stop 50

  pt(10) = 12
  call prlevel(0, pt, la)
  if (trim(la) .ne. " Cumulonimbus Top") stop 50

  pt(10) = 20
  call prlevel(0, pt, la)
  if (trim(la) .ne. " Isothermal level") stop 50

  pt(10) = 200
  call prlevel(0, pt, la)
  if (trim(la) .ne. " Entire Atmosphere") stop 50

  pt(10) = 201
  call prlevel(0, pt, la)
  if (trim(la) .ne. " Entire ocean") stop 50

  pt(10) = 204
  call prlevel(0, pt, la)
  if (trim(la) .ne. " Highest Frz. lvl") stop 50

  pt(10) = 206
  call prlevel(0, pt, la)
  if (trim(la) .ne. " Grid scale cloud bl") stop 50

  pt(10) = 207
  call prlevel(0, pt, la)
  if (trim(la) .ne. " Grid scale cloud tl") stop 50

  pt(10) = 209
  call prlevel(0, pt, la)
  if (trim(la) .ne. " Boundary layer cbl") stop 50

  pt(10) = 210
  call prlevel(0, pt, la)
  if (trim(la) .ne. " Boundary layer ctl") stop 50

  pt(10) = 211
  call prlevel(0, pt, la)
  if (trim(la) .ne. " Boundary layer cl") stop 50

  pt(10) = 212
  call prlevel(0, pt, la)
  if (trim(la) .ne. " Low cloud bot. lvl") stop 50

  pt(10) = 213
  call prlevel(0, pt, la)
  if (trim(la) .ne. " Low cloud top lvl") stop 50

  pt(10) = 214
  call prlevel(0, pt, la)
  if (trim(la) .ne. " Low cloud layer") stop 50

  pt(10) = 215
  call prlevel(0, pt, la)
  if (trim(la) .ne. " Cloud ceiling") stop 50

  pt(10) = 220
  call prlevel(0, pt, la)
  if (trim(la) .ne. " Planetary boundary") stop 50

  pt(10) = 221
  call prlevel(0, pt, la)
  if (trim(la) .ne. " Layer 2 Hybrid lvl ") stop 50

  pt(10) = 222
  call prlevel(0, pt, la)
  if (trim(la) .ne. " Mid. cloud bot. lvl") stop 50

  pt(10) = 223
  call prlevel(0, pt, la)
  if (trim(la) .ne. " Mid. cloud top lvl") stop 50

  pt(10) = 224
  call prlevel(0, pt, la)
  if (trim(la) .ne. " Middle cloud layer") stop 50

  pt(10) = 232
  call prlevel(0, pt, la)
  if (trim(la) .ne. " High cloud bot. lvl") stop 50

  pt(10) = 233
  call prlevel(0, pt, la)
  if (trim(la) .ne. " High cloud top lvl") stop 50

  pt(10) = 234
  call prlevel(0, pt, la)
  if (trim(la) .ne. " High cloud layer") stop 50

  pt(10) = 235
  call prlevel(0, pt, la)
  if (trim(la) .ne. " Ocean Isotherm lvl") stop 50

  pt(10) = 236
  call prlevel(0, pt, la)
  if (trim(la) .ne. " Layer 2-depth below") stop 50

  pt(10) = 237
  call prlevel(0, pt, la)
  if (trim(la) .ne. " Bot. Ocean mix. lyr") stop 50

  pt(10) = 238
  call prlevel(0, pt, la)
  if (trim(la) .ne. " Bot. Ocean iso. lyr") stop 50

  pt(10) = 239
  call prlevel(0, pt, la)
  if (trim(la) .ne. " layer ocean sfc 26C") stop 50

  pt(10) = 240
  call prlevel(0, pt, la)
  if (trim(la) .ne. " Ocean Mixed Layer") stop 50

  pt(10) = 241
  call prlevel(0, pt, la)
  if (trim(la) .ne. " Order Seq. Of Data") stop 50

  pt(10) = 242
  call prlevel(0, pt, la)
  if (trim(la) .ne. " Con. cloud bot. lvl") stop 50

  pt(10) = 243
  call prlevel(0, pt, la)
  if (trim(la) .ne. " Con. cloud top lvl") stop 50

  pt(10) = 244
  call prlevel(0, pt, la)
  if (trim(la) .ne. " Conv. cloud layer") stop 50

  pt(10) = 245
  call prlevel(0, pt, la)
  if (trim(la) .ne. " Lowest lvl wet bulb") stop 50

  pt(10) = 246
  call prlevel(0, pt, la)
  if (trim(la) .ne. " Max. equi. potential") stop 50

  pt(10) = 247
  call prlevel(0, pt, la)
  if (trim(la) .ne. " Equilibrium level") stop 50

  pt(10) = 248
  call prlevel(0, pt, la)
  if (trim(la) .ne. " Shallow con. cld bl") stop 50

  pt(10) = 249
  call prlevel(0, pt, la)
  if (trim(la) .ne. " Shallow con. cld tl") stop 50

  pt(10) = 251
  call prlevel(0, pt, la)
  if (trim(la) .ne. " Deep conv. cld bl") stop 50

  pt(10) = 252
  call prlevel(0, pt, la)
  if (trim(la) .ne. " Deep conv. cld tl") stop 50

  pt(10) = 253
  call prlevel(0, pt, la)
  if (trim(la) .ne. " Lowest bot. lvl sup") stop 50

  pt(10) = 254
  call prlevel(0, pt, la)
  if (trim(la) .ne. " highest top lvl sup") stop 50

  pt(10) = 999
  call prlevel(0, pt, la)
!  print *, '/', trim(la), '/'
  if (trim(la) .ne. "  999 (Unknown Lvl)") stop 50

  ! Template 8 with various options.
  call prlevel(8, pt_8_0, la)
  if (trim(la) .ne. " Surface") stop 40
  call prvtime(8, pt_8_0, s1_0, ta)
  if (trim(ta) .ne.  "(0 -1 hr) valid  0 hour after 2022111719:00:00 to 2022111720:00:00") stop 41

  print *, 'SUCCESS!'
  
end program test_degrib2_int
