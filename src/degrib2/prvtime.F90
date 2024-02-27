!> @file
!> @brief Convert date and time from GRIB2 info to string output.
!> @author Stephen Gilbert @date 2010-09-08

!> Convert date and time from GRIB2 info to string output.
!>
!> @param[in] ipdtn Product Definition Template Number ([Code Table
!> 4.0]
!> (https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_table4-0.shtml)).
!> @param[in] ipdtmpl Array of data values for the Product Definition
!> Template specified by ipdtn.
!> @param[in] listsec1 Contains information read from GRIB
!> Identification Section 1. Must be dimensioned >= 13.
!> @param[out] tabbrev Character array that will get the date and time
!> string. Must be of length 100.
!>
!> @author Stephen Gilbert @date 2010-09-08
subroutine prvtime(ipdtn, ipdtmpl, listsec1, tabbrev)
  implicit none

  integer, intent(in) :: ipdtn
  integer, intent(in) :: ipdtmpl(*), listsec1(*)
  character(len = 110), intent(out) :: tabbrev

  character(len = 16) :: reftime, endtime
  character(len = 12) :: tmpval2
  character(len = 12) :: tmpval
  character(len = 10) :: tunit
  integer, dimension(200) :: ipos, ipos2
  integer :: is, itemp, itemp2, iunit, iuni2t2, iunit2, iutpos, iutpos2, j
  
  data ipos  /7*0, 16, 23, 17, 19, 18, 32, 31, 27*0, 17, 20, 0, 0, 22,  &
       25, 43*0, 23, 109*0/

  data ipos2 /7*0, 26, 33, 27, 29, 28, 42, 41, 27*0, 22, 30, 0, 0, 32,  &
       35, 43*0, 33, 109*0/

  tabbrev(1:100) = " "

  ! Determine unit of time range.
  if ((ipdtn .ge. 0 .and. ipdtn .le. 15) .or. ipdtn .eq. 32 &
       .or. ipdtn .eq. 50 .or. ipdtn .eq. 51    &
       .or. ipdtn .eq. 91) then
     iutpos = 8
  elseif (ipdtn .ge. 40 .and. ipdtn .le. 43) then
     iutpos = 9
  elseif (ipdtn .ge. 44 .and. ipdtn .le. 47) then
     iutpos = 14
  elseif (ipdtn .eq. 48) then
     iutpos = 19
  elseif (ipdtn .eq. 52) then
     iutpos = 11
  else
     iutpos = 8
  endif

  ! Determine first unit of time range.
  selectcase(ipdtmpl(iutpos))
  case (0)
     tunit = "minute"
     iunit = 1
  case (1)
     tunit = "hour"
     iunit = 1
  case (2)
     tunit = "day"
     iunit = 1
  case (3)
     tunit = "month"
     iunit = 1
  case (4)
     tunit = "year"
     iunit = 1
  case (10)
     tunit = "hour"
     iunit = 3
  case (11)
     tunit = "hour"
     iunit = 6
  case default
     tunit = "hour"
     iunit = 1
  end select

  ! Determine second unit of time range.
  if (ipdtn .eq. 0) then
     iunit2 = 1
     iutpos2 = 0
  else
     iutpos2 = ipos2(ipdtn)
     if (iutpos2 .gt. 0) then
        selectcase(ipdtmpl(iutpos2))
        case (0)
           iunit2 = 1
        case (1)
           iunit2 = 1
        case (2)
           iunit2 = 1
        case (3)
           iuni2t2 = 1
        case (4)
           iunit2 = 1
        case (10)
           iunit2 = 3
        case (11)
           iunit2 = 6
        case default
           iunit2 = 1
        end select
     endif
  endif

  write(reftime, fmt = '(i4,3i2.2,":",i2.2,":",i2.2)') (listsec1(j), j = 6, 11)
  itemp = abs(ipdtmpl(iutpos + 1)) * iunit
  write(tmpval, '(I0)') itemp
  write(tabbrev, fmt = '("valid at  ", i4)') ipdtmpl(iutpos + 1)

  ! Determine Reference Time: Year, Month, Day, Hour, Minute, Second.
  if ((ipdtn .ge. 0 .and. ipdtn .le. 7) .or. ipdtn .eq. 15 &
       .or. ipdtn .eq. 20 .or. (ipdtn .ge. 30 .and. ipdtn .le. 32) &
       .or. ipdtn .eq. 40 .or. ipdtn .eq. 41 .or. ipdtn .eq. 44 &
       .or. ipdtn .eq. 45 .or. ipdtn .eq. 48 .or.  &
       (ipdtn .ge. 50 .and. ipdtn .le. 52)) then ! Point in time
     tabbrev = "valid  " // trim(tmpval) // " " // trim(tunit) // " after " // reftime
  else
     is = ipos(ipdtn) ! Continuous time interval
     write(endtime, fmt = '(i4,3i2.2,":",i2.2,":",i2.2)') (ipdtmpl(j), j = is, is + 5)
     itemp2 = abs(ipdtmpl(iutpos2 + 1)) * iunit2
     itemp2 = itemp + itemp2
     write(tmpval2, '(I0)') itemp2
     if (ipdtn .eq. 8 .and. ipdtmpl(9) .lt. 0) then
        tabbrev = "(" // trim(tmpval) // " -" &
             // trim(tmpval2) // ") valid  " // trim(tmpval) // &
             " " // trim(tunit) // " before " &
             // reftime // " to " //endtime
     elseif ((ipdtn .ge. 8 .and. ipdtn .le. 14) .or. &
          (ipdtn .ge. 42 .and. ipdtn .le. 47) .or. &
          ipdtn .eq. 91) then ! Continuous time interval
        tabbrev = "(" // trim(tmpval) // " -" &
             // trim(tmpval2) // " hr) valid  " // trim(tmpval) // &
             " " // trim(tunit) // " after " &
             // reftime // " to " // endtime
     endif
  endif

  return
end subroutine prvtime
