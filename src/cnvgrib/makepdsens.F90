!> @file
!> @brief Create GRIB1 NCEP Ensemble PDS extension information from a
!> GRIB2 Product Definition Template.
!> @author Stephen Gilbert @date 2003-06-12

!> This routine creates the GRIB1 NCEP Ensemble PDS extension
!> information from appropriate information from a GRIB2 Product
!> Definition Template.
!>
!> @note  Use pds2pdtens for ensemble related PDS.
!>
!> ### Program History Log
!> Date | Programmer | Comments
!> -----|------------|---------
!> 2003-06-12 | Gilbert | Initial
!> 2007-05-14 | Boi Vuong | Corrected scale factor probabilities
!> 2010-07-26 | Boi Vuong | Added two type of ensemblers (4 and 192)
!>
!> @param[in] ipdsnum GRIB2 Product Definition Template Number
!> @param[in] ipdstmpl GRIB2 Product Definition Template entries for
!> PDT 4.ipdsnum
!> @param[inout] kpds GRIB1 PDS info as specified in W3FI63.
!> - 1 id of center
!> - 2 generating process id number
!> - 3 grid definition
!> - 4 gds/bms flag (right adj copy of octet 8)
!> - 5 indicator of parameter
!> - 6 type of level
!> - 7 height/pressure , etc of level
!> - 8 year including (century-1)
!> - 9 month of year
!> - 10 day of month
!> - 11 hour of day
!> - 12 minute of hour
!> - 13 indicator of forecast time unit
!> - 14 time range 1
!> - 15 time range 2
!> - 16 time range flag
!> - 17 number included in average
!> - 18 version nr of grib specification
!> - 19 version nr of parameter table
!> - 20 nr missing from average/accumulation
!> - 21 century of reference time of data
!> - 22 units decimal scale factor
!> - 23 subcenter number
!> @param[out] kens Ensemble identification for PDS octets 41-45
!> @param[out] kprob Ensemble probability info for PDS octets 46  47
!> @param[out] xprob Ensemble probability info for PDS octets 48-55
!> @param[out] kclust Ensemble cluster info for PDS octets 61-76
!> @param[out] kmembr Ensemble membership info for PDS octest 77-86
!> @param[out] iret Error return value:
!> - 0 Successful
!> - 2 Unrecognized GRIB2 PDT 4.ipdsnum
!>
!> @author Stephen Gilbert @date 2003-06-12
subroutine makepdsens(ipdsnum,ipdstmpl,kpds,kens,kprob, &
     xprob,kclust,kmembr,iret)
  use params

  integer,intent(in) :: ipdstmpl(*)
  integer,intent(in) :: ipdsnum
  integer,intent(inout) :: kpds(*)
  integer,intent(out) :: kens(5),kprob(2)
  integer,intent(out) :: kclust(16),kmembr(80)
  real,intent(out) :: xprob(2)
  integer,intent(out) :: iret

  iret=0
  kpds(23)=2          !  subcenter = ensemble

  kens(1:5)=0
  kprob(1:2)=0
  xprob(1:2)=0.
  kclust(1:16)=0
  kmembr(1:80)=0
  !
  !  Individual Ensemble Fcst
  !
  if (ipdsnum.eq.1.OR.ipdsnum.eq.11) then
     kens(1)=1
     selectcase (ipdstmpl(16))
     case(0)
        kens(2)=1
        kens(3)=1
     case(1)
        kens(2)=1
        kens(3)=2
     case(2)
        kens(2)=2
        kens(3)=ipdstmpl(17)
     case(3)
        kens(2)=3
        kens(3)=ipdstmpl(17)
     case(4)
        kens(2)=3
        kens(3)=ipdstmpl(17)
     case(192)
        kens(2)=3
        kens(3)=ipdstmpl(17)
     end select
     kens(4)=1
     kens(5)=255
     !
     !  Probability Fcst
     !
  elseif (ipdsnum.eq.5.OR.ipdsnum.eq.9) then
     kens(1)=1
     kens(2)=5
     kens(3)=0
     kens(4)=0
     kens(5)=255
     kprob(1)=kpds(5)
     kpds(5)=191
     kprob(2)=ipdstmpl(18)+1
     if (kprob(2).eq.1) then
        rscale=10.**ipdstmpl(19)
        xprob(1)=real(ipdstmpl(20))/rscale
        xprob(2)=0.0
     elseif (kprob(2).eq.2) then
        xprob(1)=0.0
        rscale=10.**ipdstmpl(21)
        xprob(2)=real(ipdstmpl(22))/rscale
     elseif (kprob(2).eq.3) then
        rscale=10.**ipdstmpl(19)
        xprob(1)=real(ipdstmpl(20))/rscale
        rscale=10.**ipdstmpl(21)
        xprob(2)=real(ipdstmpl(22))/rscale
     endif
     kclust(1)=ipdstmpl(17)
     !
     !  Derived Ensemble Fcst
     !
  elseif (ipdsnum.eq.2.OR.ipdsnum.eq.12) then
     kens(1)=1
     kens(2)=5
     kens(3)=0
     selectcase (ipdstmpl(16))
     case(0)
        kens(4)=1
     case(1)
        kens(4)=2
     case(2)
        kens(4)=11
     case(3)
        kens(4)=12
     end select
     !kens(5)=89
     kens(5)=0
     kclust(1)=ipdstmpl(17)
  else
     print *,'makepdsens: Don:t know GRIB2 PDT 4.',ipdsnum
     iret=2
  endif

  return
end subroutine makepdsens
