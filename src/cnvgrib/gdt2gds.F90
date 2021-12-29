!> @file
!> @brief Convert grid information from a GRIB2 Grid Description
!> Section to GRIB1 GDS info.
!> @author Stephen Gilbert @date 2003-06-17

!> This routine converts grid information from a GRIB2 Grid Description
!> Section as well as its Grid Definition Template to GRIB1 GDS info. In
!> addition, a check is made to determine if the grid is an NCEP
!> predefined grid.
!>
!> ### Program History Log
!> Date | Programmer | Comments
!> -----|------------|---------
!> 2003-06-17 | Gilbert | Initial.
!> 2004-04-27 | Gilbert | Added support for gaussian grids.
!> 2007-04-16 | Vuong | Added Curvilinear Orthogonal grids.
!> 2007-05-29 | Vuong | Added Rotate Lat/Lon E-grid (203)
!>
!> @param[in] igds Contains information read from the appropriate GRIB
!> Grid Definition Section 3 for the field being returned. Must be
!> dimensioned >= 5.
!> - igds(1) Source of grid definition (see Code Table 3.0)
!> - igds(2) Number of grid points in the defined grid.
!> - igds(3) Number of octets needed for each additional grid points
!> definition. Used to define number of points in each row (or column)
!> for non-regular grids. = 0, if using regular grid.
!> - igds(4) Interpretation of list for optional points
!> definition. (Code Table 3.11).
!> - igds(5) Grid Definition Template Number (Code Table 3.1)
!> @param[in] igdstmpl Grid Definition Template values for GDT 3.igds(5)
!> @param[in] idefnum The number of entries in array
!> ideflist. i.e. number of rows (or columns) for which optional grid
!> points are defined.
!> @param[in] ideflist Optional integer array containing the number of
!> grid points contained in each row (or column).
!> @param[out] kgds GRIB1 GDS as described in w3fi63 format.
!> @param[out] igrid NCEP predefined GRIB1 grid number
!>                set to 255, if not NCEP grid
!> @param[out] iret Error return value:
!> - 0 Successful
!> - 1 Unrecognized GRIB2 GDT number 3.igds(5).
!>
!> @author Stephen Gilbert @date 2003-06-17 &
subroutine gdt2gds(igds,igdstmpl,idefnum,ideflist,kgds, &
     igrid,iret)

  !
  integer,intent(in) :: idefnum
  integer,intent(in) :: igds(*),igdstmpl(*),ideflist(*)
  integer,intent(out) :: kgds(*),igrid,iret

  integer :: kgds72(200),kgds71(200),idum(200),jdum(200)

  iret=0
  if (igds(5).eq.0) then       !  Lat/Lon grid
     kgds(1)=0
     kgds(2)=igdstmpl(8)            ! Ni
     kgds(3)=igdstmpl(9)            ! Nj
     kgds(4)=igdstmpl(12)/1000      ! Lat of 1st grid point
     kgds(5)=igdstmpl(13)/1000      ! Long of 1st grid point
     kgds(6)=0                      ! resolution and component flags
     if (igdstmpl(1)==2) kgds(6)=64
     if (btest(igdstmpl(14),4).OR.btest(igdstmpl(14),5))  &
          kgds(6)=kgds(6)+128
     if (btest(igdstmpl(14),3)) kgds(6)=kgds(6)+8
     kgds(7)=igdstmpl(15)/1000      ! Lat of last grid point
     kgds(8)=igdstmpl(16)/1000      ! Long of last grid point
     kgds(9)=igdstmpl(17)/1000      ! Di
     kgds(10)=igdstmpl(18)/1000     ! Dj
     kgds(11)=igdstmpl(19)          ! Scanning mode
     kgds(12)=0
     kgds(13)=0
     kgds(14)=0
     kgds(15)=0
     kgds(16)=0
     kgds(17)=0
     kgds(18)=0
     kgds(19)=0
     kgds(20)=255
     kgds(21)=0
     kgds(22)=0
     !
     !  Process irreg grid stuff, if necessary
     !
     if (idefnum.ne.0) then
        if (igdstmpl(8).eq.-1) then
           kgds(2)=65535
           kgds(9)=65535
        endif
        if (igdstmpl(9).eq.-1) then
           kgds(3)=65535
           kgds(10)=65535
        endif
        kgds(19)=0
        kgds(20)=33
        if (kgds(1).eq.1.OR.kgds(1).eq.3) kgds(20)=43
        kgds(21)=igds(2)                   ! num of grid points
        do j=1,idefnum
           kgds(21+j)=ideflist(j)
        enddo
     endif
  elseif (igds(5).eq.10) then       !  Mercator grid
     kgds(1)=1                 ! Grid Definition Template number
     kgds(2)=igdstmpl(8)            ! Ni
     kgds(3)=igdstmpl(9)            ! Nj
     kgds(4)=igdstmpl(10)/1000      ! Lat of 1st grid point
     kgds(5)=igdstmpl(11)/1000      ! Long of 1st grid point
     kgds(6)=0                      ! resolution and component flags
     if (igdstmpl(1)==2) kgds(6)=64
     if (btest(igdstmpl(12),4).OR.btest(igdstmpl(12),5))  &
          kgds(6)=kgds(6)+128
     if (btest(igdstmpl(12),3)) kgds(6)=kgds(6)+8
     kgds(7)=igdstmpl(14)/1000      ! Lat of last grid point
     kgds(8)=igdstmpl(15)/1000      ! Long of last grid point
     kgds(9)=igdstmpl(13)/1000      ! Lat intersects earth
     kgds(10)=0
     kgds(11)=igdstmpl(16)          ! Scanning mode
     kgds(12)=igdstmpl(18)/1000     ! Di
     kgds(13)=igdstmpl(19)/1000     ! Dj
     kgds(14)=0
     kgds(15)=0
     kgds(16)=0
     kgds(17)=0
     kgds(18)=0
     kgds(19)=0
     kgds(20)=255
     kgds(21)=0
     kgds(22)=0
  elseif (igds(5).eq.30) then       ! Lambert Conformal Grid
     kgds(1)=3
     kgds(2)=igdstmpl(8)            ! Nx
     kgds(3)=igdstmpl(9)            ! Ny
     kgds(4)=igdstmpl(10)/1000      ! Lat of 1st grid point
     kgds(5)=igdstmpl(11)/1000      ! Long of 1st grid point
     kgds(6)=0                      ! resolution and component flags
     if (igdstmpl(1)==2) kgds(6)=64
     if (btest(igdstmpl(12),4).OR.btest(igdstmpl(12),5))  &
          kgds(6)=kgds(6)+128
     if (btest(igdstmpl(12),3)) kgds(6)=kgds(6)+8
     kgds(7)=igdstmpl(14)/1000      ! Lon of orientation
     kgds(8)=igdstmpl(15)/1000      ! Dx
     kgds(9)=igdstmpl(16)/1000      ! Dy
     kgds(10)=igdstmpl(17)          ! Projection Center Flag
     kgds(11)=igdstmpl(18)          ! Scanning mode
     kgds(12)=igdstmpl(19)/1000     ! Lat in 1
     kgds(13)=igdstmpl(20)/1000     ! Lat in 2
     kgds(14)=igdstmpl(21)/1000     ! Lat of S. Pole of projection
     kgds(15)=igdstmpl(22)/1000     ! Lon of S. Pole of projection
     kgds(16)=0
     kgds(17)=0
     kgds(18)=0
     kgds(19)=0
     kgds(20)=255
     kgds(21)=0
     kgds(22)=0
  elseif (igds(5).eq.40) then       !  Gaussian Lat/Lon grid
     kgds(1)=4
     kgds(2)=igdstmpl(8)            ! Ni
     kgds(3)=igdstmpl(9)            ! Nj
     kgds(4)=igdstmpl(12)/1000      ! Lat of 1st grid point
     kgds(5)=igdstmpl(13)/1000      ! Long of 1st grid point
     kgds(6)=0                      ! resolution and component flags
     if (igdstmpl(1)==2) kgds(6)=64
     if (btest(igdstmpl(14),4).OR.btest(igdstmpl(14),5))  &
          kgds(6)=kgds(6)+128
     if (btest(igdstmpl(14),3)) kgds(6)=kgds(6)+8
     kgds(7)=igdstmpl(15)/1000      ! Lat of last grid point
     kgds(8)=igdstmpl(16)/1000      ! Long of last grid point
     kgds(9)=igdstmpl(17)/1000      ! Di
     kgds(10)=igdstmpl(18)          ! N - Number of parallels
     kgds(11)=igdstmpl(19)          ! Scanning mode
     kgds(12)=0
     kgds(13)=0
     kgds(14)=0
     kgds(15)=0
     kgds(16)=0
     kgds(17)=0
     kgds(18)=0
     kgds(19)=0
     kgds(20)=255
     kgds(21)=0
     kgds(22)=0
  elseif (igds(5).eq.20) then       ! Polar Stereographic Grid
     kgds(1)=5
     kgds(2)=igdstmpl(8)            ! Nx
     kgds(3)=igdstmpl(9)            ! Ny
     kgds(4)=igdstmpl(10)/1000      ! Lat of 1st grid point
     kgds(5)=igdstmpl(11)/1000      ! Long of 1st grid point
     kgds(6)=0                      ! resolution and component flags
     if (igdstmpl(1)==2) kgds(6)=64
     if (btest(igdstmpl(12),4).OR.btest(igdstmpl(12),5))  &
          kgds(6)=kgds(6)+128
     if (btest(igdstmpl(12),3)) kgds(6)=kgds(6)+8
     kgds(7)=igdstmpl(14)/1000      ! Lon of orientation
     kgds(8)=igdstmpl(15)/1000      ! Dx
     kgds(9)=igdstmpl(16)/1000      ! Dy
     kgds(10)=igdstmpl(17)          ! Projection Center Flag
     kgds(11)=igdstmpl(18)          ! Scanning mode
     kgds(12)=0
     kgds(13)=0
     kgds(14)=0
     kgds(15)=0
     kgds(16)=0
     kgds(17)=0
     kgds(18)=0
     kgds(19)=0
     kgds(20)=255
     kgds(21)=0
     kgds(22)=0
  elseif (igds(5).eq.204) then      ! Curvilinear Orthogonal
     kgds(1)=204
     kgds(2)=igdstmpl(8)            ! Ni
     kgds(3)=igdstmpl(9)            ! Nj
     kgds(4)=0
     kgds(5)=0
     kgds(6)=0                      ! resolution and component flags
     if (igdstmpl(1)==2) kgds(6)=64
     if (btest(igdstmpl(14),4).OR.btest(igdstmpl(14),5)) &
          kgds(6)=kgds(6)+128
     if (btest(igdstmpl(14),3)) kgds(6)=kgds(6)+8
     kgds(7)=0
     kgds(8)=0
     kgds(9)=0
     kgds(10)=0
     kgds(11)=igdstmpl(19)          ! Scanning mode
     kgds(12)=0
     kgds(13)=0
     kgds(14)=0
     kgds(15)=0
     kgds(16)=0
     kgds(17)=0
     kgds(18)=0
     kgds(19)=0
     kgds(20)=255
     kgds(21)=0
     kgds(22)=0
     !
     !  Process irreg grid stuff, if necessary
     !
     if (idefnum.ne.0) then
        if (igdstmpl(8).eq.-1) then
           kgds(2)=65535
           kgds(9)=65535
        endif
        if (igdstmpl(9).eq.-1) then
           kgds(3)=65535
           kgds(10)=65535
        endif
        kgds(19)=0
        kgds(20)=33
        if (kgds(1).eq.1.OR.kgds(1).eq.3) kgds(20)=43
        kgds(21)=igds(2)                   ! num of grid points
        do j=1,idefnum
           kgds(21+j)=ideflist(j)
        enddo
     endif
  elseif (igds(5).eq.32768) then    ! Rotate Lat/Lon grid
     kgds(1)=203                      ! Arakawa Staggerred E/B grid
     kgds(2)=igdstmpl(8)            ! Ni
     kgds(3)=igdstmpl(9)            ! Nj
     kgds(4)=igdstmpl(12)/1000      ! Lat of 1st grid point
     kgds(5)=igdstmpl(13)/1000      ! Lon of 1st grid point
     kgds(6)=0                      ! resolution and component flags
     if (igdstmpl(1)==2) kgds(6)=64
     if (btest(igdstmpl(14),4).OR.btest(igdstmpl(14),5)) &
          kgds(6)=kgds(6)+128
     if (btest(igdstmpl(14),3)) kgds(6)=kgds(6)+8
     kgds(7)=igdstmpl(15)/1000      ! Lat of last grid point
     kgds(8)=igdstmpl(16)/1000      ! Lon of last grid point
     kgds(9)=igdstmpl(17)/1000      ! Di
     kgds(10)=igdstmpl(18)/1000     ! Dj
     kgds(11)=igdstmpl(19)          ! Scanning mode
     kgds(12)=0
     kgds(13)=0
     kgds(14)=0
     kgds(15)=0
     kgds(16)=0
     kgds(17)=0
     kgds(18)=0
     kgds(19)=0
     kgds(20)=255
     kgds(21)=0
     kgds(22)=0
     !
     !  Process irreg grid stuff, if necessary
     !
     if (idefnum.ne.0) then
        if (igdstmpl(8).eq.-1) then
           kgds(2)=65535
           kgds(9)=65535
        endif
        if (igdstmpl(9).eq.-1) then
           kgds(3)=65535
           kgds(10)=65535
        endif
        kgds(19)=0
        kgds(20)=33
        if (kgds(1).eq.1.OR.kgds(1).eq.3) kgds(20)=43
        kgds(21)=igds(2)                   ! num of grid points
        do j=1,idefnum
           kgds(21+j)=ideflist(j)
        enddo
     endif
  elseif (igds(5).eq.32769) then    ! Rotate Lat/Lon grid
     kgds(1)=205                    ! Arakawa Staggerred for Non-E Stagger grid
     kgds(2)=igdstmpl(8)            ! Ni
     kgds(3)=igdstmpl(9)            ! Nj
     kgds(4)=igdstmpl(12)/1000      ! Lat of 1st grid point
     kgds(5)=igdstmpl(13)/1000      ! Lon of 1st grid point
     kgds(6)=0                      ! resolution and component flags
     if (igdstmpl(1)==2) kgds(6)=64
     if (btest(igdstmpl(14),4).OR.btest(igdstmpl(14),5)) &
          kgds(6)=kgds(6)+128
     if (btest(igdstmpl(14),3)) kgds(6)=kgds(6)+8
     kgds(7)=igdstmpl(15)/1000      ! Lat of last grid point
     kgds(8)=igdstmpl(16)/1000      ! Lon of last grid point
     kgds(9)=igdstmpl(17)/1000      ! Di
     kgds(10)=igdstmpl(18)/1000     ! Dj
     kgds(11)=igdstmpl(19)          ! Scanning mode
     kgds(12)=igdstmpl(20)/1000
     kgds(13)=igdstmpl(21)/1000
     kgds(14)=0
     kgds(15)=0
     kgds(16)=0
     kgds(17)=0
     kgds(18)=0
     kgds(19)=0
     kgds(20)=255
     kgds(21)=0
     kgds(22)=0
  else
     Print *,'gdt2gds: Unrecognized GRIB2 GDT = 3.',igds(5)
     iret=1
     kgds(1:22)=0
     return
  endif
  !
  !   Can we determine NCEP grid number ?
  !
  igrid=255
  do j=254,1,-1
     !do j=225,225
     kgds71=0
     kgds72=0
     call w3fi71(j,kgds71,ierr)
     if (ierr.ne.0) cycle
     ! convert W to E for longitudes
     if (kgds71(3).eq.0) then    ! lat/lon
        if (kgds71(7).lt.0) kgds71(7)=360000+kgds71(7)
        if (kgds71(10).lt.0) kgds71(10)=360000+kgds71(10)
     elseif (kgds71(3).eq.1) then    ! mercator
        if (kgds71(7).lt.0) kgds71(7)=360000+kgds71(7)
        if (kgds71(10).lt.0) kgds71(10)=360000+kgds71(10)
     elseif (kgds71(3).eq.3) then     ! lambert conformal
        if (kgds71(7).lt.0) kgds71(7)=360000+kgds71(7)
        if (kgds71(9).lt.0) kgds71(9)=360000+kgds71(9)
        if (kgds71(18).lt.0) kgds71(18)=360000+kgds71(18)
     elseif (kgds71(3).eq.4) then     ! Guassian lat/lon
        if (kgds71(7).lt.0) kgds71(7)=360000+kgds71(7)
        if (kgds71(10).lt.0) kgds71(10)=360000+kgds71(10)
     elseif (kgds71(3).eq.5) then     ! polar stereographic
        if (kgds71(7).lt.0) kgds71(7)=360000+kgds71(7)
        if (kgds71(9).lt.0) kgds71(9)=360000+kgds71(9)
     endif
     call r63w72(idum,kgds,jdum,kgds72)
     if (kgds72(3).eq.3) kgds72(14)=0    ! lambert conformal fix
     if (kgds72(3).eq.1) kgds72(15:18)=0    ! mercator fix
     if (kgds72(3).eq.5) kgds72(14:18)=0    ! polar str fix
     !           print *,' kgds71(',j,')= ', kgds71(1:30)
     !           print *,' kgds72       = ', kgds72(1:30)
     if (all(kgds71.eq.kgds72)) then
        igrid=j
        return
     endif
  enddo

  return
end subroutine gdt2gds
