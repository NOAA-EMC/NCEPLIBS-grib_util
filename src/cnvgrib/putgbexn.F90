!> @file
!> @brief Pack and write a grib message.
!> @author Mark Iredell @date 94-04-01

!> Pack and write a grib message. This subprogram is nearly the inverse
!> of getgbe.
!>
!> @note Subprogram can be called from a multiprocessing environment.
!> Do not engage the same logical unit from more than one processor.
!>
!> ### Program History Log
!> Date | Programmer | Comments
!> -----|------------|---------
!> 94-04-01 | Iredell | Initial.
!> 95-10-31 | Iredell | Removed saves and prints
!> 97-02-11 | Y. Zhu | Included probability and cluster arguments
!> 2002-03-18 | Gilbert | Modified from putgbex to account for binary scale factors.
!>
!> @param[in] lugb teger unit of the unblocked grib data file
!> @param[in] kf teger number of data points
!> @param[in] kpds teger (200) pds parameters
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
!> - 24 pds byte 29, for nmc ensemble products, 128 if forecast field
!> error, 64 if bias corrected fcst field, 32 if smoothed field,
!> warning: can be combination of more than 1.
!> - 25 pds byte 30, not used
!> @param[in] kgds teger (200) gds parameters
!> - 1 data representation type
!> - 19 number of vertical coordinate parameters
!> - 20 octet number of the list of vertical coordinate parameters or
!> octet number of the list of numbers of points in each row or 255 if
!> neither are present.
!> - 21 for grids with pl, number of points in grid
!> - 22 number of words in each row latitude/longitude grids
!> - 2 n(i) nr points on latitude circle
!> - 3 n(j) nr points on longitude meridian
!> - 4 la(1) latitude of origin
!> - 5 lo(1) longitude of origin
!> - 6 resolution flag (right adj copy of octet 17)
!> - 7 la(2) latitude of extreme point
!> - 8 lo(2) longitude of extreme point
!> - 9 di longitudinal direction of increment
!> - 10 dj latitudinal direction increment
!> - 11 scanning mode flag (right adj copy of octet 28)
!> Gaussian  grids:
!> - 2 n(i) nr points on latitude circle
!> - 3 n(j) nr points on longitude meridian
!> - 4 la(1) latitude of origin
!> - 5 lo(1) longitude of origin
!> - 6 resolution flag  (right adj copy of octet 17)
!> - 7 la(2) latitude of extreme point
!> - 8 lo(2) longitude of extreme point
!> - 9 di longitudinal direction of increment
!> - 10 n - nr of circles pole to equator
!> - 11 scanning mode flag (right adj copy of octet 28)
!> - 12 nv - nr of vert coord parameters
!> - 13 pv - octet nr of list of vert coord parameters or pl - location
!> of the list of numbers of points in each row (if no vert coord
!> parameters are present or 255 if neither are present
!> Polar Stereographic grids:
!> - 2 n(i) nr points along lat circle
!> - 3 n(j) nr points along lon circle
!> - 4 la(1) latitude of origin
!> - 5 lo(1) longitude of origin
!> - 6 resolution flag  (right adj copy of octet 17)
!> - 7 lov grid orientation
!> - 8 dx - x direction increment
!> - 9 dy - y direction increment
!> - 10 projection center flag
!> - 11 scanning mode (right adj copy of octet 28)
!> Spherical Harmonic Coefficients:
!> - 2 j pentagonal resolution parameter
!> - 3 k pentagonal resolution parameter
!> - 4 m pentagonal resolution parameter
!> - 5 representation type
!> - 6 coefficient storage mode
!> Mercator grids:
!> - 2 n(i) nr points on latitude circle
!> - 3 n(j) nr points on longitude meridian
!> - 4 la(1) latitude of origin
!> - 5 lo(1) longitude of origin
!> - 6 resolution flag (right adj copy of octet 17)
!> - 7 la(2) latitude of last grid point
!> - 8 lo(2) longitude of last grid point
!> - 9 latit - latitude of projection intersection
!> - 10 reserved
!> - 11 scanning mode flag (right adj copy of octet 28)
!> - 12 longitudinal dir grid length
!> - 13 latitudinal dir grid length
!> Lambert Conformal Grids:
!> - 2 nx nr points along x-axis
!> - 3 ny nr points along y-axis
!> - 4 la1 lat of origin (lower left)
!> - 5 lo1 lon of origin (lower left)
!> - 6 resolution (right adj copy of octet 17)
!> - 7 lov - orientation of grid
!> - 8 dx - x-dir increment
!> - 9 dy - y-dir increment
!> - 10 projection center flag
!> - 11 scanning mode flag (right adj copy of octet 28)
!> - 12 latin 1 - first lat from pole of secant cone inter
!> - 13 latin 2 - second lat from pole of secant cone inter
!> @param[in] kens teger (200) ensemble pds parms
!> - 1 application identifier
!> - 2 ensemble type
!> - 3 ensemble identifier
!> - 4 product identifier
!> - 5 smoothing flag
!> @param[in] kprob teger (2) probability ensemble parms
!> @param[in] xprob al    (2) probability ensemble parms
!> @param[in] kclust teger (16) cluster ensemble parms
!> @param[in] kmembr teger (8) cluster ensemble parms
!> @param[in] ibs teger binary scale factor (0 to ignore)
!> @param[in] nbits teger number of bits in which to pack (0 to ignore)
!> @param[in] lb gical*1 (kf) bitmap if present
!> @param[in] f al (kf) data
!> @param[out] iret teger return code
!> - 0 Success
!> - Other W3FI72 GRIB packer return code
!>
!> @author Mark Iredell @date 94-04-01
SUBROUTINE PUTGBEXN(LUGB,KF,KPDS,KGDS,KENS, &
     KPROB,XPROB,KCLUST,KMEMBR,IBS,NBITS,LB,F,IRET)

  INTEGER KPDS(200),KGDS(200),KENS(200)
  INTEGER KPROB(2),KCLUST(16),KMEMBR(80)
  REAL XPROB(2)
  LOGICAL*1 LB(KF)
  REAL F(KF)
  !      PARAMETER(MAXBIT=16)
  PARAMETER(MAXBIT=24)
  INTEGER IBM(KF),IPDS(200),IGDS(200),IBDS(200)
  CHARACTER PDS(400),GRIB(1000+KF*(MAXBIT+1)/8)

  !  GET W3FI72 PARAMETERS
  !print *,'SAGT: start putgbexn'
  CALL R63W72(KPDS,KGDS,IPDS,IGDS)
  IBDS=0

  !  COUNT VALID DATA
  KBM=KF
  IF(IPDS(7).NE.0) THEN
     KBM=0
     DO I=1,KF
        IF(LB(I)) THEN
           IBM(I)=1
           KBM=KBM+1
        ELSE
           IBM(I)=0
        ENDIF
     ENDDO
     IF(KBM.EQ.KF) IPDS(7)=0
  ENDIF

  !  GET NUMBER OF BITS AND ROUND DATA
  IF(NBITS.GT.0) THEN
     NBIT=NBITS
  ELSE
     IF(KBM.EQ.0) THEN
        DO I=1,KF
           F(I)=0.
        ENDDO
        NBIT=0
     ELSE
        !print *,'SAGT:',IPDS(7),IBS,IPDS(25),KF
        !print *,'SAGT:',count(ibm.eq.0),count(ibm.eq.1)
        CALL SETBIT(IPDS(7),-IBS,IPDS(25),KF,IBM,F,FMIN,FMAX,NBIT)
        NBIT=MIN(NBIT,MAXBIT)
     ENDIF
  ENDIF

  !  CREATE PRODUCT DEFINITION SECTION
  CALL W3FI68(IPDS,PDS)
  IF(IPDS(24).EQ.2) THEN
     ILAST=45
     IF (IPDS(8).EQ.191.OR.IPDS(8).EQ.192) ILAST=55
     IF (KENS(2).EQ.5) ILAST=76
     IF (KENS(2).EQ.5) ILAST=86
     IF (KENS(2).EQ.4) ILAST=86
     CALL PDSENS(KENS,KPROB,XPROB,KCLUST,KMEMBR,ILAST,PDS)
  ENDIF

  !  PACK AND WRITE GRIB DATA
  igflag=1
  igrid=kpds(3)
  if (igrid.ne.255) igflag=0
  !print *,minval(f(1:kf)),maxval(f(1:kf))
  !print *,nbit,kf
  !print *,(ipds(j),j=1,28)
  !write(6,fmt='(28z2)') (pds(j),j=1,28)
  !print *,(kgds(j),j=1,28)
  !print *,(igds(j),j=1,28)
  icomp=0
  CALL W3FI72(0,F,0,NBIT,1,IPDS,PDS, &
       igflag,igrid,IGDS,ICOMP,0,IBM,KF,IBDS, &
       KFO,GRIB,LGRIB,IRET)
  IF(IRET.EQ.0) CALL WRYTE(LUGB,LGRIB,GRIB)

  RETURN
END SUBROUTINE PUTGBEXN
