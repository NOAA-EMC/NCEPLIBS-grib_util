!!> @file
!>
!> @author IREDELL @date 94-04-01
!
!>  PACK AND WRITE A GRIB MESSAGE.
!>  THIS SUBPROGRAM IS NEARLY THE INVERSE OF GETGBE.
!>
!> PROGRAM HISTORY LOG:
!>   94-04-01  IREDELL
!>   95-10-31  IREDELL     REMOVED SAVES AND PRINTS
!>   97-02-11  Y.ZHU       INCLUDED PROBABILITY AND CLUSTER ARGUMENTS
!> 2002-03-18  GILBERT     MODIFIED FROM PUTGBEX TO ACCOUNT FOR
!>                         BINARY SCALE FACTORS.
!>
!> USAGE:    CALL PUTGBEXN(LUGB,KF,KPDS,KGDS,KENS,
!>    &                   KPROB,XPROB,KCLUST,KMEMBR,IBS,NBITS,LB,F,IRET)
!>   INPUT ARGUMENTS:
!>     LUGB         INTEGER UNIT OF THE UNBLOCKED GRIB DATA FILE
!>     KF           INTEGER NUMBER OF DATA POINTS
!>     KPDS         INTEGER (200) PDS PARAMETERS
!>          (1)   - ID OF CENTER
!>          (2)   - GENERATING PROCESS ID NUMBER
!>          (3)   - GRID DEFINITION
!>          (4)   - GDS/BMS FLAG (RIGHT ADJ COPY OF OCTET 8)
!>          (5)   - INDICATOR OF PARAMETER
!>          (6)   - TYPE OF LEVEL
!>          (7)   - HEIGHT/PRESSURE , ETC OF LEVEL
!>          (8)   - YEAR INCLUDING (CENTURY-1)
!>          (9)   - MONTH OF YEAR
!>          (10)  - DAY OF MONTH
!>          (11)  - HOUR OF DAY
!>          (12)  - MINUTE OF HOUR
!>          (13)  - INDICATOR OF FORECAST TIME UNIT
!>          (14)  - TIME RANGE 1
!>          (15)  - TIME RANGE 2
!>          (16)  - TIME RANGE FLAG
!>          (17)  - NUMBER INCLUDED IN AVERAGE
!>          (18)  - VERSION NR OF GRIB SPECIFICATION
!>          (19)  - VERSION NR OF PARAMETER TABLE
!>          (20)  - NR MISSING FROM AVERAGE/ACCUMULATION
!>          (21)  - CENTURY OF REFERENCE TIME OF DATA
!>          (22)  - UNITS DECIMAL SCALE FACTOR
!>          (23)  - SUBCENTER NUMBER
!>          (24)  - PDS BYTE 29, FOR NMC ENSEMBLE PRODUCTS
!>                  128 IF FORECAST FIELD ERROR
!>                   64 IF BIAS CORRECTED FCST FIELD
!>                   32 IF SMOOTHED FIELD
!>                  WARNING: CAN BE COMBINATION OF MORE THAN 1
!>          (25)  - PDS BYTE 30, NOT USED
!>     KGDS         INTEGER (200) GDS PARAMETERS
!>          (1)   - DATA REPRESENTATION TYPE
!>          (19)  - NUMBER OF VERTICAL COORDINATE PARAMETERS
!>          (20)  - OCTET NUMBER OF THE LIST OF VERTICAL COORDINATE
!>                  PARAMETERS
!>                  OR
!>                  OCTET NUMBER OF THE LIST OF NUMBERS OF POINTS
!>                  IN EACH ROW
!>                  OR
!>                  255 IF NEITHER ARE PRESENT
!>          (21)  - FOR GRIDS WITH PL, NUMBER OF POINTS IN GRID
!>          (22)  - NUMBER OF WORDS IN EACH ROW
!>       LATITUDE/LONGITUDE GRIDS
!>          (2)   - N(I) NR POINTS ON LATITUDE CIRCLE
!>          (3)   - N(J) NR POINTS ON LONGITUDE MERIDIAN
!>          (4)   - LA(1) LATITUDE OF ORIGIN
!>          (5)   - LO(1) LONGITUDE OF ORIGIN
!>          (6)   - RESOLUTION FLAG (RIGHT ADJ COPY OF OCTET 17)
!>          (7)   - LA(2) LATITUDE OF EXTREME POINT
!>          (8)   - LO(2) LONGITUDE OF EXTREME POINT
!>          (9)   - DI LONGITUDINAL DIRECTION OF INCREMENT
!>          (10)  - DJ LATITUDINAL DIRECTION INCREMENT
!>          (11)  - SCANNING MODE FLAG (RIGHT ADJ COPY OF OCTET 28)
!>       GAUSSIAN  GRIDS
!>          (2)   - N(I) NR POINTS ON LATITUDE CIRCLE
!>          (3)   - N(J) NR POINTS ON LONGITUDE MERIDIAN
!>          (4)   - LA(1) LATITUDE OF ORIGIN
!>          (5)   - LO(1) LONGITUDE OF ORIGIN
!>          (6)   - RESOLUTION FLAG  (RIGHT ADJ COPY OF OCTET 17)
!>          (7)   - LA(2) LATITUDE OF EXTREME POINT
!>          (8)   - LO(2) LONGITUDE OF EXTREME POINT
!>          (9)   - DI LONGITUDINAL DIRECTION OF INCREMENT
!>          (10)  - N - NR OF CIRCLES POLE TO EQUATOR
!>          (11)  - SCANNING MODE FLAG (RIGHT ADJ COPY OF OCTET 28)
!>          (12)  - NV - NR OF VERT COORD PARAMETERS
!>          (13)  - PV - OCTET NR OF LIST OF VERT COORD PARAMETERS
!>                             OR
!>                  PL - LOCATION OF THE LIST OF NUMBERS OF POINTS IN
!>                       EACH ROW (IF NO VERT COORD PARAMETERS
!>                       ARE PRESENT
!>                             OR
!>                  255 IF NEITHER ARE PRESENT
!>       POLAR STEREOGRAPHIC GRIDS
!>          (2)   - N(I) NR POINTS ALONG LAT CIRCLE
!>          (3)   - N(J) NR POINTS ALONG LON CIRCLE
!>          (4)   - LA(1) LATITUDE OF ORIGIN
!>          (5)   - LO(1) LONGITUDE OF ORIGIN
!>          (6)   - RESOLUTION FLAG  (RIGHT ADJ COPY OF OCTET 17)
!>          (7)   - LOV GRID ORIENTATION
!>          (8)   - DX - X DIRECTION INCREMENT
!>          (9)   - DY - Y DIRECTION INCREMENT
!>          (10)  - PROJECTION CENTER FLAG
!>          (11)  - SCANNING MODE (RIGHT ADJ COPY OF OCTET 28)
!>       SPHERICAL HARMONIC COEFFICIENTS
!>          (2)   - J PENTAGONAL RESOLUTION PARAMETER
!>          (3)   - K      "          "         "
!>          (4)   - M      "          "         "
!>          (5)   - REPRESENTATION TYPE
!>          (6)   - COEFFICIENT STORAGE MODE
!>       MERCATOR GRIDS
!>          (2)   - N(I) NR POINTS ON LATITUDE CIRCLE
!>          (3)   - N(J) NR POINTS ON LONGITUDE MERIDIAN
!>          (4)   - LA(1) LATITUDE OF ORIGIN
!>          (5)   - LO(1) LONGITUDE OF ORIGIN
!>          (6)   - RESOLUTION FLAG (RIGHT ADJ COPY OF OCTET 17)
!>          (7)   - LA(2) LATITUDE OF LAST GRID POINT
!>          (8)   - LO(2) LONGITUDE OF LAST GRID POINT
!>          (9)   - LATIT - LATITUDE OF PROJECTION INTERSECTION
!>          (10)  - RESERVED
!>          (11)  - SCANNING MODE FLAG (RIGHT ADJ COPY OF OCTET 28)
!>          (12)  - LONGITUDINAL DIR GRID LENGTH
!>          (13)  - LATITUDINAL DIR GRID LENGTH
!>       LAMBERT CONFORMAL GRIDS
!>          (2)   - NX NR POINTS ALONG X-AXIS
!>          (3)   - NY NR POINTS ALONG Y-AXIS
!>          (4)   - LA1 LAT OF ORIGIN (LOWER LEFT)
!>          (5)   - LO1 LON OF ORIGIN (LOWER LEFT)
!>          (6)   - RESOLUTION (RIGHT ADJ COPY OF OCTET 17)
!>          (7)   - LOV - ORIENTATION OF GRID
!>          (8)   - DX - X-DIR INCREMENT
!>          (9)   - DY - Y-DIR INCREMENT
!>          (10)  - PROJECTION CENTER FLAG
!>          (11)  - SCANNING MODE FLAG (RIGHT ADJ COPY OF OCTET 28)
!>          (12)  - LATIN 1 - FIRST LAT FROM POLE OF SECANT CONE INTER
!>          (13)  - LATIN 2 - SECOND LAT FROM POLE OF SECANT CONE INTER
!>     KENS         INTEGER (200) ENSEMBLE PDS PARMS
!>          (1)   - APPLICATION IDENTIFIER
!>          (2)   - ENSEMBLE TYPE
!>          (3)   - ENSEMBLE IDENTIFIER
!>          (4)   - PRODUCT IDENTIFIER
!>          (5)   - SMOOTHING FLAG
!>     KPROB        INTEGER (2) PROBABILITY ENSEMBLE PARMS
!>     XPROB        REAL    (2) PROBABILITY ENSEMBLE PARMS
!>     KCLUST       INTEGER (16) CLUSTER ENSEMBLE PARMS
!>     KMEMBR       INTEGER (8) CLUSTER ENSEMBLE PARMS
!>     IBS          INTEGER BINARY SCALE FACTOR (0 TO IGNORE)
!>     NBITS        INTEGER NUMBER OF BITS IN WHICH TO PACK (0 TO IGNORE)
!>     LB           LOGICAL*1 (KF) BITMAP IF PRESENT
!>     F            REAL (KF) DATA
!>   OUTPUT ARGUMENTS:
!>     IRET         INTEGER RETURN CODE
!>                    0      ALL OK
!>                    OTHER  W3FI72 GRIB PACKER RETURN CODE
!>
!> SUBPROGRAMS CALLED:
!>   R63W72         MAP W3FI63 PARAMETERS ONTO W3FI72 PARAMETERS
!>   GETBIT         GET NUMBER OF BITS AND ROUND DATA
!>   W3FI72         PACK GRIB
!>   WRYTE          WRITE DATA
!>
!> REMARKS: SUBPROGRAM CAN BE CALLED FROM A MULTIPROCESSING ENVIRONMENT.
!>   DO NOT ENGAGE THE SAME LOGICAL UNIT FROM MORE THAN ONE PROCESSOR.
!>
!> ATTRIBUTES:
!>   LANGUAGE: FORTRAN 77
!>   MACHINE:  CRAY, WORKSTATIONS
!>
!>
      SUBROUTINE PUTGBEXN(LUGB,KF,KPDS,KGDS,KENS,
     &                   KPROB,XPROB,KCLUST,KMEMBR,IBS,NBITS,LB,F,IRET)

      INTEGER KPDS(200),KGDS(200),KENS(200)
      INTEGER KPROB(2),KCLUST(16),KMEMBR(80)
      REAL XPROB(2)
      LOGICAL*1 LB(KF)
      REAL F(KF)
!      PARAMETER(MAXBIT=16)
      PARAMETER(MAXBIT=24)
      INTEGER IBM(KF),IPDS(200),IGDS(200),IBDS(200)
      CHARACTER PDS(400),GRIB(1000+KF*(MAXBIT+1)/8)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  GET W3FI72 PARAMETERS
      !print *,'SAGT: start putgbexn'
      CALL R63W72(KPDS,KGDS,IPDS,IGDS)
      IBDS=0
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
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
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
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
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  CREATE PRODUCT DEFINITION SECTION
      CALL W3FI68(IPDS,PDS)
      IF(IPDS(24).EQ.2) THEN
        ILAST=45
        IF ( IPDS(8).EQ.191.OR.IPDS(8).EQ.192 ) ILAST=55
        IF ( KENS(2).EQ.5) ILAST=76
        IF ( KENS(2).EQ.5) ILAST=86
        IF ( KENS(2).EQ.4) ILAST=86
        CALL PDSENS(KENS,KPROB,XPROB,KCLUST,KMEMBR,ILAST,PDS)
      ENDIF
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  PACK AND WRITE GRIB DATA
      igflag=1
      igrid=kpds(3)
      if ( igrid.ne.255 ) igflag=0
      !print *,minval(f(1:kf)),maxval(f(1:kf))
      !print *,nbit,kf
      !print *,(ipds(j),j=1,28)
      !write(6,fmt='(28z2)') (pds(j),j=1,28)
      !print *,(kgds(j),j=1,28)
      !print *,(igds(j),j=1,28)
      icomp=0
      CALL W3FI72(0,F,0,NBIT,1,IPDS,PDS,
     &            igflag,igrid,IGDS,ICOMP,0,IBM,KF,IBDS,
     &            KFO,GRIB,LGRIB,IRET)
      IF(IRET.EQ.0) CALL WRYTE(LUGB,LGRIB,GRIB)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      RETURN
      END


