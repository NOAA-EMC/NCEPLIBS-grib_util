
!! @file
!> INTERPOLATE VECTOR FIELDS (NEIGHBOR)
!> @author IREDELL @date 96-04-10
!
!> ABSTRACT: THIS SUBPROGRAM PERFORMS NEIGHBOR INTERPOLATION
!>           FROM ANY GRID TO ANY GRID FOR VECTOR FIELDS.
!>           OPTIONS ALLOW CHOOSING THE WIDTH OF THE GRID SQUARE
!>           (IPOPT(1)) TO SEARCH FOR VALID DATA, WHICH DEFAULTS TO 1
!>           (IF IPOPT(1)=-1).  ODD WIDTH SQUARES ARE CENTERED ON
!>           THE NEAREST INPUT GRID POINT; EVEN WIDTH SQUARES ARE
!>           CENTERED ON THE NEAREST FOUR INPUT GRID POINTS.
!>           SQUARES ARE SEARCHED FOR VALID DATA IN A SPIRAL PATTERN
!>           STARTING FROM THE CENTER.  NO SEARCHING IS DONE WHERE
!>           THE OUTPUT GRID IS OUTSIDE THE INPUT GRID.
!>           ONLY HORIZONTAL INTERPOLATION IS PERFORMED.
!>           THE GRIDS ARE DEFINED BY THEIR GRID DESCRIPTION SECTIONS
!>           (PASSED IN INTEGER FORM AS DECODED BY SUBPROGRAM W3FI63).
!>           THE CURRENT CODE RECOGNIZES THE FOLLOWING PROJECTIONS:
!>             (KGDS(1)=000) EQUIDISTANT CYLINDRICAL
!>             (KGDS(1)=001) MERCATOR CYLINDRICAL
!>             (KGDS(1)=003) LAMBERT CONFORMAL CONICAL
!>             (KGDS(1)=004) GAUSSIAN CYLINDRICAL (SPECTRAL NATIVE)
!>             (KGDS(1)=005) POLAR STEREOGRAPHIC AZIMUTHAL
!>             (KGDS(1)=203) ROTATED EQUIDISTANT CYLINDRICAL (E-STAGGER)
!>             (KGDS(1)=205) ROTATED EQUIDISTANT CYLINDRICAL (B-STAGGER)
!>           WHERE KGDS COULD BE EITHER INPUT KGDSI OR OUTPUT KGDSO.
!>           THE INPUT AND OUTPUT VECTORS ARE ROTATED SO THAT THEY ARE
!>           EITHER RESOLVED RELATIVE TO THE DEFINED GRID
!>           IN THE DIRECTION OF INCREASING X AND Y COORDINATES
!>           OR RESOLVED RELATIVE TO EASTERLY AND NORTHERLY DIRECTIONS,
!>           AS DESIGNATED BY THEIR RESPECTIVE GRID DESCRIPTION SECTIONS.
!>           AS AN ADDED BONUS THE NUMBER OF OUTPUT GRID POINTS
!>           AND THEIR LATITUDES AND LONGITUDES ARE ALSO RETURNED
!>           ALONG WITH THEIR VECTOR ROTATION PARAMETERS.
!>           ON THE OTHER HAND, THE OUTPUT CAN BE A SET OF STATION POINTS
!>           IF KGDSO(1)<0, IN WHICH CASE THE NUMBER OF POINTS
!>           AND THEIR LATITUDES AND LONGITUDES MUST BE INPUT 
!>           ALONG WITH THEIR VECTOR ROTATION PARAMETERS.
!>           INPUT BITMAPS WILL BE INTERPOLATED TO OUTPUT BITMAPS.
!>           OUTPUT BITMAPS WILL ALSO BE CREATED WHEN THE OUTPUT GRID
!>           EXTENDS OUTSIDE OF THE DOMAIN OF THE INPUT GRID.
!>           THE OUTPUT FIELD IS SET TO 0 WHERE THE OUTPUT BITMAP IS OFF.
!>        
!> PROGRAM HISTORY LOG:
!>   96-04-10  IREDELL
!> 1999-04-08  IREDELL  SPLIT IJKGDS INTO TWO PIECES
!> 2001-06-18  IREDELL  INCLUDE SPIRAL SEARCH OPTION
!> 2002-01-17  IREDELL  SAVE DATA FROM LAST CALL FOR OPTIMIZATION
!> 2006-01-04  GAYNO    MINOR BUG FIX
!> 2007-10-30  IREDELL  SAVE WEIGHTS AND THREAD FOR PERFORMANCE
!> 2012-06-26  GAYNO    FIX OUT-OF-BOUNDS ERROR.  SEE NCEPLIBS
!>                      TICKET #9.
!> 2015-01-27  GAYNO    REPLACE CALLS TO GDSWIZ WITH NEW MERGED
!>                      ROUTINE GDSWZD.
!>
!> USAGE:    CALL POLATEV2(IPOPT,KGDSI,KGDSO,MI,MO,KM,IBI,LI,UI,VI,
!>    &                    NO,RLAT,RLON,CROT,SROT,IBO,LO,UO,VO,IRET)
!>
!>   INPUT ARGUMENT LIST:
!>     IPOPT    - INTEGER (20) INTERPOLATION OPTIONS
!>                IPOPT(1) IS WIDTH OF SQUARE TO EXAMINE IN SPIRAL SEARCH
!>                (DEFAULTS TO 1 IF IPOPT(1)=-1)
!>     KGDSI    - INTEGER (200) INPUT GDS PARAMETERS AS DECODED BY W3FI63
!>     KGDSO    - INTEGER (200) OUTPUT GDS PARAMETERS
!>                (KGDSO(1)<0 IMPLIES RANDOM STATION POINTS)
!>     MI       - INTEGER SKIP NUMBER BETWEEN INPUT GRID FIELDS IF KM>1
!>                OR DIMENSION OF INPUT GRID FIELDS IF KM=1
!>     MO       - INTEGER SKIP NUMBER BETWEEN OUTPUT GRID FIELDS IF KM>1
!>                OR DIMENSION OF OUTPUT GRID FIELDS IF KM=1
!>     KM       - INTEGER NUMBER OF FIELDS TO INTERPOLATE
!>     IBI      - INTEGER (KM) INPUT BITMAP FLAGS
!>     LI       - LOGICAL*1 (MI,KM) INPUT BITMAPS (IF SOME IBI(K)=1)
!>     UI       - REAL (MI,KM) INPUT U-COMPONENT FIELDS TO INTERPOLATE
!>     VI       - REAL (MI,KM) INPUT V-COMPONENT FIELDS TO INTERPOLATE
!>     NO       - INTEGER NUMBER OF OUTPUT POINTS (ONLY IF KGDSO(1)<0)
!>     RLAT     - REAL (NO) OUTPUT LATITUDES IN DEGREES (IF KGDSO(1)<0)
!>     RLON     - REAL (NO) OUTPUT LONGITUDES IN DEGREES (IF KGDSO(1)<0)
!>     CROT     - REAL (NO) VECTOR ROTATION COSINES (IF KGDSO(1)<0)
!>     SROT     - REAL (NO) VECTOR ROTATION SINES (IF KGDSO(1)<0)
!>                (UGRID=CROT*UEARTH-SROT*VEARTH;
!>                 VGRID=SROT*UEARTH+CROT*VEARTH)
!>
!>   OUTPUT ARGUMENT LIST:
!>     NO       - INTEGER NUMBER OF OUTPUT POINTS (ONLY IF KGDSO(1)>=0)
!>     RLAT     - REAL (MO) OUTPUT LATITUDES IN DEGREES (IF KGDSO(1)>=0)
!>     RLON     - REAL (MO) OUTPUT LONGITUDES IN DEGREES (IF KGDSO(1)>=0)
!>     CROT     - REAL (NO) VECTOR ROTATION COSINES (IF KGDSO(1)>=0)
!>     SROT     - REAL (NO) VECTOR ROTATION SINES (IF KGDSO(1)>=0)
!>                (UGRID=CROT*UEARTH-SROT*VEARTH;
!>                 VGRID=SROT*UEARTH+CROT*VEARTH)
!>     IBO      - INTEGER (KM) OUTPUT BITMAP FLAGS
!>     LO       - LOGICAL*1 (MO,KM) OUTPUT BITMAPS (ALWAYS OUTPUT)
!>     UO       - REAL (MO,KM) OUTPUT U-COMPONENT FIELDS INTERPOLATED
!>     VO       - REAL (MO,KM) OUTPUT V-COMPONENT FIELDS INTERPOLATED
!>     IRET     - INTEGER RETURN CODE
!>                0    SUCCESSFUL INTERPOLATION
!>                2    UNRECOGNIZED INPUT GRID OR NO GRID OVERLAP
!>                3    UNRECOGNIZED OUTPUT GRID
!>
!> SUBPROGRAMS CALLED:
!>   GDSWZD       GRID DESCRIPTION SECTION WIZARD
!>   IJKGDS0      SET UP PARAMETERS FOR IJKGDS1
!>   (IJKGDS1)    RETURN FIELD POSITION FOR A GIVEN GRID POINT
!>   (MOVECT)     MOVE A VECTOR ALONG A GREAT CIRCLE
!>   POLFIXV      MAKE MULTIPLE POLE VECTOR VALUES CONSISTENT
!>
!> ATTRIBUTES:
!>   LANGUAGE: FORTRAN 90
!>
 SUBROUTINE POLATEV2(IPOPT,KGDSI,KGDSO,MI,MO,KM,IBI,LI,UI,VI, &
                     NO,RLAT,RLON,CROT,SROT,IBO,LO,UO,VO,IRET)
! 
 USE GDSWZD_MOD
!
 IMPLICIT NONE
!
 INTEGER,            INTENT(IN   ):: IPOPT(20),KGDSI(200),KGDSO(200)
 INTEGER,            INTENT(IN   ):: IBI(KM),MI,MO,KM
 INTEGER,            INTENT(INOUT):: NO
 INTEGER,            INTENT(  OUT):: IRET, IBO(KM)
!
 LOGICAL*1,          INTENT(IN   ):: LI(MI,KM)
 LOGICAL*1,          INTENT(  OUT):: LO(MO,KM)
!
 REAL,               INTENT(IN   ):: UI(MI,KM),VI(MI,KM)
 REAL,               INTENT(INOUT):: CROT(MO),SROT(MO)
 REAL,               INTENT(INOUT):: RLAT(MO),RLON(MO)
 REAL,               INTENT(  OUT):: UO(MO,KM),VO(MO,KM)
!
 REAL,               PARAMETER    :: FILL=-9999.
!cdz+1
 REAL,               PARAMETER    :: FILL9=9.999000260554009E+020
!
 INTEGER                          :: IJKGDSA(20)
 INTEGER                          :: I1,J1,IXS,JXS,MX
 INTEGER                          :: KXS,KXT,IX,JX,NX
 INTEGER                          :: MSPIRAL,N,K,NK,NV,IJKGDS1
 INTEGER,                     SAVE:: KGDSIX(200)=-1,KGDSOX(200)=-1
 INTEGER,                     SAVE:: NOX=-1,IRETX=-1
 INTEGER,         ALLOCATABLE,SAVE:: NXY(:)
!
 REAL                             :: CX,SX,CM,SM,UROT,VROT
 REAL                             :: XPTS(MO),YPTS(MO)
 REAL                             :: CROI(MI),SROI(MI)
 REAL                             :: XPTI(MI),YPTI(MI),RLOI(MI),RLAI(MI)
 REAL,            ALLOCATABLE,SAVE:: RLATX(:),RLONX(:),XPTSX(:),YPTSX(:)
 REAL,            ALLOCATABLE,SAVE:: CROTX(:),SROTX(:),CXY(:),SXY(:)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  SET PARAMETERS
 IRET=0
 MSPIRAL=MAX(IPOPT(1),1)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  SAVE OR SKIP WEIGHT COMPUTATION
 IF(IRET.EQ.0.AND.(KGDSO(1).LT.0.OR.ANY(KGDSI.NE.KGDSIX).OR.ANY(KGDSO.NE.KGDSOX))) THEN
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  COMPUTE NUMBER OF OUTPUT POINTS AND THEIR LATITUDES AND LONGITUDES.
   IF(KGDSO(1).GE.0) THEN
     CALL GDSWZD(KGDSO, 0,MO,FILL,XPTS,YPTS,RLON,RLAT,NO,CROT,SROT)
     IF(NO.EQ.0) IRET=3
   ENDIF
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  LOCATE INPUT POINTS
   CALL GDSWZD(KGDSI,-1,NO,FILL,XPTS,YPTS,RLON,RLAT,NV)
   IF(IRET.EQ.0.AND.NV.EQ.0) IRET=2
   CALL GDSWZD(KGDSI, 0,MI,FILL,XPTI,YPTI,RLOI,RLAI,NV,CROI,SROI)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  ALLOCATE AND SAVE GRID DATA
   KGDSIX=KGDSI
   KGDSOX=KGDSO
   IF(NOX.NE.NO) THEN
     IF(NOX.GE.0) DEALLOCATE(RLATX,RLONX,XPTSX,YPTSX,CROTX,SROTX,NXY,CXY,SXY)
     ALLOCATE(RLATX(NO),RLONX(NO),XPTSX(NO),YPTSX(NO), &
              CROTX(NO),SROTX(NO),NXY(NO),CXY(NO),SXY(NO))
     NOX=NO
   ENDIF
   IRETX=IRET
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  COMPUTE WEIGHTS
   IF(IRET.EQ.0) THEN
     CALL IJKGDS0(KGDSI,IJKGDSA)
!$OMP PARALLEL DO PRIVATE(N,CM,SM)
     DO N=1,NO
       RLONX(N)=RLON(N)
       RLATX(N)=RLAT(N)
       XPTSX(N)=XPTS(N)
       YPTSX(N)=YPTS(N)
       CROTX(N)=CROT(N)
       SROTX(N)=SROT(N)
       IF(XPTS(N).NE.FILL.AND.YPTS(N).NE.FILL) THEN
         NXY(N)=IJKGDS1(NINT(XPTS(N)),NINT(YPTS(N)),IJKGDSA)
         IF(NXY(N).GT.0) THEN
           CALL MOVECT(RLAI(NXY(N)),RLOI(NXY(N)),RLAT(N),RLON(N),CM,SM)
           CXY(N)=CM*CROI(NXY(N))+SM*SROI(NXY(N))
           SXY(N)=SM*CROI(NXY(N))-CM*SROI(NXY(N))
         ENDIF
       ELSE
         NXY(N)=0
       ENDIF
     ENDDO
   ENDIF
 ENDIF
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  INTERPOLATE OVER ALL FIELDS
 IF(IRET.EQ.0.AND.IRETX.EQ.0) THEN
   IF(KGDSO(1).GE.0) THEN
     NO=NOX
     DO N=1,NO
       RLON(N)=RLONX(N)
       RLAT(N)=RLATX(N)
       CROT(N)=CROTX(N)
       SROT(N)=SROTX(N)
     ENDDO
   ENDIF
   DO N=1,NO
     XPTS(N)=XPTSX(N)
     YPTS(N)=YPTSX(N)
   ENDDO
!$OMP PARALLEL DO &
!$OMP PRIVATE(NK,K,N,I1,J1,IXS,JXS,MX,KXS,KXT,IX,JX,NX) &
!$OMP PRIVATE(CM,SM,CX,SX,UROT,VROT)
   DO NK=1,NO*KM
     K=(NK-1)/NO+1
     N=NK-NO*(K-1)
     UO(N,K)=0
     VO(N,K)=0
     LO(N,K)=.FALSE.
     IF(NXY(N).GT.0) THEN
       IF(IBI(K).EQ.0.OR.LI(NXY(N),K)) THEN
!cdz+5
         IF(UI(NXY(N),K) == FILL9 .OR. VI(NXY(N),K) == FILL9) THEN
         UO(N,K)=UI(NXY(N),K)
         VO(N,K)=VI(NXY(N),K)
         LO(N,K)=.FALSE.
         ELSE
         UROT=CXY(N)*UI(NXY(N),K)-SXY(N)*VI(NXY(N),K)
         VROT=SXY(N)*UI(NXY(N),K)+CXY(N)*VI(NXY(N),K)
         UO(N,K)=CROT(N)*UROT-SROT(N)*VROT
         VO(N,K)=SROT(N)*UROT+CROT(N)*VROT
         LO(N,K)=.TRUE.
!cdz+1
         ENDIF
! SPIRAL AROUND UNTIL VALID DATA IS FOUND.
       ELSEIF(MSPIRAL.GT.1) THEN
         I1=NINT(XPTS(N))
         J1=NINT(YPTS(N))
         IXS=SIGN(1.,XPTS(N)-I1)
         JXS=SIGN(1.,YPTS(N)-J1)
         DO MX=2,MSPIRAL**2
           KXS=SQRT(4*MX-2.5)
           KXT=MX-(KXS**2/4+1)
           SELECT CASE(MOD(KXS,4))
           CASE(1)
             IX=I1-IXS*(KXS/4-KXT)
             JX=J1-JXS*KXS/4
           CASE(2)
             IX=I1+IXS*(1+KXS/4)
             JX=J1-JXS*(KXS/4-KXT)
           CASE(3)
             IX=I1+IXS*(1+KXS/4-KXT)
             JX=J1+JXS*(1+KXS/4)
           CASE DEFAULT
             IX=I1-IXS*KXS/4
             JX=J1+JXS*(KXS/4-KXT)
           END SELECT
           NX=IJKGDS1(IX,JX,IJKGDSA)
           IF(NX.GT.0) THEN
             IF(LI(NX,K)) THEN
!cdz+5
               IF(UI(NX,K) == FILL9 .OR. VI(NX,K) == FILL9) THEN
               UO(N,K)=UI(NX,K)
               VO(N,K)=VI(NX,K)
               LO(N,K)=.FALSE.
               ELSE
               CALL MOVECT(RLAI(NX),RLOI(NX),RLAT(N),RLON(N),CM,SM)
               CX=CM*CROI(NX)+SM*SROI(NX)
               SX=SM*CROI(NX)-CM*SROI(NX)
               UROT=CX*UI(NX,K)-SX*VI(NX,K)
               VROT=SX*UI(NX,K)+CX*VI(NX,K)
               UO(N,K)=CROT(N)*UROT-SROT(N)*VROT
               VO(N,K)=SROT(N)*UROT+CROT(N)*VROT
               LO(N,K)=.TRUE.
!cdz+1
               ENDIF
               EXIT
             ENDIF
           ENDIF
         ENDDO
       ENDIF
     ENDIF
   ENDDO
   DO K=1,KM
     IBO(K)=IBI(K)
     IF(.NOT.ALL(LO(1:NO,K))) IBO(K)=1
   ENDDO
   IF(KGDSO(1).EQ.0) CALL POLFIXV(NO,MO,KM,RLAT,RLON,IBO,LO,UO,VO)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 ELSE
   IF(IRET.EQ.0) IRET=IRETX
   IF(KGDSO(1).GE.0) NO=0
 ENDIF
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 END SUBROUTINE POLATEV2
