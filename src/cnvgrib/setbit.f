!> @file
!>
!> @author IREDELL @date 92-10-31

!> THE NUMBER OF BITS REQUIRED TO PACK A GIVEN FIELD FOR PARTICULAR
!> BINARY AND DECIMAL SCALINGS IS COMPUTED. THE MINIMUM AND MAXIMUM
!> ROUNDED FIELD VALUES ARE ALSO RETURNED. GRIB BITMAP MASKING FOR
!> VALID DATA IS OPTIONALLY USED.
!>
!> PROGRAM HISTORY LOG:
!>   96-09-16  IREDELL
!>
!> USAGE:    CALL SETBIT(IBM,IBS,IDS,LEN,MG,G,GMIN,GMAX,NBIT)
!>   INPUT ARGUMENT LIST:
!>     IBM      - INTEGER BITMAP FLAG (=0 FOR NO BITMAP)
!>     IBS      - INTEGER BINARY SCALING
!>                (E.G. IBS=3 TO ROUND FIELD TO NEAREST EIGHTH VALUE)
!>     IDS      - INTEGER DECIMAL SCALING
!>                (E.G. IDS=3 TO ROUND FIELD TO NEAREST MILLI-VALUE)
!>                (NOTE THAT IDS AND IBS CAN BOTH BE NONZERO,
!>                 E.G. IDS=1 AND IBS=1 ROUNDS TO THE NEAREST TWENTIETH)
!>     LEN      - INTEGER LENGTH OF THE FIELD AND BITMAP
!>     MG       - INTEGER (LEN) BITMAP IF IBM=1 (0 TO SKIP, 1 TO KEEP)
!>     G        - REAL (LEN) FIELD
!>
!>   OUTPUT ARGUMENT LIST:
!>     GMIN     - REAL MINIMUM VALID ROUNDED FIELD VALUE
!>     GMAX     - REAL MAXIMUM VALID ROUNDED FIELD VALUE
!>     NBIT     - INTEGER NUMBER OF BITS TO PACK
!>
!> ATTRIBUTES:
!>   LANGUAGE: FORTRAN 90
!>
!>
      SUBROUTINE SETBIT(IBM,IBS,IDS,LEN,MG,G,GMIN,GMAX,NBIT)

      DIMENSION MG(LEN),G(LEN)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  ROUND FIELD AND DETERMINE EXTREMES WHERE BITMAP IS ON
      S=2.**IBS*10.**IDS
      IF(IBM.EQ.0) THEN
        GMAX=G(1)
        GMIN=G(1)
        DO I=2,LEN
          GMAX=MAX(GMAX,G(I))
          GMIN=MIN(GMIN,G(I))
        ENDDO
      ELSE
        I1=1
        DOWHILE(I1.LE.LEN.AND.MG(I1).EQ.0)
          I1=I1+1
        ENDDO
        IF(I1.LE.LEN) THEN
          DO I=1,I1-1
            G(I)=0.
          ENDDO
          GMAX=G(I1)
          GMIN=G(I1)
          DO I=I1+1,LEN
            IF(MG(I).NE.0) THEN
              GMAX=MAX(GMAX,G(I))
              GMIN=MIN(GMIN,G(I))
            ELSE
              G(I)=0.
            ENDIF
          ENDDO
        ELSE
          DO I=1,LEN
            G(I)=0.
          ENDDO
          GMAX=0.
          GMIN=0.
        ENDIF
      ENDIF
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  COMPUTE NUMBER OF BITS
      NBIT=LOG((GMAX-GMIN)*S+0.9)/LOG(2.)+1.
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      RETURN
      END
