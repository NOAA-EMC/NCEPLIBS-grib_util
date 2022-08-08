!> @file
!> Compute bits required to pack a given field.
!> @author Mark Iredell @date 92-10-31

!> The number of bits required to pack a given field for particular
!> binary and decimal scalings is computed. The minimum and maximum
!> rounded field values are also returned. GRIB bitmap masking for
!> valid data is optionally used.
!>
!> @param[in] ibm integer bitmap flag (=0 for no bitmap).
!> @param[in] ibs integer binary scaling (e.g. ibs=3 to round field to
!> nearest eighth value).
!> @param[in] ids integer decimal scaling (e.g. ids=3 to round field to
!> nearest milli-value) (note that ids and ibs can both be nonzero,
!> e.g. ids=1 and ibs=1 rounds to the nearest twentieth).
!> @param[in] len integer length of the field and bitmap.
!> @param[in] mg integer (len) bitmap if ibm=1 (0 to skip, 1 to keep).
!> @param[in] g real (len) field.
!> @param[out] gmin real minimum valid rounded field value.
!> @param[out] gmax real maximum valid rounded field value.
!> @param[out] nbit integer number of bits to pack.
!>
!> @author Mark Iredell @date 92-10-31
SUBROUTINE SETBIT(IBM,IBS,IDS,LEN,MG,G,GMIN,GMAX,NBIT)

  DIMENSION MG(LEN),G(LEN)

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
     DO WHILE(I1.LE.LEN.AND.MG(I1).EQ.0)
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

  !  COMPUTE NUMBER OF BITS
  NBIT=LOG((GMAX-GMIN)*S+0.9)/LOG(2.)+1.

  RETURN
END SUBROUTINE SETBIT
