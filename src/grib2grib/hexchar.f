!> @file
!> @brief Convert 2 character hex numbers (z2) into 1 character with value of 0
!> to 255.      
!> @author Southall @date 96-05-21      

!> Convert 2 character hex numbers (z2) into 1 character with value of 0
!> to 255.
!>      
!> @note Cray cft77 fortran does not let you read character data, or
!> print character data with a z2 format.
!>
!> @param hex
!> @param out
!> @param ierr error code
!> @author Southall @date 96-05-21      
      SUBROUTINE HEXCHAR(HEX,OUT,IERR)
      CHARACTER * 1  OUT
      CHARACTER * 2  HEX
!
      INTEGER        INT
      INTEGER        INT1
      INTEGER        INT2
!
      SAVE
!
      IERR = 0
      INT1 = MOVA2I(HEX(1:1))
      IF (INT1.GE.48.AND.INT1.LE.57) THEN
        INT1 = INT1 - 48
      ELSE IF (INT1.GE.65.AND.INT1.LE.70) THEN
        INT1 = INT1 - 55
      ELSE IF (INT1.GE.97.AND.INT1.LE.102) THEN
        INT1 = INT1 - 87
      ELSE
        IERR = 1
        RETURN
      END IF
!
      INT2 = MOVA2I(HEX(2:2))
      IF (INT2.GE.48.AND.INT2.LE.57) THEN
        INT2 = INT2 - 48
      ELSE IF (INT2.GE.65.AND.INT2.LE.70) THEN
        INT2 = INT2 - 55
      ELSE IF (INT2.GE.97.AND.INT2.LE.102) THEN
        INT2 = INT2 - 87
      ELSE
        IERR = 1
        RETURN
      END IF
      INT = INT1 * 16 + INT2
      OUT = CHAR(INT)
!
      RETURN
      END
