!> @file
!> @brief Format the wmo header.
!> @author Farley @date 84-07-06

!> Forms the wmo header for a given bulletin.
!>
!> ### Program History Log
!> Date | Programmer | Comments
!> -----|------------|---------
!> 84-07-06 | Farley | Original author
!> 94-10-10 | R. E. Jones | Changes for cray
!> 95-10-18 | R. E. Jones | Add parameter kwbx to call
!> 98-06-16 | Gilbert | Changed argument list to pass in day and hour instead of the old O.N. 84 date word.
!> 2003-03-28 | Gilbert | Removed equivalences.
!>
!> @param[in] bulhed ttaaii bulletin header ft10
!> @param[in] iday day of month
!> @param[in] ihour hour of day.
!> @param[in] imin min of hour.
!> @param[in] kwbx 4 characters (kwbc to kwbq)
!> @param[out] header complete wmo header in ascii
!>
!> @author Farley @date 84-07-06
SUBROUTINE MAKWMO (BULHED,IDAY,IHOUR,IMIN,KWBX,HEADER)
  CHARACTER * 6 BULHED
  CHARACTER * 1 HEADER (*)
  CHARACTER * 1 WMOHDR (21)
  CHARACTER * 4 KWBX
  CHARACTER * 2 CTEMP
  !
  !$            1.     CREATE WMO HEADER.
  !$            1.1    CONVERT BULHED FROM EBCDIC TO ASCII.
  !
  !     WRITE (6,FMT='('' MADE IT TO MAKWMO'')')
  !
  DO I = 1,6
     WMOHDR(I) = BULHED(I:I)
  END DO
  WMOHDR(7)=char(32)        !  ASCII BLANK
  !
  !     MOVE KWBX INTO WMO HEADER
  !
  DO I = 1,4
     WMOHDR(I+7) = KWBX(I:I)
  END DO
  WMOHDR(12)=char(32)       !  ASCII BLANK
  !
  !$            1.2    PICK OFF THE DAY OF MONTH (YY)
  !$                   AND CONVERT TO ASCII.
  !
  write(ctemp,fmt='(I2.2)') IDAY
  WMOHDR(13)=ctemp(1:1)
  WMOHDR(14)=ctemp(2:2)
  !
  !$            1.3    PICK OFF THE HOUR(GG) AND CONVERT TO ASCII.
  !
  write(ctemp,fmt='(I2.2)') IHOUR
  WMOHDR(15)=ctemp(1:1)
  WMOHDR(16)=ctemp(2:2)

  write(ctemp,fmt='(I2.2)') IMIN
  WMOHDR(17)=ctemp(1:1)
  WMOHDR(18)=ctemp(2:2)

  !
  !             1.4    FIL IN REST OF HEADER
  !
  !     WMOHDR(17)=char(48)    !  ASCII "0"
  !     WMOHDR(18)=char(48)    !  ASCII "0"
  WMOHDR(19)=char(13)       !  ASCII CR = '\r'
  WMOHDR(20)=char(13)       !  ASCII CR = '\r'
  WMOHDR(21)=char(10)       !  ASCII LF = '\n'
  !
  !$            2.     MOVE WMOHDR TO OUTPUT FIELD.
  !
  DO I = 1,21
     HEADER(I) = WMOHDR(I)
  END DO
  !
  RETURN
END SUBROUTINE MAKWMO
