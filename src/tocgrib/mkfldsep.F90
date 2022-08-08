!> @file
!> @brief Makes TOC Flag Field Separator Block.
!> @author Gilbert @date 2002-09-16

!> Generates a TOC Flag Field Separator Block used to separate WMO
!> Bulletins within a transmission file to be ingested in TOC's FTP
!> Input Service, which can be used to disseminate WMO buletins (see
!> http://weather.gov/tg/ftpingest.html).
!>
!> This routine can generate different flag field separator blocks
!> depending on the value of variable iopt.
!>
!> Bulletin "Flag Field Separator" block - OPTION 1 (old)
!>
!> Bytes | Description
!> ------|------------
!> 1 - 4 | marker string (####)
!> 5 - 7 | block length [018 fixed value]
!> 8 - 13 | total length of bulletin in bytes [octets] (not including the flag field block)
!> 14 - 17 | marker string (####)
!> 18 | line Feed (ASCII "0A")
!>
!> Bulletin "Flag Field Separator" block - OPTION 1a (new)
!>
!> Bytes | Description
!> ------|------------
!> 1 - 4 | marker string (####)
!> 5 - 7 | block length (nnn) - value always greater than 018
!> 8 - 18 | total length of bulletin in bytes [octets] (not including the flag field block)
!> 19 - nnn-5 | reserved for future use
!> nnn-4 - nnn-1 | marker string (####)
!> nnn | line Feed (ASCII "0A")
!>
!>   Bulletin "Flag Field Separator" block - OPTION 2 (limited)
!>
!> Bytes | Description
!> ------|------------
!> 1 - 4 | marker string (****)
!> 5 - 14 | total length of bulletin in bytes [octets] (not including the flag field block)
!> 15 - 18 | marker string (****)
!> 19 | line Feed (ASCII "0A")
!>
!> @param[out] csep*(*) Character array containing the flag field separator.
!> @param[in] iopt Flag Field Separator block option:
!> - 1 Separator block for use with alphanumeric bulletins.
!>   - if lenin <= 18 and lenbull <= 999999, OPTION 1 block will be generated.
!>   - if lenin > 18 or lenbull > 999999, OPTION 1a block will be generated.
!> - 2 Separator block for use with GRIB/BUFR bulletins.
!> @param[in] lenin Desired length of the flag field separator
!> block. ignored, if iopt=2.
!> @param[in] lenbull Integer length of the bulletin (in bytes) that
!> will follow this separator block.
!> @param[out] lenout Integer length of the flag field separator block.
!>
!> @author Gilbert @date 2002-09-16
subroutine mkfldsep(csep,iopt,lenin,lenbull,lenout)
  !
  character*(*),intent(out) :: csep
  integer,intent(in) :: iopt,lenin,lenbull
  integer,intent(out) :: lenout
  !
  character(len=4),parameter :: cstar='****',clb='####'
  !
  if (iopt.eq.1) then
     if ( lenin .le. 18 .and. lenbull .le. 999999 ) then
        ! Create OPTION 1 separator block
        csep(1:4)=clb
        csep(5:7)='018'
        write(csep(8:13),fmt='(I6.6)') lenbull
        csep(14:17)=clb
        csep(18:18)=char(10)
        lenout=18
     else                         ! Create OPTION 1a separator block
        nnn=lenin
        if ( nnn.lt.23 ) nnn=23
        csep(1:4)=clb
        write(csep(5:7),fmt='(I3.3)') nnn
        write(csep(8:18),fmt='(I11.11)') lenbull
        csep(19:nnn-5)='0'
        csep(nnn-4:nnn-1)=clb
        csep(nnn:nnn)=char(10)
        lenout=nnn
     endif
  elseif (iopt.eq.2) then         !  Create OPTION 2 separator block
     csep(1:4)=cstar
     write(csep(5:14),fmt='(I10.10)') lenbull
     csep(15:18)=cstar
     csep(19:19)=char(10)
     lenout=19
  else
     print *,"mkfldsep: Option ",iopt," not recognized."
     csep(1:lenin)=' '
  endif
  !
  return
end subroutine mkfldsep
