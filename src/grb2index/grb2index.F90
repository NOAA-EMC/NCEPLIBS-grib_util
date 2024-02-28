!> @file
!> Write a GRIB2 index file.
!> @author Iredell @date 1992-11-22

!> This program creates an index file from a GRIB2 file.
!>
!> @return
!> - 0 successful run
!> - 1 GRIB message not found
!> - 2 incorrect arguments
!> - 8 error accessing file
!>
!> @author Iredell @date 1992-11-22
program grb2index
  implicit none
  integer narg,iargc
  character cgb*256,cgi*256
  integer :: idxver = 1
  integer :: lugb = 11, lugi = 12
  integer :: ncgb, ncgb1
  integer :: iret, ios, ncbase
  
  !  get arguments
  narg = iargc()
  if (narg.ne.2) then
     call errmsg('grb2index:  Incorrect usage')
     call errmsg('Usage: grb2index gribfile indexfile')
     call errexit(2)
  endif
  call getarg(1,cgb)
  call getarg(2,cgi)

  ! Open binary GRIB2 file for input.
  call baopenr(lugb, trim(cgb), ios)
  if (ios .ne. 0) then
     print *, 'grb2index:  Error accessing file ', trim(cgb)
     stop 2
  endif

  ! Open index file for output.
  call baopenw(lugi, trim(cgi), ios)
  if (ios .ne. 0) then
     print *, 'grb2index:  Error accessing file ', trim(cgi)
     stop 3
  endif

  ! Locate base name of file.
  ncgb = len(cgb)
  ncgb1 = ncbase(cgb,ncgb)

  ! Create the index file and write it to lugi.
  call g2_create_index(lugb, lugi, cgi, idxver, cgb(ncgb1:ncgb), iret)
  if (iret .ne. 0) stop iret

  ! Close our files.
  call baclose(lugb,iret)  
  if (iret .ne. 0) stop iret
  call baclose(lugi,iret)  
  if (iret .ne. 0) stop iret

end program grb2index

!> Locate basename of a file.
!>
!> This subprogram locates the character number after the last '/' in a
!> character string. For unix filenames, the character number returned
!> marks the beginning of the basename of the file.
!>
!> @param[in] c character string to search
!> @param[in] n integer length of string
!>
!> @return The index of the basename within the string.
!>
!> @author Iredell @date 93-11-22
integer function ncbase(c,n)
  implicit none
  character c*(*)
  integer :: n
  integer :: k

  k = n
  do while (k .ge. 1 .and. c(k:k) .ne. '/')
     k = k - 1
  enddo
  ncbase = k + 1

end function ncbase
