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
!> - 9 error closing file
!>
!> @author Iredell @date 1992-11-22
program grb2index
  implicit none
  integer narg, iargc
  character cgb * 256, cgi * 256
  character cidxver * 1
  integer :: idxver = 2
  integer :: lugb = 11, lugi = 12
  integer :: ncgb, ncgb1
  integer :: iret, ios, ncbase, argnum = 0

  interface
     subroutine g2_create_index(lugb, lugi, idxver, filename, iret)
       implicit none
       integer, intent(in) :: lugb, lugi, idxver
       character*(*) :: filename
       integer, intent(out) :: iret
     end subroutine g2_create_index
  end interface
  
  ! Get arguments.
  narg = iargc()
  if (narg .ne. 2 .and. narg .ne. 3) then
     call errmsg('grb2index:  Incorrect usage')
     call errmsg('Usage: grb2index gribfile indexfile')
     call errmsg('or: grb2index idxver gribfile indexfile')
     call exit(2)
  endif
  if (narg .eq. 3) then
     call getarg(1, cidxver)
     read(cidxver, '(i1)') idxver
     argnum = 1
  end if
  call getarg(argnum + 1, cgb)
  call getarg(argnum + 2, cgi)

  ! Open binary GRIB2 file for input.
  call baopenr(lugb, trim(cgb), ios)
  if (ios .ne. 0) then
     print *, 'grb2index:  Error accessing file ', trim(cgb)
     call exit(8)
  endif

  ! Open index file for output.
  call baopenw(lugi, trim(cgi), ios)
  if (ios .ne. 0) then
     print *, 'grb2index:  Error accessing file ', trim(cgi)
     call exit(1)
  endif

  ! Locate base name of file.
  ncgb = len(cgb)
  ncgb1 = ncbase(cgb,ncgb)

  ! Create the index file and write it to lugi.
  call g2_create_index(lugb, lugi, idxver, cgb(ncgb1:ncgb), iret)
  if (iret .ne. 0) then
     call exit(1)
  endif

  ! Close our files.
  call baclose(lugb,iret)  
  if (iret .ne. 0) stop 9
  call baclose(lugi,iret)  
  if (iret .ne. 0) stop 9

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
