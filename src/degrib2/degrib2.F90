!> @file
!> @brief Make an inventory of a GRIB2 file.
!> @author Stephen Gilbert @date 2010-09-08

!> This program reads a GRIB2 file and makes an inventory.
!>
!> Usage:
!> degrib2 [filename]
!>
!> @note Command line can have only one file name.
!>
!> @return 0 for success.
!> @author Stephen Gilbert @date 2010-09-08
program degrib2
  use grib_mod
  use params
  implicit none

  integer :: msk1, msk2, icount, ifl1, iseek, itot, j, lengrib, lgrib, lskip
  integer :: maxlocal, n, ncgb, numfields, numlocal
  real :: fldmax, fldmin, sum
  parameter(msk1 = 32000, msk2 = 4000)
  character(len = 1), allocatable, dimension(:) :: cgrib
  integer :: listsec0(3)
  integer :: listsec1(13)
  character(len = 250) :: gfile1
  character(len = 8) :: pabbrev
  character(len = 40) :: labbrev
  character(len = 100) :: tabbrev
  integer(4) narg, iargc, temparg
  integer :: currlen = 0,  numpts = 0
  logical :: unpack, expand
  type(gribfield) :: gfld
  integer :: ierr, ios, is
  
  call start()
  unpack = .true.
  expand = .false.

  ! Get arguments.
  narg = iargc()
  if (narg .ne. 1) then
     call errmsg('degrib2:  incorrect usage')
     call errmsg('usage: degrib2 grib2file')
     call errexit(2)
  endif

  ! Open the input file with the bacio library.
  ifl1 = 10
  temparg = 1
  call getarg(temparg, gfile1)
  ncgb = len_trim(gfile1)
  call baopenr(ifl1, gfile1(1:ncgb), ios)

  ! Process each message in the file.
  itot = 0
  icount = 0
  iseek = 0
  do
     ! Find a GRIB2 message in the file.
     call skgb(ifl1, iseek, msk1, lskip, lgrib)
     if (lgrib .eq. 0) exit    ! end loop at EOF or problem

     ! Read the GRIB2 message from the file.
     if (lgrib .gt. currlen) then
        if (allocated(cgrib)) deallocate(cgrib)
        allocate(cgrib(lgrib), stat = is)
        currlen = lgrib
     endif
     call baread(ifl1, lskip, lgrib, lengrib, cgrib)
     if (lgrib .ne. lengrib) then
        write(6, *)' degrib2: IO Error.'
        call errexit(9)
     endif
     iseek = lskip + lgrib
     icount = icount + 1
     write (6, *)
     write(6, '(A,I0,A,I0)') ' GRIB MESSAGE  ', icount, '  starts at ', lskip + 1
     write (6, *)

     ! Get info about the message.
     call gb_info(cgrib, lengrib, listsec0, listsec1,  &
          numfields, numlocal, maxlocal, ierr)
     if (ierr .ne. 0) then
        write(6, '(A,I0)') ' ERROR extracting field = ', ierr
        stop 10
     endif
     itot = itot + numfields
     write(6, '(A,3(1x,I0))')'  SECTION 0: ', (listsec0(j), j = 1, 3)
     write(6, '(A,13(1x,I0))')'  SECTION 1: ', (listsec1(j), j = 1, 13)
     write(6, '(A,1x,I0,1x,A,I0,1x,A)') '  Contains ', numlocal,  &
          ' Local Sections  and  ', numfields, ' data fields.'

     ! Read each field in the message.
     do n = 1, numfields
        ! Unpack GRIB2 field.
        call gf_getfld(cgrib, lengrib, n, unpack, expand, gfld, ierr)
        if (ierr .ne. 0) then
           write(6, '(A,I0)') ' ERROR extracting field = ', ierr
           cycle
        endif

        write (6, *)
        write(6, '(A,1x,I0)')'  FIELD ', n
        if (n .eq. 1) then
           write(6, '(A,2(1x,I0))')'  SECTION 0: ', gfld%discipline, gfld%version
           write(6, '(A,20(1x,I0))')'  SECTION 1: ', (gfld%idsect(j), j = 1, gfld%idsectlen)
        endif
        if ( associated(gfld%local).AND.gfld%locallen.gt.0 ) then
           write(6, '(A,I0,A)')'  SECTION 2: ', gfld%locallen, ' bytes'
        endif
        write(6, '(A,5(1x,I0))')'  SECTION 3: ', gfld%griddef, gfld%ngrdpts,  gfld%numoct_opt, &
             gfld%interp_opt, gfld%igdtnum
        write(6, '(A,1x,I0,A,100(1x,I0))')'  GRID TEMPLATE 3.',  &
             gfld%igdtnum, ' : ',  (gfld%igdtmpl(j), j = 1, gfld%igdtlen)
        if (gfld%num_opt .eq. 0) then
           write(6, *)' NO Optional List Defining Number of Data Points.'
        else
           write(6, '(A,1x,150(1x,I0))')'  Section 3 Optional List:',  &
                (gfld%list_opt(j), j = 1, gfld%num_opt)
        endif

        pabbrev = param_get_abbrev(gfld%discipline, gfld%ipdtmpl(1), gfld%ipdtmpl(2))
        call prlevel(gfld%ipdtnum, gfld%ipdtmpl, labbrev)
        call prvtime(gfld%ipdtnum, gfld%ipdtmpl, listsec1, tabbrev)

        write(6,'(A,1x,I0,A,A,3(1X,I0),A,80(1x,I0))') '  PRODUCT TEMPLATE 4.',  gfld%ipdtnum, &
             ': ( PARAMETER = ', pabbrev, gfld%discipline, gfld%ipdtmpl(1), gfld%ipdtmpl(2), ' ) ', &
             (gfld%ipdtmpl(j), j = 1, gfld%ipdtlen)

        write(6, '(A,A,A,A,A)')'  FIELD: ', pabbrev, trim(labbrev), " ", trim(tabbrev)
        if (gfld%num_coord .eq. 0) then
           write(6, *)' NO Optional Vertical Coordinate List.'
        else
           write(6, '(A,1X,150(1x,I0))') '  Section 4 Optional & Coordinates: ', &
                (gfld%coord_list(j), j = 1, gfld%num_coord)
        endif
        if (gfld%ibmap .ne. 255) then
           write(6, '(A,I0,A,I0)')'  Num. of Data Points =  ', &
                gfld%ndpts, '    with BIT-MAP  ', gfld%ibmap
        else
           write(6, '(A,I0,A)')'  Num. of Data Points =  ', gfld%ndpts, '     NO BIT-MAP '
        endif
        write(6, '(A,I0,A,20(1x,I0))')'  DRS TEMPLATE 5. ', gfld%idrtnum, ' : ', &
             (gfld%idrtmpl(j), j = 1, gfld%idrtlen)
        if (gfld%fld(1) .eq. 9.9990003E+20) then  ! checking undefined values
           fldmax = 0.0
           fldmin = 99999.99
           sum = 0.0
           numpts = 0
        else
           fldmax = gfld%fld(1)
           fldmin = gfld%fld(1)
           sum = gfld%fld(1)
           numpts = 1
        end if
        do j = 2, gfld%ndpts
           if (gfld%fld(j) .eq. 9.9990003E+20) then ! checking undefined values
              cycle
           end if
           if (gfld%fld(j) .gt. fldmax) fldmax = gfld%fld(j)
           if (gfld%fld(j) .lt. fldmin) fldmin = gfld%fld(j)
           sum = sum + gfld%fld(j)
           numpts = numpts + 1
        enddo

        write(6, *)' Data Values:'
        write(6, '(A,I0,A,I0)')'  Num. of Data Points =  ', &
             gfld%ndpts, '   Num. of Data Undefined = ', gfld%ndpts-numpts
        write(6, fmt = '( "( PARM= ",A," ) : ", " MIN=",f25.8," AVE=",f25.8, " MAX=",f25.8)') &
             trim(pabbrev), fldmin, sum / numpts, fldmax

        ! Free memory allocated to hold field.
        call gf_free(gfld)
     enddo
     
     ! If we allocated cgrib, deallocate it.
     if (allocated(cgrib)) then
        deallocate(cgrib)
        currlen = 0
     endif
     
  enddo
  write(6, *)" "
  write(6, '(A,I0)')'  Total Number of Fields Found =  ', itot

end program degrib2
