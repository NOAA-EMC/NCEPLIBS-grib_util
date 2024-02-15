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
  
  integer :: msk1, msk2
  parameter(msk1=32000,msk2=4000)
  character cgb*256,cgi*256
  character(len=1),pointer,dimension(:) :: cbuf
  character carg*300
  integer narg,iargc
  integer :: numtot, nnum, nlen, ncgi, mnum, lcarg, kw
  integer :: ios, iret, irgi, iw, ncgb, nmess
  
  interface
     subroutine getg2ir(lugb,msk1,msk2,mnum,cbuf,nlen,nnum, &
          nmess,iret)
       integer,intent(in) :: lugb,msk1,msk2,mnum
       character(len=1),pointer,dimension(:) :: cbuf
       integer,intent(out) :: nlen,nnum,nmess,iret
     end subroutine getg2ir
  end interface

  !  get arguments
  narg=iargc()
  if(narg.ne.2) then
     call errmsg('grb2index:  Incorrect usage')
     call errmsg('Usage: grb2index gribfile indexfile')
     call errexit(2)
  endif
  call getarg(1,cgb)
  ncgb=len_trim(cgb)
  call baopenr(11,cgb(1:ncgb),ios)
  !call baseto(1,1)
  if(ios.ne.0) then
     lcarg=len('grb2index:  Error accessing file '//cgb(1:ncgb))
     carg(1:lcarg)='grb2index:  Error accessing file '//cgb(1:ncgb)
     call errmsg(carg(1:lcarg))
     call errexit(8)
  endif
  call getarg(2,cgi)
  ncgi=len_trim(cgi)
  call baopen(31,cgi(1:ncgi),ios)
  if(ios.ne.0) then
     lcarg=len('grb2index:  Error accessing file '//cgi(1:ncgi))
     carg(1:lcarg)='grb2index:  Error accessing file '//cgi(1:ncgi)
     call errmsg(carg(1:lcarg))
     call errexit(8)
  endif

  !  write index file
  mnum=0
  call getg2ir(11,msk1,msk2,mnum,cbuf,nlen,nnum,nmess,irgi)
  if(irgi.gt.1.or.nnum.eq.0.or.nlen.eq.0) then
     call errmsg('grb2index:  No GRIB messages detected in file ' &
          //cgb(1:ncgb))
     call baclose(11,iret)
     call baclose(31,iret)
     call errexit(1)
  endif
  numtot=numtot+nnum
  mnum=mnum+nmess
  call wrgi1h(31,nlen,numtot,cgb(1:ncgb))
  iw=162
  call bawrite(31,iw,nlen,kw,cbuf)
  iw=iw+nlen

  !  extend index file if index buffer length too large to hold in memory
  if(irgi.eq.1) then
     do while(irgi.eq.1.and.nnum.gt.0)
        if (associated(cbuf)) then
           deallocate(cbuf)
           nullify(cbuf)
        endif
        call getg2ir(11,msk1,msk2,mnum,cbuf,nlen,nnum,nmess,irgi)
        if(irgi.le.1.and.nnum.gt.0) then
           numtot=numtot+nnum
           mnum=mnum+nmess
           call bawrite(31,iw,nlen,kw,cbuf)
           iw=iw+nlen
        endif
     enddo
     call wrgi1h(31,iw,numtot,cgb(1:ncgb))
  endif
  call baclose(11,iret)
  call baclose(31,iret)

end program grb2index

!> Write index headers.
!>
!> @param[in] lugi integer logical unit of output index file
!> @param[in] nlen integer total length of index records
!> @param[in] nnum integer number of index records
!> @param[in] cgb character name of GRIB file
!>
!> @author Iredell @date 93-11-22
subroutine wrgi1h(lugi,nlen,nnum,cgb)
  implicit none

  integer :: lugi, nlen, nnum
  character cgb*(*)
#ifdef __GFORTRAN__
  character cd8*8,ct10*10,hostname*15
  integer istat
#else
  character cd8*8,ct10*10,hostnam*15
  integer hostnm
#endif
  character chead(2)*81
  integer :: kw, ncgb, ncgb1, ncgb2, ncbase

  !  fill first 81-byte header
  ncgb=len(cgb)
  ncgb1=ncbase(cgb,ncgb)
  ncgb2=ncbase(cgb,ncgb1-2)
  call date_and_time(cd8,ct10)
  chead(1)='!GFHDR!'
  chead(1)(9:10)=' 1'
  chead(1)(12:14)='  1'
  write(chead(1)(16:20),'(i5)') 162
  chead(1)(22:31)=cd8(1:4)//'-'//cd8(5:6)//'-'//cd8(7:8)
  chead(1)(33:40)=ct10(1:2)//':'//ct10(3:4)//':'//ct10(5:6)
  chead(1)(42:47)='gb2ix1'
  !chead(1)(49:54)=cgb(ncgb2:ncgb1-2)
  chead(1)(49:54)='      '
#ifdef __GFORTRAN__
  istat=hostnm(hostname)
  if(istat.eq.0) then
     chead(1)(56:70)='0000'
  else
     chead(1)(56:70)='0001'
  endif
#else
  chead(1)(56:70)=hostnam(hostname)
#endif
  chead(1)(72:80)='grb2index'
  chead(1)(81:81)=char(10)

  !  fill second 81-byte header
  chead(2)='IX1FORM:'
  write(chead(2)(9:38),'(3i10)') 162,nlen,nnum
  chead(2)(41:80)=cgb(ncgb1:ncgb)
  chead(2)(81:81)=char(10)

  !  write headers at beginning of index file
  call bawrite(lugi,0,162,kw,chead)

  return
end subroutine wrgi1h

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
