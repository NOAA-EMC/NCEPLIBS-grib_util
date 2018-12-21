! fortran api for reading and writing grib2
! 12/2015  Wesley Ebisuzaki   Public Domain
!
! fortran 2003 / fortran 95 + TR 15581  API to read and write grib2 files
!
!   requirements:
!        f2003 or f95 + TR 15581  (allowing subroutines to deallocate and allocate)
!        callable wgrib2
!        fort_wgrib2.c (wgrib2c wrapper for callable wgrib2)
!
! Provides:
!    wgrib2a(list of strings) :: same as $ wgrib2 [list of strings]
!    wgrib2c(argc, argv) :: same as $ wgrib2 [list of strings]
!
!    grb2_mk_inv :: makes inventory file
!        grb2_mk_inv('FILE.grb', 'FILE.inv')  same as wgrib2 FILE.grb -match_inv >FILE.inv
!        The inventory file can be a temporary file if has the name @tmp:NAME
!        The inventory file can be a memory file if has the name @mem:N  N=0..9
!
!    grb2_inq :: grib2 inquire
!        grb2_inq('IN.grb', 'IN.inv', ...)
!
!    grb2_wrt :: grib2 write
!       grb2_wrt('OUT.grb', 'TEMPLATE.grb', 'ID', ...)
!       TEMPLATE.grb is a grib file that is used as a template for the new grib file
!
!	order = 'raw' .. fastest data must be in same order as template
!	order = 'ns'  .. data must be in we:nw order
!	order = 'sn'  .. data must be in we:sn order
!

module wgrib2api
use wgrib2lowapi

contains
        integer function grb2_var_args(lines,istart,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11, &
          a12,a13,a14,a15,a16,a17,a18,a19,a20)

        character (len=*), optional, intent(in):: a1,a2,a3,a4,a5,a6,a7,a8,a9,a10
        character (len=*), optional, intent(in):: a11,a12,a13,a14,a15,a16,a17
        character (len=*), optional, intent(in):: a18,a19,a20
        integer, intent(in) :: istart
        character (len=100), intent(inout) :: lines(*)

        integer :: n

        n = istart

        if (.not. present(a1)) goto 100
            n=n+1
            lines(n) = a1

        if (.not. present(a2)) goto 100
            n=n+1
            lines(n) = a2

        if (.not. present(a3)) goto 100
            n=n+1
            lines(n) = a3

        if (.not. present(a4)) goto 100
            n=n+1
            lines(n) = a4

        if (.not. present(a5)) goto 100
            n=n+1
            lines(n) = a5

        if (.not. present(a6)) goto 100
            n=n+1
            lines(n) = a6

        if (.not. present(a7)) goto 100
            n=n+1
            lines(n) = a7

        if (.not. present(a8)) goto 100
            n=n+1
            lines(n) = a8

        if (.not. present(a9)) goto 100
            n=n+1
            lines(n) = a9

        if (.not. present(a10)) goto 100
            n=n+1
            lines(n) = a10

        if (.not. present(a11)) goto 100
            n=n+1
            lines(n) = a11

        if (.not. present(a12)) goto 100
            n=n+1
            lines(n) = a12

        if (.not. present(a13)) goto 100
            n=n+1
            lines(n) = a13

        if (.not. present(a14)) goto 100
            n=n+1
            lines(n) = a14

        if (.not. present(a15)) goto 100
            n=n+1
            lines(n) = a15

        if (.not. present(a16)) goto 100
            n=n+1
            lines(n) = a16

        if (.not. present(a17)) goto 100
            n=n+1
            lines(n) = a17

        if (.not. present(a18)) goto 100
            n=n+1
            lines(n) = a18

        if (.not. present(a19)) goto 100
            n=n+1
            lines(n) = a19

        if (.not. present(a20)) goto 100
            n=n+1
            lines(n) = a20

100        continue

        grb2_var_args = n
        return
        end function

        integer function add_line(lines,n,line)

        character (len=*), intent(inout):: lines(:)
        character (len=*), intent(in):: line
        integer, intent(inout) :: n

        n = n + 1
        lines(n) = line
        add_line = 0

        return
        end function add_line

        integer function wgrib2a(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11, &
          a12,a13,a14,a15,a16,a17,a18,a19,a20)

	use iso_c_binding
	use wgrib2lowapi
        character (len=*), optional, intent(in):: a1,a2,a3,a4,a5,a6,a7,a8,a9,a10
        character (len=*), optional, intent(in):: a11,a12,a13,a14,a15,a16,a17
        character (len=*), optional, intent(in):: a18,a19,a20

        integer n
        character (len=100) :: lines(20)

        n = grb2_var_args(lines,0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11, &
          a12,a13,a14,a15,a16,a17,a18,a19,a20)

        wgrib2a = wgrib2c(n,lines,100)
        return
        end function

!
!        grb2_mk_inv
!        writes the match inventory to a file
!
        integer function grb2_mk_inv(grbfile, invfile)

	use wgrib2lowapi
        implicit none
        character (len=*), optional, intent(in):: grbfile, invfile
        character (len=100) :: cmd(10)
	integer :: n, i

	n = 0
        i = add_line(cmd,n,grbfile)
        i = add_line(cmd,n,'-rewind_init')
        i = add_line(cmd,n,grbfile)
        i = add_line(cmd,n,'-inv')
        i = add_line(cmd,n,invfile)
        i = add_line(cmd,n,'-match_inv')

	grb2_mk_inv = wgrib2c(n, cmd, len(cmd(1)))
        return
        end function

!
!        grb2_wrt: grib2 wrt
!               write grib message
!
        integer function grb2_wrt(grbfile, template_file,  template_id, &
            data1, data2, packing, order, var, meta, level, date, &
	    fhour, fminute, fhour_ave1, fhour_ave2, fhour_acc1, fhour_acc2, &
	    center, subcenter, mb)

	use iso_c_binding
	use wgrib2lowapi
        implicit none

        character (len=*), intent(in):: grbfile, template_file

        real, optional, intent(in) :: data1(:)
        real, optional, allocatable, intent(in) :: data2(:,:)
	integer, intent(in) :: template_id

        character (len=*), optional, intent(in):: var, meta, level, packing, order
        integer (kind=8), optional, intent(in) :: date
        integer, optional, intent(in) :: fhour, fminute, fhour_ave1, fhour_ave2
        integer, optional, intent(in) :: fhour_acc1, fhour_acc2
        integer, optional, intent(in) :: center, subcenter

        real, optional, intent(in) :: mb

        character (len=100) :: cmd(80)
        character (len=10) :: pack
        integer :: i,n, yrev
        logical :: present_data1, present_data2

	integer (C_INT) :: ierr
	integer (C_SIZE_T) :: ndata, nx, ny
	real (C_FLOAT), allocatable :: wgrib2_data(:)

!        write(*,*) '>> grb2_wrt'
        present_data1 = present(data1)
        present_data2 = present(data2)

        if (present_data1 .and. present_data2) then
            write(*,*) '*** FATAL ERROR grb2_wrt:  must specify data1 or data2 but not both'
            stop 8
        endif
        if (.not.present_data1 .and. .not.present_data2) then
            write(*,*) '*** FATAL ERROR grb2_wrt:  neither data1 nor data2 specified'
            stop 8
        endif

	if (present_data1) then
	    ndata = size(data1,1,C_SIZE_T)
	    ierr = wgrib2_set_reg(data1, ndata, 9)
	endif

	if (present_data2) then
	    nx = size(data2,1,C_SIZE_T)
	    ny = size(data2,2,C_SIZE_T)
	    ndata = nx*ny
	    stop 99
	    write(*,*) 'nx,ny,ndata=',nx,ny,ndata
	endif

        n = 0

        i = add_line(cmd,n,template_file)
        i = add_line(cmd,n,'-rewind_init')
        i = add_line(cmd,n,template_file)

	if (.not. present(order)) then
            i = add_line(cmd,n,'-order')
            i = add_line(cmd,n,'raw')
	else
	    if (order .eq. 'raw') then
                i = add_line(cmd,n,'-order')
                i = add_line(cmd,n,'raw')
	    else if (order .eq. 'ns' ) then
                i = add_line(cmd,n,'-order')
                i = add_line(cmd,n,'we:ns')
	    else if (order .ne. 'ns' ) then
		write(*,*) 'FATAL ERROR grb2_wrt: unknow order=',order
		stop 9
	    endif
	endif

        pack='c2'
        if (present(packing)) then
            pack=packing
        endif
        i = add_line(cmd,n,'-set_grib_type')
        i = add_line(cmd,n,pack)

        i = add_line(cmd,n,'-d')
	n = n + 1
	write(cmd(n), '(i6.6)') template_id

        i = add_line(cmd,n,'-rpn_rcl')
        i = add_line(cmd,n,'9')

	if (present(meta)) then
            i = add_line(cmd,n,'-set_metadata_str')
            n = n + 1
            cmd(n) = '0:0:' // meta
	endif

	if (present(date)) then
            i = add_line(cmd,n,'-set_date')
            n = n + 1
	    write(cmd(n),'(i14.10)') date
	endif

	if (present(var)) then
            i = add_line(cmd,n,'-set_var')
            n = n + 1
            cmd(n) = var
	endif

	if (present(level)) then
            i = add_line(cmd,n,'-set_lev')
            n = n + 1
            cmd(n) = level
	endif

	if (present(mb)) then
            i = add_line(cmd,n,'-set_lev')
            n = n + 1
	    write(cmd(n),*) mb, 'mb'
	endif

	if (present(fhour)) then
            i = add_line(cmd,n,'-set_ftime')
            n = n + 1
	    write(cmd(n),*) fhour, 'hour fcst'
	endif

	if (present(fminute)) then
            i = add_line(cmd,n,'-set_ftime')
            n = n + 1
	    write(cmd(n),*) fminute, 'min fcst'
	endif

	if (present(fhour_ave1).and.present(fhour_ave2)) then
            i = add_line(cmd,n,'-set_ave')
            n = n + 1
	    write(cmd(n),'(i5,a1,i5,a)')  fhour_ave1, '-', fhour_ave2, ' hour ave fcst'
	endif

	if (present(fhour_acc1).and.present(fhour_acc2)) then
            i = add_line(cmd,n,'-set_ave')
            n = n + 1
	    write(cmd(n),'(i5,a1,i5,a)')  fhour_acc1, '-', fhour_acc2, ' hour ave fcst'
	endif

	if (present(center)) then
            i = add_line(cmd,n,'-set')
            i = add_line(cmd,n,'center')
            n = n + 1
	    write(cmd(n),*) center
	endif

	if (present(subcenter)) then
            i = add_line(cmd,n,'-set')
            i = add_line(cmd,n,'subcenter')
            n = n + 1
	    write(cmd(n),*) subcenter
	endif

        i = add_line(cmd,n,'-append')
        i = add_line(cmd,n,'-grib_out')
        i = add_line(cmd,n,grbfile)

!        do i = 1, n
!           write(*,*) 'grb2_wrt>>> i=',i,cmd(i)
!        enddo

!        write(*,*) '>>> grb2_wrt >> wgrib2c'
        grb2_wrt = wgrib2c(n,cmd, len(cmd(1)))
        if (grb2_wrt .ne. 0) then
             write(*,*) 'ERROR: grb2_wrt err=', grb2_wrt, ' file=',grbfile
	endif
!        write(*,*) '<<< grb2_wrt << wgrib2c'

        return
        end function


!
!        grb2_inq: grib2 inquire
!               give match strings
!               return number of matches
!               use optional arguments to get more info including grid data
!
        integer function grb2_inq(grbfile, invfile, &
            a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11, &
            a12,a13,a14,a15,a16,a17,a18,a19,a20, &
            npts,nx,ny,nmatch,msgno,submsg,data1,nread,data2,lat,lon,order)

	use wgrib2lowapi
        implicit none

        character (len=*), intent(in):: grbfile, invfile
        character (len=*), optional, intent(in):: a1,a2,a3,a4,a5,a6,a7,a8,a9,a10
        character (len=*), optional, intent(in):: a11,a12,a13,a14,a15,a16,a17
        character (len=*), optional, intent(in):: a18,a19,a20

        integer, optional, intent(out) :: npts, nx, ny, nmatch, nread, msgno
        integer, optional, intent(out) :: submsg
	integer (C_SIZE_T) :: buffer_size
        real, optional, intent(out) :: data1(:)
        real, optional, allocatable, intent(inout) :: data2(:,:), lat(:,:), lon(:,:)
        character (len=*), optional, intent(in):: order
 
        character (len=100) :: lines(20), cmd(80)
        character (len=53) :: grid_id

        integer i, j, m, n, ierr
        integer isubmsg
	integer (C_SIZE_T) :: ndata, nnx, nny, imsgno, one

        m = grb2_var_args(lines,0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11, &
          a12,a13,a14,a15,a16,a17,a18,a19,a20)

        one = 1
        n = 0
!
        i = add_line(cmd,n,grbfile)
        i = add_line(cmd,n,'-i_file')
        i = add_line(cmd,n,invfile)
        i = add_line(cmd,n,'-rewind_init')
        i = add_line(cmd,n,invfile)

!	parse the match commands

        do j = 1, m
            i = add_line(cmd,n,'-egrep')
            i = add_line(cmd,n,lines(j))        
        enddo

!	add call to get grid info

        i = add_line(cmd,n,'-ftn_api_fn0')
        i = add_line(cmd,n,'-last0')
        i = add_line(cmd,n,'@mem:19')

	if (present(order)) then
	    if (order == 'we:ns') then
                i = add_line(cmd,n,'-order')
                i = add_line(cmd,n,'we:ns')
	    else if (order == 'raw') then
                i = add_line(cmd,n,'-order')
                i = add_line(cmd,n,'raw')
	    else if (order /= 'we:sn') then
		write(*,*) '*** FATAL ERROR: order should be we:ns, we:sn or raw. not', order
		stop 8
	    endif
	endif

        if (present(data1).or.present(data2)) then
            i = add_line(cmd,n,'-rpn')
            i = add_line(cmd,n,'sto_0')
        endif

        if (present(lon)) then
            i = add_line(cmd,n,'-rpn')
            i = add_line(cmd,n,'rcl_lon:sto_1')
        endif

        if (present(lat)) then
            i = add_line(cmd,n,'-rpn')
            i = add_line(cmd,n,'rcl_lat:sto_2')
        endif

        do i = 1, n
            write(*,*) 'grb2_inq>>> ',cmd(i)
        enddo

        ierr = wgrib2c(n,cmd, len(cmd(1)))
        if (ierr .ne. 0) then
            grb2_inq = -1
            return
        endif

        buffer_size = 53
	ierr = wgrib2_get_mem_buffer(19, buffer_size, grid_id)
	if (ierr.ne.0) then
           grb2_inq = 0
           return
        endif
	write(*,*) 'grid_id=',grid_id

        read(grid_id,'(i8,5(1x,i8))',end=100,err=100) grb2_inq, ndata, nnx, nny, imsgno, isubmsg
        goto 110
100     continue
        write(*,*) 'FATAL ERROR: grb2_inq reading grid info'
        grb2_inq = -1
        return
110     continue
	write(*,*) 'ndata=',ndata, nnx, nny

        if (present(npts)) then
           npts = ndata
        endif
        if (present(nx)) then
           nx = nnx
        endif
        if (present(ny)) then
           ny = nny
        endif
        if (present(nmatch)) then
            nmatch = grb2_inq
        endif
        if (present(msgno)) then
            msgno = imsgno
        endif
        if (present(submsg)) then
            submsg = isubmsg
        endif

        if (present(data1)) then
            if (ndata.le.size(data1)) then
                data1 = -1
                i = wgrib2_get_reg_data(data1, ndata, 0)
                if (i.ne.0) then
                    write(*,*) 'FATAL ERROR: get_reg_n ',i
        	    grb2_inq = -1
        	    return
                endif
            endif
            if (present(nread)) then
                nread = ndata
            endif
        endif
!
!       does not work for staggered grids
!
        if (present(data2)) then
            if (max0(nnx,one)*max0(nny,one) .ne. ndata) then
                write(*,*) '*** FATAL ERROR: ndata != nx*ny ', ndata, nnx, nny
                grb2_inq = -1
                return
            endif
            if (.not.allocated(data2)) then
                allocate(data2(max0(nnx,one),max0(nny,one)))
            else
                if (size(data2,1).ne.max0(nnx,one) .or. size(data2,2).ne.max0(nny,one)) then
                    deallocate(data2)
                    allocate(data2(max0(nnx,one),max0(nny,one)))
                endif
            endif
            i = wgrib2_get_reg_data(data2, ndata, 0)
            if (i.ne.0) then
                write(*,*) 'FATAL ERROR: get_reg_n ',i
                grb2_inq = -1
                return
            endif
            if (present(nread)) then
                nread = ndata
            endif
        endif

	if (present(lon)) then
            if (max0(nnx,one)*max0(nny,one) .ne. ndata) then
                write(*,*) '*** FATAL ERROR: ndata != nx*ny ', ndata, nnx, nny
                grb2_inq = -1
                return
            endif
            if (.not.allocated(lon)) then
                allocate(lon(max0(nnx,one),max0(nny,one)))
            else
                if (size(lon,1).ne.max0(nnx,one) .or. size(lon,2).ne.max0(nny,one)) then
                    deallocate(lon)
                    allocate(lon(max0(nnx,one),max0(nny,one)))
                endif
            endif
            i = wgrib2_get_reg_data(lon, ndata, 1)
            if (i.ne.0) then
                write(*,*) 'FATAL ERROR: get_reg_n ',i
                grb2_inq = -1
                return
            endif
        endif

        if (present(lat)) then
            if (max0(nnx,one)*max0(nny,one) .ne. ndata) then
                write(*,*) '*** FATAL ERROR: ndata != nx*ny ', ndata, nnx, nny
                grb2_inq = -1
                return
            endif
            if (.not.allocated(lat)) then
                allocate(lat(max0(nnx,one),max0(nny,one)))
            else
                if (size(lat,1).ne.max0(nnx,one) .or. size(lat,2).ne.max0(nny,one)) then
                    deallocate(lat)
                    allocate(lat(max0(nnx,one),max0(nny,one)))
                endif
            endif
            i = wgrib2_get_reg_data(lat, ndata, 2)
            if (i.ne.0) then
                write(*,*) 'FATAL ERROR: get_reg_n ',i
                grb2_inq = -1
                return
            endif
	endif

        return
        end function

!	grb2_rewind(file)
!	     rewinds internal wgrib2 file  .. grid is from N to S, default is from S to N
!
        integer function grb2_rewind(infile)
	use wgrib2lowapi

        character (len=*), optional, intent(in):: infile
        character (len=100) :: lines(2)

        lines(1) = '-rewind_init'
        lines(2) = infile
	grb2_rewind = wgrib2c(2,lines,100)
	return
	end function grb2_rewind
!
!        grb2_ncep_uv: run wgrib2 infile -ncep_uv outfile
!               combines u and v together
!

end module
