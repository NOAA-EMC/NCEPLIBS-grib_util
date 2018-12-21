! fortran lowapi for reading and writing grib2
! 10/2015  Wesley Ebisuzaki   Public Domain
!
! fortran 2003 / fortran 95 + TR 15581  API to read and write grib2 files
!
!   requirements:
!        f2003 or f95 + TR 15581  (allowing subroutine to deallocate and allocate)
!        callable wgrib2
!        fort_wgrib2.c (wgrib2c wrapper for callable wgrib2)
!
! Provides:
!    wgrib2a(list of strings) :: same as $ wgrib2 [list of strings]
!        not used by grb2_* routines
!    wgrib2c(argc, argv) :: same as $ wgrib2 [list of strings]
!

module wgrib2lowapi
    USE ISO_C_BINDING
    interface
        integer (C_SIZE_T) function wgrib2_get_reg_size(reg) bind(C)
            USE ISO_C_BINDING
            integer (C_INT), value :: reg
        end function wgrib2_get_reg_size

        integer (C_INT) function wgrib2_get_reg_data(data, ndata, reg) bind(C)
            USE ISO_C_BINDING
            integer (C_SIZE_T), value :: ndata
            integer (C_INT), value :: reg
            real (C_FLOAT) :: data(ndata)
        end function wgrib2_get_reg_data

        integer (C_INT) function wgrib2_set_reg(data, ndata, reg) bind(C)
            USE ISO_C_BINDING
            integer (C_SIZE_T), value :: ndata
            integer (C_INT), value :: reg
            real (C_FLOAT) :: data(ndata)
        end function wgrib2_set_reg

        integer (C_SIZE_T) function wgrib2_get_mem_buffer_size(n) bind(C)
            USE ISO_C_BINDING
            integer (C_INT), value :: n
        end function wgrib2_get_mem_buffer_size

	integer (C_INT) function wgrib2_get_mem_buffer(n, size_buffer, buffer) bind(C)
            USE ISO_C_BINDING
            integer (C_INT), value :: n
            integer (C_SIZE_T), value :: size_buffer
	    character (kind=c_char) :: buffer(*)
        end function wgrib2_get_mem_buffer

	integer (C_INT) function wgrib2_set_mem_buffer(n, size_buffer, buffer) bind(C)
            USE ISO_C_BINDING
            integer (C_INT), value :: n
            integer (C_SIZE_T), value :: size_buffer
	    character (kind=c_char) :: buffer(*)
        end function wgrib2_set_mem_buffer

	integer (C_INT) function wgrib2c(n, buffer, len) bind(C)
	    USE ISO_C_BINDING
            integer (C_INT), value :: n, len
	    character (kind=c_char) :: buffer(*)
	end function wgrib2c

    end interface
end module
