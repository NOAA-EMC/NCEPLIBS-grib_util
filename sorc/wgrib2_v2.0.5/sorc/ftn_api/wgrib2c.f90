module wgrib2c
    interface
	integer (c_int) function wgrib2c(n, cmd)
	    bind(c, name='wgrib2c')
            USE ISO_C_BINDING
	    integer (c_int), value :: n
            character (len=100) :: cmd(n)
	end function wgrib2c
    end interface
end module wgrib2c
