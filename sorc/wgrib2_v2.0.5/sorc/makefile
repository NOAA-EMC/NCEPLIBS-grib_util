# REQUIRES GMAKE!!!!
#
# wgrib2 uses components of varying copyrights and licences.  See wgrib2/LICENSE-wgrib2
#
# makefile for wgrib2
# 
# compiles every #@?! library needed by wgrib2
# then tries to compile wgrib2
#
# (1) must use gnu-make
# (2) the environment variable FC must be set to fortran-90 compiler or 
#        higher in order to compile the optional netcdf and the optional IPOLATES
#        not needed if netcdf and IPOLATES is not used
# (3) the environment veriable CC must be set to the C compiler
#
#
# mod 1/07 M. Schwarb (libgrib2c name change)
# mod 4/09 W. Ebisuzaki (use config.h)
# mod 6/10 W. Ebisuzaki ipolates
# mod 8/11 W. Ebisuzaki support environment variable FC={fortran 90+ compiler}
#              needed by optional netcdf4 and ipolates
# mod 3/12 W. Ebisuzaki support openmp, gctpc
# mod 8/12 M. Schwarb  gunzip -n -f,  cd "$var"
# mod 10/12 W. Ebisuzaki
# mod 7/13 W. Ebisuzaki added got netcdf4 working again, added subdirectroy lib, bin, include, man
# mod 11/14 W. Ebisuzaki added target lib, make callable wgrib2
# mod 05/16 G. Schnee add support for template 5.42 compression using libaec
#
#   Configuration
#
# NETCDF3: link in netcdf3 library to write netcdf3 files
#    change: USE_NETCDF3=1 and USE_NETCDF4=0 in configuration below
#
# NETCDF4: link in netcdf4 library to write netcdf3/4 files
#    change: USE_NETCDF3=0 and USE_NETCDF4=1 in configuration below
#    need to download netcdf4 and hdf5 libraries and to put into grib2 directory
#    need fortran90+ compiler (FC)
#
# IPOLATES: link in IPOLATES library to interpolate to new grids
#    change: USE_IPOLATES=1 in configuration below
#    need fortran90+ compiler (FC)
#    need to modify makefile and perhaps source code
#
#  MYSQL: link in interface to MySQL to write to mysql database
#    change: USE_MYSQL=1 in configuration below
#    need to have mysql installed
#    may need to modify makefile
#  
#  UDF: add commands for user-defined functions and shell commands
#    change: USE_UDF=1 in configuration below
#
#  REGEX: use regular expression library (POSIX-2), on by default
#    change: USE_REGEX=0 if REGEX library is not available
#     (preferred: get gnu source code to REGEX library)
#
#  TIGGE: ability for TIGGE-like variable names, on by default
#    change: USE_TIGGE=0 to turn off (configuration below)
#
#  USE_PROJ4: the proj4 library is used to confirm that the
#    gctpc code is working right.  
#
#  USE_AEC: enable use of the libaec library for packing with GRIB2 template
#    5.42 (https://gitlab.dkrz.de/k202009/libaec/)
#
#  USE_G2CLIB: include NCEP's g2clib (mainly for testing purposes)
#              USE_G2CLIB = 1, g2clib can be used for decoding by -g2clib 1
#                  requires USE_PNG=1 and USE_JASPER=1
#
#  DISABLE_TIMEZONE: some machines do not support timezones (TZ).
#     if your machine does not support TZ, set DISABLE_TIMEZONE=1
#    this disables the -unix_time option
#
# on NCEP AIX
# export CC=/usr/vacpp/bin/xlc_r
# export CPP=/usr/bin/cpp
# export FC=xlf_r
#
# for clang
# export CC=clang
# export FC="gfortran -fplugin=dragonegg"
#
# for OS-X: uncomment line for makefile -f scripts/makefile.darwin
#
SHELL=/bin/sh

#export CC=gcc
#export FC=gfortran
# 
# netcdf3: write netcdf files with netcdf-3 library
# netcdf4: write netcdf files with netcdf-4 library
# regex: regular expression package used by (match,not), POSIX-2
# tigge: enable -tigge option for tigge names
# mysql: write to mysql files
# ipolates: fortran interpolation library
# udf: user defined functions
# openmp: multicore support using OpenMP
# proj4: use proj4 for geolocation
# wmo_validation: used for testing new templates
# fortran_api:  fortran api for callable wgrib2, requires a fortran 2003
#               feature, subroutines can allocated/deallocated arrays
# disable_timezone: some OS do not handle time zones in POSIX maner
#
# the flags are stored in wgrib2/config.h
#

# Warning do not set both USE_NETCDF3 and USE_NETCDF4 to one
USE_NETCDF3=1
USE_NETCDF4=0
USE_REGEX=1
USE_TIGGE=1
USE_MYSQL=0
USE_IPOLATES=0
USE_UDF=0
USE_OPENMP=1
USE_PROJ4=0
USE_WMO_VALIDATION=0
DISABLE_TIMEZONE=0
MAKE_FTN_API=0

USE_G2CLIB=0
USE_PNG=1
USE_JASPER=1
USE_AEC=1

# Add any customization comments, appears in help and config pages
BUILD_COMMENTS=stock build

# often enviroment variable FC=fortran compiler, is set to f77, need f90 compiler
#
need_ftn=0

cwd:=${CURDIR}
lib:=${cwd}/lib
wLDFLAGS:=-L${lib}
a:=$(shell mkdir -p ${lib})
wCPPFLAGS:=-I${cwd}/include
netcdf3CPPFLAGS:=-I${cwd}/include
wFFLAGS:=""

a:=$(shell mkdir -p ${lib})
a:=$(shell mkdir -p ${cwd}/include)

CONFIG_H=${cwd}/wgrib2/config.h
a:=$(shell echo "/* config.h */" > ${CONFIG_H})

ifeq ($(USE_G2CLIB),1)
  ifeq ($(USE_PNG),0)
    $(error ERROR, USE_G2CLIB = 1 requires USE_PNG = 0)
  endif
  ifeq ($(USE_JASPER),0)
    $(error ERROR, USE_G2CLIB = 1 requires USE_JASPER = 0)
  endif
endif

ifeq ($(USE_NETCDF3),1)
  ifeq ($(USE_NETCDF4),1)
    $(error ERROR, USE_NETCDF3 = 1 or USE_NETCDF4 = 1, not both)
  endif
endif

ifeq ($(USE_REGEX),1)
   a:=$(shell echo "\#define USE_REGEX" >> ${CONFIG_H})
else
   a:=$(shell echo "//\#define USE_REGEX" >> ${CONFIG_H})
endif

ifeq ($(USE_TIGGE),1)
   a:=$(shell echo "\#define USE_TIGGE" >> ${CONFIG_H})
else
   a:=$(shell echo "//\#define USE_TIGGE" >> ${CONFIG_H})
endif

ifeq ($(DISABLE_TIMEZONE),1)
   a:=$(shell echo "\#define DISABLE_TIMEZONE" >> ${CONFIG_H})
else
   a:=$(shell echo "//\#define DISABLE_TIMEZONE" >> ${CONFIG_H})
endif

ifeq ($(USE_IPOLATES),1)
   need_ftn=1
endif
ifeq ($(MAKE_FTN_API),1)
   need_ftn=1
endif

ifeq ($(USE_UDF),1)
   a:=$(shell echo "\#define USE_UDF" >> ${CONFIG_H})
else
   a:=$(shell echo "//\#define USE_UDF" >> ${CONFIG_H})
endif

# C compile and load commmands
# wCPPFLAGS has the directory of the includes 
# wLDFLAGS has the directory/name of the library

ifeq ($(findstring gcc,$(notdir $(CC))),gcc)
   wCPPFLAGS+=-Wall -Wmissing-prototypes -Wold-style-definition -Werror=format-security -ffast-math  -O3
   netcdf3CPPFLAGS+=-Wall -Wmissing-prototypes -Wold-style-definition -Werror=format-security -O3
   hdf5CFLAGS+=-Wall -Wmissing-prototypes -Wold-style-definition -O1
endif
ifeq ($(findstring opencc,$(notdir $(CC))),opencc)
   wCPPFLAGS+=-O3 -Wall -ffast-math -opencc
   netcdf3CPPFLAGS+=-O3 -Wall -ffast-math -opencc
   hdf5CFLAGS+=-O1 -Wall -opencc
endif
ifeq ($(findstring icc,$(notdir $(CC))),icc)
   wCPPFLAGS+=-O2
   netcdf3CPPFLAGS+=-O2
   hdf5CFLAGS+=-O2
   $(error ERROR, makefile does not make jasper correctly with intel compiler)
endif
ifeq ($(findstring pgcc,$(notdir $(CC))),pgcc)
   wCPPFLAGS+=-O2
   netcdf3CPPFLAGS+=-O2
   hdf5CFLAGS+=-O2
   $(error ERROR, makefile does not make jasper correctly with portland compiler)
endif
ifeq ($(findstring xlc_r,$(notdir $(CC))),xlc_r)
   wCPPFLAGS+=-O3
   netcdf3CPPFLAGS+=-O3
   hdf5CFLAGS+=-O2
endif
ifeq ($(findstring clang,$(notdir $(CC))),clang)
   wCPPFLAGS+=-O3 -pedantic
   netcdf3CPPFLAGS+=-O3
   hdf5CFLAGS+=-O2
endif

# IPOLATES needs to be linked before fortran libraries
ifeq ($(USE_IPOLATES),1)
#   for HWRF
#   ip:=${cwd}/iplib_hwrf
#   iplib:=${lib}/libipolate_hwrf.a
#   wLDFLAGS+=-lipolate_hwrf

#  normal IPOLATES
#   ip:=${cwd}/iplib.v3.0.0
   ip:=${cwd}/iplib.2012
   iplib:=${lib}/libipolate.a
   wLDFLAGS+=-lipolate

   a:=$(shell echo "\#define USE_IPOLATES" >> ${CONFIG_H})
   a:=$(shell echo "\#define IPOLATES_LIB \"`basename ${ip}`\"" >> ${CONFIG_H})
else
   a:=$(shell echo "//\#define USE_IPOLATES" >> ${CONFIG_H})
endif


ifeq ($(need_ftn),1)

   ifndef FC
     $(error ERROR, configuration requires fortran90 compiler which is set by environement variable FC)
   endif

#  for compiling with fortran library
#  wLDFLAGS+= (libraries need by the fortran code)
#  wCPPFLAGS+= -D(FORTRAN Name)   see New_grid.c

# for G95 - personal system
   ifeq ($(findstring g95,$(notdir $(FC))),g95)
      libf95:=$(shell $(FC) -print-file-name=libf95.a)
      ifeq "$(libf95)" ""
          $(error ERROR, g95 missing? $(FC))
      endif
      wLDFLAGS+=-L$(dir ${libf95}) -lf95
      wCPPFLAGS+=-DG95
      wFFLAGS+=-O2
    endif

# for gfortran - redhat, ubuntu and cygwin 1.7.7-1
   ifeq ($(findstring gfortran,$(notdir $(FC))),gfortran)
      GFORTLIB:=${shell $(FC)  -print-file-name=libgfortran.so}
      ifeq "$(GFORTLIB)" ""
          $(error ERROR, gfortran missing? $(FC))
      endif
      GFORTLIBDIR:=${shell dirname ${GFORTLIB}}
      wLDFLAGS+=-L$(GFORTLIBDIR)
      wLDFLAGS+=-lgfortran
      wCPPFLAGS+=-DGFORTRAN
      wFFLAGS+=-O2
   endif

# for open64 fortran - personal system
   ifeq ($(findstring openf95,$(notdir $(FC))),openf95)
      wLDFLAGS+=/export/cpc-lw-webisuzak/wd51we/opt/x86_open64-4.5.1/lib/gcc-lib/x86_64-open64-linux/4.5.1/libfortran.a
      wLDFLAGS+=/export/cpc-lw-webisuzak/wd51we/opt/x86_open64-4.5.1/lib/gcc-lib/x86_64-open64-linux/4.5.1/libffio.a
      wCPPFLAGS+=-DOPENF95
      wFFLAGS+=-O2
   endif

# for portland f95
   ifeq ($(notdir $(FC)),pgf95)
      wCPPFLAGS+=-DPGF95
      wFFLAGS+=-O2
   endif

# intel fortran
   ifeq ($(notdir $(FC)),ifort)
      wCPPFLAGS+=-DIFORT -cxxlib
      wLDFLAGS+=-lifcore -lc -limf -lintlc
      wFFLAGS+=-O2 -nofor_main  -cxxlib
   endif

# NCEP CCS:
   ifeq ($(findstring xlf_r,$(notdir $(FC))),xlf_r)
      wLDFLAGS+=-L/usr/lib - -lxlf90_r
      wCPPFLAGS+=-DXLF
      wFFLAGS+=-O2
   endif

   ifeq ($(wFFLAGS),"")
      $(error ERROR, fortran compiler (enironment vararible FC) is not recognized)
   endif
endif



# grib2c library
# g2clib is required if USE_G2CLIB, USE_PNG or USE_JASPER
# USE_G2CLIB and USE_JASPER implies USE_PNG
#   

ifeq ($(USE_G2CLIB),1)
   g:=${cwd}/g2clib-1.4.0
   glib:=${lib}/libgrib2c.a
   wLDFLAGS+=-lgrib2c
   wCPPFLAGS+=-I$g
endif


ifeq ($(USE_G2CLIB),1)
   a:=$(shell echo "\#define USE_G2CLIB" >> ${CONFIG_H})
else
   a:=$(shell echo "//\#define USE_G2CLIB" >> ${CONFIG_H})
endif

# gctpc library
gctpc:=${cwd}/gctpc
gctpcsrc:=gctpc20a.tgz
gctpclib:=${lib}/libgeo.a
wLDFLAGS+=-lgeo
# wCPPFLAGS+=-I${gctpc}/source

# proj4 library
ifeq ($(USE_PROJ4),1)
   proj4:=${cwd}/proj-4.8.0
   proj4src:=${cwd}/proj-4.8.0.tar.gz
   proj4lib:=${lib}/libproj.a
   wLDFLAGS+=-lproj
#   wCPPFLAGS+=-I${proj4}/src
   a:=$(shell echo "\#define USE_PROJ4" >> ${CONFIG_H})
else
   a:=$(shell echo "//\#define USE_PROJ4" >> ${CONFIG_H})
endif

# Jasper

ifeq ($(USE_JASPER),1)
   j=${cwd}/jasper-1.900.1
#   jsrc=jasper-fedora19.tgz
   jsrc=jasper-1.900.1-14ubuntu3.2.debian.tgz
   jlib=${lib}/libjasper.a
   wLDFLAGS+=-ljasper
# wCPPFLAGS+=-I$j/src/libjasper/include
   a:=$(shell echo "\#define USE_JASPER" >> ${CONFIG_H})
else
   a:=$(shell echo "//\#define USE_JASPER" >> ${CONFIG_H})
endif

# AEC

ifeq ($(USE_AEC),1)
   aecdir=${cwd}/libaec-0.3.3
   aecsrc=libaec-0.3.3.tar.gz
   aeclib=${lib}/libaec.a
   wLDFLAGS+=-laec
   a:=$(shell echo "\#define USE_AEC" >> ${CONFIG_H})
else
   a:=$(shell echo "//\#define USE_AEC" >> ${CONFIG_H})
endif

ifeq ($(USE_NETCDF3),1)
   n:=${cwd}/netcdf-3.6.3
   netcdfsrc=netcdf-3.6.3.tar.gz
   nlib:=${lib}/libnetcdf.a
   wLDFLAGS+=-lnetcdf
#   wCPPFLAGS+=-I$n/libsrc
   a:=$(shell echo "\#define USE_NETCDF3" >> ${CONFIG_H})
else
   a:=$(shell echo "//\#define USE_NETCDF3" >> ${CONFIG_H})
endif

ifeq ($(USE_NETCDF4),1)
   n4:=${cwd}/netcdf-4.3.3
   netcdf4src=netcdf-4.3.3.tar.gz
   n4lib:=${lib}/libnetcdf.a
   h5:=${cwd}/hdf5-1.8.16
   hdf5src:=hdf5-1.8.16.tar.gz
   h5lib:=${lib}/libhdf5.a
   wLDFLAGS+=-lnetcdf -lhdf5_hl -lhdf5 -ldl
#   wCPPFLAGS+=-I${n4}/include -I${h5}/src -I${h5}/hl/src
   a:=$(shell echo "\#define USE_NETCDF4" >> ${CONFIG_H})
else
   a:=$(shell echo "//\#define USE_NETCDF4" >> ${CONFIG_H})
endif

ifeq ($(USE_MYSQL),1)
   wCPPFLAGS+=`mysql_config --cflags`
   wLDFLAGS+=`mysql_config --libs`
   a:=$(shell echo "\#define USE_MYSQL" >> ${CONFIG_H})
else
   a:=$(shell echo "//\#define USE_MYSQL" >> ${CONFIG_H})
endif

# OPENMP .. only select configurations

ifeq ($(USE_OPENMP),1)
   ifeq ($(findstring gcc,$(notdir $(CC))),gcc)
      a:=$(shell echo "\#define USE_OPENMP" >> ${CONFIG_H})
      wCPPFLAGS+=-fopenmp
      ifeq ($(findstring gfortran,$(notdir $(FC))),gfortran)
	 wFFLAGS+=-fopenmp
      endif
   endif
   ifeq ($(findstring opencc,$(notdir $(CC))),opencc)
      ifeq ($(findstring openf95,$(notdir $(FC))),openf95)
	 a:=$(shell echo "\#define USE_OPENMP" >> ${CONFIG_H})
	 wCPPFLAGS+=-fopenmp
	 wFFLAGS+=-fopenmp
      endif
   endif
   ifeq ($(notdir $(CC)),icc)
      ifeq ($(notdir $(FC)),ifort)
	 a:=$(shell echo "\#define USE_OPENMP" >> ${CONFIG_H})
	 wCPPFLAGS+=-openmp
	 wFFLAGS+=-openmp
      endif
   endif
   ifeq ($(findstring xlc_r,$(notdir $(CC))),xlc_r)
      ifeq ($(findstring xlf_r,$(notdir $(FC))),xlf_r)
	 a:=$(shell echo "\#define USE_OPENMP" >> ${CONFIG_H})
	 wCPPFLAGS+=-qsmp=omp
	 wFFLAGS+=-qsmp=omp
      endif
   endif
endif


# save fortran and C compiler names in config.h file

ifeq ($(findstring gcc,$(notdir $(CC))),gcc)
  a:=$(shell echo "\#define CC \"`${CC} --version | head -n 1`\"" >> ${CONFIG_H})
else
   a:=$(shell echo "\#define CC \"${CC}\"" >> ${CONFIG_H})
endif

a:=$(shell echo "\#define FORTRAN \"${FC}\"" >> ${CONFIG_H})
a:=$(shell echo "\#define BUILD_COMMENTS \"${BUILD_COMMENTS}\"" >> ${CONFIG_H})

# png 

ifeq ($(USE_PNG),1)
   p=${cwd}/libpng-1.2.56
   psrc=${cwd}/libpng-1.2.56.tar.gz
   plib=${lib}/libpng.a
   wLDFLAGS+=-lpng
# wCPPFLAGS+=-I$p
   a:=$(shell echo "\#define USE_PNG" >> ${CONFIG_H})

# z

   z=${cwd}/zlib-1.2.8
   zlib=${lib}/libz.a
   wLDFLAGS+=-lz
   # wCPPFLAGS+=-I$z
else
   a:=$(shell echo "//\#define USE_PNG" >> ${CONFIG_H})
endif

# WMO Validation testing mode
ifeq ($(USE_WMO_VALIDATION),1)
   a:=$(shell echo "\#define WMO_VALIDATION" >> ${CONFIG_H})
else
   a:=$(shell echo "//\#define WMO_VALIDATION" >> ${CONFIG_H})
endif



wLDFLAGS+=-lm
wCPPFLAGS+=-I/usr/include ${CPPFLAGS}

# -----------------------------------------------------

# check if make is GNU make else use gmake
make_is_gnu:=$(word 1,$(shell make -v))
ifeq ($(make_is_gnu),GNU)
   MAKE:=make
else
   MAKE:=gmake
endif


w=wgrib2
prog=$w/wgrib2

all:	${netcdf4src} ${hdf5src} ${prog} aux_progs/gmerge aux_progs/smallest_grib2 aux_progs/smallest_4


${prog}:        $w/*.c $w/*.h ${jlib} ${aeclib} ${nlib} ${zlib} ${plib} ${h5lib} ${glib} ${n4lib} ${iplib} ${gctpclib} ${proj4lib}
	cd "$w" && export LDFLAGS="${wLDFLAGS}" && export CPPFLAGS="${wCPPFLAGS}" && ${MAKE}

fast:        $w/*.c $w/*.h ${jlib} ${aeclib} ${nlib} ${zlib} ${plib} ${h5lib} ${glib} ${n4lib} ${iplib} ${gctpclib} ${proj4lib}
	cd "$w" && export LDFLAGS="${wLDFLAGS}" && export CPPFLAGS="${wCPPFLAGS}" && ${MAKE} fast

lib:        $w/*.c $w/*.h ${jlib} ${aeclib} ${nlib} ${zlib} ${plib} ${h5lib} ${glib} ${n4lib} ${iplib} ${gctpclib} ${proj4lib}
	cd "$w" && export LDFLAGS="${wLDFLAGS}" && export CPPFLAGS="${wCPPFLAGS}" && export FFLAGS="${wFFLAGS}" && ${MAKE} lib
	cp wgrib2/libwgrib2.a lib/libwgrib2_small.a
ifeq ($(MAKE_FTN_API),1)
	export CPPFLAGS="${wCPPFLAGS}" && export FFLAGS="${wFFLAGS}" && cd ftn_api && make
	cp ftn_api/wgrib2api.mod lib/
	cp ftn_api/wgrib2lowapi.mod lib/
	cp ftn_api/libwgrib2_api.a lib/
else
	touch lib/wgrib2api.mod && rm lib/wgrib2api.*
endif
	cp wgrib2/wgrib2_api.h lib/
	cd lib ; touch libwgrib2.a ; rm libwgrib2.a ; ar crsT libwgrib2.a *.a

${jlib}:
	cp ${jsrc}  tmpj.tar.gz
	gunzip -n -f tmpj.tar.gz
	tar -xvf tmpj.tar
	rm tmpj.tar
	cd "$j" && export CFLAGS="${wCPPFLAGS}" && ./configure --without-x --disable-libjpeg --disable-opengl --prefix=${cwd} && ${MAKE} check install

${aeclib}:
	cp ${aecsrc} tmpaec.tar.gz
	gunzip -n -f tmpaec.tar.gz
	tar -xvf tmpaec.tar
	rm tmpaec.tar
	cd "${aecdir}" && export CFLAGS="${wCPPFLAGS}" && ./configure --disable-shared --prefix=${cwd} && ${MAKE} check install


${plib}:	${zlib}
	cp ${psrc} tmpp.tar.gz
	gunzip -n -f tmpp.tar.gz
	tar -xvf tmpp.tar
	rm tmpp.tar
#       for OSX
#	export LDFLAGS="-L$z" && cd "$p" && export CPPFLAGS="${wCPPFLAGS}" && make -f scripts/makefile.darwin
#	for everybody else
#	export LDFLAGS="-L${lib}" && cd "$p" && export CPPFLAGS="${wCPPFLAGS}" && ./configure --disable-shared --prefix=${cwd} && ${MAKE} check install
	export LDFLAGS="-L${lib}" && cd "$p" && export CPPFLAGS="${wCPPFLAGS} -DPNG_USER_WIDTH_MAX=200000000L" && ./configure --disable-shared --prefix=${cwd} && ${MAKE} check install

${zlib}:
	cp $z.tar.gz tmpz.tar.gz
	gunzip -f tmpz.tar.gz
	tar -xvf tmpz.tar
	rm tmpz.tar
	cd "$z" && export CFLAGS="${wCPPFLAGS}" && ./configure --prefix=${cwd} --static && ${MAKE} check install

${glib}:	${jlib} ${plib} ${zlib}
	touch ${glib}
	rm ${glib}
	cd "$g" && export CPPFLAGS="${wCPPFLAGS}" && ${MAKE} && cp libgrib2c.a ${lib}

${gctpclib}:
	cp ${gctpcsrc} tmpgctpc.tar.gz
	gunzip -n -f tmpgctpc.tar.gz
	tar -xvf tmpgctpc.tar
	rm tmpgctpc.tar
	cp makefile.gctpc proj.h sominv.c somfor.c ${gctpc}/source/
	cd "${gctpc}/source" && export CPPFLAGS="${wCPPFLAGS}" && ${MAKE} -f makefile.gctpc
	cp ${gctpc}/source/libgeo.a ${lib}
	cp ${gctpc}/source/proj.h ${cwd}/include/

${proj4lib}:
	cp ${proj4src}  tmpproj4.tar.gz
	gunzip -f tmpproj4.tar.gz
	tar -xvf tmpproj4.tar
	rm tmpproj4.tar
	cd ${proj4} && ./configure --disable-shared --prefix=${cwd} && ${MAKE} check install

${nlib}:
	cp ${netcdfsrc} tmpn.tar.gz
	gunzip -f tmpn.tar.gz
	tar -xvf tmpn.tar
	rm tmpn.tar
	cd $n && export CPPFLAGS="${netcdf3CPPFLAGS}" && ./configure --enable-c-only --prefix=${cwd} && ${MAKE} check install

${n4lib}:	${zlib} ${netcdf4src} ${h5lib}
	cp ${netcdf4src} tmpn4.tar.gz
	gunzip -n -f tmpn4.tar.gz
	tar -xvf tmpn4.tar
	rm tmpn4.tar
	cd "${n4}" && export CPPFLAGS="${wCPPFLAGS}" && export LDFLAGS="-L${lib}" && export LIBS="-lhdf5 -ldl" && ./configure --disable-fortran --disable-cxx --disable-dap --enable-netcdf-4 --prefix=${cwd} --disable-shared && ${MAKE} install

${netcdf4src}:
	$(error ERROR, get netcdf4 source by "wget ftp://ftp.unidata.ucar.edu/pub/netcdf/netcdf-4.3.3.tar.gz" )

${h5lib}:	${hdf5src}
	cp ${hdf5src} tmph5.tar.gz
	gunzip -n -f tmph5.tar.gz
	tar -xvf tmph5.tar
	rm tmph5.tar
	cd "${h5}" && export CFLAGS="${hdf5CFLAGS}" && export LDFLAGS="${LDFLAGS}" && ./configure --disable-shared --with-zlib=$z --prefix=${cwd} && ${MAKE} all check install


${hdf5src}:
	$(error ERROR, get hdf5 source by "wget http://www.hdfgroup.org/ftp/HDF5/releases/hdf5-1.8.16/src/hdf5-1.8.16.tar.gz" )

${iplib}:
	cd "${ip}" && export FFLAGS="${wFFLAGS}" && ${MAKE} && cp libipolate.a ${iplib}

aux_progs/gmerge:	aux_progs/gmerge.c		
	cd aux_progs && ${MAKE} -f gmerge.make

aux_progs/smallest_grib2:	aux_progs/smallest_grib2.c
	cd aux_progs && ${MAKE} -f smallest_grib2.make

aux_progs/smallest_4:	aux_progs/smallest_4.c
	cd aux_progs && ${MAKE} -f smallest_4.make

clean:
	mkdir -p ${lib} && rm -r ${lib}
	mkdir -p ${cwd}/bin && rm -r ${cwd}/bin
	mkdir -p ${cwd}/include && rm -r ${cwd}/include
	mkdir -p ${cwd}/man && rm -r ${cwd}/man
	cd $w && ${MAKE} clean
	mkdir -p ${gctpc} && rm -rf ${gctpc}
ifeq ($(USE_PNG),1)
	mkdir -p $z && rm -rf $z
endif
ifeq ($(USE_JASPER),1)
	mkdir -p $j && rm -rf $j
endif
ifeq ($(USE_AEC),1)
	mkdir -p ${aecdir} && rm -r ${aecdir}
endif
ifeq ($(USE_G2CLIB),1)
	mkdir -p $g && cd $g && touch junk.a junk.o && rm *.o *.a
endif
ifeq ($(USE_IPOLATES),1)
	echo "cleanup ${ip}"
	mkdir -p ${ip} && touch ${ip}/junk.o ${ip}/junk.a ${ip}/junk.mod && rm ${ip}/*.o ${ip}/*.a ${ip}/*.mod
endif

ifeq ($(USE_NETCDF3),1)
	mkdir -p $n && rm -rf $n
endif
ifeq ($(USE_NETCDF4),1)
	mkdir -p ${n4} && rm -rf ${n4}
endif
	cd aux_progs && ${MAKE} clean -f gmerge.make
	cd aux_progs && ${MAKE} clean -f smallest_grib2.make
	cd aux_progs && ${MAKE} clean -f smallest_4.make
