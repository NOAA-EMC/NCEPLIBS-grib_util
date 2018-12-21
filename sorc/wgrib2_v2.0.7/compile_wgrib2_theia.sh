#!/bin/sh

LMOD_EXACT_MATCH=no

set -x

machine=Theia
machine_lc=theia

if [ "$machine" = "Cray" ] ; then
#
#  NOTE:  DO NOT COMPILE WGRIB2 WITH IOBUF
#  WGRIB2 DOES WORK WITH MODULE IOBUF on CRAY
#      module unload  iobuf/2.0.5
#
   module unload  iobuf
   export CC=gcc
   export FC=gfortran
   makefile=makefile_wcoss_cray
elif [ "$machine" = "Dell" ]; then
   export CC=icc
   export FC=ifort
   export COMP_SYS=intel_linux
   makefile=makefile_wcoss_dell
elif [ "$machine" = "IBM" ]; then
   export CC=gcc
   export FC=gfortran
   makefile=makefile_wcoss_ibm
elif [ "$machine" = "Theia" ]; then
   export CC=gcc
   export FC=gfortran
   makefile=makefile_wcoss_theia
else
   makefile=makefile
fi

# Load required modules
module use ../../modulefiles
if [ -n "$1" ]; then
    module load build_wgrib2/${machine_lc}/$1
else
    module load build_wgrib2/${machine_lc}
fi
module list

make -f $makefile
make -f $makefile install
make -f $makefile clean
