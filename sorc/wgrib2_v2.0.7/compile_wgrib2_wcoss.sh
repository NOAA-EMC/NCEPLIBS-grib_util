#!/bin/sh

LMOD_EXACT_MATCH=no
module load prod_util
module load prod_util/1.1.0
machine=$(getsystem.pl -t)

if [ "$machine" = "IBM" ] || [ "$machine" = "Cray" ] || [ "$machine" = "Dell" ]; then
   echo " "
   echo " You are on WCOSS:  $(getsystem.pl -p)"
else
   echo " "
   echo " Your machine, $machine, is not recognized as a WCOSS machine."
   echo " The script $0 cannot continue.  Aborting!"
   echo " "
   exit
fi
echo " "

set -x

machine_lc=${machine,,} # Get lower case

if [ "$machine" = "Cray" ] ; then
#
#  NOTE:  DO NOT COMPILE WGRIB2 WITH IOBUF
#  WGRIB2 DOES WORK WITH MODULE IOBUF on CRAY
#      module unload iobuf
#
   module unload iobuf
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
   makefile=makefile_wcoss_Theia
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
