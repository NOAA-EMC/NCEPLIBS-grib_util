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
   echo " Your machine is $machine is not recognized as a WCOSS machine."
   echo " The script $0 can not continue.  Aborting!"
   echo " "
   exit
fi
echo " "

machine_lc=${machine,,} # Get lower case
makefile=makefile_wcoss_${machine_lc}

# Load required modules
module use ../../modulefiles
module load build_grib_util/${machine_lc}
module list

make -f $makefile
make -f $makefile install
make -f $makefile clean

