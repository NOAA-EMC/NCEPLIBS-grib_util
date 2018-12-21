#!/bin/sh

module load prod_util
machine=$(getsystem.pl -t)

if [ "$machine" = "IBM" ] || [ "$machine" = "Cray" ]; then
   echo " "
   echo " You are on WCOSS:  $(getsystem.pl -p)"
else
   echo " "
   echo " Your machine is $machine NOT found "
   echo " The script $0 can not continue.  Aborted ! "
   echo " "
   echo " Your machine must be (SURGE/LUNA) or (TIDE/GYRE)"
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

