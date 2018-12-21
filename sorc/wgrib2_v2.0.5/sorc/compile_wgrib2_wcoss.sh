#!/bin/sh
cwd=$(dirname $(readlink -f "$0"))
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
module purge
echo "Loading required libraries"
module load ${cwd}/../modulefiles/wgrib2/wcoss_${machine_lc}
module list

cd $cwd/wgrib2

make -f $makefile
if [ $? -eq 0 ] && [ -s wgrib2 ]; then
    make -f $makefile install
    make -f $makefile clean
else
    >&2 echo "ERROR: BUILD NOT SUCCESSFUL!"
fi

