#!/bin/bash
export machine=$SITE

if [ $machine="TIDE" ] || [ $machine="GYRE" ] ; then
   echo " "
   echo " "
   echo " You are on WCOSS:  $machine "
   echo " "
   echo " "
else
   echo " "
   echo " "
   echo " Your machine is $machine NOT found "
   echo " The script $0 can not continue.  Aborted ! "
   echo " "
   echo " Your machine must be (SURGE/LUNA) or (TIDE/GYRE)"
   echo " "
   echo " "
   exit
fi
echo " "
echo " "

module use  /nwprod2/lib/modulefiles
module load g2/v3.0.0
module load w3nco/v2.0.6
module load bacio/v2.0.2
module load png/v1.2.44
module load jasper/v1.900.1
module load z/v1.2.6
module load ip/v3.0.0
module load sp/v2.0.2

make -f makefile_wcoss
make -f makefile_wcoss clean
mkdir -p ../../exec
cp grbindex  ../../exec
