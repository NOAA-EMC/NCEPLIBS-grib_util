#!/bin/sh
#                 Utility DEGRIB2
#  This script uses to test the utility degrib2 which compiled with new G2
#  library v3.1.0.  
#
#  The input are GRIB2 fileis   The GRIB2 files can be in any model (i.e., GFS, NAM, HRRR, RTMA, ...)
#

grib_util_ver=1.0.5
export cyc=00

. /apps/lmod/lmod/init/sh

module use /scratch3/NCEPDEV/nwprod/modulefiles
module load grib_util/v${grib_util_ver}

cnvgrib=${CNVGRIB:?}
cnvgrib21gfs=${CNVGRIB21_GFS:?}
copygb2=${COPYGB2:?}
degrib2=${DEGRIB2:?}
grb2index=${GRB2INDEX:?}
tocgrib2=${TOCGRIB2:?}

echo " "
module list
echo " "

#
#  Setup working directories
#  
# If you want to use temporary directories,
# you can change variable dir to temporary
#
dir=` pwd `
data=$dir/data
#                   ********  NOTE  *************
#  All test data files are in  $dir/dataon Theia 
#

input_file=$dir/data_grib2
output_g1=$dir/output_g1
output_g2=$dir/output_g2
mkdir -p $data $output_g1 $output_g2
#
#  Clean up temp directory before test starts
#
if [ "$(ls -A $output_g1)" ]; then
   echo "Cleaning $output_g1"
   rm $output_g1/*
fi
if [ "$(ls -A $output_g2)" ]; then
   echo "Cleaning $output_g2"
   rm $output_g2/*
fi
if [ "$(ls -A $data)" ]; then
   echo "Cleaning $data"
   rm $data/*
fi

#
#  Find out if working directory exists or not  
#
if [ ! -d  $data ] ; then
    echo " "
    echo " Your working directory $data NOT found "
    echo " "
    exit 1
fi

if [ -f $input_file/gfs.t${cyc}z.pgrb2.0p25.f012 ] ; then
   cp $input_file/gfs*  $dir/data
else
   echo " " 
   echo " " 
   echo "GRB2 File $input_file/gfs.t${cyc}z.pgrb2.0p25.f012 Does Not Exist." 
   echo " " 
   echo " No input GRIB2 file to continue " 
   echo " " 
   echo " "
   exit 1
fi

filelist=` ls -1  $dir/data `
err=0

for file in $filelist
do

#
#  Inventory GRIB2 file by using the DEGRIB2
#

echo "Testing degrib2"
set -x
$degrib2    $data/$file >   $output_g2/$file.grib2.inv
if [ $? -ne 0 ]; then err=1; fi
set +x
echo " "
if [ -s $output_g2/$file.grib2.inv ]; then echo "PASS"; else echo "FAIL!"; err=1; fi
ls -l $output_g2/$file.grib2.inv
echo

done

exit $err

