#!/bin/sh
#
#  This script uses to test the utility cnvgrib which compiled with new G2 library v3.1.0
#  The conversion cnvgrib will convert (NAM file) from grib2 to grib1.
#  Then, the WGRIB uses to display data values : min and max at  HGT 800mb field for comparison
#
#  The input are GRIB2 file. 
#
#  NOTE:  
#      $cnvgrib_test is new cnvgrib which compiled with new G2 library.
#

grib_util_ver=1.1.0
export cyc=18

module use /scratch4/NCEPDEV/nems/noscrub/emc.nemspara/soft/modulefiles
module load prod_util
machine=$(getsystem.pl -t)

if [ "$machine" = "IBM" ] || [ "$machine" = "Cray" ] || [ "$machine" = "Dell" ] || [ "$machine" = "Theia" ] ; then
   echo " "
   echo " You are on WCOSS:  $(getsystem.pl -p)"
else
   echo " "
   echo " Your machine is $machine NOT found "
   echo " The script $0 can not continue.  Aborted ! "
   echo " "
   echo " Your machine must be (SURGE/LUNA)"
   echo " or (TIDE/GYRE) or (MARS/VENUS)"
   echo " or Theia "
   echo " "
   exit
fi

#
# If you want to use temporary directories,
# you can change variable dir to temporary
#
#  Setup working directories
#
# If you want to use temporary directories,
# you can change variable dir to temporary
#
export dir=` pwd `
export data=$dir/data
output_g1=$dir/output_g1
output_g2=$dir/output_g2
mkdir -p $data $output_g1 $output_g2

if [ "$machine" = "Dell" ]; then
    module load EnvVars/1.0.2
    module load ips/18.0.1.163
    module load prod_util/1.1.0
    module load prod_envir/1.0.2
#
#   This is a test version of GRIB_UTIL.v1.1.0 on $machine
#
    module unload grib_util
    module load dev/grib_util/1.1.0
    input_file=/usrx/local/nceplibs/dev/lib/fv3gfs
elif [ "$machine" = "IBM" ]; then
#
#   This is a test version of GRIB_UTIL.v1.1.0 on $machine
#
    module unload grib_util
    module use -a /usrx/local/nceplibs/grib_util.v1.1.0/modulefiles
    module load grib_util/v1.1.0
    input_file=/usrx/local/nceplibs/gfs_data
elif [ "$machine" = "Cray" ]; then
    module unload grib_util
#
#   This is a test version of GRIB_UTIL.v1.1.0 on $machine
#
    module unload grib_util
    module use /usrx/local/nceplibs/modulefiles
    module load grib_util/1.1.0
    input_file=/usrx/local/nceplibs/gfs_data
elif [ "$machine" = "Theia" ]; then
#
#   This is a test version of GRIB_UTIL.v1.1.0 on $machine
#
    module unload grib_util
    module use /scratch3/NCEPDEV/nwprod/modulefiles
    module load grib_util/v1.1.0
    input_file=/scratch3/NCEPDEV/nwprod/gfs_data
fi

#
# These executable files (below) is in GRIB_UTIL.v1.1.0
#
cnvgrib_test=$CNVGRIB
echo " "
module list
echo " "

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

if [ -f $input_file/nam.t${cyc}z.awp15178.tm00.grib2 ] ; then
   cp $input_file/nam*  $dir/data
else
   echo " "
   echo " "
   echo "GRIB2 File $input_file/nam.t${cyc}z.awp15178.tm00.grib2 Does Not Exist."
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

echo " dir  $dir "
echo " file  $dir/$file "
echo " data $data/$file "

#
# Step 1: CNVGRIB converts from GRIB2 to GRIB1
#

echo "Running cnvgrib_cnvgrib (grib2 -> grib1)"
set -x
$cnvgrib_test -g21 $data/$file $output_g1/$file.grib2.test.g1
if [ $? -ne 0 ]; then err=1; fi
set +x
echo

export new_max=` ${WGRIB:?} -s $output_g1/$file.grib2.test.g1 |  grep ":HGT:800 mb:" |  $WGRIB -i -V $output_g1/$file.grib2.test.g1 \
         -o /dev/null | grep max | awk '{print $4}' `
export new_min=` ${WGRIB:?} -s $output_g1/$file.grib2.test.g1 |  grep ":HGT:800 mb:" |  $WGRIB -i -V $output_g1/$file.grib2.test.g1 \
         -o /dev/null | grep max | awk '{print $3}' `

echo " The new cnvgrib (cnvgrib v3.1.0) convert NAM file from GRIB2 to GRIB1."
echo " The data value MAX and MIN at HGT 800mb are correct " 

echo " "
echo " Data value MAX = " $new_max
echo " "
echo " Data value MIN  = " $new_min
echo " "
done
exit
