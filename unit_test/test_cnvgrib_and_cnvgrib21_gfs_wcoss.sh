#!/bin/sh
#                CNVGRIB and CNVGRIB21_GFS become unify utility CNVGRIB.
#  This script uses to test the utility cnvgrib and cnvgrib21_gfs which compiled with new G2
#  library v3.1.0.  The script will compare two output files: one is cnvgrib and cnvgrib21_gfs
#  The cnvgrib will correct the forecast hour > F252 when it convert from GRIB2 to GRIb1. 
# 
#  Then, the WGRIB use to display content (inventory) of grib1 file for comparison
#  and The result will write into output file:  $file.grib1.cnvgrib_cnvgrib21gfs.wgrib.o.
#
#  The input are GRIB2 file.   The GRIB2 file can be in any model (i.e., GFS, NAM, HRRR, RTMA, ...)
#

grib_util_ver=1.0.4

module use /gpfs/hps/nco/ops/nwprod/modulefiles
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

# module use /gpfs/hps/emc/global/noscrub/Boi.Vuong/modulefiles
if [ "$machine" = "IBM" ]; then
    module use /nwtest2/modulefiles
    module load grib_util/v${grib_util_ver}
elif [ "$machine" = "Cray" ]; then
    module use /gpfs/hps/nco/ops/nwtest/modulefiles
    module load grib_util/${grib_util_ver}
fi

#
# These executable files (below) compiled with G2 library V3.1.0
#
# cnvgrib=${CNVGRIB:?}
cnvgrib=/gpfs/hps/emc/global/noscrub/Boi.Vuong/cnvgrib/cnvgrib

# cnvgrib21gfs=${CNVGRIB21_GFS:?}
cnvgrib21gfs=/gpfs/hps/emc/global/noscrub/Boi.Vuong/cnvgrib21gfs/cnvgrib21_gfs

# copygb2_test=${COPYGB2:?}
copygb2_test=/gpfs/hps/emc/global/noscrub/Boi.Vuong/copygb2/copygb2

# degrib2_test=${DEGRIB2:?}
degrib2_test=/gpfs/hps/emc/global/noscrub/Boi.Vuong/degrib2/degrib2

grb2index_test=${GRB2INDEX:?}
tocgrib2_test=${TOCGRIB2:?}

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
#  All test data files are in  /gpfs/sss/emc/global/shared/nceplibs/fix on LUNA or SURGE
#

# input_file=/gpfs/sss/emc/global/shared/nceplibs/fix/gfs
input_file=/gpfs/hps/emc/global/noscrub/Boi.Vuong/v3.1.0_1/unit_test/gfs
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
if [ "$(ls -A $output_g1)" ]; then
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

# if [ -f $input_file/gfs.t00z.pgrb2.0p25.f012 ] ; then
if [ -f $input_file/gfs.t00z.pgrb2.0p25.f264 ] ; then
   cp $input_file/gfs*  $dir/data
else
   echo " " 
   echo " " 
   echo "GRB2 File $input_file/gfs.t00z.pgrb2.0p25.f012 Does Not Exist." 
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
# Step 1: CNVGRIB converts from GRIB2 to GRIB1
#

echo "Testing cnvgrib"
set -x
$cnvgrib       -g21 $data/$file  $output_g1/$file.grib2.cnvgrib.g1
if [ $? -ne 0 ]; then err=1; fi
set +x
echo

echo "Testing cnvgrib21gfs"
set -x
$cnvgrib21gfs  -g21 $data/$file  $output_g1/$file.grib2.cnvgrib21gfs.g1
if [ $? -ne 0 ]; then err=1; fi
set +x
echo

echo "Run wgrib1 on both files"
set -x
${WGRIB:?} -s $output_g1/$file.grib2.cnvgrib.g1 | cut -d : -f 4-7 >  $output_g1/$file.grib2.cnvgrib.g1.wgrib
if [ $? -ne 0 ]; then err=1; fi
${WGRIB:?} -s $output_g1/$file.grib2.cnvgrib21gfs.g1 | cut -d : -f 4-7 >  $output_g1/$file.grib2.cnvgrib21gfs.g1.wgrib
if [ $? -ne 0 ]; then err=1; fi
set +x
echo

#
# Step 2: Compare two GRIB1 output files 
#
echo "Compare output files"
set -x
diff  $output_g1/$file.grib2.cnvgrib.g1.wgrib  $output_g1/$file.grib2.cnvgrib21gfs.g1.wgrib > $output_g1/$file.grib2.cnvgrib_cnvgrib21gfs.g1.wgrib.o
if [ $? -eq 0 ]; then echo "PASS"; else echo "FAIL!"; err=1; fi
set +x
echo

done

exit $err

