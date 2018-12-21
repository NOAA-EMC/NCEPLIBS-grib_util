#!/bin/sh
#
#  This script uses to test the utility cnvgrib which compiled with new G2 library v3.1.0
#  The script will compare two output files: one is old cnvgrib (current in operational) and
#  one is new cnvgrib (cnvgrib v3.1.0).
#  The conversion cnvgrib will convert from grib2 to grib1 and from grib1 to grib2.
#  Then, the WGRIB2 and DEGRIB2 use to display content (inventory) of grib2 file for comparison
#  and The result will write into output file:  $file.grib2.prod_test.wgrib2.o.
#
#  The input are GRIB2 file.   The GRIB2 file can be any model (i.e., GFS, NAM, HRRR, RTMA, ...)
#
#  NOTE:  
#      $cnvgrib_prod is in operational and loaded by module load grib_util on WCOSS
#      $cnvgrib_test is new cnvgrib which compiled with new G2 library.
#

prod_grib_util_ver=1.0.3
test_grib_util_ver=1.0.4

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
    # get the production cnvgrib path
    module load grib_util/v${prod_grib_util_ver}
    cnvgrib_prod=${CNVGRIB:?}
    module use /nwtest2/modulefiles
    module switch grib_util/v${prod_grib_util_ver} grib_util/v${test_grib_util_ver}
elif [ "$machine" = "Cray" ]; then
    # get the production cnvgrib path
    module load grib_util/${prod_grib_util_ver}
    cnvgrib_prod=${CNVGRIB:?}
    module use /gpfs/hps/nco/ops/nwtest/modulefiles
    module switch grib_util/${prod_grib_util_ver} grib_util/${test_grib_util_ver}
fi

#
# These executable files (below) compiled with the G2 library V3.1.0
#
# cnvgrib_test=${CNVGRIB:?}
cnvgrib_test=/gpfs/hps/emc/global/noscrub/Boi.Vuong/cnvgrib/cnvgrib
# cnvgrib21gfs=${CNVGRIB21_GFS:?}
cnvgrib21gfs=/gpfs/hps/emc/global/noscrub/Boi.Vuong/cnvgrib21gfs/cnvgrib21_gfs
copygb2_test=${COPYGB2:?}
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
# All test data files are in  /gpfs/sss/emc/global/shared/nceplibs/fix on LUNA or SURGE
#
input_file=/gpfs/sss/emc/global/shared/nceplibs/fix/gfs
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

if [ -f $input_file/gfs.t00z.pgrb2.0p25.f012 ] ; then
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

echo " dir  $dir "
echo " file  $dir/$file "
echo " data $data/$file "

#
# Step 1: CNVGRIB converts from GRIB2 to GRIB1
#

echo "Run production cnvgrib (grib2 -> grib1)"
set -x
$cnvgrib_prod -g21 $data/$file $output_g1/$file.grib2.prod.g1
if [ $? -ne 0 ]; then err=1; fi
set +x
echo

echo "Run test cnvgrib (grib2 -> grib1)"
set -x
$cnvgrib_test -g21 $data/$file $output_g1/$file.grib2.test.g1
if [ $? -ne 0 ]; then err=1; fi
set +x
echo

echo "Run wgrib1 on both files"
set -x
${WGRIB:?} -s $output_g1/$file.grib2.prod.g1 | cut -d : -f 4-7 >  $output_g1/$file.grib2.prod.g1.wgrib
if [ $? -ne 0 ]; then err=1; fi
${WGRIB:?} -s $output_g1/$file.grib2.test.g1 | cut -d : -f 4-7 >  $output_g1/$file.grib2.test.g1.wgrib
if [ $? -ne 0 ]; then err=1; fi
set +x
echo

#
# Step 2: Compare two GRIB1 output files 
#
echo "Compare output files"
set -x
diff  $output_g1/$file.grib2.prod.g1.wgrib  $output_g1/$file.grib2.test.g1.wgrib > $output_g1/$file.grib2.prod_test.g1.wgrib.o
if [ $? -eq 0 ]; then echo "PASS"; else echo "FAIL!"; err=1; fi
set +x
echo

#
# Step 3: CNVGRIB converts from GRIB1 to GRIB2
#

echo "Run production cnvgrib (grib1 -> grib2)"
set -x
$cnvgrib_prod  -g12 -p31 $output_g1/$file.grib2.prod.g1 $output_g2/$file.grib2.prod
if [ $? -ne 0 ]; then err=1; fi
set +x
echo

echo "Run test cnvgrib (grib1 -> grib2)"
set -x
$cnvgrib_test  -g12 -p31 $output_g1/$file.grib2.test.g1 $output_g2/$file.grib2.test
if [ $? -ne 0 ]; then err=1; fi
set +x
echo

echo "Run degrib2"
set -x
$DEGRIB2 $data/$file  > $output_g2/$file.grib2.org.inv
if [ $? -ne 0 ]; then err=1; fi
$DEGRIB2 $output_g2/$file.grib2.prod > $output_g2/$file.grib2.prod.inv
if [ $? -ne 0 ]; then err=1; fi
$DEGRIB2 $output_g2/$file.grib2.test > $output_g2/$file.grib2.test.inv
if [ $? -ne 0 ]; then err=1; fi
set +x
echo

echo "Run wgrib2"
set -x
$WGRIB2 -s $data/$file | cut -d: -f 4-5 > $output_g2/$file.grib2.org.wgrib2
if [ $? -ne 0 ]; then err=1; fi
$WGRIB2 -s $output_g2/$file.grib2.prod | cut -d: -f 4-5 > $output_g2/$file.grib2.prod.wgrib2
if [ $? -ne 0 ]; then err=1; fi
$WGRIB2 -s $output_g2/$file.grib2.test | cut -d: -f 4-5 > $output_g2/$file.grib2.test.wgrib2
if [ $? -ne 0 ]; then err=1; fi
set +x
echo

#
# Step 4: Compare two GRIB2 output files 
#
echo "Compare output files"
set -x
diff $output_g2/$file.grib2.prod.inv $output_g2/$file.grib2.test.inv  > $output_g2/$file.grib2.prod_test.inv.o
if [ $? -eq 0 ]; then echo "PASS"; else echo "FAIL!"; err=1; fi
diff $output_g2/$file.grib2.prod.wgrib2 $output_g2/$file.grib2.test.wgrib2  > $output_g2/$file.grib2.prod_test.wgrib2.o
if [ $? -eq 0 ]; then echo "PASS"; else echo "FAIL!"; err=1; fi
diff $output_g2/$file.grib2.org.inv  $output_g2/$file.grib2.test.inv > $output_g2/$file.grib2.org_test.inv 
if [ $? -eq 0 ]; then echo "PASS"; else echo "FAIL!"; err=1; fi
diff $output_g2/$file.grib2.org.wgrib2 $output_g2/$file.grib2.test.wgrib2 > $output_g2/$file.grib2.org_test.wgrib2.o 
if [ $? -eq 0 ]; then echo "PASS"; else echo "FAIL!"; err=1; fi
set +x
echo

done

exit $err

