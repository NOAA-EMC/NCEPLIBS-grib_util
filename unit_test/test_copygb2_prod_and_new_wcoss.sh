#!/bin/sh
#
#  This script uses to test the utility copygb2 which compiled with new G2 library v3.1.0
#  The script will compare two output files: one is copygb2 (current in operational) and
#  one is new copygb2 (v1.0.4).
#  The copygb2 will interpolate into small grid. Then, the WGRIB2 and DEGRIB2 use to display content
#  (inventory) of grib2 file for comparison and The result will write into output file:
#    awps_file_f${fcsthrs}_${GRID}_copygb2_prod_test.o
#
#  The input are GRIB2 file.   The GRIB2 file can be any model (i.e., GFS, NAM, HRRR, RTMA, ...)
#
#  NOTE:
#      $copygb2_prod is in operational and loaded by module load grib_util on WCOSS
#      $copygb2_test is new copygb2 which compiled with new G2 library.
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
    # get the production copygb2 path
    module load grib_util/v${prod_grib_util_ver}
    copygb2_prod=${COPYGB2:?}
    module use /nwtest2/modulefiles
    module switch grib_util/v${prod_grib_util_ver} grib_util/v${test_grib_util_ver}
elif [ "$machine" = "Cray" ]; then
    # get the production copygb2 path
    module load grib_util/${prod_grib_util_ver}
    copygb2_prod=${COPYGB2:?}
    module use /gpfs/hps/nco/ops/nwtest/modulefiles
    module switch grib_util/${prod_grib_util_ver} grib_util/${test_grib_util_ver}
fi

#
# These executable files (below) compiled with the G2 library V3.1.0
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
# you can change variable "dir" to temporary directory
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
if  [ -f $output_g1/gfs.t00z.pgrb2.0p25.f012.grib2.cnvgrib.g1.wgrib ]; then
   echo "Cleaning $output_g1"
   rm $output_g1/*
fi
if  [ -f $output_g2/awps_f012_ak_copygb2_prod.wgrib2 ]; then
   echo "Cleaning $output_g2"
   rm $output_g2/*
fi
if  [ -f $data/gfs.t00z.pgrb2.0p25.f012 ]; then
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

cd $data

filelist=` ls -1  $dir/data `
err=0

for file in $filelist
do
set -x

export fcsthrs=`echo $file | cut -c 22-24`
########  COPYGB2  ##############

for GRID in conus ak prico pac
do
   case $GRID in
     conus)
        #  Grid 20km_conus - CONUS - 20 km Quadruple Resolution (Lambert Conformal)
        export grid_20km_conus="30 6 0 0 0 0 0 0 369 257 12190000 226541000 8 25000000 265000000 20318000 20318000 0 64 25000000 25000000 0 0"
        $copygb2_prod -g "$grid_20km_conus" -i0 -x  $file  $output_g2/awps_f${fcsthrs}_${GRID}_copygb2_prod
        if [ $? -ne 0 ]; then err=1; fi
        echo " "
        echo " "
        $copygb2_test -g "$grid_20km_conus" -i0 -x $file  $output_g2/awps_f${fcsthrs}_${GRID}_copygb2_test
        if [ $? -ne 0 ]; then err=1; fi
        echo " "
        echo " "
        ;;
     ak)
        #  Grid 20km_ak - Alaska - Double Resolution (Polar Stereographic)
        #  Redefined grid 217 for Alaska region
        export grid_20km_ak="20 6 0 0 0 0 0 0 277 225 35000000 170000000 8 60000000 210000000 22500000 22500000 0 64"
        #  export grid_20km_ak="20 6 0 0 0 0 0 0 277 213 30000000 187000000 8 60000000 225000000 22500000 22500000 0 64"
        $copygb2_prod -g "$grid_20km_ak" -i0 -x $file  $output_g2/awps_f${fcsthrs}_${GRID}_copygb2_prod
        if [ $? -ne 0 ]; then err=1; fi
        echo " "
        echo " "
        $copygb2_test -g "$grid_20km_ak" -i0 -x $file  $output_g2/awps_f${fcsthrs}_${GRID}_copygb2_test
        if [ $? -ne 0 ]; then err=1; fi
        echo " "
        echo " "
        ;;
    prico)
        #  Grid 20km_prico - 0.25 degree Lat/Lon grid for Puerto Rico (20km)
        export grid_20km_prico="0 6 0 0 0 0 0 0 275 205 0 0 50750000 271750000 48 -250000 340250000 250000 250000 0"
        $copygb2_prod -g "$grid_20km_prico" -i0 -x $file  $output_g2/awps_f${fcsthrs}_${GRID}_copygb2_prod
        if [ $? -ne 0 ]; then err=1; fi
        echo " "
        echo " "
        $copygb2_test -g "$grid_20km_prico" -i0 -x $file  $output_g2/awps_f${fcsthrs}_${GRID}_copygb2_test
        if [ $? -ne 0 ]; then err=1; fi
        echo " "
        echo " "
        ;;
     pac)
        #  Grid 20km_pac - 20 km Mercator grid for Pacific Region
        export grid_20km_pac="10 6 0 0 0 0 0 0 837 692 -45000000 110000000 48 20000000 65720000 270000000 64 0 20000000 20000000"
        $copygb2_prod -g "$grid_20km_pac" -i0 -x $file  $output_g2/awps_f${fcsthrs}_${GRID}_copygb2_prod
        if [ $? -ne 0 ]; then err=1; fi
        echo " "
        echo " "
        $copygb2_test -g "$grid_20km_pac" -i0 -x $file  $output_g2/awps_f${fcsthrs}_${GRID}_copygb2_test
        if [ $? -ne 0 ]; then err=1; fi
        echo " "
        echo " "
        ;;
   esac
   $WGRIB2 $output_g2/awps_f${fcsthrs}_${GRID}_copygb2_prod  > $output_g2/awps_f${fcsthrs}_${GRID}_copygb2_prod.wgrib2
   if [ $? -ne 0 ]; then err=1; fi
   $WGRIB2 $output_g2/awps_f${fcsthrs}_${GRID}_copygb2_test  > $output_g2/awps_f${fcsthrs}_${GRID}_copygb2_test.wgrib2
   if [ $? -ne 0 ]; then err=1; fi
   diff $output_g2/awps_f${fcsthrs}_${GRID}_copygb2_prod.wgrib2 $output_g2/awps_f${fcsthrs}_${GRID}_copygb2_test.wgrib2 > $output_g2/awps_f${fcsthrs}_${GRID}_copygb2_prod_test_wgrib2.o
   if [ $? -eq 0 ]; then echo "PASS"; else echo "FAIL!"; err=1; fi
done
done

exit $err

