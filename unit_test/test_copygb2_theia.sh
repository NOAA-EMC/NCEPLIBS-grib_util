#!/bin/sh
#
#  This script uses to test the utility copygb2 which compiled with new G2 library v3.1.0
#  The copygb2 will interpolate GFS PGRB file to AWIPS 20km (CONUS, Alaska, Puerto Rico and
#   Pacific region) grid.
#
#  The input are GRIB2 file.   The GRIB2 file can be any model (i.e., GFS, NAM, HRRR, RTMA, ...)
#
#  NOTE:
#      $copygb2_test is new copygb2 which compiled with new G2 library.
#

grib_util_ver=1.1.0
export cyc=00

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
copygb2_test=$COPYGB2
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

cd $data

filelist=` ls -1  $dir/data `
err=0

for file in $filelist
do
set +x

export fcsthrs=`echo $file | cut -c 22-24`
########  COPYGB2  ##############

for GRID in conus ak prico pac 003
do
   case $GRID in
     conus)
        #  Grid 20km_conus - CONUS - 20 km Quadruple Resolution (Lambert Conformal)
        export grid_20km_conus="30 6 0 0 0 0 0 0 369 257 12190000 226541000 8 25000000 265000000 20318000 20318000 0 64 25000000 25000000 0 0"
        echo " "
        echo "  Running copygb2_test - Grid 20km_conus"
        echo " "
        echo "  Please wait ..."
        echo " "
        $copygb2_test -g "$grid_20km_conus" -i0 -x $file  $output_g2/awps_f${fcsthrs}_${GRID}_copygb2_test
        if [ $? -eq 0 ]; then echo "  PASS: copygb2 successfully interpolates  GFS PGRB2 file to AWIPS 20km CONUS grid."; else echo "FAIL!"; err=1; fi
        echo " "
        echo " "
        ;;
     ak)
        #  Grid 20km_ak - Alaska - Double Resolution (Polar Stereographic)
        export grid_20km_ak="20 6 0 0 0 0 0 0 277 225 35000000 170000000 8 60000000 210000000 22500000 22500000 0 64"
        echo " "
        echo "  Running copygb2_test - Grid 20km_ak"
        echo " "
        echo "  Please wait ..."
        echo " "
        $copygb2_test -g "$grid_20km_ak" -i0 -x $file  $output_g2/awps_f${fcsthrs}_${GRID}_copygb2_test
        if [ $? -eq 0 ]; then echo "  PASS: copygb2 successfully interpolates GFS PGRB2 file to AWIPS 20km Alaska grid."; else echo "FAIL!"; err=1; fi
        echo " "
        echo " "
        ;;
    prico)
        #  Grid 20km_prico - 0.25 degree Lat/Lon grid for Puerto Rico (20km)
        export grid_20km_prico="0 6 0 0 0 0 0 0 275 205 0 0 50750000 271750000 48 -250000 340250000 250000 250000 0"
        echo " "
        echo "  Running copygb2_test - Grid 20km_prico"
        echo " "
        echo "  Please wait ..."
        echo " "
        $copygb2_test -g "$grid_20km_prico" -i0 -x $file  $output_g2/awps_f${fcsthrs}_${GRID}_copygb2_test
        if [ $? -eq 0 ]; then echo "  PASS: copygb2 successfully interpolates GFS PGRB2 file to AWIPS 20km Puerto Rico grid."; else echo "FAIL!"; err=1; fi
        echo " "
        echo " "
        ;;
     pac)
        #  Grid 20km_pac - 20 km Mercator grid for Pacific Region
        export grid_20km_pac="10 6 0 0 0 0 0 0 837 692 -45000000 110000000 48 20000000 65720000 270000000 64 0 20000000 20000000"
        echo " "
        echo "  Running copygb2_test - Grid 20km_pacific"
        echo " "
        echo "  Please wait ..."
        echo " "
        $copygb2_test -g "$grid_20km_pac" -i0 -x $file  $output_g2/awps_f${fcsthrs}_${GRID}_copygb2_test
        if [ $? -eq 0 ]; then echo "  PASS: copygb2 successfully interpolates GFS PGRB2 file to AWIPS 20km Pacific grid."; else echo "FAIL!"; err=1; fi
        echo " "
        echo " "
        ;;
     003)
        #  LAT/LON 1.0 DEGREE (GRID 003) Grid
        export grid003="0 6 0 0 0 0 0 0 360 181 0 0 90000000 0 48 -90000000 359000000 1000000 1000000 0"
        echo " "
        echo "  Running copygb2_test - Grid 20km_pacific"
        echo " "
        echo "  Please wait ..."
        echo " "
        $copygb2_test -g "$grid003" -i0 -x $file  $output_g2/awps_f${fcsthrs}_${GRID}_copygb2_test
        if [ $? -eq 0 ]; then echo "  PASS: copygb2 successfully interpolates GFS PGRB2 file to AWIPS LAT/LON 1.0 deg grid."; else echo "FAIL!"; err=1; fi
        echo " "
        echo " "
        ;;
   esac
done
done
   echo " "
   echo "The output files are following: "
   echo " "
   echo " "
   ls -l $output_g2/awps_f*_*_copygb2_test
   echo " "
   echo " "
   echo "  PASS: copygb2 successfully interpolates GFS PGRB2 file to AWIPS 20km Pacific grids."
   echo " "
   echo " "
exit

