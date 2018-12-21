#!/bin/sh
#
#  This script uses to test the utility wgrib2 which compiled with new G2 library v3.1.0
#  The wgrib2 will interpolate GFS PGRB file to AWIPS 20km (CONUS, Alaska, Puerto Rico and
#   Pacific region) grid.
#
#  The input are GRIB2 file.   The GRIB2 file can be any model (i.e., GFS, NAM, HRRR, RTMA, ...)
#
#  NOTE:
#      $wgrib2_test is new wgrib2 which compiled with new G2 library.
#

grib_util_ver=1.1.0
export cyc=00

module load prod_util
module load prod_util/1.1.0
machine=$(getsystem.pl -t)

if [ "$machine" = "IBM" ] || [ "$machine" = "Cray" ] || [ "$machine" = "Dell" ] ; then
   echo " "
   echo " You are on WCOSS:  $(getsystem.pl -p)"
else
   echo " "
   echo " Your machine is $machine NOT found "
   echo " The script $0 can not continue.  Aborted ! "
   echo " "
   echo " Your machine must be (SURGE/LUNA)"
   echo " or (TIDE/GYRE) or (MARS/VENUS)"
   echo " "
   exit
fi
echo " "

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
fi

#
# These executable files (below) is in GRIB_UTIL.v1.1.0
#
wgrib2_test=$WGRIB2
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
######## WGRIB2  ##############

# Set type of Interpolation for WGRIB2
export opt1=' -set_grib_type same -new_grid_winds earth '
export opt1uv=' -set_grib_type same -new_grid_winds grid '
export opt21=' -new_grid_interpolation bilinear -if '
export opt22=":(LAND|CSNOW|CRAIN|CFRZR|CICEP|ICSEV):"
export opt23=' -new_grid_interpolation neighbor -fi '
export opt24=' -set_bitmap 1 -set_grib_max_bits 16 -if '
export opt25=":(APCP|ACPCP|PRATE|CPRAT):"
export opt26=' -set_grib_max_bits 25 -fi -if '
export opt27=":(APCP|ACPCP|PRATE|CPRAT|DZDT):"
export opt28=' -new_grid_interpolation budget -fi '

for GRID in conus ak prico pac 003
do
   case $GRID in
     conus)
        #  Grid 20km_conus - CONUS - 20 km Quadruple Resolution (Lambert Conformal)
        echo " "
        echo "  Running WGRIB2 - Grid 20km_conus"
        echo " "
        echo "  Please wait ..."
        echo " "
        export gridconus="lambert:265.0:25.0:25.0 226.541:369:20318.0 12.19:257:20318.0"
        $wgrib2_test $file $opt1uv $opt21 $opt22 $opt23 $opt24 $opt25 $opt26 $opt27 $opt28 -new_grid $gridconus $output_g2/awps_f${fcsthrs}_${GRID}_wgrib2_test
        echo " "
        if [ $? -eq 0 ]; then echo "  PASS: wgrib2 successfully interpolates  GFS PGRB2 file to AWIPS 20km CONUS grid."; else echo "FAIL!"; err=1; fi
        echo " "
        echo " "
        ;;
     ak)
        #  Grid 20km_ak - Alaska - Double Resolution (Polar Stereographic)
        echo " "
        echo "  Running WGRIB2 - Grid 20km_ak"
        echo " "
        echo "  Please wait ..."
        echo " "
        export gridak="nps:210.0:60.0 170.0:277:22500 35.0:225:22500"
        $wgrib2_test $file $opt1uv $opt21 $opt22 $opt23 $opt24 $opt25 $opt26 $opt27 $opt28 -new_grid $gridak $output_g2/awps_f${fcsthrs}_${GRID}_wgrib2_test
        echo " "
        if [ $? -eq 0 ]; then echo "  PASS: wgrib2 successfully interpolates GFS PGRB2 file to AWIPS 20km Alaska grid."; else echo "FAIL!"; err=1; fi
        echo " "
        echo " "
        ;;
    prico)
        #  Grid 20km_prico - 0.25 degree Lat/Lon grid for Puerto Rico (20km)
        echo " "
        echo "  Running WGRIB2 - Grid 20km_prico"
        echo " "
        echo "  Please wait ..."
        echo " "
        export gridprico="latlon 271.75:275:0.25 50.75:205:-0.25"
        $wgrib2_test $file $opt1 $opt21 $opt22 $opt23 $opt24 $opt25 $opt26 $opt27 $opt28 -new_grid $gridprico $output_g2/awps_f${fcsthrs}_${GRID}_wgrib2_test
        echo " "
        if [ $? -eq 0 ]; then echo "  PASS: wgrib2 successfully interpolates GFS PGRB2 file to AWIPS 20km Puerto Rico grid."; else echo "FAIL!"; err=1; fi
        echo " "
        echo " "
        ;;
     pac)
        #  Grid 20km_pac - 20 km Mercator grid for Pacific Region
        echo " "
        echo "  Running WGRIB2 - Grid 20km_pacific"
        echo " "
        echo "  Please wait ..."
        echo " "
        export gridpac="mercator:20.0 110.0:837:20000:270.0 -45.0:725:20000:65.7345"
        $wgrib2_test $file $opt1 $opt21 $opt22 $opt23 $opt24 $opt25 $opt26 $opt27 $opt28 -new_grid $gridpac $output_g2/awps_f${fcsthrs}_${GRID}_wgrib2_test
        echo " "
        if [ $? -eq 0 ]; then echo "  PASS: wgrib2 successfully interpolates GFS PGRB2 file to AWIPS 20km Pacific grid."; else echo "FAIL!"; err=1; fi
        echo " "
        echo " "
        ;;
     003)
        # LAT/LON 1.0 DEGREE (GRID 003) Grid
        echo " "
        echo "  Running WGRIB2 - Grid 20km_pacific"
        echo " "
        echo "  Please wait ..."
        echo " "
        export grid003="latlon 0:360:1.0 90:181:-1.0"
        $wgrib2_test $file $opt1 $opt21 $opt22 $opt23 $opt24 $opt25 $opt26 $opt27 $opt28 -new_grid $grid003 $output_g2/awps_f${fcsthrs}_${GRID}_wgrib2_test
        echo " "
        if [ $? -eq 0 ]; then echo "  PASS: wgrib2 successfully interpolates GFS PGRB2 file to LAT/LON 1.0 deg grid."; else echo "FAIL!"; err=1; fi
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
   ls -l $output_g2/awps_f*_*_wgrib2_test
   echo " "
   echo " "
   echo "  PASS: wgrib2 successfully interpolates for AWIPS 20km grids."
   echo " "
   echo " "
exit

