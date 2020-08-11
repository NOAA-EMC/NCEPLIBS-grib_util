#!/bin/sh
export ver=1.2.0

#
#  This script uses to test the utility cnvgrib which compiled with new G2 library v3.2.0
#  The conversion cnvgrib will convert (NAM file) from grib2 to grib1.
#  Then, the WGRIB uses to display data values : min and max at  HGT 800mb field for comparison
#
#  The input are GRIB2 file. 
#
#  NOTE:  
#      $cnvgrib_test is new cnvgrib which compiled with new G2 library.
#

export cyc=18

mac=$(hostname | cut -c1-1)
mac2=$(hostname | cut -c1-2)
if [ $mac = v -o $mac = m  ] ; then   # For Dell
   machine=dell
   echo " "
   echo " You are on WCOSS :  ${machine}"
elif [ $mac = l -o $mac = s ] ; then   #    wcoss_c (i.e. luna and surge)
   machine=cray
   echo " "
   echo " You are on WCOSS :  ${machine}"
elif [ $mac2 = hf ] ; then
   machine=hera
   echo " You are on RDHPCS :  ${machine}"
else
   echo " "
   echo " Your machine is $machine NOT found "
   echo " The script $0 can not continue.  Aborted ! "
   echo " "
   echo " Your machine must be CRAY (SURGE/LUNA)"
   echo " or DELL (MARS/VENUS) or HERA "
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

if [ "$machine" = "dell" ]; then
#
#   This is a test of GRIB_UTIL.v${ver} on $machine
#
    module use -a /usrx/local/nceplibs/dev/NCEPLIBS/modulefiles
    module unload grib_util
    module load grib_util/${ver}
    input_file=/usrx/local/nceplibs/dev/lib/fv3gfs
elif [ "$machine" = "cray" ]; then
#
#   This is a test version of GRIB_UTIL.v${ver} on $machine
#
    module unload grib_util
    module use -a /usrx/local/nceplibs/modulefiles
    module load grib_util/${ver}
    input_file=/usrx/local/nceplibs/gfs_data
fi

#
# These executable files (below) is in GRIB_UTIL.v${ver}
#
cnvgrib_test=$CNVGRIB
echo " "
module list
echo " "

set +x

#
#  Clean up temp directory before test starts
#
echo "Please wait ...  Cleaning up temp directory before test starts "
echo " "
if [ "$(ls -A $output_g1)" ]; then
   echo " "
   echo "Cleaning $output_g1"
   rm $output_g1/*
fi
if [ "$(ls -A $output_g2)" ]; then
   echo " "
   echo "Cleaning $output_g2"
   rm $output_g2/*
fi
if [ "$(ls -A $data)" ]; then
   echo " "
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

# echo " dir  $dir "
# echo " file $dir/$file "
# echo " data $data/$file "

#
# Step 1: CNVGRIB converts from GRIB2 to GRIB1
#

echo " "
echo " "
echo "Wait ...  Running cnvgrib (converts from grib2 -> grib1)"
echo " "
set -x
$cnvgrib_test -g21 $data/$file $output_g1/$file.grib2.test.g1
if [ $? -ne 0 ]; then err=1; fi
set +x
echo

export new_max=` ${WGRIB:?} -s $output_g1/$file.grib2.test.g1 |  grep ":HGT:800 mb:" |  $WGRIB -i -V $output_g1/$file.grib2.test.g1 \
         -o /dev/null | grep max | awk '{print $4}' `
export new_min=` ${WGRIB:?} -s $output_g1/$file.grib2.test.g1 |  grep ":HGT:800 mb:" |  $WGRIB -i -V $output_g1/$file.grib2.test.g1 \
         -o /dev/null | grep max | awk '{print $3}' `

echo " The new cnvgrib (cnvgrib v3.2.0) convert NAM file from GRIB2 to GRIB1."
echo " The data value MAX and MIN at HGT 800mb are correct " 

echo " "
echo " Data value MAX = " $new_max
echo " "
echo " Data value MIN  = " $new_min
echo " "
done
exit
