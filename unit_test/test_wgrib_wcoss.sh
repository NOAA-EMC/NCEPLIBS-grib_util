#!/bin/sh
export ver=1.2.0

#
#  This script uses to test the utility wgrib. The wgrib will display the inventory of GRIB1 file.
#  The test uses wgrib to display content of ecmwf file.
#  Note:
#  The ecmwf file contains both GRIB2 and GRIB1.

export cyc=00

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
wgrib=$WGRIB
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

if [ -f $input_file/U1D05081200050909001 ] ; then
   cp $input_file/U1D05081200050909001   $dir/data
else
   echo " "
   echo " "
   echo "ECMWF File $input_file/U1D05081200050909001  Does Not Exist."
   echo " "
   echo " No input ECMWF file to continue "
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
    echo " "
    echo "  Running WGRIB " 
    echo " "
    echo "  Please wait ..."
    echo " "
    $wgrib  $file > $output_g1/$file.txt 
    echo " "
    if [ $? -eq 0 ]; then echo "  PASS: wgrib successfully list all records in ecmwf file"; else echo "FAIL!"; err=1; fi
    echo " "
    echo " "
done
echo " "
echo "The output file is following: "
echo " "
echo " "
ls -l $output_g1/$file.txt
echo " "
echo " "
echo "  PASS: wgrib successfully list all records in ecmwf file"
echo " "
echo " "
exit

