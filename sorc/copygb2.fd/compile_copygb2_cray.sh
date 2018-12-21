#!/bin/sh

######################################################################
#
# This script uses to build copygb2 on CRAY (craype-sandybridge)
#      with INTEL Fortran compiler for both haswell and sandybridge
#      architectures.   compiler flag is   -acCore-AVX2
#
######################################################################
######################################################################

export nwprod=/gpfs/hps/nco/ops/nwprod
export version=1.0.4
export dir=` pwd `

export machine_type=$(hostname | cut -c1-1)
 
#
#    Checking for machine before installation start  ###
#
if  [ $machine_type = l -o $machine_type = s ] ; then # For CRAY
    machine=cray
else
   echo "  "
   echo "  "
   echo "  "
   echo "  Your machine is not CRAY (Luna or Surge) "
   echo "  "
   echo "  The copygb2 only builds on Luna/Surge machine. "
   echo "  "
   echo "  "
   echo "  "
   exit
fi

#
# Checking for copygb2 source code directory
#
cd  $dir
export chkfile=$dir/copygb2.f

if  [  ! -f $chkfile ] ; then
    echo " "
    echo " "
    echo " ##### copygb2 source code NOT found  #####"
    echo " "
    echo " Building executable copygb2 can not continue.  Aborted ! "
    echo " "
    echo " "
    echo " Please change to ${nwprod}/grib_util.v${version}/sorc n CRAY "
    echo "  "
    echo "  "
    exit
fi
#
#  Loading module files on CRAY
#
   module use /gpfs/hps/nco/ops/nwprod/lib/modulefiles
   module unload craype-haswell
   module load craype-sandybridge
   module load jasper-gnu-sandybridge/1.900.1
   module load png-intel-sandybridge/1.2.49
   module load zlib-intel-sandybridge/1.2.7

# Loading Intel Compiler Suite
   module unload PrgEnv-cray
   module load PrgEnv-intel/5.2.56
   module use /gpfs/hps/emc/global/noscrub/Boi.Vuong/g2/v3.0.0/modulefiles
   module load g2-intel/3.0.0
   module show g2-intel/3.0.0
   module load bacio-intel/2.0.1
   module load w3emc-intel/2.2.0
   module load w3nco-intel/2.0.6
   module load ip-intel/3.0.0
   module load sp-intel/2.0.2

# Loading IOBUF
   module load iobuf
   echo " "

   module list 2>compile-copygb2_intel_sandybridge.log
   module list
   echo " "
   echo " "
   echo " PLEASE WAIT FOR WRITING to LOG file. "
   echo " "
   echo " "
   echo " "
   make -f makefile_cray
   make -f makefile_cray clean
   make -f makefile_cray &>>compile-copygb2_intel_sandybridge.log
   make -f makefile_cray clean &>>compile-copygb2_intel_sandybridge.log
   mkdir -p ../../exec &>>compile-copygb2_intel_sandybridge.log
   mv copygb2 ../../exec &>>compile-copygb2_intel_sandybridge.log
   echo " "
   echo " "
   echo " "
   echo " Compilation completed successfully. Please refer to log file for details."
   echo " "
   echo " "
   echo " "
