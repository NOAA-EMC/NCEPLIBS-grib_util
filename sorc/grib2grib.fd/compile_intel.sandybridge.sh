module load PrgEnv-intel
module swap craype-haswell craype-sandybridge
#
module load w3nco-intel-sandybridge
module load bacio-intel-sandybridge
#
module list
make
mv grib2grib ../../exec
make clean

