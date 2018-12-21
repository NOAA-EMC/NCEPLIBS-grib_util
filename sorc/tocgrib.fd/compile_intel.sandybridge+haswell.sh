module load PrgEnv-intel
module swap craype-haswell craype-sandybridge
#
module load w3nco-intel
module load bacio-intel
module load iobuf
#
module list
make
mv tocgrib ../../exec
make clean

