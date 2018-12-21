module load PrgEnv-intel
module swap craype-haswell craype-sandybridge
#
module load w3nco-intel
module load bacio-intel
#
module load ip-intel
module load sp-intel
module load iobuf
#
module list
make
mv copygb ../../exec
make clean

