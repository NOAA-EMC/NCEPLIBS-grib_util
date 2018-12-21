module load PrgEnv-intel
module swap craype-haswell craype-sandybridge
#
module load jasper-gnu-sandybridge
module load png-intel-sandybridge
module load zlib-intel-sandybridge
#
module load g2-intel
module load w3nco-intel
module load bacio-intel
module load iobuf
#
module list
make
mv cnvgrib21_gfs ../../exec
make clean

