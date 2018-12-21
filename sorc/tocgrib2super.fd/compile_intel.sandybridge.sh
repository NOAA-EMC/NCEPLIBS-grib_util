module load PrgEnv-intel
module swap craype-haswell craype-sandybridge
#
module load jasper-gnu-sandybridge
module load png-intel-sandybridge
module load zlib-intel-sandybridge
#
module load g2-intel-sandybridge
module load w3nco-intel-sandybridge
module load bacio-intel-sandybridge
#
module list
make
mv tocgrib2super ../../exec
make clean

