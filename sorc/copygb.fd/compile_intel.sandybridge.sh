module load PrgEnv-intel
module swap craype-haswell craype-sandybridge
#
module load w3nco-intel-sandybridge
module load bacio-intel-sandybridge
#
module load ip-intel-sandybridge
module load sp-intel-sandybridge
#
module list
make
mv copygb ../../exec
make clean

