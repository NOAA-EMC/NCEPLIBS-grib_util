module swap PrgEnv-cray PrgEnv-intel
module use /gpfs/hps/nco/ops/nwprod/lib/modulefiles
module load g2
module load w3nco
module load bacio
module load jasper
module load png
module load z
make
mv cnvgrib ../../exec
make clean

