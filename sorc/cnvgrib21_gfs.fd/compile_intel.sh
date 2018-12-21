module swap PrgEnv-cray PrgEnv-intel
module use /gpfs/hps/nco/ops/nwprod/lib/modulefiles
module load g2
module load w3nco
module load bacio
module load png
module load jasper
module load z
make
mv cnvgrib21_gfs ../../exec
make clean

