module swap PrgEnv-cray PrgEnv-intel
module use /gpfs/hps/nco/ops/nwprod/lib/modulefiles
module load g2
module load w3nco
module load bacio
make
mv grb2index ../../exec
make clean

