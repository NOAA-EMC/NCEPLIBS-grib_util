module swap PrgEnv-cray PrgEnv-intel
module use /gpfs/hps/nco/ops/nwprod/lib/modulefiles
module load w3nco
module load bacio
make
mv tocgrib ../../exec
make clean

