module swap PrgEnv-cray PrgEnv-intel
module use /gpfs/hps/nco/ops/nwprod/lib/modulefiles
module load w3nco
module load bacio
module load ip
module load sp
make
mv copygb ../../exec
make clean

