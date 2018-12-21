module swap PrgEnv-cray PrgEnv-intel
module use /gpfs/hps/nco/ops/nwprod/lib/modulefiles
module load w3nco
module load bacio
make
mv grib2grib ../../exec
make clean

