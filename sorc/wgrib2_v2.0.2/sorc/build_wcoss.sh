module purge
module load -a ../modulefiles/wgrib2/v2.0.2

module list
cd wgrib2
make clean
make
