module load PrgEnv-intel
module swap craype-haswell craype-sandybridge
module load iobuf
module list
make
mv wgrib ../../exec

