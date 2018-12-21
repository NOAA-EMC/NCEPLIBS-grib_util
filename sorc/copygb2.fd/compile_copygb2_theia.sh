#!/bin/sh

LMOD_EXACT_MATCH=no
machine_lc=theia

makefile=makefile_wcoss_${machine_lc}

# Load required modules
module use ../../modulefiles
module load build_grib_util/${machine_lc}
module list

make -f $makefile
make -f $makefile install
make -f $makefile clean

