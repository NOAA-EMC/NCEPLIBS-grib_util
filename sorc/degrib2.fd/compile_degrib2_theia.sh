#!/bin/sh

. /apps/lmod/lmod/init/sh

export machine_lc=theia

module use /scratch3/NCEPDEV/nwprod/lib/modulefiles

makefile=makefile_${machine_lc}

# Load required modules
module use ../../modulefiles
module load build_grib_util/${machine_lc}
module list

make -f $makefile
make -f $makefile install
make -f $makefile clean

