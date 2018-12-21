#!/bin/sh

set -x

. /apps/lmod/lmod/init/sh

export machine_lc=theia
module use /scratch3/NCEPDEV/nwprod/lib/modulefiles
module use /apps/modules/modulefamilies/intel

makefile=makefile_${machine_lc}

# Load required modules
module use ../../modulefiles
module load build_grib_util/${machine_lc}
module list

make -f $makefile
make -f $makefile install

