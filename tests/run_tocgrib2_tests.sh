#!/bin/sh
# This is a test script for the NCEPLIBS-grib_util project. This tests
# the tocgrib2 utility.
#
# Ed Hartnett, 4/20/24

set -e
echo ""
echo "*** Running tocgrib2 test"

# Should fail without input/output defined
unset FORT11
unset FORT51
../src/tocgrib2/tocgrib2 && exit 1

# Convert test file to GRIB1.
export FORT11="data/ref_gdas.t12z.pgrb2.1p00.anl.75r"
export FORT51=out.grib2
../src/tocgrib2/tocgrib2

echo "*** SUCCESS!"
exit 0
