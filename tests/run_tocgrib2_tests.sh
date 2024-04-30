#!/bin/sh
# This is a test script for the NCEPLIBS-grib_util project. This tests
# the tocgrib2 utility.
#
# Ed Hartnett, 4/20/24

set -e
echo ""
echo "*** Running tocgrib2 tests"

echo "*** Running tocgrib2 without input/output defined. Should return STOP 10"
unset FORT11
unset FORT51
../src/tocgrib2/tocgrib2 && exit 1

echo "*** Running tocgrib2 with input/output defined."
export FORT11="data/ref_gdaswave.t00z.wcoast.0p16.f000.grib2"
export FORT51=out.grib2
../src/tocgrib2/tocgrib2

echo "*** SUCCESS!"
exit 0
