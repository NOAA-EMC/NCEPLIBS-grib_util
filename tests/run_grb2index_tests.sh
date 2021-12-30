#!/bin/sh
# This is a test script for the NCEPLIBS-grib_util project.
#
# Ed Hartnett, 12/27/21

set -e
echo ""
echo "*** Running grb2index test"

# Create an index of a GRIB2 file.
../src/grb2index/grb2index ref_gdaswave.t00z.wcoast.0p16.f000.grib2 test_gdaswave.grb2index.idx

# Check against expected output. First 120 bytes contain differences,
# so ignore them.
cmp -i 140 test_gdaswave.grb2index.idx ref_gdaswave.grb2index.idx

echo "*** SUCCESS!"
exit 0
